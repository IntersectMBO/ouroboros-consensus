{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- | Types used throughout the implementation: handle, state, environment,
-- types, trace types, etc.
module Ouroboros.Consensus.Storage.ChainDB.Impl.Types
  ( ChainDbEnv (..)
  , ChainDbHandle (..)
  , ChainDbState (..)
  , ChainSelectionPromise (..)
  , SerialiseDiskConstraints
  , getEnv
  , getEnv1
  , getEnv2
  , getEnvSTM
  , getEnvSTM1

    -- * Exposed internals for testing purposes
  , Internal (..)
  , InternalChain (..)
  , checkInternalChain

    -- * Iterator-related
  , IteratorKey (..)

    -- * Follower-related
  , FollowerHandle (..)
  , FollowerKey (..)
  , FollowerRollState (..)
  , FollowerState (..)
  , followerRollStatePoint

    -- * Invalid blocks
  , InvalidBlockInfo (..)
  , InvalidBlocks

    -- * Blocks to add
  , BlockToAdd (..)
  , ChainSelMessage (..)
  , ChainSelQueue -- opaque
  , addBlockToAdd
  , addPerasCertToQueue
  , addReprocessLoEBlocks
  , closeChainSelQueue
  , getChainSelMessage
  , getMaxSlotNoChainSelQueue
  , memberChainSelQueue
  , newChainSelQueue
  , processedChainSelMessage

    -- * Trace types
  , SelectionChangedInfo (..)
  , TraceAddBlockEvent (..)
  , TraceAddPerasCertEvent (..)
  , TraceChainSelStarvationEvent (..)
  , TraceCopyToImmutableDBEvent (..)
  , TraceEvent (..)
  , TraceFollowerEvent (..)
  , TraceGCEvent (..)
  , TraceInitChainSelEvent (..)
  , TraceIteratorEvent (..)
  , TraceOpenEvent (..)
  , TracePipeliningEvent (..)
  , TraceValidationEvent (..)
  ) where

import Control.Monad (when)
import Control.ResourceRegistry
import Control.Tracer
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet
import Data.Typeable
import Data.Void (Void)
import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (OnlyCheckWhnfNamed (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Fragment.Diff (ChainDiff)
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import Ouroboros.Consensus.Ledger.Extended (ExtValidationError, ExtLedgerState)
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Peras.SelectView (WeightedSelectView)
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Storage.ChainDB.API
  ( AddBlockPromise (..)
  , AddBlockResult (..)
  , AddPerasCertPromise (..)
  , ChainDbError (..)
  , ChainSelectionPromise (..)
  , ChainType
  , LoE
  , StreamFrom
  , StreamTo
  , UnknownRange
  )
import Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment
  ( InvalidBlockPunishment
  )
import Ouroboros.Consensus.Storage.ImmutableDB
  ( ImmutableDB
  , ImmutableDbSerialiseConstraints
  )
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import Ouroboros.Consensus.Storage.LedgerDB
  ( LedgerDB'
  , LedgerDbSerialiseConstraints
  )
import qualified Ouroboros.Consensus.Storage.LedgerDB as LedgerDB
import Ouroboros.Consensus.Storage.PerasCertDB (PerasCertDB)
import qualified Ouroboros.Consensus.Storage.PerasCertDB as PerasCertDB
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Storage.VolatileDB
  ( VolatileDB
  , VolatileDbSerialiseConstraints
  )
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import Ouroboros.Consensus.Util (Fuse)
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.Enclose (Enclosing, Enclosing' (..))
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.Orphans ()
import Ouroboros.Consensus.Util.STM (WithFingerprint)
import Ouroboros.Network.AnchoredFragment (Anchor, AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (MaxSlotNo (..))
import Ouroboros.Network.BlockFetch.ConsensusInterface
  ( ChainSelStarvation (..)
  )

-- | All the serialisation related constraints needed by the ChainDB.
class
  ( ImmutableDbSerialiseConstraints blk
  , LedgerDbSerialiseConstraints (ExtLedgerState blk) blk
  , VolatileDbSerialiseConstraints blk
  , -- Needed for Follower
    EncodeDiskDep (NestedCtxt Header) blk
  ) =>
  SerialiseDiskConstraints blk

-- | A handle to the internal ChainDB state
newtype ChainDbHandle m blk = CDBHandle (StrictTVar m (ChainDbState m blk))

-- | Check if the ChainDB is open, if so, executing the given function on the
-- 'ChainDbEnv', otherwise, throw a 'CloseDBError'.
getEnv ::
  forall m blk r.
  (IOLike m, HasCallStack, HasHeader blk) =>
  ChainDbHandle m blk ->
  (ChainDbEnv m blk -> m r) ->
  m r
getEnv (CDBHandle varState) f =
  atomically (readTVar varState) >>= \case
    ChainDbOpen env -> f env
    ChainDbClosed -> throwIO $ ClosedDBError @blk prettyCallStack

-- | Variant 'of 'getEnv' for functions taking one argument.
getEnv1 ::
  (IOLike m, HasCallStack, HasHeader blk) =>
  ChainDbHandle m blk ->
  (ChainDbEnv m blk -> a -> m r) ->
  a ->
  m r
getEnv1 h f a = getEnv h (\env -> f env a)

-- | Variant 'of 'getEnv' for functions taking two arguments.
getEnv2 ::
  (IOLike m, HasCallStack, HasHeader blk) =>
  ChainDbHandle m blk ->
  (ChainDbEnv m blk -> a -> b -> m r) ->
  a ->
  b ->
  m r
getEnv2 h f a b = getEnv h (\env -> f env a b)

-- | Variant of 'getEnv' that works in 'STM'.
getEnvSTM ::
  forall m blk r.
  (IOLike m, HasCallStack, HasHeader blk) =>
  ChainDbHandle m blk ->
  (ChainDbEnv m blk -> STM m r) ->
  STM m r
getEnvSTM (CDBHandle varState) f =
  readTVar varState >>= \case
    ChainDbOpen env -> f env
    ChainDbClosed -> throwSTM $ ClosedDBError @blk prettyCallStack

-- | Variant of 'getEnv1' that works in 'STM'.
getEnvSTM1 ::
  forall m blk a r.
  (IOLike m, HasCallStack, HasHeader blk) =>
  ChainDbHandle m blk ->
  (ChainDbEnv m blk -> a -> STM m r) ->
  a ->
  STM m r
getEnvSTM1 (CDBHandle varState) f a =
  readTVar varState >>= \case
    ChainDbOpen env -> f env a
    ChainDbClosed -> throwSTM $ ClosedDBError @blk prettyCallStack

data ChainDbState m blk
  = ChainDbOpen !(ChainDbEnv m blk)
  | ChainDbClosed
  deriving (Generic, NoThunks)

-- | The current chain, both without and with slot times
--
-- INVARIANT @'AF.mapAnchoredFragment' 'hwtHeader' . 'icWithTime' = 'icWithoutTime'@
--
-- The fragment with times is maintained separately --- but exactly in parallel
-- --- for performance reasons and modularity reasons, trading a few thousand
-- pointers to avoid extra allocation per use, more granular interfaces
-- (notably
-- 'Ouroboros.Network.BlockFetch.ConsensusInterface.BlockFetchConsensusInterface'),
-- etc.
data InternalChain blk = InternalChain
  { icWithoutTime :: !(AnchoredFragment (Header blk))
  , icWithTime :: !(AnchoredFragment (HeaderWithTime blk))
  }
  deriving Generic

deriving instance (HasHeader blk, NoThunks (Header blk)) => NoThunks (InternalChain blk)

checkInternalChain ::
  forall blk.
  (HasHeader blk, HasHeader (Header blk)) =>
  InternalChain blk ->
  Maybe String
checkInternalChain (InternalChain cur curWithTime) =
  if cnv id cur == cnv hwtHeader curWithTime
    then Nothing
    else
      Just $
        unlines
          [ "cdbChain and cdbChainWithTime were out of sync:"
          , show (cnv id cur)
          , show (cnv hwtHeader curWithTime)
          ]
 where
  cnv ::
    HeaderHash h ~ HeaderHash blk =>
    (h -> Header blk) -> AnchoredFragment h -> (Point blk, [Point blk])
  cnv f af =
    ( castPoint $ AF.anchorPoint af
    , (headerPoint . f) `map` AF.toNewestFirst af
    )

data ChainDbEnv m blk = CDB
  { cdbImmutableDB :: !(ImmutableDB m blk)
  , cdbVolatileDB :: !(VolatileDB m blk)
  , cdbLedgerDB :: !(LedgerDB' m blk)
  , cdbChain :: !(StrictTVar m (InternalChain blk))
  -- ^ Contains the current chain fragment.
  --
  -- INVARIANT: the anchor point of this fragment is the tip of the
  -- ImmutableDB. This implies that this fragment never contains any blocks
  -- that are stored in the immutable DB.
  --
  -- Note that this fragment might be shorter than @k@ headers when the
  -- whole chain is shorter than @k@ or in case of corruption of the
  -- VolatileDB.
  --
  -- Note that this fragment might also be /longer/ than @k@ headers,
  -- because the oldest blocks from the fragment might not yet have been
  -- copied from the VolatileDB to the ImmutableDB.
  --
  -- The anchor point of this chain should be the most recent \"immutable\"
  -- block according to the protocol, i.e., a block that cannot be rolled
  -- back.
  --
  -- Note that the \"immutable\" block isn't necessarily at the tip of the
  -- ImmutableDB, but could temporarily still be on the in-memory chain
  -- fragment. When the background thread that copies blocks to the
  -- ImmutableDB has caught up, the \"immutable\" block will be at the tip
  -- of the ImmutableDB again.
  --
  -- Note that the \"immutable\" block might be less than @k@ blocks from
  -- our tip in case the whole chain is shorter than @k@ or in case of
  -- corruption of the VolatileDB.
  --
  -- Note that the \"immutable\" block will /never/ be /more/ than @k@
  -- blocks back, as opposed to the anchor point of 'cdbChain'.
  , cdbTentativeState :: !(StrictTVar m (TentativeHeaderState blk))
  , cdbTentativeHeader :: !(StrictTVar m (StrictMaybe (Header blk)))
  -- ^ The tentative header, for diffusion pipelining.
  --
  -- INVARIANT: It fits on top of the current chain, and its body is not known
  -- to be invalid, but might turn out to be.
  , cdbIterators :: !(StrictTVar m (Map IteratorKey (m ())))
  -- ^ The iterators.
  --
  -- This maps the 'IteratorKey's of each open 'Iterator' to a function
  -- that, when called, closes the iterator. This is used when closing the
  -- ChainDB: the open file handles used by iterators can be closed, and the
  -- iterators themselves are closed so that it is impossible to use an
  -- iterator after closing the ChainDB itself.
  , cdbFollowers :: !(StrictTVar m (Map FollowerKey (FollowerHandle m blk)))
  -- ^ The followers.
  --
  -- A follower is open iff its 'FollowerKey' is this 'Map'.
  --
  -- INVARIANT: the 'followerPoint' of each follower is 'withinFragmentBounds'
  -- of the current chain fragment (retrieved 'cdbGetCurrentChain', not by
  -- reading 'cdbChain' directly).
  , cdbTopLevelConfig :: !(TopLevelConfig blk)
  , cdbInvalid :: !(StrictTVar m (WithFingerprint (InvalidBlocks blk)))
  -- ^ See the docstring of 'InvalidBlocks'.
  --
  -- The 'Fingerprint' changes every time a hash is added to the map, but
  -- not when hashes are garbage-collected from the map.
  , cdbNextIteratorKey :: !(StrictTVar m IteratorKey)
  , cdbNextFollowerKey :: !(StrictTVar m FollowerKey)
  , cdbCopyFuse :: !(Fuse m)
  , cdbChainSelFuse :: !(Fuse m)
  , cdbTracer :: !(Tracer m (TraceEvent blk))
  , cdbRegistry :: !(ResourceRegistry m)
  , cdbGcDelay :: !DiffTime
  -- ^ How long to wait between copying a block from the VolatileDB to
  -- ImmutableDB and garbage collecting it from the VolatileDB
  , cdbGcInterval :: !DiffTime
  -- ^ Minimum time between two garbage collections. Is used to batch
  -- garbage collections.
  , cdbKillBgThreads :: !(StrictTVar m (m ()))
  -- ^ A handle to kill the background threads.
  , cdbChainSelQueue :: !(ChainSelQueue m blk)
  -- ^ Queue of blocks that still have to be added.
  , cdbLoE :: !(m (LoE (AnchoredFragment (HeaderWithTime blk))))
  -- ^ Configure the Limit on Eagerness. If this is 'LoEEnabled', it contains
  -- an action that returns the LoE fragment, which indicates the latest rollback
  -- point, i.e. we are not allowed to select a chain from which we could not
  -- switch back to a chain containing it. The fragment is usually anchored at
  -- a recent immutable tip; if it does not, it will conservatively be treated
  -- as the empty fragment anchored in the current immutable tip.
  , cdbChainSelStarvation :: !(StrictTVar m ChainSelStarvation)
  -- ^ Information on the last starvation of ChainSel, whether ongoing or
  -- ended recently.
  , cdbPerasCertDB :: !(PerasCertDB m blk)
  }
  deriving Generic

-- | We include @blk@ in 'showTypeOf' because it helps resolving type families
-- (but avoid including @m@ because we cannot impose @Typeable m@ as a
-- constraint and still have it work with the simulator)
instance
  (IOLike m, LedgerSupportsProtocol blk, BlockSupportsDiffusionPipelining blk) =>
  NoThunks (ChainDbEnv m blk)
  where
  showTypeOf _ = "ChainDbEnv m " ++ show (typeRep (Proxy @blk))

{-------------------------------------------------------------------------------
  Exposed internals for testing purposes
-------------------------------------------------------------------------------}

data Internal m blk = Internal
  { intCopyToImmutableDB :: m (WithOrigin SlotNo)
  -- ^ Copy the blocks older than @k@ from to the VolatileDB to the
  -- ImmutableDB and update the in-memory chain fragment correspondingly.
  --
  -- The 'SlotNo' of the tip of the ImmutableDB after copying the blocks is
  -- returned. This can be used for a garbage collection on the VolatileDB.
  , intGarbageCollect :: SlotNo -> m ()
  -- ^ Perform garbage collection for blocks <= the given 'SlotNo'.
  , intTryTakeSnapshot :: m ()
  -- ^ Write a new LedgerDB snapshot to disk and remove the oldest one(s).
  , intAddBlockRunner :: m Void
  -- ^ Start the loop that adds blocks to the ChainDB retrieved from the
  -- queue populated by 'ChainDB.addBlock'. Execute this loop in a separate
  -- thread.
  , intKillBgThreads :: StrictTVar m (m ())
  -- ^ A handle to kill the background threads.
  }

{-------------------------------------------------------------------------------
  Iterator-related
-------------------------------------------------------------------------------}

-- | We use this internally to track iterators in a map ('cdbIterators') in
-- the ChainDB state so that we can remove them from the map when the iterator
-- is closed.
--
-- We store them in the map so that the ChainDB can close all open iterators
-- when it is closed itself.
newtype IteratorKey = IteratorKey Word
  deriving stock Show
  deriving newtype (Eq, Ord, Enum, NoThunks)

{-------------------------------------------------------------------------------
  Follower-related
-------------------------------------------------------------------------------}

-- Note: these things are not in the Follower module, because 'TraceEvent'
-- depends on them, 'ChainDbEnv.cdbTracer' depends on 'TraceEvent', and most
-- modules depend on 'ChainDbEnv'. Also, 'ChainDbEnv.cdbFollowers' depends on
-- 'FollowerState'.

-- | We use this internally to track follower in a map ('cdbFollowers') in the
-- ChainDB state so that we can remove them from the map when the follower is
-- closed.
--
-- We store them in the map so that the ChainDB can close all open followers
-- when it is closed itself and to update the followers in case we switch to a
-- different chain.
newtype FollowerKey = FollowerKey Word
  deriving stock Show
  deriving newtype (Eq, Ord, Enum, NoThunks)

-- | Internal handle to a 'Follower' without an explicit @b@ (@blk@, @'Header'
-- blk@, etc.) parameter so 'Follower's with different' @b@s can be stored
-- together in 'cdbFollowers'.
data FollowerHandle m blk = FollowerHandle
  { fhChainType :: ChainType
  -- ^ Whether we follow the tentative chain.
  , fhSwitchFork :: AnchoredFragment (Header blk) -> STM m ()
  -- ^ When we have switched to a fork, all open 'Follower's must be notified.
  --
  -- Receives the suffix of the old chain anchored at the intersection with the
  -- new chain.
  , fhClose :: m ()
  -- ^ When closing the ChainDB, we must also close all open 'Follower's, as
  -- they might be holding on to resources.
  --
  -- Call 'fhClose' will release the resources used by the 'Follower'.
  --
  -- NOTE the 'Follower' is not removed from 'cdbFollowers'. (That is done by
  -- 'closeAllFollowers').
  }
  deriving NoThunks via OnlyCheckWhnfNamed "FollowerHandle" (FollowerHandle m blk)

-- | @b@ corresponds to the 'BlockComponent' that is being read.
data FollowerState m blk b
  = -- | The 'Follower' is in its initial state. Its 'FollowerRollState' is
    -- @'RollBackTo' 'genesisPoint'@.
    --
    -- This is equivalent to having a 'FollowerInImmutableDB' with the same
    -- 'FollowerRollState' and an iterator streaming after genesis. Opening such
    -- an iterator has a cost (index files will have to be read). However, in
    -- most cases, right after opening a Follower, the user of the Follower will try
    -- to move it forward, moving it from genesis to a more recent point on the
    -- chain. So we incur the cost of opening the iterator while not even using
    -- it.
    --
    -- Therefore, we have this extra initial state, that avoids this cost.
    -- When the user doesn't move the Follower forward, an iterator is opened.
    FollowerInit
  | -- | The 'Follower' is reading from the ImmutableDB.
    --
    -- Note that the iterator includes 'Point blk' in addition to @b@, as it
    -- is needed to keep track of where the iterator is.
    --
    -- INVARIANT: for all @FollowerInImmutableDB rollState immIt@: the predecessor
    -- of the next block streamed by @immIt@ must be the block identified by
    -- @followerRollStatePoint rollState@. In other words: the iterator is
    -- positioned /on/ @followerRollStatePoint rollState@.
    FollowerInImmutableDB
      !(FollowerRollState blk)
      !(ImmutableDB.Iterator m blk (Point blk, b))
  | -- | The 'Follower' is reading from the in-memory current chain fragment.
    FollowerInMem !(FollowerRollState blk)
  deriving (Generic, NoThunks)

-- | Similar to 'Ouroboros.Network.Mock.ProducerState.FollowerState'.
data FollowerRollState blk
  = -- | We don't know at which point the user is, but the next message we'll
    -- send is to roll back to this point.
    RollBackTo !(Point blk)
  | -- | We know that the follower is at this point and the next message we'll
    -- send is to roll forward to the point /after/ this point on our chain.
    RollForwardFrom !(Point blk)
  deriving (Eq, Show, Generic, NoThunks)

-- | Get the point the 'FollowerRollState' should roll back to or roll forward
-- from.
followerRollStatePoint :: FollowerRollState blk -> Point blk
followerRollStatePoint (RollBackTo pt) = pt
followerRollStatePoint (RollForwardFrom pt) = pt

{-------------------------------------------------------------------------------
  Invalid blocks
-------------------------------------------------------------------------------}

-- | Hashes corresponding to invalid blocks. This is used to ignore these
-- blocks during chain selection.
type InvalidBlocks blk = Map (HeaderHash blk) (InvalidBlockInfo blk)

-- | In addition to the reason why a block is invalid, the slot number of the
-- block is stored, so that whenever a garbage collection is performed on the
-- VolatileDB for some slot @s@, the hashes older or equal to @s@ can be
-- removed from this map.
data InvalidBlockInfo blk = InvalidBlockInfo
  { invalidBlockReason :: !(ExtValidationError blk)
  , invalidBlockSlotNo :: !SlotNo
  }
  deriving (Eq, Show, Generic, NoThunks)

{-------------------------------------------------------------------------------
  Blocks to add
-------------------------------------------------------------------------------}

-- | FIFO queue used to add blocks asynchronously to the ChainDB. Blocks are
-- read from this queue by a background thread, which processes the blocks
-- synchronously.
--
-- We also maintain a multiset of the points of all of the blocks in the queue,
-- plus potentially the one block for which chain selection is currently in
-- progress. It is used to account for queued blocks in eg 'getIsFetched' and
-- 'getMaxSlotNo'.
--
-- INVARIANT: Counted with multiplicity, @varChainSelPoints@ contains exactly
-- the same hashes or at most one additional hash compared to the hashes of
-- blocks in @varChainSelQueue@.
data ChainSelQueue m blk = ChainSelQueue
  { varChainSelQueue :: TBQueue m (ChainSelMessage m blk)
  , varChainSelPoints :: StrictTVar m (MultiSet (RealPoint blk))
  }
  deriving NoThunks via OnlyCheckWhnfNamed "ChainSelQueue" (ChainSelQueue m blk)

-- | Entry in the 'ChainSelQueue' queue: a block together with the 'TMVar's used
-- to implement 'AddBlockPromise'.
data BlockToAdd m blk = BlockToAdd
  { blockPunish :: !(InvalidBlockPunishment m)
  -- ^ Executed immediately upon determining this block or one from its prefix
  -- is invalid.
  , blockToAdd :: !blk
  , varBlockWrittenToDisk :: !(StrictTMVar m Bool)
  -- ^ Used for the 'blockWrittenToDisk' field of 'AddBlockPromise'.
  , varBlockProcessed :: !(StrictTMVar m (AddBlockResult blk))
  -- ^ Used for the 'blockProcessed' field of 'AddBlockPromise'.
  }

-- | Different async tasks for triggering ChainSel
data ChainSelMessage m blk
  = -- | Add a new block
    ChainSelAddBlock !(BlockToAdd m blk)
  | -- | Add a Peras certificate
    ChainSelAddPerasCert
      !(ValidatedPerasCert blk)
      -- | Used for 'AddPerasCertPromise'.
      !(StrictTMVar m ())
  | -- | Reprocess blocks that have been postponed by the LoE.
    ChainSelReprocessLoEBlocks
      -- | Used for 'ChainSelectionPromise'.
      !(StrictTMVar m ())

-- | Create a new 'ChainSelQueue' with the given size.
newChainSelQueue :: (IOLike m, StandardHash blk, Typeable blk) => Word -> m (ChainSelQueue m blk)
newChainSelQueue chainSelQueueCapacity = do
  varChainSelQueue <- newTBQueueIO (fromIntegral chainSelQueueCapacity)
  varChainSelPoints <- newTVarIO MultiSet.empty
  pure
    ChainSelQueue
      { varChainSelQueue
      , varChainSelPoints
      }

-- | Add a block to the 'ChainSelQueue' queue. Can block when the queue is full.
addBlockToAdd ::
  (IOLike m, HasHeader blk) =>
  Tracer m (TraceAddBlockEvent blk) ->
  ChainSelQueue m blk ->
  InvalidBlockPunishment m ->
  blk ->
  m (AddBlockPromise m blk)
addBlockToAdd tracer (ChainSelQueue{varChainSelQueue, varChainSelPoints}) punish blk = do
  varBlockWrittenToDisk <- newEmptyTMVarIO
  varBlockProcessed <- newEmptyTMVarIO
  let !toAdd =
        BlockToAdd
          { blockPunish = punish
          , blockToAdd = blk
          , varBlockWrittenToDisk
          , varBlockProcessed
          }
      pt = blockRealPoint blk
  traceWith tracer $ AddedBlockToQueue pt RisingEdge
  queueSize <- atomically $ do
    writeTBQueue varChainSelQueue (ChainSelAddBlock toAdd)
    modifyTVar varChainSelPoints $ MultiSet.insert pt
    lengthTBQueue varChainSelQueue
  traceWith tracer $
    AddedBlockToQueue (blockRealPoint blk) (FallingEdgeWith (fromIntegral queueSize))
  return
    AddBlockPromise
      { blockWrittenToDisk = readTMVar varBlockWrittenToDisk
      , blockProcessed = readTMVar varBlockProcessed
      }

-- | Add a Peras certificate to the background queue.
addPerasCertToQueue ::
  (IOLike m, StandardHash blk) =>
  Tracer m (TraceAddPerasCertEvent blk) ->
  ChainSelQueue m blk ->
  ValidatedPerasCert blk ->
  m (AddPerasCertPromise m)
addPerasCertToQueue tracer ChainSelQueue{varChainSelQueue} cert = do
  varProcessed <- newEmptyTMVarIO
  traceWith tracer $ addedToQueue RisingEdge
  queueSize <- atomically $ do
    writeTBQueue varChainSelQueue $ ChainSelAddPerasCert cert varProcessed
    lengthTBQueue varChainSelQueue
  traceWith tracer $ addedToQueue $ FallingEdgeWith $ fromIntegral queueSize
  pure
    AddPerasCertPromise
      { waitPerasCertProcessed = atomically $ readTMVar varProcessed
      }
 where
  addedToQueue = AddedPerasCertToQueue (getPerasCertRound cert) (getPerasCertBoostedBlock cert)

-- | Try to add blocks again that were postponed due to the LoE.
addReprocessLoEBlocks ::
  IOLike m =>
  Tracer m (TraceAddBlockEvent blk) ->
  ChainSelQueue m blk ->
  m (ChainSelectionPromise m)
addReprocessLoEBlocks tracer ChainSelQueue{varChainSelQueue} = do
  varProcessed <- newEmptyTMVarIO
  let waitUntilRan = atomically $ readTMVar varProcessed
  traceWith tracer $ AddedReprocessLoEBlocksToQueue RisingEdge
  queueSize <- atomically $ do
    writeTBQueue varChainSelQueue $
      ChainSelReprocessLoEBlocks varProcessed
    lengthTBQueue varChainSelQueue
  traceWith tracer $
    AddedReprocessLoEBlocksToQueue (FallingEdgeWith (fromIntegral queueSize))
  return $ ChainSelectionPromise waitUntilRan

-- | Get the oldest message from the 'ChainSelQueue' queue. Can block when the
-- queue is empty; in that case, reports the starvation (and its end) via the
-- given tracer.
getChainSelMessage ::
  forall m blk.
  (HasHeader blk, IOLike m) =>
  Tracer m (TraceChainSelStarvationEvent blk) ->
  StrictTVar m ChainSelStarvation ->
  ChainSelQueue m blk ->
  m (ChainSelMessage m blk)
getChainSelMessage starvationTracer starvationVar chainSelQueue =
  atomically (tryReadTBQueue queue) >>= \case
    Just msg -> pure msg
    Nothing -> do
      startStarvationMeasure
      msg <- atomically $ readTBQueue queue
      terminateStarvationMeasure msg
      pure msg
 where
  ChainSelQueue
    { varChainSelQueue = queue
    } = chainSelQueue

  startStarvationMeasure :: m ()
  startStarvationMeasure = do
    prevStarvation <- atomically $ swapTVar starvationVar ChainSelStarvationOngoing
    when (prevStarvation /= ChainSelStarvationOngoing) $
      traceWith starvationTracer $
        ChainSelStarvation RisingEdge

  terminateStarvationMeasure :: ChainSelMessage m blk -> m ()
  terminateStarvationMeasure = \case
    ChainSelAddBlock BlockToAdd{blockToAdd = block} -> do
      let pt = blockRealPoint block
      traceWith starvationTracer $ ChainSelStarvation (FallingEdgeWith pt)
      atomically . writeTVar starvationVar . ChainSelStarvationEndedAt =<< getMonotonicTime
    ChainSelAddPerasCert{} -> pure ()
    ChainSelReprocessLoEBlocks{} -> pure ()

-- | Flush the 'ChainSelQueue' queue and notify the waiting threads.
closeChainSelQueue :: IOLike m => ChainSelQueue m blk -> STM m ()
closeChainSelQueue ChainSelQueue{varChainSelQueue = queue} = do
  traverse_ deliverPromise =<< flushTBQueue queue
 where
  deliverPromise = \case
    ChainSelAddBlock ab ->
      tryPutTMVar (varBlockProcessed ab) (FailedToAddBlock "Queue flushed")
    ChainSelAddPerasCert _cert varProcessed ->
      tryPutTMVar varProcessed ()
    ChainSelReprocessLoEBlocks varProcessed ->
      tryPutTMVar varProcessed ()

-- | To invoke when the given 'ChainSelMessage' has been processed by ChainSel.
-- This is used to remove the respective point from the multiset of points in
-- the 'ChainSelQueue' (as the block has now been written to disk by ChainSel).
processedChainSelMessage ::
  (IOLike m, HasHeader blk) =>
  ChainSelQueue m blk ->
  ChainSelMessage m blk ->
  STM m ()
processedChainSelMessage ChainSelQueue{varChainSelPoints} = \case
  ChainSelAddBlock BlockToAdd{blockToAdd = blk} ->
    modifyTVar varChainSelPoints $ MultiSet.delete (blockRealPoint blk)
  ChainSelAddPerasCert{} ->
    pure ()
  ChainSelReprocessLoEBlocks{} ->
    pure ()

-- | Return a function to test the membership
memberChainSelQueue ::
  (IOLike m, HasHeader blk) =>
  ChainSelQueue m blk ->
  STM m (RealPoint blk -> Bool)
memberChainSelQueue ChainSelQueue{varChainSelPoints} =
  flip MultiSet.member <$> readTVar varChainSelPoints

getMaxSlotNoChainSelQueue ::
  IOLike m =>
  ChainSelQueue m blk ->
  STM m MaxSlotNo
getMaxSlotNoChainSelQueue ChainSelQueue{varChainSelPoints} =
  aux <$> readTVar varChainSelPoints
 where
  -- \| The 'Ord' instance of 'RealPoint' orders by 'SlotNo' first, so the
  -- maximal key of the map has the greatest 'SlotNo'.
  aux :: MultiSet (RealPoint blk) -> MaxSlotNo
  aux pts = case MultiSet.maxView pts of
    Nothing -> NoMaxSlotNo
    Just (RealPoint s _, _) -> MaxSlotNo s

{-------------------------------------------------------------------------------
  Trace types
-------------------------------------------------------------------------------}

-- | Trace type for the various events of the ChainDB.
data TraceEvent blk
  = TraceAddBlockEvent (TraceAddBlockEvent blk)
  | TraceFollowerEvent (TraceFollowerEvent blk)
  | TraceCopyToImmutableDBEvent (TraceCopyToImmutableDBEvent blk)
  | TraceGCEvent (TraceGCEvent blk)
  | TraceInitChainSelEvent (TraceInitChainSelEvent blk)
  | TraceOpenEvent (TraceOpenEvent blk)
  | TraceIteratorEvent (TraceIteratorEvent blk)
  | TraceLedgerDBEvent (LedgerDB.TraceEvent blk)
  | TraceImmutableDBEvent (ImmutableDB.TraceEvent blk)
  | TraceVolatileDBEvent (VolatileDB.TraceEvent blk)
  | TracePerasCertDbEvent (PerasCertDB.TraceEvent blk)
  | TraceLastShutdownUnclean
  | TraceChainSelStarvationEvent (TraceChainSelStarvationEvent blk)
  | TraceAddPerasCertEvent (TraceAddPerasCertEvent blk)
  deriving Generic

deriving instance
  ( Show (Header blk)
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  ) =>
  Show (TraceEvent blk)

data TraceOpenEvent blk
  = -- | The ChainDB started the process of opening.
    StartedOpeningDB
  | -- | The ChainDB was opened.
    OpenedDB
      -- | Immutable tip
      (Point blk)
      -- | Tip of the current chain
      (Point blk)
  | -- | The ChainDB was closed.
    ClosedDB
      -- | Immutable tip
      (Point blk)
      -- | Tip of the current chain
      (Point blk)
  | -- | The ImmutableDB started the process of opening.
    StartedOpeningImmutableDB
  | -- | The ImmutableDB was opened.
    OpenedImmutableDB
      -- | Immutable tip
      (Point blk)
      -- | Chunk number of the immutable tip
      ImmutableDB.ChunkNo
  | -- | The VolatileDB started opening.
    StartedOpeningVolatileDB
  | -- | The VolatileDB was opened, with the highest seen slot number for any
    -- block currently in the DB.
    OpenedVolatileDB MaxSlotNo
  | -- | The LedgerDB started opening.
    StartedOpeningLgrDB
  | -- | The LedgerDB was opened.
    OpenedLgrDB
  deriving (Generic, Eq, Show)

-- | Information on having changed our selection to a chain with a (necessarily)
-- new tip.
--
-- NOTE: the fields of this record are intentionally lazy to prevent the
-- forcing of this information in case it doesn't have to be traced. However,
-- this means that the tracer processing this message /must not/ hold on to
-- it, otherwise it leaks memory.
data SelectionChangedInfo blk = SelectionChangedInfo
  { newTipPoint :: RealPoint blk
  -- ^ The new tip of the current chain.
  , newTipEpoch :: EpochNo
  -- ^ The epoch of the new tip.
  , newTipSlotInEpoch :: Word64
  -- ^ The slot in the epoch, i.e., the relative slot number, of the new
  -- tip.
  , newTipTrigger :: Maybe (RealPoint blk)
  -- ^ The new tip of the current chain ('newTipPoint') is the result of
  -- performing chain selection for a /trigger/ block ('newTipTrigger').
  -- In most cases, we add a new block to the tip of the current chain, in
  -- which case the new tip /is/ the trigger block.
  --
  -- However, this is not always the case. For example, with our current
  -- chain being A and having a disconnected C lying around, adding B will
  -- result in A -> B -> C as the new chain. The trigger B /= the new tip
  -- C.
  --
  -- Due to the Ouroboros Genesis (Limit on Eagerness), chain selection can also
  -- be triggered without any particular trigger block, in which case this is
  -- 'Nothing'.
  , newSuffixSelectView :: WeightedSelectView (BlockProtocol blk)
  -- ^ The 'WeightedSelectView' of the suffix of our new selection that was not
  -- already present in the old selection. It is guaranteed that
  --
  -- > preferCandidate cfg
  -- >   (withEmptyFragmentFromMaybe oldSuffixSelectView)
  -- >   newSuffixSelectView
  , oldSuffixSelectView :: Maybe (WeightedSelectView (BlockProtocol blk))
  -- ^ The 'WeightedSelectView' of the orphaned suffix of our old selection.
  -- This is 'Nothing' when we extended our selection.
  }
  deriving Generic

deriving stock instance
  (Show (TiebreakerView (BlockProtocol blk)), StandardHash blk) => Show (SelectionChangedInfo blk)
deriving stock instance
  (Eq (TiebreakerView (BlockProtocol blk)), StandardHash blk) => Eq (SelectionChangedInfo blk)

-- | Trace type for the various events that occur when adding a block.
data TraceAddBlockEvent blk
  = -- | A block with a 'BlockNo' not newer than the immutable tip was ignored.
    IgnoreBlockOlderThanImmTip (RealPoint blk)
  | -- | A block that is already in the Volatile DB was ignored.
    IgnoreBlockAlreadyInVolatileDB (RealPoint blk)
  | -- | A block that is know to be invalid was ignored.
    IgnoreInvalidBlock (RealPoint blk) (ExtValidationError blk)
  | -- | The block was added to the queue and will be added to the ChainDB by
    -- the background thread. The size of the queue is included.
    AddedBlockToQueue (RealPoint blk) (Enclosing' Word)
  | -- | Popping a new message for the chain selection background thread from
    -- the queue.
    PoppingFromQueue
  | -- | The block popped from the queue and will imminently be added to the
    -- ChainDB.
    PoppedBlockFromQueue (RealPoint blk)
  | -- | A message was added to the queue that requests that ChainSel reprocess
    -- blocks that were postponed by the LoE. The size of the queue is included.
    AddedReprocessLoEBlocksToQueue (Enclosing' Word)
  | -- | ChainSel will reprocess blocks that were postponed by the LoE.
    PoppedReprocessLoEBlocksFromQueue
  | -- | A block was added to the Volatile DB
    AddedBlockToVolatileDB (RealPoint blk) BlockNo IsEBB Enclosing
  | -- | The block fits onto the current chain, we'll try to use it to extend
    -- our chain.
    TryAddToCurrentChain (RealPoint blk)
  | -- | The block fits onto some fork, we'll try to switch to that fork (if
    -- it is preferable to our chain).
    TrySwitchToAFork (RealPoint blk) (ChainDiff (HeaderFields blk))
  | -- | The block doesn't fit onto any other block, so we store it and ignore
    -- it.
    StoreButDontChange (RealPoint blk)
  | -- | Debugging information about chain selection and LoE
    ChainSelectionLoEDebug (AnchoredFragment (Header blk)) (LoE (AnchoredFragment (Header blk)))
  | -- | The new block fits onto the current chain (first
    -- fragment) and we have successfully used it to extend our (new) current
    -- chain (second fragment).
    AddedToCurrentChain
      [LedgerEvent blk]
      (SelectionChangedInfo blk)
      (AnchoredFragment (Header blk))
      (AnchoredFragment (Header blk))
  | -- | The new block fits onto some fork and we have switched to that fork
    -- (second fragment), as it is preferable to our (previous) current chain
    -- (first fragment).
    SwitchedToAFork
      [LedgerEvent blk]
      (SelectionChangedInfo blk)
      (AnchoredFragment (Header blk))
      (AnchoredFragment (Header blk))
  | -- | An event traced during validating performed while adding a block.
    AddBlockValidation (TraceValidationEvent blk)
  | -- | The tentative header (in the context of diffusion pipelining) has been
    -- updated.
    PipeliningEvent (TracePipeliningEvent blk)
  | -- | Herald of 'AddedToCurrentChain' or 'SwitchedToAFork'. Lists the tip of
    -- the new chain.
    ChangingSelection (Point blk)
  deriving Generic

deriving instance
  ( Eq (Header blk)
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  ) =>
  Eq (TraceAddBlockEvent blk)
deriving instance
  ( Show (Header blk)
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  ) =>
  Show (TraceAddBlockEvent blk)

data TraceValidationEvent blk
  = -- | A point was found to be invalid.
    InvalidBlock
      (ExtValidationError blk)
      (RealPoint blk)
  | -- | A candidate chain was valid.
    ValidCandidate (AnchoredFragment (Header blk))
  | UpdateLedgerDbTraceEvent (LedgerDB.TraceValidateEvent blk)
  deriving Generic

deriving instance
  ( Eq (Header blk)
  , LedgerSupportsProtocol blk
  ) =>
  Eq (TraceValidationEvent blk)
deriving instance
  ( Show (Header blk)
  , LedgerSupportsProtocol blk
  ) =>
  Show (TraceValidationEvent blk)

data TracePipeliningEvent blk
  = -- | A new tentative header got set.
    SetTentativeHeader (Header blk) Enclosing
  | -- | The body of tentative block turned out to be invalid.
    TrapTentativeHeader (Header blk)
  | -- | We selected a new (better) chain, which cleared the previous tentative
    -- header.
    OutdatedTentativeHeader (Header blk)

deriving stock instance Eq (Header blk) => Eq (TracePipeliningEvent blk)
deriving stock instance Show (Header blk) => Show (TracePipeliningEvent blk)

data TraceInitChainSelEvent blk
  = -- | An event traced when inital chain selection has started during the
    -- initialization of ChainDB
    StartedInitChainSelection
  | -- | An event traced when inital chain has been selected
    InitialChainSelected
  | -- | An event traced during validation performed while performing initial
    -- chain selection.
    InitChainSelValidation (TraceValidationEvent blk)
  deriving Generic

deriving instance
  ( Eq (Header blk)
  , LedgerSupportsProtocol blk
  ) =>
  Eq (TraceInitChainSelEvent blk)
deriving instance
  ( Show (Header blk)
  , LedgerSupportsProtocol blk
  ) =>
  Show (TraceInitChainSelEvent blk)

data TraceFollowerEvent blk
  = -- | A new follower was created.
    NewFollower
  | -- | The follower was in the 'FollowerInMem' state but its point is no longer on
    -- the in-memory chain fragment, so it has to switch to the
    -- 'FollowerInImmutableDB' state.
    FollowerNoLongerInMem (FollowerRollState blk)
  | -- | The follower was in the 'FollowerInImmutableDB' state and is switched to
    -- the 'FollowerInMem' state.
    FollowerSwitchToMem
      -- | Point at which the follower is
      (Point blk)
      -- | Slot number at the tip of the ImmutableDB
      (WithOrigin SlotNo)
  | -- | The follower is in the 'FollowerInImmutableDB' state but the iterator is
    -- exhausted while the ImmutableDB has grown, so we open a new iterator to
    -- stream these blocks too.
    FollowerNewImmIterator
      -- | Point at which the follower is
      (Point blk)
      -- | Slot number at the tip of the ImmutableDB
      (WithOrigin SlotNo)
  deriving (Generic, Eq, Show)

data TraceCopyToImmutableDBEvent blk
  = -- | A block was successfully copied to the ImmutableDB.
    CopiedBlockToImmutableDB (Point blk)
  | -- | There are no block to copy to the ImmutableDB.
    NoBlocksToCopyToImmutableDB
  deriving (Generic, Eq, Show)

data TraceGCEvent blk
  = -- | A garbage collection for the given 'SlotNo' was scheduled to happen
    -- at the given time.
    ScheduledGC SlotNo Time
  | -- | A garbage collection for the given 'SlotNo' was performed.
    PerformedGC SlotNo
  deriving (Generic, Eq, Show)

data TraceIteratorEvent blk
  = -- | An unknown range was requested, see 'UnknownRange'.
    UnknownRangeRequested (UnknownRange blk)
  | -- | Stream only from the VolatileDB.
    StreamFromVolatileDB
      (StreamFrom blk)
      (StreamTo blk)
      [RealPoint blk]
  | -- | Stream only from the ImmutableDB.
    StreamFromImmutableDB
      (StreamFrom blk)
      (StreamTo blk)
  | -- | Stream from both the VolatileDB and the ImmutableDB.
    StreamFromBoth
      (StreamFrom blk)
      (StreamTo blk)
      [RealPoint blk]
  | -- | A block is no longer in the VolatileDB because it has been garbage
    -- collected. It might now be in the ImmutableDB if it was part of the
    -- current chain.
    BlockMissingFromVolatileDB (RealPoint blk)
  | -- | A block that has been garbage collected from the VolatileDB is now
    -- found and streamed from the ImmutableDB.
    BlockWasCopiedToImmutableDB (RealPoint blk)
  | -- | A block is no longer in the VolatileDB and isn't in the ImmutableDB
    -- either; it wasn't part of the current chain.
    BlockGCedFromVolatileDB (RealPoint blk)
  | -- | We have streamed one or more blocks from the ImmutableDB that were part
    -- of the VolatileDB when initialising the iterator. Now, we have to look
    -- back in the VolatileDB again because the ImmutableDB doesn't have the
    -- next block we're looking for.
    SwitchBackToVolatileDB
  deriving (Generic, Eq, Show)

-- | Chain selection is /starved/ when the background thread runs out of work.
-- This is the usual case and innocent while caught-up; but while syncing, it
-- means that we are downloading blocks at a smaller rate than we can validate
-- them, even though we generally expect to be CPU-bound.
--
-- TODO: Investigate why it happens regularly during syncing for very short
-- times.
--
-- The point in the trace is the block that finished the starvation.
newtype TraceChainSelStarvationEvent blk
  = ChainSelStarvation (Enclosing' (RealPoint blk))
  deriving (Generic, Eq, Show)

data TraceAddPerasCertEvent blk
  = -- | The Peras certificate from the given round boosting the given block was
    -- added to the queue. The size of the queue is included.
    AddedPerasCertToQueue PerasRoundNo (Point blk) (Enclosing' Word)
  | -- | The Peras certificate from the given round boosting the given block was
    -- popped from the queue.
    PoppedPerasCertFromQueue PerasRoundNo (Point blk)
  | -- | The Peras certificate from the given round boosting the given block was
    -- too old, ie its slot was older than the current immutable slot (the third
    -- argument).
    IgnorePerasCertTooOld PerasRoundNo (Point blk) (Anchor blk)
  | -- | The Peras certificate from the given round boosts a block on the
    -- current selection.
    PerasCertBoostsCurrentChain PerasRoundNo (Point blk)
  | -- | The Peras certificate from the given round boosts the Genesis point.
    PerasCertBoostsGenesis PerasRoundNo
  | -- | The Peras certificate from the given round boosts a block that we have
    -- not (yet) received.
    PerasCertBoostsBlockNotYetReceived PerasRoundNo (Point blk)
  | -- | Perform chain selection for a block boosted by a Peras certificate.
    ChainSelectionForBoostedBlock PerasRoundNo (Point blk)
  deriving (Generic, Eq, Show)
