{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Types used throughout the implementation: handle, state, environment,
-- types, trace types, etc.
module Ouroboros.Consensus.Storage.ChainDB.Impl.Types (
    ChainDbEnv (..)
  , ChainDbHandle (..)
  , ChainDbState (..)
  , SerialiseDiskConstraints
  , getEnv
  , getEnv1
  , getEnv2
  , getEnvSTM
  , getEnvSTM1
    -- * Exposed internals for testing purposes
  , Internal (..)
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
    -- * Future blocks
  , FutureBlocks
    -- * Blocks to add
  , BlockToAdd (..)
  , ChainSelMessage (..)
  , ChainSelQueue -- opaque
  , addBlockToAdd
  , addReprocessLoEBlocks
  , closeChainSelQueue
  , deleteBlockToAdd
  , getBlocksToAddMaxSlotNo
  , getChainSelMessage
  , memberBlocksToAdd
  , newChainSelQueue
    -- * Trace types
  , SelectionChangedInfo (..)
  , TraceAddBlockEvent (..)
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

import           Control.Monad (join, when)
import           Control.Tracer
import           Data.Foldable (for_)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe.Strict (StrictMaybe (..))
import           Data.Set (Set)
import           Data.Typeable
import           Data.Void (Void)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (OnlyCheckWhnfNamed (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Fragment.Diff (ChainDiff)
import           Ouroboros.Consensus.Fragment.InFuture (CheckInFuture)
import           Ouroboros.Consensus.Ledger.Extended (ExtValidationError)
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ChainDB.API (AddBlockPromise (..),
                     AddBlockResult (..), ChainDbError (..), ChainType,
                     InvalidBlockReason, LoE, StreamFrom, StreamTo,
                     UnknownRange)
import           Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment
                     (InvalidBlockPunishment)
import           Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB (LgrDB,
                     LgrDbSerialiseConstraints)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.LgrDB as LgrDB
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB,
                     ImmutableDbSerialiseConstraints)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Storage.LedgerDB (UpdateLedgerDbTraceEvent)
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Storage.VolatileDB (VolatileDB,
                     VolatileDbSerialiseConstraints)
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolatileDB
import           Ouroboros.Consensus.Util (Fuse)
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.Enclose (Enclosing, Enclosing' (..))
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM (WithFingerprint)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.Block (MaxSlotNo (..))
import           Ouroboros.Network.BlockFetch.ConsensusInterface
                     (ChainSelStarvation (..))

-- | All the serialisation related constraints needed by the ChainDB.
class ( ImmutableDbSerialiseConstraints blk
      , LgrDbSerialiseConstraints blk
      , VolatileDbSerialiseConstraints blk
        -- Needed for Follower
      , EncodeDiskDep (NestedCtxt Header) blk
      ) => SerialiseDiskConstraints blk

-- | A handle to the internal ChainDB state
newtype ChainDbHandle m blk = CDBHandle (StrictTVar m (ChainDbState m blk))

-- | Check if the ChainDB is open, if so, executing the given function on the
-- 'ChainDbEnv', otherwise, throw a 'CloseDBError'.
getEnv :: forall m blk r. (IOLike m, HasCallStack, HasHeader blk)
       => ChainDbHandle m blk
       -> (ChainDbEnv m blk -> m r)
       -> m r
getEnv (CDBHandle varState) f = atomically (readTVar varState) >>= \case
    ChainDbOpen env -> f env
    ChainDbClosed   -> throwIO $ ClosedDBError @blk prettyCallStack

-- | Variant 'of 'getEnv' for functions taking one argument.
getEnv1 :: (IOLike m, HasCallStack, HasHeader blk)
        => ChainDbHandle m blk
        -> (ChainDbEnv m blk -> a -> m r)
        -> a -> m r
getEnv1 h f a = getEnv h (\env -> f env a)

-- | Variant 'of 'getEnv' for functions taking two arguments.
getEnv2 :: (IOLike m, HasCallStack, HasHeader blk)
        => ChainDbHandle m blk
        -> (ChainDbEnv m blk -> a -> b -> m r)
        -> a -> b -> m r
getEnv2 h f a b = getEnv h (\env -> f env a b)


-- | Variant of 'getEnv' that works in 'STM'.
getEnvSTM :: forall m blk r. (IOLike m, HasCallStack, HasHeader blk)
          => ChainDbHandle m blk
          -> (ChainDbEnv m blk -> STM m r)
          -> STM m r
getEnvSTM (CDBHandle varState) f = readTVar varState >>= \case
    ChainDbOpen env -> f env
    ChainDbClosed   -> throwSTM $ ClosedDBError @blk prettyCallStack

-- | Variant of 'getEnv1' that works in 'STM'.
getEnvSTM1 ::
     forall m blk a r. (IOLike m, HasCallStack, HasHeader blk)
  => ChainDbHandle m blk
  -> (ChainDbEnv m blk -> a -> STM m r)
  -> a -> STM m r
getEnvSTM1 (CDBHandle varState) f a = readTVar varState >>= \case
    ChainDbOpen env -> f env a
    ChainDbClosed   -> throwSTM $ ClosedDBError @blk prettyCallStack

data ChainDbState m blk
  = ChainDbOpen   !(ChainDbEnv m blk)
  | ChainDbClosed
  deriving (Generic, NoThunks)

data ChainDbEnv m blk = CDB
  { cdbImmutableDB     :: !(ImmutableDB m blk)
  , cdbVolatileDB      :: !(VolatileDB m blk)
  , cdbLgrDB           :: !(LgrDB m blk)
  , cdbChain           :: !(StrictTVar m (AnchoredFragment (Header blk)))
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
  , cdbTentativeState  :: !(StrictTVar m (TentativeHeaderState blk))
  , cdbTentativeHeader :: !(StrictTVar m (StrictMaybe (Header blk)))
    -- ^ The tentative header, for diffusion pipelining.
    --
    -- INVARIANT: It fits on top of the current chain, and its body is not known
    -- to be invalid, but might turn out to be.
  , cdbIterators       :: !(StrictTVar m (Map IteratorKey (m ())))
    -- ^ The iterators.
    --
    -- This maps the 'IteratorKey's of each open 'Iterator' to a function
    -- that, when called, closes the iterator. This is used when closing the
    -- ChainDB: the open file handles used by iterators can be closed, and the
    -- iterators themselves are closed so that it is impossible to use an
    -- iterator after closing the ChainDB itself.
  , cdbFollowers       :: !(StrictTVar m (Map FollowerKey (FollowerHandle m blk)))
    -- ^ The followers.
    --
    -- A follower is open iff its 'FollowerKey' is this 'Map'.
    --
    -- INVARIANT: the 'followerPoint' of each follower is 'withinFragmentBounds'
    -- of the current chain fragment (retrieved 'cdbGetCurrentChain', not by
    -- reading 'cdbChain' directly).
  , cdbTopLevelConfig  :: !(TopLevelConfig blk)
  , cdbInvalid         :: !(StrictTVar m (WithFingerprint (InvalidBlocks blk)))
    -- ^ See the docstring of 'InvalidBlocks'.
    --
    -- The 'Fingerprint' changes every time a hash is added to the map, but
    -- not when hashes are garbage-collected from the map.
  , cdbNextIteratorKey :: !(StrictTVar m IteratorKey)
  , cdbNextFollowerKey :: !(StrictTVar m FollowerKey)
  , cdbCopyFuse        :: !(Fuse m)
  , cdbChainSelFuse    :: !(Fuse m)
  , cdbTracer          :: !(Tracer m (TraceEvent blk))
  , cdbRegistry        :: !(ResourceRegistry m)
    -- ^ Resource registry that will be used to (re)start the background
    -- threads, see 'cdbBgThreads'.
  , cdbGcDelay         :: !DiffTime
    -- ^ How long to wait between copying a block from the VolatileDB to
    -- ImmutableDB and garbage collecting it from the VolatileDB
  , cdbGcInterval      :: !DiffTime
    -- ^ Minimum time between two garbage collections. Is used to batch
    -- garbage collections.
  , cdbKillBgThreads   :: !(StrictTVar m (m ()))
    -- ^ A handle to kill the background threads.
  , cdbCheckInFuture   :: !(CheckInFuture m blk)
  , cdbChainSelQueue   :: !(ChainSelQueue m blk)
    -- ^ Queue of blocks that still have to be added.
    --
    -- NOTE: the set of blocks in this queue are /not/ disjoint from the set of
    -- blocks in the VolatileDB. When processing the next block in the queue, we
    -- do not remove the block from the queue /until/ it has been added to the
    -- VolatileDB and processed by chain selection. This means the block
    -- currently being added will be both in the queue and the VolatileDB for a
    -- short while.
    --
    -- If we would remove the block from the queue before adding it to the
    -- VolatileDB, then it would be in /neither/ for a short time, and
    -- 'getIsFetched' would incorrectly return 'False'.
  , cdbFutureBlocks    :: !(StrictTVar m (FutureBlocks m blk))
    -- ^ Blocks from the future
    --
    -- Blocks that were added to the ChainDB but that were from the future
    -- according to 'CheckInFuture', without exceeding the clock skew
    -- ('inFutureExceedsClockSkew'). Blocks exceeding the clock skew are
    -- considered to be invalid ('InFutureExceedsClockSkew') and will be added
    -- 'cdbInvalid'.
    --
    -- Whenever a block is added to the ChainDB, we first trigger chain
    -- selection for all the blocks in this map so that blocks no longer from
    -- the future can get adopted. Note that when no blocks are added to the
    -- ChainDB, we will /not/ actively trigger chain selection for the blocks
    -- in this map.
    --
    -- The number of blocks from the future is bounded by the number of
    -- upstream peers multiplied by the max clock skew divided by the slot
    -- length.
  , cdbLoE             :: !(m (LoE (AnchoredFragment (Header blk))))
    -- ^ Configure the Limit on Eagerness. If this is 'LoEEnabled', it contains
    -- an action that returns the LoE fragment, which indicates the latest rollback
    -- point, i.e. we are not allowed to select a chain from which we could not
    -- switch back to a chain containing it. The fragment is usually anchored at
    -- a recent immutable tip; if it does not, it will conservatively be treated
    -- as the empty fragment anchored in the current immutable tip.
  , cdbChainSelStarvation :: !(StrictTVar m ChainSelStarvation)
    -- ^ Information on the last starvation of ChainSel, whether ongoing or
    -- ended recently.
  } deriving (Generic)

-- | We include @blk@ in 'showTypeOf' because it helps resolving type families
-- (but avoid including @m@ because we cannot impose @Typeable m@ as a
-- constraint and still have it work with the simulator)
instance (IOLike m, LedgerSupportsProtocol blk, BlockSupportsDiffusionPipelining blk)
      => NoThunks (ChainDbEnv m blk) where
    showTypeOf _ = "ChainDbEnv m " ++ show (typeRep (Proxy @blk))

{-------------------------------------------------------------------------------
  Exposed internals for testing purposes
-------------------------------------------------------------------------------}

data Internal m blk = Internal
  { intCopyToImmutableDB     :: m (WithOrigin SlotNo)
    -- ^ Copy the blocks older than @k@ from to the VolatileDB to the
    -- ImmutableDB and update the in-memory chain fragment correspondingly.
    --
    -- The 'SlotNo' of the tip of the ImmutableDB after copying the blocks is
    -- returned. This can be used for a garbage collection on the VolatileDB.
  , intGarbageCollect        :: SlotNo -> m ()
    -- ^ Perform garbage collection for blocks <= the given 'SlotNo'.
  , intUpdateLedgerSnapshots :: m ()
    -- ^ Write a new LedgerDB snapshot to disk and remove the oldest one(s).
  , intAddBlockRunner        :: m Void
    -- ^ Start the loop that adds blocks to the ChainDB retrieved from the
    -- queue populated by 'ChainDB.addBlock'. Execute this loop in a separate
    -- thread.
  , intKillBgThreads         :: StrictTVar m (m ())
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
  deriving stock   (Show)
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
  deriving stock   (Show)
  deriving newtype (Eq, Ord, Enum, NoThunks)

-- | Internal handle to a 'Follower' without an explicit @b@ (@blk@, @'Header'
-- blk@, etc.) parameter so 'Follower's with different' @b@s can be stored
-- together in 'cdbFollowers'.
data FollowerHandle m blk = FollowerHandle
  { fhChainType  :: ChainType
    -- ^ Whether we follow the tentative chain.
  , fhSwitchFork :: Point blk -> Set (Point blk) -> STM m ()
    -- ^ When we have switched to a fork, all open 'Follower's must be notified.
  , fhClose      :: m ()
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
  = FollowerInit
    -- ^ The 'Follower' is in its initial state. Its 'FollowerRollState' is
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
  | FollowerInImmutableDB
      !(FollowerRollState blk)
      !(ImmutableDB.Iterator m blk (Point blk, b))
    -- ^ The 'Follower' is reading from the ImmutableDB.
    --
    -- Note that the iterator includes 'Point blk' in addition to @b@, as it
    -- is needed to keep track of where the iterator is.
    --
    -- INVARIANT: for all @FollowerInImmutableDB rollState immIt@: the predecessor
    -- of the next block streamed by @immIt@ must be the block identified by
    -- @followerRollStatePoint rollState@. In other words: the iterator is
    -- positioned /on/ @followerRollStatePoint rollState@.
  | FollowerInMem !(FollowerRollState blk)
    -- ^ The 'Follower' is reading from the in-memory current chain fragment.
  deriving (Generic, NoThunks)

-- | Similar to 'Ouroboros.Network.Mock.ProducerState.FollowerState'.
data FollowerRollState blk
  = RollBackTo      !(Point blk)
    -- ^ We don't know at which point the user is, but the next message we'll
    -- send is to roll back to this point.
  | RollForwardFrom !(Point blk)
    -- ^ We know that the follower is at this point and the next message we'll
    -- send is to roll forward to the point /after/ this point on our chain.
  deriving (Eq, Show, Generic, NoThunks)

-- | Get the point the 'FollowerRollState' should roll back to or roll forward
-- from.
followerRollStatePoint :: FollowerRollState blk -> Point blk
followerRollStatePoint (RollBackTo      pt) = pt
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
  { invalidBlockReason :: !(InvalidBlockReason blk)
  , invalidBlockSlotNo :: !SlotNo
  } deriving (Eq, Show, Generic, NoThunks)

{-------------------------------------------------------------------------------
  Future blocks
-------------------------------------------------------------------------------}

-- | Blocks from the future for which we still need to trigger chain
-- selection.
--
-- See 'cdbFutureBlocks' for more info.
type FutureBlocks m blk = Map (HeaderHash blk) (Header blk, InvalidBlockPunishment m)

{-------------------------------------------------------------------------------
  Blocks to add
-------------------------------------------------------------------------------}

-- | FIFO queue used to add blocks asynchronously to the ChainDB. Blocks are
-- read from this queue by a background thread, which processes the blocks
-- synchronously.
data ChainSelQueue m blk = ChainSelQueue {
    -- TODO use a better data structure, e.g., a heap from the @heaps@
    -- package. Wish list:
    -- + O(1) pop min value
    -- + O(log n) insert
    -- + O(n) get all
    -- + Bounded in size
    --
    -- TODO join consecutive blocks into a fragment that can be added at
    -- once.
    varChainSelQueue :: !(StrictTVar m (Map (RealPoint blk) (BlockToAdd m blk)))
  , chainSelQueueCapacity :: !Word
  , varChainSelReprocessLoEBlocks :: !(StrictTVar m Bool)
  }
  deriving (NoThunks) via OnlyCheckWhnfNamed "ChainSelQueue" (ChainSelQueue m blk)

-- | Entry in the 'ChainSelQueue' queue: a block together with the 'TMVar's used
-- to implement 'AddBlockPromise'.
data BlockToAdd m blk = BlockToAdd
  { blockPunish           :: !(InvalidBlockPunishment m)
    -- ^ Executed immediately upon determining this block or one from its prefix
    -- is invalid.
  , blockToAdd            :: !blk
  , varBlockWrittenToDisk :: !(StrictTMVar m Bool)
    -- ^ Used for the 'blockWrittenToDisk' field of 'AddBlockPromise'.
  , varBlockProcessed     :: !(StrictTMVar m (AddBlockResult blk))
    -- ^ Used for the 'blockProcessed' field of 'AddBlockPromise'.
  }
  deriving NoThunks via OnlyCheckWhnfNamed "BlockToAdd" (BlockToAdd m blk)

-- | Different async tasks for triggering ChainSel
data ChainSelMessage m blk
  -- | Add a new block
  = ChainSelAddBlock !(BlockToAdd m blk)
  -- | Reprocess blocks that have been postponed by the LoE
  | ChainSelReprocessLoEBlocks

-- | Create a new 'ChainSelQueue' with the given size.
newChainSelQueue :: (IOLike m, StandardHash blk, Typeable blk) => Word -> m (ChainSelQueue m blk)
newChainSelQueue chainSelQueueCapacity = do
  varChainSelQueue <- newTVarIO mempty
  varChainSelReprocessLoEBlocks <- newTVarIO False
  return $ ChainSelQueue {varChainSelQueue, chainSelQueueCapacity, varChainSelReprocessLoEBlocks}

-- | Add a block to the 'ChainSelQueue' queue. Can block when the queue is full.
addBlockToAdd ::
     (IOLike m, HasHeader blk)
  => Tracer m (TraceAddBlockEvent blk)
  -> ChainSelQueue m blk
  -> InvalidBlockPunishment m
  -> blk
  -> m (AddBlockPromise m blk)
addBlockToAdd tracer (ChainSelQueue {varChainSelQueue, chainSelQueueCapacity}) punish blk = do
    varBlockWrittenToDisk <- newEmptyTMVarIO
    varBlockProcessed     <- newEmptyTMVarIO
    let !toAdd = BlockToAdd
          { blockPunish           = punish
          , blockToAdd            = blk
          , varBlockWrittenToDisk
          , varBlockProcessed
          }
    traceWith tracer $ AddedBlockToQueue (blockRealPoint blk) RisingEdge
    queueSize <- atomically $ do
      chainSelQueue <- readTVar varChainSelQueue
      let chainSelQueue'    = Map.insert (blockRealPoint blk) toAdd chainSelQueue
          chainSelQueueSize = Map.size chainSelQueue'
      check (fromIntegral chainSelQueueSize <= chainSelQueueCapacity)
      writeTVar varChainSelQueue chainSelQueue'
      return chainSelQueueSize
    traceWith tracer $
      AddedBlockToQueue (blockRealPoint blk) (FallingEdgeWith (fromIntegral queueSize))
    return AddBlockPromise
      { blockWrittenToDisk      = readTMVar varBlockWrittenToDisk
      , blockProcessed          = readTMVar varBlockProcessed
      }

-- | Try to add blocks again that were postponed due to the LoE.
addReprocessLoEBlocks
  :: IOLike m
  => Tracer m (TraceAddBlockEvent blk)
  -> ChainSelQueue m blk
  -> m ()
addReprocessLoEBlocks tracer (ChainSelQueue {varChainSelReprocessLoEBlocks}) = do
  traceWith tracer $ AddedReprocessLoEBlocksToQueue
  atomically $ writeTVar varChainSelReprocessLoEBlocks True

-- | Get the oldest message from the 'ChainSelQueue' queue. Can block when the
-- queue is empty; in that case, reports the starvation (and its end) to the
-- callback.
getChainSelMessage
  :: forall m blk. (HasHeader blk, IOLike m)
  => Tracer m (TraceChainSelStarvationEvent blk)
  -> StrictTVar m ChainSelStarvation
  -> ChainSelQueue m blk
  -> m (ChainSelMessage m blk)
getChainSelMessage starvationTracer starvationVar queue = go
  where
    go = join $ atomically $
      readTVar varChainSelReprocessLoEBlocks >>= \case
        True -> do
          writeTVar varChainSelReprocessLoEBlocks False
          pure $ pure ChainSelReprocessLoEBlocks
        False -> do
          chainSelQueue <- readTVar varChainSelQueue
          case Map.minView chainSelQueue of
            Just (blockToAdd, chainSelQueue') -> do
              writeTVar varChainSelQueue chainSelQueue'
              pure $ do
                terminateStarvationMeasure blockToAdd
                pure $ ChainSelAddBlock blockToAdd
            Nothing -> pure $ do
              startStarvationMeasure
              blockUntilMoreWork
              go

    ChainSelQueue {varChainSelQueue, varChainSelReprocessLoEBlocks} = queue

    -- Wait until we either need to reprocess blocks due to the LoE, or until a
    -- new block arrives.
    blockUntilMoreWork :: m ()
    blockUntilMoreWork = atomically $ do
        reprocessLoEBlocks <- readTVar varChainSelReprocessLoEBlocks
        chainSelQueue      <- readTVar varChainSelQueue
        check $ reprocessLoEBlocks || not (Map.null chainSelQueue)

    startStarvationMeasure = do
      prevStarvation <- atomically $ swapTVar starvationVar ChainSelStarvationOngoing
      when (prevStarvation /= ChainSelStarvationOngoing) $
        traceWith starvationTracer . ChainSelStarvationStarted =<< getMonotonicTime

    terminateStarvationMeasure BlockToAdd{blockToAdd=block} = do
      prevStarvation <- readTVarIO starvationVar
      when (prevStarvation == ChainSelStarvationOngoing) $ do
        tf <- getMonotonicTime
        traceWith starvationTracer (ChainSelStarvationEnded tf $ blockRealPoint block)
        atomically $ writeTVar starvationVar (ChainSelStarvationEndedAt tf)

-- | Flush the 'ChainSelQueue' queue and notify the waiting threads.
--
-- REVIEW: What about all the threads that are waiting to write in the queue and
-- will write after the flush?!
closeChainSelQueue :: IOLike m => ChainSelQueue m blk -> STM m ()
closeChainSelQueue ChainSelQueue {varChainSelQueue} = do
  chainSelQueue <- swapTVar varChainSelQueue Map.empty
  for_ chainSelQueue $ \BlockToAdd {varBlockProcessed} ->
    putTMVar varBlockProcessed $ FailedToAddBlock "Queue flushed"

-- | Delete the given 'BlockToAdd' from the 'ChainSelQueue'.
--
-- PRECONDITION: the given 'BlockToAdd' is in 'ChainSelQueue'.
deleteBlockToAdd ::
     (IOLike m, HasHeader blk)
  => BlockToAdd m blk
  -> ChainSelQueue m blk
  -> m ()
deleteBlockToAdd (BlockToAdd _ blk _ _) (ChainSelQueue {varChainSelQueue}) =
  atomically $ modifyTVar varChainSelQueue $ Map.delete (blockRealPoint blk)

-- | Return a function to test the membership for the given 'BlocksToAdd'.
memberBlocksToAdd ::
     (IOLike m, HasHeader blk)
  => ChainSelQueue m blk
  -> STM m (RealPoint blk -> Bool)
memberBlocksToAdd (ChainSelQueue {varChainSelQueue}) =
    flip Map.member <$> readTVar varChainSelQueue

getBlocksToAddMaxSlotNo ::
     IOLike m
  => ChainSelQueue m blk
  -> STM m MaxSlotNo
getBlocksToAddMaxSlotNo (ChainSelQueue {varChainSelQueue}) = aux <$> readTVar varChainSelQueue
  where
    -- | The 'Ord' instance of 'RealPoint' orders by 'SlotNo' first, so the
    -- maximal key of the map has the greatest 'SlotNo'.
    aux :: Map (RealPoint blk) (BlockToAdd m blk) -> MaxSlotNo
    aux queue = case Map.lookupMax queue of
        Nothing                 -> NoMaxSlotNo
        Just (RealPoint s _, _) -> MaxSlotNo s

{-------------------------------------------------------------------------------
  Trace types
-------------------------------------------------------------------------------}

-- | Trace type for the various events of the ChainDB.
data TraceEvent blk
  = TraceAddBlockEvent          (TraceAddBlockEvent           blk)
  | TraceFollowerEvent          (TraceFollowerEvent           blk)
  | TraceCopyToImmutableDBEvent (TraceCopyToImmutableDBEvent  blk)
  | TraceGCEvent                (TraceGCEvent                 blk)
  | TraceInitChainSelEvent      (TraceInitChainSelEvent       blk)
  | TraceOpenEvent              (TraceOpenEvent               blk)
  | TraceIteratorEvent          (TraceIteratorEvent           blk)
  | TraceSnapshotEvent          (LgrDB.TraceSnapshotEvent     blk)
  | TraceLedgerReplayEvent      (LgrDB.TraceReplayEvent       blk)
  | TraceImmutableDBEvent       (ImmutableDB.TraceEvent       blk)
  | TraceVolatileDBEvent        (VolatileDB.TraceEvent        blk)
  | TraceChainSelStarvationEvent(TraceChainSelStarvationEvent blk)
  deriving (Generic)


deriving instance
  ( Eq (Header blk)
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  ) => Eq (TraceEvent blk)
deriving instance
  ( Show (Header blk)
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  ) => Show (TraceEvent blk)

data TraceOpenEvent blk =
    -- | The ChainDB started the process of opening.
    StartedOpeningDB
    -- | The ChainDB was opened.
  | OpenedDB
      (Point blk)  -- ^ Immutable tip
      (Point blk)  -- ^ Tip of the current chain

    -- | The ChainDB was closed.
  | ClosedDB
      (Point blk)  -- ^ Immutable tip
      (Point blk)  -- ^ Tip of the current chain

    -- | The ImmutableDB started the process of opening.
  | StartedOpeningImmutableDB

    -- | The ImmutableDB was opened.
  | OpenedImmutableDB
      (Point blk)          -- ^ Immutable tip
      ImmutableDB.ChunkNo  -- ^ Chunk number of the immutable tip

    -- | The VolatileDB started opening.
  | StartedOpeningVolatileDB

    -- | The VolatileDB was opened, with the highest seen slot number for any
    -- block currently in the DB.
  | OpenedVolatileDB MaxSlotNo

    -- | The LedgerDB started opening.
  | StartedOpeningLgrDB

    -- | The LedgerDB was opened.
  | OpenedLgrDB
  deriving (Generic, Eq, Show)

-- | Information on having changed our selection to a chain with a (necessarily)
-- new tip.
--
-- NOTE: the fields of this record are intentionally lazy to prevent the
-- forcing of this information in case it doesn't have to be traced. However,
-- this means that the tracer processing this message /must not/ hold on to
-- it, otherwise it leaks memory.
data SelectionChangedInfo blk = SelectionChangedInfo {
      newTipPoint       :: RealPoint blk
      -- ^ The new tip of the current chain.
    , newTipEpoch       :: EpochNo
      -- ^ The epoch of the new tip.
    , newTipSlotInEpoch :: Word64
      -- ^ The slot in the epoch, i.e., the relative slot number, of the new
      -- tip.
    , newTipTrigger     :: RealPoint blk
      -- ^ The new tip of the current chain ('newTipPoint') is the result of
      -- performing chain selection for a /trigger/ block ('newTipTrigger').
      -- In most cases, we add a new block to the tip of the current chain, in
      -- which case the new tip /is/ the trigger block.
      --
      -- However, this is not always the case. For example, with our current
      -- chain being A and having a disconnected C lying around, adding B will
      -- result in A -> B -> C as the new chain. The trigger B /= the new tip
      -- C.
    , newTipSelectView  :: SelectView (BlockProtocol blk)
      -- ^ The 'SelectView' of the new tip. It is guaranteed that
      --
      -- > Just newTipSelectView > oldTipSelectView
      -- True
    , oldTipSelectView  :: Maybe (SelectView (BlockProtocol blk))
      -- ^ The 'SelectView' of the old, previous tip. This can be 'Nothing' when
      -- the previous chain/tip was Genesis.
    }
  deriving (Generic)

deriving stock instance (Show (SelectView (BlockProtocol blk)), StandardHash blk) => Show (SelectionChangedInfo blk)
deriving stock instance (Eq   (SelectView (BlockProtocol blk)), StandardHash blk) => Eq   (SelectionChangedInfo blk)

-- | Trace type for the various events that occur when adding a block.
data TraceAddBlockEvent blk =
    -- | A block with a 'BlockNo' more than @k@ back than the current tip was
    -- ignored.
    IgnoreBlockOlderThanK (RealPoint blk)

    -- | A block that is already in the Volatile DB was ignored.
  | IgnoreBlockAlreadyInVolatileDB (RealPoint blk)

    -- | A block that is know to be invalid was ignored.
  | IgnoreInvalidBlock (RealPoint blk) (InvalidBlockReason blk)

    -- | The block was added to the queue and will be added to the ChainDB by
    -- the background thread. The size of the queue is included.
  | AddedBlockToQueue (RealPoint blk) (Enclosing' Word)

    -- | The block popped from the queue and will imminently be added to the
    -- ChainDB.
  | PoppedBlockFromQueue (Enclosing' (RealPoint blk))

    -- | A message was added to the queue that requests that ChainSel reprocess
    -- blocks that were postponed by the LoE.
  | AddedReprocessLoEBlocksToQueue

    -- | ChainSel will reprocess blocks that were postponed by the LoE.
  | PoppedReprocessLoEBlocksFromQueue

    -- | The block is from the future, i.e., its slot number is greater than
    -- the current slot (the second argument).
  | BlockInTheFuture (RealPoint blk) SlotNo

    -- | A block was added to the Volatile DB
  | AddedBlockToVolatileDB (RealPoint blk) BlockNo IsEBB Enclosing

    -- | The block fits onto the current chain, we'll try to use it to extend
    -- our chain.
  | TryAddToCurrentChain (RealPoint blk)

    -- | The block fits onto some fork, we'll try to switch to that fork (if
    -- it is preferable to our chain).
  | TrySwitchToAFork (RealPoint blk) (ChainDiff (HeaderFields blk))

    -- | The block doesn't fit onto any other block, so we store it and ignore
    -- it.
  | StoreButDontChange (RealPoint blk)

    -- | Debugging information about chain selection and LoE
  | ChainSelectionLoEDebug (AnchoredFragment (Header blk)) (LoE (AnchoredFragment (Header blk)))

    -- | The new block fits onto the current chain (first
    -- fragment) and we have successfully used it to extend our (new) current
    -- chain (second fragment).
  | AddedToCurrentChain
      [LedgerEvent blk]
      (SelectionChangedInfo blk)
      (AnchoredFragment (Header blk))
      (AnchoredFragment (Header blk))

    -- | The new block fits onto some fork and we have switched to that fork
    -- (second fragment), as it is preferable to our (previous) current chain
    -- (first fragment).
  | SwitchedToAFork
      [LedgerEvent blk]
      (SelectionChangedInfo blk)
      (AnchoredFragment (Header blk))
      (AnchoredFragment (Header blk))

    -- | An event traced during validating performed while adding a block.
  | AddBlockValidation (TraceValidationEvent blk)

    -- | Run chain selection for a block that was previously from the future.
    -- This is done for all blocks from the future each time a new block is
    -- added.
  | ChainSelectionForFutureBlock (RealPoint blk)

    -- | The tentative header (in the context of diffusion pipelining) has been
    -- updated.
  | PipeliningEvent (TracePipeliningEvent blk)

    -- | Herald of 'AddedToCurrentChain' or 'SwitchedToAFork'. Lists the tip of
    -- the new chain.
  | ChangingSelection (Point blk)

  deriving (Generic)

deriving instance
  ( Eq (Header blk)
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  ) => Eq (TraceAddBlockEvent blk)
deriving instance
  ( Show (Header blk)
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  ) => Show (TraceAddBlockEvent blk)

data TraceValidationEvent blk =
    -- | A point was found to be invalid.
    InvalidBlock
      (ExtValidationError blk)
      (RealPoint blk)

    -- | A candidate chain was valid.
  | ValidCandidate (AnchoredFragment (Header blk))

    -- | Candidate contains headers from the future which do no exceed the
    -- clock skew.
  | CandidateContainsFutureBlocks
      (AnchoredFragment (Header blk))
      -- ^ Candidate chain containing headers from the future
      [Header blk]
      -- ^ Headers from the future, not exceeding clock skew

    -- | Candidate contains headers from the future which exceed the
    -- clock skew, making them invalid.
  | CandidateContainsFutureBlocksExceedingClockSkew
      (AnchoredFragment (Header blk))
      -- ^ Candidate chain containing headers from the future
      [Header blk]
      -- ^ Headers from the future, exceeding clock skew
  | UpdateLedgerDbTraceEvent (UpdateLedgerDbTraceEvent blk)
  deriving (Generic)

deriving instance
  ( Eq (Header             blk)
  , LedgerSupportsProtocol blk
  ) => Eq (TraceValidationEvent blk)
deriving instance
  ( Show (Header           blk)
  , LedgerSupportsProtocol blk
  ) => Show (TraceValidationEvent blk)

data TracePipeliningEvent blk =
    -- | A new tentative header got set.
    SetTentativeHeader (Header blk) Enclosing
    -- | The body of tentative block turned out to be invalid.
  | TrapTentativeHeader (Header blk)
    -- | We selected a new (better) chain, which cleared the previous tentative
    -- header.
  | OutdatedTentativeHeader (Header blk)

deriving stock instance Eq   (Header blk) => Eq   (TracePipeliningEvent blk)
deriving stock instance Show (Header blk) => Show (TracePipeliningEvent blk)

data TraceInitChainSelEvent blk =
    StartedInitChainSelection
    -- ^ An event traced when inital chain selection has started during the
    -- initialization of ChainDB
  | InitialChainSelected
    -- ^ An event traced when inital chain has been selected
  | InitChainSelValidation (TraceValidationEvent blk)
    -- ^ An event traced during validation performed while performing initial
    -- chain selection.
  deriving (Generic)

deriving instance
  ( Eq (Header             blk)
  , LedgerSupportsProtocol blk
  ) => Eq (TraceInitChainSelEvent blk)
deriving instance
  ( Show (Header           blk)
  , LedgerSupportsProtocol blk
  ) => Show (TraceInitChainSelEvent blk)


data TraceFollowerEvent blk =
    -- | A new follower was created.
    NewFollower

    -- | The follower was in the 'FollowerInMem' state but its point is no longer on
    -- the in-memory chain fragment, so it has to switch to the
    -- 'FollowerInImmutableDB' state.
  | FollowerNoLongerInMem (FollowerRollState blk)

    -- | The follower was in the 'FollowerInImmutableDB' state and is switched to
    -- the 'FollowerInMem' state.
  | FollowerSwitchToMem
      (Point blk)          -- ^ Point at which the follower is
      (WithOrigin SlotNo)  -- ^ Slot number at the tip of the ImmutableDB

    -- | The follower is in the 'FollowerInImmutableDB' state but the iterator is
    -- exhausted while the ImmutableDB has grown, so we open a new iterator to
    -- stream these blocks too.
  | FollowerNewImmIterator
      (Point blk)          -- ^ Point at which the follower is
      (WithOrigin SlotNo)  -- ^ Slot number at the tip of the ImmutableDB
  deriving (Generic, Eq, Show)


data TraceCopyToImmutableDBEvent blk
  = CopiedBlockToImmutableDB (Point blk)
    -- ^ A block was successfully copied to the ImmutableDB.
  | NoBlocksToCopyToImmutableDB
    -- ^ There are no block to copy to the ImmutableDB.
  deriving (Generic, Eq, Show)

data TraceGCEvent blk
  = ScheduledGC SlotNo Time
    -- ^ A garbage collection for the given 'SlotNo' was scheduled to happen
    -- at the given time.
  | PerformedGC SlotNo
    -- ^ A garbage collection for the given 'SlotNo' was performed.
  deriving (Generic, Eq, Show)

data TraceIteratorEvent blk
    -- | An unknown range was requested, see 'UnknownRange'.
  = UnknownRangeRequested (UnknownRange blk)

    -- | Stream only from the VolatileDB.
  | StreamFromVolatileDB
      (StreamFrom blk)
      (StreamTo   blk)
      [RealPoint  blk]

    -- | Stream only from the ImmutableDB.
  | StreamFromImmutableDB
      (StreamFrom blk)
      (StreamTo   blk)

    -- | Stream from both the VolatileDB and the ImmutableDB.
  | StreamFromBoth
      (StreamFrom blk)
      (StreamTo   blk)
      [RealPoint  blk]

    -- | A block is no longer in the VolatileDB because it has been garbage
    -- collected. It might now be in the ImmutableDB if it was part of the
    -- current chain.
  | BlockMissingFromVolatileDB (RealPoint blk)

    -- | A block that has been garbage collected from the VolatileDB is now
    -- found and streamed from the ImmutableDB.
  | BlockWasCopiedToImmutableDB (RealPoint blk)

    -- | A block is no longer in the VolatileDB and isn't in the ImmutableDB
    -- either; it wasn't part of the current chain.
  | BlockGCedFromVolatileDB    (RealPoint blk)

    -- | We have streamed one or more blocks from the ImmutableDB that were part
    -- of the VolatileDB when initialising the iterator. Now, we have to look
    -- back in the VolatileDB again because the ImmutableDB doesn't have the
    -- next block we're looking for.
  | SwitchBackToVolatileDB
  deriving (Generic, Eq, Show)

data TraceChainSelStarvationEvent blk
  = ChainSelStarvationStarted Time
  | ChainSelStarvationEnded Time (RealPoint blk)
  deriving (Generic, Eq, Show)
