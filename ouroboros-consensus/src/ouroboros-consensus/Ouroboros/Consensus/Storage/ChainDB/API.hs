{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.ChainDB.API
  ( -- * Main ChainDB API
    ChainDB (..)
  , getCurrentTip
  , getTipBlockNo

    -- * Adding a block
  , AddBlockPromise (..)
  , AddBlockResult (..)
  , addBlock
  , addBlockWaitWrittenToDisk
  , addBlock_

    -- * Adding a Peras certificate
  , AddPerasCertPromise (..)
  , addPerasCertSync

    -- * Trigger chain selection
  , ChainSelectionPromise (..)
  , triggerChainSelection
  , triggerChainSelectionAsync

    -- * Serialised block/header with its point
  , WithPoint (..)
  , getPoint
  , getSerialisedBlockWithPoint
  , getSerialisedHeaderWithPoint

    -- * BlockComponent
  , BlockComponent (..)

    -- * Support for tests
  , fromChain
  , toChain

    -- * Iterator API
  , Iterator (..)
  , IteratorResult (..)
  , StreamFrom (..)
  , StreamTo (..)
  , UnknownRange (..)
  , emptyIterator
  , streamAll
  , streamFrom
  , traverseIterator
  , validBounds

    -- * Followers
  , ChainType (..)
  , Follower (..)
  , traverseFollower

    -- * Recovery
  , ChainDbFailure (..)
  , IsEBB (..)

    -- * Exceptions
  , ChainDbError (..)

    -- * Genesis
  , GetLoEFragment
  , LoE (..)
  ) where

import Control.Monad (void)
import Control.ResourceRegistry
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HeaderStateHistory
  ( HeaderStateHistory (..)
  )
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment
import Ouroboros.Consensus.Storage.Common
import Ouroboros.Consensus.Storage.LedgerDB
  ( GetForkerError
  , ReadOnlyForker'
  , Statistics
  )
import Ouroboros.Consensus.Storage.PerasCertDB.API (PerasCertSnapshot)
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.STM (WithFingerprint)
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block
  ( ChainUpdate
  , MaxSlotNo
  , Serialised (..)
  )
import qualified Ouroboros.Network.Block as Network
import Ouroboros.Network.BlockFetch.ConsensusInterface
  ( ChainSelStarvation (..)
  )
import Ouroboros.Network.Mock.Chain (Chain (..))
import qualified Ouroboros.Network.Mock.Chain as Chain
import Ouroboros.Network.Protocol.LocalStateQuery.Type
import System.FS.API.Types (FsError)

-- | The chain database
--
-- The chain database provides a unified interface on top of:
--
-- * The ImmutableDB, storing the part of the chain that can't roll back.
-- * The VolatileDB, storing the blocks near the tip of the chain, possibly in
--   multiple competing forks.
-- * The LedgerDB, storing snapshots of the ledger state for blocks in the
--   ImmutableDB (and in-memory snapshots for the rest).
--
-- In addition to providing a unifying interface on top of these disparate
-- components, the main responsibilities that the ChainDB itself has are:
--
-- * Chain selection (on initialization and whenever a block is added)
-- * Trigger full recovery whenever we detect disk failure in any component
-- * Provide iterators across fixed fragments of the current chain
-- * Provide followers that track the status of the current chain
--
-- The ChainDB instantiates all the various type parameters of these databases
-- to conform to the unified interface we provide here.
data ChainDB m blk = ChainDB
  { addBlockAsync :: InvalidBlockPunishment m -> blk -> m (AddBlockPromise m blk)
  -- ^ Add a block to the heap of blocks
  --
  -- We do /not/ assume that the block is valid (under the legder rules);
  -- it is the responsibility of the Chain DB itself to only select chains
  -- that are valid.
  --
  -- Conversely, the caller cannot assume that the new block will be added
  -- to the current chain; even if the block is valid, it will not become
  -- part of the chain if there are other chains available that are
  -- preferred by the consensus algorithm (typically, longer chains).
  --
  -- This function typically returns immediately, yielding a
  -- 'AddBlockPromise' which can be used to wait for the result. You can
  -- use 'addBlock' to add the block synchronously.
  --
  -- NOTE: back pressure can be applied when overloaded.
  --
  -- PRECONDITON: the block to be added must not be from the future.
  --
  -- The current code ensures that the two sources of blocks
  -- ('ChainSync' and forging) do not allow blocks from the future,
  -- however this is not guaranteed when during initialization if the
  -- VolatileDB contains blocks from the future. See:
  -- https://github.com/IntersectMBO/ouroboros-consensus/blob/main/docs/website/contents/for-developers/HandlingBlocksFromTheFuture.md#handling-blocks-from-the-future
  , chainSelAsync :: m (ChainSelectionPromise m)
  -- ^ Trigger reprocessing of blocks postponed by the LoE.
  , getCurrentChain :: STM m (AnchoredFragment (Header blk))
  -- ^ Get the current chain fragment
  --
  -- Suppose the current chain is
  --
  -- > a -> b -> c -> d -> e -> f
  --
  -- and suppose @k = 2@; this means that the most distant fork we can
  -- switch to is something like
  --
  -- > a -> b -> c -> d -> e' -> f'
  --
  -- The fragment we return will be @[e, f]@, anchored at @d@. In other
  -- words, the length of the fragment will under normal circumstances
  -- be exactly @k@ blocks long. It may be shorter if
  --
  -- * We are near genesis
  --   The anchor will be the genesis point
  --   (which does not correspond to an actual block)
  --
  -- * The volatile DB suffered some data loss
  --   Typically (but not necessarily) the volatile DB will not be empty
  --   and the anchor will be pointing to the tip of the immutable DB.
  --
  -- POSTCONDITION: The Chain DB will be able to switch to any fork starting
  -- from the anchor of the returned fragment or any subsequent block
  -- (provided the new fork is at least of the same length as the old).
  --
  -- NOTE: A direct consequence of this guarantee is that the anchor of the
  -- fragment will move as the chain grows.
  , getCurrentChainWithTime ::
      STM m (AnchoredFragment (HeaderWithTime blk))
  -- ^ Exact same as 'getCurrentChain', except each header is annotated
  -- with the 'RelativeTime' of the onset of its slot (translated according
  -- to the chain it is on)
  --
  -- INVARIANT @'hwtHeader' <$> 'getCurrentChainWithTime' = 'getCurrentChain'@
  , getCurrentLedger :: STM m (ExtLedgerState blk EmptyMK)
  -- ^ Get current ledger
  , getImmutableLedger :: STM m (ExtLedgerState blk EmptyMK)
  -- ^ Get the immutable ledger, i.e., typically @k@ blocks back.
  , getPastLedger :: Point blk -> STM m (Maybe (ExtLedgerState blk EmptyMK))
  -- ^ Get the ledger for the given point.
  --
  -- When the given point is not among the last @k@ blocks of the current
  -- chain (i.e., older than @k@ or not on the current chain), 'Nothing' is
  -- returned.
  , getHeaderStateHistory :: STM m (HeaderStateHistory blk)
  -- ^ Get a 'HeaderStateHistory' populated with the 'HeaderState's of the
  -- last @k@ blocks of the current chain.
  , getReadOnlyForkerAtPoint ::
      ResourceRegistry m ->
      Target (Point blk) ->
      m (Either GetForkerError (ReadOnlyForker' m blk))
  -- ^ Acquire a read-only forker at a specific point if that point exists
  -- on the db.
  --
  -- Note that the forker should be closed by the caller of this function.
  --
  -- The forker is read-only becase a read-write forker could be used to
  -- change the internal state of the LedgerDB.
  , getTipBlock :: m (Maybe blk)
  -- ^ Get block at the tip of the chain, if one exists
  --
  -- Returns 'Nothing' if the database is empty.
  , getTipHeader :: m (Maybe (Header blk))
  -- ^ Get header at the tip of the chain
  --
  -- NOTE: Calling 'getTipHeader' is cheaper than 'getTipBlock' and then
  -- extracting the header: most of the time the header at the tip is
  -- actually in memory, whereas the block never is.
  --
  -- Returns 'Nothing' if the database is empty.
  , getTipPoint :: STM m (Point blk)
  -- ^ Get point of the tip of the chain
  --
  -- Will return 'genesisPoint' if the database is empty; if the
  -- current chain fragment is empty due to data loss in the volatile DB,
  -- 'getTipPoint' will return the tip of the immutable DB.
  , getBlockComponent ::
      forall b.
      BlockComponent blk b ->
      RealPoint blk ->
      m (Maybe b)
  -- ^ Get the given component(s) of the block at the specified point. If
  -- there is no block at the given point, 'Nothing' is returned.
  , getIsFetched :: STM m (Point blk -> Bool)
  -- ^ Return membership check function for recent blocks. This includes
  -- blocks in the VolatileDB and blocks that are currently being processed
  -- or are waiting in a queue to be processed.
  --
  -- This check is only reliable for blocks up to @k@ away from the tip.
  -- For blocks older than that the results should be regarded as
  -- non-deterministic.
  , getIsValid :: STM m (RealPoint blk -> Maybe Bool)
  -- ^ Return a function that tells whether a block is known to be valid
  -- or invalid.
  --
  -- The function will return:
  --
  -- * @Just True@: for blocks in the volatile DB that have been validated
  --   and were found to be valid. All blocks in the current chain
  --   fragment (i.e., 'getCurrentChain') are valid.
  --
  -- * @Just False@: for blocks in the volatile DB that have been
  --   validated and were found to be invalid.
  --
  -- * @Nothing@: for blocks not or no longer in the volatile DB, whether
  --   they are valid or not, including blocks in the immutable DB. Also
  --   for blocks in the volatile DB that haven't been validated (yet),
  --   e.g., because they are disconnected from the current chain or they
  --   are part of a shorter fork.
  , getMaxSlotNo :: STM m MaxSlotNo
  -- ^ Get the highest slot number stored in the ChainDB (this includes
  -- blocks that are waiting in the background queue to be processed).
  --
  -- Note that the corresponding block doesn't have to be part of the
  -- current chain, it could be part of some fork, or even be a
  -- disconnected block.
  , stream ::
      forall b.
      ResourceRegistry m ->
      BlockComponent blk b ->
      StreamFrom blk ->
      StreamTo blk ->
      m (Either (UnknownRange blk) (Iterator m blk b))
  -- ^ Stream blocks
  --
  -- Streaming is not restricted to the current fork, but there must be an
  -- unbroken path from the starting point to the end point /at the time
  -- of initialization/ of the iterator. Once the iterator has been
  -- initialized, it will not be affected by subsequent calls to
  -- 'addBlock'. To track the current chain, use a 'Follower' instead.
  --
  -- Streaming blocks older than @k@ is permitted, but only when they are
  -- part of the current fork (at the time of initialization). Streaming a
  -- fork that forks off more than @k@ blocks in the past is not permitted
  -- and an 'UnknownRange' error will be returned in that case.
  --
  -- The iterator /does/ have a limited lifetime, however. The chain DB
  -- internally partitions the chain into an " immutable " part and a
  -- " volatile " part, moving blocks from the volatile DB to the immutable
  -- DB when they become more than @k@ deep into the chain. When a block
  -- with slot number @n@ is added to the immutble DB, a time delay @t@
  -- kicks in; after that time delay expires, all blocks older than @n@ may
  -- be removed from the volatile DB, /including any blocks that happen to
  -- live on other forks/ (since those forks must now, by definition, be too
  -- distant). This time delay @t@ also provides a worst-case bound for the
  -- lifetime of the iterator: if the iterator traverses a chain that
  -- forks off from our current chain at the tip of the immutable DB,
  -- then the first block on that fork will become unavailable as soon as
  -- another block is pushed to the current chain and the subsequent
  -- time delay expires.
  --
  -- Note: although blocks are moved from the volatile DB to the immutable
  -- DB after they have become @k@ deep into the chain, due to data
  -- corruption the suffix of the chain in the volatile DB might be
  -- shorter than @k@. The immutable DB /always/ determines the maximum
  -- rollback, which may therefore be shorter than @k@ under such
  -- circumstances. In addition, streaming blocks which aren't on the
  -- current fork is permitted, but the oldest volatile block must fit on
  -- to the tip of the immutable DB.
  --
  -- When the given bounds are nonsensical, an 'InvalidIteratorRange' is
  -- thrown.
  --
  -- When the given bounds are not part of the chain DB, an 'UnknownRange'
  -- error is returned.
  --
  -- To stream all blocks from the current chain, use 'streamAll', as it
  -- correctly handles an empty ChainDB.
  , newFollower ::
      forall b.
      ResourceRegistry m ->
      ChainType ->
      BlockComponent blk b ->
      m (Follower m blk b)
  -- ^ Chain follower
  --
  -- A chain follower is an iterator that tracks the state of the /current/
  -- chain: calling @next@ on the iterator will either give you the next
  -- block header, or (if we have switched to a fork) the instruction to
  -- rollback.
  --
  -- The tracking iterator starts at genesis (see also 'trackForward').
  --
  -- This is intended for use by chain consumers to /reliably/ follow a
  -- chain, desipite the chain being volatile.
  --
  -- Examples of users:
  -- * The server side of the chain sync mini-protocol for the
  --   node-to-node protocol using headers and the block size.
  -- * The server side of the chain sync mini-protocol for the
  --   node-to-client protocol using blocks.
  , getIsInvalidBlock :: STM m (WithFingerprint (HeaderHash blk -> Maybe (ExtValidationError blk)))
  -- ^ Function to check whether a block is known to be invalid.
  --
  -- Blocks unknown to the ChainDB will result in 'Nothing'.
  --
  -- If the hash corresponds to a block that is known to be invalid, but
  -- is now older than @k@, this function may return 'Nothing'.
  --
  -- Whenever a new invalid block is added, the 'Fingerprint' will be
  -- changed. This is useful when \"watching\" this function in a
  -- transaction.
  --
  -- Note that when invalid blocks are garbage collected and thus no
  -- longer detected by this function, the 'Fingerprint' doesn't have to
  -- change, since the function will not detect new invalid blocks.
  --
  -- It might seem natural to have this function also return whether the
  -- ChainDB knows that a block is valid, thereby subsuming the 'getIsValid'
  -- function and simplifying the API. However, this adds the overhead of
  -- checking whether the block is valid for blocks that are not known to be
  -- invalid that does not give useful information to current clients
  -- (ChainSync), since they are only interested in whether a block is known
  -- to be invalid. The extra information of whether a block is valid is
  -- only used for testing.
  --
  -- In particular, this affects the watcher in 'bracketChainSyncClient',
  -- which rechecks the blocks in all candidate chains whenever a new
  -- invalid block is detected. These blocks are likely to be valid.
  , getChainSelStarvation :: STM m ChainSelStarvation
  -- ^ Whether ChainSel is currently starved, or when was last time it
  -- stopped being starved.
  , getStatistics :: m (Maybe Statistics)
  -- ^ Get statistics from the LedgerDB, in particular the number of entries
  -- in the tables.
  , addPerasCertAsync :: ValidatedPerasCert blk -> m (AddPerasCertPromise m)
  -- ^ TODO docs
  , getPerasWeightSnapshot :: STM m (WithFingerprint (PerasWeightSnapshot blk))
  -- ^ TODO
  , getPerasCertSnapshot :: STM m (PerasCertSnapshot blk)
  -- ^ TODO
  , closeDB :: m ()
  -- ^ Close the ChainDB
  --
  -- Idempotent.
  --
  -- Should only be called on shutdown.
  , isOpen :: STM m Bool
  -- ^ Return 'True' when the database is open.
  --
  -- 'False' when the database is closed.
  }

getCurrentTip ::
  (Monad (STM m), HasHeader (Header blk)) =>
  ChainDB m blk -> STM m (Network.Tip blk)
getCurrentTip = fmap (AF.anchorToTip . AF.headAnchor) . getCurrentChain

getTipBlockNo ::
  (Monad (STM m), HasHeader (Header blk)) =>
  ChainDB m blk -> STM m (WithOrigin BlockNo)
getTipBlockNo = fmap Network.getTipBlockNo . getCurrentTip

{-------------------------------------------------------------------------------
  Adding a block
-------------------------------------------------------------------------------}

data AddBlockPromise m blk = AddBlockPromise
  { blockWrittenToDisk :: STM m Bool
  -- ^ Use this 'STM' transaction to wait until the block has been written
  -- to disk.
  --
  -- Returns 'True' when the block was written to disk or 'False' when it
  -- was ignored, e.g., because it was older than @k@.
  --
  -- If the 'STM' transaction has returned 'True' then 'getIsFetched' will
  -- return 'True' for the added block.
  --
  -- NOTE: Even when the result is 'False', 'getIsFetched' might still
  -- return 'True', e.g., the block was older than @k@, but it has been
  -- downloaded and stored on disk before.
  , blockProcessed :: STM m (AddBlockResult blk)
  -- ^ Use this 'STM' transaction to wait until the block has been
  -- processed: the block has been written to disk and chain selection has
  -- been performed for the block, /unless/ the block is from the future.
  --
  -- The ChainDB's tip after chain selection is returned. When this tip
  -- doesn't match the added block, it doesn't necessarily mean the block
  -- wasn't adopted. We might have adopted a longer chain of which the
  -- added block is a part, but not the tip.
  --
  -- It returns 'FailedToAddBlock' if the thread adding the block died.
  --
  -- NOTE: When the block is from the future, chain selection for the
  -- block won't be performed until the block is no longer in the future,
  -- which might take some time. For that reason, this transaction will
  -- not wait for chain selection of a block from the future. It will
  -- return the current tip of the ChainDB after writing the block to
  -- disk.
  }

-- | This is a wrapper type for 'blockProcessed' function above.
--
-- As it is mentioned the 'SuccessfullyAddedBlock' constructor will containt
-- the ChainDB's tip after chain selection is returned.
--
-- The 'FailedToAddBlock' case will be returned if the thread adding the block
-- died.
data AddBlockResult blk
  = SuccesfullyAddedBlock (Point blk)
  | FailedToAddBlock String
  deriving (Eq, Show)

-- | Add a block synchronously: wait until the block has been written to disk
-- (see 'blockWrittenToDisk').
addBlockWaitWrittenToDisk :: IOLike m => ChainDB m blk -> InvalidBlockPunishment m -> blk -> m Bool
addBlockWaitWrittenToDisk chainDB punish blk = do
  promise <- addBlockAsync chainDB punish blk
  atomically $ blockWrittenToDisk promise

-- | Add a block synchronously: wait until the block has been processed (see
-- 'blockProcessed'). The new tip of the ChainDB is returned unless the thread adding the
-- block died, in that case 'FailedToAddBlock' will be returned.
--
-- Note: this is a partial function, only to support tests.
--
-- PRECONDITION: the block to be added must not be from the future. See 'addBlockAsync'.
addBlock :: IOLike m => ChainDB m blk -> InvalidBlockPunishment m -> blk -> m (AddBlockResult blk)
addBlock chainDB punish blk = do
  promise <- addBlockAsync chainDB punish blk
  atomically $ blockProcessed promise

-- | Add a block synchronously. Variant of 'addBlock' that doesn't return the
-- new tip of the ChainDB.
--
-- Note: this is a partial function, only to support tests.
addBlock_ :: IOLike m => ChainDB m blk -> InvalidBlockPunishment m -> blk -> m ()
addBlock_ = void ..: addBlock

-- | Alias for naming consistency.
-- The short name was chosen to avoid a larger diff from alignment changes.
triggerChainSelectionAsync :: ChainDB m blk -> m (ChainSelectionPromise m)
triggerChainSelectionAsync = chainSelAsync

-- | A promise that the chain selection will be performed. It is returned by
-- 'triggerChainSelectionAsync' and contains a monadic action that waits until
-- the corresponding run of Chain Selection is done.
newtype ChainSelectionPromise m = ChainSelectionPromise
  { -- NOTE: We might want a mechanism similar to 'AddBlockPromise' and
    -- 'AddBlockResult', in case the background ChainDB thread dies; but we
    -- currently only use the synchronous variant in tests.
    waitChainSelectionPromise :: m ()
  }

-- | Trigger selection synchronously: wait until the chain selection has been
-- performed. This is a partial function, only to support tests.
triggerChainSelection :: IOLike m => ChainDB m blk -> m ()
triggerChainSelection chainDB =
  waitChainSelectionPromise =<< chainSelAsync chainDB

{-------------------------------------------------------------------------------
  Adding a Peras certificate
-------------------------------------------------------------------------------}

newtype AddPerasCertPromise m = AddPerasCertPromise
  { waitPerasCertProcessed :: m ()
  -- ^ Wait until the Peras certificate has been processed (which potentially
  -- includes switching to a different chain). If the PerasCertDB did already
  -- contain a certificate for this round, the certificate is ignored (as the
  -- two certificates must be identical because certificate equivocation is
  -- impossible).
  }

addPerasCertSync :: IOLike m => ChainDB m blk -> ValidatedPerasCert blk -> m ()
addPerasCertSync chainDB cert =
  waitPerasCertProcessed =<< addPerasCertAsync chainDB cert

{-------------------------------------------------------------------------------
  Serialised block/header with its point
-------------------------------------------------------------------------------}

-- | A @b@ together with its 'Point'.
--
-- The 'Point' is needed because we often need to know the hash, slot, or
-- point itself of the block or header in question, and we don't want to
-- deserialise the block to obtain it.
data WithPoint blk b = WithPoint
  { withoutPoint :: !b
  , point :: !(Point blk)
  }

type instance HeaderHash (WithPoint blk b) = HeaderHash blk
instance StandardHash blk => StandardHash (WithPoint blk b)

getPoint :: BlockComponent blk (Point blk)
getPoint = BlockPoint <$> GetSlot <*> GetHash

getSerialisedBlockWithPoint ::
  BlockComponent blk (WithPoint blk (Serialised blk))
getSerialisedBlockWithPoint =
  WithPoint <$> (Serialised <$> GetRawBlock) <*> getPoint

getSerialisedHeader :: BlockComponent blk (SerialisedHeader blk)
getSerialisedHeader =
  curry serialisedHeaderFromPair
    <$> GetNestedCtxt
    <*> GetRawHeader

getSerialisedHeaderWithPoint ::
  BlockComponent blk (WithPoint blk (SerialisedHeader blk))
getSerialisedHeaderWithPoint =
  WithPoint <$> getSerialisedHeader <*> getPoint

{-------------------------------------------------------------------------------
  Support for tests
-------------------------------------------------------------------------------}

toChain ::
  forall m blk.
  (HasCallStack, IOLike m, HasHeader blk) =>
  ChainDB m blk -> m (Chain blk)
toChain chainDB = withRegistry $ \registry ->
  streamAll chainDB registry GetBlock >>= go Genesis
 where
  go :: Chain blk -> Iterator m blk blk -> m (Chain blk)
  go chain it = do
    next <- iteratorNext it
    case next of
      IteratorResult blk -> go (Chain.addBlock blk chain) it
      IteratorExhausted -> return chain
      IteratorBlockGCed _ ->
        error "block on the current chain was garbage-collected"

fromChain ::
  forall m blk.
  IOLike m =>
  m (ChainDB m blk) ->
  Chain blk ->
  m (ChainDB m blk)
fromChain openDB chain = do
  chainDB <- openDB
  mapM_ (addBlock_ chainDB noPunishment) $ Chain.toOldestFirst chain
  return chainDB

{-------------------------------------------------------------------------------
  Iterator API
-------------------------------------------------------------------------------}

data Iterator m blk b = Iterator
  { iteratorNext :: m (IteratorResult blk b)
  , iteratorClose :: m ()
  -- ^ When 'fmap'-ing or 'traverse'-ing (or using 'traverseIterator') an
  -- 'Iterator', the resulting iterator will still refer to and use the
  -- original one. This means that when either of them is closed, both
  -- will be closed in practice.
  }
  deriving (Functor, Foldable, Traversable)

-- | An iterator that is immediately exhausted.
emptyIterator :: Monad m => Iterator m blk b
emptyIterator =
  Iterator
    { iteratorNext = return IteratorExhausted
    , iteratorClose = return ()
    }

-- | Variant of 'traverse' instantiated to @'Iterator' m blk@ that executes
-- the monadic function when calling 'iteratorNext'.
traverseIterator ::
  Monad m =>
  (b -> m b') ->
  Iterator m blk b ->
  Iterator m blk b'
traverseIterator f it =
  it
    { iteratorNext = iteratorNext it >>= traverse f
    }

data IteratorResult blk b
  = IteratorExhausted
  | IteratorResult b
  | -- | The block that was supposed to be streamed was garbage-collected from
    -- the VolatileDB, but not added to the ImmutableDB.
    --
    -- This will only happen when streaming very old forks very slowly.
    --
    -- The following example illustrates a situation in which an iterator result
    -- could be a 'IteratorBlockGCed' value. Suppose we start with an iterator
    -- positioned at block @c@, where @[[ x ]]@ denotes a block in the
    -- immutable DB:
    --
    -- @
    --                           iterator i
    --                             ↓
    -- ... ⟶ [[ a ]] → [[ b ]] → [ c ] -> [ d ]
    -- ──────────────────────╯   ╰────────────╯
    --   Immutable DB             Current chain
    -- @
    --
    -- Suppose we switch to a longer fork that branches off from the immutable
    -- tip ('[[b]]').
    --
    -- @
    --                            iterator i
    --                             ↓
    -- ... ⟶ [[ a ]] → [[ b ]] → [ c ] -> [ d ]
    -- ──────────────────────╯│
    --    Immutable DB        │
    --                        ╰-→ [ e ] -> [ f ] -> [ g ]
    --                            ╰─────────────────────╯
    --                                  Current chain
    -- @
    --
    -- Assume @k=2@. This means that block @e@ is the new immutable tip. If we
    -- would call 'iteratorNext' on @i@ __after__ block @e@ is copied to the
    -- immutable DB and @c@ and @d@ are garbage collected, then we will get
    -- 'IteratorBlockGCed'.
    IteratorBlockGCed (RealPoint blk)
  deriving (Functor, Foldable, Traversable)

deriving instance
  (Eq blk, Eq b, StandardHash blk) =>
  Eq (IteratorResult blk b)
deriving instance
  (Show blk, Show b, StandardHash blk) =>
  Show (IteratorResult blk b)

data UnknownRange blk
  = -- | The block at the given point was not found in the ChainDB.
    MissingBlock (RealPoint blk)
  | -- | The requested range forks off too far in the past, i.e. it doesn't
    -- fit on the tip of the ImmutableDB.
    ForkTooOld (StreamFrom blk)
  deriving (Eq, Show)

-- | Stream all blocks from the current chain.
streamAll ::
  (MonadSTM m, HasHeader blk, HasCallStack) =>
  ChainDB m blk ->
  ResourceRegistry m ->
  BlockComponent blk b ->
  m (Iterator m blk b)
streamAll = streamFrom (StreamFromExclusive GenesisPoint)

-- | Stream blocks from the given point up to the tip from the current chain.
--
-- To stream all blocks from the current chain from the ChainDB, one would use
-- @'StreamFromExclusive' 'genesisPoint'@ as the lower bound and
-- @'StreamToInclusive' tip@ as the upper bound where @tip@ is retrieved with
-- 'getTipPoint'.
--
-- However, when the ChainDB is empty, @tip@ will be 'genesisPoint' too, in
-- which case the bounds don't make sense. This function correctly handles
-- this case.
--
-- Note that this is not a 'Follower', so the stream will not include blocks
-- that are added to the current chain after starting the stream.
streamFrom ::
  (MonadSTM m, HasHeader blk, HasCallStack) =>
  StreamFrom blk ->
  ChainDB m blk ->
  ResourceRegistry m ->
  BlockComponent blk b ->
  m (Iterator m blk b)
streamFrom from db registry blockComponent = do
  tip <- atomically $ getTipPoint db
  case pointToWithOriginRealPoint tip of
    Origin -> return emptyIterator
    NotOrigin tip' -> do
      errIt <-
        stream
          db
          registry
          blockComponent
          from
          (StreamToInclusive tip')
      case errIt of
        Right it -> return it
        Left e -> error $ "failed to stream from genesis to tip: " <> show e

{-------------------------------------------------------------------------------
  Followers
-------------------------------------------------------------------------------}

-- | Chain type
--
-- 'Follower's can choose to track changes to the "normal" 'SelectedChain', or
-- track the 'TentativeChain', which might contain a pipelineable header at the
-- tip.
data ChainType = SelectedChain | TentativeChain
  deriving (Eq, Show, Generic)

-- | Follower
--
-- Unlike an 'Iterator', which is used to request a static segment of the
-- current chain or a recent fork, a follower is used to __follow__ the
-- __current chain__ either from the start or from a given point.
--
-- Unlike an 'Iterator', a 'Follower' is __dynamic__, that is, it will follow
-- the chain when it grows or forks.
--
-- A follower is __pull-based__, which avoids the neeed to have a growing queue
-- of changes to the chain on the server side in case the client is slower.
--
-- A follower always has an __implicit position__ associated with it. The
-- 'followerInstruction' and 'followerInstructionBlocking' operations request
-- the next 'ChainUpdate' wrt the follower's implicit position.
--
-- The type parameter @a@ will be instantiated with @blk@ or @'Header' blk@.
data Follower m blk a = Follower
  { followerInstruction :: m (Maybe (ChainUpdate blk a))
  -- ^ The next chain update (if one exists)
  --
  -- The 'AddBlock' instruction (see 'ChainUpdate') indicates that, to
  -- follow the current chain, the follower should extend its chain with the
  -- given block component (which will be a value of type 'a').
  --
  -- The 'RollBack' instruction indicates that the follower should perform a
  -- rollback by first backtracking to a certain point.
  --
  -- If a follower should switch to a fork, then it will first receive a
  -- 'RollBack' instruction followed by as many 'AddBlock' as necessary to
  -- reach the tip of the new chain.
  --
  -- When the follower's (implicit) position is in the immutable part of the
  -- chain, no rollback instructions will be encountered.
  --
  -- Not in @STM@ because might have to read the blocks or headers from
  -- disk.
  --
  -- We may roll back more than @k@, but only in case of data loss.
  , followerInstructionBlocking :: m (ChainUpdate blk a)
  -- ^ Blocking version of 'followerInstruction'
  , followerForward :: [Point blk] -> m (Maybe (Point blk))
  -- ^ Move the follower forward
  --
  -- Must be given a list of points in order of preference; the iterator
  -- will move forward to the first point on the list that is on the current
  -- chain. Returns 'Nothing' if the iterator did not move, or the new point
  -- otherwise.
  --
  -- When successful, the first call to 'followerInstruction' after
  -- 'followerForward' will be a 'RollBack' to the point returned by
  -- 'followerForward'.
  --
  -- Cannot live in @STM@ because the points specified might live in the
  -- immutable DB.
  , followerClose :: m ()
  -- ^ Close the follower.
  --
  -- Idempotent.
  --
  -- After closing, all other operations on the follower will throw
  -- 'ClosedFollowerError'.
  }
  deriving Functor

-- | Variant of 'traverse' instantiated to @'Follower' m blk@ that executes the
-- monadic function when calling 'followerInstruction' and
-- 'followerInstructionBlocking'.
traverseFollower ::
  Monad m =>
  (b -> m b') ->
  Follower m blk b ->
  Follower m blk b'
traverseFollower f flr =
  Follower
    { followerInstruction = followerInstruction flr >>= traverse (traverse f)
    , followerInstructionBlocking = followerInstructionBlocking flr >>= traverse f
    , followerForward = followerForward flr
    , followerClose = followerClose flr
    }

{-------------------------------------------------------------------------------
  Recovery
-------------------------------------------------------------------------------}

-- | Database failure
--
-- This exception wraps any kind of unexpected problem with the on-disk
-- storage of the chain.
--
-- The various constructors only serve to give more detailed information about
-- what went wrong, in case sysadmins want to investigate the disk failure.
-- The Chain DB itself does not differentiate; all disk failures are treated
-- equal and all trigger the same recovery procedure.
data ChainDbFailure blk
  = -- | The ledger DB threw a file-system error
    LgrDbFailure FsError
  | -- | Block missing from the chain DB
    --
    -- Thrown when we are not sure in which DB the block /should/ have been.
    ChainDbMissingBlock (RealPoint blk)

deriving instance StandardHash blk => Show (ChainDbFailure blk)

instance (Typeable blk, StandardHash blk) => Exception (ChainDbFailure blk) where
  displayException = \case
    LgrDbFailure fse -> fsError fse
    ChainDbMissingBlock{} -> corruption
   where
    corruption =
      "The database got corrupted, full validation will be enabled for the next startup"

    -- The output will be a bit too detailed, but it will be quite clear.
    fsError :: FsError -> String
    fsError = displayException

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Database error
--
-- Thrown upon incorrect use: invalid input.
data ChainDbError blk
  = -- | The ChainDB is closed.
    --
    -- This will be thrown when performing any operation on the ChainDB except
    -- for 'isOpen' and 'closeDB'. The 'CallStack' of the operation on the
    -- ChainDB is included in the error.
    ClosedDBError PrettyCallStack
  | -- | The follower is closed.
    --
    -- This will be thrown when performing any operation on a closed followers,
    -- except for 'followerClose'.
    ClosedFollowerError
  | -- | When there is no chain/fork that satisfies the bounds passed to
    -- 'streamBlocks'.
    --
    -- * The lower and upper bound are not on the same chain.
    -- * The bounds don't make sense, e.g., the lower bound starts after the
    --   upper bound, or the lower bound starts from genesis, /inclusive/.
    InvalidIteratorRange (StreamFrom blk) (StreamTo blk)

deriving instance (Typeable blk, StandardHash blk) => Show (ChainDbError blk)

instance (Typeable blk, StandardHash blk) => Exception (ChainDbError blk) where
  displayException = \case
    -- The user should not see the exception below, a fatal exception with
    -- more information about the specific will have been thrown. This
    -- exception will only be thrown if some thread still tries to use the
    -- ChainDB afterwards, which should not happen.
    ClosedDBError{} ->
      "The database was used after it was closed because it encountered an unrecoverable error"
    -- The user won't see the exceptions below, they are not fatal.
    ClosedFollowerError{} ->
      "The block/header follower was used after it was closed"
    InvalidIteratorRange{} ->
      "An invalid range of blocks was requested"

-- | The Limit on Eagerness (LoE) is a mechanism for keeping ChainSel from
-- advancing the current selection in the case of competing chains.
--
-- The LoE tip is the youngest header that is present on all candidate
-- fragments. Thus, after the LoE tip, peers either disagree on how the chain
-- follows, or they do not offer more headers.
--
-- The LoE restrains the current selection of the node to be on the same chain
-- as the LoE tip, and to not extend more than k blocks from it.
--
-- It requires a resolution mechanism to prevent indefinite stalling, which
-- is implemented by the Genesis Density Disconnection governor, a component
-- that disconnects from peers with forks it considers inferior.
-- See "Ouroboros.Consensus.Genesis.Governor" for details.
--
-- This type indicates whether LoE is enabled, and contains a value if it is.
-- There is no a priori meaning assigned to the type parameter @a@.
-- @LoE a@ is isomorphic to @Maybe a@, with the added meaning that
-- @Just/LoEEnabled@ is only used when the LoE is enabled.
data LoE a
  = -- | The LoE is disabled, so ChainSel will not keep the selection from
    -- advancing.
    LoEDisabled
  | -- | The LoE is enabled.
    LoEEnabled !a
  deriving (Eq, Show, Generic, NoThunks, Functor, Foldable, Traversable)

-- | Get the current LoE fragment (if the LoE is enabled), see 'LoE' for more
-- details. This fragment must be anchored in a (recent) point on the immutable
-- chain, just like candidate fragments.
type GetLoEFragment m blk = m (LoE (AnchoredFragment (HeaderWithTime blk)))
