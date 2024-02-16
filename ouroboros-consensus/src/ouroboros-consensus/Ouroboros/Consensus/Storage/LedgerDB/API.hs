{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingVia              #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE FunctionalDependencies   #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NumericUnderscores       #-}
{-# LANGUAGE QuantifiedConstraints    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

-- | The Ledger DB is responsible for the following tasks:
--
-- - __Maintaining the in-memory ledger state at the tip__: When we try to
--     extend our chain with a new block fitting onto our tip, the block must
--     first be validated using the right ledger state, i.e., the ledger state
--     corresponding to the tip.
--
-- - __Maintaining the past \(k\) in-memory ledger states__: we might roll back
--     up to \(k\) blocks when switching to a more preferable fork. Consider the
--     example below:
--
--     <<docs/haddocks/ledgerdb-switch.svg>>
--
--     Our current chain's tip is \(C_2\), but the fork containing blocks
--     \(F_1\), \(F_2\), and \(F_3\) is more preferable. We roll back our chain
--     to the intersection point of the two chains, \(I\), which must be not
--     more than \(k\) blocks back from our current tip. Next, we must validate
--     block \(F_1\) using the ledger state at block \(I\), after which we can
--     validate \(F_2\) using the resulting ledger state, and so on.
--
--     This means that we need access to all ledger states of the past \(k\)
--     blocks, i.e., the ledger states corresponding to the volatile part of the
--     current chain. Note that applying a block to a ledger state is not an
--     invertible operation, so it is not possible to simply /unapply/ \(C_1\)
--     and \(C_2\) to obtain \(I\).
--
--     Access to the last \(k\) ledger states is not only needed for validating
--     candidate chains, but also by the:
--
--     - __Local state query server__: To query any of the past \(k\) ledger
--       states.
--
--     - __Chain sync client__: To validate headers of a chain that intersects
--        with any of the past \(k\) blocks.
--
-- - __Providing 'Ouroboros.Consensus.Ledger.Tables.Basics.LedgerTable's at any
--     of the last \(k\) ledger states__: To apply blocks or transactions on top
--     of ledger states, the LedgerDB must be able to provide the appropriate
--     ledger tables at any of those ledger states.
--
-- - __Storing snapshots on disk__: To obtain a ledger state for the current tip
--     of the chain, one has to apply /all blocks in the chain/ one-by-one to
--     the initial ledger state. When starting up the system with an on-disk
--     chain containing millions of blocks, all of them would have to be read
--     from disk and applied. This process can take hours, depending on the
--     storage and CPU speed, and is thus too costly to perform on each startup.
--
--     For this reason, a recent snapshot of the ledger state should be
--     periodically written to disk. Upon the next startup, that snapshot can be
--     read and used to restore the current ledger state, as well as the past
--     \(k\) ledger states.
--
-- - __Flushing 'LedgerTable' differences__: The running Consensus has to
--     periodically flush chunks of [differences]("Data.Map.Diff.Strict")
--     from the 'DbChangelog' to the 'BackingStore', so that memory is
--     off-loaded to the backing store, and if the backing store is an on-disk
--     implementation, reduce the memory usage.
--
-- Note that whenever we say /ledger state/ we mean the @'ExtLedgerState' blk
-- mk@ type described in "Ouroboros.Consensus.Ledger.Basics".
--
-- === __(image code)__
-- >>> import Image.LaTeX.Render
-- >>> import Control.Monad
-- >>> import System.Directory
-- >>>
-- >>> createDirectoryIfMissing True "docs/haddocks/"
-- >>> :{
-- >>> either (error . show) pure =<<
-- >>>  renderToFile "docs/haddocks/ledgerdb-switch.svg" defaultEnv (tikz ["positioning", "arrows"]) "\
-- >>> \ \\draw (0, 0) -- (50pt, 0) coordinate (I);\
-- >>> \  \\draw (I) -- ++(20pt,  20pt) coordinate (C1) -- ++(20pt, 0) coordinate (C2);\
-- >>> \  \\draw (I) -- ++(20pt, -20pt) coordinate (F1) -- ++(20pt, 0) coordinate (F2) -- ++(20pt, 0) coordinate (F3);\
-- >>> \  \\node at (I)  {$\\bullet$};\
-- >>> \  \\node at (C1) {$\\bullet$};\
-- >>> \  \\node at (C2) {$\\bullet$};\
-- >>> \  \\node at (F1) {$\\bullet$};\
-- >>> \  \\node at (F2) {$\\bullet$};\
-- >>> \  \\node at (F3) {$\\bullet$};\
-- >>> \  \\node at (I) [above left] {$I$};\
-- >>> \  \\node at (C1) [above] {$C_1$};\
-- >>> \  \\node at (C2) [above] {$C_2$};\
-- >>> \  \\node at (F1) [below] {$F_1$};\
-- >>> \  \\node at (F2) [below] {$F_2$};\
-- >>> \  \\node at (F3) [below] {$F_3$};\
-- >>> \  \\draw (60pt, 50pt) node {$\\overbrace{\\hspace{60pt}}$};\
-- >>> \  \\draw (60pt, 60pt) node[fill=white] {$k$};\
-- >>> \  \\draw [dashed] (30pt, -40pt) -- (30pt, 45pt);"
-- >>> :}
--
module Ouroboros.Consensus.Storage.LedgerDB.API (
    -- * Main API
    LedgerDB (..)
  , LedgerDB'
  , currentPoint
    -- * Exceptions
  , LedgerDbError (..)
    -- * Arguments
  , QueryBatchSize (..)
  , defaultQueryBatchSize
    -- * Forker
  , ExceededRollback (..)
  , Forker (..)
  , Forker'
  , GetForkerError (..)
  , RangeQuery (..)
  , Statistics (..)
  , forkerCurrentPoint
  , getForker
  , getTipStatistics
  , readLedgerStateAtTipFor
  , readLedgerTablesAtFor
  , withPrivateTipForker
  , withTipForker
    -- ** Read-only forkers
  , ReadOnlyForker (..)
  , ReadOnlyForker'
  , readOnlyForker
    -- * Snapshots
  , DiskSnapshot (..)
  , SnapCounters (..)
  , SnapshotFailure (..)
    -- * Validation
  , AnnLedgerError (..)
  , ValidateResult (..)
    -- * Tracing
  , TraceLedgerDBEvent (..)
    -- ** Replay events
  , ReplayGoal (..)
  , ReplayStart (..)
  , TraceReplayEvent (..)
  , decorateReplayTracerWithGoal
  , decorateReplayTracerWithStart
    -- ** Snapshot events
  , TraceSnapshotEvent (..)
    -- ** Validation events
  , PushGoal (..)
  , PushStart (..)
  , Pushing (..)
  , TraceValidateEvent (..)
  ) where

import           Control.Monad (forM)
import           Control.Monad.Class.MonadTime.SI
import           Control.Tracer
import           Data.Functor.Contravariant
import           Data.Kind
import           Data.Set (Set)
import           Data.Word
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderStateHistory
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.CBOR
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

-- | The core API of the LedgerDB component
type LedgerDB :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data LedgerDB m l blk = LedgerDB {
    -- | Get the empty ledger state at the (volatile) tip of the LedgerDB.
    getVolatileTip         ::              STM m (l EmptyMK)
    -- | Get the empty ledger state at the immutable tip of the LedgerDB.
  , getImmutableTip        ::              STM m (l EmptyMK)
    -- | Get an empty ledger state at a requested point in the LedgerDB, if it
    -- exists.
  , getPastLedgerState     :: Point blk -> STM m (Maybe (l EmptyMK))
    -- | Get the header state history for all ledger states in the LedgerDB.
  , getHeaderStateHistory  ::
         (l ~ ExtLedgerState blk)
      => STM m (HeaderStateHistory blk)
    -- | Acquire a 'Forker' at the tip.
  , getForkerAtTip  :: ResourceRegistry m -> m (Forker m l blk)
    -- | Acquire a 'Forker' at the requested point. If a ledger state associated
    -- with the requested point does not exist in the LedgerDB, it will return a
    -- 'GetForkerError'.
  , getForkerAtPoint ::
         ResourceRegistry m
      -> Point blk
      -> m (Either GetForkerError (Forker m l blk))
    -- | Acquire a 'Forker' at a requested @n@ blocks back from the tip.
    --
    -- You could view this as a rollback of @n@ blocks, and acquiring the
    -- 'Forker' at that point.
  , getForkerAtFromTip ::
         ResourceRegistry m
      -> Word64
      -> m (Either ExceededRollback (Forker m l blk))
  , validate ::
         (l ~ ExtLedgerState blk)
      => ResourceRegistry m
      -> (TraceValidateEvent blk -> m ())
      -> BlockCache blk
      -> Word64
      -> [Header blk]
      -> m (ValidateResult m l blk)
    -- | Get the references to blocks that have previously been applied.
  , getPrevApplied :: STM m (Set (RealPoint blk))
    -- | Garbage collect references to old blocks that have been previously
    -- applied.
  , garbageCollect :: SlotNo -> STM m ()
    -- | If the provided arguments indicate so (based on the DiskPolicy with
    -- which this LedgerDB was opened), take a snapshot and delete stale ones.
  , tryTakeSnapshot ::
         (l ~ ExtLedgerState blk)
      => Maybe (Time, Time)
#if __GLASGOW_HASKELL__ >= 902
         -- ^ If a snapshot has been taken already, the time at which it was
         -- taken and the current time.
#endif
      -> Word64
#if __GLASGOW_HASKELL__ >= 902
         -- ^ How many blocks have been processed since the last snapshot.
#endif
      -> m SnapCounters
    -- | Flush in-memory LedgerDB state to disk, if possible. This is a no-op
    -- for implementations that do not need an explicit flush function.
  , tryFlush :: m ()
      -- | Close the ChainDB
      --
      -- Idempotent.
      --
      -- Should only be called on shutdown.
  , closeDB :: m ()
  }
  deriving NoThunks via OnlyCheckWhnfNamed "LedgerDB" (LedgerDB m l blk)

type instance HeaderHash (LedgerDB m l blk) = HeaderHash blk

type LedgerDB' m blk = LedgerDB m (ExtLedgerState blk) blk

currentPoint ::
     (GetTip l, HeaderHash l ~ HeaderHash blk, Functor (STM m))
  => LedgerDB m l blk
  -> STM m (Point blk)
currentPoint ldb = castPoint . getTip <$> getVolatileTip ldb

{-------------------------------------------------------------------------------
  Exceptions
-------------------------------------------------------------------------------}

-- | Database error
--
-- Thrown upon incorrect use: invalid input.
data LedgerDbError blk =
      -- | The LedgerDB is closed.
      --
      -- This will be thrown when performing some operations on the LedgerDB. The
      -- 'CallStack' of the operation on the LedgerDB is included in the error.
      ClosedDBError PrettyCallStack
      -- | A Forker is closed.
    | ClosedForkerError PrettyCallStack
    deriving (Show)
    deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Arguments
-------------------------------------------------------------------------------}

-- | The /maximum/ number of keys to read in a backing store range query.
--
-- When performing a ledger state query that involves on-disk parts of the
-- ledger state, we might have to read ranges of key-value pair data (e.g.,
-- UTxO) from disk using backing store range queries. Instead of reading all
-- data in one go, we read it in batches. 'QueryBatchSize' determines the size
-- of these batches.
--
-- INVARIANT: Should be at least 1.
--
-- It is fine if the result of a range read contains less than this number of
-- keys, but it should never return more.
data QueryBatchSize =
    -- | A default value, which is determined by a specific 'DiskPolicy'. See
    -- 'defaultDiskPolicy' as an example.
    DefaultQueryBatchSize
    -- | A requested value: the number of keys to read from disk in each batch.
  | RequestedQueryBatchSize Word64
  deriving (Show, Eq, Generic)
  deriving anyclass NoThunks

defaultQueryBatchSize :: QueryBatchSize -> Word64
defaultQueryBatchSize requestedQueryBatchSize = case requestedQueryBatchSize of
    RequestedQueryBatchSize value -> value
    DefaultQueryBatchSize         -> 100_000

{-------------------------------------------------------------------------------
  Forker
-------------------------------------------------------------------------------}

-- | An independent handle to a point the LedgerDB, which can be advanced to
-- evaluate forks in the chain.
type Forker :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data Forker m l blk = Forker {
    -- | Close the current forker (idempotent).
    --
    -- Other functions on forkers should throw a 'ClosedForkError' once the
    -- forker is closed.
    --
    -- Note: always use this functions before the forker is forgotten!
    -- Otherwise, cleanup of (on-disk) state might not be prompt or guaranteed.
    --
    -- This function should release any resources that are held by the forker,
    -- and not by the LedgerDB.
    forkerClose :: !(m ())

    -- * Queries

    -- | Read ledger tables from disk.
  , forkerReadTables :: !(LedgerTables l KeysMK -> m (LedgerTables l ValuesMK))
    -- | Range-read ledger tables from disk.
  , forkerRangeReadTables :: !(RangeQuery l -> m (LedgerTables l ValuesMK))
    -- | Like 'forkerRangeReadTables', but using the 'QueryBatchSize' that the
    -- 'LedgerDB' was opened with.
  , forkerRangeReadTablesDefault :: !(Maybe (LedgerTables l KeysMK) -> m (LedgerTables l ValuesMK))
    -- | Get the full ledger state without tables.
    --
    -- If empty ledger state is all you need, use 'getVolatileTip',
    -- 'getImmutableTip', or 'getPastLedgerState' instead.
  , forkerGetLedgerState  :: !(STM m (l EmptyMK))
    -- | Get statistics about the current state of the handle if possible.
    --
    -- Returns 'Nothing' if the implementation is backed by @lsm-tree@.
  , forkerReadStatistics :: !(m (Maybe Statistics))

    -- * Updates

    -- | Advance the fork handle by pushing a new ledger state to the tip of the
    -- current fork.
  , forkerPush :: !(l DiffMK -> m ())
    -- | Commit the fork, which was constructed using 'forkerPush', as the
    -- current version of the LedgerDB.
  , forkerCommit :: !(STM m ())
  }

type instance HeaderHash (Forker m l blk) = HeaderHash l

type Forker' m blk = Forker m (ExtLedgerState blk) blk

-- TODO(jdral_ldb): get rid of this instance
instance Show (Forker m l blk) where show _ = "Forker"

instance (GetTip l, HeaderHash l ~ HeaderHash blk, MonadSTM m)
      => GetTipSTM m (Forker m l blk) where
  getTipSTM forker = castPoint . getTip <$> forkerGetLedgerState forker

-- TODO: document
data RangeQuery l = RangeQuery {
    rqPrev  :: !(Maybe (LedgerTables l KeysMK))
  , rqCount :: !Int
  }

-- TODO: document
newtype Statistics = Statistics {
    ledgerTableSize :: Int
  }

-- | Errors that can be thrown while acquiring forkers.
data GetForkerError =
    -- | The requested point was not found in the LedgerDB, but the point is
    -- recent enough that the point is not in the immutable part of the chain
    PointNotOnChain
    -- | The requested point was not found in the LedgerDB because the point is
    -- in the immutable part of the chain.
  | PointTooOld
  deriving (Show, Eq)

-- | Exceeded maximum rollback supported by the current ledger DB state
--
-- Under normal circumstances this will not arise. It can really only happen
-- in the presence of data corruption (or when switching to a shorter fork,
-- but that is disallowed by all currently known Ouroboros protocols).
--
-- Records both the supported and the requested rollback.
data ExceededRollback = ExceededRollback {
      rollbackMaximum   :: Word64
    , rollbackRequested :: Word64
    }

forkerCurrentPoint ::
     (GetTip l, HeaderHash l ~ HeaderHash blk, Functor (STM m))
  => Forker m l blk
  -> STM m (Point blk)
forkerCurrentPoint forker =
      castPoint
    . getTip
    <$> forkerGetLedgerState forker

-- | 'bracket'-style usage of a forker at the LedgerDB tip.
withTipForker ::
     IOLike m
  => LedgerDB m l blk
  -> ResourceRegistry m
  -> (Forker m l blk -> m a) -> m a
withTipForker ldb rr = bracket (getForkerAtTip ldb rr) forkerClose

-- | Like 'withTipForker', but it uses a private registry to allocate and
-- de-allocate the forker.
withPrivateTipForker ::
     IOLike m
  => LedgerDB m l blk
  -> (Forker m l blk -> m a) -> m a
withPrivateTipForker ldb = bracketWithPrivateRegistry (getForkerAtTip ldb) forkerClose

getForker ::
     MonadSTM m
  => LedgerDB m l blk
  -> ResourceRegistry m
  -> Maybe (Point blk)
  -> m (Either GetForkerError (Forker m l blk))
getForker ldb rr = \case
    Nothing -> Right <$> getForkerAtTip ldb rr
    Just pt -> getForkerAtPoint ldb rr pt

-- | Read a table of values at the requested point.
readLedgerTablesAtFor ::
     IOLike m
  => LedgerDB m l blk
  -> Point blk
  -> LedgerTables l KeysMK
  -> m (Either GetForkerError (LedgerTables l ValuesMK))
readLedgerTablesAtFor ldb p ks =
    bracketWithPrivateRegistry
      (\rr -> getForkerAtPoint ldb rr p)
      (mapM_ forkerClose)
      $ \foEith -> do
        forM foEith $ \fo -> do
          fo `forkerReadTables` ks

-- | Get statistics from the tip of the LedgerDB.
getTipStatistics ::
     IOLike m
  => LedgerDB m l blk
  -> m (Maybe Statistics)
getTipStatistics ldb = withPrivateTipForker ldb forkerReadStatistics

readLedgerStateAtTipFor ::
     (IOLike m, HasLedgerTables l)
  => LedgerDB m l blk
  -> LedgerTables l KeysMK
  -> m (l ValuesMK)
readLedgerStateAtTipFor ldb ks = withPrivateTipForker ldb $ \forker -> do
    state <- atomically $ forkerGetLedgerState forker
    tables <- forkerReadTables forker ks
    pure $ state `withLedgerTables` tables

{-------------------------------------------------------------------------------
  Read-only forkers
-------------------------------------------------------------------------------}

-- | Read-only 'Forker'.
type ReadOnlyForker :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data ReadOnlyForker m l blk = ReadOnlyForker {
    -- | See 'forkerClose'
    roforkerClose :: !(m ())
    -- | See 'forkerReadTables'
  , roforkerReadTables :: !(LedgerTables l KeysMK -> m (LedgerTables l ValuesMK))
    -- | See 'forkerRangeReadTables'.
  , roforkerRangeReadTables :: !(RangeQuery l -> m (LedgerTables l ValuesMK))
    -- | See 'forkerRangeReadTablesDefault'
  , roforkerRangeReadTablesDefault :: !(Maybe (LedgerTables l KeysMK) -> m (LedgerTables l ValuesMK))
    -- | See 'forkerGetLedgerState'
  , roforkerGetLedgerState  :: !(STM m (l EmptyMK))
    -- | See 'forkerReadStatistics'
  , roforkerReadStatistics :: !(m (Maybe Statistics))
  }

type instance HeaderHash (ReadOnlyForker m l blk) = HeaderHash l

type ReadOnlyForker' m blk = ReadOnlyForker m (ExtLedgerState blk) blk

readOnlyForker :: Forker m l blk -> ReadOnlyForker m l blk
readOnlyForker forker = ReadOnlyForker {
      roforkerClose = forkerClose forker
    , roforkerReadTables = forkerReadTables forker
    , roforkerRangeReadTables = forkerRangeReadTables forker
    , roforkerRangeReadTablesDefault = forkerRangeReadTablesDefault forker
    , roforkerGetLedgerState = forkerGetLedgerState forker
    , roforkerReadStatistics = forkerReadStatistics forker
    }

{-------------------------------------------------------------------------------
  Snapshots
-------------------------------------------------------------------------------}

-- | Counters to keep track of when we made the last snapshot.
data SnapCounters = SnapCounters {
    -- | When was the last time we made a snapshot
    prevSnapshotTime      :: !(Maybe Time)
    -- | How many blocks have we processed since the last snapshot
  , ntBlocksSinceLastSnap :: !Word64
  }

data DiskSnapshot = DiskSnapshot {
      -- | Snapshots are numbered. We will try the snapshots with the highest
      -- number first.
      --
      -- When creating a snapshot, we use the slot number of the ledger state it
      -- corresponds to as the snapshot number. This gives an indication of how
      -- recent the snapshot is.
      --
      -- Note that the snapshot names are only indicative, we don't rely on the
      -- snapshot number matching the slot number of the corresponding ledger
      -- state. We only use the snapshots numbers to determine the order in
      -- which we try them.
      dsNumber :: Word64

      -- | Snapshots can optionally have a suffix, separated by the snapshot
      -- number with an underscore, e.g., @4492799_last_Byron@. This suffix acts
      -- as metadata for the operator of the node. Snapshots with a suffix will
      -- /not be trimmed/.
    , dsSuffix :: Maybe String
    }
  deriving (Show, Eq, Ord, Generic)

data SnapshotFailure blk =
    -- | We failed to deserialise the snapshot
    --
    -- This can happen due to data corruption in the ledger DB.
    InitFailureRead ReadIncrementalErr

    -- | This snapshot is too recent (ahead of the tip of the chain)
  | InitFailureTooRecent (RealPoint blk)

    -- | This snapshot was of the ledger state at genesis, even though we never
    -- take snapshots at genesis, so this is unexpected.
  | InitFailureGenesis
  deriving (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

-- | When validating a sequence of blocks, these are the possible outcomes.
--
-- TODO: add forker to ledger error
data ValidateResult m l blk =
    ValidateSuccessful       (Forker m l blk)
  | ValidateLedgerError      (AnnLedgerError m l blk)
  | ValidateExceededRollBack ExceededRollback

-- | Annotated ledger errors
data AnnLedgerError m l blk = AnnLedgerError {
        -- | The ledger DB just /before/ this block was applied
      annLedgerState  :: Forker m l blk

      -- | Reference to the block that had the error
    , annLedgerErrRef :: RealPoint blk

      -- | The ledger error itself
    , annLedgerErr    :: LedgerErr l
    }

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

data TraceLedgerDBEvent blk =
    LedgerDBSnapshotEvent (TraceSnapshotEvent blk)
  deriving (Show, Eq, Generic)

{-------------------------------------------------------------------------------
  Trace replay events
-------------------------------------------------------------------------------}

-- | Add the tip of the Immutable DB to the trace event
--
-- Between the tip of the immutable DB and the point of the starting block,
-- the node could (if it so desired) easily compute a "percentage complete".
decorateReplayTracerWithGoal
  :: Point blk -- ^ Tip of the ImmutableDB
  -> Tracer m (TraceReplayEvent blk)
  -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
decorateReplayTracerWithGoal immTip = (($ ReplayGoal immTip) >$<)

-- | Add the block at which a replay started.
--
-- This allows to compute a "percentage complete" when tracing the events.
decorateReplayTracerWithStart
  :: Point blk -- ^ Starting point of the replay
  -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk)
decorateReplayTracerWithStart start = (($ ReplayStart start) >$<)

-- | Which point the replay started from
newtype ReplayStart blk = ReplayStart (Point blk) deriving (Eq, Show)

-- | Which point the replay is expected to end at
newtype ReplayGoal blk = ReplayGoal (Point blk) deriving (Eq, Show)

-- | Events traced while replaying blocks against the ledger to bring it up to
-- date w.r.t. the tip of the ImmutableDB during initialisation. As this
-- process takes a while, we trace events to inform higher layers of our
-- progress.
data TraceReplayEvent blk
  = -- | There were no LedgerDB snapshots on disk, so we're replaying all blocks
    -- starting from Genesis against the initial ledger.
    ReplayFromGenesis
        (ReplayGoal blk)  -- ^ the block at the tip of the ImmutableDB
    -- | There was a LedgerDB snapshot on disk corresponding to the given tip.
    -- We're replaying more recent blocks against it.
  | ReplayFromSnapshot
        DiskSnapshot
        (RealPoint blk)
        (ReplayStart blk) -- ^ the block at which this replay started
        (ReplayGoal blk)  -- ^ the block at the tip of the ImmutableDB
  -- | We replayed the given block (reference) on the genesis snapshot during
  -- the initialisation of the LedgerDB. Used during ImmutableDB replay.
  | ReplayedBlock
        (RealPoint blk)   -- ^ the block being replayed
        [LedgerEvent blk]
        (ReplayStart blk) -- ^ the block at which this replay started
        (ReplayGoal blk)  -- ^ the block at the tip of the ImmutableDB
  deriving (Generic, Eq, Show)

{-------------------------------------------------------------------------------
  Tracing snapshot events
-------------------------------------------------------------------------------}

data TraceSnapshotEvent blk
  = InvalidSnapshot DiskSnapshot (SnapshotFailure blk)
    -- ^ An on disk snapshot was skipped because it was invalid.
  | TookSnapshot DiskSnapshot (RealPoint blk)
    -- ^ A snapshot was written to disk.
  | DeletedSnapshot DiskSnapshot
    -- ^ An old or invalid on-disk snapshot was deleted
  deriving (Generic, Eq, Show)

{-------------------------------------------------------------------------------
  Trace validation events
-------------------------------------------------------------------------------}

newtype PushStart blk = PushStart { unPushStart :: RealPoint blk }
  deriving (Show, Eq)

newtype PushGoal blk = PushGoal { unPushGoal :: RealPoint blk }
  deriving (Show, Eq)

newtype Pushing blk = Pushing { unPushing :: RealPoint blk }
  deriving (Show, Eq)

data TraceValidateEvent blk =
    -- | Event fired when we are about to push a block to a forker
      StartedPushingBlockToTheLedgerDb
        !(PushStart blk)
        -- ^ Point from which we started pushing new blocks
        (PushGoal blk)
        -- ^ Point to which we are updating the ledger, the last event
        -- StartedPushingBlockToTheLedgerDb will have Pushing and PushGoal
        -- wrapping over the same RealPoint
        !(Pushing blk)
        -- ^ Point which block we are about to push
  deriving (Show, Eq, Generic)
