{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

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
    -- * Forker
  , ExceededRollback (..)
  , Forker (..)
  , Forker'
  , ForkerKey (..)
  , GetForkerError (..)
  , RangeQuery (..)
  , Statistics (..)
  , forkerCurrentPoint
  , getReadOnlyForker
  , getTipStatistics
  , readLedgerTablesAtFor
  , withPrivateTipForker
  , withTipForker
    -- ** Read-only forkers
  , ReadOnlyForker (..)
  , ReadOnlyForker'
  , readOnlyForker
    -- * Snapshots
  , SnapCounters (..)
    -- * Validation
  , ValidateResult (..)
  , ValidateResult'
    -- ** Annotated ledger errors
  , AnnLedgerError (..)
  , AnnLedgerError'
    -- * Tracing
    -- ** Validation events
  , PushGoal (..)
  , PushStart (..)
  , Pushing (..)
  , TraceValidateEvent (..)
    -- ** Forker events
  , TraceForkerEvent (..)
  , TraceForkerEventWithKey (..)
  ) where

import           Control.Monad (forM)
import           Control.Monad.Class.MonadTime.SI
import           Data.Kind
import           Data.Set (Set)
import           Data.Word
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderStateHistory
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import           Ouroboros.Consensus.Util.CallStack
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
  , getForkerAtTip  ::
         ResourceRegistry m
#if __GLASGOW_HASKELL__ >= 902
         -- ^ The producer/consumer registry.
#endif
      -> m (Forker m l blk)
    -- | Acquire a 'Forker' at the requested point. If a ledger state associated
    -- with the requested point does not exist in the LedgerDB, it will return a
    -- 'GetForkerError'.
  , getForkerAtPoint ::
         ResourceRegistry m
#if __GLASGOW_HASKELL__ >= 902
         -- ^ The producer/consumer registry.
#endif
      -> Point blk
      -> m (Either GetForkerError (Forker m l blk))
  , validate ::
         (l ~ ExtLedgerState blk)
      => ResourceRegistry m
#if __GLASGOW_HASKELL__ >= 902
         -- ^ The producer/consumer registry.
#endif
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
    | ClosedForkerError ForkerKey PrettyCallStack
    deriving (Show)
    deriving anyclass (Exception)

{-------------------------------------------------------------------------------
  Forker
-------------------------------------------------------------------------------}

-- | An independent handle to a point in the LedgerDB, which can be advanced to
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

-- | An identifier for a 'Forker'. See 'ldbForkers'.
newtype ForkerKey = ForkerKey Word16
  deriving stock (Show, Eq, Ord)
  deriving newtype (Enum, NoThunks, Num)

type instance HeaderHash (Forker m l blk) = HeaderHash l

type Forker' m blk = Forker m (ExtLedgerState blk) blk

instance (GetTip l, HeaderHash l ~ HeaderHash blk, MonadSTM m)
      => GetTipSTM m (Forker m l blk) where
  getTipSTM forker = castPoint . getTip <$> forkerGetLedgerState forker

-- TODO: This type is unsuitable for FlavorV2 queries, in particular for LSM
-- queries. Those will work by splitting the UTxO set in chunks, which means
-- that there is no number of keys to read, but instead what fraction of the
-- UTxO range of keys to consult.
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

-- | Get statistics from the tip of the LedgerDB.
getTipStatistics ::
     IOLike m
  => LedgerDB m l blk
  -> m (Maybe Statistics)
getTipStatistics ldb = withPrivateTipForker ldb forkerReadStatistics

{-------------------------------------------------------------------------------
  Read-only forkers
-------------------------------------------------------------------------------}

-- | Read-only 'Forker'.
--
-- These forkers are not allowed to commit. They are used everywhere except in
-- Chain Selection. In particular they are now used in:
--
-- - LocalStateQuery server, via 'getReadOnlyForkerAtPoint'
--
-- - Forging loop.
--
-- - Mempool.
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

getReadOnlyForker ::
     MonadSTM m
  => LedgerDB m l blk
  -> ResourceRegistry m
  -> Maybe (Point blk)
  -> m (Either GetForkerError (ReadOnlyForker m l blk))
getReadOnlyForker ldb rr = \case
    Nothing -> Right . readOnlyForker <$> getForkerAtTip ldb rr
    Just pt -> fmap readOnlyForker <$> getForkerAtPoint ldb rr pt

-- | Read a table of values at the requested point via a 'ReadOnlyForker'
readLedgerTablesAtFor ::
     IOLike m
  => LedgerDB m l blk
  -> Point blk
  -> LedgerTables l KeysMK
  -> m (Either GetForkerError (LedgerTables l ValuesMK))
readLedgerTablesAtFor ldb p ks =
    bracketWithPrivateRegistry
      (\rr -> fmap readOnlyForker <$> getForkerAtPoint ldb rr p)
      (mapM_ roforkerClose)
      $ \foEith -> do
        forM foEith $ \fo -> do
          fo `roforkerReadTables` ks

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

type ValidateResult' m blk = ValidateResult m (ExtLedgerState blk) blk

{-------------------------------------------------------------------------------
  An annotated ledger error
-------------------------------------------------------------------------------}

-- | Annotated ledger errors
data AnnLedgerError m l blk = AnnLedgerError {
        -- | The ledger DB just /before/ this block was applied
      annLedgerState  :: Forker m l blk

      -- | Reference to the block that had the error
    , annLedgerErrRef :: RealPoint blk

      -- | The ledger error itself
    , annLedgerErr    :: LedgerErr l
    }

type AnnLedgerError' m blk = AnnLedgerError m (ExtLedgerState blk) blk

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

{-------------------------------------------------------------------------------
  Forker events
-------------------------------------------------------------------------------}

data TraceForkerEventWithKey =
  TraceForkerEventWithKey ForkerKey TraceForkerEvent
  deriving (Show, Eq)

data TraceForkerEvent =
    ForkerOpen
  | ForkerCloseUncommitted
  | ForkerCloseCommitted
  | ForkerReadTablesStart
  | ForkerReadTablesEnd
  | ForkerRangeReadTablesStart
  | ForkerRangeReadTablesEnd
  | ForkerReadStatistics
  | ForkerPushStart
  | ForkerPushEnd
  deriving (Show, Eq)
