{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
--      \(F_1\), \(F_2\), and \(F_3\) is more preferable. We roll back our chain
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
-- - __Providing 'Ouroboros.Consensus.Ledger.Tables.Basics.LedgerTable's at any of the last \(k\) ledger states__: To apply blocks or transactions on top
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
--      \(k\) ledger states.
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
    CanUpgradeLedgerTables (..)
  , LedgerDB (..)
  , LedgerDB'
  , LedgerDbSerialiseConstraints
  , LedgerSupportsInMemoryLedgerDB
  , LedgerSupportsLedgerDB
  , LedgerSupportsOnDiskLedgerDB
  , ResolveBlock
  , currentPoint
    -- * Initialization
  , InitDB (..)
  , InitLog (..)
  , initialize
    -- ** Tracing
  , ReplayGoal (..)
  , ReplayStart (..)
  , TraceReplayEvent (..)
  , TraceReplayProgressEvent (..)
  , TraceReplayStartEvent (..)
  , decorateReplayTracerWithGoal
  , decorateReplayTracerWithStart
    -- * Configuration
  , LedgerDbCfg
  , LedgerDbCfgF (..)
  , configLedgerDb
    -- * Exceptions
  , LedgerDbError (..)
    -- * Forker
  , getReadOnlyForker
  , getTipStatistics
  , readLedgerTablesAtFor
  , withPrivateTipForker
  , withTipForker
    -- * Snapshots
  , SnapCounters (..)
    -- * Testing
  , TestInternals (..)
  , TestInternals'
  , WhereToTakeSnapshot (..)
  ) where

import           Codec.Serialise
import qualified Control.Monad as Monad
import           Control.Monad.Class.MonadTime.SI
import           Control.Monad.Except
import           Control.ResourceRegistry
import           Control.Tracer
import           Data.Functor.Contravariant ((>$<))
import           Data.Kind
import qualified Data.Map.Strict as Map
import           Data.MemPack
import           Data.Set (Set)
import           Data.Void (absurd)
import           Data.Word
import           GHC.Generics (Generic)
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderStateHistory
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import           Ouroboros.Consensus.Storage.ImmutableDB.Stream
import           Ouroboros.Consensus.Storage.LedgerDB.Forker
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IndexedMemPack
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Block
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
import           System.FS.API

{-------------------------------------------------------------------------------
  Main API
-------------------------------------------------------------------------------}

-- | Serialization constraints required by the 'LedgerDB' to be properly
-- instantiated with a @blk@.
type LedgerDbSerialiseConstraints blk =
  ( Serialise      (HeaderHash  blk)
  , EncodeDisk blk (LedgerState blk EmptyMK)
  , DecodeDisk blk (LedgerState blk EmptyMK)
  , EncodeDisk blk (AnnTip      blk)
  , DecodeDisk blk (AnnTip      blk)
  , EncodeDisk blk (ChainDepState (BlockProtocol blk))
  , DecodeDisk blk (ChainDepState (BlockProtocol blk))
    -- For InMemory LedgerDBs
  , MemPack (TxOut (LedgerState blk))
  , MemPack (TxIn (LedgerState blk))
    -- For OnDisk LedgerDBs
  , IndexedMemPack (LedgerState blk EmptyMK) (TxOut (LedgerState blk))
  )

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

    -- | Acquire a 'Forker' at the requested point. If a ledger state associated
    -- with the requested point does not exist in the LedgerDB, it will return a
    -- 'GetForkerError'.
    --
    -- We pass in the producer/consumer registry.
  , getForkerAtTarget  ::
         ResourceRegistry m
      -> Target (Point blk)
      -> m (Either GetForkerError (Forker m l blk))

    -- | Try to apply a sequence of blocks on top of the LedgerDB, first rolling
    -- back as many blocks as the passed @Word64@.
  , validateFork ::
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
    -- applied and committed.
  , garbageCollect :: SlotNo -> STM m ()

    -- | If the provided arguments indicate so (based on the SnapshotPolicy with
    -- which this LedgerDB was opened), take a snapshot and delete stale ones.
    --
    -- The arguments are:
    --
    -- - If a snapshot has been taken already, the time at which it was taken
    --   and the current time.
    --
    -- - How many blocks have been processed since the last snapshot.
  , tryTakeSnapshot ::
         (l ~ ExtLedgerState blk)
      => Maybe (Time, Time)
      -> Word64
      -> m SnapCounters

    -- | Flush V1 in-memory LedgerDB state to disk, if possible. This is a no-op
    -- for implementations that do not need an explicit flush function.
    --
    -- Note that this is rate-limited by 'ldbShouldFlush'.
  , tryFlush :: m ()

      -- | Close the LedgerDB
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

data WhereToTakeSnapshot = TakeAtImmutableTip | TakeAtVolatileTip deriving Eq

data TestInternals m l blk = TestInternals {
    wipeLedgerDB       :: m ()
  , takeSnapshotNOW    :: WhereToTakeSnapshot -> Maybe String -> m ()
  , push               :: ExtLedgerState blk DiffMK -> m ()
  , reapplyThenPushNOW :: blk -> m ()
  , truncateSnapshots  :: m ()
  , closeLedgerDB      :: m ()
  }
  deriving NoThunks via OnlyCheckWhnfNamed "TestInternals" (TestInternals m l blk)

type TestInternals' m blk = TestInternals m (ExtLedgerState blk) blk

{-------------------------------------------------------------------------------
  Config
-------------------------------------------------------------------------------}

data LedgerDbCfgF f l = LedgerDbCfg {
      ledgerDbCfgSecParam            :: !(HKD f SecurityParam)
    , ledgerDbCfg                    :: !(HKD f (LedgerCfg l))
    , ledgerDbCfgComputeLedgerEvents :: !ComputeLedgerEvents
    }
  deriving (Generic)

type LedgerDbCfg l = Complete LedgerDbCfgF l

deriving instance NoThunks (LedgerCfg l) => NoThunks (LedgerDbCfg l)

configLedgerDb ::
     ConsensusProtocol (BlockProtocol blk)
  => TopLevelConfig blk
  -> ComputeLedgerEvents
  -> LedgerDbCfg (ExtLedgerState blk)
configLedgerDb config evs = LedgerDbCfg {
      ledgerDbCfgSecParam            = configSecurityParam config
    , ledgerDbCfg                    = ExtLedgerCfg config
    , ledgerDbCfgComputeLedgerEvents = evs
    }

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

-- | 'bracket'-style usage of a forker at the LedgerDB tip.
withTipForker ::
     IOLike m
  => LedgerDB m l blk
  -> ResourceRegistry m
  -> (Forker m l blk -> m a)
  -> m a
withTipForker ldb rr =
  bracket
    (do
        eFrk <- getForkerAtTarget ldb rr VolatileTip
        case eFrk of
          Left {}   -> error "Unreachable, volatile tip MUST be in the LedgerDB"
          Right frk -> pure frk
    )
    forkerClose

-- | Like 'withTipForker', but it uses a private registry to allocate and
-- de-allocate the forker.
withPrivateTipForker ::
     IOLike m
  => LedgerDB m l blk
  -> (Forker m l blk -> m a) -> m a
withPrivateTipForker ldb =
  bracketWithPrivateRegistry
    (\rr -> do
        eFrk <- getForkerAtTarget ldb rr VolatileTip
        case eFrk of
          Left {}   -> error "Unreachable, volatile tip MUST be in the LedgerDB"
          Right frk -> pure frk
    )
    forkerClose

-- | Get statistics from the tip of the LedgerDB.
getTipStatistics ::
     IOLike m
  => LedgerDB m l blk
  -> m (Maybe Statistics)
getTipStatistics ldb = withPrivateTipForker ldb forkerReadStatistics

getReadOnlyForker ::
     MonadSTM m
  => LedgerDB m l blk
  -> ResourceRegistry m
  -> Target (Point blk)
  -> m (Either GetForkerError (ReadOnlyForker m l blk))
getReadOnlyForker ldb rr pt = fmap readOnlyForker <$> getForkerAtTarget ldb rr pt

-- | Read a table of values at the requested point via a 'ReadOnlyForker'
readLedgerTablesAtFor ::
     IOLike m
  => LedgerDB m l blk
  -> Point blk
  -> LedgerTables l KeysMK
  -> m (Either GetForkerError (LedgerTables l ValuesMK))
readLedgerTablesAtFor ldb p ks =
    bracketWithPrivateRegistry
      (\rr -> fmap readOnlyForker <$> getForkerAtTarget ldb rr (SpecificPoint p))
      (mapM_ roforkerClose)
      $ \foEith -> Monad.forM foEith (`roforkerReadTables` ks)

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
  Initialization
-------------------------------------------------------------------------------}

-- | Initialization log
--
-- The initialization log records which snapshots from disk were considered,
-- in which order, and why some snapshots were rejected. It is primarily useful
-- for monitoring purposes.
data InitLog blk =
    -- | Defaulted to initialization from genesis
    --
    -- NOTE: Unless the blockchain is near genesis, or this is the first time we
    -- boot the node, we should see this /only/ if data corruption occurred.
    InitFromGenesis

    -- | Used a snapshot corresponding to the specified tip
  | InitFromSnapshot DiskSnapshot (RealPoint blk)

    -- | Initialization skipped a snapshot
    --
    -- We record the reason why it was skipped.
    --
    -- NOTE: We should /only/ see this if data corruption occurred or codecs
    -- for snapshots changed.
  | InitFailure DiskSnapshot (SnapshotFailure blk) (InitLog blk)
  deriving (Show, Eq, Generic)

-- | Functions required to initialize a LedgerDB
type InitDB :: Type -> (Type -> Type) -> Type -> Type
data InitDB db m blk = InitDB {
    initFromGenesis  :: !(m db)
    -- ^ Create a DB from the genesis state
  , initFromSnapshot :: !(Flag "DoDiskSnapshotChecksum" -> DiskSnapshot -> m (Either (SnapshotFailure blk) (db, RealPoint blk)))
    -- ^ Create a DB from a Snapshot
  , closeDb          :: !(db -> m ())
    -- ^ Closing the database, to be reopened again with a different snapshot or
    -- with the genesis state.
  , initReapplyBlock :: !(LedgerDbCfg (ExtLedgerState blk) -> blk -> db -> m db)
    -- ^ Reapply a block from the immutable DB when initializing the DB.
  , currentTip       :: !(db -> LedgerState blk EmptyMK)
    -- ^ Getting the current tip for tracing the Ledger Events.
  , pruneDb          :: !(db -> m db)
    -- ^ Prune the database so that no immutable states are considered volatile.
  , mkLedgerDb       :: !(db -> m (LedgerDB m (ExtLedgerState blk) blk, TestInternals m (ExtLedgerState blk) blk))
    -- ^ Create a LedgerDB from the initialized data structures from previous
    -- steps.
  }

-- | Initialize the ledger DB from the most recent snapshot on disk
--
-- If no such snapshot can be found, use the genesis ledger DB. Returns the
-- initialized DB as well as a log of the initialization and the number of
-- blocks replayed between the snapshot and the tip of the immutable DB.
--
-- We do /not/ catch any exceptions thrown during streaming; should any be
-- thrown, it is the responsibility of the 'ChainDB' to catch these
-- and trigger (further) validation. We only discard snapshots if
--
-- * We cannot deserialise them, or
--
-- * they are /ahead/ of the chain, they refer to a slot which is later than the
--     last slot in the immutable db.
--
-- We do /not/ attempt to use multiple ledger states from disk to construct the
-- ledger DB. Instead we load only a /single/ ledger state from disk, and
-- /compute/ all subsequent ones. This is important, because the ledger states
-- obtained in this way will (hopefully) share much of their memory footprint
-- with their predecessors.
initialize ::
     forall m blk db.
     ( IOLike m
     , LedgerSupportsProtocol blk
     , InspectLedger blk
     , HasCallStack
     )
  => Tracer m (TraceReplayEvent blk)
  -> Tracer m (TraceSnapshotEvent blk)
  -> SomeHasFS m
  -> LedgerDbCfg (ExtLedgerState blk)
  -> StreamAPI m blk blk
  -> Point blk
  -> InitDB db m blk
  -> Maybe DiskSnapshot
  -> Flag "DoDiskSnapshotChecksum"
  -> m (InitLog blk, db, Word64)
initialize replayTracer
           snapTracer
           hasFS
           cfg
           stream
           replayGoal
           dbIface
           fromSnapshot
           doDoDiskSnapshotChecksum =
    case fromSnapshot of
      Nothing   -> listSnapshots hasFS >>= tryNewestFirst doDoDiskSnapshotChecksum id
      Just snap -> tryNewestFirst doDoDiskSnapshotChecksum id [snap]
  where
    InitDB {initFromGenesis, initFromSnapshot, closeDb} = dbIface

    tryNewestFirst :: Flag "DoDiskSnapshotChecksum"
                   -> (InitLog blk -> InitLog blk)
                   -> [DiskSnapshot]
                   -> m ( InitLog   blk
                        , db
                        , Word64
                        )
    tryNewestFirst _ acc [] = do
      -- We're out of snapshots. Start at genesis
      traceWith (TraceReplayStartEvent >$< replayTracer) ReplayFromGenesis
      let replayTracer'' = decorateReplayTracerWithStart (Point Origin) replayTracer'
      initDb <- initFromGenesis
      eDB <- runExceptT $ replayStartingWith
                            replayTracer''
                            cfg
                            stream
                            initDb
                            (Point Origin)
                            dbIface

      case eDB of
        Left err -> do
          closeDb initDb
          error $ "Invariant violation: invalid immutable chain " <> show err
        Right (db, replayed) -> do
          db' <- pruneDb dbIface db
          return ( acc InitFromGenesis
                 , db'
                 , replayed
                 )

    tryNewestFirst doChecksum acc allSnapshot@(s:ss) = do
      eInitDb <- initFromSnapshot doChecksum s
      case eInitDb of
        -- If a checksum file is missing for a snapshot,
        -- issue a warning and retry the same snapshot
        -- ignoring the checksum
        Left (InitFailureRead (ReadSnapshotCRCError _ CRCNoFile)) -> do
          traceWith snapTracer $ SnapshotMissingChecksum s
          tryNewestFirst NoDoDiskSnapshotChecksum acc allSnapshot

        -- If we fail to use this snapshot for any other reason, delete it and
        -- try an older one
        Left err -> do
          Monad.when (diskSnapshotIsTemporary s || err == InitFailureGenesis) $
            deleteSnapshot hasFS s
          traceWith snapTracer . InvalidSnapshot s $ err
          -- reset checksum flag to the initial state after failure
          tryNewestFirst doChecksum (acc . InitFailure s err) ss

        Right (initDb, pt) -> do
          let pt' = realPointToPoint pt
          traceWith (TraceReplayStartEvent >$< replayTracer) (ReplayFromSnapshot s (ReplayStart pt'))
          let replayTracer'' = decorateReplayTracerWithStart pt' replayTracer'
          eDB <- runExceptT
                   $ replayStartingWith
                       replayTracer''
                       cfg
                       stream
                       initDb
                       pt'
                       dbIface
          case eDB of
            Left err -> do
              traceWith snapTracer . InvalidSnapshot s $ err
              terminateIfLegacySnapshot hasFS s
              Monad.when (diskSnapshotIsTemporary s) $ deleteSnapshot hasFS s
              closeDb initDb
              tryNewestFirst doChecksum (acc . InitFailure s err) ss
            Right (db, replayed) -> do
              db' <- pruneDb dbIface db
              return (acc (InitFromSnapshot s pt), db', replayed)

    replayTracer' = decorateReplayTracerWithGoal
                                       replayGoal
                                       (TraceReplayProgressEvent >$< replayTracer)

    terminateIfLegacySnapshot (SomeHasFS hasFS') s = do
      exists <- doesFileExist hasFS' $ snapshotToDirPath s
      Monad.when exists $ error $ unlines
        [ "Detected a legacy-style snapshot!"
        , ""
        , "Legacy snapshots are incompatible with UTxO-HD. The node was shutdown before automatically deleting the snapshot to avoid having to replay the entire chain."
        , ""
        , "To convert your snapshot to the new UTxO-HD format, use the `snapshot-converter` tool in `ouroboros-consensus`:"
        , ""
        , "\t$ snapshot-converter Legacy <path-to-snapshot> [Mem|LMDB] <new-path> cardano --config <path-to-node's-config-file>"
        , ""
        , "After running the converter, put the new snapshot in the ledger database path and start the node again. You can also just delete all snapshots and replay from Genesis."
        ]

-- | Replay all blocks in the Immutable database using the 'StreamAPI' provided
-- on top of the given @LedgerDB' blk@.
--
-- It will also return the number of blocks that were replayed.
replayStartingWith ::
     forall m blk db. (
         IOLike m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , HasCallStack
       )
  => Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayProgressEvent blk)
  -> LedgerDbCfg (ExtLedgerState blk)
  -> StreamAPI m blk blk
  -> db
  -> Point blk
  -> InitDB db m blk
  -> ExceptT (SnapshotFailure blk) m (db, Word64)
replayStartingWith tracer cfg stream initDb from InitDB{initReapplyBlock, currentTip} = do
    streamAll stream from
        InitFailureTooRecent
        (initDb, 0)
        push
  where
    push :: blk
         -> (db, Word64)
         -> m (db, Word64)
    push blk (!db, !replayed) = do
        !db' <- initReapplyBlock cfg blk db

        let !replayed' = replayed + 1

            events = inspectLedger
                       (getExtLedgerCfg (ledgerDbCfg cfg))
                       (currentTip db)
                       (currentTip db')

        traceWith tracer (ReplayedBlock (blockRealPoint blk) events)
        return (db', replayed')

{-------------------------------------------------------------------------------
  Trace replay events
-------------------------------------------------------------------------------}

data TraceReplayEvent blk =
      TraceReplayStartEvent (TraceReplayStartEvent blk)
    | TraceReplayProgressEvent (TraceReplayProgressEvent blk)
    deriving (Show, Eq)

-- | Add the tip of the Immutable DB to the trace event
decorateReplayTracerWithGoal
  :: Point blk -- ^ Tip of the ImmutableDB
  -> Tracer m (TraceReplayProgressEvent blk)
  -> Tracer m (ReplayGoal blk -> TraceReplayProgressEvent blk)
decorateReplayTracerWithGoal immTip = (($ ReplayGoal immTip) >$<)

-- | Add the block at which a replay started.
decorateReplayTracerWithStart
  :: Point blk -- ^ Starting point of the replay
  -> Tracer m (ReplayGoal blk -> TraceReplayProgressEvent blk)
  -> Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayProgressEvent blk)
decorateReplayTracerWithStart start = (($ ReplayStart start) >$<)

-- | Which point the replay started from
newtype ReplayStart blk = ReplayStart (Point blk) deriving (Eq, Show)

-- | Which point the replay is expected to end at
newtype ReplayGoal blk = ReplayGoal (Point blk) deriving (Eq, Show)

-- | Events traced while replaying blocks against the ledger to bring it up to
-- date w.r.t. the tip of the ImmutableDB during initialisation. As this
-- process takes a while, we trace events to inform higher layers of our
-- progress.
data TraceReplayStartEvent blk
  = -- | There were no LedgerDB snapshots on disk, so we're replaying all blocks
    -- starting from Genesis against the initial ledger.
    ReplayFromGenesis
    -- | There was a LedgerDB snapshot on disk corresponding to the given tip.
    -- We're replaying more recent blocks against it.
  | ReplayFromSnapshot
        DiskSnapshot
        (ReplayStart blk) -- ^ the block at which this replay started
  deriving (Generic, Eq, Show)

-- | We replayed the given block (reference) on the genesis snapshot during
-- the initialisation of the LedgerDB. Used during ImmutableDB replay.
--
-- Using this trace the node could (if it so desired) easily compute a
-- "percentage complete".
data TraceReplayProgressEvent blk =
  ReplayedBlock
    (RealPoint blk)   -- ^ the block being replayed
    [LedgerEvent blk]
    (ReplayStart blk) -- ^ the block at which this replay started
    (ReplayGoal blk)  -- ^ the block at the tip of the ImmutableDB
  deriving (Generic, Eq, Show)

{-------------------------------------------------------------------------------
  Updating ledger tables
-------------------------------------------------------------------------------}

type LedgerSupportsInMemoryLedgerDB blk = (CanUpgradeLedgerTables (LedgerState blk))

-- | When pushing differences on InMemory Ledger DBs, we will sometimes need to
-- update ledger tables to the latest era. For unary blocks this is a no-op, but
-- for the Cardano block, we will need to upgrade all TxOuts in memory.
--
-- No correctness property relies on this, as Consensus can work with TxOuts
-- from multiple eras, but the performance depends on it as otherwise we will be
-- upgrading the TxOuts every time we consult them.
class CanUpgradeLedgerTables l where
  upgradeTables ::
       l mk1 -- ^ The original ledger state before the upgrade. This will be the
             -- tip before applying the block.
    -> l mk2 -- ^ The ledger state after the upgrade, which might be in a
             -- different era than the one above.
    -> LedgerTables l ValuesMK -- ^ The tables we want to maybe upgrade.
    -> LedgerTables l ValuesMK

instance CanUpgradeLedgerTables (LedgerState blk)
      => CanUpgradeLedgerTables (ExtLedgerState blk) where
  upgradeTables (ExtLedgerState st0 _) (ExtLedgerState st1 _) =
    castLedgerTables . upgradeTables st0 st1 . castLedgerTables

instance LedgerTablesAreTrivial l
      => CanUpgradeLedgerTables (TrivialLedgerTables l) where
  upgradeTables _ _ (LedgerTables (ValuesMK mk)) =
    LedgerTables (ValuesMK (Map.map absurd mk))

{-------------------------------------------------------------------------------
  Supporting On-Disk backing stores
-------------------------------------------------------------------------------}

type LedgerSupportsOnDiskLedgerDB blk =
  ( IndexedMemPack (LedgerState blk EmptyMK) (TxOut (LedgerState blk))
  )

type LedgerSupportsLedgerDB blk =
  ( LedgerSupportsOnDiskLedgerDB blk
  , LedgerSupportsInMemoryLedgerDB blk
  )
