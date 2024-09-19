{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Logic for initializing the LedgerDB.
--
-- Each implementation of the LedgerDB has to provide an instantiation of
-- 'InitDB'. See 'initialize' for a description of the initialization process.
module Ouroboros.Consensus.Storage.LedgerDB.Impl.Init (
    -- * Initialization interface
    InitDB (..)
    -- * Initialization logic
  , InitLog (..)
  , openDB
  , openDBInternal
    -- * Testing
  , initialize
  ) where

import           Control.Monad (when)
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Tracer
import           Data.Functor.Contravariant ((>$<))
import           Data.Kind (Type)
import           Data.Word
import           GHC.Generics hiding (from)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Stream
import           Ouroboros.Consensus.Storage.LedgerDB.API
import           Ouroboros.Consensus.Storage.LedgerDB.API.Config
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Args
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Common
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Block
import           System.FS.API

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
    -- NOTE: We should /only/ see this if data corruption occurred.
  | InitFailure DiskSnapshot (SnapshotFailure blk) (InitLog blk)
  deriving (Show, Eq, Generic)

-- | Functions required to initialize a LedgerDB
type InitDB :: Type -> (Type -> Type) -> Type -> Type
data InitDB db m blk = InitDB {
    initFromGenesis  :: !(m db)
    -- ^ Create a DB from the genesis state
  , initFromSnapshot :: !(DiskSnapshot -> m (Either (SnapshotFailure blk) (db, RealPoint blk)))
    -- ^ Create a DB from a Snapshot
  , closeDb          :: !(db -> m ())
    -- ^ Closing the database, to be reopened again with a different snapshot or
    -- with the genesis state.
  , initReapplyBlock :: !(LedgerDbCfg (ExtLedgerState blk) -> blk -> db -> m db)
    -- ^ Reapply a block from the immutable DB when initializing the DB.
  , currentTip       :: !(db -> LedgerState blk EmptyMK)
    -- ^ Getting the current tip for tracing the Ledger Events.
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
-- Note that after initialization, the ledger db should be pruned so that no
-- ledger states are considered volatile. Otherwise we would be able to rollback
-- the immutable DB.
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
  -> m (InitLog blk, db, Word64)
initialize replayTracer
           snapTracer
           hasFS
           cfg
           stream
           replayGoal
           dbIface
           fromSnapshot =
    case fromSnapshot of
      Nothing   -> listSnapshots hasFS >>= tryNewestFirst id
      Just snap -> tryNewestFirst id [snap]
  where
    InitDB {initFromGenesis, initFromSnapshot, closeDb} = dbIface

    tryNewestFirst :: (InitLog blk -> InitLog blk)
                   -> [DiskSnapshot]
                   -> m ( InitLog   blk
                        , db
                        , Word64
                        )
    tryNewestFirst acc [] = do
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
          return ( acc InitFromGenesis
                 , db
                 , replayed
                 )

    tryNewestFirst acc (s:ss) = do
      eInitDb <- initFromSnapshot s
      case eInitDb of
        Left err -> do
          when (diskSnapshotIsTemporary s || err == InitFailureGenesis) $
            deleteSnapshot hasFS s
          traceWith snapTracer . InvalidSnapshot s $ err
          tryNewestFirst (acc . InitFailure s err) ss
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
              when (diskSnapshotIsTemporary s) $ deleteSnapshot hasFS s
              closeDb initDb
              tryNewestFirst (acc . InitFailure s err) ss
            Right (db, replayed) -> do
              return (acc (InitFromSnapshot s pt), db, replayed)

    replayTracer' = decorateReplayTracerWithGoal
                                       replayGoal
                                       (TraceReplayProgressEvent >$< replayTracer)

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
  Opening a LedgerDB
-------------------------------------------------------------------------------}

openDB ::
  forall m blk db. ( IOLike m
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  , HasCallStack
  )
  => Complete LedgerDbArgs m blk
  -> InitDB db m blk
  -> StreamAPI m blk blk
  -> Point blk
  -> m (LedgerDB' m blk, Word64)
openDB args initDb stream replayGoal =
    f <$> openDBInternal args initDb stream replayGoal
  where f (ldb, replayCounter, _) = (ldb, replayCounter)

-- | Open the ledger DB and expose internals for testing purposes
openDBInternal ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  , HasCallStack
  )
  => Complete LedgerDbArgs m blk
  -> InitDB db m blk
  -> StreamAPI m blk blk
  -> Point blk
  -> m (LedgerDB' m blk, Word64, TestInternals' m blk)
openDBInternal args@(LedgerDbArgs { lgrHasFS = SomeHasFS fs }) initDb stream replayGoal = do
    createDirectoryIfMissing fs True (mkFsPath [])
    (_initLog, db, replayCounter) <-
          initialize
            replayTracer
            snapTracer
            lgrHasFS
            lgrConfig
            stream
            replayGoal
            initDb
            lgrStartSnapshot
    (ledgerDb, internal) <- mkLedgerDb initDb db
    return (ledgerDb, replayCounter, internal)

  where
    LedgerDbArgs {
        lgrConfig
      , lgrTracer
      , lgrHasFS
      , lgrStartSnapshot
      } = args

    replayTracer = LedgerReplayEvent     >$< lgrTracer
    snapTracer   = LedgerDBSnapshotEvent >$< lgrTracer
