{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Ouroboros.Consensus.Storage.LedgerDB.V1.Init (
    InitLog (..)
  , initialize
  ) where

import           Codec.CBOR.Decoding
import           Control.Monad (when)
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Tracer
import           Data.Functor.Contravariant
import           Data.Word
import           GHC.Generics
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Stream
import           Ouroboros.Consensus.Storage.LedgerDB.API
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog as DbCh
                     (empty)
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Flush
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Snapshots
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Block
import           System.FS.API

-- | Initialization log
--
-- The initialization log records which snapshots from disk were considered,
-- in which order, and why some snapshots were rejected. It is primarily useful
-- for monitoring purposes.
data InitLog blk =
    -- | Defaulted to initialization from genesis
    --
    -- NOTE: Unless the blockchain is near genesis, we should see this /only/
    -- if data corrupted occurred.
    InitFromGenesis

    -- | Used a snapshot corresponding to the specified tip
  | InitFromSnapshot DiskSnapshot (RealPoint blk)

    -- | Initialization skipped a snapshot
    --
    -- We record the reason why it was skipped.
    --
    -- NOTE: We should /only/ see this if data corrupted occurred.
  | InitFailure DiskSnapshot (SnapshotFailure blk) (InitLog blk)
  deriving (Show, Eq, Generic)

-- | Initialize the ledger DB from the most recent snapshot on disk
--
-- If no such snapshot can be found, use the genesis ledger DB. Returns the
-- initialized DB as well as the block reference corresponding to the snapshot
-- we found on disk (the latter primarily for testing/monitoring purposes).
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
-- It is possible that the Ledger DB will not be able to roll back @k@ blocks
-- after initialization if the chain has been truncated (data corruption).
--
-- We do /not/ attempt to use multiple ledger states from disk to construct the
-- ledger DB. Instead we load only a /single/ ledger state from disk, and
-- /compute/ all subsequent ones. This is important, because the ledger states
-- obtained in this way will (hopefully) share much of their memory footprint
-- with their predecessors.
initialize ::
     forall m blk. (
         IOLike m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , CanSerializeLedgerTables (LedgerState blk)
       , HasCallStack
       )
  => Tracer m BackingStoreTraceByBackend
  -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> Tracer m (TraceLedgerDBEvent blk)
  -> SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk EmptyMK))
  -> (forall s. Decoder s (HeaderHash blk))
  -> DbChangelogCfg (ExtLedgerState blk)
  -> (Word64 -> Bool)
  -> m (ExtLedgerState blk ValuesMK) -- ^ Genesis ledger state
  -> StreamAPI m blk blk
  -> BackingStoreSelector m
  -> m (InitLog blk, DbChangelog' blk, Word64, BackingStore' m blk)
initialize bsTracer
           replayTracer
           tracer
           hasFS
           decLedger
           decHash
           cfg
           onDiskShouldFlush
           getGenesisLedger
           stream
           bss =
    listSnapshots hasFS >>= tryNewestFirst id
  where
    bsiTrace :: TraceBackingStoreInitEvent
    bsiTrace = case bss of
      InMemoryBackingStore    -> BackingStoreInitialisedInMemory
      LMDBBackingStore limits -> BackingStoreInitialisedLMDB limits

    tryNewestFirst :: (InitLog blk -> InitLog blk)
                   -> [DiskSnapshot]
                   -> m ( InitLog   blk
                        , DbChangelog' blk
                        , Word64
                        , BackingStore' m blk
                        )
    tryNewestFirst acc [] = do
      -- We're out of snapshots. Start at genesis
      traceWith replayTracer ReplayFromGenesis
      genesisLedger <- getGenesisLedger
      let replayTracer' = decorateReplayTracerWithStart (Point Origin) replayTracer
          initDb        = DbCh.empty (forgetLedgerTables genesisLedger)
      backingStore <-
          newBackingStore bsTracer bss hasFS (projectLedgerTables genesisLedger)
      traceWith (InitEvent >$< bsTracer) bsiTrace
      eDB <- runExceptT $ replayStartingWith
                            replayTracer'
                            cfg
                            onDiskShouldFlush
                            backingStore
                            stream
                            initDb
      case eDB of
        Left err -> do
          bsClose backingStore
          error $ "Invariant violation: invalid immutable chain " <> show err
        Right (db, replayed) -> do
          return ( acc InitFromGenesis
                 , db
                 , replayed
                 , backingStore
                 )

    tryNewestFirst acc (s:ss) = do
      eExtLedgerSt <- runExceptT $ readSnapshot hasFS decLedger decHash s
      case eExtLedgerSt of
        Left err -> do
          when (diskSnapshotIsTemporary s) $
            deleteSnapshot hasFS s
          traceWith tracer . LedgerDBSnapshotEvent . InvalidSnapshot s . InitFailureRead $ err
          tryNewestFirst (acc . InitFailure s (InitFailureRead err)) ss
        Right extLedgerSt -> do
          let initialPoint =
                  withOrigin (Point Origin) annTipPoint
                . headerStateTip
                . headerState
                $ extLedgerSt
          case pointToWithOriginRealPoint (castPoint (getTip extLedgerSt)) of
            Origin        -> do
              -- Delete the snapshot of the Genesis ledger state. It should have
              -- never existed.
              deleteSnapshot hasFS s
              traceWith tracer . LedgerDBSnapshotEvent . InvalidSnapshot s $ InitFailureGenesis
              tryNewestFirst (acc . InitFailure s InitFailureGenesis) []

            NotOrigin pt -> do
              backingStore <- restoreBackingStore bsTracer bss hasFS (snapshotToTablesPath s)
              traceWith (InitEvent >$< bsTracer) bsiTrace
              traceWith replayTracer $
                ReplayFromSnapshot s pt (ReplayStart initialPoint)
              let tracer' = decorateReplayTracerWithStart initialPoint replayTracer
                  initDb  = DbCh.empty extLedgerSt
              eDB <- runExceptT $ replayStartingWith
                                    tracer'
                                    cfg
                                    onDiskShouldFlush
                                    backingStore
                                    stream
                                    initDb
              case eDB of
                Left err -> do
                  traceWith tracer . LedgerDBSnapshotEvent . InvalidSnapshot s $ err
                  when (diskSnapshotIsTemporary s) $ deleteSnapshot hasFS s
                  bsClose backingStore
                  tryNewestFirst (acc . InitFailure s err) ss
                Right (db, replayed) -> do
                  return (acc (InitFromSnapshot s pt), db, replayed, backingStore)

-- | Replay all blocks in the Immutable database using the 'StreamAPI' provided
-- on top of the given @LedgerDB' blk@.
--
-- It will also return the number of blocks that were replayed.
--
-- NOTE: we do flush differences into the 'BackingStore' as we go, but we don't
-- take snapshots of the in-memory parts.
replayStartingWith ::
     forall m blk. (
         IOLike m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , HasCallStack
       )
  => Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk)
  -> DbChangelogCfg (ExtLedgerState blk)
  -> (Word64 -> Bool)
  -> BackingStore' m blk
  -> StreamAPI m blk blk
  -> DbChangelog' blk
  -> ExceptT (SnapshotFailure blk) m (DbChangelog' blk, Word64)
replayStartingWith tracer cfg onDiskShouldFlush backingStore stream initDb = do
    streamAll stream (castPoint (tip $ anchorlessChangelog initDb))
        InitFailureTooRecent
        (initDb, 0)
        push
  where
    push :: blk
         -> (DbChangelog' blk, Word64)
         -> m (DbChangelog' blk, Word64)
    push blk (!db, !replayed) = do
        !db' <- onChangelogM (applyThenPush cfg (ReapplyVal blk) (readKeySets backingStore)) db

        -- It's OK to flush without a lock here, since the `LedgerDB` has not
        -- finishined initializing: only this thread has access to the backing
        -- store.
        db'' <-
          if onDiskShouldFlush (flushableLength $ anchorlessChangelog db')
          then do
            let (toFlush, toKeep) = splitForFlushing db'
            mapM_ (flushIntoBackingStore backingStore) toFlush
            pure toKeep
          else pure db'

        let replayed' :: Word64
            !replayed' = replayed + 1

            events :: [LedgerEvent blk]
            events = inspectLedger
                       (getExtLedgerCfg (dbChangelogCfg cfg))
                       (ledgerState (current $ anchorlessChangelog db))
                       (ledgerState (current $ anchorlessChangelog db''))

        traceWith tracer (ReplayedBlock (blockRealPoint blk) events)
        return (db'', replayed')
