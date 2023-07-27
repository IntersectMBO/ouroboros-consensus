{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | LedgerDB initialization either from a LedgerState or from a DiskSnapshot
module Ouroboros.Consensus.Storage.LedgerDB.Impl (
    -- * LedgerDB
    LedgerDBState (..)
  , Snapshots.LedgerDbSerialiseConstraints
  , mkLedgerDB
  , openDB
    -- * Initialization
  , BackingStoreSelector (..)
  , InitLog (..)
  , ReplayStart (..)
  , initialize
  , newBackingStore
  , newBackingStoreInitialiser
  , restoreBackingStore
    -- * Trace
  , BackingStoreTrace (..)
  , ReplayGoal (..)
  , TraceBackingStoreInitEvent (..)
  , TraceLedgerDBEvent (..)
  , TraceReplayEvent (..)
  , decorateReplayTracerWithGoal
  , decorateReplayTracerWithStart
  ) where

import           Codec.CBOR.Decoding (Decoder)
import           Codec.Serialise (Serialise (decode))
import           Control.Monad.Except
import           Control.Tracer
import           Data.Functor.Contravariant ((>$<))
import           Data.Functor.Identity
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack (HasCallStack)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config (configCodec)
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDB)
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl.Stream
import           Ouroboros.Consensus.Storage.LedgerDB.API
import           Ouroboros.Consensus.Storage.LedgerDB.Args
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore hiding
                     (BackingStoreTrace)
import           Ouroboros.Consensus.Storage.LedgerDB.BackingStore.Init
import           Ouroboros.Consensus.Storage.LedgerDB.Config
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog
                     (DbChangelog (..), DbChangelog', onChangelog, onChangelogM)
import qualified Ouroboros.Consensus.Storage.LedgerDB.DbChangelog as DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog.Query
import           Ouroboros.Consensus.Storage.LedgerDB.DbChangelog.Update
                     (Ap (..))
import qualified Ouroboros.Consensus.Storage.LedgerDB.DbChangelog.Update as DbChangelog
import           Ouroboros.Consensus.Storage.LedgerDB.Lock hiding
                     (withWriteLock)
import qualified Ouroboros.Consensus.Storage.LedgerDB.Lock as Lock
import qualified Ouroboros.Consensus.Storage.LedgerDB.Query as Query
import           Ouroboros.Consensus.Storage.LedgerDB.ReadsKeySets
import qualified Ouroboros.Consensus.Storage.LedgerDB.Snapshots as Snapshots
import           Ouroboros.Consensus.Storage.LedgerDB.Types
import qualified Ouroboros.Consensus.Storage.LedgerDB.Update as Update
import           Ouroboros.Consensus.Storage.Serialisation
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Block (Point (Point))
import           System.FS.API
import           System.FS.API.Types (mkFsPath)
{-------------------------------------------------------------------------------
  Initialize the DB
-------------------------------------------------------------------------------}

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
  | InitFromSnapshot Snapshots.DiskSnapshot (RealPoint blk)

    -- | Initialization skipped a snapshot
    --
    -- We record the reason why it was skipped.
    --
    -- NOTE: We should /only/ see this if data corrupted occurred.
  | InitFailure Snapshots.DiskSnapshot (Snapshots.SnapshotFailure blk) (InitLog blk)
  deriving (Show, Eq, Generic)

-- | Open the ledger DB
--
-- In addition to the ledger DB also returns the number of immutable blocks that
-- were replayed, and the number of blocks since the last flush.
openDB :: forall m blk.
          ( IOLike m
          , LedgerSupportsProtocol blk
          , Snapshots.LedgerDbSerialiseConstraints blk
          , InspectLedger blk
          , HasCallStack
          )
       => LedgerDBArgs Identity m blk
       -- ^ Stateless initializaton arguments
       -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
       -- ^ Used to trace the progress while replaying blocks against the
       -- ledger.
       -> ImmutableDB m blk
       -- ^ Reference to the immutable DB
       --
       -- After reading a snapshot from disk, the ledger DB will be brought
       -- up to date with tip of the immutable DB. The corresponding ledger
       -- state can then be used as the starting point for chain selection in
       -- the ChainDB driver.
       -> (RealPoint blk -> m blk)
       -- ^ Read a block from disk
       --
       -- The block may be in the immutable DB or in the volatile DB; the ledger
       -- DB does not know where the boundary is at any given point.
       -> m (LedgerDB m blk, Word64)
openDB args@LedgerDBArgs { lgrHasFS = SomeHasFS fs } replayTracer immutableDB getBlock = do
    createDirectoryIfMissing fs True (mkFsPath [])
    (db, replayCounter, lgrBackingStore) <- initFromDisk args replayTracer (lgrDiskPolicy args) immutableDB
    -- When initializing the ledger DB from disk we:
    --
    -- - Look for the newest valid snapshot, say 'Lbs', which corresponds to the
    --   application of a block in the immutable DB, say 'b'.
    --
    -- - Push onto the ledger DB all the ledger states that result from applying
    --   blocks found in the on-disk immutable DB, starting from the successor
    --   of 'b'.
    --
    -- The anchor of 'LedgerDB' must be the oldest point we can rollback to. So
    -- if we follow the procedure described above (that 'initFromDisk'
    -- implements), the newest ledger state in 'db', say 'Lbn' corresponds to
    -- the most recent block in the immutable DB. If this block is in the
    -- immutable DB, it means that at some point it was part of a chain that was
    -- >k blocks long. Thus 'Lbn' is the oldest point we can roll back to.
    -- Therefore, we need to make the newest state (current) of the ledger DB
    -- the anchor.
    let dbPrunedToImmDBTip = onChangelog (DbChangelog.prune (SecurityParam 0)) db
    (varDB, prevApplied) <-
      (,) <$> newTVarIO dbPrunedToImmDBTip <*> newTVarIO Set.empty
    flushLock <- mkLedgerDBLock
    let st = LedgerDBState {
                 ldbChangelog    = varDB
               , varPrevApplied  = prevApplied
               , ldbBackingStore = lgrBackingStore
               , ldbLock         = flushLock
               }
    ledgerDB <- mkLedgerDB st args getBlock
    return (ledgerDB, replayCounter)

-- | Construct a ledger db from the given state
mkLedgerDB ::
  forall m blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , Snapshots.LedgerDbSerialiseConstraints blk
  , HasCallStack
  )
  => LedgerDBState m blk
  -> LedgerDBArgs Identity m blk
  -> DbChangelog.ResolveBlock m blk
  -> m (LedgerDB m blk)
mkLedgerDB st args getBlock = do
  h <- fmap LDBHandle $ newTVarIO $ LedgerDBOpen st
  return $ LedgerDB {
      getCurrent = getStateSTM h Query.getCurrent

    , getPrevApplied = getStateSTM h Query.getPrevApplied

    , getLedgerTablesAtFor = \pt k -> getState h $ \st' ->
        Query.getLedgerTablesAtFor pt k (ldbChangelog st') (ldbBackingStore st')

    , acquireLDBReadView = \se stm -> getState h $ \st' ->
        Query.acquireLDBReadView
          se
          (ldbChangelog st')
          (ldbLock st')
          (ldbBackingStore st')
          policy
          stm

    , garbageCollect = getStateSTM1 h $ \st' ->
        Update.garbageCollectPrevApplied (varPrevApplied st')

    , setCurrent = getStateSTM1 h $ \st' ->
        Update.setCurrent (ldbChangelog st')

    , tryFlush = getState h $ \st' -> do
        ldb <- atomically $ Query.getCurrent st'
        when (onDiskShouldFlush policy $ flushableLength $ anchorlessChangelog ldb)
           (Lock.withWriteLock
              (ldbLock st')
              (Update.flushLedgerDB (ldbChangelog st') (ldbBackingStore st'))
           )

    , validate = \ldbh chg cache rb tr hs -> getState h $ \st' ->
        Update.validate (varPrevApplied st') getBlock cfg ldbh chg cache rb tr hs

    , tryTakeSnapshot = getState2 h $ \st' mTime nrBlocks ->
        if onDiskShouldTakeSnapshot policy (uncurry (flip diffTime) <$> mTime) nrBlocks then do
          void $ Snapshots.takeSnapshot
                   (ldbChangelog st')
                   (ldbLock st')
                   (configCodec cfg)
                   (LedgerDBSnapshotEvent >$< tracer)
                   fs
                   (ldbBackingStore st')
          void $ Snapshots.trimSnapshots
                   (LedgerDBSnapshotEvent >$< tracer)
                   fs
                   policy
          (`SnapCounters` 0) . Just <$> maybe getMonotonicTime (pure . snd) mTime
        else
          pure $ SnapCounters (fst <$> mTime) nrBlocks
    }
  where
    LedgerDBArgs {
        lgrTopLevelConfig = cfg
      , lgrHasFS = fs
      , lgrTracer = tracer
      , lgrDiskPolicy = policy
      } = args

initFromDisk
  :: forall blk m.
     ( IOLike m
     , LedgerSupportsProtocol blk
     , Snapshots.LedgerDbSerialiseConstraints blk
     , InspectLedger blk
     , HasCallStack
     )
  => LedgerDBArgs Identity m blk
  -> Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> DiskPolicy
  -> ImmutableDB m blk
  -> m (DbChangelog' blk, Word64, LedgerBackingStore' m blk)
initFromDisk args
             replayTracer
             policy
             immutableDB = do
    (_initLog, db, replayCounter, backingStore) <-
      initialize
        replayTracer
        lgrTracer
        lgrHasFS
        decodeExtLedgerState'
        decode
        (configLedgerDb lgrTopLevelConfig)
        policy
        lgrGenesis
        (streamAPI immutableDB)
        lgrBackingStoreSelector
    return (db, replayCounter, backingStore)
  where
    LedgerDBArgs {
      lgrHasFS
      , lgrTracer
      , lgrTopLevelConfig
      , lgrGenesis
      , lgrBackingStoreSelector
      } = args

    ccfg = configCodec lgrTopLevelConfig

    decodeExtLedgerState' :: forall s. Decoder s (ExtLedgerState blk EmptyMK)
    decodeExtLedgerState' = decodeExtLedgerState
                              (decodeDisk ccfg)
                              (decodeDisk ccfg)
                              (decodeDisk ccfg)

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
-- * they are /ahead/ of the chain
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
  => Tracer m (ReplayGoal blk -> TraceReplayEvent blk)
  -> Tracer m (TraceLedgerDBEvent blk)
  -> SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk EmptyMK))
  -> (forall s. Decoder s (HeaderHash blk))
  -> LedgerDbCfg (ExtLedgerState blk)
  -> DiskPolicy
  -> m (ExtLedgerState blk ValuesMK) -- ^ Genesis ledger state
  -> StreamAPI m blk blk
  -> BackingStoreSelector m
  -> m (InitLog blk, DbChangelog' blk, Word64, LedgerBackingStore' m blk)
initialize replayTracer
           tracer
           hasFS
           decLedger
           decHash
           cfg
           policy
           getGenesisLedger
           stream
           bss =
    Snapshots.listSnapshots hasFS >>= tryNewestFirst id
  where
    lbsi ::
         (HasLedgerTables l, NoThunks (LedgerTables l ValuesMK)
         , CanSerializeLedgerTables l)
      => BackingStoreInitializer m l
    lbsi = newBackingStoreInitialiser (BackingStoreEvent >$< tracer) bss

    bsiTrace :: TraceBackingStoreInitEvent
    bsiTrace = case bss of
      InMemoryBackingStore    -> BackingStoreInitialisedInMemory
      LMDBBackingStore limits -> BackingStoreInitialisedLMDB limits

    tryNewestFirst :: (InitLog blk -> InitLog blk)
                   -> [Snapshots.DiskSnapshot]
                   -> m ( InitLog   blk
                        , DbChangelog' blk
                        , Word64
                        , LedgerBackingStore' m blk
                        )
    tryNewestFirst acc [] = do
      -- We're out of snapshots. Start at genesis
      traceWith replayTracer ReplayFromGenesis
      genesisLedger <- getGenesisLedger
      let replayTracer' = decorateReplayTracerWithStart (Point Origin) replayTracer
          initDb        = DbChangelog.empty (forgetLedgerTables genesisLedger)
      backingStore <-
          newBackingStore lbsi hasFS (projectLedgerTables genesisLedger)
      traceWith (BackingStoreInitEvent >$< tracer) bsiTrace
      eDB <- runExceptT $ replayStartingWith
                            replayTracer'
                            cfg
                            policy
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
      eExtLedgerSt <- runExceptT $ Snapshots.readSnapshot hasFS decLedger decHash s
      case eExtLedgerSt of
        Left err -> do
          when (Snapshots.diskSnapshotIsTemporary s) $
            Snapshots.deleteSnapshot hasFS s
          traceWith tracer . LedgerDBSnapshotEvent . Snapshots.InvalidSnapshot s . Snapshots.InitFailureRead $ err
          tryNewestFirst (acc . InitFailure s (Snapshots.InitFailureRead err)) ss
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
              Snapshots.deleteSnapshot hasFS s
              traceWith tracer . LedgerDBSnapshotEvent . Snapshots.InvalidSnapshot s $ Snapshots.InitFailureGenesis
              tryNewestFirst (acc . InitFailure s Snapshots.InitFailureGenesis) []

            NotOrigin pt -> do
              backingStore <- restoreBackingStore lbsi hasFS s
              traceWith (BackingStoreInitEvent >$< tracer) bsiTrace
              traceWith replayTracer $
                ReplayFromSnapshot s pt (ReplayStart initialPoint)
              let tracer' = decorateReplayTracerWithStart initialPoint replayTracer
                  initDb  = DbChangelog.empty extLedgerSt
              eDB <- runExceptT $ replayStartingWith
                                    tracer'
                                    cfg
                                    policy
                                    backingStore
                                    stream
                                    initDb
              case eDB of
                Left err -> do
                  traceWith tracer . LedgerDBSnapshotEvent . Snapshots.InvalidSnapshot s $ err
                  when (Snapshots.diskSnapshotIsTemporary s) $ Snapshots.deleteSnapshot hasFS s
                  bsClose backingStore
                  tryNewestFirst (acc . InitFailure s err) ss
                Right (db, replayed) -> do
                  return (acc (InitFromSnapshot s pt), db, replayed, backingStore)

-- | Replay all blocks in the Immutable database using the 'StreamAPI' provided
-- on top of the given @LedgerDB' blk@.
--
-- It will also return the number of blocks that were replayed.
--
-- Note we do flush differences into the 'BackingStore' as we go, but we don't
-- take snapshots of the in-memory parts.
--
-- TODO: #4402 expose the flushing frequence as a configuration
replayStartingWith ::
     forall m blk. (
         IOLike m
       , LedgerSupportsProtocol blk
       , InspectLedger blk
       , HasCallStack
       )
  => Tracer m (ReplayStart blk -> ReplayGoal blk -> TraceReplayEvent blk)
  -> LedgerDbCfg (ExtLedgerState blk)
  -> DiskPolicy
  -> LedgerBackingStore' m blk
  -> StreamAPI m blk blk
  -> DbChangelog' blk
  -> ExceptT (Snapshots.SnapshotFailure blk) m (DbChangelog' blk, Word64)
replayStartingWith tracer cfg policy backingStore stream initDb = do
    streamAll stream (castPoint (tip $ anchorlessChangelog initDb))
        Snapshots.InitFailureTooRecent
        (initDb, 0)
        push
  where
    DiskPolicy { onDiskShouldFlush } = policy

    push :: blk
         -> (DbChangelog' blk, Word64)
         -> m (DbChangelog' blk, Word64)
    push blk (!db, !replayed) = do
        !db' <- onChangelogM (DbChangelog.applyThenPush cfg (ReapplyVal blk) (readKeySets backingStore)) db

        -- It's OK to flush without a lock here, since the `LedgerDB` has not
        -- finishined initializing: only this thread has access to the backing
        -- store.
        db'' <-
          if onDiskShouldFlush (flushableLength $ anchorlessChangelog db')
          then do
            let (toFlush, toKeep) = DbChangelog.splitForFlushing db'
            mapM_ (Update.flushIntoBackingStore backingStore) toFlush
            pure toKeep
          else pure db'

        let replayed' :: Word64
            !replayed' = replayed + 1

            events :: [LedgerEvent blk]
            events = inspectLedger
                       (getExtLedgerCfg (ledgerDbCfg cfg))
                       (ledgerState (current $ anchorlessChangelog db))
                       (ledgerState (current $ anchorlessChangelog db''))

        traceWith tracer (ReplayedBlock (blockRealPoint blk) events)
        return (db'', replayed')

{-------------------------------------------------------------------------------
  Trace events
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
        Snapshots.DiskSnapshot
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
