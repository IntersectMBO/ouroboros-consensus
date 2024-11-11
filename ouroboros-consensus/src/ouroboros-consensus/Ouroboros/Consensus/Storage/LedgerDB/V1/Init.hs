{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Storage.LedgerDB.V1.Init (mkInitDb) where

import           Control.Monad
import           Control.ResourceRegistry
import           Data.Bifunctor (first)
import qualified Data.Foldable as Foldable
import           Data.Functor.Contravariant ((>$<))
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.HeaderStateHistory
                     (HeaderStateHistory (..), mkHeaderStateWithTimeFromSummary)
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import           Ouroboros.Consensus.Storage.LedgerDB.API
import           Ouroboros.Consensus.Storage.LedgerDB.API.Config
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Args
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Common
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Init
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.Impl.Validate as Validate
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Args as V1
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as BS
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Common
import           Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog as DbCh
                     (empty, flushableLength)
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Flush
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Forker
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Lock
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Snapshots
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Network.AnchoredSeq as AS
import           System.FS.API

mkInitDb ::
  forall m blk.
  ( LedgerSupportsProtocol blk
  , IOLike m
  , LedgerDbSerialiseConstraints blk
  , HasHardForkHistory blk
#if __GLASGOW_HASKELL__ < 906
  , HasAnnTip blk
#endif
  )
  => Complete LedgerDbArgs m blk
  -> Complete V1.LedgerDbFlavorArgs m
  -> Validate.ResolveBlock m blk
  -> InitDB (DbChangelog' blk, BackingStore' m blk) m blk
mkInitDb args bss getBlock =
  InitDB {
    initFromGenesis = do
      st <- lgrGenesis
      let chlog = DbCh.empty (forgetLedgerTables st)
      (_, backingStore) <-
        allocate
          lgrRegistry
          (\_ -> newBackingStore bsTracer baArgs lgrHasFS' (projectLedgerTables st))
          bsClose
      pure (chlog, backingStore)
  , initFromSnapshot =
      loadSnapshot bsTracer baArgs (configCodec . getExtLedgerCfg . ledgerDbCfg $ lgrConfig) lgrHasFS'
  , closeDb = bsClose . snd
  , initReapplyBlock = \cfg blk (chlog, bstore) -> do
      !chlog' <- reapplyThenPush cfg blk (readKeySets bstore) chlog
      -- It's OK to flush without a lock here, since the `LedgerDB` has not
      -- finishined initializing: only this thread has access to the backing
      -- store.
      chlog'' <- unsafeIgnoreWriteLock
        $ if shouldFlush flushFreq (flushableLength chlog')
          then do
            let (toFlush, toKeep) = splitForFlushing chlog'
            mapM_ (flushIntoBackingStore bstore) toFlush
            pure toKeep
          else pure chlog'
      pure (chlog'', bstore)
  , currentTip = ledgerState . current . fst
  , pruneDb = pure . first pruneToImmTipOnly
  , mkLedgerDb = \(db, lgrBackingStore) -> do
      (varDB, prevApplied) <-
        (,) <$> newTVarIO db <*> newTVarIO Set.empty
      flushLock <- mkLedgerDBLock
      forkers <- newTVarIO Map.empty
      nextForkerKey <- newTVarIO (ForkerKey 0)
      let env = LedgerDBEnv {
                 ldbChangelog       = varDB
               , ldbBackingStore    = lgrBackingStore
               , ldbLock            = flushLock
               , ldbPrevApplied     = prevApplied
               , ldbForkers         = forkers
               , ldbNextForkerKey   = nextForkerKey
               , ldbSnapshotPolicy  = defaultSnapshotPolicy (ledgerDbCfgSecParam lgrConfig) lgrSnapshotPolicyArgs
               , ldbTracer          = lgrTracer
               , ldbCfg             = lgrConfig
               , ldbHasFS           = lgrHasFS'
               , ldbShouldFlush     = shouldFlush flushFreq
               , ldbQueryBatchSize  = queryBatchSizeArg
               , ldbResolveBlock    = getBlock
               }
      h <- LDBHandle <$> newTVarIO (LedgerDBOpen env)
      pure $ implMkLedgerDb h
  }
  where
    bsTracer = LedgerDBFlavorImplEvent . FlavorImplSpecificTraceV1 >$< lgrTracer

    LedgerDbArgs {
        lgrHasFS
      , lgrTracer
      , lgrSnapshotPolicyArgs
      , lgrConfig
      , lgrGenesis
      , lgrRegistry
      } = args

    lgrHasFS' = SnapshotsFS lgrHasFS

    V1Args flushFreq queryBatchSizeArg baArgs = bss

implMkLedgerDb ::
     forall m l blk.
     ( IOLike m
     , HasCallStack
     , StandardHash l
     , LedgerDbSerialiseConstraints blk
     , LedgerSupportsProtocol blk
     , ApplyBlock l blk
     , l ~ ExtLedgerState blk
#if __GLASGOW_HASKELL__ < 906
     , HasAnnTip blk
#endif
     , HasHardForkHistory blk
     )
  => LedgerDBHandle m l blk
  -> (LedgerDB' m blk, TestInternals' m blk)
implMkLedgerDb h = (LedgerDB {
      getVolatileTip            = getEnvSTM  h implGetVolatileTip
    , getImmutableTip           = getEnvSTM  h implGetImmutableTip
    , getPastLedgerState        = getEnvSTM1 h implGetPastLedgerState
    , getHeaderStateHistory     = getEnvSTM  h implGetHeaderStateHistory
    , getForkerAtTarget         = newForkerAtTarget h
    , validate                  = getEnv5    h (implValidate h)
    , getPrevApplied            = getEnvSTM  h implGetPrevApplied
    , garbageCollect            = getEnvSTM1 h implGarbageCollect
    , tryTakeSnapshot           = getEnv2    h implTryTakeSnapshot
    , tryFlush                  = getEnv     h implTryFlush
    , closeDB                   = implCloseDB h
    }, mkInternals h)

implGetVolatileTip ::
     (MonadSTM m, GetTip l)
  => LedgerDBEnv m l blk
  -> STM m (l EmptyMK)
implGetVolatileTip = fmap current . readTVar . ldbChangelog

implGetImmutableTip ::
     MonadSTM m
  => LedgerDBEnv m l blk
  -> STM m (l EmptyMK)
implGetImmutableTip = fmap anchor . readTVar . ldbChangelog

implGetPastLedgerState ::
     ( MonadSTM m , HasHeader blk, IsLedger l, StandardHash l
     , HasLedgerTables l, HeaderHash l ~ HeaderHash blk )
  => LedgerDBEnv m l blk -> Point blk -> STM m (Maybe (l EmptyMK))
implGetPastLedgerState env point = getPastLedgerAt point <$> readTVar (ldbChangelog env)

implGetHeaderStateHistory ::
     ( MonadSTM m
     , l ~ ExtLedgerState blk
     , IsLedger (LedgerState blk)
     , HasHardForkHistory blk
     , HasAnnTip blk
     )
  => LedgerDBEnv m l blk -> STM m (HeaderStateHistory blk)
implGetHeaderStateHistory env = do
    ldb <- readTVar (ldbChangelog env)
    let currentLedgerState = ledgerState $ current ldb
        -- This summary can convert all tip slots of the ledger states in the
        -- @ledgerDb@ as these are not newer than the tip slot of the current
        -- ledger state (Property 17.1 in the Consensus report).
        summary = hardForkSummary (configLedger $ getExtLedgerCfg $ ledgerDbCfg $ ldbCfg env) currentLedgerState
        mkHeaderStateWithTime' =
              mkHeaderStateWithTimeFromSummary summary
            . headerState
    pure
      . HeaderStateHistory
      . AS.bimap mkHeaderStateWithTime' mkHeaderStateWithTime'
      $ changelogStates ldb

implValidate ::
     forall m l blk. (
       IOLike m
     , LedgerSupportsProtocol blk
     , HasCallStack
     , l ~ ExtLedgerState blk
     )
  => LedgerDBHandle m l blk
  -> LedgerDBEnv m l blk
  -> ResourceRegistry m
  -> (TraceValidateEvent blk -> m ())
  -> BlockCache blk
  -> Word64
  -> [Header blk]
  -> m (ValidateResult m (ExtLedgerState blk) blk)
implValidate h ldbEnv rr tr cache rollbacks hdrs =
  Validate.validate $
    Validate.ValidateArgs
      (ldbResolveBlock ldbEnv)
      (getExtLedgerCfg . ledgerDbCfg $ ldbCfg ldbEnv)
      (\l -> do
          prev <- readTVar (ldbPrevApplied ldbEnv)
          writeTVar (ldbPrevApplied ldbEnv) (Foldable.foldl' (flip Set.insert) prev l))
      (readTVar (ldbPrevApplied ldbEnv))
      (newForkerByRollback h)
      rr
      tr
      cache
      rollbacks
      hdrs

implGetPrevApplied :: MonadSTM m => LedgerDBEnv m l blk -> STM m (Set (RealPoint blk))
implGetPrevApplied env = readTVar (ldbPrevApplied env)

-- | Remove all points with a slot older than the given slot from the set of
-- previously applied points.
implGarbageCollect :: MonadSTM m => LedgerDBEnv m l blk -> SlotNo -> STM m ()
implGarbageCollect env slotNo = modifyTVar (ldbPrevApplied env) $
    Set.dropWhileAntitone ((< slotNo) . realPointSlot)

implTryTakeSnapshot ::
     ( l ~ ExtLedgerState blk
     , IOLike m, LedgerDbSerialiseConstraints blk, LedgerSupportsProtocol blk
     )
  => LedgerDBEnv m l blk -> Maybe (Time, Time) -> Word64 -> m SnapCounters
implTryTakeSnapshot env mTime nrBlocks =
    if onDiskShouldTakeSnapshot (ldbSnapshotPolicy env) (uncurry (flip diffTime) <$> mTime) nrBlocks then do
      void $ withReadLock (ldbLock env) (takeSnapshot
                                          (ldbChangelog env)
                                          (configCodec . getExtLedgerCfg . ledgerDbCfg $ ldbCfg env)
                                          (LedgerDBSnapshotEvent >$< ldbTracer env)
                                          (ldbHasFS env)
                                          (ldbBackingStore env)
                                          Nothing)
      void $ trimSnapshots
                (LedgerDBSnapshotEvent >$< ldbTracer env)
                (snapshotsFs $ ldbHasFS env)
                (ldbSnapshotPolicy env)
      (`SnapCounters` 0) . Just <$> maybe getMonotonicTime (pure . snd) mTime
    else
      pure $ SnapCounters (fst <$> mTime) nrBlocks

-- If the DbChangelog in the LedgerDB can flush (based on the SnapshotPolicy
-- with which this LedgerDB was opened), flush differences to the backing
-- store. Note this acquires a write lock on the backing store.
implTryFlush ::
     (IOLike m, HasLedgerTables l, GetTip l)
  => LedgerDBEnv m l blk -> m ()
implTryFlush env = do
    ldb <- readTVarIO $ ldbChangelog env
    when (ldbShouldFlush env $ DbCh.flushableLength ldb)
        (withWriteLock
          (ldbLock env)
          (flushLedgerDB (ldbChangelog env) (ldbBackingStore env))
        )

implCloseDB :: IOLike m => LedgerDBHandle m l blk -> m ()
implCloseDB (LDBHandle varState) = do
    mbOpenEnv <- atomically $ readTVar varState >>= \case
      -- Idempotent
      LedgerDBClosed   -> return Nothing
      LedgerDBOpen env -> do
        writeTVar varState LedgerDBClosed
        return $ Just env

    -- Only when the LedgerDB was open
    whenJust mbOpenEnv $ \env -> do
      closeAllForkers env
      bsClose (ldbBackingStore env)

mkInternals ::
     ( IOLike m
     , LedgerDbSerialiseConstraints blk
     , LedgerSupportsProtocol blk
     , ApplyBlock (ExtLedgerState blk) blk
     )
  => LedgerDBHandle m (ExtLedgerState blk) blk
  -> TestInternals' m blk
mkInternals h = TestInternals {
      takeSnapshotNOW = getEnv2 h implIntTakeSnapshot
    , reapplyThenPushNOW = getEnv1 h implIntReapplyThenPushBlock
    , wipeLedgerDB = getEnv h $ void . destroySnapshots . snapshotsFs . ldbHasFS
    , closeLedgerDB = getEnv h $ bsClose . ldbBackingStore
    , truncateSnapshots = getEnv h $ void . implIntTruncateSnapshots . ldbHasFS
    }

-- | Testing only! Truncate all snapshots in the DB.
implIntTruncateSnapshots :: MonadThrow m => SnapshotsFS m -> m ()
implIntTruncateSnapshots (SnapshotsFS (SomeHasFS fs)) = do
  dirs <- Set.lookupMax . Set.filter (isJust . snapshotFromPath) <$> listDirectory fs (mkFsPath [])
  mapM_ (truncateRecursively . (:[])) dirs
  where
    truncateRecursively pre = do
      dirs <- listDirectory fs (mkFsPath pre)
      mapM_ (\d -> do
            let d' = pre ++ [d]
            isDir <- doesDirectoryExist fs $ mkFsPath d'
            if isDir
              then truncateRecursively d'
              else withFile fs (mkFsPath d') (AppendMode AllowExisting) $ \h -> hTruncate fs h 0
        ) dirs

implIntTakeSnapshot ::
     ( IOLike m
     , LedgerDbSerialiseConstraints blk
     , LedgerSupportsProtocol blk
     , l ~ ExtLedgerState blk
     )
  => LedgerDBEnv m l blk -> WhereToTakeSnapshot -> Maybe String -> m ()
implIntTakeSnapshot env whereTo suffix = do
  when (whereTo == TakeAtVolatileTip) $ atomically $ modifyTVar (ldbChangelog env) pruneToImmTipOnly
  withWriteLock
          (ldbLock env)
          (flushLedgerDB (ldbChangelog env) (ldbBackingStore env))
  void $ withReadLock (ldbLock env) $
    takeSnapshot
      (ldbChangelog env)
      (configCodec . getExtLedgerCfg . ledgerDbCfg $ ldbCfg env)
      (LedgerDBSnapshotEvent >$< ldbTracer env)
      (ldbHasFS env)
      (ldbBackingStore env)
      suffix

implIntReapplyThenPushBlock ::
     ( IOLike m
     , ApplyBlock l blk
     , l ~ ExtLedgerState blk
     )
  => LedgerDBEnv m l blk -> blk -> m ()
implIntReapplyThenPushBlock env blk = do
  chlog <- readTVarIO $ ldbChangelog env
  chlog' <- reapplyThenPush (ldbCfg env)  blk (readKeySets (ldbBackingStore env)) chlog
  atomically $ writeTVar (ldbChangelog env) chlog'
