{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

#if __GLASGOW_HASKELL__ <= 906
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

module Ouroboros.Consensus.Storage.LedgerDB.V1.Init (mkInitDb) where

import Data.Maybe (isJust)
import           Control.Monad
import           Control.Monad.Base
import Control.Tracer (nullTracer)
import           Data.Foldable
import           Data.Functor.Contravariant ((>$<))
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HeaderStateHistory
                     (HeaderStateHistory (..))
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
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Network.AnchoredSeq (AnchoredSeq)
import qualified Ouroboros.Network.AnchoredSeq as AS
import System.FS.API

mkInitDb ::
  forall m blk.
  ( LedgerSupportsProtocol blk
  , IOLike m
  , LedgerDbSerialiseConstraints blk
  , MonadBase m m
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
          (\_ -> newBackingStore bsTracer baArgs ldbLiveTablesHasFS ldbTablesHasFS (projectLedgerTables st))
          bsClose
      pure (chlog, backingStore)
  , initFromSnapshot =
      loadSnapshot bsTracer baArgs (configCodec . getExtLedgerCfg . ledgerDbCfg $ lgrConfig) ldbStateHasFS ldbTablesHasFS ldbLiveTablesHasFS
  , closeDb = bsClose . snd
  , initReapplyBlock = \cfg blk (chlog, bstore) -> do
      !chlog' <- onChangelogM (reapplyThenPush cfg blk (readKeySets bstore)) chlog
      -- It's OK to flush without a lock here, since the `LedgerDB` has not
      -- finishined initializing: only this thread has access to the backing
      -- store.
      chlog'' <- unsafeIgnoreWriteLock
        $ if defaultShouldFlush flushFreq (flushableLength $ anchorlessChangelog chlog')
          then do
            let (toFlush, toKeep) = splitForFlushing chlog'
            mapM_ (flushIntoBackingStore bstore) toFlush
            pure toKeep
          else pure chlog'
      pure (chlog'', bstore)
  , currentTip = ledgerState . current . anchorlessChangelog . fst
  , mkLedgerDb = \(db, lgrBackingStore) -> do
      let dbPrunedToImmDBTip = onChangelog pruneToImmTipOnly db
      (varDB, prevApplied) <-
        (,) <$> newTVarIO dbPrunedToImmDBTip <*> newTVarIO Set.empty
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
               , ldbTablesHasFS
               , ldbStateHasFS
               , ldbLiveTablesHasFS
               , ldbShouldFlush     = defaultShouldFlush flushFreq
               , ldbQueryBatchSize  = queryBatchSize
               , ldbResolveBlock    = getBlock
               }
      h <- LDBHandle <$> newTVarIO (LedgerDBOpen env)
      pure $ implMkLedgerDb h
  }
  where
    ldbTablesHasFS     = if lgrSnapshotTablesSSD then lgrSSDHasFS else lgrHasFS
    ldbStateHasFS      = if lgrSnapshotStateSSD  then lgrSSDHasFS else lgrHasFS
    ldbLiveTablesHasFS = lgrSSDHasFS

    bsTracer = nullTracer --LedgerDBFlavorImplEvent . FlavorImplSpecificTraceV1 >$< lgrTracer

    LedgerDbArgs {
        lgrHasFS
      , lgrSSDHasFS
      , lgrSnapshotTablesSSD
      , lgrSnapshotStateSSD
      , lgrTracer
      , lgrSnapshotPolicyArgs
      , lgrConfig
      , lgrGenesis
      , lgrRegistry
      } = args

    V1Args flushFreq queryBatchSize baArgs = bss

implMkLedgerDb ::
     forall m l blk.
     ( IOLike m
     , HasCallStack
     , StandardHash l
     , LedgerDbSerialiseConstraints blk
     , LedgerSupportsProtocol blk
     , MonadBase m m
     , ApplyBlock l blk
     , l ~ ExtLedgerState blk
     )
  => LedgerDBHandle m l blk
  -> (LedgerDB' m blk, TestInternals' m blk)
implMkLedgerDb h = (LedgerDB {
      getVolatileTip            = getEnvSTM  h implGetVolatileTip
    , getImmutableTip           = getEnvSTM  h implGetImmutableTip
    , getPastLedgerState        = getEnvSTM1 h implGetPastLedgerState
    , getHeaderStateHistory     = getEnvSTM  h implGetHeaderStateHistory
    , getForkerAtWellKnownPoint = newForkerAtWellKnownPoint h
    , getForkerAtPoint          = newForkerAtPoint h
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
implGetVolatileTip = fmap (current . anchorlessChangelog) . readTVar . ldbChangelog

implGetImmutableTip ::
     MonadSTM m
  => LedgerDBEnv m l blk
  -> STM m (l EmptyMK)
implGetImmutableTip = fmap (anchor . anchorlessChangelog) . readTVar . ldbChangelog

implGetPastLedgerState ::
     ( MonadSTM m , HasHeader blk, IsLedger l, StandardHash l
     , HasLedgerTables l, HeaderHash l ~ HeaderHash blk )
  => LedgerDBEnv m l blk -> Point blk -> STM m (Maybe (l EmptyMK))
implGetPastLedgerState env point = getPastLedgerAt point . anchorlessChangelog <$> readTVar (ldbChangelog env)

implGetHeaderStateHistory ::
     (MonadSTM m, l ~ ExtLedgerState blk)
  => LedgerDBEnv m l blk -> STM m (HeaderStateHistory blk)
implGetHeaderStateHistory env = toHeaderStateHistory . adcStates . anchorlessChangelog <$> readTVar (ldbChangelog env)
  where
    toHeaderStateHistory ::
         AnchoredSeq (WithOrigin SlotNo) (ExtLedgerState blk EmptyMK) (ExtLedgerState blk EmptyMK)
      -> HeaderStateHistory blk
    toHeaderStateHistory =
          HeaderStateHistory
        . AS.bimap headerState headerState

implValidate ::
     forall m l blk. (
       IOLike m
     , LedgerSupportsProtocol blk
     , HasCallStack
     , l ~ ExtLedgerState blk
     , MonadBase m m
     )
  => LedgerDBHandle m l blk
  -> LedgerDBEnv m l blk
  -> ResourceRegistry m
  -> (TraceValidateEvent blk -> m ())
  -> BlockCache blk
  -> Word64
  -> [Header blk]
  -> m (ValidateResult m (ExtLedgerState blk) blk)
implValidate h ldbEnv =
  Validate.validate
    (ldbResolveBlock ldbEnv)
    (getExtLedgerCfg . ledgerDbCfg $ ldbCfg ldbEnv)
    (\l -> do
        prev <- readTVar (ldbPrevApplied ldbEnv)
        writeTVar (ldbPrevApplied ldbEnv) (foldl' (flip Set.insert) prev l))
    (readTVar (ldbPrevApplied ldbEnv))
    (newForkerAtFromTip h)


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
                                          (ldbStateHasFS env)
                                          (ldbBackingStore env)
                                          Nothing)
      void $ trimSnapshots
                (LedgerDBSnapshotEvent >$< ldbTracer env)
                [ldbStateHasFS env, ldbTablesHasFS env]
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
    when (ldbShouldFlush env $ DbCh.flushableLength $ anchorlessChangelog ldb)
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
     , MonadBase m m

     )
  => LedgerDBHandle m (ExtLedgerState blk) blk
  -> TestInternals' m blk
mkInternals h = TestInternals {
      takeSnapshotNOW = getEnv1 h implIntTakeSnapshot
    , reapplyThenPushNOW = getEnv1 h implIntReapplyThenPushBlock
    , wipeLedgerDB = getEnv h $ \env ->
        mapM_ destroySnapshots [ldbTablesHasFS env, ldbStateHasFS env]
    , closeLedgerDB = getEnv h $ \env -> bsClose $ ldbBackingStore env
    , truncateSnapshots = getEnv h $ \env ->
        mapM_ implIntTruncateSnapshots [ldbTablesHasFS env, ldbStateHasFS env]
    }

-- | Testing only! Destroy all snapshots in the DB.
destroySnapshots :: Monad m => SomeHasFS m -> m ()
destroySnapshots (SomeHasFS fs) = do
  dirs <- Set.lookupMax . Set.filter (isJust . snapshotFromPath) <$> listDirectory fs (mkFsPath [])
  mapM_ ((\d -> do
            isDir <- doesDirectoryExist fs d
            if isDir
              then removeDirectoryRecursive fs d
              else removeFile fs d
        ) . mkFsPath . (:[])) dirs

-- | Testing only! Truncate all snapshots in the DB.
implIntTruncateSnapshots :: MonadThrow m => SomeHasFS m -> m ()
implIntTruncateSnapshots (SomeHasFS fs) = do
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
  => LedgerDBEnv m l blk -> Maybe DiskSnapshot -> m ()
implIntTakeSnapshot env diskSnapshot = do
  withWriteLock
          (ldbLock env)
          (flushLedgerDB (ldbChangelog env) (ldbBackingStore env))
  void $ withReadLock (ldbLock env) $
    takeSnapshot
      (ldbChangelog env)
      (configCodec . getExtLedgerCfg . ledgerDbCfg $ ldbCfg env)
      (LedgerDBSnapshotEvent >$< ldbTracer env)
      (ldbStateHasFS env)
      (ldbBackingStore env)
      diskSnapshot

implIntReapplyThenPushBlock ::
     ( IOLike m
     , ApplyBlock l blk
     , MonadBase m m
     , l ~ ExtLedgerState blk
     )
  => LedgerDBEnv m l blk -> blk -> m ()
implIntReapplyThenPushBlock env blk = do
  chlog <- readTVarIO $ ldbChangelog env
  chlog' <- onChangelogM (reapplyThenPush (ldbCfg env)  blk (readKeySets (ldbBackingStore env))) chlog
  atomically $ writeTVar (ldbChangelog env) chlog'
