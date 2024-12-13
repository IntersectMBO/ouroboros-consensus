{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Many functions here are very similar to the ones in
-- "Ouroboros.Consensus.Storage.LedgerDB.V2". When we delete V1, this
-- module will be gone.
module Ouroboros.Consensus.Storage.LedgerDB.V1 (mkInitDb) where

import           Control.Arrow ((>>>))
import           Control.Monad
import           Control.ResourceRegistry
import           Control.Tracer
import           Data.Bifunctor (first)
import qualified Data.Foldable as Foldable
import           Data.Functor.Contravariant ((>$<))
import           Data.Kind (Type)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (isJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           GHC.Generics (Generic)
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
import           Ouroboros.Consensus.Storage.LedgerDB.Args
import           Ouroboros.Consensus.Storage.LedgerDB.Forker
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import           Ouroboros.Consensus.Storage.LedgerDB.TraceEvent
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Args as V1
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as BS
import           Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog as DbCh
                     (empty, flushableLength)
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Forker
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Lock
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Snapshots
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
import           System.FS.API

mkInitDb ::
  forall m blk.
  ( LedgerSupportsProtocol blk
  , IOLike m
  , LedgerDbSerialiseConstraints blk
  , HasHardForkHistory blk
  )
  => Complete LedgerDbArgs m blk
  -> Complete V1.LedgerDbFlavorArgs m
  -> ResolveBlock m blk
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
  , initFromSnapshot = \doChecksum ds ->
      loadSnapshot bsTracer baArgs (configCodec . getExtLedgerCfg . ledgerDbCfg $ lgrConfig) lgrHasFS' ds doChecksum
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
               , ldbQueryBatchSize  = lgrQueryBatchSize
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
      , lgrQueryBatchSize
      } = args

    lgrHasFS' = SnapshotsFS lgrHasFS

    V1Args flushFreq baArgs = bss

implMkLedgerDb ::
     forall m l blk.
     ( IOLike m
     , HasCallStack
     , StandardHash l
     , LedgerDbSerialiseConstraints blk
     , LedgerSupportsProtocol blk
     , ApplyBlock l blk
     , l ~ ExtLedgerState blk
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
    , validateFork              = getEnv5    h (implValidate h)
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
  validate $
    ValidateArgs
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
                                          Nothing
                                          (onDiskShouldChecksumSnapshots $ ldbSnapshotPolicy env)
                                        )
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
    , push = getEnv1 h implIntPush
    , reapplyThenPushNOW = getEnv1 h implIntReapplyThenPush
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
      (onDiskShouldChecksumSnapshots $ ldbSnapshotPolicy env)

implIntPush ::
     ( IOLike m
     , ApplyBlock l blk
     , l ~ ExtLedgerState blk
     )
  => LedgerDBEnv m l blk -> l DiffMK -> m ()
implIntPush env st = do
  chlog <- readTVarIO $ ldbChangelog env
  let chlog' = prune (ledgerDbCfgSecParam $ ldbCfg env) $ extend st chlog
  atomically $ writeTVar (ldbChangelog env) chlog'

implIntReapplyThenPush ::
     ( IOLike m
     , ApplyBlock l blk
     , l ~ ExtLedgerState blk
     )
  => LedgerDBEnv m l blk -> blk -> m ()
implIntReapplyThenPush env blk = do
  chlog <- readTVarIO $ ldbChangelog env
  chlog' <- reapplyThenPush (ldbCfg env)  blk (readKeySets (ldbBackingStore env)) chlog
  atomically $ writeTVar (ldbChangelog env) chlog'

{-------------------------------------------------------------------------------
  Flushing
-------------------------------------------------------------------------------}

flushLedgerDB :: (MonadSTM m, GetTip l, HasLedgerTables l)
              => StrictTVar m (DbChangelog l)
              -> LedgerBackingStore m l
              -> WriteLocked m ()
flushLedgerDB chlogVar bstore = do
  diffs <- writeLocked $ atomically $ do
    ldb' <- readTVar chlogVar
    let (toFlush, toKeep) = splitForFlushing ldb'
    case toFlush of
      Nothing -> pure ()
      Just {} -> writeTVar chlogVar toKeep
    pure toFlush
  mapM_ (flushIntoBackingStore bstore) diffs

-- | Flush **all the changes in this DbChangelog** into the backing store
--
-- Note that 'flush' must have been called to split the 'DbChangelog' on the
-- immutable tip and produce two 'DbChangelog's, one to flush and one to keep.
--
-- The write lock must be held before calling this function.
flushIntoBackingStore :: LedgerBackingStore m l -> DiffsToFlush l -> WriteLocked m ()
flushIntoBackingStore backingStore dblog = writeLocked $
  bsWrite
    backingStore
    (toFlushSlot dblog)
    (toFlushDiffs dblog)

{-------------------------------------------------------------------------------
  LedgerDB internal state
-------------------------------------------------------------------------------}

newtype LedgerDBHandle m l blk = LDBHandle (StrictTVar m (LedgerDBState m l blk))
  deriving Generic

data LedgerDBState m l blk =
    LedgerDBOpen !(LedgerDBEnv m l blk)
  | LedgerDBClosed
  deriving Generic

deriving instance ( IOLike m
                  , LedgerSupportsProtocol blk
                  , NoThunks (l EmptyMK)
                  , NoThunks (TxIn l)
                  , NoThunks (TxOut l)
                  , NoThunks (LedgerCfg l)
                  ) => NoThunks (LedgerDBState m l blk)

type LedgerDBEnv :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data LedgerDBEnv m l blk = LedgerDBEnv {
    -- | INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip of
    -- the current chain of the ChainDB.
    ldbChangelog      :: !(StrictTVar m (DbChangelog l))
    -- | Handle to the ledger's backing store, containing the parts that grow too
    -- big for in-memory residency
  , ldbBackingStore   :: !(LedgerBackingStore m l)
    -- | The flush lock to the 'BackingStore'. This lock is crucial when it
    -- comes to keeping the data in memory consistent with the data on-disk.
    --
    -- This lock should be held whenever we want to keep a consistent view of
    -- the backing store for some time. In particular we use this:
    --
    -- - when performing a query on the ledger state, we need to hold a
    --   'LocalStateQueryView' which, while live, must maintain a consistent view
    --   of the DB, and therefore we acquire a Read lock.
    --
    -- - when taking a snapshot of the ledger db, we need to prevent others (eg
    --   ChainSel) from altering the backing store at the same time, thus we
    --   acquire a Write lock.
  , ldbLock           :: !(LedgerDBLock m)
    -- | INVARIANT: this set contains only points that are in the
    -- VolatileDB.
    --
    -- INVARIANT: all points on the current chain fragment are in this set.
    --
    -- The VolatileDB might contain invalid blocks, these will not be in
    -- this set.
    --
    -- When a garbage-collection is performed on the VolatileDB, the points
    -- of the blocks eligible for garbage-collection should be removed from
    -- this set.
  , ldbPrevApplied    :: !(StrictTVar m (Set (RealPoint blk)))
    -- | Open forkers.
    --
    -- INVARIANT: a forker is open iff its 'ForkerKey' is in this 'Map.
  , ldbForkers        :: !(StrictTVar m (Map ForkerKey (ForkerEnv m l blk)))
  , ldbNextForkerKey  :: !(StrictTVar m ForkerKey)

  , ldbSnapshotPolicy :: !SnapshotPolicy
  , ldbTracer         :: !(Tracer m (TraceEvent blk))
  , ldbCfg            :: !(LedgerDbCfg l)
  , ldbHasFS          :: !(SnapshotsFS m)
    -- | Determine whether we should flush depending on the number of flushable
    -- diffs that we currently have in the LedgerDB, based on the flush
    -- frequency that was provided when opening the LedgerDB.
  , ldbShouldFlush    :: !(Word64 -> Bool)
  , ldbQueryBatchSize :: !QueryBatchSize
  , ldbResolveBlock   :: !(ResolveBlock m blk)
  } deriving (Generic)

deriving instance ( IOLike m
                  , LedgerSupportsProtocol blk
                  , NoThunks (l EmptyMK)
                  , NoThunks (TxIn l)
                  , NoThunks (TxOut l)
                  , NoThunks (LedgerCfg l)
                  ) => NoThunks (LedgerDBEnv m l blk)

-- | Check if the LedgerDB is open, if so, executing the given function on the
-- 'LedgerDBEnv', otherwise, throw a 'CloseDBError'.
getEnv ::
     forall m l blk r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> m r)
  -> m r
getEnv (LDBHandle varState) f = readTVarIO varState >>= \case
    LedgerDBOpen env -> f env
    LedgerDBClosed   -> throwIO $ ClosedDBError @blk prettyCallStack

-- | Variant 'of 'getEnv' for functions taking one argument.
getEnv1 ::
     (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> a -> m r)
  -> a -> m r
getEnv1 h f a = getEnv h (`f` a)

-- | Variant 'of 'getEnv' for functions taking two arguments.
getEnv2 ::
     (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> a -> b -> m r)
  -> a -> b -> m r
getEnv2 h f a b = getEnv h (\env -> f env a b)

-- | Variant 'of 'getEnv' for functions taking five arguments.
getEnv5 ::
     (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> a -> b -> c -> d -> e -> m r)
  -> a -> b -> c -> d -> e -> m r
getEnv5 h f a b c d e = getEnv h (\env -> f env a b c d e)

-- | Variant of 'getEnv' that works in 'STM'.
getEnvSTM ::
     forall m l blk r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> STM m r)
  -> STM m r
getEnvSTM (LDBHandle varState) f = readTVar varState >>= \case
    LedgerDBOpen env -> f env
    LedgerDBClosed   -> throwSTM $ ClosedDBError @blk prettyCallStack

-- | Variant of 'getEnv1' that works in 'STM'.
getEnvSTM1 ::
     forall m l blk a r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> (LedgerDBEnv m l blk -> a -> STM m r)
  -> a -> STM m r
getEnvSTM1 (LDBHandle varState) f a = readTVar varState >>= \case
    LedgerDBOpen env -> f env a
    LedgerDBClosed   -> throwSTM $ ClosedDBError @blk prettyCallStack

{-------------------------------------------------------------------------------
  Forkers
-------------------------------------------------------------------------------}

getForkerEnv ::
     forall m l blk r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> ForkerKey
  -> (ForkerEnv m l blk -> m r)
  -> m r
getForkerEnv (LDBHandle varState) forkerKey f = do
    forkerEnv <- atomically $ readTVar varState >>= \case
      LedgerDBClosed   -> throwIO $ ClosedDBError @blk prettyCallStack
      LedgerDBOpen env -> (Map.lookup forkerKey <$> readTVar (ldbForkers env)) >>= \case
        Nothing        -> throwSTM $ ClosedForkerError @blk forkerKey prettyCallStack
        Just forkerEnv -> pure forkerEnv

    f forkerEnv

getForkerEnv1 ::
     (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> ForkerKey
  -> (ForkerEnv m l blk -> a -> m r)
  -> a -> m r
getForkerEnv1 h forkerKey f a = getForkerEnv h forkerKey (`f` a)

getForkerEnvSTM ::
     forall m l blk r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> ForkerKey
  -> (ForkerEnv m l blk -> STM m r)
  -> STM m r
getForkerEnvSTM (LDBHandle varState) forkerKey f = readTVar varState >>= \case
    LedgerDBClosed   -> throwIO $ ClosedDBError @blk prettyCallStack
    LedgerDBOpen env -> readTVar (ldbForkers env) >>= (Map.lookup forkerKey >>> \case
      Nothing        -> throwSTM $ ClosedForkerError @blk forkerKey prettyCallStack
      Just forkerEnv -> f forkerEnv)

-- | Will call 'error' if the point is not on the LedgerDB
newForkerAtTarget ::
     ( HeaderHash l ~ HeaderHash blk
     , IOLike m
     , IsLedger l
     , StandardHash l
     , HasLedgerTables l
     , LedgerSupportsProtocol blk
     )
  => LedgerDBHandle m l blk
  -> ResourceRegistry m
  -> Target (Point blk)
  ->  m (Either GetForkerError (Forker m l blk))
newForkerAtTarget h rr pt = getEnv h $ \ldbEnv ->
    withReadLock (ldbLock ldbEnv) (acquireAtTarget ldbEnv rr (Right pt)) >>= traverse (newForker h ldbEnv)

newForkerByRollback ::
     ( HeaderHash l ~ HeaderHash blk
     , IOLike m
     , IsLedger l
     , StandardHash l
     , HasLedgerTables l
     , LedgerSupportsProtocol blk
     )
  => LedgerDBHandle m l blk
  -> ResourceRegistry m
     -- | How many blocks to rollback from the tip
  -> Word64
  -> m (Either GetForkerError (Forker m l blk))
newForkerByRollback h rr n = getEnv h $ \ldbEnv -> do
    withReadLock (ldbLock ldbEnv) (acquireAtTarget ldbEnv rr (Left n)) >>= traverse (newForker h ldbEnv)

-- | Close all open block and header 'Forker's.
closeAllForkers ::
     IOLike m
  => LedgerDBEnv m l blk
  -> m ()
closeAllForkers ldbEnv =
  do
    forkerEnvs <- atomically $ do
      forkerEnvs <- Map.elems <$> readTVar forkersVar
      writeTVar forkersVar Map.empty
      return forkerEnvs
    mapM_ closeForkerEnv forkerEnvs
  where
    forkersVar = ldbForkers ldbEnv

type Resources m l =
    (LedgerBackingStoreValueHandle m l, DbChangelog l)

-- | Acquire both a value handle and a db changelog at the tip. Holds a read lock
-- while doing so.
acquireAtTarget ::
     forall m l blk. (
       HeaderHash l ~ HeaderHash blk
     , IOLike m
     , IsLedger l
     , StandardHash l
     , HasLedgerTables l
     , LedgerSupportsProtocol blk
     )
  => LedgerDBEnv m l blk
  -> ResourceRegistry m
  -> Either Word64 (Target (Point blk))
  -> ReadLocked m (Either GetForkerError (Resources m l))
acquireAtTarget ldbEnv rr (Right VolatileTip) =
    readLocked $ do
      dblog <- readTVarIO (ldbChangelog ldbEnv)
      Right . (,dblog) <$> acquire ldbEnv rr dblog
acquireAtTarget ldbEnv rr (Right ImmutableTip) =
    readLocked $ do
      dblog <- readTVarIO (ldbChangelog ldbEnv)
      Right . (, rollbackToAnchor dblog)
        <$> acquire ldbEnv rr dblog
acquireAtTarget ldbEnv rr (Right (SpecificPoint pt)) =
    readLocked $ do
      dblog <- readTVarIO (ldbChangelog ldbEnv)
      let immTip = getTip $ anchor dblog
      case rollback pt dblog of
        Nothing     | pointSlot pt < pointSlot immTip -> pure $ Left $ PointTooOld Nothing
                    | otherwise   -> pure $ Left PointNotOnChain
        Just dblog' -> Right . (,dblog') <$> acquire ldbEnv rr dblog'
acquireAtTarget ldbEnv rr (Left n) = readLocked $ do
      dblog <- readTVarIO (ldbChangelog ldbEnv)
      case rollbackN n dblog of
        Nothing ->
          return $ Left $ PointTooOld $ Just $ ExceededRollback {
              rollbackMaximum   = maxRollback dblog
            , rollbackRequested = n
            }
        Just dblog' ->
           Right . (,dblog') <$> acquire ldbEnv rr dblog'

acquire ::
     (IOLike m, GetTip l)
  => LedgerDBEnv m l blk
  -> ResourceRegistry m
  -> DbChangelog l
  -> m (LedgerBackingStoreValueHandle m l)
acquire ldbEnv rr dblog =  do
  -- bsvhClose is idempotent, so we let the resource call it even if the value
  -- handle might have been closed somewhere else
  (_, vh) <- allocate rr (\_ -> bsValueHandle $ ldbBackingStore ldbEnv) bsvhClose
  let dblogSlot = getTipSlot (changelogLastFlushedState dblog)
  if bsvhAtSlot vh == dblogSlot
    then pure vh
    else bsvhClose vh >>
         error (  "Critical error: Value handles are created at "
                <> show (bsvhAtSlot vh)
                <> " while the db changelog is at "
                <> show dblogSlot
                <> ". There is either a race condition or a logic bug"
                )

{-------------------------------------------------------------------------------
  Make forkers from consistent views
-------------------------------------------------------------------------------}

newForker ::
     ( IOLike m
     , HasLedgerTables l
     , LedgerSupportsProtocol blk
     , NoThunks (l EmptyMK)
     , GetTip l
     )
  => LedgerDBHandle m l blk
  -> LedgerDBEnv m l blk
  -> Resources m l
  -> m (Forker m l blk)
newForker h ldbEnv (vh, dblog) = do
  dblogVar     <- newTVarIO dblog
  forkerKey    <- atomically $ stateTVar (ldbNextForkerKey ldbEnv) $ \r -> (r, r + 1)
  let forkerEnv = ForkerEnv {
      foeBackingStoreValueHandle = vh
    , foeChangelog               = dblogVar
    , foeSwitchVar               = ldbChangelog ldbEnv
    , foeSecurityParam           = ledgerDbCfgSecParam $ ldbCfg ldbEnv
    , foeTracer                  = LedgerDBForkerEvent . TraceForkerEventWithKey forkerKey >$< ldbTracer ldbEnv
    }
  atomically $ modifyTVar (ldbForkers ldbEnv) $ Map.insert forkerKey forkerEnv
  traceWith (foeTracer forkerEnv) ForkerOpen
  pure $ mkForker h (ldbQueryBatchSize ldbEnv) forkerKey

mkForker ::
     ( IOLike m
     , HasHeader blk
     , HasLedgerTables l
     , GetTip l
     )
  => LedgerDBHandle m l blk
  -> QueryBatchSize
  -> ForkerKey
  -> Forker m l blk
mkForker h qbs forkerKey = Forker {
      forkerClose                  = implForkerClose h forkerKey
    , forkerReadTables             = getForkerEnv1   h forkerKey implForkerReadTables
    , forkerRangeReadTables        = getForkerEnv1   h forkerKey (implForkerRangeReadTables qbs)
    , forkerGetLedgerState         = getForkerEnvSTM h forkerKey implForkerGetLedgerState
    , forkerReadStatistics         = getForkerEnv    h forkerKey implForkerReadStatistics
    , forkerPush                   = getForkerEnv1   h forkerKey implForkerPush
    , forkerCommit                 = getForkerEnvSTM h forkerKey implForkerCommit
    }

implForkerClose ::
     IOLike m
  => LedgerDBHandle m l blk
  -> ForkerKey
  -> m ()
implForkerClose (LDBHandle varState) forkerKey = do
    envMay <- atomically $ readTVar varState >>= \case
      LedgerDBClosed       -> pure Nothing
      LedgerDBOpen ldbEnv -> do
        stateTVar
            (ldbForkers ldbEnv)
            (Map.updateLookupWithKey (\_ _ -> Nothing) forkerKey)
    whenJust envMay closeForkerEnv
