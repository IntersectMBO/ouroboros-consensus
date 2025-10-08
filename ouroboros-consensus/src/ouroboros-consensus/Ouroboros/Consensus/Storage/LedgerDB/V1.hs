{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Many functions here are very similar to the ones in
-- "Ouroboros.Consensus.Storage.LedgerDB.V2". When we delete V1, this
-- module will be gone.
module Ouroboros.Consensus.Storage.LedgerDB.V1 (mkInitDb) where

import Control.Arrow ((>>>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Control.ResourceRegistry
import Control.Tracer
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Foldable as Foldable
import Data.Functor ((<&>))
import Data.Functor.Contravariant ((>$<))
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word
import GHC.Generics (Generic)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.HeaderStateHistory
  ( HeaderStateHistory (..)
  , mkHeaderStateWithTimeFromSummary
  )
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Args
import Ouroboros.Consensus.Storage.LedgerDB.Forker
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.TraceEvent
import Ouroboros.Consensus.Storage.LedgerDB.V1.Args as V1
import Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore as BS
import Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog
import qualified Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog as DbCh
  ( empty
  , flushableLength
  )
import Ouroboros.Consensus.Storage.LedgerDB.V1.Forker
import Ouroboros.Consensus.Storage.LedgerDB.V1.Lock
import Ouroboros.Consensus.Storage.LedgerDB.V1.Snapshots
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Network.AnchoredSeq as AS
import Ouroboros.Network.Protocol.LocalStateQuery.Type
import System.FS.API

type SnapshotManagerV1 m blk =
  SnapshotManager m (ReadLocked m) blk (StrictTVar m (DbChangelog' blk), BackingStore' m blk)

mkInitDb ::
  forall m blk.
  ( LedgerSupportsProtocol blk
  , IOLike m
  , HasHardForkHistory blk
  , LedgerSupportsLedgerDB blk
  ) =>
  Complete LedgerDbArgs m blk ->
  V1.LedgerDbBackendArgs m (ExtLedgerState blk) ->
  ResolveBlock m blk ->
  SnapshotManagerV1 m blk ->
  GetVolatileSuffix m blk ->
  InitDB (DbChangelog' blk, ResourceKey m, BackingStore' m blk) m blk
mkInitDb args bss getBlock snapManager getVolatileSuffix =
  InitDB
    { initFromGenesis = do
        st <- lgrGenesis
        let genesis = forgetLedgerTables st
            chlog = DbCh.empty genesis
        (bsKey, backingStore) <-
          allocate
            lgrRegistry
            (\_ -> newBackingStore bsTracer baArgs lgrHasFS' genesis (projectLedgerTables st))
            bsClose
        pure (chlog, bsKey, backingStore)
    , initFromSnapshot =
        runExceptT
          . loadSnapshot
            bsTracer
            baArgs
            (configCodec . getExtLedgerCfg . ledgerDbCfg $ lgrConfig)
            lgrHasFS'
            lgrRegistry
    , abortLedgerDbInit = \(_, r, _) -> void $ release r
    , initReapplyBlock = \cfg blk (chlog, r, bstore) -> do
        !chlog' <- reapplyThenPush cfg blk (readKeySets bstore) chlog
        -- It's OK to flush without a lock here, since the `LedgerDB` has not
        -- finished initializing, only this thread has access to the backing
        -- store.
        chlog'' <-
          unsafeIgnoreWriteLock $
            if shouldFlush flushFreq (flushableLength chlog')
              then do
                let (toFlush, toKeep) = splitForFlushing chlog'
                mapM_ (flushIntoBackingStore bstore) toFlush
                pure toKeep
              else pure chlog'
        pure (chlog'', r, bstore)
    , currentTip = \(ch, _, _) -> ledgerState . current $ ch
    , mkLedgerDb = \(db, ldbBackingStoreKey, ldbBackingStore) -> do
        (varDB, prevApplied) <-
          (,) <$> newTVarIO db <*> newTVarIO Set.empty
        flushLock <- mkLedgerDBLock
        forkers <- newTVarIO Map.empty
        nextForkerKey <- newTVarIO (ForkerKey 0)
        lastSnapshotWrite <- newTVarIO Nothing
        let env =
              LedgerDBEnv
                { ldbChangelog = varDB
                , ldbBackingStore = ldbBackingStore
                , ldbBackingStoreKey = ldbBackingStoreKey
                , ldbLock = flushLock
                , ldbPrevApplied = prevApplied
                , ldbForkers = forkers
                , ldbNextForkerKey = nextForkerKey
                , ldbSnapshotPolicy = defaultSnapshotPolicy (ledgerDbCfgSecParam lgrConfig) lgrSnapshotPolicyArgs
                , ldbTracer = lgrTracer
                , ldbCfg = lgrConfig
                , ldbHasFS = lgrHasFS'
                , ldbShouldFlush = shouldFlush flushFreq
                , ldbQueryBatchSize = lgrQueryBatchSize
                , ldbResolveBlock = getBlock
                , ldbGetVolatileSuffix = getVolatileSuffix
                , ldbLastSnapshotWrite = lastSnapshotWrite
                }
        h <- LDBHandle <$> newTVarIO (LedgerDBOpen env)
        pure $ implMkLedgerDb h snapManager
    }
 where
  !bsTracer = LedgerDBFlavorImplEvent . FlavorImplSpecificTraceV1 >$< tr
  !tr = lgrTracer

  LedgerDbArgs
    { lgrHasFS
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
  ) =>
  LedgerDBHandle m l blk ->
  SnapshotManagerV1 m blk ->
  (LedgerDB' m blk, TestInternals' m blk)
implMkLedgerDb h snapManager =
  ( LedgerDB
      { getVolatileTip = getEnvSTM h implGetVolatileTip
      , getImmutableTip = getEnvSTM h implGetImmutableTip
      , getPastLedgerState = getEnvSTM1 h implGetPastLedgerState
      , getHeaderStateHistory = getEnvSTM h implGetHeaderStateHistory
      , getForkerAtTarget = newForkerAtTarget h
      , validateFork = getEnv5 h (implValidate h)
      , getPrevApplied = getEnvSTM h implGetPrevApplied
      , garbageCollect = getEnv1 h implGarbageCollect
      , tryTakeSnapshot = getEnv1 h (implTryTakeSnapshot snapManager)
      , tryFlush = getEnv h implTryFlush
      , closeDB = implCloseDB h
      }
  , mkInternals h snapManager
  )

implGetVolatileTip ::
  (MonadSTM m, GetTip l) =>
  LedgerDBEnv m l blk ->
  STM m (l EmptyMK)
implGetVolatileTip = fmap current . readTVar . ldbChangelog

implGetImmutableTip ::
  (MonadSTM m, GetTip l) =>
  LedgerDBEnv m l blk ->
  STM m (l EmptyMK)
implGetImmutableTip env = do
  volSuffix <- getVolatileSuffix $ ldbGetVolatileSuffix env
  -- The DbChangelog might contain more than k states if they have not yet
  -- been garbage-collected.
  fmap (AS.anchor . volSuffix . changelogStates)
    . readTVar
    $ ldbChangelog env

implGetPastLedgerState ::
  ( MonadSTM m
  , HasHeader blk
  , IsLedger l
  , StandardHash l
  , HasLedgerTables l
  , HeaderHash l ~ HeaderHash blk
  ) =>
  LedgerDBEnv m l blk -> Point blk -> STM m (Maybe (l EmptyMK))
implGetPastLedgerState env point = do
  volSuffix <- getVolatileSuffix $ ldbGetVolatileSuffix env
  readTVar (ldbChangelog env) <&> \chlog -> do
    -- The DbChangelog might contain more than k states if they have not yet
    -- been garbage-collected, so make sure that the point is volatile (or the
    -- immutable tip).
    guard $
      AS.withinBounds
        (pointSlot point)
        ((point ==) . castPoint . either getTip getTip)
        (volSuffix (changelogStates chlog))
    getPastLedgerAt point chlog

implGetHeaderStateHistory ::
  ( MonadSTM m
  , l ~ ExtLedgerState blk
  , IsLedger (LedgerState blk)
  , HasHardForkHistory blk
  , HasAnnTip blk
  ) =>
  LedgerDBEnv m l blk -> STM m (HeaderStateHistory blk)
implGetHeaderStateHistory env = do
  ldb <- readTVar (ldbChangelog env)
  volSuffix <- getVolatileSuffix $ ldbGetVolatileSuffix env
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
    -- The DbChangelog might contain more than k states if they have not yet
    -- been garbage-collected, so only take the corresponding suffix.
    . volSuffix
    $ changelogStates ldb

implValidate ::
  forall m l blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , HasCallStack
  , l ~ ExtLedgerState blk
  ) =>
  LedgerDBHandle m l blk ->
  LedgerDBEnv m l blk ->
  ResourceRegistry m ->
  (TraceValidateEvent blk -> m ()) ->
  BlockCache blk ->
  Word64 ->
  [Header blk] ->
  m (ValidateResult m (ExtLedgerState blk) blk)
implValidate h ldbEnv rr tr cache rollbacks hdrs =
  validate (ledgerDbCfgComputeLedgerEvents $ ldbCfg ldbEnv) $
    ValidateArgs
      (ldbResolveBlock ldbEnv)
      (getExtLedgerCfg . ledgerDbCfg $ ldbCfg ldbEnv)
      ( \l -> do
          prev <- readTVar (ldbPrevApplied ldbEnv)
          writeTVar (ldbPrevApplied ldbEnv) (Foldable.foldl' (flip Set.insert) prev l)
      )
      (readTVar (ldbPrevApplied ldbEnv))
      (newForkerByRollback h)
      rr
      tr
      cache
      rollbacks
      hdrs

implGetPrevApplied :: MonadSTM m => LedgerDBEnv m l blk -> STM m (Set (RealPoint blk))
implGetPrevApplied env = readTVar (ldbPrevApplied env)

-- | Remove 'DbChangelog' states older than the given slot, and all points with
-- a slot older than the given slot from the set of previously applied points.
implGarbageCollect ::
  ( MonadSTM m
  , IsLedger (LedgerState blk)
  , l ~ ExtLedgerState blk
  ) =>
  LedgerDBEnv m l blk -> SlotNo -> m ()
implGarbageCollect env slotNo = atomically $ do
  modifyTVar (ldbChangelog env) $
    prune (LedgerDbPruneBeforeSlot slotNo)
  modifyTVar (ldbPrevApplied env) $
    Set.dropWhileAntitone ((< slotNo) . realPointSlot)

implTryTakeSnapshot ::
  ( IsLedger (LedgerState blk)
  , l ~ ExtLedgerState blk
  , HasLedgerTables l
  , IOLike m
  ) =>
  SnapshotManagerV1 m blk ->
  LedgerDBEnv m l blk ->
  Time ->
  m ()
implTryTakeSnapshot snapManager env now = do
  timeSinceLastWrite <- do
    mLastWrite <- readTVarIO $ ldbLastSnapshotWrite env
    forM mLastWrite $ \lastWrite -> do
      pure $ now `diffTime` lastWrite
  -- Get all states before the volatile suffix.
  immutableStates <- atomically $ do
    states <- changelogStates <$> readTVar (ldbChangelog env)
    volSuffix <- getVolatileSuffix (ldbGetVolatileSuffix env)
    pure $ AS.dropNewest (AS.length (volSuffix states)) states
  let immutableSlots :: [SlotNo] =
        -- Remove duplicates due to EBBs.
        nubOrd . mapMaybe (withOriginToMaybe . getTipSlot) $
          AS.anchor immutableStates : AS.toOldestFirst immutableStates
      snapshotSlots =
        onDiskSnapshotSelector
          (ldbSnapshotPolicy env)
          SnapshotSelectorContext
            { sscTimeSinceLast = timeSinceLastWrite
            , sscSnapshotSlots = immutableSlots
            }
  forM_ snapshotSlots $ \slot -> do
    -- Prune the 'DbChangelog' such that the resulting anchor state has slot
    -- number @slot@.
    let pruneStrat = LedgerDbPruneBeforeSlot (slot + 1)
    atomically $ modifyTVar (ldbChangelog env) (prune pruneStrat)
    -- Flush the LedgerDB such that we can take a snapshot for the new anchor
    -- state due to the previous prune.
    withWriteLock
      (ldbLock env)
      (flushLedgerDB (ldbChangelog env) (ldbBackingStore env))
    -- Now, taking a snapshot (for the last flushed state) will do what we want.
    void $
      withReadLock (ldbLock env) $
        takeSnapshot
          snapManager
          Nothing
          (ldbChangelog env, ldbBackingStore env)
    finished <- getMonotonicTime
    atomically $ writeTVar (ldbLastSnapshotWrite env) (Just $! finished)
    void $ trimSnapshots snapManager (ldbSnapshotPolicy env)

-- If the DbChangelog in the LedgerDB can flush (based on the SnapshotPolicy
-- with which this LedgerDB was opened), flush differences to the backing
-- store. Note this acquires a write lock on the backing store.
implTryFlush ::
  (IOLike m, HasLedgerTables l, GetTip l) =>
  LedgerDBEnv m l blk -> m ()
implTryFlush env = do
  ldb <- readTVarIO $ ldbChangelog env
  when
    (ldbShouldFlush env $ DbCh.flushableLength ldb)
    ( withWriteLock
        (ldbLock env)
        (flushLedgerDB (ldbChangelog env) (ldbBackingStore env))
    )

implCloseDB :: IOLike m => LedgerDBHandle m l blk -> m ()
implCloseDB (LDBHandle varState) = do
  mbOpenEnv <-
    atomically $
      readTVar varState >>= \case
        -- Idempotent
        LedgerDBClosed -> return Nothing
        LedgerDBOpen env -> do
          -- By writing this tvar, we already make sure that no
          -- forkers can perform operations other than closing, as
          -- they rely on accessing the LedgerDB, which is now closed.
          writeTVar varState LedgerDBClosed
          return $ Just env

  -- Only when the LedgerDB was open
  whenJust mbOpenEnv $ void . release . ldbBackingStoreKey

mkInternals ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBHandle m (ExtLedgerState blk) blk ->
  SnapshotManagerV1 m blk ->
  TestInternals' m blk
mkInternals h snapManager =
  TestInternals
    { takeSnapshotNOW = getEnv2 h (implIntTakeSnapshot snapManager)
    , wipeLedgerDB = void $ destroySnapshots snapManager
    , truncateSnapshots = getEnv h $ void . implIntTruncateSnapshots . ldbHasFS
    , push = getEnv1 h implIntPush
    , reapplyThenPushNOW = getEnv1 h implIntReapplyThenPush
    , closeLedgerDB = getEnv h $ void . release . ldbBackingStoreKey
    , getNumLedgerTablesHandles = pure 0
    }

-- | Testing only! Truncate all snapshots in the DB.
implIntTruncateSnapshots :: MonadThrow m => SnapshotsFS m -> m ()
implIntTruncateSnapshots (SnapshotsFS (SomeHasFS fs)) = do
  dirs <- Set.lookupMax . Set.filter (isJust . snapshotFromPath) <$> listDirectory fs (mkFsPath [])
  mapM_ (truncateRecursively . (: [])) dirs
 where
  truncateRecursively pre = do
    dirs <- listDirectory fs (mkFsPath pre)
    mapM_
      ( \d -> do
          let d' = pre ++ [d]
          isDir <- doesDirectoryExist fs $ mkFsPath d'
          if isDir
            then truncateRecursively d'
            else withFile fs (mkFsPath d') (AppendMode AllowExisting) $ \h -> hTruncate fs h 0
      )
      dirs

implIntTakeSnapshot ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  , l ~ ExtLedgerState blk
  ) =>
  SnapshotManagerV1 m blk ->
  LedgerDBEnv m l blk ->
  WhereToTakeSnapshot ->
  Maybe String ->
  m ()
implIntTakeSnapshot snapManager env whereTo suffix = do
  when (whereTo == TakeAtVolatileTip) $ atomically $ modifyTVar (ldbChangelog env) pruneToImmTipOnly
  withWriteLock
    (ldbLock env)
    (flushLedgerDB (ldbChangelog env) (ldbBackingStore env))
  void $
    withReadLock (ldbLock env) $
      takeSnapshot
        snapManager
        suffix
        (ldbChangelog env, ldbBackingStore env)

implIntPush ::
  ( IOLike m
  , ApplyBlock l blk
  , l ~ ExtLedgerState blk
  ) =>
  LedgerDBEnv m l blk -> l DiffMK -> m ()
implIntPush env st = do
  chlog <- readTVarIO $ ldbChangelog env
  let chlog' = pruneToImmTipOnly $ extend st chlog
  atomically $ writeTVar (ldbChangelog env) chlog'

implIntReapplyThenPush ::
  ( IOLike m
  , ApplyBlock l blk
  , l ~ ExtLedgerState blk
  ) =>
  LedgerDBEnv m l blk -> blk -> m ()
implIntReapplyThenPush env blk = do
  chlog <- readTVarIO $ ldbChangelog env
  chlog' <- reapplyThenPush (ldbCfg env) blk (readKeySets (ldbBackingStore env)) chlog
  atomically $ writeTVar (ldbChangelog env) chlog'

{-------------------------------------------------------------------------------
  Flushing
-------------------------------------------------------------------------------}

flushLedgerDB ::
  (MonadSTM m, GetTip l, HasLedgerTables l) =>
  StrictTVar m (DbChangelog l) ->
  LedgerBackingStore m l ->
  WriteLocked m ()
flushLedgerDB chlogVar bstore = do
  diffs <- writeLocked $ atomically $ do
    ldb' <- readTVar chlogVar
    let (toFlush, toKeep) = splitForFlushing ldb'
    case toFlush of
      Nothing -> pure ()
      Just{} -> writeTVar chlogVar toKeep
    pure toFlush
  mapM_ (flushIntoBackingStore bstore) diffs

-- | Flush **all the changes in this DbChangelog** into the backing store
--
-- Note that 'flush' must have been called to split the 'DbChangelog' on the
-- immutable tip and produce two 'DbChangelog's, one to flush and one to keep.
--
-- The write lock must be held before calling this function.
flushIntoBackingStore :: LedgerBackingStore m l -> DiffsToFlush l -> WriteLocked m ()
flushIntoBackingStore backingStore dblog =
  writeLocked $
    bsWrite
      backingStore
      (toFlushSlot dblog)
      (toFlushState dblog)
      (toFlushDiffs dblog)

{-------------------------------------------------------------------------------
  LedgerDB internal state
-------------------------------------------------------------------------------}

newtype LedgerDBHandle m l blk = LDBHandle (StrictTVar m (LedgerDBState m l blk))
  deriving Generic

data LedgerDBState m l blk
  = LedgerDBOpen !(LedgerDBEnv m l blk)
  | LedgerDBClosed
  deriving Generic

deriving instance
  ( IOLike m
  , LedgerSupportsProtocol blk
  , NoThunks (l EmptyMK)
  , NoThunks (TxIn l)
  , NoThunks (TxOut l)
  , NoThunks (LedgerCfg l)
  ) =>
  NoThunks (LedgerDBState m l blk)

type LedgerDBEnv :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data LedgerDBEnv m l blk = LedgerDBEnv
  { ldbChangelog :: !(StrictTVar m (DbChangelog l))
  -- ^ INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip of
  -- the current chain of the ChainDB.
  , ldbBackingStore :: !(LedgerBackingStore m l)
  -- ^ Handle to the ledger's backing store, containing the parts that grow too
  -- big for in-memory residency
  , ldbBackingStoreKey :: !(ResourceKey m)
  -- ^ When deallocating the backing store upon closing the LedgerDB
  -- (via the ChainDB shutting down), we will release the backing
  -- store with this action.
  , ldbLock :: !(LedgerDBLock m)
  -- ^ The flush lock to the 'BackingStore'. This lock is crucial when it
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
  , ldbPrevApplied :: !(StrictTVar m (Set (RealPoint blk)))
  -- ^ INVARIANT: this set contains only points that are in the
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
  , ldbForkers :: !(StrictTVar m (Map ForkerKey (ForkerEnv m l blk)))
  -- ^ Open forkers.
  --
  -- INVARIANT: a forker is open iff its 'ForkerKey' is in this 'Map.
  --
  -- The resources that could possibly be held by these forkers will
  -- be released by each one of the client's registries. This means
  -- that for example ChainSelection will, upon closing its registry,
  -- release its forker and any resources associated.
  --
  -- Upon closing the LedgerDB we will overwrite this variable such
  -- that existing forkers can only be closed, as closing doesn't
  -- involve accessing this map (other than possibly removing the
  -- forker from it if the map still exists).
  --
  -- As the LedgerDB should outlive any clients, this is fine.
  , ldbNextForkerKey :: !(StrictTVar m ForkerKey)
  , ldbSnapshotPolicy :: !SnapshotPolicy
  , ldbTracer :: !(Tracer m (TraceEvent blk))
  , ldbCfg :: !(LedgerDbCfg l)
  , ldbHasFS :: !(SnapshotsFS m)
  , ldbShouldFlush :: !(Word64 -> Bool)
  -- ^ Determine whether we should flush depending on the number of flushable
  -- diffs that we currently have in the LedgerDB, based on the flush
  -- frequency that was provided when opening the LedgerDB.
  , ldbQueryBatchSize :: !QueryBatchSize
  , ldbResolveBlock :: !(ResolveBlock m blk)
  , ldbGetVolatileSuffix :: !(GetVolatileSuffix m blk)
  , ldbLastSnapshotWrite :: !(StrictTVar m (Maybe Time))
  -- ^ When did we finish writing the last snapshot.
  }
  deriving Generic

deriving instance
  ( IOLike m
  , LedgerSupportsProtocol blk
  , NoThunks (l EmptyMK)
  , NoThunks (TxIn l)
  , NoThunks (TxOut l)
  , NoThunks (LedgerCfg l)
  ) =>
  NoThunks (LedgerDBEnv m l blk)

-- | Check if the LedgerDB is open, if so, executing the given function on the
-- 'LedgerDBEnv', otherwise, throw a 'CloseDBError'.
getEnv ::
  forall m l blk r.
  (IOLike m, HasCallStack, HasHeader blk) =>
  LedgerDBHandle m l blk ->
  (LedgerDBEnv m l blk -> m r) ->
  m r
getEnv (LDBHandle varState) f =
  readTVarIO varState >>= \case
    LedgerDBOpen env -> f env
    LedgerDBClosed -> throwIO $ ClosedDBError @blk prettyCallStack

-- | Variant 'of 'getEnv' for functions taking one argument.
getEnv1 ::
  (IOLike m, HasCallStack, HasHeader blk) =>
  LedgerDBHandle m l blk ->
  (LedgerDBEnv m l blk -> a -> m r) ->
  a ->
  m r
getEnv1 h f a = getEnv h (`f` a)

-- | Variant 'of 'getEnv' for functions taking two arguments.
getEnv2 ::
  (IOLike m, HasCallStack, HasHeader blk) =>
  LedgerDBHandle m l blk ->
  (LedgerDBEnv m l blk -> a -> b -> m r) ->
  a ->
  b ->
  m r
getEnv2 h f a b = getEnv h (\env -> f env a b)

-- | Variant 'of 'getEnv' for functions taking five arguments.
getEnv5 ::
  (IOLike m, HasCallStack, HasHeader blk) =>
  LedgerDBHandle m l blk ->
  (LedgerDBEnv m l blk -> a -> b -> c -> d -> e -> m r) ->
  a ->
  b ->
  c ->
  d ->
  e ->
  m r
getEnv5 h f a b c d e = getEnv h (\env -> f env a b c d e)

-- | Variant of 'getEnv' that works in 'STM'.
getEnvSTM ::
  forall m l blk r.
  (IOLike m, HasCallStack, HasHeader blk) =>
  LedgerDBHandle m l blk ->
  (LedgerDBEnv m l blk -> STM m r) ->
  STM m r
getEnvSTM (LDBHandle varState) f =
  readTVar varState >>= \case
    LedgerDBOpen env -> f env
    LedgerDBClosed -> throwSTM $ ClosedDBError @blk prettyCallStack

-- | Variant of 'getEnv1' that works in 'STM'.
getEnvSTM1 ::
  forall m l blk a r.
  (IOLike m, HasCallStack, HasHeader blk) =>
  LedgerDBHandle m l blk ->
  (LedgerDBEnv m l blk -> a -> STM m r) ->
  a ->
  STM m r
getEnvSTM1 (LDBHandle varState) f a =
  readTVar varState >>= \case
    LedgerDBOpen env -> f env a
    LedgerDBClosed -> throwSTM $ ClosedDBError @blk prettyCallStack

{-------------------------------------------------------------------------------
  Forkers
-------------------------------------------------------------------------------}

getForkerEnv ::
  forall m l blk r.
  (IOLike m, HasCallStack, HasHeader blk) =>
  LedgerDBHandle m l blk ->
  ForkerKey ->
  (ForkerEnv m l blk -> m r) ->
  m r
getForkerEnv (LDBHandle varState) forkerKey f = do
  forkerEnv <-
    atomically $
      readTVar varState >>= \case
        LedgerDBClosed -> throwIO $ ClosedDBError @blk prettyCallStack
        LedgerDBOpen env ->
          (Map.lookup forkerKey <$> readTVar (ldbForkers env)) >>= \case
            Nothing -> throwSTM $ ClosedForkerError @blk forkerKey prettyCallStack
            Just forkerEnv -> pure forkerEnv

  f forkerEnv

getForkerEnv1 ::
  (IOLike m, HasCallStack, HasHeader blk) =>
  LedgerDBHandle m l blk ->
  ForkerKey ->
  (ForkerEnv m l blk -> a -> m r) ->
  a ->
  m r
getForkerEnv1 h forkerKey f a = getForkerEnv h forkerKey (`f` a)

getForkerEnvSTM ::
  forall m l blk r.
  (IOLike m, HasCallStack, HasHeader blk) =>
  LedgerDBHandle m l blk ->
  ForkerKey ->
  (ForkerEnv m l blk -> STM m r) ->
  STM m r
getForkerEnvSTM (LDBHandle varState) forkerKey f =
  readTVar varState >>= \case
    LedgerDBClosed -> throwIO $ ClosedDBError @blk prettyCallStack
    LedgerDBOpen env ->
      readTVar (ldbForkers env)
        >>= ( Map.lookup forkerKey >>> \case
                Nothing -> throwSTM $ ClosedForkerError @blk forkerKey prettyCallStack
                Just forkerEnv -> f forkerEnv
            )

-- | Will call 'error' if the point is not on the LedgerDB
newForkerAtTarget ::
  ( HeaderHash l ~ HeaderHash blk
  , IOLike m
  , IsLedger l
  , StandardHash l
  , HasLedgerTables l
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBHandle m l blk ->
  ResourceRegistry m ->
  Target (Point blk) ->
  m (Either GetForkerError (Forker m l blk))
newForkerAtTarget h rr pt = withTransferrableReadAccess h rr (Right pt)

newForkerByRollback ::
  ( HeaderHash l ~ HeaderHash blk
  , IOLike m
  , IsLedger l
  , StandardHash l
  , HasLedgerTables l
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBHandle m l blk ->
  ResourceRegistry m ->
  -- | How many blocks to rollback from the tip
  Word64 ->
  m (Either GetForkerError (Forker m l blk))
newForkerByRollback h rr n = withTransferrableReadAccess h rr (Left n)

-- | Acquire read access and then allocate a forker, acquiring it at the given
-- point or rollback.
withTransferrableReadAccess ::
  ( HeaderHash l ~ HeaderHash blk
  , IOLike m
  , IsLedger l
  , StandardHash l
  , HasLedgerTables l
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBHandle m l blk ->
  ResourceRegistry m ->
  Either Word64 (Target (Point blk)) ->
  m (Either GetForkerError (Forker m l blk))
withTransferrableReadAccess h rr f = getEnv h $ \ldbEnv -> do
  -- This TVar will be used to maybe release the read lock by the resource
  -- registry. Once the forker was opened it will be emptied.
  tv <- newTVarIO (pure ())
  (rk, _) <-
    allocate
      rr
      ( \_ -> atomically $ do
          -- Populate the tvar with the releasing action. Creating the forker will empty this
          writeTVar tv (atomically $ unsafeReleaseReadAccess (ldbLock ldbEnv))
          -- Acquire the read access
          unsafeAcquireReadAccess (ldbLock ldbEnv)
      )
      ( \_ ->
          -- Run the contents of the releasing TVar which will be `pure ()` if
          -- the forker was opened.
          join $ readTVarIO tv
      )
  unsafeRunReadLocked
    ( acquireAtTarget ldbEnv f
        >>= \case
          Left err -> do
            ReadLocked $ void $ release rk
            pure (Left err)
          Right chlog -> do
            Right <$> newForker h ldbEnv (rk, tv) rr chlog
    )

-- | Acquire both a value handle and a db changelog at the tip. Holds a read lock
-- while doing so.
acquireAtTarget ::
  forall m l blk.
  ( HeaderHash l ~ HeaderHash blk
  , IOLike m
  , IsLedger l
  , StandardHash l
  , HasLedgerTables l
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBEnv m l blk ->
  Either Word64 (Target (Point blk)) ->
  ReadLocked m (Either GetForkerError (DbChangelog l))
acquireAtTarget ldbEnv target = readLocked $ runExceptT $ do
  (dblog, volStates) <- lift $ atomically $ do
    dblog <- readTVar (ldbChangelog ldbEnv)
    -- The DbChangelog might contain more than k states if they have not yet
    -- been garbage-collected.
    volSuffix <- getVolatileSuffix $ ldbGetVolatileSuffix ldbEnv
    pure (dblog, volSuffix $ changelogStates dblog)

  let immTip :: Point blk
      immTip = castPoint $ getTip $ AS.anchor volStates

      rollbackMax :: Word64
      rollbackMax = fromIntegral $ AS.length volStates

      rollbackTo pt
        | pointSlot pt < pointSlot immTip = throwError $ PointTooOld Nothing
        | otherwise = case rollback pt dblog of
            Nothing -> throwError PointNotOnChain
            Just dblog' -> pure dblog'
  -- Get the prefix of the dblog ending in the specified target.
  case target of
    Right VolatileTip -> pure dblog
    Right ImmutableTip -> rollbackTo immTip
    Right (SpecificPoint pt) -> rollbackTo pt
    Left n -> do
      when (n > rollbackMax) $
        throwError $
          PointTooOld $
            Just
              ExceededRollback
                { rollbackMaximum = rollbackMax
                , rollbackRequested = n
                }
      case rollbackN n dblog of
        Nothing -> error "unreachable"
        Just dblog' -> pure dblog'

{-------------------------------------------------------------------------------
  Make forkers from consistent views
-------------------------------------------------------------------------------}

newForker ::
  forall m l blk.
  ( IOLike m
  , HasLedgerTables l
  , LedgerSupportsProtocol blk
  , NoThunks (l EmptyMK)
  , GetTip l
  , StandardHash l
  ) =>
  LedgerDBHandle m l blk ->
  LedgerDBEnv m l blk ->
  (ResourceKey m, StrictTVar m (m ())) ->
  ResourceRegistry m ->
  DbChangelog l ->
  ReadLocked m (Forker m l blk)
newForker h ldbEnv (rk, releaseVar) rr dblog =
  readLocked $ do
    (rk', frk) <-
      allocate
        rr
        ( \_ -> do
            dblogVar <- newTVarIO dblog
            forkerKey <- atomically $ stateTVar (ldbNextForkerKey ldbEnv) $ \r -> (r, r + 1)
            forkerMVar <- newMVar $ Left (ldbLock ldbEnv, ldbBackingStore ldbEnv, rr)
            forkerCommitted <- newTVarIO False
            let forkerEnv =
                  ForkerEnv
                    { foeBackingStoreValueHandle = forkerMVar
                    , foeChangelog = dblogVar
                    , foeSwitchVar = ldbChangelog ldbEnv
                    , foeTracer =
                        LedgerDBForkerEvent . TraceForkerEventWithKey forkerKey >$< ldbTracer ldbEnv
                    , foeWasCommitted = forkerCommitted
                    }
            atomically $ do
              -- Note that we add the forkerEnv to the 'ldbForkers' so that an exception
              -- which will close all the forkers, also closes this one, releasing the
              -- read access we acquired above.
              modifyTVar (ldbForkers ldbEnv) $ Map.insert forkerKey forkerEnv
              -- Empty the tvar created for allocating the unsafe read access,
              -- so that it is the forker the one that takes care of releasing
              -- it.
              writeTVar releaseVar (pure ())
            void $ release rk
            traceWith (foeTracer forkerEnv) ForkerOpen
            pure $ (mkForker h (ldbQueryBatchSize ldbEnv) forkerKey forkerEnv)
        )
        forkerClose
    pure $ frk{forkerClose = void $ release rk'}

mkForker ::
  ( IOLike m
  , HasHeader blk
  , HasLedgerTables l
  , GetTip l
  , StandardHash l
  ) =>
  LedgerDBHandle m l blk ->
  QueryBatchSize ->
  ForkerKey ->
  ForkerEnv m l blk ->
  Forker m l blk
mkForker h qbs forkerKey forkerEnv =
  Forker
    { forkerClose = implForkerClose h forkerKey forkerEnv
    , forkerReadTables = getForkerEnv1 h forkerKey implForkerReadTables
    , forkerRangeReadTables = getForkerEnv1 h forkerKey (implForkerRangeReadTables qbs)
    , forkerGetLedgerState = getForkerEnvSTM h forkerKey implForkerGetLedgerState
    , forkerReadStatistics = getForkerEnv h forkerKey implForkerReadStatistics
    , forkerPush = getForkerEnv1 h forkerKey implForkerPush
    , forkerCommit = getForkerEnvSTM h forkerKey implForkerCommit
    }

-- | This function receives an environment instead of reading it from
-- the DB such that we can close the forker even if the LedgerDB is
-- closed. In fact this should never happen as clients of the LedgerDB
-- (which are the ones opening forkers) should never outlive the
-- LedgerDB.
implForkerClose ::
  IOLike m =>
  LedgerDBHandle m l blk ->
  ForkerKey ->
  ForkerEnv m l blk ->
  m ()
implForkerClose (LDBHandle varState) forkerKey env = do
  frk <-
    atomically $
      readTVar varState >>= \case
        LedgerDBClosed -> pure Nothing
        LedgerDBOpen ldbEnv -> do
          stateTVar
            (ldbForkers ldbEnv)
            (\m -> Map.updateLookupWithKey (\_ _ -> Nothing) forkerKey m)
  case frk of
    Nothing -> pure ()
    Just e -> do
      wc <- readTVarIO (foeWasCommitted e)
      traceWith (foeTracer e) (ForkerClose $ if wc then ForkerWasCommitted else ForkerWasUncommitted)
  closeForkerEnv env
