{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Many functions here are very similar to the ones in
-- "Ouroboros.Consensus.Storage.LedgerDB.V2". When we delete V1, this
-- module will be gone.
module Ouroboros.Consensus.Storage.LedgerDB.V1 (mkInitDb) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Control.Tracer
import Data.Containers.ListUtils (nubOrd)
import qualified Data.Foldable as Foldable
import Data.Functor ((<&>))
import Data.Functor.Contravariant ((>$<))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
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

newtype SnapshotExc blk = SnapshotExc {getSnapshotFailure :: SnapshotFailure blk}
  deriving (Show, Exception)

mkInitDb ::
  forall m blk.
  ( LedgerSupportsProtocol blk
  , IOLike m
  , HasHardForkHistory blk
  , LedgerSupportsLedgerDB blk
  ) =>
  Complete LedgerDbArgs m blk ->
  V1.LedgerDbBackendArgs m ExtLedgerState blk ->
  ResolveBlock m blk ->
  SnapshotManagerV1 m blk ->
  GetVolatileSuffix m blk ->
  InitDB (DbChangelog' blk, BackingStore' m blk) m blk
mkInitDb args bss getBlock snapManager getVolatileSuffix =
  InitDB
    { initFromGenesis = do
        st <- lgrGenesis
        let genesis = forgetLedgerTables st
            chlog = DbCh.empty genesis
        backingStore <- newBackingStore bsTracer baArgs lgrHasFS' genesis (projectLedgerTables st)
        pure (chlog, backingStore)
    , initFromSnapshot = \ds ->
        runExceptT
          ( loadSnapshot
              bsTracer
              baArgs
              (configCodec . getExtLedgerCfg . ledgerDbCfg $ lgrConfig)
              lgrHasFS'
              ds
          )
    , initReapplyBlock = \cfg blk (chlog, bstore) -> do
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
        pure (chlog'', bstore)
    , currentTip = \(ch, _) -> ledgerState . current $ ch
    , mkLedgerDb = \(db, ldbBackingStore) -> do
        (varDB, prevApplied) <-
          (,) <$> newTVarIO db <*> newTVarIO Set.empty
        flushLock <- mkLedgerDBLock
        nextForkerKey <- newTVarIO (ForkerKey 0)
        lastSnapshotRequestedAt <- newTVarIO Nothing
        let env =
              LedgerDBEnv
                { ldbChangelog = varDB
                , ldbBackingStore = ldbBackingStore
                , ldbLock = flushLock
                , ldbPrevApplied = prevApplied
                , ldbNextForkerKey = nextForkerKey
                , ldbSnapshotPolicy = defaultSnapshotPolicy (ledgerDbCfgSecParam lgrConfig) lgrSnapshotPolicyArgs
                , ldbTracer = lgrTracer
                , ldbCfg = lgrConfig
                , ldbHasFS = lgrHasFS'
                , ldbShouldFlush = shouldFlush flushFreq
                , ldbQueryBatchSize = lgrQueryBatchSize
                , ldbResolveBlock = getBlock
                , ldbGetVolatileSuffix = getVolatileSuffix
                , ldbLastSuccessfulSnapshotRequestedAt = lastSnapshotRequestedAt
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
    , lgrQueryBatchSize
    } = args

  lgrHasFS' = SnapshotsFS lgrHasFS

  V1Args flushFreq baArgs = bss

implMkLedgerDb ::
  forall m blk.
  ( IOLike m
  , HasCallStack
  , LedgerSupportsProtocol blk
  , HasHardForkHistory blk
  ) =>
  LedgerDBHandle m ExtLedgerState blk ->
  SnapshotManagerV1 m blk ->
  (LedgerDB' m blk, TestInternals' m blk)
implMkLedgerDb h snapManager =
  ( LedgerDB
      { getVolatileTip = getEnvSTM h implGetVolatileTip
      , getImmutableTip = getEnvSTM h implGetImmutableTip
      , getPastLedgerState = getEnvSTM1 h implGetPastLedgerState
      , getHeaderStateHistory = getEnvSTM h implGetHeaderStateHistory
      , openForkerAtTarget = openNewForkerAtTarget h
      , validateFork = getEnv5 h (implValidate h)
      , getPrevApplied = getEnvSTM h implGetPrevApplied
      , garbageCollect = getEnv1 h implGarbageCollect
      , tryTakeSnapshot = getEnv2 h (implTryTakeSnapshot snapManager)
      , tryFlush = getEnv h implTryFlush
      , closeDB = implCloseDB h
      }
  , mkInternals h snapManager
  )

implGetVolatileTip ::
  (MonadSTM m, GetTip (l blk)) =>
  LedgerDBEnv m l blk ->
  STM m (l blk EmptyMK)
implGetVolatileTip = fmap current . readTVar . ldbChangelog

implGetImmutableTip ::
  (MonadSTM m, GetTip (l blk)) =>
  LedgerDBEnv m l blk ->
  STM m (l blk EmptyMK)
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
  , IsLedger l blk
  , StandardHash (l blk)
  , HasLedgerTables l blk
  , HeaderHash (l blk) ~ HeaderHash blk
  ) =>
  LedgerDBEnv m l blk -> Point blk -> STM m (Maybe (l blk EmptyMK))
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
  , IsLedger LedgerState blk
  , HasHardForkHistory blk
  , HasAnnTip blk
  ) =>
  LedgerDBEnv m ExtLedgerState blk -> STM m (HeaderStateHistory blk)
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
  , StandardHash (l blk)
  , ApplyBlock l blk
  ) =>
  LedgerDBHandle m l blk ->
  LedgerDBEnv m l blk ->
  (TraceValidateEvent blk -> m ()) ->
  BlockCache blk ->
  Word64 ->
  NonEmpty (Header blk) ->
  SuccessForkerAction m l blk ->
  m (ValidateResult l blk)
implValidate h ldbEnv tr cache rollbacks hdrs onSuccess =
  validate (ledgerDbCfgComputeLedgerEvents $ ldbCfg ldbEnv) $
    ValidateArgs
      (ldbResolveBlock ldbEnv)
      (ledgerDbCfg $ ldbCfg ldbEnv)
      ( \l -> do
          prev <- readTVar (ldbPrevApplied ldbEnv)
          writeTVar (ldbPrevApplied ldbEnv) (Foldable.foldl' (flip Set.insert) prev l)
      )
      (readTVar (ldbPrevApplied ldbEnv))
      (withForkerByRollback h)
      onSuccess
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
  , IsLedger LedgerState blk
  ) =>
  LedgerDBEnv m ExtLedgerState blk -> SlotNo -> m ()
implGarbageCollect env slotNo = atomically $ do
  modifyTVar (ldbChangelog env) $
    prune (LedgerDbPruneBeforeSlot slotNo)
  modifyTVar (ldbPrevApplied env) $
    Set.dropWhileAntitone ((< slotNo) . realPointSlot)

implTryTakeSnapshot ::
  (IsLedger LedgerState blk, HasLedgerTables LedgerState blk, IOLike m) =>
  SnapshotManagerV1 m blk ->
  LedgerDBEnv m ExtLedgerState blk ->
  m () ->
  (SnapshotDelayRange -> m DiffTime) ->
  m ()
implTryTakeSnapshot snapManager env copyBlocks getRandomDelay = do
  now <- getMonotonicTime
  timeSinceLastSnapshot <- do
    mLastSnapshotRequested <- readTVarIO $ ldbLastSuccessfulSnapshotRequestedAt env
    forM mLastSnapshotRequested $ \lastSnapshotRequested -> do
      pure $ now `diffTime` lastSnapshotRequested
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
            { sscTimeSinceLast = timeSinceLastSnapshot
            , sscSnapshotSlots = immutableSlots
            }
  case NonEmpty.nonEmpty snapshotSlots of
    Nothing -> pure ()
    Just nonEmptySnapshotSlots -> do
      copyBlocks

      delayBeforeSnapshotting <- getRandomDelay (onDiskSnapshotDelayRange (ldbSnapshotPolicy env))
      traceWith (LedgerDBSnapshotEvent >$< ldbTracer env) $
        SnapshotRequestDelayed now delayBeforeSnapshotting nonEmptySnapshotSlots
      threadDelay delayBeforeSnapshotting

      forM_ nonEmptySnapshotSlots $ \slot -> do
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
        atomically $ writeTVar (ldbLastSuccessfulSnapshotRequestedAt env) (Just $! now)
        void $
          trimSnapshots snapManager (ldbSnapshotPolicy env)
      traceWith (LedgerDBSnapshotEvent >$< ldbTracer env) $
        SnapshotRequestCompleted

-- If the DbChangelog in the LedgerDB can flush (based on the SnapshotPolicy
-- with which this LedgerDB was opened), flush differences to the backing
-- store. Note this acquires a write lock on the backing store.
implTryFlush ::
  (IOLike m, HasLedgerTables l blk, GetTip (l blk)) =>
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
  whenJust mbOpenEnv $ void . bsClose . ldbBackingStore

mkInternals ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBHandle m ExtLedgerState blk ->
  SnapshotManagerV1 m blk ->
  TestInternals' m blk
mkInternals h snapManager =
  TestInternals
    { takeSnapshotNOW = getEnv2 h (implIntTakeSnapshot snapManager)
    , wipeLedgerDB = void $ destroySnapshots snapManager
    , truncateSnapshots = getEnv h $ void . implIntTruncateSnapshots . ldbHasFS
    , push = getEnv1 h implIntPush
    , reapplyThenPushNOW = getEnv1 h implIntReapplyThenPush
    , closeLedgerDB = getEnv h $ void . bsClose . ldbBackingStore
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
  , LedgerSupportsProtocol blk
  ) =>
  SnapshotManagerV1 m blk ->
  LedgerDBEnv m ExtLedgerState blk ->
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
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBEnv m ExtLedgerState blk -> ExtLedgerState blk DiffMK -> m ()
implIntPush env st = do
  chlog <- readTVarIO $ ldbChangelog env
  let chlog' = pruneToImmTipOnly $ extend st chlog
  atomically $ writeTVar (ldbChangelog env) chlog'

implIntReapplyThenPush ::
  ( IOLike m
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBEnv m ExtLedgerState blk -> blk -> m ()
implIntReapplyThenPush env blk = do
  chlog <- readTVarIO $ ldbChangelog env
  chlog' <- reapplyThenPush (ldbCfg env) blk (readKeySets (ldbBackingStore env)) chlog
  atomically $ writeTVar (ldbChangelog env) chlog'

{-------------------------------------------------------------------------------
  Flushing
-------------------------------------------------------------------------------}

flushLedgerDB ::
  (MonadSTM m, GetTip (l blk), HasLedgerTables l blk) =>
  StrictTVar m (DbChangelog l blk) ->
  LedgerBackingStore m l blk ->
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
flushIntoBackingStore :: LedgerBackingStore m l blk -> DiffsToFlush l blk -> WriteLocked m ()
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
  , NoThunks (l blk EmptyMK)
  , NoThunks (TxIn blk)
  , NoThunks (TxOut blk)
  , NoThunks (LedgerCfg l blk)
  ) =>
  NoThunks (LedgerDBState m l blk)

type LedgerDBEnv :: (Type -> Type) -> StateKind -> Type -> Type
data LedgerDBEnv m l blk = LedgerDBEnv
  { ldbChangelog :: !(StrictTVar m (DbChangelog l blk))
  -- ^ INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip of
  -- the current chain of the ChainDB.
  , ldbBackingStore :: !(LedgerBackingStore m l blk)
  -- ^ Handle to the ledger's backing store, containing the parts that grow too
  -- big for in-memory residency
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
  , ldbNextForkerKey :: !(StrictTVar m ForkerKey)
  , ldbSnapshotPolicy :: !SnapshotPolicy
  , ldbTracer :: !(Tracer m (TraceEvent blk))
  , ldbCfg :: !(LedgerDbCfg l blk)
  , ldbHasFS :: !(SnapshotsFS m)
  , ldbShouldFlush :: !(Word64 -> Bool)
  -- ^ Determine whether we should flush depending on the number of flushable
  -- diffs that we currently have in the LedgerDB, based on the flush
  -- frequency that was provided when opening the LedgerDB.
  , ldbQueryBatchSize :: !QueryBatchSize
  , ldbResolveBlock :: !(ResolveBlock m blk)
  , ldbGetVolatileSuffix :: !(GetVolatileSuffix m blk)
  , ldbLastSuccessfulSnapshotRequestedAt :: !(StrictTVar m (Maybe Time))
  -- ^ The time at which the latest successfully-completed snapshot was
  -- requested. Note that this is not the the last time any snapshot was
  -- requested -- there may be later snapshot requests that have failed, or that
  -- are currently in progress (but may be blocked by a snapshot delay or
  -- working).
  }
  deriving Generic

deriving instance
  ( IOLike m
  , LedgerSupportsProtocol blk
  , NoThunks (l blk EmptyMK)
  , NoThunks (TxIn blk)
  , NoThunks (TxOut blk)
  , NoThunks (LedgerCfg l blk)
  ) =>
  NoThunks (LedgerDBEnv m l blk)

-- | Check if the LedgerDB is open, if so, executing the given function on the
-- 'LedgerDBEnv', otherwise, throw a 'CloseDBError'.
getEnv ::
  forall m l blk r.
  (IOLike m, HasCallStack) =>
  LedgerDBHandle m l blk ->
  (LedgerDBEnv m l blk -> m r) ->
  m r
getEnv (LDBHandle varState) f =
  readTVarIO varState >>= \case
    LedgerDBOpen env -> f env
    LedgerDBClosed -> throwIO $ ClosedDBError prettyCallStack

-- | Variant 'of 'getEnv' for functions taking one argument.
getEnv1 ::
  (IOLike m, HasCallStack) =>
  LedgerDBHandle m l blk ->
  (LedgerDBEnv m l blk -> a -> m r) ->
  a ->
  m r
getEnv1 h f a = getEnv h (`f` a)

-- | Variant 'of 'getEnv' for functions taking two arguments.
getEnv2 ::
  (IOLike m, HasCallStack) =>
  LedgerDBHandle m l blk ->
  (LedgerDBEnv m l blk -> a -> b -> m r) ->
  a ->
  b ->
  m r
getEnv2 h f a b = getEnv h (\env -> f env a b)

-- | Variant 'of 'getEnv' for functions taking five arguments.
getEnv5 ::
  (IOLike m, HasCallStack) =>
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
  (IOLike m, HasCallStack) =>
  LedgerDBHandle m l blk ->
  (LedgerDBEnv m l blk -> STM m r) ->
  STM m r
getEnvSTM (LDBHandle varState) f =
  readTVar varState >>= \case
    LedgerDBOpen env -> f env
    LedgerDBClosed -> throwSTM $ ClosedDBError prettyCallStack

-- | Variant of 'getEnv1' that works in 'STM'.
getEnvSTM1 ::
  forall m l blk a r.
  (IOLike m, HasCallStack) =>
  LedgerDBHandle m l blk ->
  (LedgerDBEnv m l blk -> a -> STM m r) ->
  a ->
  STM m r
getEnvSTM1 (LDBHandle varState) f a =
  readTVar varState >>= \case
    LedgerDBOpen env -> f env a
    LedgerDBClosed -> throwSTM $ ClosedDBError prettyCallStack

{-------------------------------------------------------------------------------
  Forkers
-------------------------------------------------------------------------------}

-- | Will call 'error' if the point is not on the LedgerDB
openNewForkerAtTarget ::
  ( HeaderHash (l blk) ~ HeaderHash blk
  , IOLike m
  , IsLedger l blk
  , StandardHash (l blk)
  , HasLedgerTables l blk
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBHandle m l blk ->
  Target (Point blk) ->
  m (Either GetForkerError (Forker m l blk))
openNewForkerAtTarget h pt = withTransferrableReadAccess h (Right pt)

withForkerByRollback ::
  ( HeaderHash (l blk) ~ HeaderHash blk
  , IOLike m
  , IsLedger l blk
  , StandardHash (l blk)
  , HasLedgerTables l blk
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBHandle m l blk ->
  -- | How many blocks to rollback from the tip
  Word64 ->
  (Forker m l blk -> m r) ->
  m (Either GetForkerError r)
withForkerByRollback h n k =
  bracket
    (withTransferrableReadAccess h (Left n))
    (either (const $ pure ()) forkerClose)
    (either (pure . Left) (fmap Right . k))

-- | Acquire read access and then allocate a forker, acquiring it at the given
-- point or rollback.
withTransferrableReadAccess ::
  ( HeaderHash (l blk) ~ HeaderHash blk
  , IOLike m
  , IsLedger l blk
  , StandardHash (l blk)
  , HasLedgerTables l blk
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBHandle m l blk ->
  Either Word64 (Target (Point blk)) ->
  m (Either GetForkerError (Forker m l blk))
withTransferrableReadAccess h f = getEnv h $ \ldbEnv -> do
  -- This TVar will be used to maybe release the read lock by the resource
  -- registry. Once the forker was opened it will be emptied.
  bracket
    ( do
        tv <- newTVarIO (pure ())
        atomically $ do
          -- Populate the tvar with the releasing action. Creating the forker will empty this
          writeTVar tv (atomically $ unsafeReleaseReadAccess (ldbLock ldbEnv))
          -- Acquire the read access
          unsafeAcquireReadAccess (ldbLock ldbEnv)
        pure tv
    )
    (join . readTVarIO)
    ( \tv ->
        unsafeRunReadLocked
          ( unsafeAcquireAtTarget ldbEnv f
              >>= \case
                Left err -> do
                  pure (Left err)
                Right chlog -> do
                  Right <$> newForker ldbEnv tv chlog
          )
    )

-- | Acquire both a value handle and a db changelog at the tip. Holds a read lock
-- while doing so.
unsafeAcquireAtTarget ::
  forall m l blk.
  ( HeaderHash (l blk) ~ HeaderHash blk
  , IOLike m
  , IsLedger l blk
  , StandardHash (l blk)
  , HasLedgerTables l blk
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBEnv m l blk ->
  Either Word64 (Target (Point blk)) ->
  ReadLocked m (Either GetForkerError (DbChangelog l blk))
unsafeAcquireAtTarget ldbEnv target = readLocked $ runExceptT $ do
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
  , HasLedgerTables l blk
  , NoThunks (l blk EmptyMK)
  , GetTip (l blk)
  , StandardHash (l blk)
  ) =>
  LedgerDBEnv m l blk ->
  StrictTVar m (m ()) ->
  DbChangelog l blk ->
  ReadLocked m (Forker m l blk)
newForker ldbEnv releaseVar dblog = readLocked $ do
  dblogVar <- newTVarIO dblog
  forkerKey <- atomically $ stateTVar (ldbNextForkerKey ldbEnv) $ \r -> (r, r + 1)
  forkerMVar <- newMVar $ Left (ldbLock ldbEnv, ldbBackingStore ldbEnv)
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
  atomically $
    -- Empty the tvar created for allocating the unsafe read access,
    -- so that it is the forker the one that takes care of releasing
    -- it.
    writeTVar releaseVar (pure ())
  traceWith (foeTracer forkerEnv) ForkerOpen
  pure $
    Forker
      { forkerClose = closeForkerEnv forkerEnv
      , forkerReadTables = implForkerReadTables forkerEnv
      , forkerRangeReadTables = implForkerRangeReadTables (ldbQueryBatchSize ldbEnv) forkerEnv
      , forkerGetLedgerState = implForkerGetLedgerState forkerEnv
      , forkerReadStatistics = implForkerReadStatistics forkerEnv
      , forkerPush = implForkerPush forkerEnv
      , forkerCommit = pure <$> implForkerCommit forkerEnv
      }
