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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2 (mkInitDb) where

import qualified Control.Monad as Monad (join, void)
import Control.Monad.Except
import Control.RAWLock
import qualified Control.RAWLock as RAWLock
import Control.ResourceRegistry
import Control.Tracer
import Data.Bifunctor (bimap)
import qualified Data.Foldable as Foldable
import Data.Functor.Contravariant ((>$<))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import Data.Tuple (Solo (..))
import Data.Word
import GHC.Generics
import NoThunks.Class
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
import Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Args
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.TraceEvent
import Ouroboros.Consensus.Storage.LedgerDB.V2.Backend
import Ouroboros.Consensus.Storage.LedgerDB.V2.Forker
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Ouroboros.Consensus.Util (whenJust)
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.NormalForm.StrictTVar ()
import qualified Ouroboros.Network.AnchoredSeq as AS
import Ouroboros.Network.Protocol.LocalStateQuery.Type
import System.FS.API
import Prelude hiding (read)

type SnapshotManagerV2 m blk = SnapshotManager m m blk (StateRef m (ExtLedgerState blk))

newtype SnapshotExc blk = SnapshotExc {getSnapshotFailure :: SnapshotFailure blk}
  deriving (Show, Exception)

mkInitDb ::
  forall m blk backend.
  ( LedgerSupportsProtocol blk
  , HasHardForkHistory blk
  , Backend m backend blk
  , IOLike m
  ) =>
  Complete LedgerDbArgs m blk ->
  ResolveBlock m blk ->
  SnapshotManagerV2 m blk ->
  GetVolatileSuffix m blk ->
  Resources m backend ->
  InitDB (LedgerSeq' m blk) m blk
mkInitDb args getBlock snapManager getVolatileSuffix res = do
  InitDB
    { initFromGenesis = do
        genesis <- lgrGenesis
        runWithTempRegistry $ do
          sr <- createAndPopulateStateRefFromGenesis v2Tracer res genesis
          let lseq = LedgerSeq . AS.Empty $ sr
          pure (lseq, lseq)
    , initCloseDB = closeLedgerSeq
    , initFromSnapshot = \ds ->
        fmap (bimap getSnapshotFailure id)
          $ try
          $ runWithTempRegistry
          $ ( >>=
                ( \case
                    -- captured above
                    Left failure -> throwIO $ SnapshotExc failure
                    Right (st, p) ->
                      let lseq = LedgerSeq (AS.Empty st)
                       in pure ((lseq, p), lseq)
                )
            )
          $ runExceptT
            ( openStateRefFromSnapshot
                v2Tracer
                (configCodec . getExtLedgerCfg . ledgerDbCfg $ lgrConfig)
                lgrHasFS
                res
                ds
            )
    , initReapplyBlock = reapplyThenPush
    , currentTip = ledgerState . current
    , mkLedgerDb = \lseq -> do
        varDB <- newTVarIO lseq
        prevApplied <- newTVarIO Set.empty
        lock <- RAWLock.new ()
        nextForkerKey <- newTVarIO (ForkerKey 0)
        let env =
              LedgerDBEnv
                { ldbSeq = varDB
                , ldbPrevApplied = prevApplied
                , ldbNextForkerKey = nextForkerKey
                , ldbSnapshotPolicy = defaultSnapshotPolicy (ledgerDbCfgSecParam lgrConfig) lgrSnapshotPolicyArgs
                , ldbTracer = tr
                , ldbCfg = lgrConfig
                , ldbHasFS = lgrHasFS
                , ldbResolveBlock = getBlock
                , ldbQueryBatchSize = lgrQueryBatchSize
                , ldbOpenHandlesLock = lock
                , ldbGetVolatileSuffix = getVolatileSuffix
                , ldbBackendResources = SomeResources res
                }
        h <- LDBHandle <$> newTVarIO (LedgerDBOpen env)
        pure $ implMkLedgerDb h snapManager
    }
 where
  LedgerDbArgs
    { lgrConfig
    , lgrGenesis
    , lgrHasFS
    , lgrSnapshotPolicyArgs
    , lgrQueryBatchSize
    } = args

  v2Tracer :: Tracer m LedgerDBV2Trace
  !v2Tracer = LedgerDBFlavorImplEvent . FlavorImplSpecificTraceV2 >$< tr

  !tr = lgrTracer args

implMkLedgerDb ::
  forall m l blk.
  ( IOLike m
  , HasCallStack
  , StandardHash l
  , LedgerSupportsProtocol blk
  , HasHardForkHistory blk
  , ApplyBlock l blk
  ) =>
  LedgerDBHandle m l blk ->
  SnapshotManager m m blk (StateRef m l) ->
  (LedgerDB m l blk, TestInternals m l blk)
implMkLedgerDb h snapManager =
  ( LedgerDB
      { getVolatileTip = getEnvSTM h implGetVolatileTip
      , getImmutableTip = getEnvSTM h implGetImmutableTip
      , getPastLedgerState = \s -> getEnvSTM h (flip implGetPastLedgerState s)
      , getHeaderStateHistory = getEnvSTM h implGetHeaderStateHistory
      , unsafeGetForkerAtTarget = unsafeNewForkerAtTarget h
      , validateFork = getEnv5 h (implValidate h)
      , getPrevApplied = getEnvSTM h implGetPrevApplied
      , garbageCollect = \s -> getEnv h (flip implGarbageCollect s)
      , tryTakeSnapshot = getEnv3 h (implTryTakeSnapshot snapManager)
      , tryFlush = getEnv h implTryFlush
      , closeDB = implCloseDB h
      }
  , mkInternals h snapManager
  )

mkInternals ::
  forall m l blk.
  ( IOLike m
  , LedgerSupportsProtocol blk
  , ApplyBlock (ExtLedgerState blk) blk
  , StandardHash l
  , ApplyBlock l blk
  ) =>
  LedgerDBHandle m l blk ->
  SnapshotManager m m blk (StateRef m l) ->
  TestInternals m l blk
mkInternals h snapManager =
  TestInternals
    { takeSnapshotNOW = \whereTo suff -> getEnv h $ \env -> do
        let selectWhereTo = case whereTo of
              TakeAtImmutableTip -> anchorHandle
              TakeAtVolatileTip -> currentHandle
        withStateRef env (MkSolo . selectWhereTo) $ \(MkSolo st) ->
          Monad.void $
            takeSnapshot
              snapManager
              suff
              st
    , wipeLedgerDB = destroySnapshots snapManager
    , truncateSnapshots = getEnv h $ implIntTruncateSnapshots snapManager . ldbHasFS
    , push = \st -> do
        bracket
          ( either (error "Unreachable, Volatile tip MUST be in LedgerDB") id
              <$> unsafeNewForkerAtTarget h VolatileTip
          )
          forkerClose
          ( \frk -> do
              forkerPush frk st >> Monad.join (atomically (forkerCommit frk))
              getEnv h pruneLedgerSeq
          )
    , reapplyThenPushNOW = \blk -> getEnv h $ \env -> do
        bracket
          ( either (error "Unreachable, Volatile tip MUST be in LedgerDB") id
              <$> unsafeNewForkerAtTarget h VolatileTip
          )
          forkerClose
          ( \frk -> do
              st <- atomically $ forkerGetLedgerState frk
              tables <- forkerReadTables frk (getBlockKeySets blk)
              let st' =
                    tickThenReapply
                      (ledgerDbCfgComputeLedgerEvents (ldbCfg env))
                      (ledgerDbCfg $ ldbCfg env)
                      blk
                      (st `withLedgerTables` tables)
              forkerPush frk st' >> Monad.join (atomically (forkerCommit frk))
              pruneLedgerSeq env
          )
    , closeLedgerDB = implCloseDB h
    , getNumLedgerTablesHandles = getEnv h $ \env -> do
        l <- readTVarIO (ldbSeq env)
        -- We always have a state at the anchor.
        pure $ 1 + maxRollback l
    , withForkerAtTip = implWithForkerAtTip h
    }
 where
  pruneLedgerSeq :: LedgerDBEnv m l blk -> m ()
  pruneLedgerSeq env =
    Monad.join $ atomically $ stateTVar (ldbSeq env) $ pruneToImmTipOnly

-- | Testing only! Truncate all snapshots in the DB. We only truncate the state
-- file because it is unclear how to truncate the LSM database without
-- corrupting it.
implIntTruncateSnapshots :: MonadThrow m => SnapshotManager m m blk st -> SomeHasFS m -> m ()
implIntTruncateSnapshots snapManager (SomeHasFS fs) = do
  snapshotsMapM_ snapManager $
    \pre -> withFile fs (snapshotToStatePath pre) (AppendMode AllowExisting) $
      \h -> hTruncate fs h 0

implGetVolatileTip ::
  (MonadSTM m, GetTip l) =>
  LedgerDBEnv m l blk ->
  STM m (l EmptyMK)
implGetVolatileTip = fmap current . getVolatileLedgerSeq

implGetImmutableTip ::
  (MonadSTM m, GetTip l) =>
  LedgerDBEnv m l blk ->
  STM m (l EmptyMK)
implGetImmutableTip = fmap anchor . getVolatileLedgerSeq

implGetPastLedgerState ::
  ( MonadSTM m
  , HasHeader blk
  , IsLedger l
  , StandardHash l
  , HeaderHash l ~ HeaderHash blk
  ) =>
  LedgerDBEnv m l blk -> Point blk -> STM m (Maybe (l EmptyMK))
implGetPastLedgerState env point =
  getPastLedgerAt point <$> getVolatileLedgerSeq env

implGetHeaderStateHistory ::
  ( MonadSTM m
  , l ~ ExtLedgerState blk
  , IsLedger (LedgerState blk)
  , HasHardForkHistory blk
  , HasAnnTip blk
  ) =>
  LedgerDBEnv m l blk -> STM m (HeaderStateHistory blk)
implGetHeaderStateHistory env = do
  ldb <- getVolatileLedgerSeq env
  let currentLedgerState = ledgerState $ current ldb
      -- This summary can convert all tip slots of the ledger states in the
      -- @ledgerDb@ as these are not newer than the tip slot of the current
      -- ledger state (Property 17.1 in the Consensus report).
      summary = hardForkSummary (configLedger $ getExtLedgerCfg $ ledgerDbCfg $ ldbCfg env) currentLedgerState
      mkHeaderStateWithTime' =
        mkHeaderStateWithTimeFromSummary summary
          . headerState
          . state
  pure
    . HeaderStateHistory
    . AS.bimap mkHeaderStateWithTime' mkHeaderStateWithTime'
    . getLedgerSeq
    $ ldb

implValidate ::
  forall m l blk.
  ( IOLike m
  , HasCallStack
  , ApplyBlock l blk
  , StandardHash l
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBHandle m l blk ->
  LedgerDBEnv m l blk ->
  (TraceValidateEvent blk -> m ()) ->
  BlockCache blk ->
  Word64 ->
  NonEmpty (Header blk) ->
  (Forker m l -> m ()) ->
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

-- | Remove 'LedgerSeq' states older than the given slot, and all points with a
-- slot older than the given slot from the set of previously applied points.
implGarbageCollect :: (IOLike m, GetTip l) => LedgerDBEnv m l blk -> SlotNo -> m ()
implGarbageCollect env slotNo = do
  atomically $
    modifyTVar (ldbPrevApplied env) $
      Set.dropWhileAntitone ((< slotNo) . realPointSlot)
  Monad.join $ RAWLock.withWriteAccess (ldbOpenHandlesLock env) $ \() -> do
    close <- atomically $ stateTVar (ldbSeq env) $ prune (LedgerDbPruneBeforeSlot slotNo)
    pure (close, ())

implTryTakeSnapshot ::
  forall m l blk.
  ( IOLike m
  , GetTip l
  ) =>
  SnapshotManager m m blk (StateRef m l) ->
  LedgerDBEnv m l blk ->
  m () ->
  Maybe (Time, Time) ->
  Word64 ->
  m SnapCounters
implTryTakeSnapshot snapManager env copyBlocks mTime nrBlocks =
  if onDiskShouldTakeSnapshot (ldbSnapshotPolicy env) (uncurry (flip diffTime) <$> mTime) nrBlocks
    then do
      copyBlocks
      withStateRef env (MkSolo . anchorHandle) $ \(MkSolo st) ->
        Monad.void $
          takeSnapshot
            snapManager
            Nothing
            st
      Monad.void $
        trimSnapshots
          snapManager
          (ldbSnapshotPolicy env)
      (`SnapCounters` 0) . Just <$> maybe getMonotonicTime (pure . snd) mTime
    else
      pure $ SnapCounters (fst <$> mTime) nrBlocks

-- In the first version of the LedgerDB for UTxO-HD, there is a need to
-- periodically flush the accumulated differences to the disk. However, in the
-- second version there is no need to do so, and because of that, this function
-- does nothing in this case.
implTryFlush :: Applicative m => LedgerDBEnv m l blk -> m ()
implTryFlush _ = pure ()

implCloseDB :: forall m l blk. IOLike m => LedgerDBHandle m l blk -> m ()
implCloseDB (LDBHandle varState) = do
  res <-
    atomically $
      readTVar varState >>= \case
        -- Idempotent
        LedgerDBClosed -> pure Nothing
        LedgerDBOpen env -> do
          writeTVar varState LedgerDBClosed
          pure (Just $ (ldbSeq env, ldbBackendResources env))
  whenJust
    res
    ( \(s, SomeResources res') -> do
        s' <- readTVarIO s
        closeLedgerSeq s'
        releaseResources (Proxy @blk) res'
    )

{-------------------------------------------------------------------------------
  The LedgerDBEnv
-------------------------------------------------------------------------------}

type LedgerDBEnv :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data LedgerDBEnv m l blk = LedgerDBEnv
  { ldbSeq :: !(StrictTVar m (LedgerSeq m l))
  -- ^ INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip of
  -- the current chain of the ChainDB.
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
  , ldbCfg :: !(LedgerDbCfg l)
  , ldbHasFS :: !(SomeHasFS m)
  , ldbResolveBlock :: !(ResolveBlock m blk)
  , ldbQueryBatchSize :: !QueryBatchSize
  , ldbOpenHandlesLock :: !(RAWLock m ())
  -- ^ While holding a read lock (at least), all handles in the 'ldbSeq' are
  -- guaranteed to be open. During this time, the handle can be duplicated and
  -- then be used independently, see 'unsafeGetStateRef' and 'withStateRef'.
  --
  -- We acquire read access when opening a duplicate of a handle (see
  -- 'unsafeGetStateRef').
  --
  -- We acquire write access when pruning the LedgerDB (see
  -- 'implGarbageCollect') and when closing orphaned handles in Chain selection
  -- (see 'implForkerCommit').
  , ldbBackendResources :: !(SomeResources m blk)
  -- ^ Resource keys used in the LSM backend so that the closing function used
  -- in tests can release such resources. These are the resource keys for the
  -- LSM session and the resource key for the BlockIO interface.
  , ldbGetVolatileSuffix :: !(GetVolatileSuffix m blk)
  }
  deriving Generic

deriving instance
  ( IOLike m
  , LedgerSupportsProtocol blk
  , NoThunks (l EmptyMK)
  , NoThunks (TxIn l)
  , NoThunks (TxOut l)
  , NoThunks (LedgerCfg l)
  , NoThunks (SomeResources m blk)
  ) =>
  NoThunks (LedgerDBEnv m l blk)

{-------------------------------------------------------------------------------
  The LedgerDBHandle
-------------------------------------------------------------------------------}

type LedgerDBHandle :: (Type -> Type) -> LedgerStateKind -> Type -> Type
newtype LedgerDBHandle m l blk
  = LDBHandle (StrictTVar m (LedgerDBState m l blk))
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
  , NoThunks (SomeResources m blk)
  ) =>
  NoThunks (LedgerDBState m l blk)

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

-- | Variant 'of 'getEnv' for functions taking two arguments.
getEnv3 ::
  (IOLike m, HasCallStack, HasHeader blk) =>
  LedgerDBHandle m l blk ->
  (LedgerDBEnv m l blk -> a -> b -> c -> m r) ->
  a ->
  b ->
  c ->
  m r
getEnv3 h f a b c = getEnv h (\env -> f env a b c)

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

{-------------------------------------------------------------------------------
  Acquiring consistent views
-------------------------------------------------------------------------------}

-- | Take the suffix of the 'ldbSeq' containing the only the volatile states
-- (and the first immutable state at the anchor). The 'LedgerSeq' can contain
-- more than one immutable state if we adopted new blocks, but garbage
-- collection has not yet been run.
getVolatileLedgerSeq ::
  (MonadSTM m, GetTip l) =>
  LedgerDBEnv m l blk -> STM m (LedgerSeq m l)
getVolatileLedgerSeq env = do
  volSuffix <- getVolatileSuffix (ldbGetVolatileSuffix env)
  LedgerSeq . volSuffix . getLedgerSeq <$> readTVar (ldbSeq env)

-- | Get a 'StateRef' from the 'LedgerSeq' in the 'LedgerDBEnv', with the
-- 'LedgerTablesHandle' having been duplicated (such that the original can be
-- closed). The caller should close the handle using the returned @ResourceKey@,
-- although closing the registry will also release the handle.
--
-- For more flexibility, an arbitrary 'Traversable' of the 'StateRef' can be
-- returned; for the simple use case of getting a single 'StateRef', use @t ~
-- 'Solo'@.
unsafeGetStateRef ::
  (IOLike m, Traversable t, GetTip l) =>
  LedgerDBEnv m l blk ->
  (LedgerSeq m l -> t (StateRef m l)) ->
  m (t (StateRef m l))
unsafeGetStateRef ldbEnv project =
  RAWLock.withReadAccess (ldbOpenHandlesLock ldbEnv) $ \() -> do
    tst <- project <$> atomically (getVolatileLedgerSeq ldbEnv)
    for tst $ \st -> do
      tables' <- unsafeDuplicate (tables st)
      pure st{tables = tables'}

-- | Like 'StateRef', but takes care of closing the handle when the given action
-- returns or errors.
withStateRef ::
  (IOLike m, Traversable t, GetTip l) =>
  LedgerDBEnv m l blk ->
  (LedgerSeq m l -> t (StateRef m l)) ->
  (t (StateRef m l) -> m a) ->
  m a
withStateRef ldbEnv project f =
  bracket
    (unsafeGetStateRef ldbEnv project)
    (traverse (close . tables))
    f

unsafeAcquireAtTarget ::
  ( HeaderHash l ~ HeaderHash blk
  , IOLike m
  , GetTip l
  , StandardHash l
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBEnv m l blk ->
  Either Word64 (Target (Point blk)) ->
  m (Either GetForkerError (StateRef m l))
unsafeAcquireAtTarget ldbEnv target =
  unsafeGetStateRef ldbEnv $ \l -> case target of
    Right VolatileTip -> pure $ currentHandle l
    Right ImmutableTip -> pure $ anchorHandle l
    Right (SpecificPoint pt) -> do
      let immTip = getTip $ anchor l
      case rollback pt l of
        Nothing
          | pointSlot pt < pointSlot immTip -> throwError $ PointTooOld Nothing
          | otherwise -> throwError PointNotOnChain
        Just t' -> pure $ currentHandle t'
    Left n -> case rollbackN n l of
      Nothing ->
        throwError $
          PointTooOld $
            Just
              ExceededRollback
                { rollbackMaximum = maxRollback l
                , rollbackRequested = n
                }
      Just l' -> pure $ currentHandle l'

unsafeNewForkerAtTarget ::
  ( HeaderHash l ~ HeaderHash blk
  , IOLike m
  , IsLedger l
  , HasLedgerTables l
  , LedgerSupportsProtocol blk
  , StandardHash l
  ) =>
  LedgerDBHandle m l blk ->
  Target (Point blk) ->
  m (Either GetForkerError (Forker m l))
unsafeNewForkerAtTarget h pt = getEnv h $ \ldbEnv ->
  unsafeAcquireAtTarget ldbEnv (Right pt) >>= traverse (newForker ldbEnv)

withForkerByRollback ::
  ( HeaderHash l ~ HeaderHash blk
  , IOLike m
  , IsLedger l
  , StandardHash l
  , HasLedgerTables l
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBHandle m l blk ->
  Word64 ->
  (Forker m l -> m r) ->
  m (Either GetForkerError r)
withForkerByRollback h n k = getEnv h $ \ldbEnv ->
  bracket
    (unsafeAcquireAtTarget ldbEnv (Left n) >>= traverse (newForker ldbEnv))
    (either (const $ pure ()) forkerClose)
    (either (pure . Left) (fmap Right . k))

implWithForkerAtTip ::
  ( HeaderHash l ~ HeaderHash blk
  , IOLike m
  , IsLedger l
  , StandardHash l
  , HasLedgerTables l
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBHandle m l blk ->
  (Forker m l -> m r) ->
  m r
implWithForkerAtTip h k = getEnv h $ \ldbEnv ->
  bracket
    (unsafeAcquireAtTarget ldbEnv (Right VolatileTip) >>= traverse (newForker ldbEnv))
    (either (const $ pure ()) forkerClose)
    (either (error "Impossible! The Volatile Tip must exist in all chains!") k)

-- | Will release all handles in the 'foeLedgerSeq'.
--
-- This function receives an environment instead of reading it from
-- the DB such that we can close the forker even if the LedgerDB is
-- closed. In fact this should never happen as clients of the LedgerDB
-- (which are the ones opening forkers) should never outlive the
-- LedgerDB.
implForkerClose ::
  IOLike m =>
  ForkerEnv m l ->
  m ()
implForkerClose forkerEnv0 = withForkerEnv forkerEnv0 $ \fState ->
  if foeWasCommitted fState
    then
      traceWith (foeTracer fState) (ForkerClose ForkerWasCommitted)
    else do
      closeLedgerSeq (foeLedgerSeq fState)
      traceWith (foeTracer fState) (ForkerClose ForkerWasUncommitted)

newForker ::
  ( IOLike m
  , HasLedgerTables l
  , NoThunks (l EmptyMK)
  , GetTip l
  , StandardHash l
  ) =>
  LedgerDBEnv m l blk ->
  StateRef m l ->
  m (Forker m l)
newForker ldbEnv st = do
  forkerKey <- atomically $ stateTVar (ldbNextForkerKey ldbEnv) $ \r -> (r, r + 1)
  let tr = LedgerDBForkerEvent . TraceForkerEventWithKey forkerKey >$< ldbTracer ldbEnv
  traceWith tr ForkerOpen
  let forkerState =
        ForkerState
          { foeLedgerSeq = LedgerSeq . AS.Empty $ st
          , foeSwitchVar = ldbSeq ldbEnv
          , foeTracer = tr
          , foeLedgerDbLock = ldbOpenHandlesLock ldbEnv
          , foeWasCommitted = False
          }
  forkerEnv <- ForkerEnv <$> newTVarIO forkerState
  pure $
    Forker
      { forkerReadTables = implForkerReadTables forkerEnv
      , forkerRangeReadTables = implForkerRangeReadTables (ldbQueryBatchSize ldbEnv) forkerEnv
      , forkerGetLedgerState = implForkerGetLedgerState forkerEnv
      , forkerReadStatistics = implForkerReadStatistics forkerEnv
      , forkerPush = implForkerPush forkerEnv
      , forkerCommit = implForkerCommit forkerEnv
      , forkerClose = implForkerClose forkerEnv
      }
