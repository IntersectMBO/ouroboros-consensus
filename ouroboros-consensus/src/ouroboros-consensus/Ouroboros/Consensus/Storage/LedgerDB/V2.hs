{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2 (mkInitDb) where

import Control.Arrow ((>>>))
import qualified Control.Monad as Monad (void, (>=>))
import Control.Monad.Except
import Control.RAWLock
import qualified Control.RAWLock as RAWLock
import Control.ResourceRegistry
import Control.Tracer
import Data.Foldable (traverse_)
import qualified Data.Foldable as Foldable
import Data.Functor.Contravariant ((>$<))
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import Data.Tuple (Solo (..))
import Data.Void
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
import Ouroboros.Consensus.Storage.LedgerDB.V2.Args as V2
import Ouroboros.Consensus.Storage.LedgerDB.V2.Forker
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory as InMemory
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.NormalForm.StrictTVar ()
import qualified Ouroboros.Network.AnchoredSeq as AS
import Ouroboros.Network.Protocol.LocalStateQuery.Type
import System.FS.API
import Prelude hiding (read)

mkInitDb ::
  forall m blk.
  ( LedgerSupportsProtocol blk
  , IOLike m
  , LedgerDbSerialiseConstraints blk
  , HasHardForkHistory blk
  , LedgerSupportsInMemoryLedgerDB blk
  ) =>
  Complete LedgerDbArgs m blk ->
  Complete V2.LedgerDbFlavorArgs m ->
  ResolveBlock m blk ->
  InitDB (LedgerSeq' m blk) m blk
mkInitDb args flavArgs getBlock =
  InitDB
    { initFromGenesis = emptyF =<< lgrGenesis
    , initFromSnapshot =
        loadSnapshot (configCodec . getExtLedgerCfg . ledgerDbCfg $ lgrConfig) lgrHasFS
    , closeDb = closeLedgerSeq
    , initReapplyBlock = \a b c -> do
        (x, y) <- reapplyThenPush lgrRegistry a b c
        x
        pure y
    , currentTip = ledgerState . current
    , pruneDb = \lseq -> do
        let (rel, dbPrunedToImmDBTip) = pruneToImmTipOnly lseq
        rel
        pure dbPrunedToImmDBTip
    , mkLedgerDb = \lseq -> do
        varDB <- newTVarIO lseq
        prevApplied <- newTVarIO Set.empty
        lock <- RAWLock.new ()
        forkers <- newTVarIO Map.empty
        nextForkerKey <- newTVarIO (ForkerKey 0)
        let env =
              LedgerDBEnv
                { ldbSeq = varDB
                , ldbPrevApplied = prevApplied
                , ldbForkers = forkers
                , ldbNextForkerKey = nextForkerKey
                , ldbSnapshotPolicy = defaultSnapshotPolicy (ledgerDbCfgSecParam lgrConfig) lgrSnapshotPolicyArgs
                , ldbTracer = lgrTracer
                , ldbCfg = lgrConfig
                , ldbHasFS = lgrHasFS
                , ldbResolveBlock = getBlock
                , ldbQueryBatchSize = lgrQueryBatchSize
                , ldbOpenHandlesLock = lock
                }
        h <- LDBHandle <$> newTVarIO (LedgerDBOpen env)
        pure $ implMkLedgerDb h bss
    }
 where
  LedgerDbArgs
    { lgrConfig
    , lgrGenesis
    , lgrHasFS
    , lgrSnapshotPolicyArgs
    , lgrTracer
    , lgrQueryBatchSize
    , lgrRegistry
    } = args

  bss = case flavArgs of V2Args bss0 -> bss0

  v2Tracer :: Tracer m V2.FlavorImplSpecificTrace
  v2Tracer = LedgerDBFlavorImplEvent . FlavorImplSpecificTraceV2 >$< lgrTracer

  emptyF ::
    ExtLedgerState blk ValuesMK ->
    m (LedgerSeq' m blk)
  emptyF st =
    empty' st $ case bss of
      InMemoryHandleArgs -> InMemory.newInMemoryLedgerTablesHandle v2Tracer lgrHasFS
      LSMHandleArgs x -> absurd x

  loadSnapshot ::
    CodecConfig blk ->
    SomeHasFS m ->
    DiskSnapshot ->
    m (Either (SnapshotFailure blk) (LedgerSeq' m blk, RealPoint blk))
  loadSnapshot ccfg fs ds = case bss of
    InMemoryHandleArgs -> runExceptT $ InMemory.loadSnapshot v2Tracer lgrRegistry ccfg fs ds
    LSMHandleArgs x -> absurd x

implMkLedgerDb ::
  forall m l blk.
  ( IOLike m
  , HasCallStack
  , IsLedger l
  , l ~ ExtLedgerState blk
  , StandardHash l
  , HasLedgerTables l
  , LedgerSupportsProtocol blk
  , LedgerDbSerialiseConstraints blk
  , HasHardForkHistory blk
  ) =>
  LedgerDBHandle m l blk ->
  HandleArgs ->
  (LedgerDB m l blk, TestInternals m l blk)
implMkLedgerDb h bss =
  ( LedgerDB
      { getVolatileTip = getEnvSTM h implGetVolatileTip
      , getImmutableTip = getEnvSTM h implGetImmutableTip
      , getPastLedgerState = \s -> getEnvSTM h (flip implGetPastLedgerState s)
      , getHeaderStateHistory = getEnvSTM h implGetHeaderStateHistory
      , getForkerAtTarget = newForkerAtTarget h
      , validateFork = getEnv5 h (implValidate h)
      , getPrevApplied = getEnvSTM h implGetPrevApplied
      , garbageCollect = \s -> getEnvSTM h (flip implGarbageCollect s)
      , tryTakeSnapshot = getEnv2 h (implTryTakeSnapshot bss)
      , tryFlush = getEnv h implTryFlush
      , closeDB = implCloseDB h
      }
  , mkInternals bss h
  )

mkInternals ::
  forall m blk.
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  , ApplyBlock (ExtLedgerState blk) blk
  ) =>
  HandleArgs ->
  LedgerDBHandle m (ExtLedgerState blk) blk ->
  TestInternals' m blk
mkInternals bss h =
  TestInternals
    { takeSnapshotNOW = \whereTo suff -> getEnv h $ \env -> do
        let selectWhereTo = case whereTo of
              TakeAtImmutableTip -> anchorHandle
              TakeAtVolatileTip -> currentHandle
        withStateRef env (MkSolo . selectWhereTo) $ \(MkSolo st) ->
          Monad.void $
            takeSnapshot
              (configCodec . getExtLedgerCfg . ledgerDbCfg $ ldbCfg env)
              (LedgerDBSnapshotEvent >$< ldbTracer env)
              (ldbHasFS env)
              suff
              st
    , push = \st -> withRegistry $ \reg -> do
        eFrk <- newForkerAtTarget h reg VolatileTip
        case eFrk of
          Left{} -> error "Unreachable, Volatile tip MUST be in LedgerDB"
          Right frk ->
            forkerPush frk st >> atomically (forkerCommit frk) >> forkerClose frk
    , reapplyThenPushNOW = \blk -> getEnv h $ \env -> withRegistry $ \reg -> do
        eFrk <- newForkerAtTarget h reg VolatileTip
        case eFrk of
          Left{} -> error "Unreachable, Volatile tip MUST be in LedgerDB"
          Right frk -> do
            st <- atomically $ forkerGetLedgerState frk
            tables <- forkerReadTables frk (getBlockKeySets blk)
            let st' =
                  tickThenReapply
                    (ledgerDbCfgComputeLedgerEvents (ldbCfg env))
                    (ledgerDbCfg $ ldbCfg env)
                    blk
                    (st `withLedgerTables` tables)
            forkerPush frk st' >> atomically (forkerCommit frk) >> forkerClose frk
    , wipeLedgerDB = getEnv h $ destroySnapshots . ldbHasFS
    , closeLedgerDB =
        let LDBHandle tvar = h
         in atomically (writeTVar tvar LedgerDBClosed)
    , truncateSnapshots = getEnv h $ implIntTruncateSnapshots . ldbHasFS
    , getNumLedgerTablesHandles = getEnv h $ \env -> do
        l <- readTVarIO (ldbSeq env)
        -- We always have a state at the anchor.
        pure $ 1 + maxRollback l
    }
 where
  takeSnapshot ::
    CodecConfig blk ->
    Tracer m (TraceSnapshotEvent blk) ->
    SomeHasFS m ->
    Maybe String ->
    StateRef m (ExtLedgerState blk) ->
    m (Maybe (DiskSnapshot, RealPoint blk))
  takeSnapshot = case bss of
    InMemoryHandleArgs -> InMemory.takeSnapshot
    LSMHandleArgs x -> absurd x

-- | Testing only! Truncate all snapshots in the DB.
implIntTruncateSnapshots :: MonadThrow m => SomeHasFS m -> m ()
implIntTruncateSnapshots sfs@(SomeHasFS fs) = do
  snapshotsMapM_ sfs (truncateRecursively . (: []))
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

implGetVolatileTip ::
  (MonadSTM m, GetTip l) =>
  LedgerDBEnv m l blk ->
  STM m (l EmptyMK)
implGetVolatileTip = fmap current . readTVar . ldbSeq

implGetImmutableTip ::
  MonadSTM m =>
  LedgerDBEnv m l blk ->
  STM m (l EmptyMK)
implGetImmutableTip = fmap anchor . readTVar . ldbSeq

implGetPastLedgerState ::
  ( MonadSTM m
  , HasHeader blk
  , IsLedger l
  , StandardHash l
  , HeaderHash l ~ HeaderHash blk
  ) =>
  LedgerDBEnv m l blk -> Point blk -> STM m (Maybe (l EmptyMK))
implGetPastLedgerState env point = getPastLedgerAt point <$> readTVar (ldbSeq env)

implGetHeaderStateHistory ::
  ( MonadSTM m
  , l ~ ExtLedgerState blk
  , IsLedger (LedgerState blk)
  , HasHardForkHistory blk
  , HasAnnTip blk
  ) =>
  LedgerDBEnv m l blk -> STM m (HeaderStateHistory blk)
implGetHeaderStateHistory env = do
  ldb <- readTVar (ldbSeq env)
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
    $ getLedgerSeq ldb

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

-- | Remove all points with a slot older than the given slot from the set of
-- previously applied points.
implGarbageCollect :: MonadSTM m => LedgerDBEnv m l blk -> SlotNo -> STM m ()
implGarbageCollect env slotNo =
  modifyTVar (ldbPrevApplied env) $
    Set.dropWhileAntitone ((< slotNo) . realPointSlot)

implTryTakeSnapshot ::
  forall m l blk.
  ( l ~ ExtLedgerState blk
  , IOLike m
  , LedgerSupportsProtocol blk
  , LedgerDbSerialiseConstraints blk
  ) =>
  HandleArgs ->
  LedgerDBEnv m l blk ->
  Maybe (Time, Time) ->
  Word64 ->
  m SnapCounters
implTryTakeSnapshot bss env mTime nrBlocks =
  if onDiskShouldTakeSnapshot (ldbSnapshotPolicy env) (uncurry (flip diffTime) <$> mTime) nrBlocks
    then do
      withStateRef env (MkSolo . anchorHandle) $ \(MkSolo st) ->
        Monad.void $
          takeSnapshot
            (configCodec . getExtLedgerCfg . ledgerDbCfg $ ldbCfg env)
            (LedgerDBSnapshotEvent >$< ldbTracer env)
            (ldbHasFS env)
            st
      Monad.void $
        trimSnapshots
          (LedgerDBSnapshotEvent >$< ldbTracer env)
          (ldbHasFS env)
          (ldbSnapshotPolicy env)
      (`SnapCounters` 0) . Just <$> maybe getMonotonicTime (pure . snd) mTime
    else
      pure $ SnapCounters (fst <$> mTime) nrBlocks
 where
  takeSnapshot ::
    CodecConfig blk ->
    Tracer m (TraceSnapshotEvent blk) ->
    SomeHasFS m ->
    StateRef m (ExtLedgerState blk) ->
    m (Maybe (DiskSnapshot, RealPoint blk))
  takeSnapshot config trcr fs ref = case bss of
    InMemoryHandleArgs ->
      InMemory.takeSnapshot
        config
        trcr
        fs
        Nothing
        ref
    LSMHandleArgs x -> absurd x

-- In the first version of the LedgerDB for UTxO-HD, there is a need to
-- periodically flush the accumulated differences to the disk. However, in the
-- second version there is no need to do so, and because of that, this function
-- does nothing in this case.
implTryFlush :: Applicative m => LedgerDBEnv m l blk -> m ()
implTryFlush _ = pure ()

implCloseDB :: IOLike m => LedgerDBHandle m l blk -> m ()
implCloseDB (LDBHandle varState) =
  atomically $
    readTVar varState >>= \case
      -- Idempotent
      LedgerDBClosed -> pure ()
      LedgerDBOpen env -> do
        writeTVar (ldbForkers env) Map.empty
        writeTVar varState LedgerDBClosed

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
  , ldbHasFS :: !(SomeHasFS m)
  , ldbResolveBlock :: !(ResolveBlock m blk)
  , ldbQueryBatchSize :: !QueryBatchSize
  , ldbOpenHandlesLock :: !(RAWLock m ())
  -- ^ While holding a read lock (at least), all handles in the 'ldbSeq' are
  -- guaranteed to be open. During this time, the handle can be duplicated and
  -- then be used independently, see 'getStateRef' and 'withStateRef'.
  --
  -- Therefore, closing any handles which were previously in 'ldbSeq' requires
  -- acquiring a write lock. Concretely, both of the following approaches are
  -- fine:
  --
  --  * Modify 'ldbSeq' without any locking, and then close the removed handles
  --    while holding a write lock. See e.g. 'closeForkerEnv'.
  --
  --  * Modify 'ldbSeq' while holding a write lock, and then close the removed
  --    handles without any locking.
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

{-------------------------------------------------------------------------------
  Acquiring consistent views
-------------------------------------------------------------------------------}

-- | Get a 'StateRef' from the 'LedgerSeq' in the 'LedgerDBEnv', with the
-- 'LedgerTablesHandle' having been duplicated (such that the original can be
-- closed). The caller is responsible for closing the handle.
--
-- For more flexibility, an arbitrary 'Traversable' of the 'StateRef' can be
-- returned; for the simple use case of getting a single 'StateRef', use @t ~
-- 'Solo'@.
getStateRef ::
  (IOLike m, Traversable t) =>
  LedgerDBEnv m l blk ->
  (LedgerSeq m l -> t (StateRef m l)) ->
  m (t (StateRef m l))
getStateRef ldbEnv project =
  RAWLock.withReadAccess (ldbOpenHandlesLock ldbEnv) $ \() -> do
    tst <- project <$> readTVarIO (ldbSeq ldbEnv)
    for tst $ \st -> do
      tables' <- duplicate $ tables st
      pure st{tables = tables'}

-- | Like 'StateRef', but takes care of closing the handle when the given action
-- returns or errors.
withStateRef ::
  (IOLike m, Traversable t) =>
  LedgerDBEnv m l blk ->
  (LedgerSeq m l -> t (StateRef m l)) ->
  (t (StateRef m l) -> m a) ->
  m a
withStateRef ldbEnv project =
  bracket (getStateRef ldbEnv project) (traverse_ (close . tables))

acquireAtTarget ::
  ( HeaderHash l ~ HeaderHash blk
  , IOLike m
  , GetTip l
  , StandardHash l
  , LedgerSupportsProtocol blk
  ) =>
  LedgerDBEnv m l blk ->
  Either Word64 (Target (Point blk)) ->
  m (Either GetForkerError (StateRef m l))
acquireAtTarget ldbEnv target =
  getStateRef ldbEnv $ \l -> case target of
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

newForkerAtTarget ::
  ( HeaderHash l ~ HeaderHash blk
  , IOLike m
  , IsLedger l
  , HasLedgerTables l
  , LedgerSupportsProtocol blk
  , StandardHash l
  ) =>
  LedgerDBHandle m l blk ->
  ResourceRegistry m ->
  Target (Point blk) ->
  m (Either GetForkerError (Forker m l blk))
newForkerAtTarget h rr pt = getEnv h $ \ldbEnv ->
  acquireAtTarget ldbEnv (Right pt) >>= traverse (newForker h ldbEnv rr)

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
  Word64 ->
  m (Either GetForkerError (Forker m l blk))
newForkerByRollback h rr n = getEnv h $ \ldbEnv ->
  acquireAtTarget ldbEnv (Left n) >>= traverse (newForker h ldbEnv rr)

closeForkerEnv ::
  IOLike m => ForkerEnv m l blk -> m ()
closeForkerEnv ForkerEnv{foeResourcesToRelease = (lock, key, toRelease)} =
  RAWLock.withWriteAccess lock $
    const $ do
      id =<< atomically (swapTVar toRelease (pure ()))
      _ <- release key
      pure ((), ())

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
          readTVar (ldbForkers env)
            >>= ( Map.lookup forkerKey >>> \case
                    Nothing -> throwSTM $ ClosedForkerError @blk forkerKey prettyCallStack
                    Just forkerEnv -> pure forkerEnv
                )
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

-- | Will release all handles in the 'foeLedgerSeq'.
--
-- This function receives an environment instead of reading it from
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
implForkerClose (LDBHandle varState) forkerKey forkerEnv = do
  atomically $
    readTVar varState >>= \case
      LedgerDBClosed -> pure ()
      LedgerDBOpen ldbEnv -> do
        modifyTVar
          (ldbForkers ldbEnv)
          (Map.delete forkerKey)

  closeForkerEnv forkerEnv

newForker ::
  ( IOLike m
  , HasLedgerTables l
  , LedgerSupportsProtocol blk
  , NoThunks (l EmptyMK)
  , GetTip l
  , StandardHash l
  ) =>
  LedgerDBHandle m l blk ->
  LedgerDBEnv m l blk ->
  ResourceRegistry m ->
  StateRef m l ->
  m (Forker m l blk)
newForker h ldbEnv rr st = do
  forkerKey <- atomically $ stateTVar (ldbNextForkerKey ldbEnv) $ \r -> (r, r + 1)
  let tr = LedgerDBForkerEvent . TraceForkerEventWithKey forkerKey >$< ldbTracer ldbEnv
  traceWith tr ForkerOpen
  lseqVar <- newTVarIO . LedgerSeq . AS.Empty $ st
  (k, toRelease) <- allocate rr (\_ -> newTVarIO (pure ())) (readTVarIO Monad.>=> id)
  let forkerEnv =
        ForkerEnv
          { foeLedgerSeq = lseqVar
          , foeSwitchVar = ldbSeq ldbEnv
          , foeSecurityParam = ledgerDbCfgSecParam $ ldbCfg ldbEnv
          , foeTracer = tr
          , foeResourcesToRelease = (ldbOpenHandlesLock ldbEnv, k, toRelease)
          }
  atomically $ modifyTVar (ldbForkers ldbEnv) $ Map.insert forkerKey forkerEnv
  pure $
    Forker
      { forkerReadTables = getForkerEnv1 h forkerKey implForkerReadTables
      , forkerRangeReadTables =
          getForkerEnv1 h forkerKey (implForkerRangeReadTables (ldbQueryBatchSize ldbEnv))
      , forkerGetLedgerState = getForkerEnvSTM h forkerKey implForkerGetLedgerState
      , forkerReadStatistics = getForkerEnv h forkerKey implForkerReadStatistics
      , forkerPush = getForkerEnv1 h forkerKey implForkerPush
      , forkerCommit = getForkerEnvSTM h forkerKey implForkerCommit
      , forkerClose = implForkerClose h forkerKey forkerEnv
      }
