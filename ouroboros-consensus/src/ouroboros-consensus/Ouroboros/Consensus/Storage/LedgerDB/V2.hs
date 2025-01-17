{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2 (
    LedgerSupportsV2LedgerDB
  , mkInitDb
  ) where

import           Control.Arrow ((>>>))
import qualified Control.Monad as Monad (void, (>=>))
import           Control.Monad.Except
import           Control.RAWLock
import qualified Control.RAWLock as RAWLock
import           Control.ResourceRegistry
import           Control.Tracer
import qualified Data.Foldable as Foldable
import           Data.Functor.Contravariant ((>$<))
import           Data.Kind (Type)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Void
import           Data.Word
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.HeaderStateHistory
                     (HeaderStateHistory (..), mkHeaderStateWithTimeFromSummary)
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import           Ouroboros.Consensus.Storage.LedgerDB.API
import           Ouroboros.Consensus.Storage.LedgerDB.Args
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import           Ouroboros.Consensus.Storage.LedgerDB.TraceEvent
import           Ouroboros.Consensus.Storage.LedgerDB.V2.Args as V2
import           Ouroboros.Consensus.Storage.LedgerDB.V2.Forker
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory as InMemory
import           Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.NormalForm.StrictTVar ()
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
import           Prelude hiding (read)
import           System.FS.API

type LedgerSupportsV2LedgerDB blk =
  (InMemory.CanUpgradeLedgerTables (LedgerState blk))

mkInitDb :: forall m blk.
            ( LedgerSupportsProtocol blk
            , IOLike m
            , LedgerDbSerialiseConstraints blk
            , HasHardForkHistory blk
            , InMemory.CanUpgradeLedgerTables (LedgerState blk)
            )
         => Complete LedgerDbArgs m blk
         -> Complete V2.LedgerDbFlavorArgs m
         -> ResolveBlock m blk
         -> InitDB (LedgerSeq' m blk) m blk
mkInitDb args flavArgs getBlock =
  InitDB {
      initFromGenesis = emptyF =<< lgrGenesis
    , initFromSnapshot =
        loadSnapshot (configCodec . getExtLedgerCfg . ledgerDbCfg $ lgrConfig) lgrHasFS
    , closeDb = closeLedgerSeq
    , initReapplyBlock = \a b c -> do
        (x, y) <- reapplyThenPush lgrRegistry a b c
        closeLedgerSeq x
        pure y
    , currentTip = ledgerState . current
    , pruneDb = \lseq -> do
        let (LedgerSeq rel, dbPrunedToImmDBTip) = pruneToImmTipOnly lseq
        mapM_ (close . tables) (AS.toOldestFirst rel)
        pure dbPrunedToImmDBTip
    , mkLedgerDb = \lseq -> do
        varDB <- newTVarIO lseq
        prevApplied <- newTVarIO Set.empty
        forkers <- newTVarIO Map.empty
        nextForkerKey <- newTVarIO (ForkerKey 0)
        lock <- RAWLock.new LDBLock
        let env = LedgerDBEnv {
                 ldbSeq             = varDB
               , ldbPrevApplied     = prevApplied
               , ldbForkers         = forkers
               , ldbNextForkerKey   = nextForkerKey
               , ldbSnapshotPolicy  = defaultSnapshotPolicy (ledgerDbCfgSecParam lgrConfig) lgrSnapshotPolicyArgs
               , ldbTracer          = lgrTracer
               , ldbCfg             = lgrConfig
               , ldbHasFS           = lgrHasFS
               , ldbResolveBlock    = getBlock
               , ldbQueryBatchSize  = lgrQueryBatchSize
               , ldbOpenHandlesLock = lock
               }
        h <- LDBHandle <$> newTVarIO (LedgerDBOpen env)
        pure $ implMkLedgerDb h bss
    }
 where
   LedgerDbArgs {
       lgrConfig
     , lgrGenesis
     , lgrHasFS
     , lgrSnapshotPolicyArgs
     , lgrTracer
     , lgrQueryBatchSize
     , lgrRegistry
     } = args

   bss = case flavArgs of V2Args bss0 -> bss0

   emptyF :: ExtLedgerState blk ValuesMK
          -> m (LedgerSeq' m blk)
   emptyF st =
     empty' st $ case bss of
                  InMemoryHandleArgs -> InMemory.newInMemoryLedgerTablesHandle lgrHasFS
                  LSMHandleArgs  x   -> absurd x

   loadSnapshot :: CodecConfig blk
                -> SomeHasFS m
                -> Flag "DoDiskSnapshotChecksum"
                -> DiskSnapshot
                -> m (Either (SnapshotFailure blk) (LedgerSeq' m blk, RealPoint blk))
   loadSnapshot ccfg fs f ds = case bss of
     InMemoryHandleArgs -> runExceptT $ InMemory.loadSnapshot lgrRegistry ccfg fs f ds
     LSMHandleArgs x    -> absurd x

implMkLedgerDb ::
     forall m l blk.
     ( IOLike m
     , HasCallStack
     , IsLedger l
     , l ~ ExtLedgerState blk
     , StandardHash l, HasLedgerTables l
#if __GLASGOW_HASKELL__ < 908
     , HeaderHash l ~ HeaderHash blk
#endif
     , LedgerSupportsProtocol blk
     , LedgerDbSerialiseConstraints blk
     , HasHardForkHistory blk
     )
  => LedgerDBHandle m l blk
  -> HandleArgs
  -> (LedgerDB m l blk, TestInternals m l blk)
implMkLedgerDb h bss = (LedgerDB {
      getVolatileTip            = getEnvSTM  h implGetVolatileTip
    , getImmutableTip           = getEnvSTM  h implGetImmutableTip
    , getPastLedgerState        = \s -> getEnvSTM  h (flip implGetPastLedgerState s)
    , getHeaderStateHistory     = getEnvSTM  h implGetHeaderStateHistory
    , getForkerAtTarget         = newForkerAtTarget h
    , validateFork              = getEnv5    h (implValidate h)
    , getPrevApplied            = getEnvSTM  h implGetPrevApplied
    , garbageCollect            = \s -> getEnvSTM  h (flip implGarbageCollect s)
    , tryTakeSnapshot           = getEnv2    h (implTryTakeSnapshot bss)
    , tryFlush                  = getEnv     h implTryFlush
    , closeDB                   = implCloseDB h
    }, mkInternals bss h)

mkInternals ::
     forall m blk.
     ( IOLike m
     , LedgerDbSerialiseConstraints blk
     , LedgerSupportsProtocol blk
     , ApplyBlock (ExtLedgerState blk) blk
     )
  => HandleArgs
  -> LedgerDBHandle m (ExtLedgerState blk) blk
  -> TestInternals' m blk
mkInternals bss h = TestInternals {
      takeSnapshotNOW = \whereTo suff -> getEnv h $ \env -> do
          st <- (case whereTo of
            TakeAtVolatileTip  -> anchorHandle
            TakeAtImmutableTip -> currentHandle) <$> readTVarIO (ldbSeq env)
          Monad.void $ takeSnapshot
                (configCodec . getExtLedgerCfg . ledgerDbCfg $ ldbCfg env)
                (LedgerDBSnapshotEvent >$< ldbTracer env)
                (ldbHasFS env)
                suff
                (onDiskShouldChecksumSnapshots $ ldbSnapshotPolicy env)
                st
    , push = \st -> withRegistry $ \reg -> do
          eFrk <- newForkerAtTarget h reg VolatileTip
          case eFrk of
            Left {} -> error "Unreachable, Volatile tip MUST be in LedgerDB"
            Right frk ->
              forkerPush frk st >> atomically (forkerCommit frk) >> forkerClose frk
    , reapplyThenPushNOW = \blk -> getEnv h $ \env -> withRegistry $ \reg -> do
          eFrk <- newForkerAtTarget h reg VolatileTip
          case eFrk of
            Left {} -> error "Unreachable, Volatile tip MUST be in LedgerDB"
            Right frk -> do
              st <- atomically $ forkerGetLedgerState frk
              tables <- forkerReadTables frk (getBlockKeySets blk)
              let st' = tickThenReapply (ledgerDbCfg $ ldbCfg env) blk (st `withLedgerTables` tables)
              forkerPush frk st' >> atomically (forkerCommit frk) >> forkerClose frk
    , wipeLedgerDB = getEnv h $ destroySnapshots . ldbHasFS
    , closeLedgerDB =
       let LDBHandle tvar = h in
         atomically (writeTVar tvar LedgerDBClosed)
    , truncateSnapshots = getEnv h $ implIntTruncateSnapshots . ldbHasFS
    }
  where
     takeSnapshot :: CodecConfig blk
                  -> Tracer m (TraceSnapshotEvent blk)
                  -> SomeHasFS m
                  -> Maybe String
                  -> Flag "DoDiskSnapshotChecksum"
                  -> StateRef m (ExtLedgerState blk)
                  -> m (Maybe (DiskSnapshot, RealPoint blk))
     takeSnapshot = case bss of
       InMemoryHandleArgs -> InMemory.takeSnapshot
       LSMHandleArgs x    -> absurd x

-- | Testing only! Truncate all snapshots in the DB.
implIntTruncateSnapshots :: MonadThrow m => SomeHasFS m -> m ()
implIntTruncateSnapshots sfs@(SomeHasFS fs) = do
  snapshotsMapM_ sfs (truncateRecursively . (:[]))
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

implGetVolatileTip ::
     (MonadSTM m, GetTip l)
  => LedgerDBEnv m l blk
  -> STM m (l EmptyMK)
implGetVolatileTip = fmap current . readTVar . ldbSeq

implGetImmutableTip ::
     MonadSTM m
  => LedgerDBEnv m l blk
  -> STM m (l EmptyMK)
implGetImmutableTip = fmap anchor . readTVar . ldbSeq

implGetPastLedgerState ::
     ( MonadSTM m , HasHeader blk, IsLedger l, StandardHash l
     , HeaderHash l ~ HeaderHash blk )
  => LedgerDBEnv m l blk -> Point blk -> STM m (Maybe (l EmptyMK))
implGetPastLedgerState env point = getPastLedgerAt point <$> readTVar (ldbSeq env)

implGetHeaderStateHistory ::
     ( MonadSTM m
     , l ~ ExtLedgerState blk
     , IsLedger (LedgerState blk)
     , HasHardForkHistory blk
     , HasAnnTip blk
     )
  => LedgerDBEnv m l blk -> STM m (HeaderStateHistory blk)
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
     forall m l blk.
     ( l ~ ExtLedgerState blk
     , IOLike m
     , LedgerSupportsProtocol blk
     , LedgerDbSerialiseConstraints blk
     )
  => HandleArgs
  -> LedgerDBEnv m l blk
  -> Maybe (Time, Time)
  -> Word64
  -> m SnapCounters
implTryTakeSnapshot bss env mTime nrBlocks =
    if onDiskShouldTakeSnapshot (ldbSnapshotPolicy env) (uncurry (flip diffTime) <$> mTime) nrBlocks then do
      Monad.void . takeSnapshot
                (configCodec . getExtLedgerCfg . ledgerDbCfg $ ldbCfg env)
                (LedgerDBSnapshotEvent >$< ldbTracer env)
                (ldbHasFS env)
                . anchorHandle
                =<< readTVarIO (ldbSeq env)
      Monad.void $ trimSnapshots
                (LedgerDBSnapshotEvent >$< ldbTracer env)
                (ldbHasFS env)
                (ldbSnapshotPolicy env)
      (`SnapCounters` 0) . Just <$> maybe getMonotonicTime (pure . snd) mTime
    else
      pure $ SnapCounters (fst <$> mTime) nrBlocks
  where
     takeSnapshot :: CodecConfig blk
                  -> Tracer m (TraceSnapshotEvent blk)
                  -> SomeHasFS m
                  -> StateRef m (ExtLedgerState blk)
                  -> m (Maybe (DiskSnapshot, RealPoint blk))
     takeSnapshot config trcr fs ref = case bss of
       InMemoryHandleArgs ->
           InMemory.takeSnapshot
             config
             trcr
             fs
             Nothing
             (onDiskShouldChecksumSnapshots $ ldbSnapshotPolicy env)
             ref
       LSMHandleArgs x    -> absurd x

-- In the first version of the LedgerDB for UTxO-HD, there is a need to
-- periodically flush the accumulated differences to the disk. However, in the
-- second version there is no need to do so, and because of that, this function
-- does nothing in this case.
implTryFlush :: Applicative m => LedgerDBEnv m l blk -> m ()
implTryFlush _ = pure ()

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

{-------------------------------------------------------------------------------
  The LedgerDBEnv
-------------------------------------------------------------------------------}

data LDBLock = LDBLock deriving (Generic, NoThunks)

type LedgerDBEnv :: (Type -> Type) -> LedgerStateKind -> Type -> Type
data LedgerDBEnv m l blk = LedgerDBEnv {
    -- | INVARIANT: the tip of the 'LedgerDB' is always in sync with the tip of
    -- the current chain of the ChainDB.
    ldbSeq             :: !(StrictTVar m (LedgerSeq m l))
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
  , ldbPrevApplied     :: !(StrictTVar m (Set (RealPoint blk)))
    -- | Open forkers.
    --
    -- INVARIANT: a forker is open iff its 'ForkerKey' is in this 'Map.
  , ldbForkers         :: !(StrictTVar m (Map ForkerKey (ForkerEnv m l blk)))
  , ldbNextForkerKey   :: !(StrictTVar m ForkerKey)

  , ldbSnapshotPolicy  :: !SnapshotPolicy
  , ldbTracer          :: !(Tracer m (TraceEvent blk))
  , ldbCfg             :: !(LedgerDbCfg l)
  , ldbHasFS           :: !(SomeHasFS m)
  , ldbResolveBlock    :: !(ResolveBlock m blk)
  , ldbQueryBatchSize  :: !QueryBatchSize
  , ldbOpenHandlesLock :: !(RAWLock m LDBLock)
  } deriving (Generic)

deriving instance ( IOLike m
                  , LedgerSupportsProtocol blk
                  , NoThunks (l EmptyMK)
                  , NoThunks (TxIn l)
                  , NoThunks (TxOut l)
                  , NoThunks (LedgerCfg l)
                  ) => NoThunks (LedgerDBEnv m l blk)

{-------------------------------------------------------------------------------
  The LedgerDBHandle
-------------------------------------------------------------------------------}

type LedgerDBHandle :: (Type -> Type) -> LedgerStateKind -> Type -> Type
newtype LedgerDBHandle m l blk =
    LDBHandle (StrictTVar m (LedgerDBState m l blk))
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

{-------------------------------------------------------------------------------
  Acquiring consistent views
-------------------------------------------------------------------------------}

-- | This function must hold the 'LDBLock' such that handles are not released
-- before they are duplicated.
acquireAtTarget ::
     ( HeaderHash l ~ HeaderHash blk
     , IOLike m
     , GetTip l
     , StandardHash l
     , LedgerSupportsProtocol blk
     )
  => LedgerDBEnv m l blk
  -> Either Word64 (Target (Point blk))
  -> LDBLock
  -> m (Either GetForkerError (StateRef m l))
acquireAtTarget ldbEnv (Right VolatileTip) _ = do
  l <- readTVarIO (ldbSeq ldbEnv)
  let StateRef st tbs = currentHandle l
  t <- duplicate tbs
  pure $ Right $ StateRef st t
acquireAtTarget ldbEnv (Right ImmutableTip) _ = do
  l <- readTVarIO (ldbSeq ldbEnv)
  let StateRef st tbs = anchorHandle l
  t <- duplicate tbs
  pure $ Right $ StateRef st t
acquireAtTarget ldbEnv (Right (SpecificPoint pt)) _ = do
  dblog <- readTVarIO (ldbSeq ldbEnv)
  let immTip = getTip $ anchor dblog
  case currentHandle <$> rollback pt dblog of
    Nothing | pointSlot pt < pointSlot immTip -> pure $ Left $ PointTooOld Nothing
            | otherwise   -> pure $ Left PointNotOnChain
    Just (StateRef st tbs) ->
          Right . StateRef st <$> duplicate tbs
acquireAtTarget ldbEnv (Left n) _ = do
      dblog <- readTVarIO (ldbSeq ldbEnv)
      case currentHandle <$> rollbackN n dblog of
        Nothing ->
          return $ Left $ PointTooOld $ Just $ ExceededRollback {
              rollbackMaximum   = maxRollback dblog
            , rollbackRequested = n
            }
        Just (StateRef st tbs) ->
              Right . StateRef st <$> duplicate tbs

newForkerAtTarget ::
     ( HeaderHash l ~ HeaderHash blk
     , IOLike m
     , IsLedger l
     , HasLedgerTables l
     , LedgerSupportsProtocol blk
     , StandardHash l
     )
  => LedgerDBHandle m l blk
  -> ResourceRegistry m
  -> Target (Point blk)
  -> m (Either GetForkerError (Forker m l blk))
newForkerAtTarget h rr pt = getEnv h $ \ldbEnv@LedgerDBEnv{ldbOpenHandlesLock = lock} ->
    RAWLock.withReadAccess lock (acquireAtTarget ldbEnv (Right pt)) >>= traverse (newForker h ldbEnv rr)

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
  -> Word64
  -> m (Either GetForkerError (Forker m l blk))
newForkerByRollback h rr n = getEnv h $ \ldbEnv@LedgerDBEnv{ldbOpenHandlesLock = lock} -> do
    RAWLock.withReadAccess lock (acquireAtTarget ldbEnv (Left n)) >>= traverse (newForker h ldbEnv rr)

-- | Close all open 'Forker's.
closeAllForkers ::
     IOLike m
  => LedgerDBEnv m l blk
  -> m ()
closeAllForkers ldbEnv = do
    toClose <- fmap (ldbEnv,) <$> (atomically $ stateTVar forkersVar (, Map.empty))
    mapM_ closeForkerEnv toClose
  where
    forkersVar = ldbForkers ldbEnv

closeForkerEnv :: IOLike m => (LedgerDBEnv m l blk, ForkerEnv m l blk) -> m ()
closeForkerEnv (LedgerDBEnv{ldbOpenHandlesLock}, frkEnv) =
  RAWLock.withWriteAccess ldbOpenHandlesLock $
    const $ do
      id =<< readTVarIO (foeResourcesToRelease frkEnv)
      atomically $ writeTVar (foeResourcesToRelease frkEnv) (pure ())
      pure ((), LDBLock)

getForkerEnv ::
     forall m l blk r. (IOLike m, HasCallStack, HasHeader blk)
  => LedgerDBHandle m l blk
  -> ForkerKey
  -> (ForkerEnv m l blk -> m r)
  -> m r
getForkerEnv (LDBHandle varState) forkerKey f = do
    forkerEnv <- atomically $ readTVar varState >>= \case
      LedgerDBClosed   -> throwIO $ ClosedDBError @blk prettyCallStack
      LedgerDBOpen env -> readTVar (ldbForkers env) >>= (Map.lookup forkerKey >>> \case
        Nothing        -> throwSTM $ ClosedForkerError @blk forkerKey prettyCallStack
        Just forkerEnv -> pure forkerEnv)
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

-- | Will release all handles in the 'foeLedgerSeq'.
implForkerClose ::
     IOLike m
  => LedgerDBHandle m l blk
  -> ForkerKey
  -> m ()
implForkerClose (LDBHandle varState) forkerKey = do
    menv <- atomically $ readTVar varState >>= \case
      LedgerDBClosed      -> pure Nothing
      LedgerDBOpen ldbEnv -> fmap (ldbEnv,) <$>
        stateTVar
            (ldbForkers ldbEnv)
            (Map.updateLookupWithKey (\_ _ -> Nothing) forkerKey)
    whenJust menv closeForkerEnv

newForker ::
     ( IOLike m
     , HasLedgerTables l
     , LedgerSupportsProtocol blk
     , NoThunks (l EmptyMK)
     , GetTip l
     , StandardHash l
     )
  => LedgerDBHandle m l blk
  -> LedgerDBEnv m l blk
  -> ResourceRegistry m
  -> StateRef m l
  -> m (Forker m l blk)
newForker h ldbEnv rr st = do
    forkerKey <- atomically $ stateTVar (ldbNextForkerKey ldbEnv) $ \r -> (r, r + 1)
    let tr = LedgerDBForkerEvent . TraceForkerEventWithKey forkerKey >$< ldbTracer ldbEnv
    traceWith tr ForkerOpen
    lseqVar   <- newTVarIO . LedgerSeq . AS.Empty $ st
    (_, toRelease) <- allocate rr (\_ -> newTVarIO (pure ())) (readTVarIO Monad.>=> id)
    let forkerEnv = ForkerEnv {
        foeLedgerSeq          = lseqVar
      , foeSwitchVar          = ldbSeq ldbEnv
      , foeSecurityParam      = ledgerDbCfgSecParam $ ldbCfg ldbEnv
      , foeTracer             = tr
      , foeResourcesToRelease = toRelease
      }
    atomically $ modifyTVar (ldbForkers ldbEnv) $ Map.insert forkerKey forkerEnv
    pure $ Forker {
        forkerReadTables             = getForkerEnv1   h forkerKey implForkerReadTables
      , forkerRangeReadTables        = getForkerEnv1   h forkerKey (implForkerRangeReadTables (ldbQueryBatchSize ldbEnv))
      , forkerGetLedgerState         = getForkerEnvSTM h forkerKey implForkerGetLedgerState
      , forkerReadStatistics         = getForkerEnv    h forkerKey implForkerReadStatistics
      , forkerPush                   = getForkerEnv1   h forkerKey implForkerPush
      , forkerCommit                 = getForkerEnvSTM h forkerKey implForkerCommit
      , forkerClose                  = implForkerClose h forkerKey
      }
