{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.Init (mkInitDb) where

import           Control.Monad (void)
import qualified Control.RAWLock as RAWLock
import           Control.ResourceRegistry
import           Control.Tracer
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
import           Ouroboros.Consensus.Storage.ChainDB.Impl.BlockCache
import           Ouroboros.Consensus.Storage.LedgerDB.API
import           Ouroboros.Consensus.Storage.LedgerDB.API.Config
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Args
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Common
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Init
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots
import qualified Ouroboros.Consensus.Storage.LedgerDB.Impl.Validate as Validate
import           Ouroboros.Consensus.Storage.LedgerDB.V2.Args as V2
import           Ouroboros.Consensus.Storage.LedgerDB.V2.Common
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory as InMemory
import           Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.CallStack
import           Ouroboros.Consensus.Util.IOLike
import qualified Ouroboros.Network.AnchoredSeq as AS
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
import           System.FS.API

mkInitDb :: forall m blk.
            ( LedgerSupportsProtocol blk
            , IOLike m
            , LedgerDbSerialiseConstraints blk
            , HasHardForkHistory blk
#if __GLASGOW_HASKELL__ < 906
            , HasAnnTip blk
#endif
            )
         => Complete LedgerDbArgs m blk
         -> Complete V2.LedgerDbFlavorArgs m
         -> Validate.ResolveBlock m blk
         -> InitDB (LedgerSeq' m blk) m blk
mkInitDb args flavArgs getBlock =
  InitDB {
      initFromGenesis = emptyF =<< lgrGenesis
    , initFromSnapshot = loadSnapshot (configCodec . getExtLedgerCfg . ledgerDbCfg $ lgrConfig) lgrHasFS
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
               , ldbQueryBatchSize  = Nothing
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
     , lgrRegistry
     } = args

   bss = case flavArgs of V2Args bss0 -> bss0

   emptyF :: ExtLedgerState blk ValuesMK
          -> m (LedgerSeq' m blk)
   emptyF st =
     empty' st $ case bss of
                  InMemoryHandleArgs -> InMemory.newInMemoryLedgerTablesHandle lgrHasFS
                  --TODO LSMHandleArgs      -> LSM.newLSMLedgerTablesHandle

   loadSnapshot :: CodecConfig blk
                -> SomeHasFS m
                -> DiskSnapshot
                -> m (Either (SnapshotFailure blk) (LedgerSeq' m blk, RealPoint blk))
   loadSnapshot = case bss of
     InMemoryHandleArgs -> InMemory.loadSnapshot lgrRegistry
     --TODO LSMHandleArgs      -> LSM.loadSnapshot

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
    , validate                  = getEnv5    h (implValidate h)
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
            TakeAtVolatileTip -> anchorHandle
            TakeAtImmutableTip -> currentHandle) <$> readTVarIO (ldbSeq env)
          void $ takeSnapshot
                (configCodec . getExtLedgerCfg . ledgerDbCfg $ ldbCfg env)
                (LedgerDBSnapshotEvent >$< ldbTracer env)
                (ldbHasFS env)
                suff
                st
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
                  -> StateRef m (ExtLedgerState blk)
                  -> m (Maybe (DiskSnapshot, RealPoint blk))
     takeSnapshot = case bss of
       InMemoryHandleArgs -> InMemory.takeSnapshot
       --TODO LSMHandleArgs      -> LSM.takeSnapshot

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
      void . takeSnapshot
                (configCodec . getExtLedgerCfg . ledgerDbCfg $ ldbCfg env)
                (LedgerDBSnapshotEvent >$< ldbTracer env)
                (ldbHasFS env)
                . anchorHandle
                =<< readTVarIO (ldbSeq env)
      void $ trimSnapshots
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
       InMemoryHandleArgs -> InMemory.takeSnapshot config trcr fs Nothing ref
       --TODO LSMHandleArgs      -> LSM.takeSnapshot config trcr fs Nothing ref

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
