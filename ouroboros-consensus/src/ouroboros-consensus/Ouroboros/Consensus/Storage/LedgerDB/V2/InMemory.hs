{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory
  ( mkInMemoryInitDb

    -- * Snapshots
  , loadSnapshot
  , snapshotManager

    -- * snapshot-converter
  , implTakeSnapshot
  ) where

import Cardano.Binary as CBOR
import qualified Codec.CBOR.Write as CBOR
import Codec.Serialise (decode)
import qualified Control.Monad as Monad
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import qualified Control.RAWLock as RAWLock
import Control.ResourceRegistry
import Control.Tracer
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Identity
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.String (fromString)
import GHC.Generics
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Args
import Ouroboros.Consensus.Storage.LedgerDB.Backends
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.TraceEvent
import Ouroboros.Consensus.Storage.LedgerDB.V2
import Ouroboros.Consensus.Storage.LedgerDB.V2.Forker
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.CBOR (readIncremental)
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.NormalForm.StrictTVar ()
import System.FS.API
import System.FS.CRC
import Prelude hiding (read)

{-------------------------------------------------------------------------------
  InMemory implementation of LedgerTablesHandles
-------------------------------------------------------------------------------}

data LedgerTablesHandleState l
  = LedgerTablesHandleOpen !(LedgerTables l ValuesMK)
  | LedgerTablesHandleClosed
  deriving Generic

deriving instance NoThunks (LedgerTables l ValuesMK) => NoThunks (LedgerTablesHandleState l)

data InMemoryClosedExn = InMemoryClosedExn
  deriving (Show, Exception)

guardClosed :: LedgerTablesHandleState l -> (LedgerTables l ValuesMK -> a) -> a
guardClosed LedgerTablesHandleClosed _ = error $ show InMemoryClosedExn
guardClosed (LedgerTablesHandleOpen st) f = f st

data InMem

newInMemoryLedgerTablesHandle ::
  forall m l.
  ( IOLike m
  , HasLedgerTables l
  , CanUpgradeLedgerTables l
  , SerializeTablesWithHint l
  ) =>
  Tracer m (Trace m InMem) ->
  SomeHasFS m ->
  LedgerTables l ValuesMK ->
  m (LedgerTablesHandle m l)
newInMemoryLedgerTablesHandle tracer someFS@(SomeHasFS hasFS) l = do
  !tv <- newTVarIO (LedgerTablesHandleOpen l)
  traceWith tracer TraceLedgerTablesHandleCreate
  pure
    LedgerTablesHandle
      { close = do
          p <- atomically $ swapTVar tv LedgerTablesHandleClosed
          case p of
            LedgerTablesHandleOpen{} -> traceWith tracer TraceLedgerTablesHandleClose
            _ -> pure ()
      , duplicate = do
          hs <- readTVarIO tv
          !x <- guardClosed hs $ newInMemoryLedgerTablesHandle tracer someFS
          pure x
      , read = \_ keys -> do
          hs <- readTVarIO tv
          guardClosed
            hs
            (pure . flip (ltliftA2 (\(ValuesMK v) (KeysMK k) -> ValuesMK $ v `Map.restrictKeys` k)) keys)
      , readRange = \_ (f, t) -> do
          hs <- readTVarIO tv
          guardClosed
            hs
            ( \(LedgerTables (ValuesMK m)) ->
                let m' = Map.take t . (maybe id (\g -> snd . Map.split g) f) $ m
                 in pure (LedgerTables (ValuesMK m'), fst <$> Map.lookupMax m')
            )
      , readAll = \_ -> do
          hs <- readTVarIO tv
          guardClosed hs pure
      , pushDiffs = \st0 !diffs ->
          atomically $
            modifyTVar
              tv
              ( \r ->
                  guardClosed
                    r
                    ( LedgerTablesHandleOpen
                        . flip
                          (ltliftA2 (\(ValuesMK vals) (DiffMK d) -> ValuesMK (Diff.applyDiff vals d)))
                          (projectLedgerTables diffs)
                        . upgradeTables st0 diffs
                    )
              )
      , takeHandleSnapshot = \hint snapshotName -> do
          createDirectoryIfMissing hasFS True $ mkFsPath [snapshotName, "tables"]
          h <- readTVarIO tv
          guardClosed h $
            \values ->
              withFile hasFS (mkFsPath [snapshotName, "tables", "tvar"]) (WriteMode MustBeNew) $ \hf ->
                fmap (Just . snd) $
                  hPutAllCRC hasFS hf $
                    CBOR.toLazyByteString $
                      valuesMKEncoder hint values
      , tablesSize = do
          hs <- readTVarIO tv
          guardClosed hs (pure . Just . Map.size . getValuesMK . getLedgerTables)
      }

{-------------------------------------------------------------------------------
  Snapshots
-------------------------------------------------------------------------------}

snapshotManager ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  ) =>
  Complete LedgerDbArgs m blk ->
  SnapshotManager m m blk (StateRef m (ExtLedgerState blk))
snapshotManager args =
  snapshotManager'
    (configCodec . getExtLedgerCfg . ledgerDbCfg $ lgrConfig args)
    (LedgerDBSnapshotEvent >$< lgrTracer args)
    (lgrHasFS args)

snapshotManager' ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  ) =>
  CodecConfig blk ->
  Tracer m (TraceSnapshotEvent blk) ->
  SomeHasFS m ->
  SnapshotManager m m blk (StateRef m (ExtLedgerState blk))
snapshotManager' ccfg tracer fs =
  SnapshotManager
    { listSnapshots = defaultListSnapshots fs
    , deleteSnapshot = defaultDeleteSnapshot fs tracer
    , takeSnapshot = implTakeSnapshot ccfg tracer fs
    }

writeSnapshot ::
  MonadThrow m =>
  SomeHasFS m ->
  (ExtLedgerState blk EmptyMK -> Encoding) ->
  DiskSnapshot ->
  StateRef m (ExtLedgerState blk) ->
  m ()
writeSnapshot fs@(SomeHasFS hasFs) encLedger ds st = do
  createDirectoryIfMissing hasFs True $ snapshotToDirPath ds
  crc1 <- writeExtLedgerState fs encLedger (snapshotToStatePath ds) $ state st
  crc2 <- takeHandleSnapshot (tables st) (state st) $ snapshotToDirName ds
  writeSnapshotMetadata fs ds $
    SnapshotMetadata
      { snapshotBackend = UTxOHDMemSnapshot
      , snapshotChecksum = maybe crc1 (crcOfConcat crc1) crc2
      , snapshotTablesCodecVersion = TablesCodecVersion1
      }

implTakeSnapshot ::
  ( IOLike m
  , LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  ) =>
  CodecConfig blk ->
  Tracer m (TraceSnapshotEvent blk) ->
  SomeHasFS m ->
  Maybe String ->
  StateRef m (ExtLedgerState blk) ->
  m (Maybe (DiskSnapshot, RealPoint blk))
implTakeSnapshot ccfg tracer hasFS suffix st = do
  case pointToWithOriginRealPoint (castPoint (getTip $ state st)) of
    Origin -> return Nothing
    NotOrigin t -> do
      let number = unSlotNo (realPointSlot t)
          snapshot = DiskSnapshot number suffix
      diskSnapshots <- defaultListSnapshots hasFS
      if List.any (== DiskSnapshot number suffix) diskSnapshots
        then
          return Nothing
        else do
          encloseTimedWith (TookSnapshot snapshot t >$< tracer) $
            writeSnapshot hasFS (encodeDiskExtLedgerState ccfg) snapshot st
          return $ Just (snapshot, t)

-- | Read snapshot from disk.
--
--   Fail on data corruption, i.e. when the checksum of the read data differs
--   from the one tracked by @'DiskSnapshot'@.
loadSnapshot ::
  forall blk m.
  ( LedgerDbSerialiseConstraints blk
  , LedgerSupportsProtocol blk
  , IOLike m
  , LedgerSupportsInMemoryLedgerDB (LedgerState blk)
  ) =>
  Tracer m (Trace m InMem) ->
  ResourceRegistry m ->
  CodecConfig blk ->
  SomeHasFS m ->
  DiskSnapshot ->
  ExceptT (SnapshotFailure blk) m (LedgerSeq' m blk, RealPoint blk)
loadSnapshot tracer _rr ccfg fs ds = do
  snapshotMeta <-
    withExceptT (InitFailureRead . ReadMetadataError (snapshotToMetadataPath ds)) $
      loadSnapshotMetadata fs ds
  Monad.when (snapshotBackend snapshotMeta /= UTxOHDMemSnapshot) $ do
    throwE $ InitFailureRead $ ReadMetadataError (snapshotToMetadataPath ds) MetadataBackendMismatch
  (extLedgerSt, checksumAsRead) <-
    withExceptT
      (InitFailureRead . ReadSnapshotFailed)
      $ readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode (snapshotToStatePath ds)
  case pointToWithOriginRealPoint (castPoint (getTip extLedgerSt)) of
    Origin -> throwE InitFailureGenesis
    NotOrigin pt -> do
      (values, Identity crcTables) <-
        withExceptT (InitFailureRead . ReadSnapshotFailed) $
          ExceptT $
            readIncremental
              fs
              Identity
              (valuesMKDecoder extLedgerSt)
              ( fsPathFromList $
                  fsPathToList (snapshotToDirPath ds)
                    <> [fromString "tables", fromString "tvar"]
              )
      let computedCRC = crcOfConcat checksumAsRead crcTables
      Monad.when (computedCRC /= snapshotChecksum snapshotMeta) $
        throwE $
          InitFailureRead $
            ReadSnapshotDataCorruption
      (,pt) <$> lift (empty extLedgerSt values (newInMemoryLedgerTablesHandle tracer fs))

mkInMemoryInitDb ::
  forall m blk.
  ( LedgerSupportsProtocol blk
  , IOLike m
  , LedgerDbSerialiseConstraints blk
  , HasHardForkHistory blk
  , LedgerSupportsV2LedgerDB (LedgerState blk)
  ) =>
  Complete LedgerDbArgs m blk ->
  ResolveBlock m blk ->
  SnapshotManagerV2 m blk ->
  InitDB (LedgerSeq' m blk) m blk
mkInMemoryInitDb args getBlock snapManager =
  InitDB
    { initFromGenesis = emptyF =<< lgrGenesis
    , initFromSnapshot =
        runExceptT
          . loadSnapshot v2Tracer lgrRegistry (configCodec . getExtLedgerCfg . ledgerDbCfg $ lgrConfig) lgrHasFS
    , abortLedgerDbInit = closeLedgerSeq
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
                , ldbResources = Nothing
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
    , lgrTracer
    , lgrQueryBatchSize
    , lgrRegistry
    } = args

  v2Tracer :: Tracer m (Trace m InMem)
  v2Tracer = LedgerDBFlavorImplEvent . SomeBackendTrace >$< lgrTracer

  emptyF ::
    ExtLedgerState blk ValuesMK ->
    m (LedgerSeq' m blk)
  emptyF st =
    empty' st $ newInMemoryLedgerTablesHandle v2Tracer lgrHasFS

instance
  ( LedgerSupportsProtocol blk
  , IOLike m
  , LedgerDbSerialiseConstraints blk
  , HasHardForkHistory blk
  , LedgerSupportsV2LedgerDB (LedgerState blk)
  ) =>
  LedgerDBBackend m InMem blk
  where
  data Args m InMem = NoArgs
  data Resources m InMem = NoResources
  data Trace m InMem
    = -- \| Created a new 'LedgerTablesHandle', potentially by duplicating an
      -- existing one.
      TraceLedgerTablesHandleCreate
    | -- \| Closed a 'LedgerTablesHandle'.
      TraceLedgerTablesHandleClose
    deriving Show

  type Db m blk = LedgerSeq' m blk
  type St m blk = StateRef m (ExtLedgerState blk)

  type N m InMem = m

  openLedgerDB NoArgs getBlock =
    pure (mkInMemoryInitDb args getBlock snapManager, snapManager)
   where
    snapManager = snapshotManager args
    args = undefined

  releaseResources _ NoResources = pure ()
