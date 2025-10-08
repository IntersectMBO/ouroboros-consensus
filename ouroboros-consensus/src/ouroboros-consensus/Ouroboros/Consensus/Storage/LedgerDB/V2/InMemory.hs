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
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory
  ( -- * LedgerTablesHandle
    newInMemoryLedgerTablesHandle

    -- * Snapshots
  , loadSnapshot
  , snapshotManager

    -- * snapshot-converter
  , implTakeSnapshot
  ) where

import qualified Codec.CBOR.Write as CBOR
import Codec.Serialise (decode)
import qualified Control.Monad as Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Control.ResourceRegistry
import Control.Tracer
import Data.Functor.Contravariant ((>$<))
import Data.Functor.Identity
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.String (fromString)
import GHC.Generics
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import Ouroboros.Consensus.Storage.LedgerDB.API
import Ouroboros.Consensus.Storage.LedgerDB.Args
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Storage.LedgerDB.TraceEvent
import qualified Ouroboros.Consensus.Storage.LedgerDB.V2.Args as V2
import Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.CBOR (readIncremental)
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike
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

newInMemoryLedgerTablesHandle ::
  forall m l.
  ( IOLike m
  , HasLedgerTables l
  , CanUpgradeLedgerTables l
  , SerializeTablesWithHint l
  ) =>
  Tracer m V2.FlavorImplSpecificTrace ->
  SomeHasFS m ->
  LedgerTables l ValuesMK ->
  m (LedgerTablesHandle m l)
newInMemoryLedgerTablesHandle tracer someFS@(SomeHasFS hasFS) l = do
  !tv <- newTVarIO (LedgerTablesHandleOpen l)
  traceWith tracer V2.TraceLedgerTablesHandleCreate
  pure
    LedgerTablesHandle
      { close = implClose tracer tv
      , duplicate = implDuplicate tracer tv someFS
      , read = implRead tv
      , readRange = implReadRange tv
      , readAll = implReadAll tv
      , pushDiffs = implPushDiffs tv
      , takeHandleSnapshot = implTakeHandleSnapshot tv hasFS
      , tablesSize = implTablesSize tv
      }

{-# INLINE implClose #-}
{-# INLINE implDuplicate #-}
{-# INLINE implRead #-}
{-# INLINE implReadRange #-}
{-# INLINE implReadAll #-}
{-# INLINE implPushDiffs #-}
{-# INLINE implTakeHandleSnapshot #-}
{-# INLINE implTablesSize #-}

implClose ::
  IOLike m =>
  Tracer m V2.FlavorImplSpecificTrace ->
  StrictTVar m (LedgerTablesHandleState l) ->
  m ()
implClose tracer tv = do
  p <- atomically $ swapTVar tv LedgerTablesHandleClosed
  case p of
    LedgerTablesHandleOpen{} -> traceWith tracer V2.TraceLedgerTablesHandleClose
    _ -> pure ()

implDuplicate ::
  ( IOLike m
  , HasLedgerTables l
  , CanUpgradeLedgerTables l
  , SerializeTablesWithHint l
  ) =>
  Tracer m V2.FlavorImplSpecificTrace ->
  StrictTVar m (LedgerTablesHandleState l) ->
  SomeHasFS m ->
  m (LedgerTablesHandle m l)
implDuplicate tracer tv someFS = do
  hs <- readTVarIO tv
  !x <- guardClosed hs $ newInMemoryLedgerTablesHandle tracer someFS
  pure x

implRead ::
  ( IOLike m
  , HasLedgerTables l
  ) =>
  StrictTVar m (LedgerTablesHandleState l) ->
  l EmptyMK ->
  LedgerTables l KeysMK ->
  m (LedgerTables l ValuesMK)
implRead tv _ keys = do
  hs <- readTVarIO tv
  guardClosed
    hs
    (pure . flip (ltliftA2 (\(ValuesMK v) (KeysMK k) -> ValuesMK $ v `Map.restrictKeys` k)) keys)

implReadRange ::
  (IOLike m, HasLedgerTables l) =>
  StrictTVar m (LedgerTablesHandleState l) ->
  l EmptyMK ->
  (Maybe (TxIn l), Int) ->
  m (LedgerTables l ValuesMK, Maybe (TxIn l))
implReadRange tv _ (f, t) = do
  hs <- readTVarIO tv
  guardClosed
    hs
    ( \(LedgerTables (ValuesMK m)) ->
        let m' = Map.take t . (maybe id (\g -> snd . Map.split g) f) $ m
         in pure (LedgerTables (ValuesMK m'), fst <$> Map.lookupMax m')
    )

implReadAll ::
  IOLike m =>
  StrictTVar m (LedgerTablesHandleState l) ->
  l EmptyMK ->
  m (LedgerTables l ValuesMK)
implReadAll tv _ = do
  hs <- readTVarIO tv
  guardClosed hs pure

implPushDiffs ::
  ( IOLike m
  , HasLedgerTables l
  , CanUpgradeLedgerTables l
  ) =>
  StrictTVar m (LedgerTablesHandleState l) ->
  l mk1 ->
  l DiffMK ->
  m ()
implPushDiffs tv st0 !diffs =
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

implTakeHandleSnapshot ::
  (IOLike m, SerializeTablesWithHint l) =>
  StrictTVar m (LedgerTablesHandleState l) ->
  HasFS m h ->
  l EmptyMK ->
  String ->
  m (Maybe CRC)
implTakeHandleSnapshot tv hasFS hint snapshotName = do
  createDirectoryIfMissing hasFS True $ mkFsPath [snapshotName, "tables"]
  h <- readTVarIO tv
  guardClosed h $
    \values ->
      withFile hasFS (mkFsPath [snapshotName, "tables", "tvar"]) (WriteMode MustBeNew) $ \hf ->
        fmap (Just . snd) $
          hPutAllCRC hasFS hf $
            CBOR.toLazyByteString $
              valuesMKEncoder hint values

implTablesSize ::
  IOLike m =>
  StrictTVar m (LedgerTablesHandleState l) ->
  m (Maybe Int)
implTablesSize tv = do
  hs <- readTVarIO tv
  guardClosed hs (pure . Just . Map.size . getValuesMK . getLedgerTables)

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

{-# INLINE implTakeSnapshot #-}
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
implTakeSnapshot ccfg tracer shfs@(SomeHasFS hasFS) suffix st = do
  case pointToWithOriginRealPoint (castPoint (getTip $ state st)) of
    Origin -> return Nothing
    NotOrigin t -> do
      let number = unSlotNo (realPointSlot t)
          snapshot = DiskSnapshot number suffix
      diskSnapshots <- defaultListSnapshots shfs
      if List.any (== DiskSnapshot number suffix) diskSnapshots
        then
          return Nothing
        else do
          encloseTimedWith (TookSnapshot snapshot t >$< tracer) $
            writeSnapshot snapshot
          return $ Just (snapshot, t)
 where
  writeSnapshot ds = do
    createDirectoryIfMissing hasFS True $ snapshotToDirPath ds
    crc1 <- writeExtLedgerState shfs (encodeDiskExtLedgerState ccfg) (snapshotToStatePath ds) $ state st
    crc2 <- takeHandleSnapshot (tables st) (state st) $ snapshotToDirName ds
    writeSnapshotMetadata shfs ds $
      SnapshotMetadata
        { snapshotBackend = UTxOHDMemSnapshot
        , snapshotChecksum = maybe crc1 (crcOfConcat crc1) crc2
        }

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
  Tracer m V2.FlavorImplSpecificTrace ->
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
