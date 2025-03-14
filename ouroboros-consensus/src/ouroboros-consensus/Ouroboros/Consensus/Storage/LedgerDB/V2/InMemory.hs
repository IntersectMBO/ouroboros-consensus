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

module Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory (
    -- * LedgerTablesHandle
    newInMemoryLedgerTablesHandle
    -- * Snapshots
  , loadSnapshot
  , snapshotToStatePath
  , snapshotToTablePath
  , takeSnapshot
  ) where

import           Cardano.Binary as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise (decode)
import qualified Control.Monad as Monad
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except
import           Control.ResourceRegistry
import           Control.Tracer
import qualified Data.Aeson as Aeson
import           Data.Functor.Contravariant ((>$<))
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.String (fromString)
import           GHC.Generics
import           NoThunks.Class
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import qualified Ouroboros.Consensus.Ledger.Tables.Diff as Diff
import           Ouroboros.Consensus.Storage.LedgerDB.API
import           Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import           Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
import           Ouroboros.Consensus.Util.CBOR (readIncremental)
import           Ouroboros.Consensus.Util.CRC
import           Ouroboros.Consensus.Util.Enclose
import           Ouroboros.Consensus.Util.IOLike
import           Prelude hiding (read)
import           System.FS.API
import           System.FS.API.Lazy
import           System.FS.CRC

{-------------------------------------------------------------------------------
  InMemory implementation of LedgerTablesHandles
-------------------------------------------------------------------------------}

data LedgerTablesHandleState l =
    LedgerTablesHandleOpen !(LedgerTables l ValuesMK)
  | LedgerTablesHandleClosed
  deriving Generic

deriving instance NoThunks (LedgerTables l ValuesMK) => NoThunks (LedgerTablesHandleState l)

data InMemoryClosedExn = InMemoryClosedExn
  deriving (Show, Exception)

guardClosed :: LedgerTablesHandleState l -> (LedgerTables l ValuesMK -> a) -> a
guardClosed LedgerTablesHandleClosed    _ = error $ show InMemoryClosedExn
guardClosed (LedgerTablesHandleOpen st) f = f st

newInMemoryLedgerTablesHandle ::
     forall m l.
     ( IOLike m
     , HasLedgerTables l
     , CanUpgradeLedgerTables l
     )
  => SomeHasFS m
  -> LedgerTables l ValuesMK
  -> m (LedgerTablesHandle m l)
newInMemoryLedgerTablesHandle someFS@(SomeHasFS hasFS) l = do
  !tv <- newTVarIO (LedgerTablesHandleOpen l)
  pure LedgerTablesHandle {
      close =
        atomically $ writeTVar tv LedgerTablesHandleClosed
    , duplicate = do
        hs <- readTVarIO tv
        !x <- guardClosed hs $ newInMemoryLedgerTablesHandle someFS
        pure x
    , read = \keys -> do
        hs <- readTVarIO tv
        guardClosed hs (pure . flip (ltliftA2 (\(ValuesMK v) (KeysMK k) -> ValuesMK $ v `Map.restrictKeys` k)) keys)
    , readRange = \(f, t) -> do
        hs <- readTVarIO tv
        guardClosed hs (\(LedgerTables (ValuesMK m)) ->
                          pure . LedgerTables . ValuesMK . Map.take t . (maybe id (\g -> snd . Map.split g) f) $ m)
    , readAll = do
        hs <- readTVarIO tv
        guardClosed hs pure
    , pushDiffs = \st0 !diffs ->
        atomically
        $ modifyTVar tv
        (\r -> guardClosed r (LedgerTablesHandleOpen . flip (ltliftA2 (\(ValuesMK vals) (DiffMK d) -> ValuesMK (Diff.applyDiff vals d))) (projectLedgerTables diffs) . upgradeTables st0 diffs))
    , takeHandleSnapshot = \snapshotName -> do
        createDirectoryIfMissing hasFS True $ mkFsPath [snapshotName, "tables"]
        h <- readTVarIO tv
        guardClosed h $
          \values ->
            withFile hasFS (mkFsPath [snapshotName, "tables", "tvar"]) (WriteMode MustBeNew) $ \hf ->
              fmap snd $ hPutAllCRC hasFS hf
                   $ CBOR.toLazyByteString
                   $ valuesMKEncoder values
    , tablesSize = do
        hs <- readTVarIO tv
        guardClosed hs (pure . Just . Map.size . getValuesMK . getLedgerTables)
    , isOpen = do
        hs <- readTVarIO tv
        case hs of
          LedgerTablesHandleOpen{}   -> pure True
          LedgerTablesHandleClosed{} -> pure False
    }

{-------------------------------------------------------------------------------
  Snapshots
-------------------------------------------------------------------------------}

-- | The path within the LedgerDB's filesystem to the file that contains the
-- snapshot's serialized ledger state
snapshotToStatePath :: DiskSnapshot -> FsPath
snapshotToStatePath = mkFsPath . (\x -> [x, "state"]) . snapshotToDirName

snapshotToTablePath :: DiskSnapshot -> FsPath
snapshotToTablePath = mkFsPath . (\x -> [x, "tables", "tvar"]) . snapshotToDirName

writeSnapshot ::
     MonadThrow m
  => SomeHasFS m
  -> Flag "DoDiskSnapshotChecksum"
  -> (ExtLedgerState blk EmptyMK -> Encoding)
  -> DiskSnapshot
  -> StateRef m (ExtLedgerState blk)
  -> m ()
writeSnapshot fs@(SomeHasFS hasFs) doChecksum encLedger ds st = do
    createDirectoryIfMissing hasFs True $ snapshotToDirPath ds
    crc1 <- writeExtLedgerState fs encLedger (snapshotToStatePath ds) $ state st
    crc2 <- takeHandleSnapshot (tables st) $ snapshotToDirName ds
    writeSnapshotMetadata fs ds $ SnapshotMetadata
      { metadataChecksum = do
          Monad.guard (getFlag doChecksum)
          pure $ crcOfConcat crc1 crc2
      }

-- | Write a snapshot metadata JSON file.
writeSnapshotMetadata ::
     MonadThrow m
  => SomeHasFS m
  -> DiskSnapshot
  -> SnapshotMetadata
  -> m ()
writeSnapshotMetadata (SomeHasFS hasFS) ds meta = do
  let metadataPath = snapshotToMetadataPath ds
  withFile hasFS metadataPath (WriteMode MustBeNew) $ \h ->
    Monad.void $ hPutAll hasFS h $ Aeson.encode meta

takeSnapshot ::
     ( IOLike m
     , LedgerDbSerialiseConstraints blk
     , LedgerSupportsProtocol blk
     )
  => CodecConfig blk
  -> Tracer m (TraceSnapshotEvent blk)
  -> SomeHasFS m
  -> Maybe String
  -> Flag "DoDiskSnapshotChecksum"
  -> StateRef m (ExtLedgerState blk)
  -> m (Maybe (DiskSnapshot, RealPoint blk))
takeSnapshot ccfg tracer hasFS suffix doChecksum st = do
  case pointToWithOriginRealPoint (castPoint (getTip $ state st)) of
    Origin -> return Nothing
    NotOrigin t -> do
      let number   = unSlotNo (realPointSlot t)
          snapshot = DiskSnapshot number suffix
      diskSnapshots <- listSnapshots hasFS
      if List.any (== DiskSnapshot number suffix) diskSnapshots then
        return Nothing
        else do
          encloseTimedWith (TookSnapshot snapshot t >$< tracer)
              $ writeSnapshot hasFS doChecksum (encodeDiskExtLedgerState ccfg) snapshot st
          return $ Just (snapshot, t)

-- | Read snapshot from disk.
--
--   Fail on data corruption, i.e. when the checksum of the read data differs
--   from the one tracked by @'DiskSnapshot'@.
loadSnapshot ::
    forall blk m. ( LedgerDbSerialiseConstraints blk
    , LedgerSupportsProtocol blk
    , IOLike m
    , LedgerSupportsInMemoryLedgerDB blk
    )
    => ResourceRegistry m
    -> CodecConfig blk
    -> SomeHasFS m
    -> Flag "DoDiskSnapshotChecksum"
    -> DiskSnapshot
    -> ExceptT (SnapshotFailure blk) m (LedgerSeq' m blk, RealPoint blk)
loadSnapshot _rr ccfg fs doChecksum ds = do
  (extLedgerSt, mbChecksumAsRead)  <- withExceptT
      (InitFailureRead . ReadSnapshotFailed) $
      readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode doChecksum (snapshotToStatePath ds)
  case pointToWithOriginRealPoint (castPoint (getTip extLedgerSt)) of
    Origin        -> throwE InitFailureGenesis
    NotOrigin pt -> do
      (values, mbCrcTables)  <-
        withExceptT (InitFailureRead . ReadSnapshotFailed) $
          ExceptT $ readIncremental fs (getFlag doChecksum)
                  valuesMKDecoder
                  (fsPathFromList
                    $ fsPathToList (snapshotToDirPath ds)
                    <> [fromString "tables", fromString "tvar"])
      let metaPath = snapshotToMetadataPath ds
      snapshotMeta <- withExceptT (InitFailureRead . ReadMetadataError metaPath) $
        loadSnapshotMetadata fs ds
      Monad.when (getFlag doChecksum) $ do
        -- TODO: not sure that I like relying on all of these being non-Nothing
        let computedCRC = crcOfConcat <$> mbChecksumAsRead <*> mbCrcTables
        Monad.when (computedCRC /= metadataChecksum snapshotMeta) $
          throwE $ InitFailureRead $ ReadSnapshotDataCorruption
      (,pt) <$> lift (empty extLedgerSt values (newInMemoryLedgerTablesHandle fs))

-- | Load a snapshot metadata JSON file.
--
--   - Fails with 'MetadataFileDoesNotExist' when the file doesn't exist;
--   - Fails with 'MetadataInvalid' when the contents of the file cannot be
--     deserialised correctly
loadSnapshotMetadata ::
     IOLike m
  => SomeHasFS m
  -> DiskSnapshot
  -> ExceptT MetadataErr m SnapshotMetadata
loadSnapshotMetadata (SomeHasFS hasFS) ds = ExceptT $ do
  let metadataPath = snapshotToMetadataPath ds
  exists <- doesFileExist hasFS metadataPath
  if not exists
    then pure $ Left MetadataFileDoesNotExist
    else do
      withFile hasFS metadataPath ReadMode $ \h -> do
        bs <- hGetAll hasFS h
        case Aeson.eitherDecode bs of
          Left decodeErr -> pure $ Left $ MetadataInvalid decodeErr
          Right meta -> pure $ Right meta
