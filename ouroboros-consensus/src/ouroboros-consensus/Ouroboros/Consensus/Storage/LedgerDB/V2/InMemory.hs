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
{-# LANGUAGE ViewPatterns #-}

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
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise (decode)
import           Control.Monad (unless, void)
import qualified Control.Monad as Monad
import           Control.Monad.Trans.Except
import           Control.ResourceRegistry
import           Control.Tracer
import           Data.Bits
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Char hiding (isHexDigit)
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
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Common
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots
import           Ouroboros.Consensus.Storage.LedgerDB.V2.LedgerSeq
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
     ( IOLike m
     , HasLedgerTables l
     , CanSerializeLedgerTables l
     )
  => SomeHasFS m
  -> LedgerTables l ValuesMK
  -> m (LedgerTablesHandle m l)
newInMemoryLedgerTablesHandle someFS@(SomeHasFS hasFS) l = do
  !tv <- newTVarIO (LedgerTablesHandleOpen l)
  pure LedgerTablesHandle {
      close =
        atomically $ modifyTVar tv (\_ -> LedgerTablesHandleClosed)
    , duplicate = do
        hs <- readTVarIO tv
        !x <- guardClosed hs $ newInMemoryLedgerTablesHandle someFS
        pure x
    , read = \keys -> do
        hs <- readTVarIO tv
        guardClosed hs (\st -> pure $ ltliftA2 rawRestrictValues st keys)
    , readRange = \(f, t) -> do
        hs <- readTVarIO tv
        guardClosed hs (\(LedgerTables (ValuesMK m)) ->
                          pure . LedgerTables . ValuesMK . Map.take t . (maybe id (\g -> snd . Map.split g) f) $ m)
    , write = \(!diffs) ->
        atomically
        $ modifyTVar tv
        (\r -> guardClosed r (\st -> LedgerTablesHandleOpen (ltliftA2 rawApplyDiffs st diffs)))
    , writeToDisk = \snapshotName -> do
        createDirectoryIfMissing hasFS True $ mkFsPath [snapshotName, "tables"]
        h <- readTVarIO tv
        guardClosed h $
          \values ->
            withFile hasFS (mkFsPath [snapshotName, "tables", "tvar"]) (WriteMode MustBeNew) $ \hf ->
              void $ hPutAll hasFS hf
                   $ CBOR.toLazyByteString
                   $ valuesMKEncoder values
    , tablesSize = do
        hs <- readTVarIO tv
        guardClosed hs (\(getLedgerTables -> ValuesMK m) -> pure $ Just $ Map.size m)
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
    -- TODO
    _crc2 <- writeToDisk (tables st) $ snapshotToDirName ds
    Monad.when (getFlag doChecksum) $
      withFile hasFs (snapshotToChecksumPath ds) (WriteMode MustBeNew) $ \h ->
        void $ hPutAll hasFs h . BS.toLazyByteString . BS.word32HexFixed $ getCRC crc1

takeSnapshot ::
     ( MonadThrow m
     , LedgerDbSerialiseConstraints blk
     , LedgerSupportsProtocol blk
     )
  => CodecConfig blk
  -> Tracer m (TraceSnapshotEvent blk)
  -> SomeHasFS m
  -> Maybe DiskSnapshot
  -> Flag "DoDiskSnapshotChecksum"
  -> StateRef m (ExtLedgerState blk)
  -> m (Maybe (DiskSnapshot, RealPoint blk))
takeSnapshot ccfg tracer hasFS dsOverride doChecksum st = do
  case pointToWithOriginRealPoint (castPoint (getTip $ state st)) of
    Origin -> return Nothing
    NotOrigin t -> do
      let number   = unSlotNo (realPointSlot t)
          snapshot = fromMaybe (DiskSnapshot number Nothing) dsOverride
      diskSnapshots <- listSnapshots hasFS
      if List.any ((== number) . dsNumber) diskSnapshots then
        return Nothing
        else do
          writeSnapshot hasFS doChecksum (encodeDiskExtLedgerState ccfg) snapshot st
          traceWith tracer $ TookSnapshot snapshot t
          return $ Just (snapshot, t)

-- | Read snapshot from disk.
--
--   Fail on data corruption, i.e. when the checksum of the read data differs
--   from the one tracked by @'DiskSnapshot'@.
loadSnapshot ::
    forall blk m. ( LedgerDbSerialiseConstraints blk
    , LedgerSupportsProtocol blk
    , IOLike m
    )
    => ResourceRegistry m
    -> CodecConfig blk
    -> SomeHasFS m
    -> DiskSnapshot
    -> Flag "DoDiskSnapshotChecksum"
    -> m (Either (SnapshotFailure blk) (LedgerSeq' m blk, RealPoint blk))
loadSnapshot _rr ccfg fs@(SomeHasFS hasFS) ds doChecksum = do
  eExtLedgerSt <- runExceptT $ readExtLedgerState fs (decodeDiskExtLedgerState ccfg) decode doChecksum (snapshotToStatePath ds)
  case eExtLedgerSt of
    Left err -> pure (Left $ InitFailureRead $ ReadSnapshotFailed err)
    Right (extLedgerSt, mbChecksumAsRead) ->
      let cont =
            case pointToWithOriginRealPoint (castPoint (getTip extLedgerSt)) of
              Origin        -> pure (Left InitFailureGenesis)
              NotOrigin pt -> do
                values <- withFile hasFS ( fsPathFromList
                                         $ fsPathToList (snapshotToDirPath ds)
                                         <> [fromString "tables", fromString "tvar"]) ReadMode $ \h -> do
                  bs <- hGetAll hasFS h
                  case CBOR.deserialiseFromBytes valuesMKDecoder bs of
                    Left  err        -> error $ show err
                    Right (extra, x) -> do
                      unless (BSL.null extra) $ error "Trailing bytes in snapshot"
                      pure x
                Right . (,pt) <$> empty extLedgerSt values (newInMemoryLedgerTablesHandle fs)
      in
      if getFlag doChecksum
        then do
          !snapshotCRC <- runExceptT $ readCRC (snapshotToChecksumPath ds)
          case snapshotCRC of
            Left err -> pure $ Left $ InitFailureRead err
            Right storedCrc ->
              if mbChecksumAsRead /= Just storedCrc then
                pure $ Left $ InitFailureRead $ ReadSnapshotDataCorruption
              else cont
        else cont

  where
    readCRC ::
      FsPath
      -> ExceptT ReadSnapshotErr m CRC
    readCRC crcPath = ExceptT $ do
        crcExists <- doesFileExist hasFS crcPath
        if not crcExists
          then pure (Left $ ReadSnapshotNoChecksumFile crcPath)
          else do
            withFile hasFS crcPath ReadMode $ \h -> do
              str <- BSL.toStrict <$> hGetAll hasFS h
              if not (BSC.length str == 8 && BSC.all isHexDigit str)
                then pure (Left $ ReadSnapshotInvalidChecksumFile crcPath)
                else pure . Right . CRC $ fromIntegral (hexdigitsToInt str)
        -- TODO: remove the functions in the where clause when we start depending on lsm-tree
      where
        isHexDigit :: Char -> Bool
        isHexDigit c = (c >= '0' && c <= '9')
                    || (c >= 'a' && c <= 'f') --lower case only

        -- Precondition: BSC.all isHexDigit
        hexdigitsToInt :: BSC.ByteString -> Word
        hexdigitsToInt =
            BSC.foldl' accumdigit 0
          where
            accumdigit :: Word -> Char -> Word
            accumdigit !a !c =
              (a `shiftL` 4) .|. hexdigitToWord c


        -- Precondition: isHexDigit
        hexdigitToWord :: Char -> Word
        hexdigitToWord c
          | let !dec = fromIntegral (ord c - ord '0')
          , dec <= 9  = dec

          | let !hex = fromIntegral (ord c - ord 'a' + 10)
          , otherwise = hex
