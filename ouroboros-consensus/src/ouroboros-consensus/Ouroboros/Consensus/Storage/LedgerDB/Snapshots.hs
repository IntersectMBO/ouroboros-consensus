{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.Storage.LedgerDB.Snapshots (
    DiskSnapshot (..)
    -- * Read from disk
  , ReadSnapshotErr (..)
  , SnapshotFailure (..)
  , diskSnapshotIsTemporary
  , listSnapshots
  , readSnapshot
    -- * Write to disk
  , takeSnapshot
  , trimSnapshots
  , writeSnapshot
    -- * Low-level API (primarily exposed for testing)
  , decodeSnapshotBackwardsCompatible
  , deleteSnapshot
  , encodeSnapshot
  , snapshotToFileName
  , snapshotToPath
    -- * Trace
  , TraceSnapshotEvent (..)
  ) where

import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise.Decoding (Decoder)
import qualified Codec.Serialise.Decoding as Dec
import           Codec.Serialise.Encoding (Encoding)
import           Control.Monad (forM, void, when)
import           Control.Monad.Except (ExceptT (..), throwError, withExceptT)
import           Control.Tracer
import           Data.Bits
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import           Data.Char (ord)
import           Data.Functor.Contravariant ((>$<))
import qualified Data.List as List
import           Data.Maybe (isJust, mapMaybe)
import           Data.Ord (Down (..), comparing)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           GHC.Generics (Generic)
import           GHC.Stack
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
import           Ouroboros.Consensus.Util.CBOR (ReadIncrementalErr,
                     decodeWithOrigin, readIncremental)
import           Ouroboros.Consensus.Util.Enclose
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Versioned
import           System.FS.API.Lazy
import           System.FS.CRC (CRC (..), hPutAllCRC)
import           Text.Read (readMaybe)

{-------------------------------------------------------------------------------
  Write to disk
-------------------------------------------------------------------------------}

data SnapshotFailure blk =
    -- | We failed to deserialise the snapshot
    --
    -- This can happen due to data corruption in the ledger DB.
    InitFailureRead ReadSnapshotErr

    -- | This snapshot is too recent (ahead of the tip of the chain)
  | InitFailureTooRecent (RealPoint blk)

    -- | This snapshot was of the ledger state at genesis, even though we never
    -- take snapshots at genesis, so this is unexpected.
  | InitFailureGenesis
  deriving (Show, Eq, Generic)

data TraceSnapshotEvent blk
  = InvalidSnapshot DiskSnapshot (SnapshotFailure blk)
    -- ^ An on disk snapshot was skipped because it was invalid.
  | TookSnapshot DiskSnapshot (RealPoint blk) EnclosingTimed
    -- ^ A snapshot was written to disk.
  | DeletedSnapshot DiskSnapshot
    -- ^ An old or invalid on-disk snapshot was deleted.
  | SnapshotMissingChecksum DiskSnapshot
    -- ^ The checksum file for a snapshot was missing and was not checked
  deriving (Generic, Eq, Show)

-- | Take a snapshot of the /oldest ledger state/ in the ledger DB
--
-- We write the /oldest/ ledger state to disk because the intention is to only
-- write ledger states to disk that we know to be immutable. Primarily for
-- testing purposes, 'takeSnapshot' returns the block reference corresponding
-- to the snapshot that we wrote.
--
-- If a snapshot with the same number already exists on disk or if the tip is at
-- genesis, no snapshot is taken.
--
-- Note that an EBB can have the same slot number and thus snapshot number as
-- the block after it. This doesn't matter. The one block difference in the
-- ledger state doesn't warrant an additional snapshot. The number in the name
-- of the snapshot is only indicative, we don't rely on it being correct.
--
-- NOTE: This is a lower-level API that takes a snapshot independent from
-- whether this snapshot corresponds to a state that is more than @k@ back.
--
-- TODO: Should we delete the file if an error occurs during writing?
takeSnapshot ::
     forall m blk. (MonadThrow m, MonadMonotonicTime m, IsLedger (LedgerState blk))
  => Tracer m (TraceSnapshotEvent blk)
  -> SomeHasFS m
  -> Flag "DoDiskSnapshotChecksum"
  -> (ExtLedgerState blk -> Encoding)
  -> ExtLedgerState blk -> m (Maybe (DiskSnapshot, RealPoint blk))
takeSnapshot tracer hasFS doChecksum encLedger oldest =
    case pointToWithOriginRealPoint (castPoint (getTip oldest)) of
      Origin ->
        return Nothing
      NotOrigin tip -> do
        let number   = unSlotNo (realPointSlot tip)
            snapshot = DiskSnapshot number Nothing
        snapshots <- listSnapshots hasFS
        if List.any ((== number) . dsNumber) snapshots then
          return Nothing
        else do
          encloseTimedWith (TookSnapshot snapshot tip >$< tracer)
              $ writeSnapshot hasFS doChecksum encLedger snapshot oldest
          return $ Just (snapshot, tip)

-- | Trim the number of on disk snapshots so that at most 'onDiskNumSnapshots'
-- snapshots are stored on disk. The oldest snapshots are deleted.
--
-- The deleted snapshots are returned.
trimSnapshots ::
     Monad m
  => Tracer m (TraceSnapshotEvent r)
  -> SomeHasFS m
  -> DiskPolicy
  -> m [DiskSnapshot]
trimSnapshots tracer hasFS DiskPolicy{..} = do
    -- We only trim temporary snapshots
    snapshots <- filter diskSnapshotIsTemporary <$> listSnapshots hasFS
    -- The snapshot are most recent first, so we can simply drop from the
    -- front to get the snapshots that are "too" old.
    forM (drop (fromIntegral onDiskNumSnapshots) snapshots) $ \snapshot -> do
      deleteSnapshot hasFS snapshot
      traceWith tracer $ DeletedSnapshot snapshot
      return snapshot

{-------------------------------------------------------------------------------
  Internal: reading from disk
-------------------------------------------------------------------------------}

-- | Name of a disk snapshot.
--
--   The snapshot itself might not yet exist on disk.
data DiskSnapshot = DiskSnapshot {
      -- | Snapshots are numbered. We will try the snapshots with the highest
      -- number first.
      --
      -- When creating a snapshot, we use the slot number of the ledger state it
      -- corresponds to as the snapshot number. This gives an indication of how
      -- recent the snapshot is.
      --
      -- Note that the snapshot names are only indicative, we don't rely on the
      -- snapshot number matching the slot number of the corresponding ledger
      -- state. We only use the snapshots numbers to determine the order in
      -- which we try them.
      dsNumber :: Word64

      -- | Snapshots can optionally have a suffix, separated by the snapshot
      -- number with an underscore, e.g., @4492799_last_Byron@. This suffix acts
      -- as metadata for the operator of the node. Snapshots with a suffix will
      -- /not be trimmed/.
    , dsSuffix :: Maybe String
    }
  deriving (Show, Eq, Generic)

instance Ord DiskSnapshot where
  compare = comparing dsNumber

-- | Named snapshot are permanent, they will never be deleted when trimming.
diskSnapshotIsPermanent :: DiskSnapshot -> Bool
diskSnapshotIsPermanent = isJust . dsSuffix

-- | The snapshots that are periodically created are temporary, they will be
-- deleted when trimming
diskSnapshotIsTemporary :: DiskSnapshot -> Bool
diskSnapshotIsTemporary = not . diskSnapshotIsPermanent

data ReadSnapshotErr =
    -- | Error while de-serialising data
    ReadSnapshotFailed ReadIncrementalErr
    -- | Checksum of read snapshot differs from the one tracked by
    --   the corresponding '.checksum' file
  | ReadSnapshotDataCorruption
    -- | A '.checksum' file does not exist for a @'DiskSnapshot'@
  | ReadSnapshotNoChecksumFile FsPath
    -- | A '.checksum' file exists for a @'DiskSnapshot'@, but its contents is invalid
  | ReadSnapshotInvalidChecksumFile FsPath
  deriving (Eq, Show)

-- | Read snapshot from disk.
--
--   Fail on data corruption, i.e. when the checksum of the read data differs
--   from the one tracked by @'DiskSnapshot'@.
readSnapshot ::
     forall m blk. IOLike m
  => SomeHasFS m
  -> (forall s. Decoder s (ExtLedgerState blk))
  -> (forall s. Decoder s (HeaderHash blk))
  -> Flag "DoDiskSnapshotChecksum"
  -> DiskSnapshot
  -> ExceptT ReadSnapshotErr m (ExtLedgerState blk)
readSnapshot someHasFS decLedger decHash doChecksum snapshotName = do
  (ledgerState, mbChecksumAsRead) <- withExceptT ReadSnapshotFailed . ExceptT $
      readIncremental someHasFS (getFlag doChecksum) decoder (snapshotToPath snapshotName)
  when (getFlag doChecksum) $ do
    !snapshotCRC <- readCRC someHasFS (snapshotToChecksumPath snapshotName)
    when (mbChecksumAsRead /= Just snapshotCRC) $
      throwError ReadSnapshotDataCorruption
  pure ledgerState
  where
    decoder :: Decoder s (ExtLedgerState blk)
    decoder = decodeSnapshotBackwardsCompatible (Proxy @blk) decLedger decHash

    readCRC ::
      SomeHasFS m
      -> FsPath
      -> ExceptT ReadSnapshotErr m CRC
    readCRC (SomeHasFS hasFS) crcPath = ExceptT $ do
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

-- | Write a ledger state snapshot to disk
--
--   This function writes two files:
--   * the snapshot file itself, with the name generated by @'snapshotToPath'@
--   * the checksum file, with the name generated by @'snapshotToChecksumPath'@
writeSnapshot ::
     forall m blk. MonadThrow m
  => SomeHasFS m
  -> Flag "DoDiskSnapshotChecksum"
  -> (ExtLedgerState blk -> Encoding)
  -> DiskSnapshot
  -> ExtLedgerState blk -> m ()
writeSnapshot (SomeHasFS hasFS) doChecksum encLedger ss cs = do
    crc <- withFile hasFS (snapshotToPath ss) (WriteMode MustBeNew) $ \h ->
      snd <$> hPutAllCRC hasFS h (CBOR.toLazyByteString $ encode cs)
    when (getFlag doChecksum) $
      withFile hasFS (snapshotToChecksumPath ss) (WriteMode MustBeNew) $ \h ->
        void $ hPutAll hasFS h . BS.toLazyByteString . BS.word32HexFixed $ getCRC crc
  where
    encode :: ExtLedgerState blk -> Encoding
    encode = encodeSnapshot encLedger

-- | Delete snapshot from disk
deleteSnapshot :: Monad m => HasCallStack => SomeHasFS m -> DiskSnapshot -> m ()
deleteSnapshot (SomeHasFS hasFS) snapshot = do
  removeFile hasFS (snapshotToPath snapshot)
  checksumFileExists <- doesFileExist hasFS (snapshotToChecksumPath snapshot)
  when checksumFileExists $
    removeFile hasFS (snapshotToChecksumPath snapshot)

-- | List on-disk snapshots, highest number first.
listSnapshots :: Monad m => SomeHasFS m -> m [DiskSnapshot]
listSnapshots (SomeHasFS HasFS{..}) =
    aux <$> listDirectory (mkFsPath [])
  where
    aux :: Set String -> [DiskSnapshot]
    aux = List.sortOn Down . mapMaybe snapshotFromPath . Set.toList

snapshotToChecksumFileName :: DiskSnapshot -> String
snapshotToChecksumFileName = (<> ".checksum") . snapshotToFileName

snapshotToFileName :: DiskSnapshot -> String
snapshotToFileName DiskSnapshot { dsNumber, dsSuffix } =
    show dsNumber <> suffix
  where
    suffix = case dsSuffix of
      Nothing -> ""
      Just s  -> "_" <> s

snapshotToChecksumPath :: DiskSnapshot -> FsPath
snapshotToChecksumPath = mkFsPath . (:[]) . snapshotToChecksumFileName

snapshotToPath :: DiskSnapshot -> FsPath
snapshotToPath = mkFsPath . (:[]) . snapshotToFileName

snapshotFromPath :: String -> Maybe DiskSnapshot
snapshotFromPath fileName = do
    number <- readMaybe prefix
    return $ DiskSnapshot number suffix'
  where
    (prefix, suffix) = break (== '_') fileName

    suffix' :: Maybe String
    suffix' = case suffix of
      ""      -> Nothing
      _ : str -> Just str

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

-- | Version 1: uses versioning ('Ouroboros.Consensus.Util.Versioned') and only
-- encodes the ledger state @l@.
snapshotEncodingVersion1 :: VersionNumber
snapshotEncodingVersion1 = 1

-- | Encoder to be used in combination with 'decodeSnapshotBackwardsCompatible'.
encodeSnapshot :: (l -> Encoding) -> l -> Encoding
encodeSnapshot encodeLedger l =
    encodeVersion snapshotEncodingVersion1 (encodeLedger l)

-- | To remain backwards compatible with existing snapshots stored on disk, we
-- must accept the old format as well as the new format.
--
-- The old format:
-- * The tip: @WithOrigin (RealPoint blk)@
-- * The chain length: @Word64@
-- * The ledger state: @l@
--
-- The new format is described by 'snapshotEncodingVersion1'.
--
-- This decoder will accept and ignore them. The encoder ('encodeSnapshot') will
-- no longer encode them.
decodeSnapshotBackwardsCompatible ::
     forall l blk.
     Proxy blk
  -> (forall s. Decoder s l)
  -> (forall s. Decoder s (HeaderHash blk))
  -> forall s. Decoder s l
decodeSnapshotBackwardsCompatible _ decodeLedger decodeHash =
    decodeVersionWithHook
      decodeOldFormat
      [(snapshotEncodingVersion1, Decode decodeVersion1)]
  where
    decodeVersion1 :: forall s. Decoder s l
    decodeVersion1 = decodeLedger

    decodeOldFormat :: Maybe Int -> forall s. Decoder s l
    decodeOldFormat (Just 3) = do
        _ <- withOriginRealPointToPoint <$>
               decodeWithOrigin (decodeRealPoint @blk decodeHash)
        _ <- Dec.decodeWord64
        decodeLedger
    decodeOldFormat mbListLen =
        fail $
          "decodeSnapshotBackwardsCompatible: invalid start " <>
          show mbListLen
