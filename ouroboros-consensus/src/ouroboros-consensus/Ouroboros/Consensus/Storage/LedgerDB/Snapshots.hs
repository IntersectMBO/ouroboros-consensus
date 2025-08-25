{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Common logic and types for LedgerDB Snapshots.
--
-- Snapshots are saved copies of Ledger states in the chain which can be used to
-- restart the node without having to replay the whole chain. Regardless of the
-- actual LedgerDB implementation chosen, the general management of snapshots is
-- common to all implementations.
module Ouroboros.Consensus.Storage.LedgerDB.Snapshots
  ( -- * Snapshots
    CRCError (..)
  , DiskSnapshot (..)
  , MetadataErr (..)
  , NumOfDiskSnapshots (..)
  , ReadSnapshotErr (..)
  , SnapshotBackend (..)
  , SnapshotFailure (..)
  , SnapshotMetadata (..)
  , SnapshotPolicyArgs (..)
  , defaultSnapshotPolicyArgs

    -- * Codec
  , readExtLedgerState
  , writeExtLedgerState

    -- * Paths
  , diskSnapshotIsTemporary
  , snapshotFromPath
  , snapshotToChecksumPath
  , snapshotToStatePath
  , snapshotToDirName
  , snapshotToDirPath
  , snapshotToMetadataPath

    -- * Management
  , SnapshotManager (..)
  , defaultDeleteSnapshot
  , defaultListSnapshots
  , trimSnapshots
  , loadSnapshotMetadata
  , writeSnapshotMetadata

    -- * Policy
  , SnapshotInterval (..)
  , SnapshotPolicy (..)
  , defaultSnapshotPolicy
  , pattern DoDiskSnapshotChecksum
  , pattern NoDoDiskSnapshotChecksum

    -- * Tracing
  , TraceSnapshotEvent (..)

    -- * Re-exports
  , Flag (..)

    -- * Testing
  , decodeLBackwardsCompatible
  , destroySnapshots
  , encodeL
  , snapshotsMapM_
  ) where

import Cardano.Ledger.BaseTypes
import Codec.CBOR.Decoding
import Codec.CBOR.Encoding
import qualified Codec.CBOR.Write as CBOR
import qualified Codec.Serialise.Decoding as Dec
import Control.Monad
import qualified Control.Monad as Monad
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Except
import Control.Tracer
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Functor.Identity
import qualified Data.List as List
import Data.Maybe (isJust, mapMaybe)
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (secondsToDiffTime)
import Data.Word
import GHC.Generics
import NoThunks.Class
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Ledger.Abstract (EmptyMK)
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Util (Flag (..))
import Ouroboros.Consensus.Util.CBOR
  ( ReadIncrementalErr
  , decodeWithOrigin
  , readIncremental
  )
import Ouroboros.Consensus.Util.CRC
import Ouroboros.Consensus.Util.CallStack
import Ouroboros.Consensus.Util.Enclose
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.Versioned
import System.FS.API
import System.FS.API.Lazy
import System.FS.CRC
import Text.Read (readMaybe)

-- | Name of a disk snapshot.
--
--   The snapshot itself might not yet exist on disk.
data DiskSnapshot = DiskSnapshot
  { dsNumber :: Word64
  -- ^ Snapshots are numbered. We will try the snapshots with the highest
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
  , dsSuffix :: Maybe String
  -- ^ Snapshots can optionally have a suffix, separated by the snapshot
  -- number with an underscore, e.g., @4492799_last_Byron@. This suffix acts
  -- as metadata for the operator of the node. Snapshots with a suffix will
  -- /not be deleted/.
  }
  deriving (Show, Eq, Generic)

instance Ord DiskSnapshot where
  compare = comparing dsNumber

data SnapshotFailure blk
  = -- | We failed to deserialise the snapshot
    --
    -- This can happen due to data corruption in the ledger DB or if the codecs
    -- changed.
    InitFailureRead ReadSnapshotErr
  | -- | This snapshot is too recent (ahead of the tip of the immutable chain)
    InitFailureTooRecent (RealPoint blk)
  | -- | This snapshot was of the ledger state at genesis, even though we never
    -- take snapshots at genesis, so this is unexpected.
    InitFailureGenesis
  deriving (Show, Eq, Generic)

data ReadSnapshotErr
  = -- | Error while de-serialising data
    ReadSnapshotFailed ReadIncrementalErr
  | -- | Checksum of read snapshot differs from the one tracked by
    --   its corresponding metadata file
    ReadSnapshotDataCorruption
  | -- | An error occurred while reading the snapshot metadata file
    ReadMetadataError FsPath MetadataErr
  deriving (Eq, Show)

data SnapshotMetadata = SnapshotMetadata
  { snapshotBackend :: SnapshotBackend
  , snapshotChecksum :: CRC
  }
  deriving (Eq, Show)

instance ToJSON SnapshotMetadata where
  toJSON sm =
    Aeson.object
      [ "backend" .= snapshotBackend sm
      , "checksum" .= getCRC (snapshotChecksum sm)
      ]

instance FromJSON SnapshotMetadata where
  parseJSON = Aeson.withObject "SnapshotMetadata" $ \o ->
    SnapshotMetadata
      <$> o .: "backend"
      <*> fmap CRC (o .: "checksum")

data SnapshotBackend
  = UTxOHDMemSnapshot
  | UTxOHDLMDBSnapshot
  | UTxOHDLSMSnapshot
  deriving (Eq, Show)

instance ToJSON SnapshotBackend where
  toJSON = \case
    UTxOHDMemSnapshot -> "utxohd-mem"
    UTxOHDLMDBSnapshot -> "utxohd-lmdb"
    UTxOHDLSMSnapshot -> "utxohd-lsm"

instance FromJSON SnapshotBackend where
  parseJSON = Aeson.withText "SnapshotBackend" $ \case
    "utxohd-mem" -> pure UTxOHDMemSnapshot
    "utxohd-lmdb" -> pure UTxOHDLMDBSnapshot
    "utxohd-lsm" -> pure UTxOHDLSMSnapshot
    _ -> fail "unknown SnapshotBackend"

data MetadataErr
  = -- | The metadata file does not exist
    MetadataFileDoesNotExist
  | -- | The metadata file is invalid and does not deserialize
    MetadataInvalid String
  | -- | The metadata file has the incorrect backend
    MetadataBackendMismatch
  deriving (Eq, Show)

-- | Management of snapshots for the different LedgerDB backends.
--
-- The LedgerDB V1 takes snapshots in @ReadLocked m@, hence the two different
-- @m@ and @n@ monad types.
data SnapshotManager m n blk st = SnapshotManager
  { listSnapshots :: m [DiskSnapshot]
  , deleteSnapshot :: DiskSnapshot -> m ()
  , takeSnapshot ::
      Maybe String ->
      -- \^ The (possibly empty) suffix for the snapshot name
      st ->
      -- \^ The state needed for taking the snapshot:
      -- - In V1: this will be the DbChangelog and the Backing store
      -- - In V2: this will be a StateRef
      n (Maybe (DiskSnapshot, RealPoint blk))
      -- \^ If a Snapshot was taken, its information and the point at which it
      -- was taken.
  }

-- | Named snapshot are permanent, they will never be deleted even if failing to
-- deserialize.
diskSnapshotIsPermanent :: DiskSnapshot -> Bool
diskSnapshotIsPermanent = isJust . dsSuffix

-- | The snapshots that are periodically created are temporary, they will be
-- deleted when trimming or if they fail to deserialize.
diskSnapshotIsTemporary :: DiskSnapshot -> Bool
diskSnapshotIsTemporary = not . diskSnapshotIsPermanent

snapshotFromPath :: String -> Maybe DiskSnapshot
snapshotFromPath fileName = do
  number <- readMaybe prefix
  return $ DiskSnapshot number suffix'
 where
  (prefix, suffix) = break (== '_') fileName

  suffix' :: Maybe String
  suffix' = case suffix of
    "" -> Nothing
    _ : str -> Just str

-- | List on-disk snapshots, highest number first.
defaultListSnapshots :: Monad m => SomeHasFS m -> m [DiskSnapshot]
defaultListSnapshots (SomeHasFS HasFS{listDirectory}) =
  aux <$> listDirectory (mkFsPath [])
 where
  aux :: Set String -> [DiskSnapshot]
  aux = List.sortOn (Down . dsNumber) . mapMaybe snapshotFromPath . Set.toList

-- | Delete snapshot from disk
defaultDeleteSnapshot ::
  (Monad m, HasCallStack) => SomeHasFS m -> Tracer m (TraceSnapshotEvent blk) -> DiskSnapshot -> m ()
defaultDeleteSnapshot (SomeHasFS HasFS{doesDirectoryExist, removeDirectoryRecursive}) tracer ss = do
  let p = snapshotToDirPath ss
  exists <- doesDirectoryExist p
  when exists (removeDirectoryRecursive p)
  traceWith tracer (DeletedSnapshot ss)

-- | Write a snapshot metadata JSON file.
writeSnapshotMetadata ::
  MonadThrow m =>
  SomeHasFS m ->
  DiskSnapshot ->
  SnapshotMetadata ->
  m ()
writeSnapshotMetadata (SomeHasFS hasFS) ds meta = do
  let metadataPath = snapshotToMetadataPath ds
  withFile hasFS metadataPath (WriteMode MustBeNew) $ \h ->
    Monad.void $ hPutAll hasFS h $ Aeson.encode meta

-- | Load a snapshot metadata JSON file.
--
--   - Fails with 'MetadataFileDoesNotExist' when the file doesn't exist;
--   - Fails with 'MetadataInvalid' when the contents of the file cannot be
--     deserialised correctly
loadSnapshotMetadata ::
  IOLike m =>
  SomeHasFS m ->
  DiskSnapshot ->
  ExceptT MetadataErr m SnapshotMetadata
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

snapshotsMapM_ :: Monad m => SnapshotManager m n blk st -> (DiskSnapshot -> m a) -> m ()
snapshotsMapM_ snapManager f =
  mapM_ f =<< listSnapshots snapManager

-- | Testing only! Destroy all snapshots in the DB.
destroySnapshots :: Monad m => SnapshotManager m n blk st -> m ()
destroySnapshots snapManager =
  snapshotsMapM_
    snapManager
    (deleteSnapshot snapManager)

-- | Read an extended ledger state from disk
readExtLedgerState ::
  forall m blk.
  IOLike m =>
  SomeHasFS m ->
  (forall s. Decoder s (ExtLedgerState blk EmptyMK)) ->
  (forall s. Decoder s (HeaderHash blk)) ->
  FsPath ->
  ExceptT ReadIncrementalErr m (ExtLedgerState blk EmptyMK, CRC)
readExtLedgerState hasFS decLedger decHash =
  do
    ExceptT
    . fmap (fmap (fmap runIdentity))
    . readIncremental hasFS Identity decoder
 where
  decoder :: Decoder s (ExtLedgerState blk EmptyMK)
  decoder = decodeLBackwardsCompatible (Proxy @blk) decLedger decHash

-- | Write an extended ledger state to disk
writeExtLedgerState ::
  forall m blk.
  MonadThrow m =>
  SomeHasFS m ->
  (ExtLedgerState blk EmptyMK -> Encoding) ->
  FsPath ->
  ExtLedgerState blk EmptyMK ->
  m CRC
writeExtLedgerState (SomeHasFS hasFS) encLedger path cs = do
  withFile hasFS path (WriteMode MustBeNew) $ \h ->
    snd <$> hPutAllCRC hasFS h (CBOR.toLazyByteString $ encoder cs)
 where
  encoder :: ExtLedgerState blk EmptyMK -> Encoding
  encoder = encodeL encLedger

-- | Trim the number of on disk snapshots so that at most 'onDiskNumSnapshots'
-- snapshots are stored on disk. The oldest snapshots are deleted.
--
-- The deleted snapshots are returned.
trimSnapshots ::
  Monad m =>
  SnapshotManager m n blk st ->
  SnapshotPolicy ->
  m [DiskSnapshot]
trimSnapshots snapManager SnapshotPolicy{onDiskNumSnapshots} = do
  -- We only trim temporary snapshots
  ss <- filter diskSnapshotIsTemporary <$> listSnapshots snapManager
  -- The snapshot are most recent first, so we can simply drop from the
  -- front to get the snapshots that are "too" old.
  let ssTooOld = drop (fromIntegral onDiskNumSnapshots) ss
  mapM
    ( \s -> do
        deleteSnapshot snapManager s
        pure s
    )
    ssTooOld

snapshotToDirName :: DiskSnapshot -> String
snapshotToDirName DiskSnapshot{dsNumber, dsSuffix} =
  show dsNumber <> suffix
 where
  suffix = case dsSuffix of
    Nothing -> ""
    Just s -> "_" <> s

snapshotToChecksumPath :: DiskSnapshot -> FsPath
snapshotToChecksumPath = mkFsPath . (\x -> [x, "checksum"]) . snapshotToDirName

snapshotToMetadataPath :: DiskSnapshot -> FsPath
snapshotToMetadataPath = mkFsPath . (\x -> [x, "meta"]) . snapshotToDirName

-- | The path within the LedgerDB's filesystem to the snapshot's directory
snapshotToDirPath :: DiskSnapshot -> FsPath
snapshotToDirPath = mkFsPath . (: []) . snapshotToDirName

-- | The path within the LedgerDB's filesystem to the file that contains the
-- snapshot's serialized ledger state
snapshotToStatePath :: DiskSnapshot -> FsPath
snapshotToStatePath = mkFsPath . (\x -> [x, "state"]) . snapshotToDirName

-- | Version 1: uses versioning ('Ouroboros.Consensus.Util.Versioned') and only
-- encodes the ledger state @l@.
snapshotEncodingVersion1 :: VersionNumber
snapshotEncodingVersion1 = 1

-- | Encoder to be used in combination with 'decodeSnapshotBackwardsCompatible'.
encodeL :: (l -> Encoding) -> l -> Encoding
encodeL encodeLedger l =
  encodeVersion snapshotEncodingVersion1 (encodeLedger l)

-- | To remain backwards compatible with existing snapshots stored on disk, we
-- must accept the old format as well as the new format.
--
-- The old format:
--
-- * The tip: @WithOrigin (RealPoint blk)@
--
-- * The chain length: @Word64@
--
-- * The ledger state: @l@
--
-- The new format is described by 'snapshotEncodingVersion1'.
--
-- This decoder will accept and ignore them. The encoder ('encodeSnapshot') will
-- no longer encode them.
decodeLBackwardsCompatible ::
  forall l blk.
  Proxy blk ->
  (forall s. Decoder s l) ->
  (forall s. Decoder s (HeaderHash blk)) ->
  forall s.
  Decoder s l
decodeLBackwardsCompatible _ decodeLedger decodeHash =
  decodeVersionWithHook
    decodeOldFormat
    [(snapshotEncodingVersion1, Decode decodeVersion1)]
 where
  decodeVersion1 :: forall s. Decoder s l
  decodeVersion1 = decodeLedger

  decodeOldFormat :: Maybe Int -> forall s. Decoder s l
  decodeOldFormat (Just 3) = do
    _ <-
      withOriginRealPointToPoint
        <$> decodeWithOrigin (decodeRealPoint @blk decodeHash)
    _ <- Dec.decodeWord64
    decodeLedger
  decodeOldFormat mbListLen =
    fail $
      "decodeSnapshotBackwardsCompatible: invalid start "
        <> show mbListLen

{-------------------------------------------------------------------------------
  Policy
-------------------------------------------------------------------------------}

-- | Length of time that has to pass after which a snapshot is taken.
data SnapshotInterval
  = DefaultSnapshotInterval
  | RequestedSnapshotInterval DiffTime
  | DisableSnapshots
  deriving stock (Eq, Generic, Show)

-- | Number of snapshots to be stored on disk. This is either the default value
-- as determined by the @'SnapshotPolicy'@, or it is provided by the user. See the
-- @'SnapshotPolicy'@ documentation for more information.
data NumOfDiskSnapshots
  = DefaultNumOfDiskSnapshots
  | RequestedNumOfDiskSnapshots Word
  deriving stock (Eq, Generic, Show)

-- | Type-safe flag to regulate the checksum policy of the ledger state snapshots.
--
-- These patterns are exposed to cardano-node and will be passed as part of @'SnapshotPolicy'@.
pattern DoDiskSnapshotChecksum, NoDoDiskSnapshotChecksum :: Flag "DoDiskSnapshotChecksum"
pattern DoDiskSnapshotChecksum = Flag True
pattern NoDoDiskSnapshotChecksum = Flag False

-- | Snapshots policy
--
-- We only write ledger states that are older than @k@ blocks to disk (that is,
-- snapshots that are guaranteed valid). The on-disk policy determines how often
-- we write to disk and how many checkpoints we keep.
data SnapshotPolicy = SnapshotPolicy
  { onDiskNumSnapshots :: Word
  -- ^ How many snapshots do we want to keep on disk?
  --
  -- A higher number of on-disk snapshots is primarily a safe-guard against
  -- disk corruption: it trades disk space for reliability.
  --
  -- Examples:
  --
  -- * @0@: Delete the snapshot immediately after writing.
  --        Probably not a useful value :-D
  -- * @1@: Delete the previous snapshot immediately after writing the next
  --        Dangerous policy: if for some reason the deletion happens before
  --        the new snapshot is written entirely to disk (we don't @fsync@),
  --        we have no choice but to start at the genesis snapshot on the
  --        next startup.
  -- * @2@: Always keep 2 snapshots around. This means that when we write
  --        the next snapshot, we delete the oldest one, leaving the middle
  --        one available in case of truncation of the write. This is
  --        probably a sane value in most circumstances.
  , onDiskShouldTakeSnapshot :: Maybe DiffTime -> Word64 -> Bool
  -- ^ Should we write a snapshot of the ledger state to disk?
  --
  -- This function is passed two bits of information:
  --
  -- * The time since the last snapshot, or 'NoSnapshotTakenYet' if none was taken yet.
  --   Note that 'NoSnapshotTakenYet' merely means no snapshot had been taking yet
  --   since the node was started; it does not necessarily mean that none
  --   exist on disk.
  --
  -- * The distance in terms of blocks applied to the /oldest/ ledger
  --   snapshot in memory. During normal operation, this is the number of
  --   blocks written to the ImmutableDB since the last snapshot. On
  --   startup, it is computed by counting how many immutable blocks we had
  --   to reapply to get to the chain tip. This is useful, as it allows the
  --   policy to decide to take a snapshot /on node startup/ if a lot of
  --   blocks had to be replayed.
  --
  -- See also 'defaultSnapshotPolicy'
  }
  deriving NoThunks via OnlyCheckWhnf SnapshotPolicy

data SnapshotPolicyArgs = SnapshotPolicyArgs
  { spaInterval :: !SnapshotInterval
  , spaNum :: !NumOfDiskSnapshots
  }

defaultSnapshotPolicyArgs :: SnapshotPolicyArgs
defaultSnapshotPolicyArgs =
  SnapshotPolicyArgs
    DefaultSnapshotInterval
    DefaultNumOfDiskSnapshots

-- | Default on-disk policy suitable to use with cardano-node
defaultSnapshotPolicy ::
  SecurityParam ->
  SnapshotPolicyArgs ->
  SnapshotPolicy
defaultSnapshotPolicy
  (SecurityParam k)
  (SnapshotPolicyArgs requestedInterval reqNumOfSnapshots) =
    SnapshotPolicy
      { onDiskNumSnapshots
      , onDiskShouldTakeSnapshot
      }
   where
    onDiskNumSnapshots :: Word
    onDiskNumSnapshots = case reqNumOfSnapshots of
      DefaultNumOfDiskSnapshots -> 2
      RequestedNumOfDiskSnapshots value -> value

    onDiskShouldTakeSnapshot ::
      Maybe DiffTime ->
      Word64 ->
      Bool
    onDiskShouldTakeSnapshot Nothing blocksSinceLast =
      -- If users never leave their wallet running for long, this would mean
      -- that under some circumstances we would never take a snapshot
      -- So, on startup (when the 'time since the last snapshot' is `Nothing`),
      -- we take a snapshot as soon as there are @k@ blocks replayed.
      -- This means that even if users frequently shut down their wallet, we still
      -- take a snapshot roughly every @k@ blocks. It does mean the possibility of
      -- an extra unnecessary snapshot during syncing (if the node is restarted), but
      -- that is not a big deal.
      blocksSinceLast >= unNonZero k
    onDiskShouldTakeSnapshot (Just timeSinceLast) blocksSinceLast =
      snapshotInterval timeSinceLast
        || substantialAmountOfBlocksWereProcessed blocksSinceLast timeSinceLast

    -- \| We want to create a snapshot after a substantial amount of blocks were
    -- processed (hard-coded to 50k blocks). Given the fact that during bootstrap
    -- a fresh node will see a lot of blocks over a short period of time, we want
    -- to limit this condition to happen not more often then a fixed amount of
    -- time (here hard-coded to 6 minutes)
    substantialAmountOfBlocksWereProcessed blocksSinceLast timeSinceLast =
      let minBlocksBeforeSnapshot = 50_000
          minTimeBeforeSnapshot = 6 * secondsToDiffTime 60
       in blocksSinceLast >= minBlocksBeforeSnapshot
            && timeSinceLast >= minTimeBeforeSnapshot

    -- \| Requested snapshot interval can be explicitly provided by the
    -- caller (RequestedSnapshotInterval) or the caller might request the default
    -- snapshot interval (DefaultSnapshotInterval). If the latter then the
    -- snapshot interval is defaulted to k * 2 seconds - when @k = 2160@ the interval
    -- defaults to 72 minutes.
    snapshotInterval t = case requestedInterval of
      RequestedSnapshotInterval value -> t >= value
      DefaultSnapshotInterval -> t >= secondsToDiffTime (fromIntegral $ unNonZero k * 2)
      DisableSnapshots -> False

{-------------------------------------------------------------------------------
  Tracing snapshot events
-------------------------------------------------------------------------------}

data TraceSnapshotEvent blk
  = -- | An on disk snapshot was skipped because it was invalid.
    InvalidSnapshot DiskSnapshot (SnapshotFailure blk)
  | -- | A snapshot was written to disk.
    TookSnapshot DiskSnapshot (RealPoint blk) EnclosingTimed
  | -- | An old or invalid on-disk snapshot was deleted
    DeletedSnapshot DiskSnapshot
  deriving (Generic, Eq, Show)
