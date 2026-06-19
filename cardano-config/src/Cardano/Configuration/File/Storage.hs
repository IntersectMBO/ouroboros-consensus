-- | Options related to storage
module Cardano.Configuration.File.Storage
  ( adjustDbPath
  , StorageConfiguration (..)

    -- * LedgerDB
  , LedgerDbConfiguration (..)

    -- ** Snapshots
  , SnapshotPolicy (..)
  , SnapshotOptions (..)

    -- ** Backend
  , LedgerDbBackendSelector (..)
  ) where

import Cardano.Configuration.Common
import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Data.Word
import GHC.Generics

-- | An explicit set of snapshot policy options. All fields are optional; when
-- unset the node applies its own defaults.
data SnapshotOptions = SnapshotOptions
  { snapshotInterval :: Maybe Word64
  -- ^ How many slots between attempts to write a snapshot to disk (non-zero).
  , slotOffset :: Maybe Word64
  -- ^ The slot at which the snapshot schedule is anchored: snapshots are taken
  -- at @slotOffset + n * snapshotInterval@.
  , snapshotRateLimit :: Maybe Word64
  -- ^ The minimum wall-clock time, in seconds, between two snapshots.
  , minDelay :: Maybe Word64
  -- ^ Lower bound, in seconds, of the random delay before taking a snapshot.
  , maxDelay :: Maybe Word64
  -- ^ Upper bound, in seconds, of the random delay before taking a snapshot.
  , numOfDiskSnapshots :: Maybe Word64
  -- ^ How many snapshots the node should keep on disk.
  }
  deriving (Generic, Show)

-- | Parse the snapshot interval, failing on a non-positive value as the node
-- does.
parseSnapshotInterval :: Object -> Parser (Maybe Word64)
parseSnapshotInterval v = do
  interval <- v .:? "SnapshotInterval"
  case interval of
    Just 0 -> fail "Non-positive SnapshotInterval: 0"
    _ -> pure interval

instance FromJSON SnapshotOptions where
  parseJSON = withObject "SnapshotOptions" $ \v -> do
    interval <- parseSnapshotInterval v
    offset <- v .:? "SlotOffset"
    rateLimit <- v .:? "RateLimit"
    lo <- v .:? "MinDelay"
    hi <- v .:? "MaxDelay"
    num <- v .:? "NumOfDiskSnapshots"
    case (lo, hi) of
      (Just l, Just h)
        | l > h ->
            fail $ "Invalid snapshot delay range, MinDelay > MaxDelay: " <> show l <> " > " <> show h
      _ -> pure ()
    pure
      SnapshotOptions
        { snapshotInterval = interval
        , slotOffset = offset
        , snapshotRateLimit = rateLimit
        , minDelay = lo
        , maxDelay = hi
        , numOfDiskSnapshots = num
        }

-- | The snapshot policy: either a named, predefined policy (e.g. @"Mithril"@)
-- or an explicit set of options.
data SnapshotPolicy
  = NamedSnapshotPolicy String
  | CustomSnapshotPolicy SnapshotOptions
  deriving (Generic, Show)

instance FromJSON SnapshotPolicy where
  parseJSON v =
    NamedSnapshotPolicy <$> parseJSON v
      <|> CustomSnapshotPolicy <$> parseJSON v

-- | Selector for the backend that keeps track of differences in the UTxO set.
data LedgerDbBackendSelector
  = -- | The in-memory backend.
    V2InMemory
  | -- | The LSM-tree backend. The first field is an optional custom path to the
    -- database (the @LSMDatabasePath@ key); if it is not provided, the default
    -- is used. The second field is an optional directory into which the backend
    -- exports snapshots as it takes them (the @LSMExportPath@ key). Both are
    -- only meaningful for the LSM backend.
    V2LSM (Maybe FilePath) (Maybe FilePath)
  deriving (Generic, Show)

instance Default LedgerDbBackendSelector where
  def = V2InMemory

parseLedgerDbBackend :: Object -> Parser LedgerDbBackendSelector
parseLedgerDbBackend v = do
  backend <- v .:? "Backend" .!= "V2InMemory"
  case backend :: String of
    "V2InMemory" -> pure V2InMemory
    "V2LSM" -> V2LSM <$> v .:? "LSMDatabasePath" <*> v .:? "LSMExportPath"
    x -> fail $ "Malformed LedgerDB Backend: " <> x

-- | The Ledger DB configuration
data LedgerDbConfiguration = LedgerDbConfiguration
  { snapshots :: Maybe SnapshotPolicy
  , -- | When reading a big amount of data from the backend (for example by
    -- QueryUTxOByAddress), do it in chunks of what size.
    queryBatchSize :: Maybe Word64
  , backendSelector :: LedgerDbBackendSelector
  }
  deriving (Generic, Show)

instance Default LedgerDbConfiguration where
  def = LedgerDbConfiguration Nothing Nothing def

instance FromJSON LedgerDbConfiguration where
  parseJSON = withObject "LedgerDB" $ \v ->
    LedgerDbConfiguration
      <$> v .:? "Snapshots"
      <*> v .:? "QueryBatchSize"
      <*> parseLedgerDbBackend v

-- | Finally resolve the storage configuration with a final 'NodeDatabasePaths'.
adjustDbPath :: StorageConfiguration Maybe -> NodeDatabasePaths -> StorageConfiguration Identity
adjustDbPath sc db =
  sc
    { databasePath = Identity db
    , ledgerDbConfiguration = Identity $ fromMaybe def $ ledgerDbConfiguration sc
    }

-- | The storage configuration
data StorageConfiguration f = StorageConfiguration
  { databasePath :: f NodeDatabasePaths
  , ledgerDbConfiguration :: f LedgerDbConfiguration
  }
  deriving Generic

deriving instance Show (StorageConfiguration Maybe)
deriving instance Show (StorageConfiguration Identity)

instance FromJSON (StorageConfiguration Maybe) where
  parseJSON =
    withObject "StorageConfiguration" $ \v ->
      StorageConfiguration
        <$> v .:? "DatabasePath"
        <*> v .:? "LedgerDB"
