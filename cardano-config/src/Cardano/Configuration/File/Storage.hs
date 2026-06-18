-- | Options related to storage
module Cardano.Configuration.File.Storage
  ( adjustDbPath
  , StorageConfiguration (..)

    -- * LedgerDB
  , LedgerDbConfiguration (..)

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

-- | Selector for the backend that keeps track of differences in the UTxO set.
data LedgerDbBackendSelector
  = -- | The in-memory backend.
    V2InMemory
  | -- | The LSM-tree backend, with an optional custom path to the database. If
    -- it is not provided, the default is used.
    V2LSM (Maybe FilePath)
  deriving (Generic, Show)

instance Default LedgerDbBackendSelector where
  def = V2InMemory

-- | The Ledger DB configuration
data LedgerDbConfiguration = LedgerDbConfiguration
  { -- | How many slots between attempts to write a snapshot to disk. Must be
    -- non-zero.
    snapshotInterval :: Maybe Word64
  , -- | The slot at which the snapshot schedule is anchored: snapshots are
    -- taken at @slotOffset + n * snapshotInterval@.
    slotOffset :: Maybe Word64
  , -- | The minimum wall-clock time, in seconds, that must elapse between two
    -- snapshots.
    snapshotRateLimit :: Maybe Word64
  , -- | How many snapshots should the node keep on the disk.
    numOfDiskSnapshots :: Maybe Word64
  , -- | When reading a big amount of data from the backend (for example by
    -- QueryUTxOByAddress), do it in chunks of what size.
    queryBatchSize :: Maybe Word64
  , backendSelector :: LedgerDbBackendSelector
  }
  deriving (Generic, Show)

instance Default LedgerDbConfiguration where
  def = LedgerDbConfiguration Nothing Nothing Nothing Nothing Nothing def

-- | Finally resolve the storage configuration with a final 'NodeDatabasePaths'.
adjustDbPath :: StorageConfiguration Maybe -> NodeDatabasePaths -> StorageConfiguration Identity
adjustDbPath sc db =
  sc
    { databasePath = Identity db
    , ledgerDbConfiguration = Identity $ fromMaybe def $ ledgerDbConfiguration sc
    }

-- | Parse the snapshot interval, failing on a non-positive value as the node
-- does.
parseSnapshotInterval :: Object -> Parser (Maybe Word64)
parseSnapshotInterval v = do
  interval <- v .:? "SnapshotInterval"
  case interval of
    Just 0 -> fail "Non-positive SnapshotInterval: 0"
    _ -> pure interval

parseLedgerDbBackend :: Object -> Parser LedgerDbBackendSelector
parseLedgerDbBackend v = do
  backend <- v .:? "Backend" .!= "V2InMemory"
  case backend :: String of
    "V2InMemory" -> pure V2InMemory
    "V2LSM" -> V2LSM <$> v .:? "LSMDatabasePath"
    x -> fail $ "Malformed LedgerDB Backend: " <> x

-- | Parse the nested @LedgerDB@ object. The top-level (deprecated)
-- @SnapshotInterval@ and @NumOfDiskSnapshots@ are passed in as fallbacks; the
-- nested values take precedence, matching the node.
parseLedgerDbConfig ::
  Maybe Word64 ->
  Maybe Word64 ->
  Value ->
  Parser LedgerDbConfiguration
parseLedgerDbConfig topInterval topNum =
  withObject "LedgerDB" $ \v -> do
    interval <- (<|> topInterval) <$> parseSnapshotInterval v
    num <- (<|> topNum) <$> v .:? "NumOfDiskSnapshots"
    offset <- v .:? "SlotOffset"
    rateLimit <- v .:? "RateLimit"
    qsize <- v .:? "QueryBatchSize"
    selector <- parseLedgerDbBackend v
    pure
      LedgerDbConfiguration
        { snapshotInterval = interval
        , slotOffset = offset
        , snapshotRateLimit = rateLimit
        , numOfDiskSnapshots = num
        , queryBatchSize = qsize
        , backendSelector = selector
        }

parseLedgerDbConfiguration :: Object -> Parser (Maybe LedgerDbConfiguration)
parseLedgerDbConfiguration v = do
  topInterval <- parseSnapshotInterval v
  topNum <- v .:? "NumOfDiskSnapshots"
  v .:? "LedgerDB" >>= \case
    Nothing
      | Nothing <- topInterval
      , Nothing <- topNum ->
          pure Nothing
      | otherwise ->
          pure $ Just def{snapshotInterval = topInterval, numOfDiskSnapshots = topNum}
    Just ldb -> Just <$> parseLedgerDbConfig topInterval topNum ldb

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
    withObject "StorageConfiguration" $ \v -> do
      dbPath <- v .:? "DatabasePath"
      ldb <- parseLedgerDbConfiguration v
      pure $ StorageConfiguration dbPath ldb
