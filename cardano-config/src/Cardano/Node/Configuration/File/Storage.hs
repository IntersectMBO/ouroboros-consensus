{-# OPTIONS_GHC -Wno-orphans #-}

-- | Options related to storage
module Cardano.Node.Configuration.File.Storage
  ( adjustDbPath
  , parseStorageConfiguration
  , StorageConfiguration (..)

    -- * LedgerDB
  , LedgerDbConfiguration (..)
  , NumOfDiskSnapshots (..)
  , SnapshotInterval (..)
  , QueryBatchSize (..)

    -- ** Backend
  , LedgerDbBackendSelector (..)

    -- *** LMDB
  , FlushFrequency (..)
  , MaxMapSize (..)
  , MaxReaders (..)
  ) where

import Cardano.Node.Configuration.Basics
import Cardano.Node.Configuration.Common
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default
import Data.Functor.Identity
import Data.Word
import Debug.Trace (trace)
import GHC.Generics
import Prelude hiding (FilePath)

-- | How often should we flush differences into the LMDB backend. In number of
-- blocks.
newtype FlushFrequency = FlushFrequency Word64
  deriving (Generic, Show)
  deriving newtype FromJSON

-- | How big should the LMDB file grow. If this value is reached, restarting the
-- node with a larger value should suffice.
newtype MaxMapSize = MaxMapSize Word64
  deriving (Generic, Show)
  deriving newtype FromJSON

-- | Maximum number of readers allowed to access the LMDB database.
newtype MaxReaders = MaxReaders Word64
  deriving (Generic, Show)
  deriving newtype FromJSON

-- | Selector for the backend
data LedgerDbBackendSelector f
  = V1LMDB
      (Override FlushFrequency)
      (f (FilePath "LMDB"))
      (Override MaxMapSize)
      (Override MaxReaders)
  | V2InMemory
  | V2LSM (f (FilePath "LSM"))
  deriving Generic

deriving instance Show (LedgerDbBackendSelector Maybe)
deriving instance Show (LedgerDbBackendSelector Identity)

-- | How many snapshots should the node keep on the disk
newtype NumOfDiskSnapshots = NumOfDiskSnapshots Word64
  deriving (Generic, Show)
  deriving newtype FromJSON

-- | How many seconds must pass between snapshots
newtype SnapshotInterval = SnapshotInterval Word64
  deriving (Generic, Show)
  deriving newtype FromJSON

-- | When reading a big amount of data from the backend (for example by
-- QueryUTxOByAddress), do it in chunks of what size.
newtype QueryBatchSize = QueryBatchSize Word64
  deriving (Generic, Show)
  deriving newtype FromJSON

-- | The Ledger DB configuration
data LedgerDbConfiguration f
  = LedgerDbConfiguration
  { numOfDiskSnapshots :: Override NumOfDiskSnapshots
  , snapshotInterval :: Override SnapshotInterval
  , queryBatchSize :: Override QueryBatchSize
  , backendSelector :: LedgerDbBackendSelector f
  }
  deriving Generic

deriving instance Show (LedgerDbConfiguration Maybe)
deriving instance Show (LedgerDbConfiguration Identity)

resolvePaths :: LedgerDbConfiguration Maybe -> NodeDatabasePaths -> LedgerDbConfiguration Identity
resolvePaths ldb@(LedgerDbConfiguration _ _ _ (V1LMDB d Nothing e f)) ndb =
  ldb{backendSelector = V1LMDB d (Identity (defGiven ndb)) e f}
resolvePaths ldb@(LedgerDbConfiguration _ _ _ (V2LSM Nothing)) ndb =
  ldb{backendSelector = V2LSM (Identity (defGiven ndb))}
resolvePaths ldb@(LedgerDbConfiguration _ _ _ (V1LMDB d (Just e) f g)) _ =
  ldb{backendSelector = V1LMDB d (Identity e) f g}
resolvePaths ldb@(LedgerDbConfiguration _ _ _ (V2LSM (Just a))) _ =
  ldb{backendSelector = V2LSM (Identity a)}
resolvePaths ldb@(LedgerDbConfiguration _ _ _ V2InMemory) _ =
  ldb{backendSelector = V2InMemory}

-- | Finally resolve the LedgerDB configuration with a final NodeDatabasePaths
adjustDbPath :: StorageConfiguration Maybe -> NodeDatabasePaths -> StorageConfiguration Identity
adjustDbPath sc db =
  sc
    { databasePath = Identity db
    , ledgerDbConfiguration = resolvePaths (ledgerDbConfiguration sc) db
    }

instance Default (RelativeFilePath "LMDB") where
  def = RelativeFilePath "lmdb"

instance Default (RelativeFilePath "LSM") where
  def = RelativeFilePath "lsm"

instance Default (LedgerDbBackendSelector f) where
  def = V2InMemory

instance Default (LedgerDbConfiguration f) where
  def = LedgerDbConfiguration def def def def

class DefaultGiven given a where
  defGiven :: given -> a

instance DefaultGiven NodeDatabasePaths (FilePath "LMDB") where
  defGiven (Unique fp) = fp `anchorRelativePath` def
  defGiven (Split _ fp) = fp `anchorRelativePath` def

instance DefaultGiven NodeDatabasePaths (FilePath "LSM") where
  defGiven (Unique fp) = fp `anchorRelativePath` def
  defGiven (Split _ fp) = fp `anchorRelativePath` def

instance
  (Default (RelativeFilePath a), DefaultGiven b (FilePath a)) =>
  DefaultGiven (Maybe b) (Maybe (FilePath a))
  where
  defGiven Nothing = Nothing
  defGiven (Just ndb) = Just $ defGiven ndb

parseLedgerDbConfig :: Maybe NodeDatabasePaths -> Value -> Parser (LedgerDbConfiguration Maybe)
parseLedgerDbConfig dbPath =
  withObject "LedgerDB" $ \v -> do
    trace "Parsing" $
      LedgerDbConfiguration
        <$> v .:= "NumOfDiskSnapshots"
        <*> v .:= "SnapshotInterval"
        <*> v .:= "QueryBatchSize"
        <*> parseLedgerDbBackend dbPath (Object v)

parseLedgerDbBackend :: Maybe NodeDatabasePaths -> Value -> Parser (LedgerDbBackendSelector Maybe)
parseLedgerDbBackend dbPath =
  withObject "LedgerDB" $ \v -> do
    bknd <- v .: "Backend" :: Parser String
    case bknd of
      "V2InMemory" ->
        pure V2InMemory
      "V1LMDB" -> do
        ff <- v .:= "FlushFrequency"
        ltp <- (fmap Just <$> v .:? "LiveTablesPath") .!= defGiven dbPath
        sz <- v .:= "MaxMapSize"
        rds <- v .:= "MaxReaders"
        pure $ V1LMDB ff ltp sz rds
      "V2LSM" ->
        V2LSM <$> (fmap Just <$> v .:? "LSMDatabasePath") .!= defGiven dbPath
      _ -> fail $ "Unknown backend: " <> show bknd

parseLedgerDbConfiguration ::
  Maybe NodeDatabasePaths -> Value -> Parser (LedgerDbConfiguration Maybe)
parseLedgerDbConfiguration dbPath =
  withObject "NodeConfiguration" $ \v ->
    maybe (pure def) (parseLedgerDbConfig dbPath)
      =<< v .:? "LedgerDB"

-- | The storage configuration
data StorageConfiguration f = StorageConfiguration
  { databasePath :: f NodeDatabasePaths
  , ledgerDbConfiguration :: LedgerDbConfiguration f
  }
  deriving Generic

deriving instance Show (StorageConfiguration Maybe)
deriving instance Show (StorageConfiguration Identity)

parseStorageConfiguration :: Value -> Parser (StorageConfiguration Maybe)
parseStorageConfiguration =
  withObject "StorageConfiguration" $ \v -> do
    dbPath <- v .:? "DatabasePath"
    ldb <- parseLedgerDbConfiguration dbPath (Object v)
    pure $ StorageConfiguration dbPath ldb
