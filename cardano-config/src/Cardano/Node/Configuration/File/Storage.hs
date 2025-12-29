{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Node.Configuration.File.Storage where

import Cardano.Node.Configuration.Basics
import Cardano.Node.Configuration.Common
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default
import Data.Word
import Debug.Trace (trace)
import GHC.Generics
import Prelude hiding (FilePath)

newtype FlushFrequency = FlushFrequency Word64
  deriving (Generic, Show)
  deriving newtype FromJSON

newtype MaxMapSize = MaxMapSize Word64
  deriving (Generic, Show)
  deriving newtype FromJSON

newtype MaxReaders = MaxReaders Word64
  deriving (Generic, Show)
  deriving newtype FromJSON

data LedgerDbBackendSelector
  = V1LMDB
      (Override FlushFrequency)
      (Either (FilePath "LMDB") (RelativeFilePath "LMDB"))
      (Override MaxMapSize)
      (Override MaxReaders)
  | V2InMemory
  | V2LSM (Either (FilePath "LSM") (RelativeFilePath "LSM"))
  deriving (Generic, Show)

newtype NumOfDiskSnapshots = NumOfDiskSnapshots Word64
  deriving (Generic, Show)
  deriving newtype FromJSON

newtype SnapshotInterval = SnapshotInterval Word64
  deriving (Generic, Show)
  deriving newtype FromJSON

newtype QueryBatchSize = QueryBatchSize Word64
  deriving (Generic, Show)
  deriving newtype FromJSON

data LedgerDbConfiguration
  = LedgerDbConfiguration
  { numOfDiskSnapshots :: Override NumOfDiskSnapshots
  , snapshotInterval :: Override SnapshotInterval
  , queryBatchSize :: Override QueryBatchSize
  , backendSelector :: LedgerDbBackendSelector
  }
  deriving (Generic, Show)

instance Default (RelativeFilePath "LMDB") where
  def = RelativeFilePath "lmdb"

instance Default (RelativeFilePath "LSM") where
  def = RelativeFilePath "lsm"

instance Default LedgerDbBackendSelector where
  def = V2InMemory

instance Default LedgerDbConfiguration where
  def = LedgerDbConfiguration def def def def

instance DefaultGiven NodeDatabasePaths (FilePath "LMDB") where
  defGiven (Unique fp) = fp `anchorRelativePath` def
  defGiven (Split _ fp) = fp `anchorRelativePath` def

instance DefaultGiven NodeDatabasePaths (FilePath "LSM") where
  defGiven (Unique fp) = fp `anchorRelativePath` def
  defGiven (Split _ fp) = fp `anchorRelativePath` def

instance
  (Default (RelativeFilePath a), DefaultGiven b (FilePath a)) =>
  DefaultGiven (Maybe b) (Either (FilePath a) (RelativeFilePath a))
  where
  defGiven Nothing = Right def
  defGiven (Just ndb) = Left $ defGiven ndb

parseLedgerDbConfig :: Maybe NodeDatabasePaths -> Value -> Parser LedgerDbConfiguration
parseLedgerDbConfig dbPath =
  withObject "LedgerDB" $ \v -> do
    trace "Parsing" $
      LedgerDbConfiguration
        <$> v .:= "NumOfDiskSnapshots"
        <*> v .:= "SnapshotInterval"
        <*> v .:= "QueryBatchSize"
        <*> parseLedgerDbBackend dbPath (Object v)

parseLedgerDbBackend :: Maybe NodeDatabasePaths -> Value -> Parser LedgerDbBackendSelector
parseLedgerDbBackend dbPath =
  withObject "LedgerDB" $ \v -> do
    bknd <- v .: "Backend" :: Parser String
    case bknd of
      "V2InMemory" ->
        pure V2InMemory
      "V1LMDB" -> do
        ff <- v .:= "FlushFrequency"
        ltp <- (fmap Left <$> v .:? "LiveTablesPath") .!= defGiven dbPath
        sz <- v .:= "MaxMapSize"
        rds <- v .:= "MaxReaders"
        pure $ V1LMDB ff ltp sz rds
      "V2LSM" ->
        V2LSM <$> (fmap Left <$> v .:? "LSMDatabasePath") .!= defGiven dbPath
      _ -> fail $ "Unknown backend: " <> show bknd

parseLedgerDbConfiguration :: Maybe NodeDatabasePaths -> Value -> Parser LedgerDbConfiguration
parseLedgerDbConfiguration dbPath =
  withObject "NodeConfiguration" $ \v ->
    maybe (pure def) (parseLedgerDbConfig dbPath)
      =<< v .:? "LedgerDB"

data StorageConfiguration = StorageConfiguration
  { databasePath :: Maybe NodeDatabasePaths
  , ledgerDbConfiguration :: LedgerDbConfiguration
  }
  deriving (Generic, Show)

parseStorageConfiguration :: Value -> Parser StorageConfiguration
parseStorageConfiguration =
  withObject "StorageConfiguration" $ \v -> do
    dbPath <- v .:? "DatabasePath"
    ldb <- parseLedgerDbConfiguration dbPath (Object v)
    pure $ StorageConfiguration dbPath ldb
