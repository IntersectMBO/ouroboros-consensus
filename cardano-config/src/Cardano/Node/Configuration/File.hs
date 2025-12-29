{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Node.Configuration.File
  ( -- * Configuration file
    NodeConfiguration (..)
  , parseConfigurationFiles

    -- * Specific components configurations
  , StorageConfiguration (..)
  , ConsensusConfiguration (..)
  , ProtocolConfiguration (..)
  , NetworkConfiguration (..)
  , LocalConnectionsConfig (..)
  , TracingConfiguration (..)
  , TestingConfiguration (..)
  ) where

import Cardano.Node.Configuration.File.Consensus
import Cardano.Node.Configuration.File.Network
import Cardano.Node.Configuration.File.Protocol
import Cardano.Node.Configuration.File.Storage
import Cardano.Node.Configuration.File.Testing
import Cardano.Node.Configuration.File.Tracing
import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.Trans (lift)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import Data.Functor.Identity (Identity (..))
import Data.String (fromString)
import Debug.Trace
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))

data NodeConfiguration f
  = NodeConfigurationV1
  { storageConfiguration :: f StorageConfiguration
  , consensusConfiguration :: f ConsensusConfiguration
  , protocolConfiguration :: f ProtocolConfiguration
  , networkConfiguration :: f NetworkConfiguration
  , localConnectionsConfig :: LocalConnectionsConfig
  , tracingConfiguration :: f TracingConfiguration
  , testingConfiguration :: TestingConfiguration
  }
  deriving Generic

deriving instance Show (NodeConfiguration (Either FilePath))
deriving instance Show (NodeConfiguration Identity)

data SubFileOrValue a
  = SubFile (IO (Either String a))
  | AValue a

subFileParser ::
  FilePath ->
  String ->
  (Value -> Parser a) ->
  Value ->
  Parser (SubFileOrValue a)
subFileParser root sectionName p v =
  withObject
    "Config file"
    ( ( \v' -> do
          fn <- v' .: fromString sectionName
          pure $
            SubFile $ do
              exists <- doesFileExist $ root </> fn
              if exists
                then
                  parseEither p
                    . either (\err -> error $ traceShowId $ "Invalid JSON: " <> err) id
                    . eitherDecodeStrict
                    <$> BS.readFile (traceShowId $ root </> fn)
                else pure $ parseEither p v
      )
    )
    v
    <|> (AValue <$> p v)

parseSubFile :: SubFileOrValue a -> IO (Either String (Identity a))
parseSubFile (AValue v) = pure $ Right $ Identity v
parseSubFile (SubFile f) = fmap Identity <$> trace "running" f

parseConfigurationVersion1 ::
  FilePath ->
  Value ->
  Parser (NodeConfiguration SubFileOrValue)
parseConfigurationVersion1 root v =
  NodeConfigurationV1
    <$> subFileParser root "Storage" parseStorageConfiguration v
    <*> subFileParser root "Consensus" parseJSON v
    <*> subFileParser root "Protocol" parseJSON v
    <*> subFileParser root "Network" parseJSON v
    <*> parseJSON v
    <*> subFileParser root "Tracing" parseJSON v
    <*> parseJSON v

parseConfigurationFile ::
  FilePath -> Value -> Parser (NodeConfiguration SubFileOrValue)
parseConfigurationFile root v = do
  configVersion <- withObject "Configuration" (.:? "ConfigurationVersion") v
  case configVersion :: Maybe Int of
    Nothing -> parseConfigurationVersion1 root v
    Just 1 -> parseConfigurationVersion1 root v
    _ -> fail $ "Unknown configuration version: " <> show configVersion

parseConfigurationFiles :: FilePath -> IO (Either String (NodeConfiguration Identity))
parseConfigurationFiles cfgFile = runExceptT $ do
  bs <- lift $ BS.readFile cfgFile
  cfg <- liftEither $ do
    v <- eitherDecodeStrict bs
    parseEither (parseConfigurationFile (takeDirectory cfgFile)) v
  NodeConfigurationV1
    <$> ExceptT (parseSubFile (storageConfiguration cfg))
    <*> ExceptT (parseSubFile (consensusConfiguration cfg))
    <*> ExceptT (parseSubFile (protocolConfiguration cfg))
    <*> ExceptT (parseSubFile (networkConfiguration cfg))
    <*> pure (localConnectionsConfig cfg)
    <*> ExceptT (parseSubFile (tracingConfiguration cfg))
    <*> pure (testingConfiguration cfg)
