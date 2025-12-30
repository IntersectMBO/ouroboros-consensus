{-# LANGUAGE GADTs #-}

-- | The representation of the configuration file
module Cardano.Node.Configuration.File
  ( -- * Configuration file
    NodeConfigurationFromFile
  , NodeConfigurationFromFileF (..)
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
import Data.Bifunctor (bimap)
import qualified Data.ByteString as BS
import Data.Functor.Identity (Identity (..))
import Data.String (fromString)
import Data.Yaml
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory, (</>))

-- | The configuration from the files, parsed with 'parseConfigurationFiles'
type NodeConfigurationFromFile = NodeConfigurationFromFileF Identity

-- | The configuration from the files, initially maybe pointing to sub-files and
-- finally fully parsed.
data NodeConfigurationFromFileF f
  = NodeConfigurationFromFileV1
  { storageConfiguration :: f (StorageConfiguration Maybe)
  , consensusConfiguration :: f ConsensusConfiguration
  , protocolConfiguration :: f (ProtocolConfiguration Maybe)
  , networkConfiguration :: f NetworkConfiguration
  , localConnectionsConfig :: LocalConnectionsConfig
  , tracingConfiguration :: f TracingConfiguration
  , testingConfiguration :: TestingConfiguration
  }
  deriving Generic

deriving instance Show (NodeConfigurationFromFileF (Either FilePath))
deriving instance Show (NodeConfigurationFromFileF Identity)

-- | Either an action to parse another file, or directly a value
data SubFileOrValue a
  = SubFile (IO (Either String a))
  | AValue a

-- | If the consulted key is a filepath, then prepare an action to parse that
-- other file.
subFileParser ::
  FilePath ->
  String ->
  (Value -> Parser a) ->
  Value ->
  Parser (SubFileOrValue a)
subFileParser root sectionName p v =
  withObject
    ("Config file: " <> sectionName)
    ( ( \v' -> do
          fn <- v' .: fromString sectionName
          pure $
            SubFile $ do
              exists <- doesFileExist $ root </> fn
              if exists
                then
                  parseEither p
                    . either (\err -> error $ "Invalid JSON: " <> show err) id
                    . decodeEither'
                    <$> BS.readFile (root </> fn)
                else pure $ parseEither p v
      )
    )
    v
    <|> (AValue <$> p v)

parseSubFile :: SubFileOrValue a -> IO (Either String (Identity a))
parseSubFile (AValue v) = pure $ Right $ Identity v
parseSubFile (SubFile f) = fmap Identity <$> f

parseConfigurationVersion1 ::
  FilePath ->
  Value ->
  Parser (NodeConfigurationFromFileF SubFileOrValue)
parseConfigurationVersion1 root v =
  NodeConfigurationFromFileV1
    <$> subFileParser root "Storage" parseStorageConfiguration v
    <*> subFileParser root "Consensus" parseJSON v
    <*> subFileParser root "Protocol" parseJSON v
    <*> subFileParser root "Network" parseJSON v
    <*> parseJSON v
    <*> subFileParser root "Tracing" parseJSON v
    <*> parseJSON v

-- | Parse the configuration file, but do not parse the children files
-- referenced from it yet.
parseConfigurationFile ::
  FilePath -> Value -> Parser (NodeConfigurationFromFileF SubFileOrValue)
parseConfigurationFile root v = do
  configVersion <- withObject "Configuration" (.:? "ConfigurationVersion") v
  case configVersion :: Maybe Int of
    Nothing -> parseConfigurationVersion1 root v
    Just 1 -> parseConfigurationVersion1 root v
    _ -> fail $ "Unknown configuration version: " <> show configVersion

-- | Parse the configuration file and parse any other children configuration
-- files referenced from it.
parseConfigurationFiles :: FilePath -> IO (Either String NodeConfigurationFromFile)
parseConfigurationFiles cfgFile = runExceptT $ do
  bs <- lift $ BS.readFile cfgFile
  cfg <- liftEither $ do
    v <- bimap show id (decodeEither' bs)
    parseEither (parseConfigurationFile (takeDirectory cfgFile)) v
  NodeConfigurationFromFileV1
    <$> ExceptT (parseSubFile (storageConfiguration cfg))
    <*> ExceptT (parseSubFile (consensusConfiguration cfg))
    <*> ExceptT (parseSubFile (protocolConfiguration cfg))
    <*> ExceptT (parseSubFile (networkConfiguration cfg))
    <*> pure (localConnectionsConfig cfg)
    <*> ExceptT (parseSubFile (tracingConfiguration cfg))
    <*> pure (testingConfiguration cfg)
