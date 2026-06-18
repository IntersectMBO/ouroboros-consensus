-- | Options related to the Cardano protocol
module Cardano.Configuration.File.Protocol
  ( -- * Configuration
    ProtocolConfiguration (..)

    -- * Hashed files
  , Hashed (..)
  , parseEraGenesis

    -- * Particular eras
  , ByronGenesisConfiguration (..)
  ) where

import Cardano.Crypto.Hash
import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types
import Data.Functor.Identity (Identity)
import Data.String (fromString)
import Data.Word
import GHC.Generics

-- | A maybe hashed entity, possibly a file.
data Hashed a = Hashed
  { hashed :: a
  , hash :: Maybe (Hash Blake2b_256 ByteString)
  }
  deriving (Generic, Show)

-- | Parse the genesis file (and optional hash) for the era with the given key
-- prefix, e.g. @\"Shelley\"@ reads @ShelleyGenesisFile@ and @ShelleyGenesisHash@.
parseEraGenesis :: String -> Object -> Parser (Hashed FilePath)
parseEraGenesis era v =
  Hashed
    <$> v .: fromString (era <> "GenesisFile")
    <*> v .:? fromString (era <> "GenesisHash")

-- | Configuration for byron era
data ByronGenesisConfiguration = ByronGenesisConfiguration
  { byronGenesisFile :: !(Hashed FilePath)
  , byronReqNetworkMagic :: !String
  , byronPbftSignatureThresh :: !(Maybe Double)
  , byronSupportedProtocolVersionMajor :: !Word16
  , byronSupportedProtocolVersionMinor :: !Word16
  , byronSupportedProtocolVersionAlt :: !Word8
  }
  deriving (Generic, Show)

instance FromJSON ByronGenesisConfiguration where
  parseJSON =
    withObject "Configuration" $ \v -> do
      byronGenesisFile <- Hashed <$> v .: "ByronGenesisFile" <*> v .:? "ByronGenesisHash"
      byronReqNetworkMagic <- v .:? "RequiresNetworkMagic" .!= "RequiresNoMagic"
      byronPbftSignatureThresh <- v .:? "PBftSignatureThreshold"
      protVerMajor <- v .: "LastKnownBlockVersion-Major"
      protVerMinor <- v .: "LastKnownBlockVersion-Minor"
      protVerAlt <- v .: "LastKnownBlockVersion-Alt" .!= 0

      pure
        ByronGenesisConfiguration
          { byronGenesisFile
          , byronReqNetworkMagic
          , byronPbftSignatureThresh
          , byronSupportedProtocolVersionMajor = protVerMajor
          , byronSupportedProtocolVersionMinor = protVerMinor
          , byronSupportedProtocolVersionAlt = protVerAlt
          }

-- | Configuration for the protocol
data ProtocolConfiguration f = ProtocolConfiguration
  { byronGenesis :: ByronGenesisConfiguration
  , shelleyGenesis :: !(Hashed FilePath)
  , alonzoGenesis :: !(Hashed FilePath)
  , conwayGenesis :: !(Hashed FilePath)
  , startAsNonProducingNode :: !(f Bool)
  , checkpointsFile :: !(Maybe (Hashed FilePath))
  }
  deriving Generic

deriving instance Show (ProtocolConfiguration Maybe)
deriving instance Show (ProtocolConfiguration Identity)

instance FromJSON (ProtocolConfiguration Maybe) where
  parseJSON =
    withObject "Configuration" $ \v ->
      ProtocolConfiguration
        <$> parseJSON (Object v)
        <*> parseEraGenesis "Shelley" v
        <*> parseEraGenesis "Alonzo" v
        <*> parseEraGenesis "Conway" v
        <*> v .:? "StartAsNonProducingNode"
        <*> ((fmap Just . Hashed <$> v .: "CheckpointsFile" <*> v .:? "CheckpointsFileHash") <|> pure Nothing)
