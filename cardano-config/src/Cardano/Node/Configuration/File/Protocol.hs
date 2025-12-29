{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Node.Configuration.File.Protocol where

import Cardano.Crypto (RequiresNetworkMagic (..))
import Cardano.Crypto.Hash
import Cardano.Ledger.Alonzo
import Cardano.Ledger.Conway
import Cardano.Ledger.Core hiding (Value)
import Cardano.Ledger.Dijkstra
import Cardano.Ledger.Shelley
import Cardano.Node.Configuration.Basics
import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Strict
import Data.String (fromString)
import Data.Word
import GHC.Generics
import Prelude hiding (FilePath)

data Hashed a = Hashed
  { hashed :: a
  , hash :: Maybe (Hash Blake2b_256 ByteString)
  }
  deriving (Generic, Show)

newtype EraGenesis era = EraGenesis (Hashed (FilePath "Genesis"))
  deriving (Generic, Show)

parseEraGenesis :: forall era. Era era => Value -> Parser (EraGenesis era)
parseEraGenesis =
  withObject "Configuration" $ \v ->
    fmap EraGenesis . Hashed
      <$> v .: (fromString (eraName @era) <> "GenesisFile")
      <*> v .:? (fromString (eraName @era) <> "GenesisHash")

data ByronGenesisConfiguration = ByronGenesisConfiguration
  { npcByronGenesisFile :: !(Hashed (FilePath "ByronGenesis"))
  , npcByronReqNetworkMagic :: !RequiresNetworkMagic
  , npcByronPbftSignatureThresh :: !(Maybe Double)
  , npcByronSupportedProtocolVersionMajor :: !Word16
  , npcByronSupportedProtocolVersionMinor :: !Word16
  , npcByronSupportedProtocolVersionAlt :: !Word8
  }
  deriving (Generic, Show)

instance FromJSON ByronGenesisConfiguration where
  parseJSON =
    withObject "Configuration" $ \v -> do
      npcByronGenesisFile <- Hashed <$> v .: "ByronGenesisFile" <*> v .:? "ByronGenesisHash"
      npcByronReqNetworkMagic <- v .:? "RequiresNetworkMagic" .!= RequiresNoMagic
      npcByronPbftSignatureThresh <- v .:? "PBftSignatureThreshold"
      protVerMajor <- v .: "LastKnownBlockVersion-Major"
      protVerMinor <- v .: "LastKnownBlockVersion-Minor"
      protVerAlt <- v .: "LastKnownBlockVersion-Alt" .!= 0

      pure
        ByronGenesisConfiguration
          { npcByronGenesisFile
          , npcByronReqNetworkMagic
          , npcByronPbftSignatureThresh
          , npcByronSupportedProtocolVersionMajor = protVerMajor
          , npcByronSupportedProtocolVersionMinor = protVerMinor
          , npcByronSupportedProtocolVersionAlt = protVerAlt
          }

type CardanoErasWithGenesis = '[ShelleyEra, AlonzoEra, ConwayEra]
type ExperimentalCardanoEra = DijkstraEra

data ProtocolConfiguration = ProtocolConfiguration
  { pncByronGenesis :: ByronGenesisConfiguration
  , pncGeneses :: !(NP EraGenesis CardanoErasWithGenesis)
  , pncStartAsNonProducingNode :: !Bool
  , checkpointsFile :: !(Maybe (Hashed (FilePath "Checkpoints")))
  }
  deriving (Generic, Show)

instance FromJSON ProtocolConfiguration where
  parseJSON =
    withObject "Configuration" $ \v ->
      ProtocolConfiguration
        <$> parseJSON (Object v)
        <*> hsequence' (hcpure (Proxy @Era) (Comp $ parseEraGenesis (Object v)))
        <*> v .:? "StartAsNonProducingNode" .!= False
        <*> ((fmap Just . Hashed <$> v .: "CheckpointsFile" <*> v .:? "CheckpointsFileHash") <|> pure Nothing)
