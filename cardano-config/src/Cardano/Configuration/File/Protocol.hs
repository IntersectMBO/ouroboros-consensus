-- | Options related to the Cardano protocol
module Cardano.Configuration.File.Protocol
  ( -- * Configuration
    ProtocolConfiguration (..)

    -- * Hashed files
  , Hashed (..)
  , EraGenesis (..)
  , parseEraGenesis

    -- * Particular eras
  , ByronGenesisConfiguration (..)
  , CardanoErasWithGenesis
  , ExperimentalCardanoEra
  ) where

import Cardano.Configuration.Basics
import Cardano.Crypto (RequiresNetworkMagic (..))
import Cardano.Crypto.Hash
import Cardano.Ledger.Alonzo
import Cardano.Ledger.Conway
import Cardano.Ledger.Core hiding (Value)
import Cardano.Ledger.Dijkstra
import Cardano.Ledger.Shelley
import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types
import Data.Functor.Identity (Identity)
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Strict
import Data.String (fromString)
import Data.Word
import GHC.Generics

-- | A maybe hashed entity, possibly a file.
data Hashed a = Hashed
  { hashed :: a
  , hash :: Maybe (Hash Blake2b_256 ByteString)
  }
  deriving (Generic, Show)

newtype EraGenesis era = EraGenesis (Hashed (File "Genesis"))
  deriving (Generic, Show)

parseEraGenesis :: forall era. Era era => Value -> Parser (EraGenesis era)
parseEraGenesis =
  withObject "Configuration" $ \v ->
    fmap EraGenesis . Hashed
      <$> v .: (fromString (eraName @era) <> "GenesisFile")
      <*> v .:? (fromString (eraName @era) <> "GenesisHash")

-- | Configuration for byron era
data ByronGenesisConfiguration = ByronGenesisConfiguration
  { byronGenesisFile :: !(Hashed (File "ByronGenesis"))
  , byronReqNetworkMagic :: !RequiresNetworkMagic
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
      byronReqNetworkMagic <- v .:? "RequiresNetworkMagic" .!= RequiresNoMagic
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

-- | The eras in Cardano that require a Genesis
type CardanoErasWithGenesis = '[ShelleyEra, AlonzoEra, ConwayEra]

-- | The new upcoming era, still experimental
type ExperimentalCardanoEra = DijkstraEra

-- | Configuration for the protocol
data ProtocolConfiguration f = ProtocolConfiguration
  { byronGenesis :: ByronGenesisConfiguration
  , geneses :: !(NP EraGenesis CardanoErasWithGenesis)
  , startAsNonProducingNode :: !(f Bool)
  , checkpointsFile :: !(Maybe (Hashed (File "Checkpoints")))
  }
  deriving Generic

deriving instance Show (ProtocolConfiguration Maybe)
deriving instance Show (ProtocolConfiguration Identity)

instance FromJSON (ProtocolConfiguration Maybe) where
  parseJSON =
    withObject "Configuration" $ \v ->
      ProtocolConfiguration
        <$> parseJSON (Object v)
        <*> hsequence' (hcpure (Proxy @Era) (Comp $ parseEraGenesis (Object v)))
        <*> v .:? "StartAsNonProducingNode"
        <*> ((fmap Just . Hashed <$> v .: "CheckpointsFile" <*> v .:? "CheckpointsFileHash") <|> pure Nothing)
