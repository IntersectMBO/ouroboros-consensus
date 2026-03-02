-- | Values related to testing, which are unused by a real node
module Cardano.Configuration.File.Testing
  ( MempoolCapacityBytes (..)
  , TestingConfiguration (..)
  ) where

import Cardano.Configuration.Basics
import Cardano.Configuration.File.Protocol
import Data.Aeson
import Data.Word
import GHC.Generics (Generic)

-- | Overriding the maximum size of the mempool
newtype MempoolCapacityBytes = MempoolCapacityBytes Word64
  deriving (Generic, Show)
  deriving newtype FromJSON

-- | The testing configuration
data TestingConfiguration = TestingConfiguration
  { pncMaybeMempoolCapacityMaybe :: !(Maybe MempoolCapacityBytes)
  , pncExperimentalGenesis :: Maybe (EraGenesis ExperimentalCardanoEra)
  }
  deriving (Generic, Show)

instance FromJSON TestingConfiguration where
  parseJSON =
    withObject "Configuration" $ \v ->
      TestingConfiguration
        <$> v .:= "MempoolCapacityBytesMaybe"
        <*> ( do
                enabled <- v .:? "ExperimentalHardForksEnabled" .!= False
                if enabled
                  then Just <$> parseEraGenesis (Object v)
                  else pure Nothing
            )
