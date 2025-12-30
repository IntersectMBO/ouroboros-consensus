-- | Options related to the Consensus layer
module Cardano.Node.Configuration.File.Consensus
  ( ConsensusConfiguration (..)
  , GenesisConfigFlags (..)
  ) where

import Cardano.Ledger.BaseTypes
import Cardano.Node.Configuration.Basics
import Data.Aeson
import Data.Default
import Data.Time.Clock (DiffTime)
import GHC.Generics (Generic)

-- | In which mode should the node run.
data ConsensusConfiguration
  = PraosMode
  | GenesisMode GenesisConfigFlags
  deriving (Generic, Show)

instance FromJSON ConsensusConfiguration where
  parseJSON =
    withObject "ConsensusMode" $ \v -> do
      v .:? "ConsensusMode" >>= \case
        Just "GenesisMode" -> GenesisMode <$> parseJSON (Object v)
        Just "PraosMode" -> pure PraosMode
        Nothing -> pure def
        Just x -> fail $ "Unknown consensus mode: " <> x

instance Default ConsensusConfiguration where
  def = PraosMode

-- | Configuration options for Genesis parameters
data GenesisConfigFlags = GenesisConfigFlags
  { gcfEnableCSJ :: Override Bool
  , gcfEnableLoEAndGDD :: Override Bool
  , gcfEnableLoP :: Override Bool
  , gcfBlockFetchGracePeriod :: Override DiffTime
  , gcfBucketCapacity :: Override Integer
  , gcfBucketRate :: Override Integer
  , gcfCSJJumpSize :: Override SlotNo
  , gcfGDDRateLimit :: Override DiffTime
  }
  deriving (Generic, Show)

instance FromJSON GenesisConfigFlags where
  parseJSON = withObject "GenesisConfigFlags" $ \v ->
    GenesisConfigFlags
      <$> v .:= "EnableCSJ"
      <*> v .:= "EnableLoEAndGDD"
      <*> v .:= "EnableLoP"
      <*> v .:= "BlockFetchGracePeriod"
      <*> v .:= "BucketCapacity"
      <*> v .:= "BucketRate"
      <*> v .:= "CSJJumpSize"
      <*> v .:= "GDDRateLimit"
