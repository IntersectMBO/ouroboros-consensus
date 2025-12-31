-- | Options related to the Consensus layer
module Cardano.Configuration.File.Consensus
  ( ConsensusConfiguration (..)
  , GenesisConfigFlags (..)
  ) where

import Cardano.Configuration.Basics
import Cardano.Ledger.BaseTypes
import Data.Aeson
import Data.Default
import Data.Functor.Identity (Identity)
import Data.Time.Clock (DiffTime)
import GHC.Generics (Generic)

data ConsensusMode
  = PraosMode
  | GenesisMode GenesisConfigFlags
  deriving (Generic, Show)

instance Default ConsensusMode where
  def = PraosMode

-- | In which mode should the node run.
newtype ConsensusConfiguration f = ConsensusConfiguration {getConsensusConfiguration :: f ConsensusMode}

deriving instance Show (ConsensusConfiguration Maybe)
deriving instance Show (ConsensusConfiguration Identity)

instance FromJSON (ConsensusConfiguration Maybe) where
  parseJSON val =
    ConsensusConfiguration
      <$> withObject
        "ConsensusMode"
        ( \v -> do
            v .:? "ConsensusMode" >>= \case
              Just "GenesisMode" -> Just . GenesisMode <$> parseJSON val
              Just "PraosMode" -> pure $ Just PraosMode
              Nothing -> pure Nothing
              Just x -> fail $ "Unknown consensus mode: " <> x
        )
        val

-- | Configuration options for Genesis parameters
data GenesisConfigFlags = GenesisConfigFlags
  { gcfEnableCSJ :: Maybe Bool
  , gcfEnableLoEAndGDD :: Maybe Bool
  , gcfEnableLoP :: Maybe Bool
  , gcfBlockFetchGracePeriod :: Maybe DiffTime
  , gcfBucketCapacity :: Maybe Integer
  , gcfBucketRate :: Maybe Integer
  , gcfCSJJumpSize :: Maybe SlotNo
  , gcfGDDRateLimit :: Maybe DiffTime
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
