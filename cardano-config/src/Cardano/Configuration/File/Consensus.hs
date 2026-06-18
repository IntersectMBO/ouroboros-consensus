-- | Options related to the Consensus layer
module Cardano.Configuration.File.Consensus
  ( ConsensusConfiguration (..)
  , GenesisConfigFlags (..)
  ) where

import Data.Aeson
import Data.Default
import Data.Functor.Identity (Identity)
import Data.Time.Clock (DiffTime)
import Data.Word
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
              -- The Genesis flags only apply in Genesis mode, so we read them
              -- (from the @LowLevelGenesisOptions@ key) only here.
              Just "GenesisMode" -> Just . GenesisMode <$> v .:? "LowLevelGenesisOptions" .!= def
              Just "PraosMode" -> pure $ Just PraosMode
              Nothing -> pure Nothing
              Just x -> fail $ "Unknown consensus mode: " <> x
        )
        val

-- | Configuration options for Genesis parameters
data GenesisConfigFlags = GenesisConfigFlags
  { gcfEnableCSJ :: Bool
  , gcfEnableLoEAndGDD :: Bool
  , gcfEnableLoP :: Bool
  , gcfBlockFetchGracePeriod :: Maybe DiffTime
  , gcfBucketCapacity :: Maybe Integer
  , gcfBucketRate :: Maybe Integer
  , gcfCSJJumpSize :: Maybe Word64
  , gcfGDDRateLimit :: Maybe DiffTime
  }
  deriving (Generic, Show)

instance Default GenesisConfigFlags where
  def = GenesisConfigFlags True True True Nothing Nothing Nothing Nothing Nothing

instance FromJSON GenesisConfigFlags where
  parseJSON = withObject "GenesisConfigFlags" $ \v ->
    GenesisConfigFlags
      <$> v .:? "EnableCSJ" .!= True
      <*> v .:? "EnableLoEAndGDD" .!= True
      <*> v .:? "EnableLoP" .!= True
      <*> v .:? "BlockFetchGracePeriod"
      <*> v .:? "BucketCapacity"
      <*> v .:? "BucketRate"
      <*> v .:? "CSJJumpSize"
      <*> v .:? "GDDRateLimit"
