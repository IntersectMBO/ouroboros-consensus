{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Node.Configuration.File.Consensus where

import Cardano.Ledger.BaseTypes
import Cardano.Node.Configuration.Basics
import Data.Aeson
import Data.Default
import Data.Time.Clock (DiffTime)
import GHC.Generics (Generic)

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
