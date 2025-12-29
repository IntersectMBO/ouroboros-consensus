{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Node.Configuration.File.Testing where

import Cardano.Node.Configuration.Basics
import Cardano.Node.Configuration.File.Protocol
import Data.Aeson
import Data.Word
import GHC.Generics (Generic)

newtype MempoolCapacityBytes = MempoolCapacityBytes Word64
  deriving (Generic, Show)
  deriving newtype FromJSON

data TestingConfiguration = TestingConfiguration
  { pncMaybeMempoolCapacityOverride :: !(Override MempoolCapacityBytes)
  , pncExperimentalGenesis :: Maybe (EraGenesis ExperimentalCardanoEra)
  }
  deriving (Generic, Show)

instance FromJSON TestingConfiguration where
  parseJSON =
    withObject "Configuration" $ \v ->
      TestingConfiguration
        <$> v .:= "MempoolCapacityBytesOverride"
        <*> ( do
                enabled <- v .:? "ExperimentalHardForksEnabled" .!= False
                if enabled
                  then Just <$> parseEraGenesis (Object v)
                  else pure Nothing
            )
