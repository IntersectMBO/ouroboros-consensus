{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Node.Protocol.Types
  ( Protocol (..)
  ) where

import Cardano.Node.Orphans ()
-- import Cardano.Node.Queries (HasKESInfo, HasKESMetricsData)
-- import Cardano.Node.TraceConstraints (TraceConstraints)
import Control.DeepSeq (NFData)
import Data.Aeson
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)

data Protocol = CardanoProtocol
  deriving (Eq, Generic)

instance Show Protocol where
  show CardanoProtocol = "Byron; Shelley"

deriving instance NFData Protocol
deriving instance NoThunks Protocol

instance FromJSON Protocol where
  parseJSON =
    withText "Protocol" $ \str -> case str of
      "Cardano" -> pure CardanoProtocol
      _ -> fail $ "Parsing of Protocol failed. " <> show str <> " is not a valid protocol"
