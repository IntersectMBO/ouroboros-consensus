{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Tools.ImmDBServer.Json.Say (module Cardano.Tools.ImmDBServer.Json.Say) where

import qualified Data.Aeson as Aeson
import           GHC.Generics (Generic)

-- | The event as it appears in the log file.
data SayEvent tm = MkSayEvent {
        at :: tm
      , msg :: String
      }
  deriving stock (Generic)
  deriving anyclass (Aeson.ToJSON)
