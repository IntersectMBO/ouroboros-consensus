{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Tools.ImmDBServer.Json.SendRecv (module Cardano.Tools.ImmDBServer.Json.SendRecv) where

import qualified Data.Aeson as Aeson
import           GHC.Generics (Generic)

-- | Whether the @immdb-server@ received or sent a message.
data Direction = Recv | Send
  deriving stock (Eq, Generic, Ord)
  deriving anyclass (Aeson.ToJSON)

-- | The event as it appears in the log file.
data SendRecvEvent tm cnt = MkSendRecvEvent {
        at :: tm
        -- ^ when
      , direction :: Direction
      , msg :: String
        -- ^ which message
      , connectionId :: String
        -- ^ which peer
      , prevCount :: cnt
        -- ^ how many previous occurrences of this same event for this same peer
      }
  deriving stock (Eq, Generic, Ord)
  deriving anyclass (Aeson.ToJSON)
