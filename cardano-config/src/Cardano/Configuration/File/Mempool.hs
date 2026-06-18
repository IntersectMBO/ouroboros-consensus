-- | Options related to the mempool
module Cardano.Configuration.File.Mempool
  ( MempoolConfiguration (..)
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Time.Clock (DiffTime)
import Data.Word
import GHC.Generics (Generic)

-- | The mempool configuration. All fields are optional; when unset the node
-- applies its own defaults.
data MempoolConfiguration = MempoolConfiguration
  { -- | Overriding the maximum size of the mempool, in bytes.
    mempoolCapacityOverride :: Maybe Word64
  , mempoolTimeoutSoft :: Maybe DiffTime
  , mempoolTimeoutHard :: Maybe DiffTime
  , mempoolTimeoutCapacity :: Maybe DiffTime
  }
  deriving (Generic, Show)

instance FromJSON MempoolConfiguration where
  parseJSON =
    withObject "MempoolConfiguration" $ \v ->
      MempoolConfiguration
        <$> parseMempoolCapacityBytesOverride v
        <*> v .:? "MempoolTimeoutSoft"
        <*> v .:? "MempoolTimeoutHard"
        <*> v .:? "MempoolTimeoutCapacity"

-- | Parse the optional mempool capacity override, accepting either a byte count
-- or the string @"NoOverride"@, as the node does.
parseMempoolCapacityBytesOverride :: Object -> Parser (Maybe Word64)
parseMempoolCapacityBytesOverride v = parseOverride <|> parseNoOverride
 where
  parseOverride = v .:? "MempoolCapacityBytesOverride"
  parseNoOverride =
    v .:? "MempoolCapacityBytesOverride" >>= \case
      Just ("NoOverride" :: String) -> pure Nothing
      Just invalid ->
        fail $
          "Invalid value for 'MempoolCapacityBytesOverride'. "
            <> "Expecting byte count or NoOverride. Value was: "
            <> show invalid
      Nothing -> pure Nothing
