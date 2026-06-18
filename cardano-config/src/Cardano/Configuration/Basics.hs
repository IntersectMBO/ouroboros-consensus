-- | Basic types for configuration
module Cardano.Configuration.Basics
  ( -- * Defaults
    (.:=)
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default

--------------------------------------------------------------------------------

-- | If the key is missing, use the default value
(.:=) :: (FromJSON a, Default a) => Object -> Key -> Parser a
a .:= b = a .:? b .!= def

infixr 8 .:=
