-- | Basic types for configuration
module Cardano.Node.Configuration.Basics
  ( -- * FilePaths
    FilePath (..)
  , (</>)
  , RelativeFilePath (..)
  , anchorRelativePath

    -- * Defaults
  , Override (..)
  , (.:=)
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default
import Data.String (IsString)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)
import qualified System.FilePath as F
import Prelude hiding (FilePath)
import qualified Prelude as P

-- | A filepath annotated with the intended use for it
newtype FilePath (s :: Symbol) = FilePath {getFilePath :: P.FilePath}
  deriving (Generic, Show)
  deriving newtype (FromJSON, IsString)

(</>) :: FilePath a -> FilePath b -> FilePath c
FilePath a </> FilePath b = FilePath (a F.</> b)

-- | A FilePath that requires a mount point
newtype RelativeFilePath (s :: Symbol)
  = RelativeFilePath {relativeFilePath :: FilePath s}
  deriving (Generic, Show)

anchorRelativePath :: FilePath a -> RelativeFilePath b -> FilePath b
anchorRelativePath fp1 (RelativeFilePath fp2) = fp1 </> fp2

--------------------------------------------------------------------------------

-- | Signal whether the default value should not be overriden, but don't provide
-- a default at this level. The particular component will have to then apply
-- whatever defaults it considers acceptable.
data Override a = NoOverride | Override a deriving (Generic, Show, Eq)

instance Default (Override a) where
  def = NoOverride

instance FromJSON a => FromJSON (Override a) where
  parseJSON v =
    withText
      "Override"
      ( \case
          "NoOverride" -> pure NoOverride
          _ -> fail "Not text"
      )
      v
      <|> Override <$> parseJSON v

--------------------------------------------------------------------------------

-- | If the key is missing, use the default value
(.:=) :: (FromJSON a, Default a) => Object -> Key -> Parser a
a .:= b = a .:? b .!= def
