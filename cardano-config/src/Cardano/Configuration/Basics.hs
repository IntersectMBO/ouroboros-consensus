-- | Basic types for configuration
module Cardano.Configuration.Basics
  ( -- * FilePaths
    File (..)
  , (</>)
  , RelativeFile (..)
  , anchorRelativePath

    -- * Defaults
  , (.:=)
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Default
import Data.String (IsString)
import GHC.Generics (Generic)
import qualified System.FilePath as F
import Prelude hiding (FilePath)
import qualified Prelude as P

-- | A filepath annotated with the intended use for it
newtype File s = File {getFile :: P.FilePath}
  deriving (Generic, Show)
  deriving newtype (Eq, Ord, FromJSON, IsString)

(</>) :: File a -> File b -> File c
File a </> File b = File (a F.</> b)

-- | A FilePath that requires a mount point
newtype RelativeFile s
  = RelativeFile {relativeFile :: File s}
  deriving (Generic, Show)

-- | WARNING: if the RelativeFile contains an absolute path, the first path will
-- be ignored. Make sure to only pass relative paths as the second argument.
anchorRelativePath :: File a -> RelativeFile b -> File b
anchorRelativePath fp1 (RelativeFile fp2) = fp1 </> fp2

--------------------------------------------------------------------------------

-- | If the key is missing, use the default value
(.:=) :: (FromJSON a, Default a) => Object -> Key -> Parser a
a .:= b = a .:? b .!= def

infixr 8 .:=
