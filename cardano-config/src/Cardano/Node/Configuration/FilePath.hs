module Cardano.Node.Configuration.Basics where

newtype FilePath (s :: Symbol) = FilePath {getFilePath :: P.FilePath}
  deriving (Generic, Show)
  deriving newtype (FromJSON, IsString)

(</>) :: FilePath a -> FilePath b -> FilePath c
FilePath a </> FilePath b = FilePath (a F.</> b)

newtype RelativeFilePath (s :: Symbol)
  = RelativeFilePath {relativeFilePath :: FilePath s}
  deriving (Generic, Show)

anchorRelativePath :: FilePath a -> RelativeFilePath b -> FilePath b
anchorRelativePath fp1 (RelativeFilePath fp2) = fp1 </> fp2

class DefaultGiven given a where
  defGiven :: given -> a
