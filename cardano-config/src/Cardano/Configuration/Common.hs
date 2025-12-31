-- | Types that are common to CLI arguments and the configuration files
module Cardano.Configuration.Common
  ( NodeDatabasePaths (..)
  , parseNodeDatabasePaths
  , parseStartAsNonProducingNode
  ) where

import Cardano.Configuration.Basics
import Data.Aeson
import Data.Default
import qualified Data.Text as T
import GHC.Generics
import Options.Applicative

--------------------------------------------------------------------------------

-- | The databases that will be used by the node
data NodeDatabasePaths
  = -- | Store everything in a single directory
    Unique (File "Database")
  | -- | Store the immutable data in one (possibly slower) directory and the
    -- volatile data in a different (possible faster) directory
    Split (File "ImmutableDB") (File "VolatileDB")
  deriving (Generic, Show)

instance Default NodeDatabasePaths where
  def = Unique "mainnet/db"

instance FromJSON NodeDatabasePaths where
  parseJSON v =
    withText "OneDatabase" (pure . Unique . File . T.unpack) v
      <|> withObject
        "MultipleDatabases"
        (\v' -> Split <$> v' .: "ImmutablePath" <*> v' .: "VolatilePath")
        v

parseNodeDatabasePaths :: Parser (Maybe NodeDatabasePaths)
parseNodeDatabasePaths =
  optional $ parseMultipleDbPaths <|> parseDbPath

parseDbPath :: Parser NodeDatabasePaths
parseDbPath =
  fmap Unique $
    strOption $
      mconcat
        [ long "database-path"
        , metavar "FILEPATH"
        , help "Directory where the state is stored"
        , completer (bashCompleter "file")
        ]

parseMultipleDbPaths :: Parser NodeDatabasePaths
parseMultipleDbPaths = Split <$> parseImmutableDbPath <*> parseVolatileDbPath

parseVolatileDbPath :: Parser (File "VolatileDB")
parseVolatileDbPath =
  strOption $
    mconcat
      [ long "volatile-database-path"
      , metavar "FILEPATH"
      , help "Directory where the volatile state is stored"
      , completer (bashCompleter "file")
      ]

parseImmutableDbPath :: Parser (File "ImmutableDB")
parseImmutableDbPath =
  strOption $
    mconcat
      [ long "immutable-database-path"
      , metavar "FILEPATH"
      , help "Directory where the immutable state is stored"
      , completer (bashCompleter "file")
      ]

-- | The value missing means "unset" not @False@, hence the @Maybe Bool@.
parseStartAsNonProducingNode :: Parser (Maybe Bool)
parseStartAsNonProducingNode =
  flag Nothing (Just True) $
    mconcat
      [ long "start-as-non-producing-node"
      , help $
          mconcat
            [ "Start the node as a non block-producing node even if "
            , "credentials are specified"
            ]
      ]
