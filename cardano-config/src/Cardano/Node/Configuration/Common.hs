{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Node.Configuration.Common
  ( NodeDatabasePaths (..)
  , parseNodeDatabasePaths
  , parseStartAsNonProducingNode
  ) where

import Cardano.Node.Configuration.Basics
import Data.Aeson
import Data.Default
import qualified Data.Text as T
import GHC.Generics
import Options.Applicative
import Prelude hiding (FilePath)

--------------------------------------------------------------------------------

-- * NodeDatabasePaths

--------------------------------------------------------------------------------

data NodeDatabasePaths
  = Unique (FilePath "Database")
  | Split (FilePath "ImmutableDB") (FilePath "VolatileDB")
  deriving (Generic, Show)

instance Default NodeDatabasePaths where
  def = Unique "mainnet/db"

instance FromJSON NodeDatabasePaths where
  parseJSON v =
    withText "OneDatabase" (pure . Unique . FilePath . T.unpack) v
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

parseVolatileDbPath :: Parser (FilePath "VolatileDB")
parseVolatileDbPath =
  strOption $
    mconcat
      [ long "volatile-database-path"
      , metavar "FILEPATH"
      , help "Directory where the volatile state is stored"
      , completer (bashCompleter "file")
      ]

parseImmutableDbPath :: Parser (FilePath "ImmutableDB")
parseImmutableDbPath =
  strOption $
    mconcat
      [ long "immutable-database-path"
      , metavar "FILEPATH"
      , help "Directory where the immutable state is stored"
      , completer (bashCompleter "file")
      ]

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
