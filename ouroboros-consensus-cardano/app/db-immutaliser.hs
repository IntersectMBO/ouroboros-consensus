{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Tools.DBImmutaliser.Run (DBDirs (..), Opts (..))
import qualified Cardano.Tools.DBImmutaliser.Run as DBImmutaliser
import Main.Utf8 (withStdTerminalHandles)
import Options.Applicative

main :: IO ()
main = withStdTerminalHandles $ do
  cryptoInit
  DBImmutaliser.run =<< execParser optsParser

optsParser :: ParserInfo Opts
optsParser =
  info (helper <*> parse) $ fullDesc <> progDesc desc
 where
  desc = "Copy a specific chain out of a VolatileDB into an ImmutableDB"

  parse = do
    dbDirs <- do
      immDBDir <-
        strOption $
          mconcat
            [ long "immutable-db"
            , help "Path to the ImmutableDB"
            , metavar "PATH"
            ]
      volDBDir <-
        strOption $
          mconcat
            [ long "volatile-db"
            , help "Path to the VolatileDB"
            , metavar "PATH"
            ]
      pure DBDirs{immDBDir, volDBDir}
    configFile <-
      strOption $
        mconcat
          [ long "config"
          , help "Path to config file, in the same format as for the node or db-analyser"
          , metavar "PATH"
          ]
    verbose <-
      switch $
        mconcat
          [ long "verbose"
          , help "Enable verbose logging"
          ]
    dotOut <-
      optional $
        strOption $
          mconcat
            [ long "dot-out"
            , help "Write the volatile block tree to a file in DOT format"
            ]
    dryRun <-
      switch $
        mconcat
          [ long "dry-run"
          , help "Do not actually append anything to the ImmutableDB"
          ]
    pure Opts{dbDirs, configFile, verbose, dotOut, dryRun}
