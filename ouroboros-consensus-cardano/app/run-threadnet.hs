{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Tools.ThreadNet.Run (slots, threadNetConfigFile, txsPerSlot)
import qualified Cardano.Tools.ThreadNet.Run as Run
import Main.Utf8 (withStdTerminalHandles)
import Options.Applicative

data Opts
  = Run Run.Opts
  | DumpExampleConfig

main :: IO ()
main = withStdTerminalHandles $ do
  cryptoInit
  res <- customExecParser (prefs showHelpOnEmpty) optsParserInfo
  case res of
    Run runOpts -> Run.run runOpts
    DumpExampleConfig -> Run.writeExampleThreadNetConfig

optsParserInfo :: ParserInfo Opts
optsParserInfo =
  info (helper <*> optsParser) $ fullDesc <> progDesc desc
 where
  desc = "Run ThreadNet, ie. run nodes in a simulated IO environment"

  optsParser =
    hsubparser
      ( command "run" (info (Run <$> runParser) (progDesc "Run the simulation"))
          <> command
            "dump-example-config"
            ( info
                (pure DumpExampleConfig)
                (progDesc "Dump an example configuration file (example-threadnet.config)")
            )
      )

  runParser = do
    threadNetConfigFile <-
      strOption $
        mconcat
          [ long "threadnet-config"
          , help "Path to ThreadNet config file"
          , metavar "PATH"
          ]
    slots <-
      option auto $
        mconcat
          [ long "slots"
          , help "How many slots to run the simulation for"
          , metavar "INT"
          , value 30
          , showDefault
          ]
    txsPerSlot <-
      option auto $
        mconcat
          [ long "txs-per-slot"
          , help "How many transactions to add in each slot"
          , metavar "INT"
          , value 100
          , showDefault
          ]

    pure
      Run.Opts
        { threadNetConfigFile
        , txsPerSlot
        , slots
        }
