{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Tools.ThreadNet.Run (Opts (..))
import qualified Cardano.Tools.ThreadNet.Run as RunThreadNet
import Main.Utf8 (withStdTerminalHandles)
import Options.Applicative

main :: IO ()
main = withStdTerminalHandles $ do
  cryptoInit
  RunThreadNet.run =<< execParser optsParser

optsParser :: ParserInfo Opts
optsParser =
  info (helper <*> parse) $ fullDesc <> progDesc desc
 where
  desc = "Run ThreadNet, ie. run nodes in a simulated IO environment"

  parse = do
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
      Opts
        { threadNetConfigFile
        , txsPerSlot
        , slots
        }
