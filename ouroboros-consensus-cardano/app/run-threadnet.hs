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
    configFile <-
      strOption $
        mconcat
          [ long "config"
          , help "Path to config file, in the same format as for the node or db-analyser"
          , metavar "PATH"
          ]
    pure Opts{configFile}
