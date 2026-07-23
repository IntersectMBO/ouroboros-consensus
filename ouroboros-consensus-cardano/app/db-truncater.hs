module Main (main) where

import Cardano.Crypto.Init (cryptoInit)
import Cardano.Tools.DBTruncater.Run
import Cardano.Tools.DBTruncater.Types
import DBAnalyser.Parsers
import qualified DBTruncater.Parsers as DBTruncater
import Main.Utf8 (withStdTerminalHandles)
import Options.Applicative
  ( execParser
  , fullDesc
  , helper
  , info
  , progDesc
  , (<**>)
  )
import Ouroboros.Consensus.Storage.ImmutableDB.Impl ()
import Prelude hiding (truncate)

main :: IO ()
main = withStdTerminalHandles $ do
  cryptoInit
  uncurry truncate =<< getCommandLineConfig

getCommandLineConfig :: IO (DBTruncaterConfig, CardanoBlockArgs)
getCommandLineConfig = execParser opts
 where
  opts =
    info
      (DBTruncater.commandLineParser <**> helper)
      (fullDesc <> progDesc "Utility for truncating an ImmutableDB")
