module Main (main) where

import Prelude hiding (truncate)
import Cardano.Tools.DBTruncater.Run
import Cardano.Tools.DBAnalyser.Types (BlockType(..))
import Cardano.Tools.DBTruncater.Types 
import qualified DBTruncater.Parsers as DBTruncater
import Options.Applicative (execParser, info, helper, fullDesc, progDesc, (<**>))
import Ouroboros.Consensus.Storage.ImmutableDB.Impl ()

main :: IO ()
main = do
  config <- getCommandLineConfig
  case blockType config of
    CardanoBlock args -> truncate config args
    ByronBlock args -> truncate config args
    ShelleyBlock args -> truncate config args

getCommandLineConfig :: IO DBTruncaterConfig
getCommandLineConfig = execParser opts
  where
    opts = info (DBTruncater.commandLineParser <**> helper)
      (fullDesc <> progDesc "Utility for truncating an ImmutableDB")
