{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Cardano.Crypto.Init (cryptoInit)
import           Cardano.Tools.DBTruncater.Run
import           Cardano.Tools.DBTruncater.Types
import           DBAnalyser.Parsers (BlockType (..))
import qualified DBTruncater.Parsers as DBTruncater
import           Options.Applicative (execParser, fullDesc, helper, info,
                     progDesc, (<**>))
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl ()
import           Prelude hiding (truncate)

main :: IO ()
main = do
    cryptoInit
    (conf, blocktype) <- getCommandLineConfig
    case blocktype of
      ByronBlock   args -> truncate conf args
      ShelleyBlock args -> truncate conf args
      CardanoBlock args -> truncate conf args

getCommandLineConfig :: IO (DBTruncaterConfig, BlockType)
getCommandLineConfig = execParser opts
  where
    opts = info (DBTruncater.commandLineParser <**> helper)
      (fullDesc <> progDesc "Utility for truncating an ImmutableDB")
