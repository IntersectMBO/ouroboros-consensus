{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Database stressing tool.
--
-- Usage: db-stresser --db PATH [--verbose]
module Main (main) where

import           Cardano.Crypto.Init (cryptoInit)
import           Cardano.Tools.DBStresser.Run
import           Cardano.Tools.DBTruncater.Types
import           DBAnalyser.Parsers (BlockType (..))
import qualified DBTruncater.Parsers as DBTruncater
import           Main.Utf8 (withStdTerminalHandles)
import           Options.Applicative (execParser, fullDesc, helper, info,
                     progDesc, (<**>))
import           Ouroboros.Consensus.Storage.ImmutableDB.Impl ()
import           Prelude hiding (truncate)

main :: IO ()
main = withStdTerminalHandles $ do
    cryptoInit
    (conf, blocktype) <- getCommandLineConfig
    case blocktype of
      ByronBlock   args -> stress conf args
      ShelleyBlock args -> stress conf args
      CardanoBlock args -> stress conf args

getCommandLineConfig :: IO (DBTruncaterConfig, BlockType)
getCommandLineConfig = execParser opts
  where
    opts = info (DBTruncater.commandLineParser <**> helper)
      (fullDesc <> progDesc "Utility for truncating an ImmutableDB")
