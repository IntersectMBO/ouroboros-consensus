{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Database analysis tool.
--
-- Usage: db-analyser --db PATH [--verbose] [--analyse-from SLOT_NUMBER]
--                    [--db-validation ARG]
--                    [--show-slot-block-no | --count-tx-outputs |
--                      --show-block-header-size | --show-block-txs-size |
--                      --show-ebbs | --store-ledger SLOT_NUMBER
--                      [--full-ledger-validation] |
--                      --count-blocks | --checkThunks BLOCK_COUNT |
--                      --trace-ledger | --repro-mempool-and-forge INT |
--                      --benchmark-ledger-ops [--out-file FILE] [--reapply] |
--                      --get-block-application-metrics NUM [--out-file FILE]]
--                    [--num-blocks-to-process INT] COMMAND
module Main (main) where

import           Cardano.Crypto.Init (cryptoInit)
import           Cardano.Tools.DBAnalyser.Run
import           Cardano.Tools.DBAnalyser.Types
import           Cardano.Tools.GitRev (gitRev)
import           Control.Monad (void)
import qualified Data.Text as T
import           DBAnalyser.Parsers
import           Main.Utf8 (withStdTerminalHandles)
import           Options.Applicative (execParser, footer, fullDesc, helper,
                     info, progDesc, (<**>))


main :: IO ()
main = withStdTerminalHandles $ do
    cryptoInit
    (conf, blocktype) <- getCmdLine
    void $ case blocktype of
      ByronBlock   args -> analyse conf args
      ShelleyBlock args -> analyse conf args
      CardanoBlock args -> analyse conf args

getCmdLine :: IO (DBAnalyserConfig, BlockType)
getCmdLine = execParser opts
  where
    opts = info (parseCmdLine <**> helper) (mconcat [
          fullDesc
        , progDesc "Simple framework used to analyse a Chain DB"
        , footer $ "ouroboros-consensus commit: " <> T.unpack gitRev
        ])
