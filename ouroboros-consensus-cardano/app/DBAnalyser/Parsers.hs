{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module DBAnalyser.Parsers
  ( parseCmdLine
  , parseCardanoArgs
  , CardanoBlockArgs
  ) where

import Cardano.Tools.DBAnalyser.Analysis
import Cardano.Tools.DBAnalyser.Block.Cardano
import Cardano.Tools.DBAnalyser.Types
import qualified Data.Foldable as Foldable
import Options.Applicative
import Ouroboros.Consensus.Block (SlotNo (..), WithOrigin (..))
import Ouroboros.Consensus.Byron.Node (PBftSignatureThreshold (..))

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

parseCmdLine :: Parser (DBAnalyserConfig, CardanoBlockArgs)
parseCmdLine = (,) <$> parseDBAnalyserConfig <*> parseCardanoArgs

parseDBAnalyserConfig :: Parser DBAnalyserConfig
parseDBAnalyserConfig =
  DBAnalyserConfig
    <$> strOption
      ( mconcat
          [ long "db"
          , help "Path to the Chain DB"
          , metavar "PATH"
          ]
      )
    <*> switch
      ( mconcat
          [ long "verbose"
          , help "Enable verbose logging"
          ]
      )
    <*> parseSelectDB
    <*> parseValidationPolicy
    <*> parseAnalysis
    <*> parseLimit
    <*> Foldable.asum
      [ flag' V1InMem $
          mconcat
            [ long "v1-in-mem"
            , help "use v1 in-memory backing store"
            ]
      , flag' V1LMDB $
          mconcat
            [ long "lmdb"
            , help "use v1 LMDB backing store"
            ]
      , flag' V2InMem $
          mconcat
            [ long "v2-in-mem"
            , help "use v2 in-memory backend"
            ]
      ]

parseSelectDB :: Parser SelectDB
parseSelectDB =
  SelectImmutableDB <$> analyseFrom
 where
  analyseFrom :: Parser (WithOrigin SlotNo)
  analyseFrom =
    fmap (maybe Origin (NotOrigin . SlotNo)) $
      optional $
        option
          auto
          ( long "analyse-from"
              <> metavar "SLOT_NUMBER"
              <> help "Start analysis from ledger state stored at specific slot number"
          )

parseValidationPolicy :: Parser (Maybe ValidateBlocks)
parseValidationPolicy =
  optional $
    option reader $
      mconcat
        [ long "db-validation"
        , help $
            "The extent of the ChainDB on-disk files validation. This is "
              <> "completely unrelated to validation of the ledger rules. "
              <> "Possible values: validate-all-blocks, minimum-block-validation."
        ]
 where
  reader = maybeReader $ \case
    "validate-all-blocks" -> Just ValidateAllBlocks
    "minimum-block-validation" -> Just MinimumBlockValidation
    _ -> Nothing

parseAnalysis :: Parser AnalysisName
parseAnalysis =
  Foldable.asum
    [ flag' ShowSlotBlockNo $
        mconcat
          [ long "show-slot-block-no"
          , help "Show slot and block number and hash of all blocks"
          ]
    , flag' CountTxOutputs $
        mconcat
          [ long "count-tx-outputs"
          , help "Show number of transaction outputs per block"
          ]
    , flag' ShowBlockHeaderSize $
        mconcat
          [ long "show-block-header-size"
          , help "Show the header sizes of all blocks"
          ]
    , flag' ShowBlockTxsSize $
        mconcat
          [ long "show-block-txs-size"
          , help "Show the total transaction sizes per block"
          ]
    , flag' ShowEBBs $
        mconcat
          [ long "show-ebbs"
          , help "Show all EBBs and their predecessors"
          ]
    , storeLedgerParser
    , flag' CountBlocks $
        mconcat
          [ long "count-blocks"
          , help "Count number of blocks processed"
          ]
    , checkNoThunksParser
    , flag' TraceLedgerProcessing $
        mconcat
          [ long "trace-ledger"
          , help $
              "Maintain ledger state and trace ledger phases in the GHC event"
                <> " log. The db-analyser tool performs era-specific analysis"
                <> " of the ledger state and inserts markers for 'significant'"
                <> " events, such as for example epoch transitions."
          ]
    , fmap ReproMempoolAndForge $
        option auto $
          mconcat
            [ long "repro-mempool-and-forge"
            , help $
                "Maintain ledger state and mempool trafficking the"
                  <> " transactions of each block. The integer is how many"
                  <> "blocks to put in the mempool at once."
            , metavar "INT"
            ]
    , benchmarkLedgerOpsParser
    , getBlockApplicationMetrics
    , pure OnlyValidation
    ]

storeLedgerParser :: Parser AnalysisName
storeLedgerParser = do
  slot <-
    SlotNo
      <$> option
        auto
        ( long "store-ledger"
            <> metavar "SLOT_NUMBER"
            <> help "Store ledger state at specific slot number"
        )
  ledgerValidation <-
    flag
      LedgerReapply
      LedgerApply
      ( long "full-ledger-validation"
          <> help
            ( "Use full block application while applying blocks to ledger states, "
                <> "also validating signatures and scripts. "
                <> "This is much slower than block reapplication (the default)."
            )
      )
  pure $ StoreLedgerStateAt slot ledgerValidation

checkNoThunksParser :: Parser AnalysisName
checkNoThunksParser =
  CheckNoThunksEvery
    <$> option
      auto
      ( long "checkThunks"
          <> metavar "BLOCK_COUNT"
          <> help "Check the ledger state for thunks every n blocks"
      )

parseLimit :: Parser Limit
parseLimit =
  Foldable.asum
    [ Limit
        <$> option
          auto
          ( mconcat
              [ long "num-blocks-to-process"
              , help "Maximum number of blocks we want to process"
              , metavar "INT"
              ]
          )
    , pure Unlimited
    ]

benchmarkLedgerOpsParser :: Parser AnalysisName
benchmarkLedgerOpsParser =
  benchmarkLedgerOpsFlagParser
    *> (BenchmarkLedgerOps <$> pMaybeOutputFile <*> pApplyMode)
 where
  benchmarkLedgerOpsFlagParser =
    flag' BenchmarkLedgerOps $
      mconcat
        [ long "benchmark-ledger-ops"
        , help $
            "Maintain ledger state and benchmark the main ledger calculations for each block."
              <> " Prints one line of stats per block to the given output file "
              <> " (defaults to stdout)."
        ]

  pApplyMode =
    flag LedgerApply LedgerReapply $
      mconcat
        [ long "reapply"
        , help $ "Measure header/block *re*application instead of full application."
        ]

getBlockApplicationMetrics :: Parser AnalysisName
getBlockApplicationMetrics = do
  fGetBlockApplicationMetrics <- partialGetBlockApplicationMetricsParser
  mOutputFile <- pMaybeOutputFile
  pure $ fGetBlockApplicationMetrics mOutputFile
 where
  partialGetBlockApplicationMetricsParser =
    GetBlockApplicationMetrics . NumberOfBlocks
      <$> option
        auto
        ( mconcat
            [ long "get-block-application-metrics"
            , metavar "NUM"
            , help $
                "Compute block application metrics every 'NUM' blocks (it currently supports slot and block numbers and UTxO size). "
                  <> "Stores the result to the given output file "
                  <> " (defaults to stdout)."
            ]
        )

pMaybeOutputFile :: Parser (Maybe FilePath)
pMaybeOutputFile =
  optional $
    strOption
      ( long "out-file"
          <> metavar "FILE"
          <> help "Optional output file. Default is to write to stdout."
          <> completer (bashCompleter "file")
      )

{-------------------------------------------------------------------------------
  Parse BlockType-specific arguments
-------------------------------------------------------------------------------}

parseCardanoArgs :: Parser CardanoBlockArgs
parseCardanoArgs =
  CardanoBlockArgs
    <$> parseConfigFile
    <*> parsePBftSignatureThreshold

parseConfigFile :: Parser FilePath
parseConfigFile =
  strOption $
    mconcat
      [ long "config"
      , help "Path to config file"
      , metavar "PATH"
      ]

parsePBftSignatureThreshold :: Parser (Maybe PBftSignatureThreshold)
parsePBftSignatureThreshold =
  optional $
    fmap PBftSignatureThreshold $
      option auto $
        mconcat
          [ long "threshold"
          , help "PBftSignatureThreshold"
          , metavar "THRESHOLD"
          ]
