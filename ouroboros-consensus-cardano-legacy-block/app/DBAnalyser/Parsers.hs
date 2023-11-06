{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP           #-}

module DBAnalyser.Parsers (parseCmdLine, blockTypeParser, BlockType (..)) where

import           Cardano.Tools.DBAnalyser.Types
#if __GLASGOW_HASKELL__ < 900
import           Data.Foldable (asum)
#endif
import           Options.Applicative
import DBAnalyser.LegacyCardano
import           Ouroboros.Consensus.Block (SlotNo (..))
import           Ouroboros.Consensus.Byron.Node (PBftSignatureThreshold (..))
import           Ouroboros.Consensus.Storage.LedgerDB (DiskSnapshot (..))

{-------------------------------------------------------------------------------
  Parsing
-------------------------------------------------------------------------------}

parseCmdLine :: Parser (DBAnalyserConfig, BlockType)
parseCmdLine = (,) <$> parseDBAnalyserConfig <*> blockTypeParser

parseDBAnalyserConfig :: Parser DBAnalyserConfig
parseDBAnalyserConfig = DBAnalyserConfig
    <$> strOption (mconcat [
            long "db"
          , help "Path to the Chain DB"
          , metavar "PATH"
          ])
    <*> switch (mconcat [
            long "verbose"
          , help "Enable verbose logging"
          ])
    <*> parseSelectDB
    <*> parseValidationPolicy
    <*> parseAnalysis
    <*> parseLimit

parseSelectDB :: Parser SelectDB
parseSelectDB = asum [
    SelectImmutableDB . snd <$> ((,) <$> onlyImmutableDB <*> analyseFrom)
  , pure SelectChainDB
  ]
  where
    onlyImmutableDB = flag' () (mconcat [
        long "only-immutable-db"
      , help "Validate only the Immutable DB (e.g. do not do ledger validation)"
      ])

    analyseFrom :: Parser (Maybe DiskSnapshot)
    analyseFrom = optional $ ((flip DiskSnapshot $ Just "db-analyser") . read) <$> strOption
      (  long "analyse-from"
      <> metavar "SLOT_NUMBER"
      <> help "Start analysis from ledger state stored at specific slot number" )


parseValidationPolicy :: Parser (Maybe ValidateBlocks)
parseValidationPolicy = parseMaybe $ asum [
      flag' ValidateAllBlocks $ mconcat [
          long "validate-all-blocks"
        , help "Validate all blocks of the Volatile and Immutable DB"
        ]
    , flag' MinimumBlockValidation $ mconcat [
          long "minimum-block-validation"
        , help "Validate a minimum part of the Volatile and Immutable DB"
        ]
    ]

parseAnalysis :: Parser AnalysisName
parseAnalysis = asum [
      flag' ShowSlotBlockNo $ mconcat [
          long "show-slot-block-no"
        , help "Show slot and block number of all blocks"
        ]
    , flag' CountTxOutputs $ mconcat [
          long "count-tx-outputs"
        , help "Show number of transaction outputs per block"
        ]
    , flag' ShowBlockHeaderSize $ mconcat [
          long "show-block-header-size"
        , help "Show the header sizes of all blocks"
        ]
    , flag' ShowBlockTxsSize $ mconcat [
          long "show-block-txs-size"
        , help "Show the total transaction sizes per block"
        ]
    , flag' ShowEBBs $ mconcat [
          long "show-ebbs"
        , help "Show all EBBs and their predecessors"
        ]
    , storeLedgerParser
    , flag' CountBlocks $ mconcat [
          long "count-blocks"
        , help "Count number of blocks processed"
        ]
    , checkNoThunksParser
    , flag' TraceLedgerProcessing $ mconcat [
          long "trace-ledger"
        , help $ "Maintain ledger state and trace ledger phases in the GHC event"
                <> " log. The db-analyser tool performs era-specific analysis"
                <> " of the ledger state and inserts markers for 'significant'"
                <> " events, such as for example epoch transitions."
        ]
    , fmap ReproMempoolAndForge $ option auto $ mconcat [
          long "repro-mempool-and-forge"
        , help $ "Maintain ledger state and mempool trafficking the"
              <> " transactions of each block. The integer is how many"
              <> "blocks to put in the mempool at once."
        , metavar "INT"
        ]
    , benchmarkLedgerOpsParser
    , pure OnlyValidation
    ]

storeLedgerParser :: Parser AnalysisName
storeLedgerParser = (StoreLedgerStateAt . SlotNo) <$> option auto
  (  long "store-ledger"
  <> metavar "SLOT_NUMBER"
  <> help "Store ledger state at specific slot number" )

checkNoThunksParser :: Parser AnalysisName
checkNoThunksParser = CheckNoThunksEvery <$> option auto
  (  long "checkThunks"
  <> metavar "BLOCK_COUNT"
  <> help "Check the ledger state for thunks every n blocks" )

parseLimit :: Parser Limit
parseLimit = asum [
    Limit <$> option auto (mconcat [
        long "num-blocks-to-process"
      , help "Maximum number of blocks we want to process"
      , metavar "INT"
      ])
  , pure Unlimited
  ]

benchmarkLedgerOpsParser :: Parser AnalysisName
benchmarkLedgerOpsParser =
    BenchmarkLedgerOps <$> (benchmarkLedgerOpsFlagParser *> pMaybeOutputFile)
  where
    benchmarkLedgerOpsFlagParser =
      flag' BenchmarkLedgerOps $ mconcat [
            long "benchmark-ledger-ops"
          , help $ "Maintain ledger state and benchmark the main ledger calculations for each block."
                  <> " Prints one line of stats per block to the given output file "
                  <> " (defaults to stdout)."
          ]

pMaybeOutputFile :: Parser (Maybe FilePath)
pMaybeOutputFile =
  optional $
    strOption
      (  long "out-file"
      <> metavar "FILE"
      <> help "Optional output file. Default is to write to stdout."
      <> completer (bashCompleter "file")
      )

parseMaybe ::  Parser a -> Parser (Maybe a)
parseMaybe parser = asum [Just <$> parser, pure Nothing]

{-------------------------------------------------------------------------------
  Parse BlockType-specific arguments
-------------------------------------------------------------------------------}

newtype BlockType = LegacyCardanoBlock LegacyCardanoBlockArgs

blockTypeParser :: Parser BlockType
blockTypeParser = subparser $ mconcat
  [ command "legacy-cardano"
      (info (parseLegacyCardanoType <**> helper) (progDesc "Analyse a Legacy Cardano DB"))
  ]

parseLegacyCardanoType :: Parser BlockType
parseLegacyCardanoType = LegacyCardanoBlock <$> parseLegacyCardanoArgs

parseLegacyCardanoArgs :: Parser LegacyCardanoBlockArgs
parseLegacyCardanoArgs = LegacyCardanoBlockArgs
    <$> parseConfigFile
    <*> parsePBftSignatureThreshold

parseConfigFile :: Parser FilePath
parseConfigFile = strOption $ mconcat [
      long "config"
    , help "Path to config file"
    , metavar "PATH"
    ]

parsePBftSignatureThreshold :: Parser (Maybe PBftSignatureThreshold)
parsePBftSignatureThreshold = optional $ fmap PBftSignatureThreshold $ option auto $ mconcat [
      long "threshold"
    , help "PBftSignatureThreshold"
    , metavar "THRESHOLD"
    ]
