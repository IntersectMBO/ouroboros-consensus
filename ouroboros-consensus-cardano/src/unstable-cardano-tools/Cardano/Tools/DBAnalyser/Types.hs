{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Tools.DBAnalyser.Types (module Cardano.Tools.DBAnalyser.Types) where

import Data.Word
import Ouroboros.Consensus.Block

data SelectDB
  = SelectImmutableDB (WithOrigin SlotNo)

data DBAnalyserConfig = DBAnalyserConfig
  { dbDir :: FilePath
  , verbose :: Bool
  , selectDB :: SelectDB
  , validation :: Maybe ValidateBlocks
  , analysis :: AnalysisName
  , confLimit :: Limit
  , ldbBackend :: LedgerDBBackend
  }

data AnalysisName
  = ShowSlotBlockNo
  | CountTxOutputs
  | ShowBlockHeaderSize
  | ShowBlockTxsSize
  | ShowEBBs
  | OnlyValidation
  | StoreLedgerStateAt SlotNo LedgerApplicationMode
  | CountBlocks
  | CheckNoThunksEvery Word64
  | TraceLedgerProcessing
  | BenchmarkLedgerOps (Maybe FilePath) LedgerApplicationMode
  | ReproMempoolAndForge Int
  | -- | Compute different block application metrics every 'NumberOfBlocks'.
    --
    -- The metrics will be written to the provided file path, or to
    -- the standard output if no file path is specified.
    GetBlockApplicationMetrics NumberOfBlocks (Maybe FilePath)
  deriving Show

data AnalysisResult
  = ResultCountBlock Int
  | ResultMaxHeaderSize Word16
  deriving (Eq, Show)

newtype NumberOfBlocks = NumberOfBlocks {unNumberOfBlocks :: Word64}
  deriving (Eq, Show, Num, Read)

data Limit = Limit Int | Unlimited

data LedgerDBBackend = V1InMem | V1LMDB | V2InMem | V2LSM

-- | The extent of the ChainDB on-disk files validation. This is completely
-- unrelated to validation of the ledger rules.
data ValidateBlocks = ValidateAllBlocks | MinimumBlockValidation

-- | Whether to apply blocks to a ledger state via /reapplication/ (eg skipping
-- signature checks/Plutus scripts) or full /application/ (much slower).
data LedgerApplicationMode = LedgerReapply | LedgerApply
  deriving (Eq, Show)
