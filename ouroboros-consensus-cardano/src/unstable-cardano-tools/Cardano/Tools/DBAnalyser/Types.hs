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
  , ldbBackend :: Maybe LedgerDBBackend
  -- ^ The LedgerDB backend selected on the command line. When 'Nothing', the
  -- backend and its settings are taken from the node configuration file
  -- instead.
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

data LedgerDBBackend
  = V2InMem
  | V2LSM LSMOptions

-- | The settings of the LSM-trees backend.
data LSMOptions = LSMOptions
  { lsmDatabasePath :: FilePath
  -- ^ The directory, relative to the LedgerDB filesystem root, holding the
  -- working LSM database.
  , lsmExportPath :: Maybe FilePath
  -- ^ The directory, relative to the LedgerDB filesystem root, into which the
  -- LSM backend exports snapshots as it takes them. When 'Nothing', snapshots
  -- are not exported.
  , lsmNoDiskCache :: Bool
  -- ^ Bypass the OS page cache for UTxO table reads/writes (instead of caching
  -- all). Intended for benchmarking.
  }

-- | The directory holding the working LSM database, used when the command line
-- selects the LSM backend and when the node configuration file does not set
-- @LedgerDB.LSMDatabasePath@.
defaultLSMDatabasePath :: FilePath
defaultLSMDatabasePath = "lsm"

-- | The extent of the ChainDB on-disk files validation. This is completely
-- unrelated to validation of the ledger rules.
data ValidateBlocks = ValidateAllBlocks | MinimumBlockValidation

-- | Whether to apply blocks to a ledger state via /reapplication/ (eg skipping
-- signature checks/Plutus scripts) or full /application/ (much slower).
data LedgerApplicationMode = LedgerReapply | LedgerApply
  deriving (Eq, Show)
