{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Tools.DBAnalyser.Types (module Cardano.Tools.DBAnalyser.Types) where

import Control.Exception (Exception (..), handle, throwIO)
import Data.Word
import Ouroboros.Consensus.Block
import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- | A tool was invoked with a configuration it cannot work with, be it in the
-- node configuration file or on the command line.
newtype ConfigError = ConfigError String
  deriving Show

instance Exception ConfigError where
  -- Deliberately just the message: these are user errors, so neither a call
  -- stack nor a @user error (..)@ wrapper would tell the user anything useful.
  displayException (ConfigError msg) = msg

-- | Abort because of a 'ConfigError'.
--
-- Every configuration problem these tools detect is reported this way, so that
-- they all reach the user in the same shape.
throwConfigError :: String -> IO a
throwConfigError = throwIO . ConfigError

-- | Report an escaping 'ConfigError' as a plain @\<prog\>: \<message\>@ on
-- stderr and exit with a failure.
--
-- Wrap a tool's @main@ in this. Without it, GHC's default handler would bury the
-- message under the exception's fully qualified type name and a call-stack
-- backtrace, neither of which helps someone who simply mistyped a configuration
-- key.
withConfigErrorHandling :: IO a -> IO a
withConfigErrorHandling =
  handle $ \(ConfigError msg) -> do
    prog <- getProgName
    hPutStrLn stderr $ prog <> ": " <> msg
    exitFailure

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

-- | The directory that @--lsm-export@ exports snapshots into.
defaultLSMExportPath :: FilePath
defaultLSMExportPath = "lsm-exported"

-- | The extent of the ChainDB on-disk files validation. This is completely
-- unrelated to validation of the ledger rules.
data ValidateBlocks = ValidateAllBlocks | MinimumBlockValidation

-- | Whether to apply blocks to a ledger state via /reapplication/ (eg skipping
-- signature checks/Plutus scripts) or full /application/ (much slower).
data LedgerApplicationMode = LedgerReapply | LedgerApply
  deriving (Eq, Show)
