{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Tools.DBAnalyser.Types (module Cardano.Tools.DBAnalyser.Types) where

import Data.Maybe (fromMaybe)
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
  , ldbBackend :: Maybe LedgerDBBackendFlags
  -- ^ The LedgerDB backend selected on the command line. When 'Nothing', the
  -- backend and its settings are taken from the node configuration file
  -- instead; see 'selectLedgerDBBackend'.
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

-- | The LedgerDB backend as the command line selects it: @--in-mem@, or @--lsm@
-- with its switches.
data LedgerDBBackendFlags
  = InMemFlag
  | LSMFlag LSMFlags

-- | The switches that accompany @--lsm@. Each can only turn its setting on, so
-- 'False' leaves the setting as the node configuration file has it.
data LSMFlags = LSMFlags
  { lsmExportFlag :: Bool
  -- ^ @--lsm-export@
  , lsmNoDiskCacheFlag :: Bool
  -- ^ @--lsm-no-cache@
  }

-- | The backend to use, from the one the command line selects and the one the
-- node configuration file selects. The command line wins, but @--lsm@ against
-- a configuration that also selects LSM overrides only what its switches set:
-- the paths come from the configuration, and @--lsm-export@ exports into the
-- configured @ExportPath@ when there is one. Against any other configuration,
-- @--lsm@ starts from the defaults.
selectLedgerDBBackend ::
  Maybe LedgerDBBackendFlags -> Maybe LedgerDBBackend -> Maybe LedgerDBBackend
selectLedgerDBBackend flags configBackend = case flags of
  Nothing -> configBackend
  Just InMemFlag -> Just V2InMem
  Just (LSMFlag LSMFlags{lsmExportFlag, lsmNoDiskCacheFlag}) ->
    Just $
      V2LSM
        base
          { lsmExportPath =
              if lsmExportFlag
                then Just (fromMaybe defaultLSMExportPath (lsmExportPath base))
                else lsmExportPath base
          , lsmNoDiskCache = lsmNoDiskCache base || lsmNoDiskCacheFlag
          }
 where
  base = case configBackend of
    Just (V2LSM opts) -> opts
    _ ->
      LSMOptions
        { lsmDatabasePath = defaultLSMDatabasePath
        , lsmExportPath = Nothing
        , lsmNoDiskCache = False
        }

-- | The directory holding the working LSM database, used when the node
-- configuration file does not set @LedgerDB.Backend.LSM.DatabasePath@.
defaultLSMDatabasePath :: FilePath
defaultLSMDatabasePath = "lsm"

-- | The directory that @--lsm-export@ exports snapshots into, when the node
-- configuration file does not set @LedgerDB.Backend.LSM.ExportPath@.
defaultLSMExportPath :: FilePath
defaultLSMExportPath = "lsm-exported"

-- | The extent of the ChainDB on-disk files validation. This is completely
-- unrelated to validation of the ledger rules.
data ValidateBlocks = ValidateAllBlocks | MinimumBlockValidation

-- | Whether to apply blocks to a ledger state via /reapplication/ (eg skipping
-- signature checks/Plutus scripts) or full /application/ (much slower).
data LedgerApplicationMode = LedgerReapply | LedgerApply
  deriving (Eq, Show)
