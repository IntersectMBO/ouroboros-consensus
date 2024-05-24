
module Cardano.Tools.DBAnalyser.Types (
    module AnalysisTypes
  , module Cardano.Tools.DBAnalyser.Types
  ) where

import           Cardano.Tools.DBAnalyser.Analysis as AnalysisTypes
                     (AnalysisName (..), AnalysisResult (..), Limit (..))
import           Ouroboros.Consensus.Storage.LedgerDB.Impl.Snapshots
                     (DiskSnapshot)
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Args
import           Ouroboros.Consensus.Util.Args

data SelectDB =
    SelectChainDB (Maybe DiskSnapshot)
  | SelectImmutableDB (Maybe DiskSnapshot)

data DBAnalyserConfig = DBAnalyserConfig {
    dbDir       :: FilePath
  , ssdDir      :: FilePath
  , stateInSSD  :: Bool
  , tablesInSSD :: Bool
  , verbose     :: Bool
  , selectDB    :: SelectDB
  , validation  :: Maybe ValidateBlocks
  , analysis    :: AnalysisName
  , confLimit   :: Limit
  , bsArgs      :: Complete BackingStoreArgs IO
  }

data ValidateBlocks = ValidateAllBlocks | MinimumBlockValidation
