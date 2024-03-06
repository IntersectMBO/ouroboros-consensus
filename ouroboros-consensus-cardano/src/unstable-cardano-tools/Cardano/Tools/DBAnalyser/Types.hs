
module Cardano.Tools.DBAnalyser.Types (
    module AnalysisTypes
  , module Cardano.Tools.DBAnalyser.Types
  ) where

import           Cardano.Tools.DBAnalyser.Analysis as AnalysisTypes
                     (AnalysisName (..), AnalysisResult (..), Limit (..),
                     NumberOfBlocks (..))
import           Ouroboros.Consensus.Storage.LedgerDB (DiskSnapshot)


data SelectDB =
    SelectChainDB
  | SelectImmutableDB (Maybe DiskSnapshot)

data DBAnalyserConfig = DBAnalyserConfig {
    dbDir      :: FilePath
  , verbose    :: Bool
  , selectDB   :: SelectDB
  , validation :: Maybe ValidateBlocks
  , analysis   :: AnalysisName
  , confLimit  :: Limit
  }

data ValidateBlocks = ValidateAllBlocks | MinimumBlockValidation
