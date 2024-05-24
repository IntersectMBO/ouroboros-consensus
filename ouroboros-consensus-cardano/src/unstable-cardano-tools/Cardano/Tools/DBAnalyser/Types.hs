module Cardano.Tools.DBAnalyser.Types (
    module AnalysisTypes
  , module Cardano.Tools.DBAnalyser.Types
  ) where

import           Cardano.Tools.DBAnalyser.Analysis as AnalysisTypes
                     (AnalysisName (..), AnalysisResult (..), Limit (..),
                     NumberOfBlocks (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Storage.LedgerDB.V1.Args
import           Ouroboros.Consensus.Util.Args

data SelectDB =
    SelectImmutableDB (WithOrigin SlotNo)

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

-- | The extent of the ChainDB on-disk files validation. This is completely
-- unrelated to validation of the ledger rules.
data ValidateBlocks = ValidateAllBlocks | MinimumBlockValidation
