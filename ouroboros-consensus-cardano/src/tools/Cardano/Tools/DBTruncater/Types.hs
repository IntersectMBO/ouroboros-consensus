module Cardano.Tools.DBTruncater.Types (module Cardano.Tools.DBTruncater.Types) where

import           Cardano.Tools.DBAnalyser.Types
import           Ouroboros.Consensus.Block.Abstract

data DBTruncaterConfig = DBTruncaterConfig {
    dbDir         :: FilePath
  , truncatePoint :: TruncatePoint
  , blockType     :: BlockType
  , verbose       :: Bool
  }

newtype TruncatePoint = TruncatePoint SlotNo
  deriving (Show, Eq)
