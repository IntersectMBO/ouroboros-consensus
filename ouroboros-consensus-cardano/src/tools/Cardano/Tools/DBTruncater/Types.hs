module Cardano.Tools.DBTruncater.Types (module Cardano.Tools.DBTruncater.Types) where

import           Cardano.Tools.DBAnalyser.Types
import           Ouroboros.Consensus.Block.Abstract

data DBTruncaterConfig = DBTruncaterConfig {
    dbDir         :: FilePath
  , truncateAfter :: TruncateAfter
  , blockType     :: BlockType
  , verbose       :: Bool
  }

data TruncateAfter
  = TruncateAfterSlot SlotNo
  | TruncateAfterBlock BlockNo
  deriving (Show, Eq)
