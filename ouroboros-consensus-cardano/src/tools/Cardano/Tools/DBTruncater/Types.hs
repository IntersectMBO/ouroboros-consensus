module Cardano.Tools.DBTruncater.Types (
    DBTruncaterConfig (..)
  , TruncateAfter (..)
  ) where

import           Cardano.Tools.DBAnalyser.Types
import           Ouroboros.Consensus.Block.Abstract

data DBTruncaterConfig = DBTruncaterConfig {
    dbDir         :: FilePath
  , truncateAfter :: TruncateAfter
  , blockType     :: BlockType
  , verbose       :: Bool
  }

-- | Slot or block number of the block intended to be the new tip of the chain
-- after the ImmutableDB is truncated.
data TruncateAfter
  = TruncateAfterSlot SlotNo
  | TruncateAfterBlock BlockNo
  deriving (Show, Eq)
