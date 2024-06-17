module Cardano.Tools.DBTruncater.Types (
    DBTruncaterConfig (..)
  , TruncateAfter (..)
  ) where

import           Ouroboros.Consensus.Block.Abstract

data DBTruncaterConfig = DBTruncaterConfig {
    dbDir         :: FilePath
  , truncateAfter :: TruncateAfter
  , verbose       :: Bool
  }

-- | Where to truncate the ImmutableDB.
data TruncateAfter
    -- | Truncate after the given slot number, such that the new tip has this
    -- exact slot number. Fail if this is not possible, ie no block has this
    -- slot number. If there are two blocks with the same slot number (due to
    -- EBBs), the tip will be the non-EBB.
  = TruncateAfterSlot SlotNo
    -- | Truncate after the given block number (such that the new tip has this
    -- block number).
  | TruncateAfterBlock BlockNo
  deriving (Show, Eq)
