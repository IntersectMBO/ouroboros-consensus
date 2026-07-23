module Cardano.Tools.DBTruncater.Types
  ( DBTruncaterConfig (..)
  , TruncateAfter (..)
  ) where

import Ouroboros.Consensus.Block.Abstract

data DBTruncaterConfig = DBTruncaterConfig
  { dbDir :: FilePath
  , truncateAfter :: TruncateAfter
  , verbose :: Bool
  }

-- | Where to truncate the ImmutableDB.
data TruncateAfter
  = -- | Truncate after the given slot number, deleting all blocks with a higher
    -- slot number.
    TruncateAfterSlot SlotNo
  | -- | Truncate after the given block number (such that the new tip has this
    -- block number).
    TruncateAfterBlock BlockNo
  deriving (Show, Eq)
