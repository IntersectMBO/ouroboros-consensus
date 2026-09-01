module Cardano.Tools.DBTruncater.Types
  ( DBTruncaterConfig (..)
  , TruncateAfter (..)
  ) where

import Ouroboros.Consensus.Block.Abstract

data DBTruncaterConfig = DBTruncaterConfig
  { dbDir :: FilePath
  , truncateAfter :: TruncateAfter
  , verbose :: Bool
  , stubbedLeiosDb :: Bool
  -- ^ Use an empty in-memory LeiosDb instead of @leios.db@ under 'dbDir'.
  --
  -- The tool cannot tell a pre-Leios chain from a Leios one, so it needs the
  -- file. This flag is how the caller says that the chain has none.
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
