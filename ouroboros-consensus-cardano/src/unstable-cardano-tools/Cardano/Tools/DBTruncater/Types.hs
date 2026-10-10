module Cardano.Tools.DBTruncater.Types
  ( DBTruncaterConfig (..)
  , TruncateAfter (..)
  ) where

import Ouroboros.Consensus.Block.Abstract

data DBTruncaterConfig = DBTruncaterConfig
  { dbDir :: FilePath
  , truncateAfter :: TruncateAfter
  , verbose :: Bool
  , leios :: Bool
  -- ^ 'False' skips every LeiosDb operation. The tool then neither opens nor
  -- modifies any LeiosDb file, whether or not one exists.
  --
  -- With 'True' the tool requires the LeiosDb files, because it cannot tell a
  -- pre-Leios chain from a Leios one before it reads the chain.
  --
  -- This field should be gone once Leios is deployed and we always have a
  -- LeiosDB.
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
