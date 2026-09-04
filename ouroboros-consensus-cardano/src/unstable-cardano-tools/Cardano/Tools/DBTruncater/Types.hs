module Cardano.Tools.DBTruncater.Types
  ( DBTruncaterConfig (..)
  , TruncateAfter (..)
  ) where

import Cardano.Tools.LeiosDb (LeiosDbSource)
import Ouroboros.Consensus.Block.Abstract

data DBTruncaterConfig = DBTruncaterConfig
  { dbDir :: FilePath
  , truncateAfter :: TruncateAfter
  , verbose :: Bool
  , leiosDbSource :: LeiosDbSource
  -- ^ 'NoLeiosDb' skips every LeiosDb operation. The tool then neither opens
  -- nor modifies the @leios.db@ under 'dbDir', whether or not that file
  -- exists.
  --
  -- With 'NodeLeiosDb' the tool requires that file, because it cannot tell a
  -- pre-Leios chain from a Leios one before it reads the chain.
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
