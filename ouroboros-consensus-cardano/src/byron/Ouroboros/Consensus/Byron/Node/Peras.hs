{-# OPTIONS_GHC -Wno-orphans #-}

-- | Empty Peras support for Byron.
--
-- NOTE: this module exists solely because the orphan module
-- 'Ouroboros.Consensus.Byron.Node.Serialisation' needs some of these instances,
-- but defining them there would be too confusing.
module Ouroboros.Consensus.Byron.Node.Peras () where

import Ouroboros.Consensus.Block.SupportsPeras (BlockSupportsPeras)
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)

{-------------------------------------------------------------------------------
  BlockSupportsPeras
-------------------------------------------------------------------------------}

instance BlockSupportsPeras ByronBlock
