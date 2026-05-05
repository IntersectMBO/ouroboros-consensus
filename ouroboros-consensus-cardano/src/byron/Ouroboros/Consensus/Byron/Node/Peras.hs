{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Empty Peras support for Byron.
--
-- NOTE: this module exists solely because the orphan module
-- 'Ouroboros.Consensus.Byron.Node.Serialisation' needs this instance, but
-- defining it there would be too confusing.
module Ouroboros.Consensus.Byron.Node.Peras () where

import Ouroboros.Consensus.Block.SupportsPeras (BlockSupportsPeras)
import Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)

{-------------------------------------------------------------------------------
  BlockSupportsPeras
-------------------------------------------------------------------------------}

-- NOTE: Byron does not support Peras, so we can use the empty instance here.
instance BlockSupportsPeras ByronBlock
