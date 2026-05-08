{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Empty Peras support for DualByron.
--
-- NOTE: this module exists solely because the orphan module
-- 'Ouroboros.Consensus.ByronDual.Node.Serialisation' needs this instance, but
-- defining it there would be too confusing.
module Ouroboros.Consensus.ByronDual.Node.Peras () where

import Ouroboros.Consensus.Block.SupportsPeras (BlockSupportsPeras)
import Ouroboros.Consensus.ByronDual.Ledger (DualByronBlock)

{-------------------------------------------------------------------------------
  BlockSupportsPeras
-------------------------------------------------------------------------------}

-- NOTE: DualByron does not support Peras, so we can use the empty instance here.
instance BlockSupportsPeras DualByronBlock
