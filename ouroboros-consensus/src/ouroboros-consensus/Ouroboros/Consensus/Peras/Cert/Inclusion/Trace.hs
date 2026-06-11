{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Peras.Cert.Inclusion.Trace
  ( TracePerasCertInclusionEvent (..)
  ) where

import Ouroboros.Consensus.Block.Abstract (SlotNo)
import Ouroboros.Consensus.Block.SupportsPeras (BlockSupportsPeras (..))
import Ouroboros.Consensus.Peras.Types (PerasRoundNo)

-- | Peras certificate inclusion events.
--
-- This is useful to know when a certificate needs to be included in a block
-- to coordinate the end of a cooldown period.
data TracePerasCertInclusionEvent blk
  = -- | A certificate needs to be included in a block.
    TracePerasCertInclusionShouldIncludeCert SlotNo PerasRoundNo (PerasCert blk)
  | -- | | A certificate does not need to be included in a block.
    TracePerasCertInclusionShouldNotIncludeCert SlotNo

deriving instance
  Eq (PerasCert blk) =>
  Eq (TracePerasCertInclusionEvent blk)
deriving instance
  Show (PerasCert blk) =>
  Show (TracePerasCertInclusionEvent blk)
