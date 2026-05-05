{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Peras.Cert.Inclusion.Trace
  ( TracePerasCertInclusionEvent (..)
  ) where

import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (SlotNo)
import Ouroboros.Consensus.Block.SupportsPeras (PerasCert)
import Ouroboros.Consensus.Peras.Types (PerasRoundNo)

-- | Peras certificate inclusion events.
--
-- This is useful to know when a certificate needs to be included in a block
-- to coordinate the end of a cooldown period.
data TracePerasCertInclusionEvent blk
  = -- | There is no latest seen certificate, so there is no certificate to
    -- possibly include in a block.
    TracePerasCertInclusionNoCertToInclude SlotNo
  | -- | A certificate needs to be included in a block.
    TracePerasCertInclusionShouldIncludeCert String SlotNo PerasRoundNo (PerasCert blk)
  | -- | A certificate does not need to be included in a block.
    TracePerasCertInclusionShouldNotIncludeCert String SlotNo
  deriving Generic

deriving instance
  Show (PerasCert blk) =>
  Show (TracePerasCertInclusionEvent blk)
deriving instance
  Eq (PerasCert blk) =>
  Eq (TracePerasCertInclusionEvent blk)
deriving instance
  NoThunks (PerasCert blk) =>
  NoThunks (TracePerasCertInclusionEvent blk)
