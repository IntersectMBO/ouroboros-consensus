{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Peras.Cert.Inclusion.Trace
  ( TracePerasCertInclusionEvent (..)
  ) where

import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (SlotNo)
import Ouroboros.Consensus.Peras.Cert.Opaque (OpaquePerasCert, OpaquePerasCertError)
import Ouroboros.Consensus.Peras.Types (PerasRoundNo)

-- | Peras certificate inclusion events.
--
-- This is useful to know when a certificate needs to be included in a block
-- to coordinate the end of a cooldown period.
data TracePerasCertInclusionEvent
  = -- | There is no latest seen certificate, so there is no certificate to
    -- possibly include in a block.
    TracePerasCertInclusionNoCertToInclude SlotNo
  | -- | A certificate needs to be included in a block.
    TracePerasCertInclusionShouldIncludeCert String SlotNo PerasRoundNo OpaquePerasCert
  | -- | A certificate does not need to be included in a block.
    TracePerasCertInclusionShouldNotIncludeCert String SlotNo
  | -- | We failed to construct an opaque Peras certificate.
    TracePerasCertInclusionFailedToConstructOpaqueCert String SlotNo PerasRoundNo OpaquePerasCertError
  deriving (Eq, Show, Generic, NoThunks)
