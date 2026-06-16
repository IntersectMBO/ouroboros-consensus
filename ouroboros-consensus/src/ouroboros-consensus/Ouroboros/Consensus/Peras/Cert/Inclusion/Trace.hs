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
  = -- | A certificate needs to be included in a block.
    TracePerasCertInclusionShouldIncludeCert SlotNo PerasRoundNo OpaquePerasCert
  | -- | A certificate does not need to be included in a block.
    TracePerasCertInclusionShouldNotIncludeCert SlotNo
  | -- | We failed to construct an opaque Peras certificate.
    TracePerasCertInclusionFailedToConstructOpaqueCert SlotNo PerasRoundNo OpaquePerasCertError
  deriving (Eq, Show, Generic, NoThunks)
