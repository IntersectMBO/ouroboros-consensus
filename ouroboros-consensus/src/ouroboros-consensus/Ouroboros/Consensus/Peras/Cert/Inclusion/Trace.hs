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
import Ouroboros.Consensus.Block.SupportsPeras (PerasCert, ValidatedPerasCert)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime)
import Ouroboros.Consensus.Peras.Cert.Inclusion (PerasCertInclusionRulesDecision)
import Ouroboros.Consensus.Peras.Types (PerasRoundNo)

-- | Peras certificate inclusion events.
data TracePerasCertInclusionEvent blk
  = -- | There is no latest certificate seen available to include in a block
    TracePerasCertInclusionNoCertToInclude
      -- | The current slot number
      SlotNo
  | -- | The decision made by the inclusion rules for a certificate
    TracePerasCertInclusionRulesDecision
      -- | The current slot number
      SlotNo
      -- | The current round number
      PerasRoundNo
      -- | The reason to include or not include a certificate
      (PerasCertInclusionRulesDecision (WithArrivalTime (ValidatedPerasCert blk)))

deriving instance
  Show (PerasCert blk) =>
  Show (TracePerasCertInclusionEvent blk)
deriving instance
  Eq (PerasCert blk) =>
  Eq (TracePerasCertInclusionEvent blk)
deriving instance
  NoThunks (PerasCert blk) =>
  NoThunks (TracePerasCertInclusionEvent blk)
deriving instance
  Generic (TracePerasCertInclusionEvent blk)
