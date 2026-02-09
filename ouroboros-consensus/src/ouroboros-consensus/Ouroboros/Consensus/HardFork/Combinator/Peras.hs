{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Peras () where

import Data.SOP (K (..))
import Data.SOP.Functors (Flip (..))
import Data.SOP.Strict (hcmap, hcollapse)
import Ouroboros.Consensus.HardFork.Combinator.Abstract.CanHardFork (CanHardFork)
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock (proxySingle)
import Ouroboros.Consensus.HardFork.Combinator.Basics (HardForkBlock (..), LedgerState (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import Ouroboros.Consensus.Ledger.SupportsPeras (LedgerSupportsPeras (..))

instance CanHardFork xs => LedgerSupportsPeras (HardForkBlock xs) where
  getLatestPerasCertRound =
    hcollapse
      . hcmap proxySingle (K . getLatestPerasCertRound . unFlip)
      . State.tip
      . hardForkLedgerStatePerEra
