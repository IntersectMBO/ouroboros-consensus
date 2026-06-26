{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger.CommonProtocolParams () where

import Ouroboros.Consensus.Ledger.Basics (EmptyMK)
import Data.SOP.Functors (Flip (..))
import Data.SOP.BasicFunctors
import Data.SOP.Strict
import Ouroboros.Consensus.HardFork.Combinator.Abstract
import Ouroboros.Consensus.HardFork.Combinator.Basics
import Ouroboros.Consensus.HardFork.Combinator.Ledger ()
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import Ouroboros.Consensus.Ledger.CommonProtocolParams

instance
  CanHardFork xs =>
  CommonProtocolParams (HardForkBlock xs)
  where
  maxHeaderSize = askCurrentLedger maxHeaderSize
  maxTxSize = askCurrentLedger maxTxSize

askCurrentLedger ::
  CanHardFork xs =>
  (forall blk. CommonProtocolParams blk => LedgerState blk EmptyMK -> a) ->
  LedgerState (HardForkBlock xs) mk ->
  a
askCurrentLedger f =
  hcollapse
    . hcmap proxySingle (K . f . unFlip)
    . State.tip
    . hardForkLedgerStatePerEra
