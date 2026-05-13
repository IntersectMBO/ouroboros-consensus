{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger.CommonProtocolParams () where

import Data.SOP.BasicFunctors
import Data.SOP.Strict
import Ouroboros.Consensus.HardFork.Combinator.Abstract
import Ouroboros.Consensus.HardFork.Combinator.Basics
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
  (forall blk. CommonProtocolParams blk => LedgerState m blk -> a) ->
  LedgerState m (HardForkBlock xs) ->
  a
askCurrentLedger f =
  hcollapse
    . hcmap proxySingle (K . f)
    . State.tip
    . hardForkLedgerStatePerEra
