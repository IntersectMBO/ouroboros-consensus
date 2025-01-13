{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.State (HardForkState(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
module Ouroboros.Consensus.HardFork.Combinator.State (
    module X
    -- * Support for defining instances
  , getTip
    -- * Serialisation support
  , recover
    -- * EpochInfo
  , epochInfoLedger
  , epochInfoPrecomputedTransitionInfo
  , mostRecentTransitionInfo
  , reconstructSummaryLedger
  ) where

import           Data.Functor.Product
import           Data.Proxy
import           Data.SOP.BasicFunctors
import           Data.SOP.Constraint
import           Data.SOP.Counting (getExactly)
import qualified Data.SOP.InPairs as InPairs
import           Data.SOP.Strict
import           Data.SOP.Telescope (ScanNext (..), Telescope)
import qualified Data.SOP.Telescope as Telescope
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.State.Infra as X
import           Ouroboros.Consensus.HardFork.Combinator.State.Instances as X ()
import           Ouroboros.Consensus.HardFork.Combinator.State.Types as X
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.Ledger.Abstract hiding (getTip)
import           Prelude hiding (sequence)

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

getTip :: forall f xs. CanHardFork xs
       => (forall blk. SingleEraBlock blk => f blk -> Point blk)
       -> HardForkState f xs -> Point (HardForkBlock xs)
getTip getLedgerTip =
      hcollapse
    . hcmap proxySingle (K . injPoint . getLedgerTip)
    . tip
  where
    injPoint :: forall blk. SingleEraBlock blk
             => Point blk -> Point (HardForkBlock xs)
    injPoint GenesisPoint     = GenesisPoint
    injPoint (BlockPoint s h) = BlockPoint s $ OneEraHash $
                                  toShortRawHash (Proxy @blk) h

{-------------------------------------------------------------------------------
  Recovery
-------------------------------------------------------------------------------}

-- | Recover 'HardForkState' from partial information
--
-- The primary goal of this is to make sure that for the /current/ state we
-- really only need to store the underlying @f@. It is not strictly essential
-- that this is possible but it helps with the unary hardfork case, and it may
-- in general help with binary compatibility.
recover :: forall f xs. CanHardFork xs
        => Telescope (K Past) f xs -> HardForkState f xs
recover =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty {} ->
          HardForkState
        . Telescope.bihmap
            (\(Pair _ past) -> past)
            recoverCurrent
        . Telescope.scanl
            (InPairs.hpure $ ScanNext $ const $ K . pastEnd . unK)
            (K History.initBound)
  where
    recoverCurrent :: Product (K History.Bound) f blk -> Current f blk
    recoverCurrent (Pair (K prevEnd) st) = Current {
          currentStart = prevEnd
        , currentState = st
        }

{-------------------------------------------------------------------------------
  Reconstruct EpochInfo
-------------------------------------------------------------------------------}

mostRecentTransitionInfo :: All SingleEraBlock xs
                         => HardForkLedgerConfig xs
                         -> HardForkState LedgerState xs
                         -> TransitionInfo
mostRecentTransitionInfo HardForkLedgerConfig{..} st =
    hcollapse $
      hczipWith3
        proxySingle
        getTransition
        cfgs
        (getExactly (History.getShape hardForkLedgerConfigShape))
        (Telescope.tip (getHardForkState st))
  where
    cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra

    getTransition :: SingleEraBlock          blk
                  => WrapPartialLedgerConfig blk
                  -> K History.EraParams     blk
                  -> Current LedgerState     blk
                  -> K TransitionInfo        blk
    getTransition cfg (K eraParams) Current{..} = K $
        case singleEraTransition' cfg eraParams currentStart currentState of
          Nothing -> TransitionUnknown (ledgerTipSlot currentState)
          Just e  -> TransitionKnown e

reconstructSummaryLedger :: All SingleEraBlock xs
                         => HardForkLedgerConfig xs
                         -> HardForkState LedgerState xs
                         -> History.Summary xs
reconstructSummaryLedger cfg@HardForkLedgerConfig{..} st =
    reconstructSummary
      hardForkLedgerConfigShape
      (mostRecentTransitionInfo cfg st)
      st

-- | Construct 'EpochInfo' from the ledger state
--
-- NOTE: The resulting 'EpochInfo' is a snapshot only, with a limited range.
-- It should not be stored.
epochInfoLedger :: All SingleEraBlock xs
                => HardForkLedgerConfig xs
                -> HardForkState LedgerState xs
                -> EpochInfo (Except PastHorizonException)
epochInfoLedger cfg st =
    History.summaryToEpochInfo $
      reconstructSummaryLedger cfg st

-- | Construct 'EpochInfo' given precomputed 'TransitionInfo'
--
-- The transition and state arguments are acquired either from a ticked ledger
-- state or a ledger view.
epochInfoPrecomputedTransitionInfo ::
     History.Shape xs
  -> TransitionInfo
  -> HardForkState f xs
  -> EpochInfo (Except PastHorizonException)
epochInfoPrecomputedTransitionInfo shape transition st =
    History.summaryToEpochInfo $
      reconstructSummary shape transition st
