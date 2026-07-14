{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Intended for qualified import
--
-- > import Ouroboros.Consensus.HardFork.Combinator.State (HardForkState(..))
-- > import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
module Ouroboros.Consensus.HardFork.Combinator.State
  ( module X

    -- * Support for defining instances
  , getTip

    -- * Serialisation support
  , recover

    -- * EpochInfo
  , epochInfoLedger
  , epochInfoPrecomputedTransitionInfo
  , mostRecentTransitionInfo
  , reconstructSummaryLedger

    -- * Ledger specific functionality
  , extendToSlot
  ) where

import Control.Monad (guard)
import Data.Functor.Product
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Counting (getExactly)
import Data.SOP.InPairs (InPairs, Requiring (..))
import qualified Data.SOP.InPairs as InPairs
import Data.SOP.Strict
import Data.SOP.Telescope (Extend (..), ScanNext (..), Telescope)
import qualified Data.SOP.Telescope as Telescope
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HardFork.Combinator.Abstract
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import Ouroboros.Consensus.HardFork.Combinator.Basics
import Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import Ouroboros.Consensus.HardFork.Combinator.State.Infra as X
import Ouroboros.Consensus.HardFork.Combinator.State.Instances as X ()
import Ouroboros.Consensus.HardFork.Combinator.State.Types as X
import Ouroboros.Consensus.HardFork.Combinator.Translation
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.Ledger.Abstract hiding (getTip)
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util
import Prelude hiding (sequence)

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

getTip ::
  forall f xs.
  CanHardFork xs =>
  (forall blk. SingleEraBlock blk => f blk -> Point blk) ->
  HardForkState f xs ->
  Point (HardForkBlock xs)
getTip getLedgerTip =
  hcollapse
    . hcmap proxySingle (K . injPoint . getLedgerTip)
    . tip
 where
  injPoint ::
    forall blk.
    SingleEraBlock blk =>
    Point blk -> Point (HardForkBlock xs)
  injPoint GenesisPoint = GenesisPoint
  injPoint (BlockPoint s h) =
    BlockPoint s $
      OneEraHash $
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
recover ::
  forall f xs.
  CanHardFork xs =>
  Telescope (K Past) f xs -> HardForkState f xs
recover =
  case isNonEmpty (Proxy @xs) of
    ProofNonEmpty{} ->
      HardForkState
        . Telescope.bihmap
          (\(Pair _ past) -> past)
          recoverCurrent
        . Telescope.scanl
          (InPairs.hpure $ ScanNext $ const $ K . pastEnd . unK)
          (K History.initBound)
 where
  recoverCurrent :: Product (K History.Bound) f blk -> Current f blk
  recoverCurrent (Pair (K prevEnd) st) =
    Current
      { currentStart = prevEnd
      , currentState = st
      }

{-------------------------------------------------------------------------------
  Reconstruct EpochInfo
-------------------------------------------------------------------------------}

mostRecentTransitionInfo ::
  All SingleEraBlock xs =>
  HardForkLedgerConfig xs ->
  HardForkState LedgerState xs ->
  TransitionInfo
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

  getTransition ::
    SingleEraBlock blk =>
    WrapPartialLedgerConfig blk ->
    K History.EraParams blk ->
    Current LedgerState blk ->
    K TransitionInfo blk
  getTransition cfg (K eraParams) Current{..} = K $
    case singleEraTransition' cfg eraParams currentStart currentState of
      Nothing -> TransitionUnknown (ledgerTipSlot currentState)
      Just e -> TransitionKnown e

reconstructSummaryLedger ::
  All SingleEraBlock xs =>
  HardForkLedgerConfig xs ->
  HardForkState LedgerState xs ->
  History.Summary xs
reconstructSummaryLedger cfg@HardForkLedgerConfig{..} st =
  reconstructSummary
    hardForkLedgerConfigShape
    (mostRecentTransitionInfo cfg st)
    st

-- | Construct 'EpochInfo' from the ledger state
--
-- NOTE: The resulting 'EpochInfo' is a snapshot only, with a limited range.
-- It should not be stored.
epochInfoLedger ::
  All SingleEraBlock xs =>
  HardForkLedgerConfig xs ->
  HardForkState LedgerState xs ->
  EpochInfo (Except PastHorizonException)
epochInfoLedger cfg st =
  History.summaryToEpochInfo $
    reconstructSummaryLedger cfg st

-- | Construct 'EpochInfo' given precomputed 'TransitionInfo'
--
-- The transition and state arguments are acquired either from a ticked ledger
-- state or a ledger view.
epochInfoPrecomputedTransitionInfo ::
  History.Shape xs ->
  TransitionInfo ->
  HardForkState f xs ->
  EpochInfo (Except PastHorizonException)
epochInfoPrecomputedTransitionInfo shape transition st =
  History.summaryToEpochInfo $
    reconstructSummary shape transition st

{-------------------------------------------------------------------------------
  Extending
-------------------------------------------------------------------------------}

-- | Extend the telescope until the specified slot is within the era at the tip.
--
-- Note that transitioning to a later era might create new values in the ledger
-- tables, therefore this function also returns the accumulated
-- @'Diff' ('HardForkBlock' xs)@.
--
-- If we are crossing no era boundaries, this whole function is a no-op that
-- only produces an empty diff, because the @Telescope.extend@ function will
-- do nothing.
--
-- If we are crossing one era boundary, the ledger tables might be populated
-- with whatever @translateLedgerStateWith@ returns.
--
-- If we are crossing multiple era boundaries, the diffs generated when crossing
-- an era boundary will be prepended to the ones produced by later era
-- boundaries and, in order to all match the resulting era, they will be
-- translated to later eras.
--
-- This means in particular that if we extend from @era1@ to @era3@ going
-- through @era2@, we will:
--
-- 1. translate the ledger state from @era1@ to @era2@, which produces a @era2@
--    ledger state together with a some set of differences.
--
-- 2. keep the @era2@ diffs aside, and translate the @era2@ ledger state,
--    which produces a @era3@ ledger state together with a set of
--    @era3@ differences.
--
-- 3. Translate the @era2@ diffs to @era3@ differences, and prepend them to the
--    ones created in the step 2.
--
-- 4. Attach the diffs resulting from step 3 to the @era3@ ledger state from
--    step 2, and return it.
extendToSlot ::
  forall xs.
  CanHardFork xs =>
  HardForkLedgerConfig xs ->
  SlotNo ->
  HardForkState LedgerState xs ->
  HardForkState (Product LedgerState WrapTickDiff) xs
extendToSlot ledgerCfg@HardForkLedgerConfig{..} slot ledgerSt@(HardForkState st) =
  HardForkState
    $ unI
    $ Telescope.extend
      ( InPairs.hcmap
          proxySingle
          ( \f -> Require $ \(K t) ->
              Extend $ \cur ->
                I $ howExtend f t cur
          )
          translateLS
      )
      ( hczipWith
          proxySingle
          (fn .: whenExtend)
          pcfgs
          (getExactly (History.getShape hardForkLedgerConfigShape))
      )
    -- In order to make this an automorphism, as required by
    -- 'Telescope.extend', we have to promote each input state to a
    -- pair with an (empty) diff alongside.
    $ hcmap
      proxySingle
      initState
      st
 where
  pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
  cfgs = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
  ei = epochInfoLedger ledgerCfg ledgerSt

  initState ::
    forall blk.
    SingleEraBlock blk =>
    Current LedgerState blk ->
    Current (Product LedgerState WrapTickDiff) blk
  initState c = c{currentState = Pair (currentState c) (WrapTickDiff (emptyTickDiff @blk))}

  -- Return the end of this era if we should transition to the next
  whenExtend ::
    SingleEraBlock blk =>
    WrapPartialLedgerConfig blk ->
    K History.EraParams blk ->
    Current (Product LedgerState a) blk ->
    (Maybe :.: K History.Bound) blk
  whenExtend pcfg (K eraParams) cur =
    let Pair curState _ = currentState cur
     in Comp $
          K <$> do
            transition <-
              singleEraTransition'
                pcfg
                eraParams
                (currentStart cur)
                curState
            let endBound =
                  History.mkUpperBound
                    eraParams
                    (currentStart cur)
                    transition
            guard (slot >= History.boundSlot endBound)
            return endBound

  howExtend ::
    BlockSupportsLedgerHD blk' =>
    TranslateLedgerState blk blk' ->
    History.Bound ->
    Current (Product LedgerState WrapTickDiff) blk ->
    (K Past blk, Current (Product LedgerState WrapTickDiff) blk')
  howExtend f currentEnd cur =
    ( K
        Past
          { pastStart = currentStart cur
          , pastEnd = currentEnd
          }
    , Current
        { currentStart = currentEnd
        , currentState =
            let Pair curState (WrapTickDiff diff) = currentState cur
                (st', diff') = translateLedgerStateWith f (History.boundEpoch currentEnd) (curState, diff)
             in Pair st' (WrapTickDiff diff')
        }
    )

  translateLS :: InPairs TranslateLedgerState xs
  translateLS =
    InPairs.requiringBoth cfgs $
      translateLedgerState hardForkEraTranslation
