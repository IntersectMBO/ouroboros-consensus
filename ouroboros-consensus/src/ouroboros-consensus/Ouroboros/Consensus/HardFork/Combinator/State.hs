{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Data.SOP.Functors (Flip (..))
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
import Ouroboros.Consensus.Ledger.Tables.Utils
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
  HardForkState (Flip LedgerState mk) xs ->
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
    Current (Flip LedgerState mk) blk ->
    K TransitionInfo blk
  getTransition cfg (K eraParams) Current{currentState = Flip curState, ..} = K $
    case singleEraTransition' cfg eraParams currentStart curState of
      Nothing -> TransitionUnknown (ledgerTipSlot curState)
      Just e -> TransitionKnown e

reconstructSummaryLedger ::
  All SingleEraBlock xs =>
  HardForkLedgerConfig xs ->
  HardForkState (Flip LedgerState mk) xs ->
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
  HardForkState (Flip LedgerState mk) xs ->
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
-- tables, therefore the result of this function is a @DiffMK@.
--
-- If we are crossing no era boundaries, this whole function is a no-op that
-- only creates an empty @DiffMK@, because the @Telescope.extend@ function will
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
-- 2. keep the @era2@ diffs aside, and translate the @era2@ ledger state without
--    ledger tables, which produces a @era3@ ledger state together with a set of
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
  HardForkState (Flip LedgerState EmptyMK) xs ->
  HardForkState (Flip LedgerState DiffMK) xs
extendToSlot ledgerCfg@HardForkLedgerConfig{..} slot ledgerSt@(HardForkState st) =
  HardForkState
    . unI
    . Telescope.extend
      ( InPairs.hczipWith
          proxySingle
          ( \f f' -> Require $ \(K t) ->
              Extend $ \cur ->
                I $ howExtend f f' t cur
          )
          translateLS
          translateLT
      )
      ( hczipWith
          proxySingle
          (fn .: whenExtend)
          pcfgs
          (getExactly (History.getShape hardForkLedgerConfigShape))
      )
    -- In order to make this an automorphism, as required by 'Telescope.extend',
    -- we have to promote the input to @DiffMK@ albeit it being empty.
    $ hcmap
      proxySingle
      ( \c ->
          c
            { currentState =
                Flip
                  . flip withLedgerTables emptyLedgerTables
                  . unFlip
                  . currentState
                  $ c
            }
      )
    $ st
 where
  pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
  cfgs = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
  ei = epochInfoLedger ledgerCfg ledgerSt

  -- Return the end of this era if we should transition to the next
  whenExtend ::
    SingleEraBlock blk =>
    WrapPartialLedgerConfig blk ->
    K History.EraParams blk ->
    Current (Flip LedgerState DiffMK) blk ->
    (Maybe :.: K History.Bound) blk
  whenExtend pcfg (K eraParams) cur =
    Comp $
      K <$> do
        transition <-
          singleEraTransition'
            pcfg
            eraParams
            (currentStart cur)
            (unFlip $ currentState cur)
        let endBound =
              History.mkUpperBound
                eraParams
                (currentStart cur)
                transition
        guard (slot >= History.boundSlot endBound)
        return endBound

  howExtend ::
    (HasLedgerTables (LedgerState blk), HasLedgerTables (LedgerState blk')) =>
    TranslateLedgerState blk blk' ->
    TranslateLedgerTables blk blk' ->
    History.Bound ->
    Current (Flip LedgerState DiffMK) blk ->
    (K Past blk, Current (Flip LedgerState DiffMK) blk')
  howExtend f f' currentEnd cur =
    ( K
        Past
          { pastStart = currentStart cur
          , pastEnd = currentEnd
          }
    , Current
        { currentStart = currentEnd
        , currentState =
            Flip
              -- We need to bring back the diffs provided by previous
              -- translations. Note that if there is only one translation or
              -- if the previous translations don't add any new tables this
              -- will just be a no-op. See the haddock for
              -- 'translateLedgerTablesWith' and 'extendToSlot' for more
              -- information.
              . prependDiffs
                ( translateLedgerTablesWith f'
                    . projectLedgerTables
                    . unFlip
                    . currentState
                    $ cur
                )
              . translateLedgerStateWith f (History.boundEpoch currentEnd)
              . forgetLedgerTables
              . unFlip
              . currentState
              $ cur
        }
    )

  translateLS :: InPairs TranslateLedgerState xs
  translateLS =
    InPairs.requiringBoth cfgs $
      translateLedgerState hardForkEraTranslation

  translateLT :: InPairs TranslateLedgerTables xs
  translateLT = translateLedgerTables hardForkEraTranslation
