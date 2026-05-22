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
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.IOLike
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
  getTransition cfg (K eraParams) Current{currentState = curState, ..} = K $
    case singleEraTransition' cfg eraParams currentStart curState of
      Nothing -> TransitionUnknown (ledgerTipSlot curState)
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
-- If the requested slot still lives within the current era, this whole
-- function is a no-op: @Telescope.extend@ has nothing to do and the
-- input 'HardForkState' is returned unchanged.
--
-- If we are crossing one or more era boundaries, each step runs the
-- corresponding 'TranslateLedgerState' in @m@: the prior era's
-- 'StateHandle' is consumed and a fresh 'StateHandle' for the next era
-- is materialised, using the 'HFLedgerTablesFactory' to construct that
-- era's 'LedgerTablesHandle'. (For UTxO-bearing transitions like
-- Byron→Shelley or Shelley→Allegra, the per-era 'TranslateLedgerState'
-- writes the appropriate inserts/deletes into the new tables handle as
-- part of its monadic action.) The result is a 'HardForkState'
-- positioned at the era containing the target slot.
extendToSlot ::
  forall m xs.
  (CanHardFork xs, MonadThrow m) =>
  HardForkLedgerConfig xs ->
  SlotNo ->
  HFLedgerTablesFactory m xs ->
  HardForkState (StateHandle m) xs ->
  m (HardForkState (StateHandle m) xs)
extendToSlot ledgerCfg@HardForkLedgerConfig{..} slot tcxt ledgerSt@(HardForkState st) =
  fmap HardForkState
    . Telescope.extend
      ( InPairs.hcmap
          proxySingle
          ( \f -> Require $ \(K t) ->
              Extend $ howExtend f t
          )
          translateLS
      )
      ( hczipWith
          proxySingle
          (fn .: whenExtend)
          pcfgs
          (getExactly (History.getShape hardForkLedgerConfigShape))
      )
    $ st
 where
  pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
  cfgs = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs
  ei = epochInfoLedger ledgerCfg (hcmap proxySingle state ledgerSt)

  -- Return the end of this era if we should transition to the next
  whenExtend ::
    SingleEraBlock blk =>
    WrapPartialLedgerConfig blk ->
    K History.EraParams blk ->
    Current (StateHandle m) blk ->
    (Maybe :.: K History.Bound) blk
  whenExtend pcfg (K eraParams) cur =
    Comp $
      K <$> do
        transition <-
          singleEraTransition'
            pcfg
            eraParams
            (currentStart cur)
            (state $ currentState cur)
        let endBound =
              History.mkUpperBound
                eraParams
                (currentStart cur)
                transition
        guard (slot >= History.boundSlot endBound)
        return endBound

  howExtend ::
    TranslateLedgerState m blk blk' ->
    History.Bound ->
    Current (StateHandle m) blk ->
    m (K Past blk, Current (StateHandle m) blk')
  howExtend f currentEnd (Current currentStart currentState) = do
    cur' <- translateLedgerStateWith f (History.boundEpoch currentEnd) $ currentState
    pure
      ( K
          Past
            { pastStart = currentStart
            , pastEnd = currentEnd
            }
      , Current
          { currentStart = currentEnd
          , currentState = cur'
          }
      )

  translateLS :: InPairs (TranslateLedgerState m) xs
  translateLS =
    InPairs.requiringBoth cfgs $
      translateLedgerState (hardForkStateHandleTranslation tcxt)
