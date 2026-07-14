{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger
  ( HardForkEnvelopeErr (..)
  , HardForkLedgerError (..)
  , HardForkLedgerUpdate (..)
  , HardForkLedgerWarning (..)

    -- * Type family instances
  , Ticked (..)

    -- * Low-level API (exported for the benefit of testing)
  , AnnForecast (..)
  , mkHardForkForecast
  ) where

import Codec.CBOR.Encoding (Encoding)
import Control.Monad (guard)
import Control.Monad.Except (throwError, withExcept)
import qualified Control.State.Transition.Extended as STS
import Data.Functor ((<&>))
import Data.Functor.Product
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Counting (getExactly)
import Data.SOP.InPairs (InPairs (..))
import qualified Data.SOP.InPairs as InPairs
import Data.SOP.Index
import qualified Data.SOP.Match as Match
import Data.SOP.Strict
import Data.SOP.Telescope (Telescope (..))
import qualified Data.SOP.Telescope as Telescope
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Forecast
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.HardFork.Combinator.Abstract
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import Ouroboros.Consensus.HardFork.Combinator.Basics
import Ouroboros.Consensus.HardFork.Combinator.Block
import Ouroboros.Consensus.HardFork.Combinator.Info
import Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import Ouroboros.Consensus.HardFork.Combinator.Protocol ()
import Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import Ouroboros.Consensus.HardFork.Combinator.State.Types
import Ouroboros.Consensus.HardFork.Combinator.Translation
import Ouroboros.Consensus.HardFork.History
  ( Bound (..)
  , EraParams
  , SafeZone (..)
  )
import qualified Ouroboros.Consensus.HardFork.History as History
import Ouroboros.Consensus.HeaderValidation
import Ouroboros.Consensus.Ledger.Abstract
import Ouroboros.Consensus.Ledger.Inspect
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util.Condense

-- $setup
-- >>> import Image.LaTeX.Render
-- >>> import Control.Monad
-- >>> import System.Directory
-- >>>
-- >>> createDirectoryIfMissing True "docs/haddocks/"

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data HardForkLedgerError xs
  = -- | Validation error from one of the eras
    HardForkLedgerErrorFromEra (OneEraLedgerError xs)
  | -- | We tried to apply a block from the wrong era
    HardForkLedgerErrorWrongEra (MismatchEraInfo xs)
  deriving (Generic, Show, Eq, NoThunks)

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance CanHardFork xs => GetTip (LedgerState (HardForkBlock xs)) where
  getTip =
    castPoint
      . State.getTip (castPoint . getTip)
      . hardForkLedgerStatePerEra

instance CanHardFork xs => GetTip (Ticked LedgerState (HardForkBlock xs)) where
  getTip =
    castPoint
      . State.getTip (castPoint . getTip)
      . tickedHardForkLedgerStatePerEra

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

data instance Ticked LedgerState (HardForkBlock xs)
  = TickedHardForkLedgerState
  { tickedHardForkLedgerStateTransition :: !TransitionInfo
  , tickedHardForkLedgerStatePerEra ::
      !(HardForkState (Ticked LedgerState) xs)
  }

type instance AuxLedgerEvent (HardForkBlock xs) = OneEraLedgerEvent xs

instance CanHardFork xs => IsLedger LedgerState (HardForkBlock xs) where
  type LedgerErr LedgerState (HardForkBlock xs) = HardForkLedgerError xs

  applyChainTickLedgerResult evs cfg@HardForkLedgerConfig{..} slot (HardForkLedgerState st0) =
    sequenceHardForkState
      ( hcizipWith
          proxySingle
          (tickOne ei slot evs)
          cfgs
          extended
      )
      <&> \tickedAndDiffs ->
        ( TickedHardForkLedgerState
            { -- We derive the 'TransitionInfo' from the /unticked/ (but
              -- extended) state; see the long note on this in the V2 design.
              tickedHardForkLedgerStateTransition =
                State.mostRecentTransitionInfo cfg $ hmap (\(Pair st _) -> st) extended
            , tickedHardForkLedgerStatePerEra =
                hmap (\(Pair ticked _) -> ticked) tickedAndDiffs
            }
        , State.tip (hmap (\(Pair _ tickDiff) -> tickDiff) tickedAndDiffs)
        )
   where
    cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
    ei = State.epochInfoLedger cfg st0

    extended :: HardForkState (Product LedgerState WrapTickDiff) xs
    extended = State.extendToSlot cfg slot st0

-- | Tick the ledger state of a single era, pairing the ticked state with the
-- diff that ticking produced (re-tagging the per-era ledger events as
-- hard-fork events).
tickOne ::
  forall blk xs.
  (SListI xs, SingleEraBlock blk) =>
  EpochInfo (Except PastHorizonException) ->
  SlotNo ->
  ComputeLedgerEvents ->
  Index xs blk ->
  WrapPartialLedgerConfig blk ->
  Product LedgerState WrapTickDiff blk ->
  (LedgerResult (HardForkBlock xs) :.: Product (Ticked LedgerState) WrapTickDiff) blk
tickOne ei slot evs sopIdx partialCfg (Pair st (WrapTickDiff transDiff)) =
  Comp
    $ fmap
      ( \(ticked, tickDiff) ->
          Pair
            ticked
            (WrapTickDiff $ combineTransAndTickDiff @blk transDiff tickDiff)
      )
    $ embedLedgerResult (injectLedgerEvent sopIdx)
    $ applyChainTickLedgerResult evs (completeLedgerConfig' ei partialCfg) slot st

{-------------------------------------------------------------------------------
  ApplyBlock
-------------------------------------------------------------------------------}

instance
  CanHardFork xs =>
  ApplyBlock LedgerState (HardForkBlock xs)
  where
  applyBlockLedgerResultWithValidation
    doValidate
    opts
    cfg
    (HardForkBlock (OneEraBlock block))
    values
    (TickedHardForkLedgerState transition st) =
      -- The values are read for the block's era, so they always align with the
      -- block; align block+values first, then align with the ticked telescope.
      case Match.matchNS block values of
        Left _ ->
          error
            "applyBlockLedgerResultWithValidation: values not read for the block's era"
        Right blockValues ->
          case State.match blockValues st of
            Left mismatch ->
              -- Block from the wrong era (note that 'applyChainTick' will
              -- already have initiated the transition to the next era if
              -- appropriate).
              throwError $
                HardForkLedgerErrorWrongEra . MismatchEraInfo $
                  Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
            Right matched ->
              fmap (fmap reassemble . sequenceHardForkState) $
                hsequence' $
                  hcizipWith proxySingle (apply doValidate opts) cfgs matched
     where
      cfgs = distribLedgerConfig ei cfg
      ei =
        State.epochInfoPrecomputedTransitionInfo
          (hardForkLedgerConfigShape cfg)
          transition
          st

      reassemble ::
        HardForkState (Product LedgerState WrapBlockDiff) xs ->
        (LedgerState (HardForkBlock xs), NS WrapBlockDiff xs)
      reassemble hs =
        ( HardForkLedgerState (hmap (\(Pair s _) -> s) hs)
        , State.tip (hmap (\(Pair _ d) -> d) hs)
        )

  applyBlockLedgerResult = defaultApplyBlockLedgerResult

  reapplyBlockLedgerResult =
    defaultReapplyBlockLedgerResult
      ( \_ ->
          -- We already applied this block to this ledger state,
          -- so it can't be from the wrong era
          error "reapplyBlockLedgerResult: can't be from other era"
      )

-- | Apply a block to the ticked state of a single era with its read values,
-- pairing the resulting state with the diff it produced (re-tagging the
-- per-era ledger error and events as hard-fork ones).
apply ::
  (SListI xs, SingleEraBlock blk) =>
  STS.ValidationPolicy ->
  ComputeLedgerEvents ->
  Index xs blk ->
  WrapLedgerConfig blk ->
  Product (Product I WrapValues) (Ticked LedgerState) blk ->
  ( Except (HardForkLedgerError xs)
      :.: LedgerResult (HardForkBlock xs)
      :.: Product LedgerState WrapBlockDiff
  )
    blk
apply doValidate opts index (WrapLedgerConfig cfg) (Pair (Pair (I block) (WrapValues values)) tickedSt) =
  Comp
    $ withExcept (injectLedgerError index)
    $ fmap
      ( Comp
          . fmap (\(st', diff) -> Pair st' (WrapBlockDiff diff))
          . embedLedgerResult (injectLedgerEvent index)
      )
    $ applyBlockLedgerResultWithValidation doValidate opts cfg block values tickedSt

{-------------------------------------------------------------------------------
  UpdateLedger
-------------------------------------------------------------------------------}

instance ApplyBlock LedgerState (HardForkBlock xs) => UpdateLedger (HardForkBlock xs)

{-------------------------------------------------------------------------------
  HasHardForkHistory
-------------------------------------------------------------------------------}

instance All SingleEraBlock xs => HasHardForkHistory (HardForkBlock xs) where
  type HardForkIndices (HardForkBlock xs) = xs

  hardForkSummary cfg =
    State.reconstructSummaryLedger cfg
      . hardForkLedgerStatePerEra

{-------------------------------------------------------------------------------
  HeaderValidation
-------------------------------------------------------------------------------}

data HardForkEnvelopeErr xs
  = -- | Validation error from one of the eras
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr xs)
  | -- | We tried to apply a block from the wrong era
    HardForkEnvelopeErrWrongEra (MismatchEraInfo xs)
  deriving (Eq, Show, Generic, NoThunks)

instance CanHardFork xs => ValidateEnvelope (HardForkBlock xs) where
  type OtherHeaderEnvelopeError (HardForkBlock xs) = HardForkEnvelopeErr xs

  additionalEnvelopeChecks
    tlc
    (HardForkLedgerView transition hardForkView) =
      \(HardForkHeader (OneEraHeader hdr)) ->
        case Match.matchNS hdr (State.tip hardForkView) of
          Left mismatch ->
            throwError $
              HardForkEnvelopeErrWrongEra . MismatchEraInfo $
                Match.bihcmap proxySingle singleEraInfo ledgerViewInfo mismatch
          Right matched ->
            hcollapse $ hcizipWith proxySingle aux cfgs matched
     where
      ei :: EpochInfo (Except PastHorizonException)
      ei =
        State.epochInfoPrecomputedTransitionInfo
          (hardForkLedgerConfigShape $ configLedger tlc)
          transition
          hardForkView

      cfgs :: NP TopLevelConfig xs
      cfgs = distribTopLevelConfig ei tlc

      aux ::
        forall blk.
        SingleEraBlock blk =>
        Index xs blk ->
        TopLevelConfig blk ->
        Product Header WrapLedgerView blk ->
        K (Except (HardForkEnvelopeErr xs) ()) blk
      aux index cfg (Pair hdr view) =
        K $
          withExcept injErr' $
            additionalEnvelopeChecks
              cfg
              (unwrapLedgerView view)
              hdr
       where
        injErr' :: OtherHeaderEnvelopeError blk -> HardForkEnvelopeErr xs
        injErr' =
          HardForkEnvelopeErrFromEra
            . OneEraEnvelopeErr
            . injectNS index
            . WrapEnvelopeErr

{-------------------------------------------------------------------------------
  LedgerSupportsProtocol
-------------------------------------------------------------------------------}

instance
  CanHardFork xs =>
  LedgerSupportsProtocol (HardForkBlock xs)
  where
  protocolLedgerView
    HardForkLedgerConfig{..}
    (TickedHardForkLedgerState transition ticked) =
      HardForkLedgerView
        { hardForkLedgerViewTransition = transition
        , hardForkLedgerViewPerEra =
            hczipWith proxySingle viewOne cfgs ticked
        }
     where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      ei =
        State.epochInfoPrecomputedTransitionInfo
          hardForkLedgerConfigShape
          transition
          ticked

      viewOne ::
        SingleEraBlock blk =>
        WrapPartialLedgerConfig blk ->
        TickedLedgerState blk ->
        WrapLedgerView blk
      viewOne cfg st =
        WrapLedgerView $
          protocolLedgerView (completeLedgerConfig' ei cfg) st

  ledgerViewForecastAt
    ledgerCfg@HardForkLedgerConfig{..}
    (HardForkLedgerState ledgerSt) =
      mkHardForkForecast
        (InPairs.requiringBoth cfgs $ crossEraForecast hardForkEraTranslation)
        annForecast
     where
      ei = State.epochInfoLedger ledgerCfg ledgerSt
      pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      cfgs = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs

      annForecast :: HardForkState (AnnForecast LedgerState WrapLedgerView) xs
      annForecast =
        HardForkState $
          hczipWith3
            proxySingle
            forecastOne
            pcfgs
            (getExactly (History.getShape hardForkLedgerConfigShape))
            (getHardForkState ledgerSt)

      forecastOne ::
        forall blk.
        SingleEraBlock blk =>
        WrapPartialLedgerConfig blk ->
        K EraParams blk ->
        Current LedgerState blk ->
        Current (AnnForecast LedgerState WrapLedgerView) blk
      forecastOne cfg (K params) (Current start st) =
        Current
          { currentStart = start
          , currentState =
              AnnForecast
                { annForecast =
                    mapForecast WrapLedgerView $
                      ledgerViewForecastAt cfg' st
                , annForecastState = st
                , annForecastTip = ledgerTipSlot st
                , annForecastEnd =
                    History.mkUpperBound params start
                      <$> singleEraTransition' cfg params start st
                }
          }
       where
        cfg' :: LedgerConfig blk
        cfg' = completeLedgerConfig' ei cfg

{-------------------------------------------------------------------------------
  Annotated forecasts
-------------------------------------------------------------------------------}

-- | Forecast annotated with details about the ledger it was derived from
data AnnForecast state view blk = AnnForecast
  { annForecast :: Forecast (view blk)
  , annForecastState :: state blk
  , annForecastTip :: WithOrigin SlotNo
  , annForecastEnd :: Maybe Bound
  }

-- | Change a telescope of a forecast into a forecast of a telescope
mkHardForkForecast ::
  forall state view xs.
  SListI xs =>
  InPairs (CrossEraForecaster state view) xs ->
  HardForkState (AnnForecast state view) xs ->
  Forecast (HardForkLedgerView_ view xs)
mkHardForkForecast translations st =
  Forecast
    { forecastAt = hcollapse (hmap (K . forecastAt . annForecast) st)
    , forecastFor = \sno -> go sno translations (getHardForkState st)
    }
 where
  go ::
    SlotNo ->
    InPairs (CrossEraForecaster state view) xs' ->
    Telescope (K Past) (Current (AnnForecast state view)) xs' ->
    Except OutsideForecastRange (HardForkLedgerView_ view xs')
  go sno pairs (TZ cur) = oneForecast sno pairs cur
  go sno (PCons _ ts) (TS past rest) = shiftView past <$> go sno ts rest

oneForecast ::
  forall state view blk blks.
  SlotNo ->
  -- | this function uses at most the first translation
  InPairs (CrossEraForecaster state view) (blk : blks) ->
  Current (AnnForecast state view) blk ->
  Except OutsideForecastRange (HardForkLedgerView_ view (blk : blks))
oneForecast sno pairs (Current start AnnForecast{..}) =
  case annForecastEnd of
    Nothing -> endUnknown <$> forecastFor annForecast sno
    Just end ->
      if sno < boundSlot end
        then beforeKnownEnd end <$> forecastFor annForecast sno
        else case pairs of
          PCons translate _ ->
            afterKnownEnd end
              <$> crossEraForecastWith translate end sno annForecastState
          PNil ->
            -- The requested slot is after the last era the code knows about.
            throwError
              OutsideForecastRange
                { outsideForecastAt = forecastAt annForecast
                , outsideForecastMaxFor = boundSlot end
                , outsideForecastFor = sno
                }
 where
  endUnknown ::
    f blk ->
    HardForkLedgerView_ f (blk : blks)
  endUnknown view =
    HardForkLedgerView
      { hardForkLedgerViewTransition =
          TransitionUnknown annForecastTip
      , hardForkLedgerViewPerEra =
          HardForkState $
            TZ (Current start view)
      }

  beforeKnownEnd ::
    Bound ->
    f blk ->
    HardForkLedgerView_ f (blk : blks)
  beforeKnownEnd end view =
    HardForkLedgerView
      { hardForkLedgerViewTransition =
          TransitionKnown (boundEpoch end)
      , hardForkLedgerViewPerEra =
          HardForkState $
            TZ (Current start view)
      }

  afterKnownEnd ::
    Bound ->
    f blk' ->
    HardForkLedgerView_ f (blk : blk' : blks')
  afterKnownEnd end view =
    HardForkLedgerView
      { hardForkLedgerViewTransition =
          -- We assume that we only ever have to translate to the /next/ era
          -- (as opposed to /any/ subsequent era)
          TransitionImpossible
      , hardForkLedgerViewPerEra =
          HardForkState $
            TS (K (Past start end)) $
              TZ (Current end view)
      }

shiftView ::
  K Past blk ->
  HardForkLedgerView_ f blks ->
  HardForkLedgerView_ f (blk : blks)
shiftView past HardForkLedgerView{..} =
  HardForkLedgerView
    { hardForkLedgerViewTransition = hardForkLedgerViewTransition
    , hardForkLedgerViewPerEra =
        HardForkState
          . TS past
          . getHardForkState
          $ hardForkLedgerViewPerEra
    }

{-------------------------------------------------------------------------------
  Inspection
-------------------------------------------------------------------------------}

data HardForkLedgerWarning xs
  = -- | Warning from the underlying era
    HardForkWarningInEra (OneEraLedgerWarning xs)
  | -- | The transition to the next era does not match the 'EraParams'
    --
    -- The 'EraParams' can specify a lower bound on when the transition to the
    -- next era will happen. If the actual transition, when confirmed, is
    -- /before/ this lower bound, the node is misconfigured and will likely
    -- not work correctly. This should be taken care of as soon as possible
    -- (before the transition happens).
    HardForkWarningTransitionMismatch (EraIndex xs) EraParams EpochNo
  | -- | Transition in the final era
    --
    -- The final era should never confirm any transitions. For clarity, we also
    -- record the index of that final era.
    HardForkWarningTransitionInFinalEra (EraIndex xs) EpochNo
  | -- | An already-confirmed transition got un-confirmed
    HardForkWarningTransitionUnconfirmed (EraIndex xs)
  | -- | An already-confirmed transition got changed
    --
    -- We record the indices of the era we are transitioning from and to,
    -- as well as the old and new 'EpochNo' of that transition, in that order.
    HardForkWarningTransitionReconfirmed (EraIndex xs) (EraIndex xs) EpochNo EpochNo

data HardForkLedgerUpdate xs
  = HardForkUpdateInEra (OneEraLedgerUpdate xs)
  | -- | Hard fork transition got confirmed
    HardForkUpdateTransitionConfirmed (EraIndex xs) (EraIndex xs) EpochNo
  | -- | Hard fork transition happened
    --
    -- We record the 'EpochNo' at the start of the era after the transition
    HardForkUpdateTransitionDone (EraIndex xs) (EraIndex xs) EpochNo
  | -- | The hard fork transition rolled back
    HardForkUpdateTransitionRolledBack (EraIndex xs) (EraIndex xs)

deriving instance CanHardFork xs => Show (HardForkLedgerWarning xs)
deriving instance CanHardFork xs => Eq (HardForkLedgerWarning xs)

deriving instance CanHardFork xs => Show (HardForkLedgerUpdate xs)
deriving instance CanHardFork xs => Eq (HardForkLedgerUpdate xs)

instance CanHardFork xs => Condense (HardForkLedgerUpdate xs) where
  condense (HardForkUpdateInEra (OneEraLedgerUpdate update)) =
    hcollapse $ hcmap proxySingle (K . condense . unwrapLedgerUpdate) update
  condense (HardForkUpdateTransitionConfirmed ix ix' t) =
    "confirmed " ++ condense (ix, ix', t)
  condense (HardForkUpdateTransitionDone ix ix' e) =
    "done " ++ condense (ix, ix', e)
  condense (HardForkUpdateTransitionRolledBack ix ix') =
    "rolled back " ++ condense (ix, ix')

instance CanHardFork xs => InspectLedger (HardForkBlock xs) where
  type LedgerWarning (HardForkBlock xs) = HardForkLedgerWarning xs
  type LedgerUpdate (HardForkBlock xs) = HardForkLedgerUpdate xs

  inspectLedger
    cfg
    (HardForkLedgerState before)
    (HardForkLedgerState after) =
      inspectHardForkLedger
        pcfgs
        (getExactly shape)
        cfgs
        (Telescope.tip (getHardForkState before))
        (Telescope.tip (getHardForkState after))
     where
      HardForkLedgerConfig{..} = configLedger cfg

      pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      shape = History.getShape hardForkLedgerConfigShape
      cfgs = distribTopLevelConfig ei cfg
      ei = State.epochInfoLedger (configLedger cfg) after

inspectHardForkLedger ::
  CanHardFork xs =>
  NP WrapPartialLedgerConfig xs ->
  NP (K EraParams) xs ->
  NP TopLevelConfig xs ->
  NS (Current LedgerState) xs ->
  NS (Current LedgerState) xs ->
  [LedgerEvent (HardForkBlock xs)]
inspectHardForkLedger = go
 where
  go ::
    All SingleEraBlock xs =>
    NP WrapPartialLedgerConfig xs ->
    NP (K EraParams) xs ->
    NP TopLevelConfig xs ->
    NS (Current LedgerState) xs ->
    NS (Current LedgerState) xs ->
    [LedgerEvent (HardForkBlock xs)]

  go (pc :* _) (K ps :* pss) (c :* _) (Z before) (Z after) =
    concat
      [ map liftEvent $
          inspectLedger
            c
            (currentState before)
            (currentState after)
      , case (pss, confirmedBefore, confirmedAfter) of
          (_, Nothing, Nothing) ->
            []
          (_, Just _, Nothing) ->
            -- TODO: This should be a warning, but this can currently happen
            -- in Byron.
            []
          -- return $ LedgerWarning $
          --   HardForkWarningTransitionUnconfirmed eraIndexZero
          (Nil, Nothing, Just transition) ->
            return $
              LedgerWarning $
                HardForkWarningTransitionInFinalEra eraIndexZero transition
          (Nil, Just transition, Just transition') -> do
            -- Only warn if the transition has changed
            guard (transition /= transition')
            return $
              LedgerWarning $
                HardForkWarningTransitionInFinalEra eraIndexZero transition
          ((:*){}, Nothing, Just transition) ->
            return $
              if validLowerBound (History.eraSafeZone ps)
                then
                  LedgerUpdate $
                    HardForkUpdateTransitionConfirmed
                      eraIndexZero
                      (eraIndexSucc eraIndexZero)
                      transition
                else
                  LedgerWarning $
                    HardForkWarningTransitionMismatch
                      eraIndexZero
                      ps
                      transition
          ((:*){}, Just transition, Just transition') -> do
            guard (transition /= transition')
            return $
              LedgerWarning $
                HardForkWarningTransitionReconfirmed
                  eraIndexZero
                  (eraIndexSucc eraIndexZero)
                  transition
                  transition'
      ]
   where
    confirmedBefore, confirmedAfter :: Maybe EpochNo
    confirmedBefore =
      singleEraTransition
        (unwrapPartialLedgerConfig pc)
        ps
        (currentStart before)
        (currentState before)
    confirmedAfter =
      singleEraTransition
        (unwrapPartialLedgerConfig pc)
        ps
        (currentStart after)
        (currentState after)
  go Nil _ _ before _ =
    case before of {}
  go (_ :* pcs) (_ :* pss) (_ :* cs) (S before) (S after) =
    map shiftEvent $ go pcs pss cs before after
  go _ _ _ (Z _) (S after) =
    return $
      LedgerUpdate $
        HardForkUpdateTransitionDone
          eraIndexZero
          (eraIndexSucc $ eraIndexFromNS after)
          (hcollapse $ hmap (K . boundEpoch . currentStart) after)
  go _ _ _ (S before) (Z _) =
    return $
      LedgerUpdate $
        HardForkUpdateTransitionRolledBack
          (eraIndexSucc $ eraIndexFromNS before)
          eraIndexZero

  validLowerBound :: SafeZone -> Bool
  validLowerBound (StandardSafeZone _) = True
  validLowerBound UnsafeIndefiniteSafeZone = False

{-------------------------------------------------------------------------------
  Internal auxiliary: lifting and shifting events
-------------------------------------------------------------------------------}

liftEvent ::
  LedgerEvent x ->
  LedgerEvent (HardForkBlock (x ': xs))
liftEvent (LedgerWarning warning) = LedgerWarning $ liftWarning warning
liftEvent (LedgerUpdate update) = LedgerUpdate $ liftUpdate update

liftWarning :: LedgerWarning x -> HardForkLedgerWarning (x ': xs)
liftWarning =
  HardForkWarningInEra
    . OneEraLedgerWarning
    . Z
    . WrapLedgerWarning

liftUpdate :: LedgerUpdate x -> HardForkLedgerUpdate (x ': xs)
liftUpdate =
  HardForkUpdateInEra
    . OneEraLedgerUpdate
    . Z
    . WrapLedgerUpdate

shiftEvent ::
  LedgerEvent (HardForkBlock xs) ->
  LedgerEvent (HardForkBlock (x ': xs))
shiftEvent (LedgerWarning warning) = LedgerWarning $ shiftWarning warning
shiftEvent (LedgerUpdate update) = LedgerUpdate $ shiftUpdate update

shiftWarning :: HardForkLedgerWarning xs -> HardForkLedgerWarning (x ': xs)
shiftWarning = go
 where
  go (HardForkWarningInEra (OneEraLedgerWarning warning)) =
    HardForkWarningInEra
      (OneEraLedgerWarning (S warning))
  go (HardForkWarningTransitionMismatch ix ps t) =
    HardForkWarningTransitionMismatch
      (eraIndexSucc ix)
      ps
      t
  go (HardForkWarningTransitionInFinalEra ix t) =
    HardForkWarningTransitionInFinalEra
      (eraIndexSucc ix)
      t
  go (HardForkWarningTransitionUnconfirmed ix) =
    HardForkWarningTransitionUnconfirmed
      (eraIndexSucc ix)
  go (HardForkWarningTransitionReconfirmed ix ix' t t') =
    HardForkWarningTransitionReconfirmed
      (eraIndexSucc ix)
      (eraIndexSucc ix')
      t
      t'

shiftUpdate :: HardForkLedgerUpdate xs -> HardForkLedgerUpdate (x ': xs)
shiftUpdate = go
 where
  go :: HardForkLedgerUpdate xs -> HardForkLedgerUpdate (x ': xs)
  go (HardForkUpdateInEra (OneEraLedgerUpdate update)) =
    HardForkUpdateInEra
      (OneEraLedgerUpdate (S update))
  go (HardForkUpdateTransitionConfirmed ix ix' t) =
    HardForkUpdateTransitionConfirmed
      (eraIndexSucc ix)
      (eraIndexSucc ix')
      t
  go (HardForkUpdateTransitionDone ix ix' e) =
    HardForkUpdateTransitionDone
      (eraIndexSucc ix)
      (eraIndexSucc ix')
      e
  go (HardForkUpdateTransitionRolledBack ix ix') =
    HardForkUpdateTransitionRolledBack
      (eraIndexSucc ix)
      (eraIndexSucc ix')

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

ledgerInfo ::
  forall blk.
  SingleEraBlock blk =>
  Current (Ticked LedgerState) blk -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

ledgerViewInfo ::
  forall blk f.
  SingleEraBlock blk =>
  f blk -> LedgerEraInfo blk
ledgerViewInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

injectLedgerError :: SListI xs => Index xs blk -> LedgerError blk -> HardForkLedgerError xs
injectLedgerError index =
  HardForkLedgerErrorFromEra
    . OneEraLedgerError
    . injectNS index
    . WrapLedgerErr

injectLedgerEvent ::
  SListI xs => Index xs blk -> AuxLedgerEvent blk -> OneEraLedgerEvent xs
injectLedgerEvent index =
  OneEraLedgerEvent
    . injectNS index
    . WrapLedgerEvent

{-------------------------------------------------------------------------------
  UTxO-HD tables for the n-ary HardForkBlock

  A 'HardForkBlock' belongs to exactly one era, so the on-disk table payloads
  are era-tagged: the keys/values/diff are an 'NS' over the per-era payloads.
  There is no canonical key and no era-sum value (contrast the pre-redesign
  'CanonicalTxIn' / 'HardForkTxOut').
-------------------------------------------------------------------------------}

-- | Same-era composition of hard-fork diffs. The only diffs combined with '<>'
-- are the tick diff and the block diff of one block application
-- ('tickThenApplyLedgerResult'), which always share that block's era, so a
-- genuine cross-era composition cannot arise. (There is deliberately no
-- 'Monoid' instance: there is no canonical empty era for 'mempty', and
-- 'BlockSupportsUTxOHD' only requires 'Semigroup'.)
instance CanHardFork xs => Semigroup (NS WrapTxsDiff xs) where
  a <> b = case Match.matchNS a b of
    Right matched -> hcmap proxySingle combine matched
    Left _ ->
      error "Diff (HardForkBlock) <>: cross-era composition (impossible)"
   where
    combine ::
      forall blk.
      SingleEraBlock blk =>
      Product WrapTxsDiff WrapTxsDiff blk ->
      WrapTxsDiff blk
    combine (Pair (WrapTxsDiff x) (WrapTxsDiff y)) = WrapTxsDiff (x <> y)

-- | Same-era union of hard-fork key sets. Only ever combines the keys of
-- transactions in the mempool, which all share the current era, so a cross-era
-- union cannot arise.
instance CanHardFork xs => Semigroup (NS WrapKeys xs) where
  a <> b = case Match.matchNS a b of
    Right matched -> hcmap proxySingle combine matched
    Left _ ->
      error "Keys (HardForkBlock) <>: cross-era union (impossible)"
   where
    combine ::
      forall blk.
      SingleEraBlock blk =>
      Product WrapKeys WrapKeys blk ->
      WrapKeys blk
    combine (Pair (WrapKeys x) (WrapKeys y)) = WrapKeys (x <> y)

instance CanHardFork xs => BlockSupportsUTxOHD (HardForkBlock xs) where
  type Keys (HardForkBlock xs) = NS WrapKeys xs
  type Values (HardForkBlock xs) = NS WrapValues xs
  type TickDiff (HardForkBlock xs) = NS WrapTickDiff xs
  type BlockDiff (HardForkBlock xs) = NS WrapBlockDiff xs
  type TickAndBlockDiff (HardForkBlock xs) = NS WrapTickAndBlockDiff xs
  type TxsDiff (HardForkBlock xs) = NS WrapTxsDiff xs

  -- The block is in exactly one era; tag its era's keys.
  blockKeys =
    hcmap proxySingle (\(I blk) -> WrapKeys (blockKeys blk))
      . getOneEraBlock
      . getHardForkBlock

  combineTickAndBlockDiff tickDiff blockDiff =
    case Match.matchNS tickDiff blockDiff of
      Left _ -> error "combineTickAndBlockDiff: cross-era call!"
      Right nspair ->
        hcmap
          proxySingle
          f
          nspair
   where
    f ::
      forall blk.
      BlockSupportsUTxOHD blk =>
      Product WrapTickDiff WrapBlockDiff blk ->
      WrapTickAndBlockDiff blk
    f (Pair (WrapTickDiff tdiff) (WrapBlockDiff bdiff)) =
      WrapTickAndBlockDiff $ combineTickAndBlockDiff @blk tdiff bdiff

  forwardTickDiff = forwardDiff @WrapTickDiff
  forwardBlockDiff = forwardDiff @WrapBlockDiff
  forwardTickAndBlockDiff = forwardDiff @WrapTickAndBlockDiff
  forwardTxsDiff = forwardDiff @WrapTxsDiff

  -- Restrict the values to the keys. The common case is same-era (keys and
  -- values carry the same era tag), so we match them and restrict per-era.
  -- Across an era boundary either side can be the older one:
  --
  --  * the keys can be older than the values, e.g. the mempool reads a
  --    transaction's input keys (tagged at the transaction's era) against the
  --    values held by the backing store at the newer ledger tip; or
  --  * the values can be older than the keys, e.g. the keys are taken at the
  --    tip but the values were read from the backing store at the previous era.
  --
  -- We upgrade whichever side is older up to the other's era (using the per-era
  -- 'translateKeys'\/'translateValues' carried on the 'CanHardFork' class,
  -- mirroring 'forward') before matching and restricting.
  restrictValues keys0 vals0 = go keys0 vals0
   where
    tk = translateKeys (hardForkEraTranslation @xs)
    tv = translateValues (hardForkEraTranslation @xs)

    go :: NS WrapKeys xs -> NS WrapValues xs -> NS WrapValues xs
    go keys vals
      | iK == iV =
          case Match.matchNS keys vals of
            Right matched -> hcmap proxySingle restrictOne matched
            Left _ -> error "restrictValues: matchNS failed at equal era index"
      | iK < iV = go (liftKeysOneEra tk keys) vals
      | otherwise = go keys (liftValuesOneEra tv vals)
     where
      iK = index_NS keys
      iV = index_NS vals

    restrictOne ::
      forall blk.
      SingleEraBlock blk =>
      Product WrapKeys WrapValues blk ->
      WrapValues blk
    restrictOne (Pair (WrapKeys k) (WrapValues v)) =
      WrapValues (restrictValues @blk k v)

  valuesSize = hcollapse . hcmap proxySingle f
   where
    f :: forall blk. SingleEraBlock blk => WrapValues blk -> K Int blk
    f (WrapValues v) = K (valuesSize @blk v)

  -- No on-disk era tag: encode the current era's arm directly (the 'NS' itself
  -- carries the era).
  encodeValues = hcollapse . hcmap proxySingle enc
   where
    enc :: forall blk. SingleEraBlock blk => WrapValues blk -> K Encoding blk
    enc (WrapValues v) = K (encodeValues @blk v)

  -- There is no on-disk era tag (see 'encodeValues'), so use the era of the
  -- already-loaded ledger state to pick which arm to decode into.
  decodeValues (HardForkLedgerState st) =
    hcollapse $
      hcimap
        proxySingle
        (\idx eraSt -> K (injectNS idx . WrapValues <$> decodeValues eraSt))
        (State.tip st)

-- | Upgrade an era-tagged 'Values' one era forward, using the adjacent
-- 'translateValues'. Used by the hard-fork 'forward' to lift values read
-- against the previous era up to a boundary block's era (one step per call).
liftValuesOneEra ::
  InPairs TranslateValues xs ->
  NS WrapValues xs ->
  NS WrapValues xs
liftValuesOneEra (PCons t _) (Z (WrapValues v)) =
  S (Z (WrapValues (translateValuesWith t v)))
liftValuesOneEra (PCons _ ts) (S v) = S (liftValuesOneEra ts v)
liftValuesOneEra PNil v = v

-- | Upgrade an era-tagged 'Keys' one era forward, using the adjacent
-- 'translateKeys'. Used by the hard-fork 'restrictValues' to lift keys read
-- against an older era up to the era of the values being restricted (one step
-- per call).
liftKeysOneEra ::
  InPairs TranslateKeys xs ->
  NS WrapKeys xs ->
  NS WrapKeys xs
liftKeysOneEra (PCons t _) (Z (WrapKeys k)) =
  S (Z (WrapKeys (translateKeysWith t k)))
liftKeysOneEra (PCons _ ts) (S v) = S (liftKeysOneEra ts v)
liftKeysOneEra PNil v = v

-----

class ForwardDiffHelper diff where
  forwardDiffHelper :: SingleEraBlock blk => diff blk -> Values blk -> Values blk
  forwardDiffHelperString :: proxy diff -> String

instance ForwardDiffHelper WrapTickDiff where
  forwardDiffHelper :: forall blk. SingleEraBlock blk => WrapTickDiff blk -> Values blk -> Values blk
  forwardDiffHelper (WrapTickDiff diff) = forwardTickDiff @blk diff
  forwardDiffHelperString _ = "TickDiff"
instance ForwardDiffHelper WrapBlockDiff where
  forwardDiffHelper :: forall blk. SingleEraBlock blk => WrapBlockDiff blk -> Values blk -> Values blk
  forwardDiffHelper (WrapBlockDiff diff) = forwardBlockDiff @blk diff
  forwardDiffHelperString _ = "BlockDiff"
instance ForwardDiffHelper WrapTickAndBlockDiff where
  forwardDiffHelper ::
    forall blk. SingleEraBlock blk => WrapTickAndBlockDiff blk -> Values blk -> Values blk
  forwardDiffHelper (WrapTickAndBlockDiff diff) = forwardTickAndBlockDiff @blk diff
  forwardDiffHelperString _ = "TickAndBlockDiff"
instance ForwardDiffHelper WrapTxsDiff where
  forwardDiffHelper :: forall blk. SingleEraBlock blk => WrapTxsDiff blk -> Values blk -> Values blk
  forwardDiffHelper (WrapTxsDiff diff) = forwardTxsDiff @blk diff
  forwardDiffHelperString _ = "TxsDiff"

-- | Apply the diffs to the values.
--
-- The common case is same-era (the diff and the values carry the same
-- era tag), so we match them and apply the per-era 'forward'. The
-- rare boundary case (the block is the first of a new era, so its
-- values were read against the previous era's state) upgrades the
-- values up to the diff's era using the per-era 'translateValues'
-- carried on the 'CanHardFork' class, then applies. No 'LedgerConfig'
-- is needed.
forwardDiff ::
  forall diff xs.
  (CanHardFork xs, ForwardDiffHelper diff) =>
  NS diff xs ->
  NS WrapValues xs ->
  NS WrapValues xs
forwardDiff diff vals0 = step vals0 diff
 where
  ts = translateValues (hardForkEraTranslation @xs)

  step :: NS WrapValues xs -> NS diff xs -> NS WrapValues xs
  step vals d
    | iV == iD =
        case Match.matchNS d vals of
          Right matched -> hcmap proxySingle applyOne matched
          Left _ -> err "matchNS failed at equal era index"
    | iV < iD = step (liftValuesOneEra ts vals) d
    | otherwise =
        err "in-flight diff older than the values (impossible)"
   where
    iV = index_NS vals
    iD = index_NS d

  applyOne ::
    forall blk.
    SingleEraBlock blk =>
    Product diff WrapValues blk ->
    WrapValues blk
  applyOne (Pair dd (WrapValues vv)) =
    WrapValues (forwardDiffHelper dd vv)

  err :: forall a. String -> a
  err s = error $ "forward" ++ forwardDiffHelperString (Proxy :: Proxy diff) ++ ": " ++ s
