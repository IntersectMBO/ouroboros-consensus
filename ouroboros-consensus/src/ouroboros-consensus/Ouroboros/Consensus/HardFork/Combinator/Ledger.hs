{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger
  ( HardForkEnvelopeErr (..)
  , HardForkLedgerError (..)
  , HardForkLedgerUpdate (..)
  , HardForkLedgerWarning (..)

    -- * Type family instances
  , FlipTickedLedgerState (..)
  , Ticked (..)

    -- * Low-level API (exported for the benefit of testing)
  , AnnForecast (..)
  , mkHardForkForecast

    -- * Ledger tables
  , ejectLedgerTables
  , injectLedgerTables

    -- ** HardForkTxIn
  , HasCanonicalTxIn (..)

    -- ** HardForkTxOut
  , DefaultHardForkTxOut
  , HasHardForkTxOut (..)
  , MemPackTxOut
  , ejectHardForkTxOutDefault
  , injectHardForkTxOutDefault
  ) where

import Control.Monad (guard)
import Control.Monad.Except (throwError, withExcept)
import qualified Control.State.Transition.Extended as STS
import Data.Functor ((<&>))
import Data.Functor.Product
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import Data.MemPack
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Counting (getExactly)
import Data.SOP.Functors (Flip (..))
import Data.SOP.InPairs (InPairs (..))
import qualified Data.SOP.InPairs as InPairs
import Data.SOP.Index
import qualified Data.SOP.Match as Match
import Data.SOP.Strict
import Data.SOP.Tails (Tails)
import qualified Data.SOP.Tails as Tails
import Data.SOP.Telescope (Telescope (..))
import qualified Data.SOP.Telescope as Telescope
import Data.Typeable
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
import Ouroboros.Consensus.Ledger.Tables.Utils
import Ouroboros.Consensus.Storage.LedgerDB
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Consensus.Util.IndexedMemPack (IndexedMemPack)
import Ouroboros.Consensus.Ledger.Extended
import qualified Data.List.Singletons as S
import Ouroboros.Consensus.Util.TypeLevel
import qualified Data.Singletons as S

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
      . State.getTip (castPoint . getTip . unFlip)
      . hardForkLedgerStatePerEra

instance CanHardFork xs => GetTip (Ticked (LedgerState (HardForkBlock xs))) where
  getTip =
    castPoint
      . State.getTip (castPoint . getTip . getFlipTickedLedgerState)
      . tickedHardForkLedgerStatePerEra

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

newtype FlipTickedLedgerState mk blk = FlipTickedLedgerState
  { getFlipTickedLedgerState :: Ticked (LedgerState blk) mk
  }

data instance Ticked (LedgerState (HardForkBlock xs)) mk
  = TickedHardForkLedgerState
  { tickedHardForkLedgerStateTransition :: !TransitionInfo
  , tickedHardForkLedgerStatePerEra ::
      !(HardForkState (FlipTickedLedgerState mk) xs)
  }

instance CanHardFork xs => IsLedger (LedgerState (HardForkBlock xs)) where
  type LedgerErr (LedgerState (HardForkBlock xs)) = HardForkLedgerError xs

  type AuxLedgerEvent (LedgerState (HardForkBlock xs)) = OneEraLedgerEvent xs

  applyChainTickLedgerResult evs cfg@HardForkLedgerConfig{..} slot (HardForkLedgerState st) =
    sequenceHardForkState
      ( hcizipWith
          proxySingle
          (tickOne ei slot evs)
          cfgs
          extended
      )
      <&> \l' ->
        TickedHardForkLedgerState
          { tickedHardForkLedgerStateTransition =
              -- We are bundling a 'TransitionInfo' with a /ticked/ ledger state,
              -- but /derive/ that 'TransitionInfo' from the /unticked/  (albeit
              -- extended) state. That requires justification. Three cases:
              --
              -- o 'TransitionUnknown'. If the transition is unknown, then it
              --   cannot become known due to ticking. In this case, we record
              --   the tip of the ledger, which ticking also does not modify
              --   (this is an explicit postcondition of 'applyChainTick').
              -- o 'TransitionKnown'. If the transition to the next epoch is
              --   already known, then ticking does not change that information.
              --   It can't be the case that the 'SlotNo' we're ticking to is
              --   /in/ that next era, because if was, then 'extendToSlot' would
              --   have extended the telescope further.
              --   (This does mean however that it is important to use the
              --   /extended/ ledger state, not the original, to determine the
              --   'TransitionInfo'.)
              -- o 'TransitionImpossible'. This has two subcases: either we are
              --   in the final era, in which case ticking certainly won't be able
              --   to change that, or we're forecasting, which is simply not
              --   applicable here.
              State.mostRecentTransitionInfo cfg extended
          , tickedHardForkLedgerStatePerEra = l'
          }
   where
    cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
    ei = State.epochInfoLedger cfg st

    extended :: HardForkState (Flip LedgerState DiffMK) xs
    extended = State.extendToSlot cfg slot st

-- | Ticking outside of era transitions for now does not generate differences
-- now that we only have the UTxO table, but we need the same type regardless of
-- whether we are crossing an era boundary or not.
--
-- This function ticks the ledger state using the particular block function, and
-- prepends the diffs that might have been created if this tick crossed an era
-- boundary.
tickOne ::
  (SListI xs, SingleEraBlock blk) =>
  EpochInfo (Except PastHorizonException) ->
  SlotNo ->
  ComputeLedgerEvents ->
  Index xs blk ->
  WrapPartialLedgerConfig blk ->
  (Flip LedgerState DiffMK) blk ->
  ( LedgerResult (LedgerState (HardForkBlock xs))
      :.: FlipTickedLedgerState DiffMK
  )
    blk
tickOne ei slot evs sopIdx partialCfg st =
  Comp
    . fmap
      ( FlipTickedLedgerState
          . prependDiffs (unFlip st)
      )
    . embedLedgerResult (injectLedgerEvent sopIdx)
    . applyChainTickLedgerResult evs (completeLedgerConfig' ei partialCfg) slot
    . forgetLedgerTables
    . unFlip
    $ st

{-------------------------------------------------------------------------------
  ApplyBlock
-------------------------------------------------------------------------------}

instance
  ( CanHardFork xs
  , HasCanonicalTxIn xs
  , HasHardForkTxOut xs
  ) =>
  ApplyBlock LedgerState (HardForkBlock xs)
  where
  applyBlockLedgerResultWithValidation
    doValidate
    opts
    cfg
    (HardForkBlock (OneEraBlock block))
    (TickedHardForkLedgerState transition st) =
      case State.match block st of
        Left mismatch ->
          -- Block from the wrong era (note that 'applyChainTick' will already
          -- have initiated the transition to the next era if appropriate).
          throwError $
            HardForkLedgerErrorWrongEra . MismatchEraInfo $
              Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
        Right matched ->
          fmap (fmap HardForkLedgerState . sequenceHardForkState) $
            hsequence' $
              hcizipWith proxySingle (apply doValidate opts) cfgs matched
     where
      cfgs = distribLedgerConfig ei cfg
      ei =
        State.epochInfoPrecomputedTransitionInfo
          (hardForkLedgerConfigShape cfg)
          transition
          st

  applyBlockLedgerResult = defaultApplyBlockLedgerResult

  reapplyBlockLedgerResult =
    defaultReapplyBlockLedgerResult
      ( \_ ->
          -- We already applied this block to this ledger state,
          -- so it can't be from the wrong era
          error "reapplyBlockLedgerResult: can't be from other era"
      )

instance GetBlockKeySets (HardForkBlock xs) where
  getBlockKeySets (HardForkBlock (OneEraBlock ns)) =
    hcollapse $
      hcimap proxySingle f ns
   where
    f ::
      SingleEraBlock x =>
      Index xs x ->
      I x ->
      K (LedgerTables (HardForkBlock xs) KeysMK) x
    f idx (I blk) = K $ injectLedgerTables idx $ getBlockKeySets blk

apply ::
  (SListI xs, SingleEraBlock blk) =>
  STS.ValidationPolicy ->
  ComputeLedgerEvents ->
  Index xs blk ->
  WrapLedgerConfig blk ->
  Product I (FlipTickedLedgerState ValuesMK) blk ->
  ( Except (HardForkLedgerError xs)
      :.: LedgerResult (LedgerState (HardForkBlock xs))
      :.: Flip LedgerState DiffMK
  )
    blk
apply doValidate opts index (WrapLedgerConfig cfg) (Pair (I block) (FlipTickedLedgerState st)) =
  Comp $
    withExcept (injectLedgerError index) $
      fmap (Comp . fmap Flip . embedLedgerResult (injectLedgerEvent index)) $
        applyBlockLedgerResultWithValidation doValidate opts cfg block st

{-------------------------------------------------------------------------------
  UpdateLedger
-------------------------------------------------------------------------------}

instance
  ( CanHardFork xs
  , HasCanonicalTxIn xs
  , HasHardForkTxOut xs
  ) =>
  UpdateLedger (HardForkBlock xs)

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
  ( CanHardFork xs
  , HasCanonicalTxIn xs
  , HasHardForkTxOut xs
  ) =>
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
        FlipTickedLedgerState mk blk ->
        WrapLedgerView blk
      viewOne cfg (FlipTickedLedgerState st) =
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
        forall blk mk.
        SingleEraBlock blk =>
        WrapPartialLedgerConfig blk ->
        K EraParams blk ->
        Current (Flip LedgerState mk) blk ->
        Current (AnnForecast LedgerState WrapLedgerView) blk
      forecastOne cfg (K params) (Current start (Flip st)) =
        Current
          { currentStart = start
          , currentState =
              AnnForecast
                { annForecast =
                    mapForecast WrapLedgerView $
                      ledgerViewForecastAt cfg' st
                , annForecastState = forgetLedgerTables st
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
  , annForecastState :: state blk EmptyMK
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
  NS (Current (Flip LedgerState mk1)) xs ->
  NS (Current (Flip LedgerState mk2)) xs ->
  [LedgerEvent (HardForkBlock xs)]
inspectHardForkLedger = go
 where
  go ::
    All SingleEraBlock xs =>
    NP WrapPartialLedgerConfig xs ->
    NP (K EraParams) xs ->
    NP TopLevelConfig xs ->
    NS (Current (Flip LedgerState mk1)) xs ->
    NS (Current (Flip LedgerState mk2)) xs ->
    [LedgerEvent (HardForkBlock xs)]

  go (pc :* _) (K ps :* pss) (c :* _) (Z before) (Z after) =
    concat
      [ map liftEvent $
          inspectLedger
            c
            (unFlip $ currentState before)
            (unFlip $ currentState after)
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
        (unFlip $ currentState before)
    confirmedAfter =
      singleEraTransition
        (unwrapPartialLedgerConfig pc)
        ps
        (currentStart after)
        (unFlip $ currentState after)
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
  forall blk mk.
  SingleEraBlock blk =>
  Current (FlipTickedLedgerState mk) blk -> LedgerEraInfo blk
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
  SListI xs => Index xs blk -> AuxLedgerEvent (LedgerState blk) -> OneEraLedgerEvent xs
injectLedgerEvent index =
  OneEraLedgerEvent
    . injectNS index
    . WrapLedgerEvent

{-------------------------------------------------------------------------------
  Ledger Tables for the Nary HardForkBlock
-------------------------------------------------------------------------------}

-- | Warning: 'projectLedgerTables' and 'withLedgerTables' are prohibitively
-- expensive when using big tables or when used multiple times. See the 'TxOut'
-- instance for the 'HardForkBlock' for more information.
instance
  ( CanHardFork xs
  , HasCanonicalTxIn xs
  , HasHardForkTxOut xs
  ) =>
  HasLedgerTables (LedgerState (HardForkBlock xs))
  where
  projectLedgerTables ::
    forall mk.
    (CanMapMK mk, CanMapKeysMK mk, ZeroableMK mk) =>
    LedgerState (HardForkBlock xs) mk ->
    LedgerTables (HardForkBlock xs) mk
  projectLedgerTables (HardForkLedgerState st) =
    hcollapse $
      hcimap (Proxy @(Compose HasLedgerTables LedgerState)) projectOne st
   where
    projectOne ::
      Compose HasLedgerTables LedgerState x =>
      Index xs x ->
      Flip LedgerState mk x ->
      K (LedgerTables (HardForkBlock xs) mk) x
    projectOne i l =
      K $
        injectLedgerTables i $
          projectLedgerTables $
            unFlip l

  withLedgerTables ::
    forall mk any.
    (CanMapMK mk, CanMapKeysMK mk, ZeroableMK mk) =>
    LedgerState (HardForkBlock xs) any ->
    LedgerTables (HardForkBlock xs) mk ->
    LedgerState (HardForkBlock xs) mk
  withLedgerTables (HardForkLedgerState st) tables =
    HardForkLedgerState $
      hcimap (Proxy @(Compose HasLedgerTables LedgerState)) withLedgerTablesOne st
   where
    withLedgerTablesOne ::
      Compose HasLedgerTables LedgerState x =>
      Index xs x ->
      Flip LedgerState any x ->
      Flip LedgerState mk x
    withLedgerTablesOne i l =
      Flip $
        withLedgerTables (unFlip l) $
          ejectLedgerTables i tables

instance
  ( CanHardFork xs
  , HasCanonicalTxIn xs
  , HasHardForkTxOut xs
  ) =>
  HasLedgerTables (Ticked (LedgerState (HardForkBlock xs)))
  where
  projectLedgerTables ::
    forall mk.
    (CanMapMK mk, CanMapKeysMK mk, ZeroableMK mk) =>
    Ticked (LedgerState (HardForkBlock xs)) mk ->
    LedgerTables (HardForkBlock xs) mk
  projectLedgerTables st =
    hcollapse $
      hcimap
        (Proxy @(Compose HasTickedLedgerTables LedgerState))
        projectOne
        (tickedHardForkLedgerStatePerEra st)
   where
    projectOne ::
      Compose HasTickedLedgerTables LedgerState x =>
      Index xs x ->
      FlipTickedLedgerState mk x ->
      K (LedgerTables (HardForkBlock xs) mk) x
    projectOne i l =
      K $
        castLedgerTables $
          injectLedgerTables i $
            castLedgerTables $
              projectLedgerTables $
                getFlipTickedLedgerState l

  withLedgerTables ::
    forall mk any.
    (CanMapMK mk, CanMapKeysMK mk, ZeroableMK mk) =>
    Ticked (LedgerState (HardForkBlock xs)) any ->
    LedgerTables (HardForkBlock xs) mk ->
    Ticked (LedgerState (HardForkBlock xs)) mk
  withLedgerTables st tables =
    st
      { tickedHardForkLedgerStatePerEra =
          hcimap
            (Proxy @(Compose HasTickedLedgerTables LedgerState))
            withLedgerTablesOne
            (tickedHardForkLedgerStatePerEra st)
      }
   where
    withLedgerTablesOne ::
      Compose HasTickedLedgerTables LedgerState x =>
      Index xs x ->
      FlipTickedLedgerState any x ->
      FlipTickedLedgerState mk x
    withLedgerTablesOne i l =
      FlipTickedLedgerState $
        withLedgerTables (getFlipTickedLedgerState l) $
          castLedgerTables $
            ejectLedgerTables i (castLedgerTables tables)

instance
  All (Compose CanStowLedgerTables LedgerState) xs =>
  CanStowLedgerTables (LedgerState (HardForkBlock xs))
  where
  stowLedgerTables ::
    LedgerState (HardForkBlock xs) ValuesMK ->
    LedgerState (HardForkBlock xs) EmptyMK
  stowLedgerTables (HardForkLedgerState st) =
    HardForkLedgerState $
      hcmap (Proxy @(Compose CanStowLedgerTables LedgerState)) stowOne st
   where
    stowOne ::
      Compose CanStowLedgerTables LedgerState x =>
      Flip LedgerState ValuesMK x ->
      Flip LedgerState EmptyMK x
    stowOne = Flip . stowLedgerTables . unFlip

  unstowLedgerTables ::
    LedgerState (HardForkBlock xs) EmptyMK ->
    LedgerState (HardForkBlock xs) ValuesMK
  unstowLedgerTables (HardForkLedgerState st) =
    HardForkLedgerState $
      hcmap (Proxy @(Compose CanStowLedgerTables LedgerState)) unstowOne st
   where
    unstowOne ::
      Compose CanStowLedgerTables LedgerState x =>
      Flip LedgerState EmptyMK x ->
      Flip LedgerState ValuesMK x
    unstowOne = Flip . unstowLedgerTables . unFlip

injectLedgerTables ::
  forall xs x mk.
  ( CanMapKeysMK mk
  , CanMapMK mk
  , HasCanonicalTxIn xs
  , HasHardForkTxOut xs
  ) =>
  Index xs x ->
  LedgerTables x mk ->
  LedgerTables (HardForkBlock xs) mk
injectLedgerTables idx =
  bimapLedgerTables (injectCanonicalTxIn idx) (injectHardForkTxOut idx)

ejectLedgerTables ::
  forall xs x mk.
  ( CanMapKeysMK mk
  , Ord (TxIn x)
  , HasCanonicalTxIn xs
  , CanMapMK mk
  , HasHardForkTxOut xs
  ) =>
  Index xs x ->
  LedgerTables (HardForkBlock xs) mk ->
  LedgerTables x mk
ejectLedgerTables idx =
  bimapLedgerTables (ejectCanonicalTxIn idx) (ejectHardForkTxOut idx)

{-------------------------------------------------------------------------------
  HardForkTxIn
-------------------------------------------------------------------------------}

-- | Must be the 'CannonicalTxIn' type, but this will probably change in the
-- future to @NS 'WrapTxIn' xs@. See 'HasCanonicalTxIn'.
type instance TxIn (HardForkBlock xs) = CanonicalTxIn xs

-- | Canonical TxIn
--
-- The Ledger and Consensus team discussed the fact that we need to be able to
-- reach the TxIn key for an entry from any era, regardless of the era in which
-- it was created, therefore we need to have a "canonical" serialization that
-- doesn't change between eras. For now we are requiring that a 'HardForkBlock'
-- has only one associated 'TxIn' type as a stop-gap, but Ledger will provide a
-- serialization function into something more efficient.
type HasCanonicalTxIn :: [Type] -> Constraint
class
  ( Show (CanonicalTxIn xs)
  , Ord (CanonicalTxIn xs)
  , NoThunks (CanonicalTxIn xs)
  , MemPack (CanonicalTxIn xs)
  ) =>
  HasCanonicalTxIn xs
  where
  data CanonicalTxIn (xs :: [Type]) :: Type

  -- | Inject an era-specific 'TxIn' into a 'TxIn' for a 'HardForkBlock'.
  injectCanonicalTxIn ::
    Index xs x ->
    TxIn x ->
    CanonicalTxIn xs

  -- | Distribute a 'TxIn' for a 'HardForkBlock' to an era-specific 'TxIn'.
  ejectCanonicalTxIn ::
    Index xs x ->
    CanonicalTxIn xs ->
    TxIn x

{-------------------------------------------------------------------------------
  HardForkTxOut
-------------------------------------------------------------------------------}

-- | Must be the 'HardForkTxOut' type
type instance TxOut (HardForkBlock xs) = HardForkTxOut xs

-- | This choice for 'HardForkTxOut' imposes some complications on the code.
--
-- We deliberately chose not to have all values in the tables be
-- @'Cardano.Ledger.Core.TxOut' era@ because this would require us to traverse
-- and translate the whole UTxO set on era boundaries. To avoid this, we are
-- holding a @'NS' 'WrapTxOut' xs@ instead.
--
-- Whenever we are carrying a @'LedgerState' ('HardForkBlock' xs) mk@ (or
-- 'Ouroboros.Consensus.Ledger.Extended.ExtLedgerState'), the tables are the
-- ones inside the particular ledger state in the 'Telescope' of the
-- 'HardForkState'.
--
-- <<docs/haddocks/hard-fork-tables-per-block.svg>>
--
-- However, when we are carrying @'LedgerTables' ('HardForkBlock' xs) mk@ we are
-- instead carrying these tables, where the 'TxOut' is an 'NS'. This means that
-- whenever we are extracting these tables, we are effectively duplicating the
-- UTxO set ('Data.Map.Map') inside, to create an identical one where every
-- element has been translated to the most recent era and unwrapped from the
-- 'NS'.
--
-- <<docs/haddocks/hard-fork-tables.svg>>
--
-- To prevent memory explosion, try to only perform one of this transformations,
-- for example:
--
-- * when applying blocks, inject the tables for the transactions only once, and
--     extract them only once.
--
-- * when performing queries on the tables (that use
--     'Ouroboros.Consensus.Ledger.Query.QFTraverseTables'), operate with the
--     tables at the hard fork level until the very end, when you have to
--     promote them to some specific era.
--
-- = __(image code)__
--
-- >>> :{
-- >>> either (error . show) pure =<<
-- >>>  renderToFile "docs/haddocks/hard-fork-tables.svg" defaultEnv (tikz ["positioning", "arrows"]) "\\node at (4.5,4.8) {\\small{LedgerTables (LedgerState (HardForkBlock xs))}};\
-- >>> \ \\draw (0,0) rectangle (9,5);\
-- >>> \ \\node (rect) at (1.5,4) [draw,minimum width=1cm,minimum height=0.5cm] {TxIn};\
-- >>> \ \\node (oneOf) at (3.5,4) [draw=none] {NS};\
-- >>> \ \\draw (rect) -> (oneOf);\
-- >>> \ \\node (sh) at (6.5,4) [draw,minimum width=1cm,minimum height=0.5cm] {BlockATxOut};\
-- >>> \ \\node (al) at (6.5,3) [draw,minimum width=1cm,minimum height=0.5cm] {BlockBTxOut};\
-- >>> \ \\node (my) at (6.5,2) [draw=none,minimum width=1cm,minimum height=0.5cm] {...};\
-- >>> \ \\node (ba) at (6.5,1) [draw,minimum width=1cm,minimum height=0.5cm] {BlockNTxOut};\
-- >>> \ \\draw (oneOf) -> (sh);\
-- >>> \ \\draw (oneOf) -> (al);\
-- >>> \ \\draw (oneOf) -> (ba);\
-- >>> \ \\draw (3,0.5) rectangle (8,4.5);"
-- >>> :}
--
-- >>> :{
-- >>> either (error . show) pure =<<
-- >>>  renderToFile "docs/haddocks/hard-fork-tables-per-block.svg" defaultEnv (tikz ["positioning", "arrows"]) "\\node at (5,4.8) {\\small{LedgerState (HardForkBlock xs)}};\
-- >>> \ \\draw (0,0) rectangle (10,5);\
-- >>> \ \\node (oneOf2) at (2,4) [draw=none] {HardForkState};\
-- >>> \ \\node (bb) at (5,4) [draw,minimum width=1cm,minimum height=0.5cm] {BlockAState};\
-- >>> \ \\node (bt) at (8,4) [draw,minimum width=1cm,minimum height=0.5cm] {BlockATables};\
-- >>> \ \\node (sb) at (5,3) [draw,minimum width=1cm,minimum height=0.5cm] {BlockBState};\
-- >>> \ \\node (st) at (8,3) [draw,minimum width=1cm,minimum height=0.5cm] {BlockBTables};\
-- >>> \ \\node (db) at (5,2) [draw=none,minimum width=1cm,minimum height=0.5cm] {...};\
-- >>> \ \\node (dt) at (8,2) [draw=none,minimum width=1cm,minimum height=0.5cm] {...};\
-- >>> \ \\node (bab) at (5,1) [draw,minimum width=1cm,minimum height=0.5cm] {BlockNState};\
-- >>> \ \\node (bat) at (8,1) [draw,minimum width=1cm,minimum height=0.5cm] {BlockNTables};\
-- >>> \ \\draw (oneOf2) -> (bb);\
-- >>> \ \\draw (bb) -> (bt);\
-- >>> \ \\draw (oneOf2) -> (sb);\
-- >>> \ \\draw (sb) -> (st);\
-- >>> \ \\draw (oneOf2) -> (bab);\
-- >>> \ \\draw (bab) -> (bat);"
-- >>> :}
type DefaultHardForkTxOut xs = NS WrapTxOut xs
type DefaultHardForkCoin xs = NS WrapCoin xs
type DefaultHardForkCredential xs = NS WrapCredential xs
type DefaultHardForkTxIn xs = NS WrapTxIn xs

-- This is just necessary because GHC fails to parse the instance below otherwise
--
-- Try:
-- type instance TablesForBlock (HardForkBlock xs)
--   = S.Nub (Unions (S.Map (S.TyCon1 TablesForBlock) xs))
data MapTablesForBlock :: Type S.~> [TAG]
type instance S.Apply MapTablesForBlock x = TablesForBlock x

type instance TablesForBlock (HardForkBlock xs)
   = S.Nub (Unions (S.Map MapTablesForBlock xs))

class
  ( Show (HardForkTxOut xs)
  , Eq (HardForkTxOut xs)
  , NoThunks (HardForkTxOut xs)
  , IndexedMemPack (LedgerState (HardForkBlock xs) EmptyMK) (HardForkTxOut xs)
  , All (SerializeTablesWithHint (LedgerState (HardForkBlock xs)) (HardForkBlock xs)) (TablesForBlock (HardForkBlock xs))
  ) =>
  HasHardForkTxOut xs
  where

  type HardForkTxIn xs :: Type
  type HardForkTxIn xs = DefaultHardForkTxIn xs

  type HardForkTxOut xs :: Type
  type HardForkTxOut xs = DefaultHardForkTxOut xs

  type HardForkCredential xs :: Type
  type HardForkCredential xs = DefaultHardForkCredential xs

  type HardForkCoin xs :: Type
  type HardForkCoin xs = DefaultHardForkCoin xs

  injectHardForkTxIn :: Index xs x -> TxIn x -> HardForkTxIn xs
  ejectHardForkTxIn :: Index xs x -> HardForkTxIn xs -> TxIn x

  injectHardForkTxOut :: Index xs x -> TxOut x -> HardForkTxOut xs
  ejectHardForkTxOut :: Index xs x -> HardForkTxOut xs -> TxOut x

  injectHardForkCredential :: Index xs x -> Credential x -> HardForkCredential xs
  ejectHardForkCredential :: Index xs x -> HardForkCredential xs -> Credential x

  injectHardForkCoin :: Index xs x -> Coin x -> HardForkCoin xs
  ejectHardForkCoin :: Index xs x -> HardForkCoin xs -> Coin x


  -- | This method is a null-arity method in a typeclass to make it a CAF, such
  -- that we only compute it once, then it is cached for the duration of the
  -- program, as we will use it very often when converting from the
  -- HardForkBlock to the particular @blk@.
  --
  -- This particular method is useful when our HardForkBlock uses
  -- DefaultHardForkTxOut, so that we can implement inject and project.
  txOutEjections :: NP (K (NS WrapTxOut xs) -.-> WrapTxOut) xs
  default txOutEjections :: CanHardFork xs => NP (K (NS WrapTxOut xs) -.-> WrapTxOut) xs
  txOutEjections = composeTxOutTranslations $ ipTranslateTxOut hardForkEraTranslation

  -- | This method is a null-arity method in a typeclass to make it a CAF, such
  -- that we only compute it once, then it is cached for the duration of the
  -- program, as we will use it very often when converting from the
  -- HardForkBlock to the particular @blk@.
  txOutTranslations :: Tails (InPairs.Fn2 WrapTxOut) xs
  default txOutTranslations :: CanHardFork xs => Tails (InPairs.Fn2 WrapTxOut) xs
  txOutTranslations =
    Tails.inPairsToTails $
      InPairs.hmap
        (\translator -> InPairs.Fn2 $ WrapTxOut . translateTxOutWith translator . unwrapTxOut)
        (translateLedgerTables (hardForkEraTranslation @xs))

instance
  (CanHardFork xs, HasHardForkTxOut xs) =>
  CanUpgradeLedgerTables (LedgerState (HardForkBlock xs))
  where
  upgradeTables
    (HardForkLedgerState (HardForkState hs0))
    (HardForkLedgerState (HardForkState hs1))
    orig@(LedgerTables (ValuesMK vs)) =
      if isJust $ Match.telescopesMismatch hs0 hs1
        then LedgerTables $ ValuesMK $ extendTables (hmap (const (K ())) t1) vs
        else orig
     where
      t1 = Telescope.tip hs1

extendTables ::
  forall xs.
  (CanHardFork xs, HasHardForkTxOut xs) =>
  NS (K ()) xs ->
  Map.Map
    (TxIn (HardForkBlock xs))
    (TxOut (HardForkBlock xs)) ->
  Map.Map
    (TxIn (HardForkBlock xs))
    (TxOut (HardForkBlock xs))
extendTables st =
  Map.map
    ( \txout ->
        hcollapse $
          hcimap
            proxySingle
            ( \idxTarget (K ()) ->
                K
                  . injectHardForkTxOut idxTarget
                  . ejectHardForkTxOut idxTarget
                  $ txout
            )
            st
    )

injectHardForkTxOutDefault ::
  SListI xs =>
  Index xs x ->
  TxOut x ->
  DefaultHardForkTxOut xs
injectHardForkTxOutDefault idx = injectNS idx . WrapTxOut

ejectHardForkTxOutDefault ::
  SListI xs =>
  HasHardForkTxOut xs =>
  Index xs x ->
  DefaultHardForkTxOut xs ->
  TxOut x
ejectHardForkTxOutDefault idx =
  unwrapTxOut
    . apFn (projectNP idx txOutEjections)
    . K

composeTxOutTranslations ::
  SListI xs =>
  InPairs TranslateTxOut xs ->
  NP (K (NS WrapTxOut xs) -.-> WrapTxOut) xs
composeTxOutTranslations = \case
  PNil ->
    fn (unZ . unK) :* Nil
  PCons (TranslateTxOut t) ts ->
    fn
      ( eitherNS
          id
          (error "composeTranslations: anachrony")
          . unK
      )
      :* hmap
        ( \innerf ->
            fn $
              apFn innerf
                . K
                . eitherNS
                  (Z . WrapTxOut . t . unwrapTxOut)
                  id
                . unK
        )
        (composeTxOutTranslations ts)
 where
  eitherNS :: (f x -> c) -> (NS f xs -> c) -> NS f (x ': xs) -> c
  eitherNS l r = \case
    Z x -> l x
    S x -> r x

class MemPack (TxOut x) => MemPackTxOut x
instance MemPack (TxOut x) => MemPackTxOut x

instance
  (All MemPackTxOut xs, Typeable xs) =>
  MemPack (DefaultHardForkTxOut xs)
  where
  packM =
    hcollapse
      . hcimap
        (Proxy @MemPackTxOut)
        ( \idx (WrapTxOut txout) -> K $ do
            packM (toWord8 idx)
            packM txout
        )

  packedByteCount txout =
    1 + hcollapse (hcmap (Proxy @MemPackTxOut) (K . packedByteCount . unwrapTxOut) txout)

  unpackM = do
    idx <- unpackM
    hsequence'
      $ hcmap
        (Proxy @MemPackTxOut)
        (const $ Comp $ WrapTxOut <$> unpackM)
      $ fromMaybe (error "Unknown tag") (nsFromIndex idx)
