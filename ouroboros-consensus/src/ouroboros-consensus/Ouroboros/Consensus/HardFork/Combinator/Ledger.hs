{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Ledger (
    HardForkEnvelopeErr (..)
  , HardForkLedgerError (..)
  , HardForkLedgerUpdate (..)
  , HardForkLedgerWarning (..)
    -- * Type family instances
  , FlipTickedLedgerState (..)
  , Ticked1 (..)
    -- * Low-level API (exported for the benefit of testing)
  , AnnForecast (..)
  , mkHardForkForecast
    -- * Ledger tables
  , HardForkHasLedgerTables
  , HasCanonicalTxIn (..)
  , distribLedgerTables
  , injectLedgerTables
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Control.Monad (guard)
import           Control.Monad.Except (throwError, withExcept)
import           Data.Functor ((<&>))
import           Data.Functor.Product
import           Data.Kind (Type)
import           Data.Monoid (First (..))
import           Data.Proxy
import           Data.SOP.Counting (getExactly)
import           Data.SOP.Functors (Flip (..))
import           Data.SOP.Index
import           Data.SOP.InPairs (InPairs (..))
import qualified Data.SOP.InPairs as InPairs
import qualified Data.SOP.Match as Match
import           Data.SOP.Strict hiding (shape, tl)
import           Data.SOP.Telescope (Telescope (..))
import qualified Data.SOP.Telescope as Telescope
import           Data.Void
import           Data.Word (Word8)
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Forecast
import           Ouroboros.Consensus.HardFork.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Protocol ()
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.History (Bound (..), EraParams,
                     SafeZone (..))
import qualified Ouroboros.Consensus.HardFork.History as History
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Inspect
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Ticked
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data HardForkLedgerError xs =
    -- | Validation error from one of the eras
    HardForkLedgerErrorFromEra (OneEraLedgerError xs)

    -- | We tried to apply a block from the wrong era
  | HardForkLedgerErrorWrongEra (MismatchEraInfo xs)
  deriving (Generic, Show, Eq, NoThunks)

{-------------------------------------------------------------------------------
  GetTip
-------------------------------------------------------------------------------}

instance CanHardFork xs => GetTip (LedgerState (HardForkBlock xs)) where
  getTip = castPoint
         . State.getTip (castPoint . getTip . unFlip)
         . hardForkLedgerStatePerEra

instance CanHardFork xs => GetTip (Ticked1 (LedgerState (HardForkBlock xs))) where
  getTip = castPoint
         . State.getTip (castPoint . getTip . getFlipTickedLedgerState)
         . tickedHardForkLedgerStatePerEra

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

newtype FlipTickedLedgerState mk blk = FlipTickedLedgerState {
  getFlipTickedLedgerState :: (Ticked1 (LedgerState blk) mk)
  } deriving (Generic)

deriving newtype instance NoThunks (Ticked1 (LedgerState blk) mk)
                       => NoThunks (FlipTickedLedgerState mk blk)

data instance Ticked1 (LedgerState (HardForkBlock xs)) mk =
    TickedHardForkLedgerState {
        tickedHardForkLedgerStateTransition :: !TransitionInfo
      , tickedHardForkLedgerStatePerEra     ::
          !(HardForkState (FlipTickedLedgerState mk) xs)
      } deriving (Generic)

deriving instance All SingleEraBlock xs
               => NoThunks (Ticked1 (LedgerState (HardForkBlock xs)) TrackingMK)

instance CanHardFork xs => IsLedger (LedgerState (HardForkBlock xs)) where
  type LedgerErr (LedgerState (HardForkBlock xs)) = HardForkLedgerError  xs

  type AuxLedgerEvent (LedgerState (HardForkBlock xs)) = OneEraLedgerEvent xs

  applyChainTickLedgerResult cfg@HardForkLedgerConfig{..} slot (HardForkLedgerState st) =
      sequenceHardForkState
        (hcizipWith proxySingle (tickOne ei slot) cfgs extended) <&> \l' ->
      TickedHardForkLedgerState {
          tickedHardForkLedgerStateTransition =
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
      ei   = State.epochInfoLedger cfg st

      extended :: HardForkState (Flip LedgerState DiffMK) xs
      extended = State.extendToSlot cfg slot st

tickOne :: SingleEraBlock blk
        => EpochInfo (Except PastHorizonException)
        -> SlotNo
        -> Index                                          xs   blk
        -> WrapPartialLedgerConfig                             blk
        -> (Flip LedgerState DiffMK)                           blk
        -> (     LedgerResult (LedgerState (HardForkBlock xs))
             :.: FlipTickedLedgerState DiffMK
           )                                                   blk
tickOne ei slot sopIdx partialCfg st =
      Comp
    . fmap ( FlipTickedLedgerState
           . prependDiffs (unFlip st)
           )
    . embedLedgerResult (injectLedgerEvent sopIdx)
    . applyChainTickLedgerResult (completeLedgerConfig' ei partialCfg) slot
    . forgetLedgerTables
    . unFlip
    $ st

{-------------------------------------------------------------------------------
  ApplyBlock
-------------------------------------------------------------------------------}

instance ( CanHardFork xs
         , HardForkHasLedgerTables xs
         , HasCanonicalTxIn xs
         )
      => ApplyBlock (LedgerState (HardForkBlock xs)) (HardForkBlock xs) where

  applyBlockLedgerResult cfg
                    (HardForkBlock (OneEraBlock block))
                    (TickedHardForkLedgerState transition st) =
      case State.match block st of
        Left mismatch ->
          -- Block from the wrong era (note that 'applyChainTick' will already
          -- have initiated the transition to the next era if appropriate).
            throwError
          $ HardForkLedgerErrorWrongEra . MismatchEraInfo
          $ Match.bihcmap proxySingle singleEraInfo ledgerInfo mismatch
        Right matched ->
            fmap (fmap HardForkLedgerState . sequenceHardForkState)
          $ hsequence'
          $ hcizipWith proxySingle apply cfgs matched
    where
      cfgs = distribLedgerConfig ei cfg
      ei   = State.epochInfoPrecomputedTransitionInfo
               (hardForkLedgerConfigShape cfg)
               transition
               st

  reapplyBlockLedgerResult cfg
                      (HardForkBlock (OneEraBlock block))
                      (TickedHardForkLedgerState transition st) =
      case State.match block st of
        Left _mismatch ->
          -- We already applied this block to this ledger state,
          -- so it can't be from the wrong era
          error "reapplyBlockLedgerResult: can't be from other era"
        Right matched ->
            fmap HardForkLedgerState
          $ sequenceHardForkState
          $ hcizipWith proxySingle reapply cfgs matched
    where
      cfgs = distribLedgerConfig ei cfg
      ei   = State.epochInfoPrecomputedTransitionInfo
               (hardForkLedgerConfigShape cfg)
               transition
               st

  getBlockKeySets (HardForkBlock (OneEraBlock ns)) =
        hcollapse
      $ hcimap proxySingle f ns
    where
      f ::
           SingleEraBlock                                           x
        => Index                                       xs           x
        -> I                                                        x
        -> K (LedgerTables (LedgerState (HardForkBlock xs)) KeysMK) x
      f idx (I blk) = K $ injectLedgerTables idx $ getBlockKeySets blk

apply :: SingleEraBlock blk
      => Index xs                                           blk
      -> WrapLedgerConfig                                   blk
      -> Product I (FlipTickedLedgerState ValuesMK)         blk
      -> (    Except (HardForkLedgerError xs)
          :.: LedgerResult (LedgerState (HardForkBlock xs))
          :.: Flip LedgerState DiffMK
         )                                                  blk
apply index (WrapLedgerConfig cfg) (Pair (I block) (FlipTickedLedgerState st)) =
      Comp
    $ withExcept (injectLedgerError index)
    $ fmap (Comp . fmap Flip . embedLedgerResult (injectLedgerEvent index))
    $ applyBlockLedgerResult cfg block st

reapply :: SingleEraBlock blk
        => Index xs                                           blk
        -> WrapLedgerConfig                                   blk
        -> Product I (FlipTickedLedgerState ValuesMK)         blk
        -> (    LedgerResult (LedgerState (HardForkBlock xs))
            :.: Flip LedgerState DiffMK
           )                                                  blk
reapply index (WrapLedgerConfig cfg) (Pair (I block) (FlipTickedLedgerState st)) =
      Comp
    $ fmap Flip
    $ embedLedgerResult (injectLedgerEvent index)
    $ reapplyBlockLedgerResult cfg block st

{-------------------------------------------------------------------------------
  UpdateLedger
-------------------------------------------------------------------------------}

instance ( CanHardFork xs
         , HardForkHasLedgerTables xs
         , HasCanonicalTxIn xs
         ) => UpdateLedger (HardForkBlock xs)

{-------------------------------------------------------------------------------
  HasHardForkHistory
-------------------------------------------------------------------------------}

instance All SingleEraBlock xs => HasHardForkHistory (HardForkBlock xs) where
  type HardForkIndices (HardForkBlock xs) = xs

  hardForkSummary cfg = State.reconstructSummaryLedger cfg
                      . hardForkLedgerStatePerEra

{-------------------------------------------------------------------------------
  HeaderValidation
-------------------------------------------------------------------------------}

data HardForkEnvelopeErr xs =
    -- | Validation error from one of the eras
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr xs)

    -- | We tried to apply a block from the wrong era
  | HardForkEnvelopeErrWrongEra (MismatchEraInfo xs)
  deriving (Eq, Show, Generic, NoThunks)

instance CanHardFork xs => ValidateEnvelope (HardForkBlock xs) where
  type OtherHeaderEnvelopeError (HardForkBlock xs) = HardForkEnvelopeErr xs

  additionalEnvelopeChecks tlc
                           (TickedHardForkLedgerView transition hardForkView) =
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
      ei = State.epochInfoPrecomputedTransitionInfo
             (hardForkLedgerConfigShape $ configLedger tlc)
             transition
             hardForkView

      cfgs :: NP TopLevelConfig xs
      cfgs = distribTopLevelConfig ei tlc

      aux :: forall blk. SingleEraBlock blk
          => Index xs blk
          -> TopLevelConfig blk
          -> Product Header (Ticked :.: WrapLedgerView) blk
          -> K (Except (HardForkEnvelopeErr xs) ()) blk
      aux index cfg (Pair hdr (Comp view)) = K $
          withExcept injErr' $
            additionalEnvelopeChecks
              cfg
              (unwrapTickedLedgerView view)
              hdr
        where
          injErr' :: OtherHeaderEnvelopeError blk -> HardForkEnvelopeErr xs
          injErr' = HardForkEnvelopeErrFromEra
                  . OneEraEnvelopeErr
                  . injectNS index
                  . WrapEnvelopeErr

{-------------------------------------------------------------------------------
  LedgerSupportsProtocol
-------------------------------------------------------------------------------}

instance ( CanHardFork xs
         , HardForkHasLedgerTables xs
         , HasCanonicalTxIn xs
         ) => LedgerSupportsProtocol (HardForkBlock xs) where
  protocolLedgerView HardForkLedgerConfig{..}
                     (TickedHardForkLedgerState transition ticked) =
      TickedHardForkLedgerView {
          tickedHardForkLedgerViewTransition = transition
        , tickedHardForkLedgerViewPerEra     =
            hczipWith proxySingle tickedViewOne cfgs ticked
        }
    where
      cfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      ei   = State.epochInfoPrecomputedTransitionInfo
               hardForkLedgerConfigShape
               transition
               ticked

      tickedViewOne :: SingleEraBlock              blk
                    => WrapPartialLedgerConfig     blk
                    -> FlipTickedLedgerState mk    blk
                    -> (Ticked :.: WrapLedgerView) blk
      tickedViewOne cfg (FlipTickedLedgerState st) = Comp $
          WrapTickedLedgerView $
            protocolLedgerView (completeLedgerConfig' ei cfg) st

  ledgerViewForecastAt ledgerCfg@HardForkLedgerConfig{..}
                       (HardForkLedgerState ledgerSt) =
      mkHardForkForecast
        (InPairs.requiringBoth cfgs $ crossEraForecast hardForkEraTranslation)
        annForecast
    where
      ei    = State.epochInfoLedger ledgerCfg ledgerSt
      pcfgs = getPerEraLedgerConfig hardForkLedgerConfigPerEra
      cfgs  = hcmap proxySingle (completeLedgerConfig'' ei) pcfgs

      annForecast :: HardForkState (AnnForecast LedgerState WrapLedgerView) xs
      annForecast = HardForkState $
          hczipWith3
            proxySingle
            forecastOne
            pcfgs
            (getExactly (History.getShape hardForkLedgerConfigShape))
            (getHardForkState ledgerSt)

      forecastOne ::
             forall blk mk. SingleEraBlock blk
          => WrapPartialLedgerConfig blk
          -> K EraParams blk
          -> Current (Flip LedgerState mk) blk
          -> Current (AnnForecast LedgerState WrapLedgerView) blk
      forecastOne cfg (K params) (Current start (Flip st)) = Current {
            currentStart = start
          , currentState = AnnForecast {
                annForecast      = mapForecast WrapTickedLedgerView $
                                     ledgerViewForecastAt cfg' st
              , annForecastState = forgetLedgerTables st
              , annForecastTip   = ledgerTipSlot st
              , annForecastEnd   = History.mkUpperBound params start <$>
                                     singleEraTransition' cfg params start st
              }
          }
        where
          cfg' :: LedgerConfig blk
          cfg' = completeLedgerConfig' ei cfg

{-------------------------------------------------------------------------------
  Annotated forecasts
-------------------------------------------------------------------------------}

-- | Forecast annotated with details about the ledger it was derived from
data AnnForecast state view blk = AnnForecast {
      annForecast      :: Forecast (view blk)
    , annForecastState :: state blk EmptyMK
    , annForecastTip   :: WithOrigin SlotNo
    , annForecastEnd   :: Maybe Bound
    }

-- | Change a telescope of a forecast into a forecast of a telescope
mkHardForkForecast ::
     forall state view xs.
     SListI xs
  => InPairs (CrossEraForecaster state view) xs
  -> HardForkState (AnnForecast state view) xs
  -> Forecast (HardForkLedgerView_ view xs)
mkHardForkForecast translations st = Forecast {
      forecastAt  = hcollapse (hmap (K . forecastAt . annForecast) st)
    , forecastFor = \sno -> go sno translations (getHardForkState st)
    }
  where
    go :: SlotNo
       -> InPairs (CrossEraForecaster state view) xs'
       -> Telescope (K Past) (Current (AnnForecast state view)) xs'
       -> Except OutsideForecastRange (Ticked (HardForkLedgerView_ view xs'))
    go sno pairs        (TZ cur)       = oneForecast sno pairs cur
    go sno (PCons _ ts) (TS past rest) = shiftView past <$> go sno ts rest

oneForecast ::
     forall state view blk blks.
     SlotNo
  -> InPairs (CrossEraForecaster state view) (blk : blks)
     -- ^ this function uses at most the first translation
  -> Current (AnnForecast state view) blk
  -> Except OutsideForecastRange (Ticked (HardForkLedgerView_ view (blk : blks)))
oneForecast sno pairs (Current start AnnForecast{..}) =
    case annForecastEnd of
      Nothing  -> endUnknown <$> forecastFor annForecast sno
      Just end ->
        if sno < boundSlot end
        then beforeKnownEnd end <$> forecastFor annForecast sno
        else case pairs of
          PCons translate _ ->
                afterKnownEnd end
            <$> crossEraForecastWith translate end sno annForecastState
          PNil              ->
            -- The requested slot is after the last era the code knows about.
            throwError OutsideForecastRange {
                outsideForecastAt     = forecastAt annForecast
              , outsideForecastMaxFor = boundSlot end
              , outsideForecastFor    = sno
              }
  where
    endUnknown ::
         Ticked (f blk)
      -> Ticked (HardForkLedgerView_ f (blk : blks))
    endUnknown view = TickedHardForkLedgerView {
          tickedHardForkLedgerViewTransition =
            TransitionUnknown annForecastTip
        , tickedHardForkLedgerViewPerEra = HardForkState $
            TZ (Current start (Comp view))
        }

    beforeKnownEnd ::
         Bound
      -> Ticked (f blk)
      -> Ticked (HardForkLedgerView_ f (blk : blks))
    beforeKnownEnd end view = TickedHardForkLedgerView {
          tickedHardForkLedgerViewTransition =
            TransitionKnown (boundEpoch end)
        , tickedHardForkLedgerViewPerEra = HardForkState $
            TZ (Current start (Comp view))
        }

    afterKnownEnd ::
         Bound
      -> Ticked (f blk')
      -> Ticked (HardForkLedgerView_ f (blk : blk' : blks'))
    afterKnownEnd end view = TickedHardForkLedgerView {
          tickedHardForkLedgerViewTransition =
            -- We assume that we only ever have to translate to the /next/ era
            -- (as opposed to /any/ subsequent era)
            TransitionImpossible
        , tickedHardForkLedgerViewPerEra = HardForkState $
            TS (K (Past start end)) $
            TZ (Current end (Comp view))
        }

shiftView :: K Past blk
          -> Ticked (HardForkLedgerView_ f blks)
          -> Ticked (HardForkLedgerView_ f (blk : blks))
shiftView past TickedHardForkLedgerView{..} = TickedHardForkLedgerView {
      tickedHardForkLedgerViewTransition = tickedHardForkLedgerViewTransition
    , tickedHardForkLedgerViewPerEra =
          HardForkState
        . TS past
        . getHardForkState
        $ tickedHardForkLedgerViewPerEra
    }

{-------------------------------------------------------------------------------
  Inspection
-------------------------------------------------------------------------------}

data HardForkLedgerWarning xs =
    -- | Warning from the underlying era
    HardForkWarningInEra (OneEraLedgerWarning xs)

    -- | The transition to the next era does not match the 'EraParams'
    --
    -- The 'EraParams' can specify a lower bound on when the transition to the
    -- next era will happen. If the actual transition, when confirmed, is
    -- /before/ this lower bound, the node is misconfigured and will likely
    -- not work correctly. This should be taken care of as soon as possible
    -- (before the transition happens).
  | HardForkWarningTransitionMismatch (EraIndex xs) EraParams EpochNo

    -- | Transition in the final era
    --
    -- The final era should never confirm any transitions. For clarity, we also
    -- record the index of that final era.
  | HardForkWarningTransitionInFinalEra (EraIndex xs) EpochNo

    -- | An already-confirmed transition got un-confirmed
  | HardForkWarningTransitionUnconfirmed (EraIndex xs)

    -- | An already-confirmed transition got changed
    --
    -- We record the indices of the era we are transitioning from and to,
    -- as well as the old and new 'EpochNo' of that transition, in that order.
  | HardForkWarningTransitionReconfirmed (EraIndex xs) (EraIndex xs) EpochNo EpochNo

data HardForkLedgerUpdate xs =
    HardForkUpdateInEra (OneEraLedgerUpdate xs)

    -- | Hard fork transition got confirmed
  | HardForkUpdateTransitionConfirmed (EraIndex xs) (EraIndex xs) EpochNo

    -- | Hard fork transition happened
    --
    -- We record the 'EpochNo' at the start of the era after the transition
  | HardForkUpdateTransitionDone (EraIndex xs) (EraIndex xs) EpochNo

    -- | The hard fork transition rolled back
  | HardForkUpdateTransitionRolledBack (EraIndex xs) (EraIndex xs)

deriving instance CanHardFork xs => Show (HardForkLedgerWarning xs)
deriving instance CanHardFork xs => Eq   (HardForkLedgerWarning xs)

deriving instance CanHardFork xs => Show (HardForkLedgerUpdate xs)
deriving instance CanHardFork xs => Eq   (HardForkLedgerUpdate xs)

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
  type LedgerUpdate  (HardForkBlock xs) = HardForkLedgerUpdate  xs

  inspectLedger cfg
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
      cfgs  = distribTopLevelConfig ei cfg
      ei    = State.epochInfoLedger (configLedger cfg) after

inspectHardForkLedger ::
     CanHardFork xs
  => NP WrapPartialLedgerConfig xs
  -> NP (K EraParams) xs
  -> NP TopLevelConfig xs
  -> NS (Current (Flip LedgerState mk1)) xs
  -> NS (Current (Flip LedgerState mk2)) xs
  -> [LedgerEvent (HardForkBlock xs)]
inspectHardForkLedger = go
  where
    go :: All SingleEraBlock xs
       => NP WrapPartialLedgerConfig xs
       -> NP (K EraParams) xs
       -> NP TopLevelConfig xs
       -> NS (Current (Flip LedgerState mk1)) xs
       -> NS (Current (Flip LedgerState mk2)) xs
       -> [LedgerEvent (HardForkBlock xs)]

    go (pc :* _) (K ps :* pss) (c :* _) (Z before) (Z after) = concat [
          map liftEvent $
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
              return $ LedgerWarning $
                HardForkWarningTransitionInFinalEra eraIndexZero transition
            (Nil, Just transition, Just transition') -> do
              -- Only warn if the transition has changed
              guard (transition /= transition')
              return $ LedgerWarning $
                HardForkWarningTransitionInFinalEra eraIndexZero transition
            ((:*){}, Nothing, Just transition) ->
              return $
                if validLowerBound (History.eraSafeZone ps)
                  then LedgerUpdate $
                         HardForkUpdateTransitionConfirmed
                           eraIndexZero
                           (eraIndexSucc eraIndexZero)
                           transition
                  else LedgerWarning $
                         HardForkWarningTransitionMismatch
                           eraIndexZero
                           ps
                           transition
            ((:*){}, Just transition, Just transition') -> do
              guard (transition /= transition')
              return $ LedgerWarning $
                HardForkWarningTransitionReconfirmed
                  eraIndexZero
                  (eraIndexSucc eraIndexZero)
                  transition
                  transition'
        ]
      where
        confirmedBefore, confirmedAfter :: Maybe EpochNo
        confirmedBefore = singleEraTransition
                            (unwrapPartialLedgerConfig pc)
                            ps
                            (currentStart before)
                            (unFlip $ currentState before)
        confirmedAfter  = singleEraTransition
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
    validLowerBound (StandardSafeZone _)     = True
    validLowerBound UnsafeIndefiniteSafeZone = False

{-------------------------------------------------------------------------------
  Internal auxiliary: lifting and shifting events
-------------------------------------------------------------------------------}

liftEvent :: LedgerEvent x
          -> LedgerEvent (HardForkBlock (x ': xs))
liftEvent (LedgerWarning warning) = LedgerWarning $ liftWarning warning
liftEvent (LedgerUpdate  update)  = LedgerUpdate  $ liftUpdate  update

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

shiftEvent :: LedgerEvent (HardForkBlock xs)
           -> LedgerEvent (HardForkBlock (x ': xs))
shiftEvent (LedgerWarning warning) = LedgerWarning $ shiftWarning warning
shiftEvent (LedgerUpdate  update)  = LedgerUpdate  $ shiftUpdate  update

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

ledgerInfo :: forall blk mk. SingleEraBlock blk
           => Current (FlipTickedLedgerState mk) blk -> LedgerEraInfo blk
ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

ledgerViewInfo :: forall blk f. SingleEraBlock blk
               => (Ticked :.: f) blk -> LedgerEraInfo blk
ledgerViewInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

injectLedgerError :: Index xs blk -> LedgerError blk -> HardForkLedgerError xs
injectLedgerError index =
      HardForkLedgerErrorFromEra
    . OneEraLedgerError
    . injectNS index
    . WrapLedgerErr

injectLedgerEvent :: Index xs blk -> AuxLedgerEvent (LedgerState blk) -> OneEraLedgerEvent xs
injectLedgerEvent index =
      OneEraLedgerEvent
    . injectNS index
    . WrapLedgerEvent

{-------------------------------------------------------------------------------
  Ledger Tables for the Nary HardForkBlock
-------------------------------------------------------------------------------}

-- | Defaults to a 'CannonicalTxIn' type, but this will probably change in the
-- future to @NS 'WrapTxIn' xs@. See 'HasCanonicalTxIn'.
type instance Key   (LedgerState (HardForkBlock xs)) = CanonicalTxIn xs
type instance Value (LedgerState (HardForkBlock xs)) = NS WrapTxOut xs

-- | Canonical TxIn
--
-- The Ledger and Consensus team discussed the fact that we need to be able to
-- reach the TxIn key for an entry from any era, regardless of the era in which
-- it was created, therefore we need to have a "canonical" serialization that
-- doesn't change between eras. For now we are requiring that a 'HardForkBlock'
-- has only one associated 'TxIn' type as a stop-gap, but Ledger will provide a
-- serialization function into something more efficient.
--
-- TODO: More data added to Tx can be added to TxOut
--
-- TODO: move to separate module
type HasCanonicalTxIn :: [Type] -> Constraint
class ( Show (CanonicalTxIn xs)
      , Ord (CanonicalTxIn xs)
      , NoThunks (CanonicalTxIn xs)
      ) => HasCanonicalTxIn xs where
  data family CanonicalTxIn xs

  injectCanonicalTxIn ::
       Index xs x
    -> Key (LedgerState x)
    -> CanonicalTxIn xs

  -- TODO: should it be called @projectCanonicalTxIn@?
  distribCanonicalTxIn ::
       Index xs x
    -> CanonicalTxIn xs
    -> Key (LedgerState x)

  -- TODO: rename to @encodeCanonicalTxIn@
  serializeCanonicalTxIn :: CanonicalTxIn xs -> CBOR.Encoding

  -- TODO: rename to @decodeCanonicalTxIn@
  deserializeCanonicalTxIn :: forall s. CBOR.Decoder s (CanonicalTxIn xs)

type HardForkHasLedgerTables :: [Type] -> Constraint
type HardForkHasLedgerTables xs = (
    All (Compose HasLedgerTables LedgerState) xs
  , All (Compose HasTickedLedgerTables LedgerState) xs
  , All (Compose Eq WrapTxOut) xs
  , All (Compose Show WrapTxOut) xs
  , All (Compose NoThunks WrapTxOut) xs
  )

instance ( All (Compose CanSerializeLedgerTables LedgerState) xs
         , HasCanonicalTxIn xs
         ) => CanSerializeLedgerTables (LedgerState (HardForkBlock xs)) where
    -- The Ledger and Consensus team discussed the fact that we need to be able
    -- to reach the TxIn key for an entry from any era, regardless of the era in
    -- which it was created, therefore we need to have a "canonical"
    -- serialization that doesn't change between eras. For now we are using
    -- @'toEraCBOR' \@('ShelleyEra' c)@ as a stop-gap, but Ledger will provide a
    -- serialization function into something more efficient.
    codecLedgerTables = LedgerTables $
        CodecMK
          serializeCanonicalTxIn
          encodeTxOut
          deserializeCanonicalTxIn
          decodeTxOut
      where
        encodeTxOut :: NS WrapTxOut xs -> CBOR.Encoding
        encodeTxOut =
              hcollapse
            . hcimap (Proxy @(Compose CanSerializeLedgerTables LedgerState)) each
          where
            each ::
                 forall x. CanSerializeLedgerTables (LedgerState x)
              => Index xs x
              -> WrapTxOut x
              -> K CBOR.Encoding x
            each idx (WrapTxOut txout) = K $
                   CBOR.encodeListLen 2
                <> CBOR.encodeWord8 (toWord8 idx)
                <> encodeValue (getLedgerTables $ codecLedgerTables @(LedgerState x)) txout

        decodeTxOut :: forall s. CBOR.Decoder s (NS WrapTxOut xs)
        decodeTxOut = do
            CBOR.decodeListLenOf 2
            tag <- CBOR.decodeWord8
            case getFirst $ aDecoder tag of
              Nothing -> error $ "decodeTxOut for HardForkBlock, unknown tag: " <> show tag
              Just x  -> x
          where
            each ::
                 forall x. CanSerializeLedgerTables (LedgerState x)
              => Index xs x
              -> forall s'. K (Word8 -> First (CBOR.Decoder s' (NS WrapTxOut xs))) x
            each idx = K $ \w -> First $
                if w /= toWord8 idx then Nothing else
                Just
                  $ injectNS idx . WrapTxOut <$> decodeValue (getLedgerTables $ codecLedgerTables @(LedgerState x))

            aDecoder = mconcat
                    $ hcollapse
                    $ hcmap
                        (Proxy @(Compose CanSerializeLedgerTables LedgerState))
                        each
                        (indices @xs)

instance ( HardForkHasLedgerTables xs
         , CanHardFork xs
         , HasCanonicalTxIn xs
         ) => HasLedgerTables (LedgerState (HardForkBlock xs)) where
  projectLedgerTables ::
       forall mk. (CanMapMK mk, CanMapKeysMK mk, ZeroableMK mk)
    => LedgerState               (HardForkBlock xs)  mk
    -> LedgerTables (LedgerState (HardForkBlock xs)) mk
  projectLedgerTables (HardForkLedgerState st) = hcollapse $
      hcimap (Proxy @(Compose HasLedgerTables LedgerState)) projectOne st
    where
      projectOne ::
           Compose HasLedgerTables LedgerState x
        => Index xs x
        -> Flip LedgerState mk x
        -> K (LedgerTables (LedgerState (HardForkBlock xs)) mk) x
      projectOne i l =
          K
        $ injectLedgerTables i
        $ projectLedgerTables
        $ unFlip l

  withLedgerTables ::
       forall mk any. (CanMapMK mk, CanMapKeysMK mk, ZeroableMK mk)
    => LedgerState               (HardForkBlock xs)  any
    -> LedgerTables (LedgerState (HardForkBlock xs)) mk
    -> LedgerState               (HardForkBlock xs)  mk
  withLedgerTables (HardForkLedgerState st) tables = HardForkLedgerState $
      hcimap (Proxy @(Compose HasLedgerTables LedgerState)) withLedgerTablesOne st
    where
      withLedgerTablesOne ::
           Compose HasLedgerTables LedgerState x
        => Index xs x
        -> Flip LedgerState any x
        -> Flip LedgerState mk  x
      withLedgerTablesOne i l =
          Flip
        $ withLedgerTables (unFlip l)
        $ distribLedgerTables i tables

instance ( HardForkHasLedgerTables xs
         , CanHardFork xs
         , HasCanonicalTxIn xs
         ) => HasLedgerTables (Ticked1 (LedgerState (HardForkBlock xs))) where
  projectLedgerTables ::
       forall mk. (CanMapMK mk, CanMapKeysMK mk, ZeroableMK mk)
    => Ticked1 (LedgerState (HardForkBlock xs)) mk
    -> LedgerTables (Ticked1 (LedgerState (HardForkBlock xs))) mk
  projectLedgerTables st = hcollapse $
      hcimap
        (Proxy @(Compose HasTickedLedgerTables LedgerState))
        projectOne
        (tickedHardForkLedgerStatePerEra st)
    where
      projectOne ::
           Compose HasTickedLedgerTables LedgerState x
        => Index xs x
        -> FlipTickedLedgerState mk x
        -> K (LedgerTables (Ticked1 (LedgerState (HardForkBlock xs))) mk) x
      projectOne i l =
          K
        $ castLedgerTables
        $ injectLedgerTables i
        $ castLedgerTables
        $ projectLedgerTables
        $ getFlipTickedLedgerState l

  withLedgerTables ::
       forall mk any. (CanMapMK mk, CanMapKeysMK mk, ZeroableMK mk)
    => Ticked1 (LedgerState (HardForkBlock xs)) any
    -> LedgerTables (Ticked1 (LedgerState (HardForkBlock xs))) mk
    -> Ticked1 (LedgerState (HardForkBlock xs)) mk
  withLedgerTables st tables = st {
        tickedHardForkLedgerStatePerEra =
          hcimap
            (Proxy @(Compose HasTickedLedgerTables LedgerState))
            withLedgerTablesOne
            (tickedHardForkLedgerStatePerEra st)
      }
    where
      withLedgerTablesOne ::
           Compose HasTickedLedgerTables LedgerState x
        => Index xs x
        -> FlipTickedLedgerState any x
        -> FlipTickedLedgerState mk x
      withLedgerTablesOne i l =
          FlipTickedLedgerState
        $ withLedgerTables (getFlipTickedLedgerState l)
        $ castLedgerTables
        $ distribLedgerTables i (castLedgerTables tables)

instance ( Key (LedgerState (HardForkBlock xs)) ~ Void
         , Value (LedgerState (HardForkBlock xs)) ~ Void
         , All (Compose LedgerTablesAreTrivial LedgerState) xs
         ) => LedgerTablesAreTrivial (LedgerState (HardForkBlock xs)) where
  convertMapKind (HardForkLedgerState st) = HardForkLedgerState $
      hcmap (Proxy @(Compose LedgerTablesAreTrivial LedgerState)) (Flip . convertMapKind . unFlip) st

instance All (Compose CanStowLedgerTables LedgerState) xs
      => CanStowLedgerTables (LedgerState (HardForkBlock xs)) where
  stowLedgerTables ::
       LedgerState (HardForkBlock xs) ValuesMK
    -> LedgerState (HardForkBlock xs) EmptyMK
  stowLedgerTables (HardForkLedgerState st) = HardForkLedgerState $
      hcmap (Proxy @(Compose CanStowLedgerTables LedgerState)) stowOne st
    where
      stowOne ::
           Compose CanStowLedgerTables LedgerState x
        => Flip LedgerState ValuesMK x
        -> Flip LedgerState EmptyMK x
      stowOne = Flip . stowLedgerTables . unFlip

  unstowLedgerTables ::
       LedgerState (HardForkBlock xs) EmptyMK
    -> LedgerState (HardForkBlock xs) ValuesMK
  unstowLedgerTables (HardForkLedgerState st) = HardForkLedgerState $
      hcmap (Proxy @(Compose CanStowLedgerTables LedgerState)) unstowOne st
    where
      unstowOne ::
           Compose CanStowLedgerTables LedgerState x
        => Flip LedgerState EmptyMK x
        -> Flip LedgerState ValuesMK x
      unstowOne = Flip . unstowLedgerTables . unFlip

injectLedgerTables ::
     forall xs x mk. (
          CanMapKeysMK mk
        , CanMapMK mk
        , HasCanonicalTxIn xs
        )
  => Index xs x
  -> LedgerTables (LedgerState                x  ) mk
  -> LedgerTables (LedgerState (HardForkBlock xs)) mk
injectLedgerTables idx =
    LedgerTables
  . mapKeysMK injTxIn
  . mapMK injTxOut
  . getLedgerTables
  where
    injTxIn :: Key (LedgerState x) -> Key (LedgerState (HardForkBlock xs))
    injTxIn  = injectCanonicalTxIn idx

    injTxOut :: Value (LedgerState x) -> Value (LedgerState (HardForkBlock xs))
    injTxOut = injectNS idx . WrapTxOut

distribLedgerTables ::
     forall xs x mk. (
          CanMapKeysMK mk
        , CanMapMK mk
        , Ord (Key (LedgerState x))
        , HasCanonicalTxIn xs
        , CanHardFork xs
        )
  => Index xs x
  -> LedgerTables (LedgerState (HardForkBlock xs)) mk
  -> LedgerTables (LedgerState                x  ) mk
distribLedgerTables idx =
    LedgerTables
  . mapKeysMK distrTxIn
  . mapMK distrTxOut
  . getLedgerTables
  where
    distrTxIn :: Key (LedgerState (HardForkBlock xs)) -> Key (LedgerState x)
    distrTxIn = distribCanonicalTxIn idx

    distrTxOut :: Value (LedgerState (HardForkBlock xs)) -> Value (LedgerState x)
    distrTxOut =
        unwrapTxOut
      . apFn (projectNP idx $ composeTxOutTranslations $ translateTxOut hardForkEraTranslation)
      . K

composeTxOutTranslations ::
     SListI xs
  => InPairs TranslateTxOut xs
  -> NP (K (NS WrapTxOut xs) -.-> WrapTxOut) xs
composeTxOutTranslations = \case
    PNil ->
      fn (unZ . unK) :* Nil
    PCons (TranslateTxOut t) ts ->
      fn ( eitherNS
              id
              (error "composeTranslations: anachrony")
          . unK
         )
      :* hmap
          (\innerf -> fn $
              apFn innerf
            . K
            . eitherNS
                (Z . WrapTxOut . t . unwrapTxOut)
                id
            . unK)
          (composeTxOutTranslations ts)
  where
    eitherNS :: (f x -> c) -> (NS f xs -> c) -> NS f (x ': xs) -> c
    eitherNS l r = \case
      Z x -> l x
      S x -> r x
