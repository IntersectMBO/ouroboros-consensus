{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Protocol (
    -- * Re-exports to keep 'Protocol.State' an internal module
    HardForkCanBeLeader
  , HardForkChainDepState
  , HardForkIsLeader
  , HardForkValidationErr (..)
    -- * Re-exports to keep 'Protocol.LedgerView' an internal module
  , HardForkLedgerView
  , HardForkLedgerView_ (..)
    -- * Type family instances
  , Ticked (..)
    -- * Internal
  , extendToHorizon
  ) where

import           Control.Monad.Except
import           Data.Functor.Product
import           Data.SOP.BasicFunctors
import           Data.SOP.Index
import           Data.SOP.InPairs (InPairs (..), Requiring (..))
import qualified Data.SOP.InPairs as InPairs
import qualified Data.SOP.Match as Match
import qualified Data.SOP.OptNP as OptNP
import           Data.SOP.Strict
import           GHC.Generics (Generic)
import           GHC.Stack
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Block
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
                     (HardForkLedgerView, HardForkLedgerView_ (..), Ticked (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.HardFork.Combinator.Translation
import           Ouroboros.Consensus.HardFork.History (Bound (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers
import           Ouroboros.Consensus.Util ((.:))

{-------------------------------------------------------------------------------
  ChainSelection
-------------------------------------------------------------------------------}

newtype HardForkSelectView xs = HardForkSelectView {
      getHardForkSelectView :: WithBlockNo OneEraSelectView xs
    }
  deriving (Show, Eq)
  deriving newtype (NoThunks)

instance CanHardFork xs => Ord (HardForkSelectView xs) where
  compare (HardForkSelectView l) (HardForkSelectView r) =
     acrossEraSelection
       hardForkChainSel
       (mapWithBlockNo getOneEraSelectView l)
       (mapWithBlockNo getOneEraSelectView r)

mkHardForkSelectView ::
     BlockNo
  -> NS WrapSelectView xs
  -> HardForkSelectView xs
mkHardForkSelectView bno view =
    HardForkSelectView $ WithBlockNo bno (OneEraSelectView view)

{-------------------------------------------------------------------------------
  ConsensusProtocol
-------------------------------------------------------------------------------}

type HardForkChainDepState xs = HardForkState WrapChainDepState xs

instance CanHardFork xs => ConsensusProtocol (HardForkProtocol xs) where
  type ChainDepState (HardForkProtocol xs) = HardForkChainDepState xs
  type ValidationErr (HardForkProtocol xs) = HardForkValidationErr xs
  type SelectView    (HardForkProtocol xs) = HardForkSelectView    xs
  type LedgerView    (HardForkProtocol xs) = HardForkLedgerView    xs
  type HorizonView   (HardForkProtocol xs) = HardForkLedgerView    xs
  type CanBeLeader   (HardForkProtocol xs) = HardForkCanBeLeader   xs
  type IsLeader      (HardForkProtocol xs) = HardForkIsLeader      xs
  type ValidateView  (HardForkProtocol xs) = OneEraValidateView    xs

  -- Operations on the state

  tickChainDepState_    = tick
  checkIsLeader         = check
  updateChainDepState   = update
  reupdateChainDepState = reupdate

  --
  -- Straight-forward extensions
  --

  -- Security parameter must be equal across /all/ eras
  protocolSecurityParam = hardForkConsensusConfigK

  projectHorizonView _cfg = id

{-------------------------------------------------------------------------------
  BlockSupportsProtocol
-------------------------------------------------------------------------------}

instance CanHardFork xs => BlockSupportsProtocol (HardForkBlock xs) where
  validateView HardForkBlockConfig{..} =
        OneEraValidateView
      . hczipWith proxySingle (WrapValidateView .: validateView) cfgs
      . getOneEraHeader
      . getHardForkHeader
    where
      cfgs = getPerEraBlockConfig hardForkBlockConfigPerEra

  selectView HardForkBlockConfig{..} hdr =
        mkHardForkSelectView (blockNo hdr)
      . hczipWith proxySingle (WrapSelectView .: selectView) cfgs
      . getOneEraHeader
      $ getHardForkHeader hdr
    where
      cfgs = getPerEraBlockConfig hardForkBlockConfigPerEra

{-------------------------------------------------------------------------------
  Ticking the chain dependent state
-------------------------------------------------------------------------------}

data instance Ticked (HardForkChainDepState xs) =
    TickedHardForkChainDepState {
        tickedHardForkChainDepStatePerEra ::
             HardForkState (Ticked :.: WrapChainDepState) xs

        -- | 'EpochInfo' constructed from the ticked 'LedgerView'
      , tickedHardForkChainDepStateEpochInfo ::
             EpochInfo (Except PastHorizonException)
      }

tick :: forall xs. CanHardFork xs
     => ConsensusConfig (HardForkProtocol xs)
     -> Ticked (LedgerView (HardForkProtocol xs))
     -> SlotNo
     -> HardForkChainDepState xs
     -> Ticked (HardForkChainDepState xs)
tick cfg ledgerView slot chainDepState =
    TickedHardForkChainDepState {
        tickedHardForkChainDepStateEpochInfo = ei
      , tickedHardForkChainDepStatePerEra    =
          extendToHorizon' finalTick ccfgs lv chainDepState
      }
  where
    HardForkConsensusConfig{..}            = cfg
    TickedHardForkLedgerView transition lv = ledgerView

    ei :: EpochInfo (Except PastHorizonException)
    ei =
        State.epochInfoPrecomputedTransitionInfo
          hardForkConsensusConfigShape
          transition
          lv

    ccfgs :: NP WrapConsensusConfig xs
    ccfgs =
        hcmap
          proxySingle
          (completeConsensusConfig'' ei)
          (getPerEraConsensusConfig hardForkConsensusConfigPerEra)

    -- tick to the final slot, @slot@
    --
    -- This is the final tick and is done according to the rules of whatever
    -- era that final slot is in.
    --
    -- If there is no era transition, then this is the only tick.
    finalTick ::
         NP (     Ticked :.: WrapLedgerView
             -.-> WrapChainDepState
             -.-> Ticked :.: WrapChainDepState
            ) xs
    finalTick =
        hcmap
          proxySingle
          (\(WrapConsensusConfig ccfg) -> fn_2 $ \(Comp tlv) cs ->
                Comp . WrapTickedChainDepState
              $ tickChainDepState
                  ccfg
                  (unwrapTickedLedgerView tlv)
                  slot
                  (unwrapChainDepState cs)
          )
          ccfgs

-- | Extend the given 'HardForkChainDepState' such that it is aligned with the
-- given 'HardForkState'.
--
-- When they are already aligned, this function returns the
-- 'HardForkChainDepState' unmodified.
--
-- For each transition (if any) from @x@ to @y@, this function first ticks just
-- across the era boundary using @x@'s logic, and then translates the @'Ticked'
-- 'ChainDepState' x@ to a @'ChainDepState' y@ using 'translateChainDepState'.
extendToHorizon ::
     forall xs f. CanHardFork xs
  => NP WrapConsensusConfig xs
  -> HardForkState f xs
     -- ^ The tip is unused; we only use the era boundary information.
  -> HardForkChainDepState xs
  -> HardForkChainDepState xs
extendToHorizon = extendToHorizon' (hpure $ fn_2 $ const id)

-- | Like 'extendToHorizon', but also allows to apply a function at the tip.
extendToHorizon' ::
     forall xs f g. CanHardFork xs
  => NP (f -.-> WrapChainDepState -.-> g) xs
  -> NP WrapConsensusConfig xs
  -> HardForkState f xs
  -> HardForkChainDepState xs
  -> HardForkState g xs
extendToHorizon' atTip ccfgs =
    State.align tickAndTranslate atTip
  where
    -- If there is no era transition, then 'State.align' won't invoke this.
    tickAndTranslate :: InPairs (Translate WrapChainDepState) xs
    tickAndTranslate =
        InPairs.requiring ccfgs
      $ InPairs.hcmap
          proxySingle
          (\translation ->
            Require $ \(WrapConsensusConfig ccfg) ->
            Translate $ \bound x ->
                -- ...then translate
                translateWith translation bound
              $ Comp . WrapTickedChainDepState
              $ -- first tick just across the era boundary...
                tickChainDepState_
                  ccfg
                  (eraTransitionHorizonView ccfg)
                  (boundSlot bound)
                  (unwrapChainDepState x)
          )
      $ InPairs.requiringBoth ccfgs
      $ translateChainDepState hardForkEraTranslation

{-------------------------------------------------------------------------------
  Leader check

  NOTE: The precondition to 'align' is satisfied: the consensus state will never
  be ahead (but possibly behind) the ledger state, which we tick first.
-------------------------------------------------------------------------------}

-- | We are a leader if we have a proof from one of the eras
type HardForkIsLeader xs = OneEraIsLeader xs

-- | We have one or more 'BlockForging's, and thus 'CanBeLeader' proofs, for
-- each era in which we can forge blocks.
type HardForkCanBeLeader xs = SomeErasCanBeLeader xs

-- | POSTCONDITION: if the result is @Just isLeader@, then 'HardForkCanBeLeader'
-- and the ticked 'ChainDepState' must be in the same era. The returned
-- @isLeader@ will be from the same era.
check :: forall xs. (CanHardFork xs, HasCallStack)
      => ConsensusConfig (HardForkProtocol xs)
      -> HardForkCanBeLeader xs
      -> Ticked (LedgerView (HardForkProtocol xs))
      -> SlotNo
      -> Ticked (ChainDepState (HardForkProtocol xs))
      -> Maybe (HardForkIsLeader xs)
check HardForkConsensusConfig{..}
      (SomeErasCanBeLeader canBeLeader)
      (TickedHardForkLedgerView _transition tickedLedgerView)
      slot
      (TickedHardForkChainDepState chainDepState ei) =
    case eLedgerData of
      Left{}          -> Nothing
      Right matchPair ->
          undistrib
        $ hcpure proxySingle (fn_3 checkOne)
            `hap` cfgs
            `hap` (OptNP.toNP canBeLeader)
            `hap` (State.tip matchPair)
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra

    eLedgerData = State.match (State.tip tickedLedgerView) chainDepState

    checkOne ::
         SingleEraBlock                 blk
      => WrapPartialConsensusConfig     blk
      -> (Maybe :.: WrapCanBeLeader)    blk
      -> ((Ticked :.: WrapLedgerView) `Product` (Ticked :.: WrapChainDepState)) blk
      -> (Maybe :.: WrapIsLeader)       blk
    checkOne cfg' (Comp mCanBeLeader) (Comp ledgerView' `Pair` Comp chainDepState') = Comp $ do
        canBeLeader' <- mCanBeLeader
        WrapIsLeader <$>
          checkIsLeader
            (completeConsensusConfig' ei cfg')
            (unwrapCanBeLeader canBeLeader')
            (unwrapTickedLedgerView ledgerView')
            slot
            (unwrapTickedChainDepState chainDepState')

    undistrib :: NS (Maybe :.: WrapIsLeader) xs -> Maybe (HardForkIsLeader xs)
    undistrib = hcollapse . himap inj
      where
        inj :: Index xs blk
            -> (Maybe :.: WrapIsLeader) blk
            -> K (Maybe (HardForkIsLeader xs)) blk
        inj index (Comp mIsLeader) = K $
            OneEraIsLeader . injectNS index <$> mIsLeader

{-------------------------------------------------------------------------------
  Rolling forward and backward
-------------------------------------------------------------------------------}

data HardForkValidationErr xs =
    -- | Validation error from one of the eras
    HardForkValidationErrFromEra (OneEraValidationErr xs)

    -- | We tried to apply a block from the wrong era
  | HardForkValidationErrWrongEra (MismatchEraInfo xs)
  deriving (Generic)

update :: forall xs. CanHardFork xs
       => ConsensusConfig (HardForkProtocol xs)
       -> OneEraValidateView xs
       -> Ticked (HardForkLedgerView xs)
       -> SlotNo
       -> Ticked (HardForkChainDepState xs)
       -> Except (HardForkValidationErr xs) (HardForkChainDepState xs)
update HardForkConsensusConfig{..}
       (OneEraValidateView view)
       ledgerView
       slot
       (TickedHardForkChainDepState chainDepState ei) =
    case State.match view chainDepState of
      Left mismatch ->
        throwError $ HardForkValidationErrWrongEra . MismatchEraInfo $
          Match.bihcmap
            proxySingle
            singleEraInfo
            (LedgerEraInfo . chainDepStateInfo . State.currentState)
            mismatch
      Right matchedPair -> case State.match (State.tip matchedPair) tickedHardForkLedgerViewPerEra of
        Left mismatch ->
          throwError $ HardForkValidationErrWrongEra . MismatchEraInfo $
            Match.bihcmap
              proxySingle
              singleEraInfo
              (LedgerEraInfo . ledgerViewInfo . State.currentState)
              mismatch
        Right matchedTriple ->
           hsequence'
         . hcizipWith proxySingle (updateEra ei slot) cfgs
         $ matchedTriple
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra

    TickedHardForkLedgerView {
        tickedHardForkLedgerViewTransition = _
      , tickedHardForkLedgerViewPerEra
      } = ledgerView

updateEra :: forall xs blk. SingleEraBlock blk
          => EpochInfo (Except PastHorizonException)
          -> SlotNo
          -> Index xs blk
          -> WrapPartialConsensusConfig blk
          -> (WrapValidateView `Product` (Ticked :.: WrapChainDepState) `Product` (Ticked :.: WrapLedgerView)) blk
          -> (Except (HardForkValidationErr xs) :.: WrapChainDepState) blk
updateEra ei slot index cfg
          (view `Pair` Comp chainDepState `Pair` Comp ledgerView) = Comp $
    withExcept (injectValidationErr index) $
      fmap WrapChainDepState $
        updateChainDepState
          (completeConsensusConfig' ei cfg)
          (unwrapValidateView view)
          (unwrapTickedLedgerView ledgerView)
          slot
          (unwrapTickedChainDepState chainDepState)

reupdate :: forall xs. CanHardFork xs
         => ConsensusConfig (HardForkProtocol xs)
         -> OneEraValidateView xs
         -> Ticked (HardForkLedgerView xs)
         -> SlotNo
         -> Ticked (HardForkChainDepState xs)
         -> HardForkChainDepState xs
reupdate HardForkConsensusConfig{..}
         (OneEraValidateView view)
         ledgerView
         slot
         (TickedHardForkChainDepState chainDepState ei) =
    case State.match view chainDepState of
      Left mismatch ->
        error $ show . HardForkValidationErrWrongEra . MismatchEraInfo $
          Match.bihcmap
            proxySingle
            singleEraInfo
            (LedgerEraInfo . chainDepStateInfo . State.currentState)
            mismatch
      Right matchedPair -> case State.match (State.tip matchedPair) tickedHardForkLedgerViewPerEra of
        Left mismatch ->
          error $ show . HardForkValidationErrWrongEra . MismatchEraInfo $
            Match.bihcmap
              proxySingle
              singleEraInfo
              (LedgerEraInfo . ledgerViewInfo . State.currentState)
              mismatch
        Right matchedTriple ->
           hczipWith proxySingle (reupdateEra ei slot) cfgs
         $ matchedTriple
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra

    TickedHardForkLedgerView {
        tickedHardForkLedgerViewTransition = _
      , tickedHardForkLedgerViewPerEra
      } = ledgerView

reupdateEra :: SingleEraBlock blk
            => EpochInfo (Except PastHorizonException)
            -> SlotNo
            -> WrapPartialConsensusConfig blk
            -> (WrapValidateView `Product` (Ticked :.: WrapChainDepState) `Product` (Ticked :.: WrapLedgerView)) blk
            -> WrapChainDepState blk
reupdateEra ei slot cfg (view `Pair` Comp chainDepState `Pair` Comp ledgerView) =
    WrapChainDepState $
      reupdateChainDepState
        (completeConsensusConfig' ei cfg)
        (unwrapValidateView view)
        (unwrapTickedLedgerView ledgerView)
        slot
        (unwrapTickedChainDepState chainDepState)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

chainDepStateInfo :: forall blk. SingleEraBlock blk
                  => (Ticked :.: WrapChainDepState) blk -> SingleEraInfo blk
chainDepStateInfo _ = singleEraInfo (Proxy @blk)

ledgerViewInfo :: forall blk. SingleEraBlock blk
                  => (Ticked :.: WrapLedgerView) blk -> SingleEraInfo blk
ledgerViewInfo _ = singleEraInfo (Proxy @blk)

injectValidationErr :: Index xs blk
                    -> ValidationErr (BlockProtocol blk)
                    -> HardForkValidationErr xs
injectValidationErr index =
      HardForkValidationErrFromEra
    . OneEraValidationErr
    . injectNS index
    . WrapValidationErr

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving instance CanHardFork xs => Eq       (HardForkValidationErr xs)
deriving instance CanHardFork xs => Show     (HardForkValidationErr xs)
deriving instance CanHardFork xs => NoThunks (HardForkValidationErr xs)
