{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.HardFork.Combinator.Protocol
  ( HardForkTiebreakerView (..)

    -- * Re-exports to keep 'Protocol.State' an internal module
  , HardForkCanBeLeader
  , HardForkChainDepState
  , HardForkIsLeader
  , HardForkValidationErr (..)

    -- * Re-exports to keep 'Protocol.LedgerView' an internal module
  , HardForkLedgerView
  , HardForkLedgerView_ (..)

    -- * Type family instances
  , Ticked (..)
  ) where

import Control.Monad.Except
import Data.Functor.Product
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.InPairs (InPairs (..))
import qualified Data.SOP.InPairs as InPairs
import Data.SOP.Index
import qualified Data.SOP.Match as Match
import qualified Data.SOP.OptNP as OptNP
import Data.SOP.Strict
import GHC.Generics (Generic)
import GHC.Stack
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HardFork.Combinator.Abstract
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import Ouroboros.Consensus.HardFork.Combinator.Basics
import Ouroboros.Consensus.HardFork.Combinator.Block
import Ouroboros.Consensus.HardFork.Combinator.Info
import Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel
import Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
  ( HardForkLedgerView
  , HardForkLedgerView_ (..)
  )
import Ouroboros.Consensus.HardFork.Combinator.State
  ( HardForkState
  , Translate (..)
  )
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import Ouroboros.Consensus.HardFork.Combinator.Translation as HFTranslation
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util ((.:))

{-------------------------------------------------------------------------------
  ChainSelection
-------------------------------------------------------------------------------}

newtype HardForkTiebreakerView xs = HardForkTiebreakerView
  { getHardForkTiebreakerView :: OneEraTiebreakerView xs
  }
  deriving (Show, Eq)
  deriving newtype NoThunks

instance CanHardFork xs => Ord (HardForkTiebreakerView xs) where
  compare (HardForkTiebreakerView l) (HardForkTiebreakerView r) =
    acrossEraSelection
      AcrossEraCompare
      (hpure Proxy)
      hardForkChainSel
      (getOneEraTiebreakerView l)
      (getOneEraTiebreakerView r)

instance CanHardFork xs => ChainOrder (HardForkTiebreakerView xs) where
  type ChainOrderConfig (HardForkTiebreakerView xs) = PerEraChainOrderConfig xs

  preferCandidate
    (PerEraChainOrderConfig cfg)
    (HardForkTiebreakerView ours)
    (HardForkTiebreakerView cand) =
      acrossEraSelection
        AcrossEraPreferCandidate
        cfg
        hardForkChainSel
        (getOneEraTiebreakerView ours)
        (getOneEraTiebreakerView cand)

{-------------------------------------------------------------------------------
  ConsensusProtocol
-------------------------------------------------------------------------------}

type HardForkChainDepState xs = HardForkState WrapChainDepState xs

instance CanHardFork xs => ConsensusProtocol (HardForkProtocol xs) where
  type ChainDepState (HardForkProtocol xs) = HardForkChainDepState xs
  type ValidationErr (HardForkProtocol xs) = HardForkValidationErr xs
  type TiebreakerView (HardForkProtocol xs) = HardForkTiebreakerView xs
  type LedgerView (HardForkProtocol xs) = HardForkLedgerView xs
  type CanBeLeader (HardForkProtocol xs) = HardForkCanBeLeader xs
  type IsLeader (HardForkProtocol xs) = HardForkIsLeader xs
  type ValidateView (HardForkProtocol xs) = OneEraValidateView xs

  -- Operations on the state

  tickChainDepState = tick
  checkIsLeader = check
  updateChainDepState = update
  reupdateChainDepState = reupdate

  --
  -- Straight-forward extensions
  --

  -- Security parameter must be equal across /all/ eras
  protocolSecurityParam = hardForkConsensusConfigK

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

  tiebreakerView HardForkBlockConfig{..} hdr =
    HardForkTiebreakerView
      . OneEraTiebreakerView
      . hczipWith proxySingle (WrapTiebreakerView .: tiebreakerView) cfgs
      . getOneEraHeader
      $ getHardForkHeader hdr
   where
    cfgs = getPerEraBlockConfig hardForkBlockConfigPerEra

  projectChainOrderConfig =
    PerEraChainOrderConfig
      . hcmap proxySingle (WrapChainOrderConfig . projectChainOrderConfig)
      . getPerEraBlockConfig
      . hardForkBlockConfigPerEra

{-------------------------------------------------------------------------------
  Ticking the chain dependent state
-------------------------------------------------------------------------------}

data instance Ticked (HardForkChainDepState xs)
  = TickedHardForkChainDepState
  { tickedHardForkChainDepStatePerEra ::
      HardForkState (Ticked :.: WrapChainDepState) xs
  , tickedHardForkChainDepStateEpochInfo ::
      EpochInfo (Except PastHorizonException)
  -- ^ 'EpochInfo' constructed from the 'LedgerView'
  }

tick ::
  CanHardFork xs =>
  ConsensusConfig (HardForkProtocol xs) ->
  HardForkLedgerView xs ->
  SlotNo ->
  HardForkChainDepState xs ->
  Ticked (HardForkChainDepState xs)
tick
  cfg@HardForkConsensusConfig{..}
  (HardForkLedgerView transition ledgerView)
  slot
  chainDepState =
    TickedHardForkChainDepState
      { tickedHardForkChainDepStateEpochInfo = ei
      , tickedHardForkChainDepStatePerEra =
          State.align
            (translateConsensus ei cfg)
            (hcmap proxySingle (fn_2 . tickOne) cfgs)
            ledgerView
            chainDepState
      }
   where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra
    ei =
      State.epochInfoPrecomputedTransitionInfo
        hardForkConsensusConfigShape
        transition
        ledgerView

    tickOne ::
      SingleEraBlock blk =>
      WrapPartialConsensusConfig blk ->
      WrapLedgerView blk ->
      WrapChainDepState blk ->
      (Ticked :.: WrapChainDepState) blk
    tickOne cfg' ledgerView' chainDepState' =
      Comp $
        WrapTickedChainDepState $
          tickChainDepState
            (completeConsensusConfig' ei cfg')
            (unwrapLedgerView ledgerView')
            slot
            (unwrapChainDepState chainDepState')

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
check ::
  forall xs.
  (CanHardFork xs, HasCallStack) =>
  ConsensusConfig (HardForkProtocol xs) ->
  HardForkCanBeLeader xs ->
  SlotNo ->
  Ticked (ChainDepState (HardForkProtocol xs)) ->
  Maybe (HardForkIsLeader xs)
check
  HardForkConsensusConfig{..}
  (SomeErasCanBeLeader canBeLeader)
  slot
  (TickedHardForkChainDepState chainDepState ei) =
    undistrib $
      hczipWith3
        proxySingle
        checkOne
        cfgs
        (OptNP.toNP canBeLeader)
        (State.tip chainDepState)
   where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra

    checkOne ::
      SingleEraBlock blk =>
      WrapPartialConsensusConfig blk ->
      (Maybe :.: WrapCanBeLeader) blk ->
      (Ticked :.: WrapChainDepState) blk ->
      (Maybe :.: WrapIsLeader) blk
    checkOne cfg' (Comp mCanBeLeader) (Comp chainDepState') = Comp $ do
      canBeLeader' <- mCanBeLeader
      WrapIsLeader
        <$> checkIsLeader
          (completeConsensusConfig' ei cfg')
          (unwrapCanBeLeader canBeLeader')
          slot
          (unwrapTickedChainDepState chainDepState')

    undistrib :: NS (Maybe :.: WrapIsLeader) xs -> Maybe (HardForkIsLeader xs)
    undistrib = hcollapse . himap inj
     where
      inj ::
        Index xs blk ->
        (Maybe :.: WrapIsLeader) blk ->
        K (Maybe (HardForkIsLeader xs)) blk
      inj index (Comp mIsLeader) =
        K $
          OneEraIsLeader . injectNS index <$> mIsLeader

{-------------------------------------------------------------------------------
  Rolling forward and backward
-------------------------------------------------------------------------------}

data HardForkValidationErr xs
  = -- | Validation error from one of the eras
    HardForkValidationErrFromEra (OneEraValidationErr xs)
  | -- | We tried to apply a block from the wrong era
    HardForkValidationErrWrongEra (MismatchEraInfo xs)
  deriving Generic

update ::
  forall xs.
  CanHardFork xs =>
  ConsensusConfig (HardForkProtocol xs) ->
  OneEraValidateView xs ->
  SlotNo ->
  Ticked (HardForkChainDepState xs) ->
  Except (HardForkValidationErr xs) (HardForkChainDepState xs)
update
  HardForkConsensusConfig{..}
  (OneEraValidateView view)
  slot
  (TickedHardForkChainDepState chainDepState ei) =
    case State.match view chainDepState of
      Left mismatch ->
        throwError $
          HardForkValidationErrWrongEra . MismatchEraInfo $
            Match.bihcmap
              proxySingle
              singleEraInfo
              (LedgerEraInfo . chainDepStateInfo . State.currentState)
              mismatch
      Right matched ->
        hsequence'
          . hcizipWith proxySingle (updateEra ei slot) cfgs
          $ matched
   where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra

updateEra ::
  forall xs blk.
  (SListI xs, SingleEraBlock blk) =>
  EpochInfo (Except PastHorizonException) ->
  SlotNo ->
  Index xs blk ->
  WrapPartialConsensusConfig blk ->
  Product WrapValidateView (Ticked :.: WrapChainDepState) blk ->
  (Except (HardForkValidationErr xs) :.: WrapChainDepState) blk
updateEra
  ei
  slot
  index
  cfg
  (Pair view (Comp chainDepState)) =
    Comp $
      withExcept (injectValidationErr index) $
        fmap WrapChainDepState $
          updateChainDepState
            (completeConsensusConfig' ei cfg)
            (unwrapValidateView view)
            slot
            (unwrapTickedChainDepState chainDepState)

reupdate ::
  forall xs.
  CanHardFork xs =>
  ConsensusConfig (HardForkProtocol xs) ->
  OneEraValidateView xs ->
  SlotNo ->
  Ticked (HardForkChainDepState xs) ->
  HardForkChainDepState xs
reupdate
  HardForkConsensusConfig{..}
  (OneEraValidateView view)
  slot
  (TickedHardForkChainDepState chainDepState ei) =
    case State.match view chainDepState of
      Left mismatch ->
        error $
          show . HardForkValidationErrWrongEra . MismatchEraInfo $
            Match.bihcmap
              proxySingle
              singleEraInfo
              (LedgerEraInfo . chainDepStateInfo . State.currentState)
              mismatch
      Right matched ->
        hczipWith proxySingle (reupdateEra ei slot) cfgs $
          matched
   where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra

reupdateEra ::
  SingleEraBlock blk =>
  EpochInfo (Except PastHorizonException) ->
  SlotNo ->
  WrapPartialConsensusConfig blk ->
  Product WrapValidateView (Ticked :.: WrapChainDepState) blk ->
  WrapChainDepState blk
reupdateEra ei slot cfg (Pair view (Comp chainDepState)) =
  WrapChainDepState $
    reupdateChainDepState
      (completeConsensusConfig' ei cfg)
      (unwrapValidateView view)
      slot
      (unwrapTickedChainDepState chainDepState)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

chainDepStateInfo ::
  forall blk.
  SingleEraBlock blk =>
  (Ticked :.: WrapChainDepState) blk -> SingleEraInfo blk
chainDepStateInfo _ = singleEraInfo (Proxy @blk)

translateConsensus ::
  forall xs.
  CanHardFork xs =>
  EpochInfo (Except PastHorizonException) ->
  ConsensusConfig (HardForkProtocol xs) ->
  InPairs (Translate WrapChainDepState) xs
translateConsensus ei HardForkConsensusConfig{..} =
  InPairs.requiringBoth cfgs $
    HFTranslation.translateChainDepState hardForkEraTranslation
 where
  pcfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra
  cfgs = hcmap proxySingle (completeConsensusConfig'' ei) pcfgs

injectValidationErr ::
  SListI xs =>
  Index xs blk ->
  ValidationErr (BlockProtocol blk) ->
  HardForkValidationErr xs
injectValidationErr index =
  HardForkValidationErrFromEra
    . OneEraValidationErr
    . injectNS index
    . WrapValidationErr

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving instance CanHardFork xs => Eq (HardForkValidationErr xs)
deriving instance CanHardFork xs => Show (HardForkValidationErr xs)
deriving instance CanHardFork xs => NoThunks (HardForkValidationErr xs)
