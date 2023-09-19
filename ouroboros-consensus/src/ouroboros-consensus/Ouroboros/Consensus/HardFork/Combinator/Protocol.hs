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
  , distribPreparedChainDepState
    -- * Re-exports to keep 'Protocol.LedgerView' an internal module
  , HardForkLedgerView
  , HardForkLedgerView_ (..)
    -- * Type family instances
  , Ticked (..)
  ) where

import           Control.Monad.Except
import           Data.Functor.Product
import           Data.SOP.BasicFunctors
import           Data.SOP.Index
import           Data.SOP.InPairs (InPairs (..))
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
import           Ouroboros.Consensus.HardFork.Combinator.State (HardForkState,
                     Translate (..))
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.Translation
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
  type CanBeLeader   (HardForkProtocol xs) = HardForkCanBeLeader   xs
  type IsLeader      (HardForkProtocol xs) = HardForkIsLeader      xs
  type ValidateView  (HardForkProtocol xs) = OneEraValidateView    xs

  -- Operations on the state

  tickChainDepState     = tick
  checkIsLeader         = check
  updateChainDepState   = update
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

tick :: CanHardFork xs
     => ConsensusConfig (HardForkProtocol xs)
     -> Ticked (HardForkLedgerView xs)
     -> SlotNo
     -> HardForkChainDepState xs
     -> Ticked (HardForkChainDepState xs)
tick cfg@HardForkConsensusConfig{..}
     (TickedHardForkLedgerView transition ledgerView)
     slot
     chainDepState = TickedHardForkChainDepState {
      tickedHardForkChainDepStateEpochInfo = ei
    , tickedHardForkChainDepStatePerEra =
         State.align
           (translateConsensus ei cfg)
           (hcmap proxySingle (fn_2 . tickOne) cfgs)
           ledgerView
           chainDepState
    }
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra
    ei   = State.epochInfoPrecomputedTransitionInfo
             hardForkConsensusConfigShape
             transition
             ledgerView

    tickOne :: SingleEraBlock                 blk
            => WrapPartialConsensusConfig     blk
            -> (Ticked :.: WrapLedgerView)    blk
            -> WrapChainDepState              blk
            -> (Ticked :.: WrapChainDepState) blk
    tickOne cfg' (Comp ledgerView') chainDepState' = Comp $
        WrapTickedChainDepState $
          tickChainDepState
            (completeConsensusConfig' ei cfg')
            (unwrapTickedLedgerView ledgerView')
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
check :: forall xs. (CanHardFork xs, HasCallStack)
      => ConsensusConfig (HardForkProtocol xs)
      -> HardForkCanBeLeader xs
      -> PreparedChainDepState (HardForkProtocol xs)
      -> Maybe (HardForkIsLeader xs)
check HardForkConsensusConfig{..}
      (SomeErasCanBeLeader canBeLeader)
      pcst =
    undistrib $
      hczipWith3
        proxySingle
        checkOne
        cfgs
        (OptNP.toNP canBeLeader)
        (State.tip (distribPreparedChainDepState pcst))
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra

    PreparedChainDepState{tickedChainDepState} = pcst

    TickedHardForkChainDepState{tickedHardForkChainDepStateEpochInfo = ei} = tickedChainDepState

    checkOne ::
         SingleEraBlock              blk
      => WrapPartialConsensusConfig  blk
      -> (Maybe :.: WrapCanBeLeader) blk
      -> WrapPreparedChainDepState   blk
      -> (Maybe :.: WrapIsLeader)    blk
    checkOne cfg' (Comp mCanBeLeader) pcst' = Comp $ do
        canBeLeader' <- mCanBeLeader
        WrapIsLeader <$>
          checkIsLeader
            (completeConsensusConfig' ei cfg')
            (unwrapCanBeLeader canBeLeader')
            (unwrapPreparedChainDepState pcst')

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
       -> PreparedChainDepState (HardForkProtocol xs)
       -> Except (HardForkValidationErr xs) (HardForkChainDepState xs)
update HardForkConsensusConfig{..}
       (OneEraValidateView view)
       pcst =
    case State.match view (distribPreparedChainDepState pcst) of
      Left mismatch ->
        throwError $ HardForkValidationErrWrongEra . MismatchEraInfo $
          Match.bihcmap
            proxySingle
            singleEraInfo
            (LedgerEraInfo . preparedChainDepStateInfo . State.currentState)
            mismatch
      Right matched ->
           hsequence'
         . hcizipWith proxySingle (updateEra ei) cfgs
         $ matched
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra

    PreparedChainDepState{tickedChainDepState} = pcst

    TickedHardForkChainDepState{tickedHardForkChainDepStateEpochInfo = ei} = tickedChainDepState

updateEra :: forall xs blk. SingleEraBlock blk
          => EpochInfo (Except PastHorizonException)
          -> Index xs blk
          -> WrapPartialConsensusConfig blk
          -> Product WrapValidateView WrapPreparedChainDepState blk
          -> (Except (HardForkValidationErr xs) :.: WrapChainDepState) blk
updateEra ei index cfg (Pair view pcst) = Comp $
    withExcept (injectValidationErr index) $
      fmap WrapChainDepState $
        updateChainDepState
          (completeConsensusConfig' ei cfg)
          (unwrapValidateView view)
          (unwrapPreparedChainDepState pcst)

reupdate :: forall xs. CanHardFork xs
         => ConsensusConfig (HardForkProtocol xs)
         -> OneEraValidateView xs
         -> PreparedChainDepState (HardForkProtocol xs)
         -> HardForkChainDepState xs
reupdate HardForkConsensusConfig{..}
         (OneEraValidateView view)
         pcst =
    case State.match view (distribPreparedChainDepState pcst) of
      Left mismatch ->
        error $ show . HardForkValidationErrWrongEra . MismatchEraInfo $
          Match.bihcmap
            proxySingle
            singleEraInfo
            (LedgerEraInfo . preparedChainDepStateInfo . State.currentState)
            mismatch
      Right matched ->
           hczipWith proxySingle (reupdateEra ei) cfgs
         $ matched
  where
    cfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra

    PreparedChainDepState{tickedChainDepState} = pcst

    TickedHardForkChainDepState{tickedHardForkChainDepStateEpochInfo = ei} = tickedChainDepState

reupdateEra :: SingleEraBlock blk
            => EpochInfo (Except PastHorizonException)
            -> WrapPartialConsensusConfig blk
            -> Product WrapValidateView WrapPreparedChainDepState blk
            -> WrapChainDepState blk
reupdateEra ei cfg (Pair view pcst) =
    WrapChainDepState $
      reupdateChainDepState
        (completeConsensusConfig' ei cfg)
        (unwrapValidateView view)
        (unwrapPreparedChainDepState pcst)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

preparedChainDepStateInfo :: forall blk. SingleEraBlock blk
                          => WrapPreparedChainDepState blk -> SingleEraInfo blk
preparedChainDepStateInfo _ = singleEraInfo (Proxy @blk)

translateConsensus :: forall xs. CanHardFork xs
                   => EpochInfo (Except PastHorizonException)
                   -> ConsensusConfig (HardForkProtocol xs)
                   -> InPairs (Translate WrapChainDepState) xs
translateConsensus ei HardForkConsensusConfig{..} =
    InPairs.requiringBoth cfgs $
       translateChainDepState hardForkEraTranslation
  where
    pcfgs = getPerEraConsensusConfig hardForkConsensusConfigPerEra
    cfgs  = hcmap proxySingle (completeConsensusConfig'' ei) pcfgs

injectValidationErr :: Index xs blk
                    -> ValidationErr (BlockProtocol blk)
                    -> HardForkValidationErr xs
injectValidationErr index =
      HardForkValidationErrFromEra
    . OneEraValidationErr
    . injectNS index
    . WrapValidationErr

distribPreparedChainDepState :: CanHardFork xs
                             => PreparedChainDepState (HardForkProtocol xs)
                             -> HardForkState WrapPreparedChainDepState xs
distribPreparedChainDepState pcst =
    case State.match
           (State.tip tickedHardForkLedgerViewPerEra)
           tickedHardForkChainDepStatePerEra
    of
        Left _err ->   -- impossible according to the INVARIANT of 'tickedChainDepState'
            error "HFC tick INVARIANT violation"
        Right nspair ->
            hmap
              (\(Comp tlv `Pair` Comp tcst) ->
                  WrapPreparedChainDepState
                $ PreparedChainDepState {
                      tickedChainDepState = unwrapTickedChainDepState tcst
                    , tickedLedgerView    = unwrapTickedLedgerView tlv
                    , tickedToSlot
                    }
              )
              nspair
  where
    PreparedChainDepState {
        tickedChainDepState
      , tickedLedgerView
      , tickedToSlot
      } = pcst

    TickedHardForkLedgerView {
        tickedHardForkLedgerViewPerEra
      } = tickedLedgerView

    TickedHardForkChainDepState {
        tickedHardForkChainDepStatePerEra
      } = tickedChainDepState

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

deriving instance CanHardFork xs => Eq       (HardForkValidationErr xs)
deriving instance CanHardFork xs => Show     (HardForkValidationErr xs)
deriving instance CanHardFork xs => NoThunks (HardForkValidationErr xs)
