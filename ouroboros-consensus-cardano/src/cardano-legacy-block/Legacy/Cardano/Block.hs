{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Legacy.Cardano.Block (
    LegacyCardanoEras
  , LegacyCardanoShelleyEras
  , LegacyCardanoBlock
  ) where

import           Control.Monad.Except
import           Data.Functor ((<&>))
import           Data.Functor.Product
import           Data.Kind
import           Data.Proxy
import           Data.SOP.Functors (Flip (..))
import           Data.SOP.Index
import           Data.SOP.Strict hiding (shape, tl)
import           Legacy.LegacyBlock
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Byron.Ledger
import           Ouroboros.Consensus.Cardano.Block
import           Ouroboros.Consensus.HardFork.Combinator.Abstract
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import           Ouroboros.Consensus.HardFork.Combinator.Basics
import           Ouroboros.Consensus.HardFork.Combinator.Info
import           Ouroboros.Consensus.HardFork.Combinator.Ledger
import           Ouroboros.Consensus.HardFork.Combinator.PartialConfig
import           Ouroboros.Consensus.HardFork.Combinator.Protocol ()
import           Ouroboros.Consensus.HardFork.Combinator.Protocol.LedgerView
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State
import           Ouroboros.Consensus.HardFork.Combinator.State.Types
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Tables.Utils
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Protocol.TPraos
import           Ouroboros.Consensus.Shelley.Ledger
import           Ouroboros.Consensus.TypeFamilyWrappers

type LegacyCardanoEras :: Type -> [Type]
type LegacyCardanoEras c = ByronBlock ': LegacyCardanoShelleyEras c

type LegacyCardanoShelleyEras :: Type -> [Type]
type LegacyCardanoShelleyEras c =
  '[ LegacyBlock (ShelleyBlock (TPraos c) (ShelleyEra c))
   , LegacyBlock (ShelleyBlock (TPraos c) (AllegraEra c))
   , LegacyBlock (ShelleyBlock (TPraos c) (MaryEra c))
   , LegacyBlock (ShelleyBlock (TPraos c) (AlonzoEra c))
   , LegacyBlock (ShelleyBlock (Praos c)  (BabbageEra c))
   , LegacyBlock (ShelleyBlock (Praos c)  (ConwayEra c))
   ]

type LegacyCardanoBlock c = LegacyBlock (HardForkBlock (LegacyCardanoEras c))

{-------------------------------------------------------------------------------
  Ticking
-------------------------------------------------------------------------------}

instance CanHardFork (LegacyCardanoEras c)
      => IsLedger (LedgerState (LegacyCardanoBlock c)) where
  type LedgerErr (LedgerState (LegacyCardanoBlock c)) = ()

  type AuxLedgerEvent (LedgerState (LegacyCardanoBlock c)) = ()

  applyChainTickLedgerResult cfg@HardForkLedgerConfig{..} slot (LegacyLedgerState (HardForkLedgerState st)) =
      sequenceHardForkState
        (hcizipWith proxySingle (\index cfg1 diff -> Comp . fmap (FlipTickedLedgerState . flip withLedgerTablesTicked emptyLedgerTables . getFlipTickedLedgerState)
                                                         . unComp $ tickOne ei slot index cfg1 diff) cfgs extended) <&> \l' ->
      TickedLegacyLedgerState $ TickedHardForkLedgerState {
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

      extended :: HardForkState (Flip LedgerState DiffMK) (LegacyCardanoEras c)
      extended = State.extendToSlot cfg slot st

tickOne :: (SingleEraBlock blk, xs ~ LegacyCardanoEras c)
        => EpochInfo (Except PastHorizonException)
        -> SlotNo
        -> Index                                          xs   blk
        -> WrapPartialLedgerConfig                             blk
        -> (Flip LedgerState DiffMK)                           blk
        -> (     LedgerResult (LedgerState (LegacyBlock (HardForkBlock xs)))
             :.: FlipTickedLedgerState DiffMK
           )                                                   blk
tickOne ei slot sopIdx partialCfg st =
      Comp
    . fmap ( FlipTickedLedgerState
           . prependLedgerTablesDiffsTicked (unFlip st)
           )
    . embedLedgerResult (injectLedgerEvent sopIdx)
    . applyChainTickLedgerResult (completeLedgerConfig' ei partialCfg) slot
    . forgetLedgerTables
    . unFlip
    $ st

{-------------------------------------------------------------------------------
  ApplyBlock
-------------------------------------------------------------------------------}

instance ( CanHardFork (LegacyCardanoEras c)
         ) => ApplyBlock (LedgerState (LegacyCardanoBlock c)) (LegacyCardanoBlock c) where

  applyBlockLedgerResult cfg
                    (LegacyBlock (HardForkBlock (OneEraBlock block)))
                    (TickedLegacyLedgerState (TickedHardForkLedgerState transition st)) =
      case State.match block st of
        Left _mismatch ->
          -- Block from the wrong era (note that 'applyChainTick' will already
          -- have initiated the transition to the next era if appropriate).
            throwError ()
        Right matched ->
            fmap (fmap (LegacyLedgerState . HardForkLedgerState) . sequenceHardForkState)
          $ hsequence'
          $ hcizipWith proxySingle apply' cfgs matched
    where
      cfgs = distribLedgerConfig ei cfg
      ei   = State.epochInfoPrecomputedTransitionInfo
               (hardForkLedgerConfigShape cfg)
               transition
               st

      apply' ::
           ( SingleEraBlock blk
           , xs ~ LegacyCardanoEras c
           )
        => Index xs blk
        -> WrapLedgerConfig blk
        -> Product I (FlipTickedLedgerState mk) blk
        -> (    Except ()
            :.: LedgerResult (LedgerState (LegacyBlock (HardForkBlock xs)))
            :.: Flip LedgerState EmptyMK
          ) blk
      apply' index cfg1 (Pair x y) = Comp
                                  . fmap (Comp . fmap (Flip . flip withLedgerTables emptyLedgerTables . unFlip) . unComp) . unComp
                                  $ apply index cfg1 (Pair x (FlipTickedLedgerState . flip withLedgerTablesTicked emptyLedgerTables . getFlipTickedLedgerState $ y))

  reapplyBlockLedgerResult cfg
                      (LegacyBlock (HardForkBlock (OneEraBlock block)))
                      (TickedLegacyLedgerState (TickedHardForkLedgerState transition st)) =
      case State.match block st of
        Left _mismatch ->
          -- We already applied this block to this ledger state,
          -- so it can't be from the wrong era
          error "reapplyBlockLedgerResult: can't be from other era"
        Right matched ->
            fmap (LegacyLedgerState . HardForkLedgerState)
          $ sequenceHardForkState
          $ hcizipWith proxySingle (\index cfg1 (Pair x y)
                                  -> Comp . fmap (Flip . flip withLedgerTables emptyLedgerTables . unFlip) . unComp
                                  $ reapply index cfg1 (Pair x (FlipTickedLedgerState . flip withLedgerTablesTicked emptyLedgerTables . getFlipTickedLedgerState $ y))) cfgs matched
    where
      cfgs = distribLedgerConfig ei cfg
      ei   = State.epochInfoPrecomputedTransitionInfo
               (hardForkLedgerConfigShape cfg)
               transition
               st

  getBlockKeySets = const NoLegacyLedgerTables

apply :: (SingleEraBlock blk, xs ~ LegacyCardanoEras c)
      => Index xs                                           blk
      -> WrapLedgerConfig                                   blk
      -> Product I (FlipTickedLedgerState ValuesMK)         blk
      -> (    Except ()
          :.: LedgerResult (LedgerState (LegacyBlock (HardForkBlock xs)))
          :.: Flip LedgerState DiffMK
         )                                                  blk
apply index (WrapLedgerConfig cfg) (Pair (I block) (FlipTickedLedgerState st)) =
      Comp
    $ withExcept (injectLedgerError index)
    $ fmap (Comp . fmap Flip . embedLedgerResult (injectLedgerEvent index))
    $ applyBlockLedgerResult cfg block st

reapply :: (SingleEraBlock blk, xs ~ LegacyCardanoEras c)
        => Index xs                                           blk
        -> WrapLedgerConfig                                   blk
        -> Product I (FlipTickedLedgerState ValuesMK)         blk
        -> (    LedgerResult (LedgerState (LegacyBlock (HardForkBlock xs)))
            :.: Flip LedgerState DiffMK
           )                                                  blk
reapply index (WrapLedgerConfig cfg) (Pair (I block) (FlipTickedLedgerState st)) =
      Comp
    $ fmap Flip
    $ embedLedgerResult (injectLedgerEvent index)
    $ reapplyBlockLedgerResult cfg block st

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

_ledgerInfo :: forall blk mk. SingleEraBlock blk
           => Current (FlipTickedLedgerState mk) blk -> LedgerEraInfo blk
_ledgerInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

_ledgerViewInfo :: forall blk f. SingleEraBlock blk
               => (Ticked :.: f) blk -> LedgerEraInfo blk
_ledgerViewInfo _ = LedgerEraInfo $ singleEraInfo (Proxy @blk)

injectLedgerError :: Index xs blk -> LedgerError blk -> ()
injectLedgerError _index = const ()

injectLedgerEvent :: Index xs blk -> AuxLedgerEvent (LedgerState blk) -> ()
injectLedgerEvent _index = const ()
