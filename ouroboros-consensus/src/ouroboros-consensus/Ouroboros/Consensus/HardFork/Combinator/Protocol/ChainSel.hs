{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Infrastructure for doing chain selection across eras
module Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel
  ( AcrossEraMode (..)
  , AcrossEraTiebreaker (..)
  , acrossEraSelection
  , OneEraReasonForSwitch (..)
  ) where

import Data.Kind (Type)
import Data.SOP.BasicFunctors (K (..))
import Data.SOP.Constraint
import Data.SOP.Strict
import Data.SOP.Tails (Tails (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.TypeFamilyWrappers
import Ouroboros.Consensus.Util ((.:))
import Ouroboros.Consensus.Util.AnchoredFragment (ShouldSwitch')

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

-- | How to compare chains of equal length across eras.
data AcrossEraTiebreaker :: Type -> Type -> Type where
  -- | No preference.
  NoTiebreakerAcrossEras :: AcrossEraTiebreaker x y
  -- | Two eras using the same 'TiebreakerView', so use the corresponding
  -- (identical) tiebreaker.
  --
  -- We use the 'ChainOrderConfig' of the 'TiebreakerView' in the newer era
  -- (with the intuition that newer eras are generally "preferred") when
  -- invoking 'compareChains'. However, this choice is arbitrary; we could also
  -- make it configurable here.
  SameTiebreakerAcrossEras ::
    TiebreakerView (BlockProtocol x) ~ TiebreakerView (BlockProtocol y) =>
    AcrossEraTiebreaker x y

newtype OneEraReasonForSwitch xs = OneEraReasonForSwitch {getOneEraReasonForSwitch :: NS WrapReasonForSwitch xs}

shiftOneEraReasonForSwitch :: OneEraReasonForSwitch xs -> OneEraReasonForSwitch (x ': xs)
shiftOneEraReasonForSwitch (OneEraReasonForSwitch reason) = OneEraReasonForSwitch (S reason)

{-------------------------------------------------------------------------------
  Compare two eras
-------------------------------------------------------------------------------}

-- | GADT indicating whether we are lifting 'compare' or 'preferCandidate' to
-- the HFC, together with the type of configuration we need for that and the
-- result type.
data AcrossEraMode cfg r w a where
  AcrossEraCompare :: AcrossEraMode Proxy r w Ordering
  AcrossEraPreferCandidate ::
    AcrossEraMode WrapChainOrderConfig r w (ShouldSwitch (w r))

applyAcrossEraMode ::
  forall cfg blk a tv.
  ChainOrder tv =>
  cfg blk ->
  (WrapChainOrderConfig blk -> ChainOrderConfig tv) ->
  (ReasonForSwitch tv -> WrapReasonForSwitch blk) ->
  AcrossEraMode cfg blk WrapReasonForSwitch a ->
  tv ->
  tv ->
  a
applyAcrossEraMode cfg f g = \case
  AcrossEraCompare -> compare
  AcrossEraPreferCandidate -> \x y -> case preferCandidate (f cfg) x y of
    ShouldSwitch reason -> ShouldSwitch (g reason)
    ShouldNotSwitch -> ShouldNotSwitch

data FlipArgs = KeepArgs | FlipArgs

acrossEras ::
  forall blk blk' cfg a.
  SingleEraBlock blk =>
  FlipArgs ->
  AcrossEraMode cfg blk' WrapReasonForSwitch a ->
  -- | The configuration corresponding to the later block/era, also see
  -- 'SameTiebreakerAcrossEras'.
  cfg blk' ->
  WrapTiebreakerView blk ->
  WrapTiebreakerView blk' ->
  AcrossEraTiebreaker blk blk' ->
  a
acrossEras
  flipArgs
  mode
  cfg
  (WrapTiebreakerView l)
  (WrapTiebreakerView r) = \case
    NoTiebreakerAcrossEras -> maybeFlip cmp NoTiebreaker NoTiebreaker
     where
      cmp =
        applyAcrossEraMode
          cfg
          (const ())
          undefined
          ( case mode of
              AcrossEraCompare -> AcrossEraCompare
              AcrossEraPreferCandidate -> AcrossEraPreferCandidate
          )
    SameTiebreakerAcrossEras -> maybeFlip cmp l r
     where
      cmp = applyAcrossEraMode cfg unwrapChainOrderConfig WrapReasonForSwitch mode
   where
    maybeFlip :: (b -> b -> c) -> b -> b -> c
    maybeFlip = case flipArgs of
      KeepArgs -> id
      FlipArgs -> flip

acrossEraSelection ::
  forall xs cfg tv a.
  All SingleEraBlock xs =>
  AcrossEraMode cfg blk WrapReasonForSwitch a ->
  NP cfg xs ->
  Tails AcrossEraTiebreaker xs ->
  NS WrapTiebreakerView xs ->
  NS WrapTiebreakerView xs ->
  a
acrossEraSelection mode = \cfg ffs l r ->
  goBoth cfg ffs (l, r)
 where
  goBoth ::
    All SingleEraBlock xs' =>
    NP cfg xs' ->
    Tails AcrossEraTiebreaker xs' ->
    ( NS WrapTiebreakerView xs'
    , NS WrapTiebreakerView xs'
    ) ->
    a
  goBoth _ TNil = \(a, _) -> case a of {}
  goBoth (cfg :* cfgs) (TCons fs ffs') = \case
    (Z a, Z b) -> cmp a b
     where
      cmp = applyAcrossEraMode cfg unwrapChainOrderConfig undefined mode
    (Z a, S b) -> goOne KeepArgs a cfgs fs b
    (S a, Z b) -> goOne FlipArgs b cfgs fs a
    (S a, S b) -> goBoth cfgs ffs' (a, b)

  goOne ::
    forall x xs'.
    (SingleEraBlock x, All SingleEraBlock xs') =>
    FlipArgs ->
    WrapTiebreakerView x ->
    NP cfg xs' ->
    NP (AcrossEraTiebreaker x) xs' ->
    NS WrapTiebreakerView xs' ->
    a
  goOne flipArgs a = go
   where
    go ::
      forall xs''.
      All SingleEraBlock xs'' =>
      NP cfg xs'' ->
      NP (AcrossEraTiebreaker x) xs'' ->
      NS WrapTiebreakerView xs'' ->
      a
    go _ Nil b = case b of {}
    go (cfg :* _) (f :* _) (Z b) = acrossEras flipArgs mode cfg a b f
    go (_ :* cfgs) (_ :* fs) (S b) = go cfgs fs b
