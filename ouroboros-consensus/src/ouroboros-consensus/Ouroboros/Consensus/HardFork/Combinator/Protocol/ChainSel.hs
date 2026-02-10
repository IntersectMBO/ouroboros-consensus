{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Infrastructure for doing chain selection across eras
module Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel
  ( AcrossEraMode (..)
  , AcrossEraTiebreaker (..)
  , acrossEraSelection
  , SwitchOutput (..)
  , OneEraReasonForSwitch (..)
  , ConstOutput (..)
  ) where

import Data.Kind (Type)
import Data.SOP.Constraint
import Data.SOP.Strict
import Data.SOP.Tails (Tails (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.TypeFamilyWrappers

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

{-------------------------------------------------------------------------------
  Compare two eras
-------------------------------------------------------------------------------}

-- | GADT indicating whether we are lifting 'compare' or 'preferCandidate' to
-- the HFC, together with the type of configuration we need for that and the
-- result type. The result type indicates whether we expect something that
-- changes between eras or some constant output.
data AcrossEraMode cfg (f :: [Type] -> Type) where
  AcrossEraCompare :: AcrossEraMode Proxy (ConstOutput Ordering)
  AcrossEraPreferCandidate :: AcrossEraMode WrapChainOrderConfig SwitchOutput

newtype OneEraReasonForSwitch xs = OneEraReasonForSwitch {getOneEraReasonForSwitch :: NS WrapReasonForSwitch xs}

-- | Abstract the result of chain selection so we can handle both
-- invariant results (Ordering) and covariant results (ShouldSwitch).
class AcrossEraOutput (f :: [Type] -> Type) where
  type SingleEraOutput f (x :: Type) :: Type

  liftHead :: SingleEraOutput f x -> f (x ': xs)
  liftTail :: f xs -> f (x ': xs)

  -- | Lift a result from (x : zs) to (x : y : zs).
  -- Keeps index 0 (x) as is, shifts indices >= 1 (zs) by one.
  -- This is needed when comparing 'x' against a list and skipping the head of the list.
  liftSkip :: f (x ': zs) -> f (x ': y ': zs)

-- | 1. Ordering Instance (Invariant)
newtype ConstOutput a xs = ConstOutput {getConstOutput :: a}

instance AcrossEraOutput (ConstOutput a) where
  type SingleEraOutput (ConstOutput a) x = a
  liftHead = ConstOutput
  liftTail = ConstOutput . getConstOutput
  liftSkip = ConstOutput . getConstOutput

-- | 2. Switch Instance (Covariant)
newtype SwitchOutput xs = SwitchOutput
  { getSwitchOutput :: ShouldSwitch (OneEraReasonForSwitch xs)
  }

instance AcrossEraOutput SwitchOutput where
  -- The leaf result corresponds to one era's wrapped reason
  type SingleEraOutput SwitchOutput x = ShouldSwitch (WrapReasonForSwitch x)

  liftHead = \case
    ShouldNotSwitch o -> SwitchOutput (ShouldNotSwitch o)
    ShouldSwitch r -> SwitchOutput $ ShouldSwitch $ OneEraReasonForSwitch (Z r)

  liftTail (SwitchOutput s) = SwitchOutput $ case s of
    ShouldNotSwitch o -> ShouldNotSwitch o
    ShouldSwitch rs -> ShouldSwitch $ OneEraReasonForSwitch (S (getOneEraReasonForSwitch rs))

  liftSkip (SwitchOutput s) = SwitchOutput $ case s of
    ShouldNotSwitch o -> ShouldNotSwitch o
    ShouldSwitch (OneEraReasonForSwitch ns) ->
      ShouldSwitch $ OneEraReasonForSwitch $ case ns of
        Z r -> Z r -- The reason was 'x' (index 0), keep it at index 0
        S rs -> S (S rs) -- The reason was in 'zs' (index 1+), shift to index 2+

applyAcrossEraMode ::
  ChainOrder (WrapTiebreakerView blk) =>
  cfg blk ->
  (WrapChainOrderConfig blk -> ChainOrderConfig (WrapTiebreakerView blk)) ->
  AcrossEraMode cfg f ->
  WrapTiebreakerView blk ->
  WrapTiebreakerView blk ->
  SingleEraOutput f blk
applyAcrossEraMode cfg f = \case
  AcrossEraCompare -> compare
  AcrossEraPreferCandidate -> \ours cand ->
    case preferCandidate (f cfg) ours cand of
      ShouldNotSwitch o -> ShouldNotSwitch o
      ShouldSwitch r -> ShouldSwitch (WrapReasonForSwitch r)

data FlipArgs = KeepArgs | FlipArgs

acrossEras ::
  forall blk blk' cfg f.
  SingleEraBlock blk =>
  FlipArgs ->
  AcrossEraMode cfg f ->
  -- | The configuration corresponding to the later block/era, also see
  -- 'SameTiebreakerAcrossEras'.
  cfg blk' ->
  WrapTiebreakerView blk ->
  WrapTiebreakerView blk' ->
  AcrossEraTiebreaker blk blk' ->
  SingleEraOutput f blk'
acrossEras
  flipArgs
  mode
  cfg
  (WrapTiebreakerView l)
  (WrapTiebreakerView r) = \case
    NoTiebreakerAcrossEras ->
      -- We would compare NoTiebreak vs NoTiebreak but this will always result
      -- in EQ and ShouldNotSwitch
      case mode of
        AcrossEraCompare -> EQ
        AcrossEraPreferCandidate -> ShouldNotSwitch EQ
    SameTiebreakerAcrossEras -> maybeFlip cmp l' r'
     where
      -- We must re-wrap the inner tiebreakers into the type expected by 'cfg' (blk')
      -- The GADT equality ensures this is type-safe.
      l' = WrapTiebreakerView l :: WrapTiebreakerView blk'
      r' = WrapTiebreakerView r :: WrapTiebreakerView blk'

      cmp = applyAcrossEraMode cfg unwrapChainOrderConfig mode
   where
    maybeFlip :: (b -> b -> r) -> b -> b -> r
    maybeFlip = case flipArgs of
      KeepArgs -> id
      FlipArgs -> flip

acrossEraSelection ::
  forall xs cfg f.
  (All SingleEraBlock xs, AcrossEraOutput f) =>
  AcrossEraMode cfg f ->
  NP cfg xs ->
  Tails AcrossEraTiebreaker xs ->
  NS WrapTiebreakerView xs ->
  NS WrapTiebreakerView xs ->
  f xs
acrossEraSelection mode = \cfg ffs l r ->
  goBoth cfg ffs (l, r)
 where
  goBoth ::
    forall xs'.
    All SingleEraBlock xs' =>
    NP cfg xs' ->
    Tails AcrossEraTiebreaker xs' ->
    (NS WrapTiebreakerView xs', NS WrapTiebreakerView xs') ->
    f xs'
  goBoth _ TNil = \(a, _) -> case a of {}
  goBoth (cfg :* cfgs) (TCons fs ffs') = \case
    (Z a, Z b) ->
      liftHead $
        applyAcrossEraMode cfg unwrapChainOrderConfig mode a b
    (Z a, S b) -> goOne KeepArgs a cfgs fs b
    (S a, Z b) -> goOne FlipArgs b cfgs fs a
    (S a, S b) ->
      liftTail $ goBoth cfgs ffs' (a, b)

  goOne ::
    forall x xs'.
    (SingleEraBlock x, All SingleEraBlock xs') =>
    FlipArgs ->
    WrapTiebreakerView x ->
    NP cfg xs' ->
    NP (AcrossEraTiebreaker x) xs' ->
    NS WrapTiebreakerView xs' ->
    f (x ': xs')
  goOne flipArgs a = go
   where
    go ::
      forall xs''.
      All SingleEraBlock xs'' =>
      NP cfg xs'' ->
      NP (AcrossEraTiebreaker x) xs'' ->
      NS WrapTiebreakerView xs'' ->
      f (x ': xs'')
    go _ Nil b = case b of {}
    go (cfg :* _) (f :* _) (Z b) =
      liftTail $ liftHead $ acrossEras flipArgs mode cfg a b f
    go (_ :* cfgs) (_ :* fs) (S b) =
      liftSkip $ go cfgs fs b
