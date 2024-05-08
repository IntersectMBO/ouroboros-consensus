{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

-- | Infrastructure for doing chain selection across eras
module Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel (
    AcrossEraMode (..)
  , AcrossEraSelection (..)
  , WithBlockNo (..)
  , acrossEraSelection
  , mapWithBlockNo
  ) where

import           Data.Kind (Type)
import           Data.SOP.Constraint
import           Data.SOP.Strict
import           Data.SOP.Tails (Tails (..))
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HardFork.Combinator.Abstract.SingleEraBlock
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.TypeFamilyWrappers

{-------------------------------------------------------------------------------
  Configuration
-------------------------------------------------------------------------------}

data AcrossEraSelection :: Type -> Type -> Type where
  -- | Just compare block numbers
  --
  -- This is a useful default when two eras run totally different consensus
  -- protocols, and we just want to choose the longer chain.
  CompareBlockNo :: AcrossEraSelection x y

  -- | Two eras using the same 'SelectView'. In this case, we can just compare
  -- chains even across eras, as the chain ordering is fully captured by
  -- 'SelectView' and its 'ChainOrder' instance.
  --
  -- We use the 'ChainOrderConfig' of the 'SelectView' in the newer era (with
  -- the intuition that newer eras are generally "preferred") when invoking
  -- 'compareChains'. However, this choice is arbitrary; we could also make it
  -- configurable here.
  CompareSameSelectView ::
       SelectView (BlockProtocol x) ~ SelectView (BlockProtocol y)
    => AcrossEraSelection x y

{-------------------------------------------------------------------------------
  Compare two eras
-------------------------------------------------------------------------------}


-- | GADT indicating whether we are lifting 'compare' or 'preferCandidate' to
-- the HFC, together with the type of configuration we need for that and the
-- result type.
data AcrossEraMode cfg a where
  AcrossEraCompare         :: AcrossEraMode Proxy                Ordering
  AcrossEraPreferCandidate :: AcrossEraMode WrapChainOrderConfig Bool

applyAcrossEraMode ::
     ChainOrder sv
  => cfg blk
  -> (WrapChainOrderConfig blk -> ChainOrderConfig sv)
  -> AcrossEraMode cfg a
  -> sv -> sv -> a
applyAcrossEraMode cfg f = \case
    AcrossEraCompare         -> compare
    AcrossEraPreferCandidate -> preferCandidate (f cfg)

data FlipArgs = KeepArgs | FlipArgs

acrossEras ::
     forall blk blk' cfg a. SingleEraBlock blk
  => FlipArgs
  -> AcrossEraMode cfg a
  -> cfg                        blk'
     -- ^ The configuration corresponding to the later block/era, also see
     -- 'CompareSameSelectView'.
  -> WithBlockNo WrapSelectView blk
  -> WithBlockNo WrapSelectView blk'
  -> AcrossEraSelection blk blk'
  -> a
acrossEras flipArgs mode cfg
  (WithBlockNo bnoL (WrapSelectView l))
  (WithBlockNo bnoR (WrapSelectView r)) = \case
    CompareBlockNo        -> maybeFlip cmp bnoL bnoR
      where
        cmp = applyAcrossEraMode cfg (const ()) mode
    CompareSameSelectView -> maybeFlip cmp l r
      where
        cmp = applyAcrossEraMode cfg (unwrapChainOrderConfig) mode
  where
    maybeFlip :: (b -> b -> a) -> b -> b -> a
    maybeFlip = case flipArgs of
      KeepArgs -> id
      FlipArgs -> flip

acrossEraSelection ::
     forall xs cfg a.
     All SingleEraBlock              xs
  => AcrossEraMode cfg a
  -> NP cfg                          xs
  -> Tails AcrossEraSelection        xs
  -> WithBlockNo (NS WrapSelectView) xs
  -> WithBlockNo (NS WrapSelectView) xs
  -> a
acrossEraSelection mode = \cfg ffs l r ->
    goBoth cfg ffs (distribBlockNo l, distribBlockNo r)
  where
    goBoth ::
         All SingleEraBlock                xs'
      => NP cfg                            xs'
      -> Tails AcrossEraSelection          xs'
      -> ( NS (WithBlockNo WrapSelectView) xs'
         , NS (WithBlockNo WrapSelectView) xs'
         )
      -> a
    goBoth _             TNil            = \(a, _) -> case a of {}
    goBoth (cfg :* cfgs) (TCons fs ffs') = \case
        (Z a, Z b) -> cmp (dropBlockNo a) (dropBlockNo b)
          where
            cmp = applyAcrossEraMode cfg unwrapChainOrderConfig mode
        (Z a, S b) -> goOne KeepArgs a cfgs fs b
        (S a, Z b) -> goOne FlipArgs b cfgs fs a
        (S a, S b) -> goBoth cfgs ffs' (a, b)

    goOne ::
         forall x xs'. (SingleEraBlock x, All SingleEraBlock xs')
      => FlipArgs
      -> WithBlockNo WrapSelectView x
      -> NP cfg                          xs'
      -> NP (AcrossEraSelection     x)   xs'
      -> NS (WithBlockNo WrapSelectView) xs'
      -> a
    goOne flipArgs a = go
      where
        go :: forall xs''. All SingleEraBlock xs''
           => NP cfg                          xs''
           -> NP (AcrossEraSelection x)       xs''
           -> NS (WithBlockNo WrapSelectView) xs''
           -> a
        go _             Nil          b  = case b of {}
        go (cfg :* _   ) (f :* _)  (Z b) = acrossEras flipArgs mode cfg a b f
        go (_   :* cfgs) (_ :* fs) (S b) = go cfgs fs b

{-------------------------------------------------------------------------------
  WithBlockNo
-------------------------------------------------------------------------------}

data WithBlockNo (f :: k -> Type) (a :: k) = WithBlockNo {
      getBlockNo  :: BlockNo
    , dropBlockNo :: f a
    }
  deriving (Show, Eq, Generic, NoThunks)

mapWithBlockNo :: (f x -> g y) -> WithBlockNo f x -> WithBlockNo g y
mapWithBlockNo f (WithBlockNo bno fx) = WithBlockNo bno (f fx)

distribBlockNo :: SListI xs => WithBlockNo (NS f) xs -> NS (WithBlockNo f) xs
distribBlockNo (WithBlockNo b ns) = hmap (WithBlockNo b) ns
