{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

-- | Infrastructure for doing chain selection across eras
module Ouroboros.Consensus.HardFork.Combinator.Protocol.ChainSel (
    AcrossEraSelection (..)
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
  -- We use the 'ChainOrderConfig' of the 'SelectView' in the newer era when
  -- invoking 'compareChains'. (We could also make this choice configurable
  -- here.)
  CompareSameSelectView ::
       SelectView (BlockProtocol x) ~ SelectView (BlockProtocol y)
    => AcrossEraSelection x y

{-------------------------------------------------------------------------------
  Compare two eras
-------------------------------------------------------------------------------}

acrossEras ::
     forall blk blk'. SingleEraBlock blk
  => ChainOrderConfig (SelectView (BlockProtocol blk'))
  -> WithBlockNo WrapSelectView blk
  -> WithBlockNo WrapSelectView blk'
  -> AcrossEraSelection blk blk'
  -> Ordering
acrossEras cfg (WithBlockNo bnoL (WrapSelectView l))
               (WithBlockNo bnoR (WrapSelectView r)) = \case
    CompareBlockNo        -> compare bnoL bnoR
    CompareSameSelectView -> compareChains cfg l r

acrossEraSelection ::
     All SingleEraBlock              xs
  => NP WrapChainOrderConfig         xs
  -> Tails AcrossEraSelection        xs
  -> WithBlockNo (NS WrapSelectView) xs
  -> WithBlockNo (NS WrapSelectView) xs
  -> Ordering
acrossEraSelection = \cfg ffs l r ->
    goLeft cfg ffs (distribBlockNo l, distribBlockNo r)
  where
    goLeft ::
         All SingleEraBlock                xs
      => NP WrapChainOrderConfig           xs
      -> Tails AcrossEraSelection          xs
      -> ( NS (WithBlockNo WrapSelectView) xs
         , NS (WithBlockNo WrapSelectView) xs
         )
      -> Ordering
    goLeft _             TNil            = \(a, _) -> case a of {}
    goLeft (cfg :* cfgs) (TCons fs ffs') = \case
        (Z a, Z b) -> compareChains cfg' (dropBlockNo a) (dropBlockNo b)
        (Z a, S b) ->          goRight a cfgs fs b
        (S a, Z b) -> invert $ goRight b cfgs fs a
        (S a, S b) -> goLeft cfgs ffs' (a, b)
      where
        cfg' = unwrapChainOrderConfig cfg

    goRight ::
         forall x xs. (SingleEraBlock x, All SingleEraBlock xs)
      => WithBlockNo WrapSelectView x
      -> NP WrapChainOrderConfig         xs
      -> NP (AcrossEraSelection     x)   xs
      -> NS (WithBlockNo WrapSelectView) xs
      -> Ordering
    goRight a = go
      where
        go :: forall xs'. All SingleEraBlock  xs'
           => NP WrapChainOrderConfig         xs'
           -> NP (AcrossEraSelection x)       xs'
           -> NS (WithBlockNo WrapSelectView) xs'
           -> Ordering
        go _             Nil          b  = case b of {}
        go (cfg :* _   ) (f :* _)  (Z b) = acrossEras cfg' a b f
          where
            cfg' = unwrapChainOrderConfig cfg
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

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

invert :: Ordering -> Ordering
invert LT = GT
invert GT = LT
invert EQ = EQ
