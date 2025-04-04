{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Intended for qualified import
--
-- > import           Data.SOP.Tails (Tails(..))
-- > import qualified Data.SOP.Tails as Tails
module Data.SOP.Tails (
    Tails (..)
    -- * Convenience constructors
  , mk1
  , mk2
  , mk3
    -- * SOP-like operators
  , extendWithTails
  , hcmap
  , hcpure
  , hmap
  , hpure
  , inPairsToTails
  ) where

import           Data.Kind (Type)
import           Data.Proxy
import           Data.SOP.Constraint
import           Data.SOP.Index
import qualified Data.SOP.InPairs as InPairs
import           Data.SOP.Sing
import           Data.SOP.Strict hiding (hcmap, hcpure, hmap, hpure)
import qualified Data.SOP.Strict as SOP

{-------------------------------------------------------------------------------
  Tails
-------------------------------------------------------------------------------}

-- | For every tail @(x ': xs)@ of the list, an @f x y@ for every @y@ in @xs@
type Tails :: (k -> k -> Type) -> [k] -> Type
data Tails f xs where
  TNil  :: Tails f '[]
  TCons :: NP (f x) xs -> Tails f xs -> Tails f (x ': xs)

{-------------------------------------------------------------------------------
  Convenience constructors
-------------------------------------------------------------------------------}

mk1 :: Tails f '[x]
mk1 = TCons Nil TNil

mk2 :: f x y -> Tails f '[x, y]
mk2 xy = TCons (xy :* Nil) mk1

mk3 :: f x y -> f x z -> f y z -> Tails f '[x, y, z]
mk3 xy xz yz = TCons (xy :* xz :* Nil) (mk2 yz)

{-------------------------------------------------------------------------------
  SOP-like operators
-------------------------------------------------------------------------------}

hmap :: SListI xs
     => (forall x y. f x y -> g x y)
     -> Tails f xs -> Tails g xs
hmap = hcmap (Proxy @Top)

hcmap :: forall proxy c f g xs. All c xs
      => proxy c
      -> (forall x y. (c x, c y) => f x y -> g x y)
      -> Tails f xs -> Tails g xs
hcmap p g = go
  where
    go :: All c xs' => Tails f xs' -> Tails g xs'
    go TNil           = TNil
    go (TCons fs fss) = TCons (SOP.hcmap p g fs) (go fss)

hpure :: SListI xs => (forall x y. f x y) -> Tails f xs
hpure = hcpure (Proxy @Top)

hcpure :: forall proxy f c xs. All c xs
       => proxy c
       -> (forall x y. (c x, c y) => f x y) -> Tails f xs
hcpure p f = go sList
  where
    go :: All c xs' => SList xs' -> Tails f xs'
    go SNil  = TNil
    go SCons = TCons (SOP.hcpure p f) (go sList)

inPairsToTails ::
     forall f xs .
     All Top xs
  => InPairs.InPairs (InPairs.Fn2 f) xs
  -> Tails (InPairs.Fn2 f) xs
inPairsToTails = go
  where
    go ::
         forall xs'.
         All Top xs'
      => InPairs.InPairs (InPairs.Fn2 f) xs'
      -> Tails (InPairs.Fn2 f) xs'
    go InPairs.PNil = mk1
    go (InPairs.PCons (InPairs.Fn2 f) n) =
      case go n of
        n'@(TCons np _) ->
          TCons
            (   InPairs.Fn2 f
             :* SOP.hmap (\(InPairs.Fn2 g) ->
                            InPairs.Fn2 (g . f)) np
            ) n'

extendWithTails ::
     SListI xs 
  => Index xs x
  -> Index xs y
  -> Tails (InPairs.Fn2 f) xs
  -> f x
  -> Maybe (f y)
extendWithTails IZ        IZ        _           = Just . id
extendWithTails IZ        (IS idx)  (TCons t _) = Just . InPairs.apFn2 (projectNP idx t)
extendWithTails (IS idx1) (IS idx2) (TCons _ n) = extendWithTails idx1 idx2 n
extendWithTails IS{}      IZ        _           = const Nothing
