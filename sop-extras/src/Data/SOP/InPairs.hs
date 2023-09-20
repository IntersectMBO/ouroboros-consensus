{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeOperators            #-}

-- | Intended for qualified import
--
-- > import           Data.SOP.InPairs (InPairs(..))
-- > import qualified Data.SOP.InPairs as InPairs
module Data.SOP.InPairs (
    -- * InPairs
    InPairs (..)
    -- * Convenience constructors
  , mk1
  , mk2
  , mk3
    -- * SOP-like operators
  , hcmap
  , hcpure
  , hmap
  , hpure
    -- * Functions
  , type (--.-->) (..)
  , (:**:) (..)
  , Le (..)
  , Ri (..)
  , Comp2 (..)
  , apNP
    -- * Requiring
  , Requiring (..)
  , RequiringBoth (..)
  , ignoring
  , ignoringBoth
  , requiring
  , requiringBoth
  ) where

import           Data.Kind (Type)
import           Data.Proxy
import           Data.SOP.Constraint
import           Data.SOP.NonEmpty
import           Data.SOP.Sing
import           Data.SOP.Strict hiding (hcmap, hcpure, hmap, hpure)

{-------------------------------------------------------------------------------
  InPairs
-------------------------------------------------------------------------------}

-- | We have an @f x y@ for each pair @(x, y)@ of successive list elements
type InPairs :: (k -> k -> Type) -> [k] -> Type
data InPairs f xs where
  PNil  :: InPairs f '[x]
  PCons :: f x y -> InPairs f (y ': zs) -> InPairs f (x ': y ': zs)

{-------------------------------------------------------------------------------
  Convenience constructors
-------------------------------------------------------------------------------}

mk1 :: InPairs f '[x]
mk1 = PNil

mk2 :: f x y -> InPairs f '[x, y]
mk2 xy = PCons xy mk1

mk3 :: f x y -> f y z -> InPairs f '[x, y, z]
mk3 xy yz = PCons xy (mk2 yz)

{-------------------------------------------------------------------------------
  SOP-like operators
-------------------------------------------------------------------------------}

hmap :: SListI xs => (forall x y. f x y -> g x y) -> InPairs f xs -> InPairs g xs
hmap = hcmap (Proxy @Top)

hcmap :: forall proxy c f g xs. All c xs
      => proxy c
      -> (forall x y. (c x, c y) => f x y -> g x y)
      -> InPairs f xs -> InPairs g xs
hcmap _ f = go
  where
    go :: All c xs' => InPairs f xs' -> InPairs g xs'
    go PNil         = PNil
    go (PCons x xs) = PCons (f x) (go xs)

hpure :: (SListI xs, IsNonEmpty xs) => (forall x y. f x y) -> InPairs f xs
hpure = hcpure (Proxy @Top)

hcpure :: forall proxy c xs f. (All c xs, IsNonEmpty xs)
       => proxy c
       -> (forall x y. (c x, c y) => f x y) -> InPairs f xs
hcpure _ f =
    case isNonEmpty (Proxy @xs) of
      ProofNonEmpty {} -> go sList
  where
    go :: (c x, All c xs') => SList xs' -> InPairs f (x ': xs')
    go SNil  = PNil
    go SCons = PCons f (go sList)

{-------------------------------------------------------------------------------
  RequiringBoth
-------------------------------------------------------------------------------}

infixr 0 --.-->

newtype Comp2 f g x y = Comp2 {unComp2 :: f (g x y)}

newtype (--.-->) f g x y = DoubleFun {apDoubleFun :: f x y -> g x y}

data (:**:) f g x y = DoublePair (f x y) (g x y)

newtype Le f x y = Le (f x)
newtype Ri f x y = Ri (f y)

newtype Requiring h f x y = Require {
      provide :: h x -> f x y
    }

newtype RequiringBoth h f x y = RequireBoth {
      provideBoth :: h x -> h y -> f x y
    }

ignoring :: f x y -> Requiring h f x y
ignoring fxy = Require $ const fxy

ignoringBoth :: f x y -> RequiringBoth h f x y
ignoringBoth fxy = RequireBoth $ \_ _ -> fxy

requiring :: SListI xs => NP h xs -> InPairs (Requiring h f) xs -> InPairs f xs
requiring np =
      requiringBoth np
    . hmap (\f -> RequireBoth $ \hx _hy -> provide f hx)

requiringBoth :: NP h xs -> InPairs (RequiringBoth h f) xs -> InPairs f xs
requiringBoth = flip go
  where
    go :: InPairs (RequiringBoth h f) xs -> NP h xs -> InPairs f xs
    go PNil         _              = PNil
    go (PCons f fs) (x :* y :* zs) = PCons (provideBoth f x y) (go fs (y :* zs))

apNP :: InPairs (Le f --.--> Ri f --.--> g) xs -> NP f xs -> InPairs g xs
apNP = go
  where
    go :: InPairs (Le f --.--> Ri f --.--> g) xs -> NP f xs -> InPairs g xs
    go PNil         _              = PNil
    go (PCons f fs) (x :* y :* zs) = PCons (f `apDoubleFun` Le x `apDoubleFun` Ri y) (go fs (y :* zs))
