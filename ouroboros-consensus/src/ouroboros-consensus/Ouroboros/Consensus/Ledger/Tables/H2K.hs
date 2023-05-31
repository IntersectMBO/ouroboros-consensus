
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- | Higher-order, double keyed functor over bifunctors.
module Ouroboros.Consensus.Ledger.Tables.H2K (
    -- * Class
    H2KAp (..)
  , H2KCollapse (..)
  , H2KPure (..)
  , H2KSequence (..)
  , H2KTraverse_ (..)
  , Key1
  , Key2
    -- * Derived functions
  , h2kliftA
  , h2kliftA2
  , h2kliftA3
  , h2kliftA4
    -- * Lifted functions
  , fn2_1
  , fn2_2
  , fn2_3
  , fn2_4
  , type (-..->) (..)
    -- * Basic bifunctors
  , K2 (..)
  , type (:..:) (..)
  ) where

import           Data.Bifunctor (Bifunctor (..))
import           Data.Kind (Constraint, Type)

{-------------------------------------------------------------------------------
  Class
-------------------------------------------------------------------------------}

type Key1 :: ((k1 -> k2 -> Type) -> Type) -> k1
type family Key1 h

type Key2 :: ((k1 -> k2 -> Type) -> Type) -> k2
type family Key2 h

-- | Generalised 'pure'.
--
-- Similar to 'HPure' from "Data.SOP".
type H2KPure :: ((k1 -> k2 -> Type) -> Type) -> Constraint
class H2KPure h where
  h2kpure :: f (Key1 h) (Key2 h) -> h f

-- | Generalised '(<*>)'.
--
-- Similar to 'HAp' from "Data.SOP".
type H2KAp :: ((k1 -> k2 -> Type)  -> Type) -> Constraint
class H2KPure h => H2KAp h where
  h2kap :: h (f -..-> g) -> h f -> h g

-- | Similar to 'HCollapse' from "Data.SOP".
type H2KCollapse :: ((k1 -> k2 -> Type)  -> Type) -> Constraint
class H2KCollapse h where
  h2kcollapse :: h (K2 a) -> a

-- | Similar to 'HTraverse_' from "Data.SOP".
type H2KTraverse_ :: ((k1 -> k2 -> Type)  -> Type) -> Constraint
class H2KTraverse_ h where
  h2ktraverse_ :: Applicative g => (f (Key1 h) (Key2 h) -> g ()) -> h f -> g ()

-- | Similar to 'HSequence' from "Data.SOP".
type H2KSequence :: ((k1 -> k2 -> Type)  -> Type) -> Constraint
class H2KAp h => H2KSequence h where
  h2ksequence' :: Applicative f => h (f :..: g) -> f (h g)
  h2ktraverse' ::
       Applicative g
    => (f (Key1 h) (Key2 h) -> g (f' (Key1 h) (Key2 h))) -> h f -> g (h f')

{-------------------------------------------------------------------------------
  Derived functions
-------------------------------------------------------------------------------}

h2kliftA ::
     H2KAp h
  => (f (Key1 h) (Key2 h) -> f' (Key1 h) (Key2 h))
  -> h f
  -> h f'
h2kliftA f x = h2kpure (fn2_1 f) `h2kap` x

h2kliftA2 ::
     H2KAp h
  => (f (Key1 h) (Key2 h) -> f' (Key1 h) (Key2 h) -> f'' (Key1 h) (Key2 h))
  -> h f
  -> h f'
  -> h f''
h2kliftA2 f x x' = h2kpure (fn2_2 f) `h2kap` x `h2kap` x'

h2kliftA3 ::
     H2KAp h
  => (  f (Key1 h) (Key2 h)
     -> f' (Key1 h) (Key2 h)
     -> f'' (Key1 h) (Key2 h)
     -> f''' (Key1 h) (Key2 h)
     )
  -> h f
  -> h f'
  -> h f''
  -> h f'''
h2kliftA3 f x x' x'' = h2kpure (fn2_3 f) `h2kap` x `h2kap` x' `h2kap` x''

h2kliftA4 ::
     H2KAp h
  => (  f     (Key1 h) (Key2 h)
     -> f'    (Key1 h) (Key2 h)
     -> f''   (Key1 h) (Key2 h)
     -> f'''  (Key1 h) (Key2 h)
     -> f'''' (Key1 h) (Key2 h)
     )
  -> h f
  -> h f'
  -> h f''
  -> h f'''
  -> h f''''
h2kliftA4 f x x' x'' x''' =
    h2kpure (fn2_4 f) `h2kap` x `h2kap` x' `h2kap` x'' `h2kap` x'''

{-------------------------------------------------------------------------------
  Lifted functions
-------------------------------------------------------------------------------}

-- | Lifted functions
type (-..->) :: (k1 -> k2 -> Type) -> (k1 -> k2 -> Type) -> k1 -> k2 -> Type
newtype (f -..-> g) a b = Fn2 { apFn2 :: f a b -> g a b }

infixr 1 -..->

-- | Construct a lifted function.
fn2_1 :: (f a b -> g a b) -> (f -..-> g) a b
fn2_1 = Fn2

-- | Construct a binary lifted function
fn2_2 :: (f a b -> f' a b -> f'' a b ) -> (f -..-> f' -..-> f'') a b
fn2_2 f = Fn2 $ \x -> Fn2 $ \x' -> f x x'

-- | Construct a ternary lifted function.
fn2_3 ::
     (f a b -> f' a b -> f'' a b -> f''' a b)
  -> (f -..-> f' -..-> f'' -..-> f''') a b
fn2_3 f = Fn2 $ \x -> Fn2 $ \x' -> Fn2 $ \x'' -> f x x' x''

-- | Construct a quaternary lifted function.
fn2_4 ::
     (f a b -> f' a b -> f'' a b -> f''' a b -> f'''' a b)
  -> (f -..-> f' -..-> f'' -..-> f''' -..-> f'''') a b
fn2_4 f = Fn2 $ \x -> Fn2 $ \x' -> Fn2 $ \x'' -> Fn2 $ \x''' -> f x x' x'' x'''

{-------------------------------------------------------------------------------
  Basic bifunctors
-------------------------------------------------------------------------------}

-- | The constant type bifunctor.
type K2 :: Type -> k1 -> k2 -> Type
newtype K2 a b c = K2 a
  deriving stock (Show, Eq)
  deriving stock (Functor, Foldable, Traversable)
  deriving newtype (Monoid, Semigroup)

instance Bifunctor (K2 a) where
  bimap _ _ (K2 x) = K2 x

-- | Composition of functor after bifunctor.
--
-- Example: @Comp2 (Just (17, True)) :: (Maybe :..: (,)) Int Bool@
type (:..:) :: (k3 -> Type) -> (k1 -> k2 -> k3) -> k1 -> k2 -> Type
newtype (:..:) f g a b = Comp2 { unComp2 :: f (g a b) }
  deriving stock (Show, Eq)
  deriving stock (Functor, Foldable)
  deriving newtype (Monoid, Semigroup)

infixr 7 :..:

deriving stock instance (Traversable f, Traversable (g a))
                     => Traversable ((f :..: g) a)

instance (Functor f, Bifunctor g) => Bifunctor (f :..: g) where
  bimap f g (Comp2 x) = Comp2 $ fmap (bimap f g) x
