{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

-- | Barbies but parametric over bifunctors
module Data.Bifunctor.Barbie (
    -- * Functor
    FunctorB2 (..)
    -- * Traversable
  , TraversableB2 (..)
    -- ** Custom utility functions
  , b2sequence
    -- * Applicative
  , ApplicativeB2 (..)
    -- ** Custom utility functions
  , b2ap
  , b2liftA
  , b2liftA2
  , b2liftA3
  , b2liftA4
    -- * Constraints and instance dictionaries
  , ConstraintsB2 (..)
    -- ** Utility functions
  , b2dicts
  , b2mapC
  , b2pureC
  , b2traverseC
    -- ** Custom utility functions
  , b2liftAC
  , b2liftAC2
  , b2liftAC3
  , b2liftAC4
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
import           Data.Bifunctor.Barbie.Constraints (Dict2 (..), requiringDict2)
import           Data.Kind (Constraint, Type)
import           Data.SOP.Functors (Product2 (..))

{-------------------------------------------------------------------------------
  Functor
-------------------------------------------------------------------------------}

-- | Like 'FunctorB', but for functors from bi-indexed-types to types.
type FunctorB2 :: ((k1 -> k2 -> Type) -> Type) -> Constraint
class FunctorB2 h where
  b2map :: (forall a b. f a b -> g a b) -> h f -> h g

{-------------------------------------------------------------------------------
  Traversable
-------------------------------------------------------------------------------}

-- | Like 'TraversableB', but for functors from bi-indexed-types to types.
type TraversableB2 :: ((k1 -> k2 -> Type) -> Type) -> Constraint
class FunctorB2 h => TraversableB2 h where
  b2traverse ::
       Applicative e
    => (forall a b. f a b -> e (g a b)) -> h f -> e (h g)

--
-- Utility functions
--

b2sequence :: (Applicative e, TraversableB2 h) => h (e :..: f) -> e (h f)
b2sequence = b2traverse unComp2

{-------------------------------------------------------------------------------
  Applicative
-------------------------------------------------------------------------------}

-- | Like 'ApplicativeB', but for functors from bi-indexed-types to types.
type ApplicativeB2 :: ((k1 -> k2 -> Type) -> Type) -> Constraint
class FunctorB2 h => ApplicativeB2 h where
  b2pure :: (forall a b. f a b) -> h f
  b2prod :: h f -> h g -> h (f `Product2` g)

--
-- Custom utility functions
--

b2ap :: ApplicativeB2 h => h (f -..-> g) -> h f -> h g
b2ap f x = b2map g $ b2prod f x
  where g (Pair2 f' x') = apFn2 f' x'

b2liftA ::
     ApplicativeB2 h
  => (forall a b. f a b -> f' a b)
  -> h f
  -> h f'
b2liftA f x = b2pure (fn2_1 f) `b2ap` x

b2liftA2 ::
     ApplicativeB2 h
  => (forall a b. f a b -> f' a b -> f'' a b)
  -> h f
  -> h f'
  -> h f''
b2liftA2 f x x' = b2pure (fn2_2 f) `b2ap` x `b2ap` x'

b2liftA3 ::
     ApplicativeB2 h
  => (forall a b. f a b -> f' a b -> f'' a b -> f''' a b)
  -> h f -> h f' -> h f'' -> h f'''
b2liftA3 f x x' x'' = b2pure (fn2_3 f) `b2ap` x `b2ap` x' `b2ap` x''

b2liftA4 ::
     ApplicativeB2 h
  => (forall a b. f a b -> f' a b -> f'' a b -> f''' a b -> f'''' a b)
  -> h f -> h f' -> h f'' -> h f''' -> h f''''
b2liftA4 f x x' x'' x''' =
    b2pure (fn2_4 f) `b2ap` x `b2ap` x' `b2ap` x'' `b2ap` x'''

{-------------------------------------------------------------------------------
  Constraints and instance dictionaries
-------------------------------------------------------------------------------}

-- | Like 'ConstraintsB', but for functors from bi-indexed-types to types.
type ConstraintsB2 :: ((k1 -> k2 -> Type) -> Type) -> Constraint
class FunctorB2 h => ConstraintsB2 h where
  type AllB2 (c :: k1 -> k2 -> Constraint) h  :: Constraint
  badd2Dicts :: forall c f. AllB2 c h => h f -> h (Dict2 c `Product2` f)

data Proxy2 a b = Proxy2

--
-- Utility functions
--

-- | Similar to 'badd2Dicts' but can produce the instance dictionaries "out of
--   the blue".
b2dicts ::
     forall c h. (ConstraintsB2 h, ApplicativeB2 h, AllB2 c h)
  => h (Dict2 c)
b2dicts
  = b2map (\(Pair2 c _) -> c) $ badd2Dicts $ b2pure Proxy2

-- | 'b2map' with constraints.
b2mapC ::
     forall c h f g. (AllB2 c h, ConstraintsB2 h)
  => (forall a b. c a b => f a b -> g a b)
  -> h f
  -> h g
b2mapC f bf = b2map go (badd2Dicts bf)
  where
    go :: forall a b. (Dict2 c `Product2` f) a b -> g a b
    go (d `Pair2` fa) = requiringDict2 (f fa) d

-- | 'b2traverse' with constraints.
b2traverseC ::
     forall c h f g e. (TraversableB2 h, ConstraintsB2 h, AllB2 c h, Applicative e)
  => (forall a b. c a b => f a b -> e (g a b))
  -> h f
  -> e (h g)
b2traverseC f b =
    b2traverse (\(Pair2 (Dict2 :: Dict2 c a b) x) -> f x) (badd2Dicts b)

-- | 'b2pure' with constraints.
b2pureC ::
     forall c f h. (AllB2 c h, ConstraintsB2 h, ApplicativeB2 h)
  => (forall a b. c a b => f a b)
  -> h f
b2pureC fa = b2map (requiringDict2 @c fa) b2dicts

--
-- Custom utility functions
--

-- | 'b2liftA' with constraints.
b2liftAC ::
     forall c h f f'. (ApplicativeB2 h, ConstraintsB2 h, AllB2 c h)
  => (forall a b. c a b => f a b -> f' a b)
  -> h f
  -> h f'
b2liftAC f x = b2pureC @c (fn2_1 f) `b2ap` x

-- | 'b2liftA2' with constraints.
b2liftAC2 ::
     forall c h f f' f''. (ApplicativeB2 h, ConstraintsB2 h, AllB2 c h)
  => (forall a b. c a b => f a b -> f' a b -> f'' a b)
  -> h f
  -> h f'
  -> h f''
b2liftAC2 f x x' = b2pureC @c (fn2_2 f) `b2ap` x `b2ap` x'

-- | 'b2liftA3' with constraints.
b2liftAC3 ::
     forall c h f f' f'' f'''. (ApplicativeB2 h, ConstraintsB2 h, AllB2 c h)
  => (forall a b. c a b => f a b -> f' a b -> f'' a b -> f''' a b)
  -> h f -> h f' -> h f'' -> h f'''
b2liftAC3 f x x' x'' = b2pureC @c (fn2_3 f) `b2ap` x `b2ap` x' `b2ap` x''

-- | 'b2liftA4' with constraints.
b2liftAC4 ::
     forall c h f f' f'' f''' f''''. (ApplicativeB2 h, ConstraintsB2 h, AllB2 c h)
  => (forall a b. c a b => f a b -> f' a b -> f'' a b -> f''' a b -> f'''' a b)
  -> h f -> h f' -> h f'' -> h f''' -> h f''''
b2liftAC4 f x x' x'' x''' =
    b2pureC @c (fn2_4 f) `b2ap` x `b2ap` x' `b2ap` x'' `b2ap` x'''

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
newtype K2 a b c = K2 { unK2 :: a }
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
