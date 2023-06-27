{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Ledger tables are barbie-types.
--
-- Ledger tables are barbie-types (see @barbies@ package), though unfortunately
-- we can not implement classes like 'FunctorB' for ledger tables because the
-- class expects a type that is indexed over a /(uni-)functor/. Ledger tables
-- are indexed over /bifunctors/ (mapkinds), so the kinds do not match. To cut
-- on boilerplate, we do not define variants of 'FunctorB' (and similar classes)
-- for types that are indexed over bifunctors. Instead, we define specialised
-- variants of class functions and utility functions. For example:
-- * 'ltmap' instead of 'bmap' or 'bmapC'
-- * 'lttraverse' instead of 'btraverse' or 'btraverseC'
-- * 'ltsequence' instead of 'bsequence'.
module Ouroboros.Consensus.Ledger.Tables.Combinators (
    -- * Common constraints
    LedgerTableConstraints
    -- * Functor
  , ltmap
    -- * Traversable
  , lttraverse
    -- ** Utility functions
  , ltsequence
    -- * Applicative
  , ltprod
  , ltpure
    -- ** Utility functions
  , ltap
  , ltliftA
  , ltliftA2
  , ltliftA3
  , ltliftA4
    -- * Applicative and Traversable
  , ltzipWith3A
    -- * Collapsing
  , ltcollapse
    -- * Lifted functions
  , fn2_1
  , fn2_2
  , fn2_3
  , fn2_4
  , type (-..->) (..)
    -- ** Re-exports of utils
  , (...:)
  , (..:)
  , (.:)
    -- * Basic bifunctors
  , K2 (..)
  , type (:..:) (..)
  ) where

import           Data.Bifunctor
import           Data.Kind
import           Data.SOP.Functors
import           Ouroboros.Consensus.Ledger.Tables.Common
import           Ouroboros.Consensus.Util ((...:), (..:), (.:))

{-------------------------------------------------------------------------------
  Common constraints
-------------------------------------------------------------------------------}

type LedgerTableConstraints l = (Ord (Key l), Eq (Value l))

{-------------------------------------------------------------------------------
  Functor
-------------------------------------------------------------------------------}

-- | Like 'bmap', but for ledger tables.
ltmap ::
     LedgerTableConstraints l
  => (forall k v. (Ord k, Eq v) => mk1 k v -> mk2 k v)
  -> LedgerTables l mk1
  -> LedgerTables l mk2
ltmap f (LedgerTables x) = LedgerTables $ f x

{-------------------------------------------------------------------------------
  Traversable
-------------------------------------------------------------------------------}

-- | Like 'btraverse', but for ledger tables.
lttraverse ::
     (Applicative f, LedgerTableConstraints l)
  => (forall k v. (Ord k, Eq v) => mk1 k v -> f (mk2 k v))
  -> LedgerTables l mk1
  -> f (LedgerTables l mk2)
lttraverse f (LedgerTables x) = LedgerTables <$> f x

--
-- Utility functions
--

ltsequence ::
     (Applicative f, LedgerTableConstraints l)
  => LedgerTables l (f :..: mk)
  -> f (LedgerTables l mk)
ltsequence = lttraverse unComp2

{-------------------------------------------------------------------------------
  Applicative
-------------------------------------------------------------------------------}

-- | Like 'bpure', but for ledger tables.
ltpure ::
       LedgerTableConstraints l
    => (forall k v. (Ord k, Eq v) => mk k v)
    -> LedgerTables l mk
ltpure = LedgerTables

-- | Like 'bprod', but for ledger tables.
ltprod :: LedgerTables l f -> LedgerTables l g -> LedgerTables l (f `Product2` g)
ltprod (LedgerTables x) (LedgerTables y) = LedgerTables (Pair2 x y)

--
-- Utility functions
--

ltap ::
     LedgerTableConstraints l
  => LedgerTables l (mk1 -..-> mk2)
  -> LedgerTables l mk1
  -> LedgerTables l mk2
ltap f x = ltmap g $ ltprod f x
  where g (Pair2 f' x') = apFn2 f' x'

ltliftA ::
     LedgerTableConstraints l
  => (forall k v. (Ord k, Eq v) => mk1 k v -> mk2 k v)
  -> LedgerTables l mk1
  -> LedgerTables l mk2
ltliftA f x = ltpure (fn2_1 f) `ltap` x

ltliftA2 ::
     LedgerTableConstraints l
  => (forall k v. (Ord k, Eq v) => mk1 k v -> mk2 k v -> mk3 k v)
  -> LedgerTables l mk1
  -> LedgerTables l mk2
  -> LedgerTables l mk3
ltliftA2 f x x' = ltpure (fn2_2 f) `ltap` x `ltap` x'

ltliftA3 ::
     LedgerTableConstraints l
  => (forall k v. (Ord k, Eq v) => mk1 k v -> mk2 k v -> mk3 k v -> mk4 k v)
  -> LedgerTables l mk1
  -> LedgerTables l mk2
  -> LedgerTables l mk3
  -> LedgerTables l mk4
ltliftA3 f x x' x'' = ltpure (fn2_3 f) `ltap` x `ltap` x' `ltap` x''

ltliftA4 ::
     LedgerTableConstraints l
  => (    forall k v. (Ord k, Eq v)
       => mk1 k v -> mk2 k v -> mk3 k v -> mk4 k v -> mk5 k v
     )
  -> LedgerTables l mk1
  -> LedgerTables l mk2
  -> LedgerTables l mk3
  -> LedgerTables l mk4
  -> LedgerTables l mk5
ltliftA4 f x x' x'' x''' =
  ltpure (fn2_4 f) `ltap` x `ltap` x' `ltap` x'' `ltap` x'''

{-------------------------------------------------------------------------------
  Applicative and Traversable
-------------------------------------------------------------------------------}

ltzipWith3A ::
     (Applicative f, LedgerTableConstraints l)
  => (forall k v. (Ord k, Eq v) => mk1 k v -> mk2 k v -> mk3 k v -> f (mk4 k v))
  -> LedgerTables l mk1
  -> LedgerTables l mk2
  -> LedgerTables l mk3
  -> f (LedgerTables l mk4)
ltzipWith3A f = ltsequence ..: ltliftA3 (Comp2 ..: f)

{-------------------------------------------------------------------------------
  Collapsing
-------------------------------------------------------------------------------}

ltcollapse :: LedgerTables l (K2 a) -> a
ltcollapse = unK2 . getLedgerTables

{-------------------------------------------------------------------------------
  Semigroup and Monoid
-------------------------------------------------------------------------------}

instance ( forall k v. (Ord k, Eq v) => Semigroup (mk k v)
         , LedgerTableConstraints l
         ) => Semigroup (LedgerTables l mk) where
  (<>) :: LedgerTables l mk -> LedgerTables l mk -> LedgerTables l mk
  (<>) = ltliftA2 (<>)

instance ( forall k v. (Ord k, Eq v) => Monoid (mk k v)
         , LedgerTableConstraints l
         ) => Monoid (LedgerTables l mk) where
  mempty :: LedgerTables l mk
  mempty = ltpure mempty

{-------------------------------------------------------------------------------
  Lifted functions
-------------------------------------------------------------------------------}

-- | Lifted functions
--
-- Similar to '(-.->)', but for @f@ and @g@ that are bifunctors.
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
