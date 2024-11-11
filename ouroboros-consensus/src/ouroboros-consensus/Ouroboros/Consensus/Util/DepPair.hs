{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Ouroboros.Consensus.Util.DepPair (
    -- * Dependent pairs
    DepPair
  , GenDepPair (GenDepPair, DepPair)
  , depPairFirst
    -- * Compare indices
  , SameDepIndex (..)
  , SameDepIndex2 (..)
    -- * Trivial dependency
  , TrivialDependency (..)
  , fromTrivialDependency
  , toTrivialDependency
    -- * Convenience re-exports
  , Proxy (..)
  , (:~:) (..)
  ) where

import           Data.Kind (Constraint, Type)
import           Data.Proxy
import           Data.SOP.BasicFunctors (I (..))
import           Data.Type.Equality ((:~:) (..))

{-------------------------------------------------------------------------------
  Dependent pairs
-------------------------------------------------------------------------------}

-- | Generalization of 'DepPair'
--
-- This adds an additional functor @g@ around the second value in the pair.
data GenDepPair g f where
  GenDepPair :: !(f a) -> !(g a) -> GenDepPair g f

-- | Dependent pair
--
-- A dependent pair is a pair of values where the type of the value depends
-- on the first value.
type DepPair = GenDepPair I

{-# COMPLETE DepPair #-}
pattern DepPair :: f a -> a -> DepPair f
pattern DepPair fa a = GenDepPair fa (I a)

depPairFirst :: (forall a. f a -> f' a) -> GenDepPair g f -> GenDepPair g f'
depPairFirst f (GenDepPair ix a) = GenDepPair (f ix) a

{-------------------------------------------------------------------------------
  Compare indices
-------------------------------------------------------------------------------}

type SameDepIndex :: (k -> Type) -> Constraint
class SameDepIndex f where
  sameDepIndex :: f a -> f b -> Maybe (a :~: b)

  default sameDepIndex :: TrivialDependency f => f a -> f b -> Maybe (a :~: b)
  sameDepIndex ix ix' = Just $ hasSingleIndex ix ix'

type SameDepIndex2 :: (k1 -> k2 -> Type) -> Constraint
class SameDepIndex2 f where
  sameDepIndex2 :: f x a -> f y b -> Maybe ('(x, a) :~: '(y, b))

{-------------------------------------------------------------------------------
  Trivial dependencies
-------------------------------------------------------------------------------}

-- | A dependency is trivial if it always maps to the same type @b@
type TrivialDependency :: (k -> Type) -> Constraint
class TrivialDependency f where
  type TrivialIndex f :: k
  hasSingleIndex :: f a -> f b -> a :~: b
  indexIsTrivial :: f (TrivialIndex f)

fromTrivialDependency :: TrivialDependency f => f a -> a -> TrivialIndex f
fromTrivialDependency ix =
    case hasSingleIndex indexIsTrivial ix of
      Refl -> id

toTrivialDependency :: TrivialDependency f => f a -> TrivialIndex f -> a
toTrivialDependency ix =
    case hasSingleIndex indexIsTrivial ix of
      Refl -> id
