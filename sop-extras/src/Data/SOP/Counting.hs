{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Type-level counting
--
-- Intended for unqualified import.
module Data.SOP.Counting (
    AtMost (..)
  , Exactly (.., ExactlyNil, ExactlyCons)
    -- * Working with 'Exactly'
  , exactlyHead
  , exactlyOne
  , exactlyReplicate
  , exactlyTail
  , exactlyTwo
  , exactlyWeaken
  , exactlyWeakenNonEmpty
  , exactlyZip
  , exactlyZipFoldable
    -- * Working with 'AtMost'
  , atMostFromNonEmpty
  , atMostHead
  , atMostInit
  , atMostLast
  , atMostNonEmpty
  , atMostOne
  , atMostZipFoldable
  ) where

import qualified Data.Foldable as Foldable
import           Data.Kind
import           Data.SOP.BasicFunctors
import           Data.SOP.Constraint
import           Data.SOP.Dict (Dict (..), all_NP)
import           Data.SOP.NonEmpty
import           Data.SOP.Strict

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

newtype Exactly xs a = Exactly { getExactly :: NP (K a) xs }

-- | At most one value for each type level index
type AtMost :: [Type] -> Type -> Type
data AtMost xs a where
  AtMostNil  :: AtMost xs a
  AtMostCons :: !a -> !(AtMost xs a) -> AtMost (x ': xs) a

deriving instance Eq a => Eq (AtMost   xs a)

deriving instance Show a => Show (AtMost   xs a)

deriving instance Functor     (AtMost xs)
deriving instance Foldable    (AtMost xs)
deriving instance Traversable (AtMost xs)

{-------------------------------------------------------------------------------
  Pattern synonyms for 'Exactly'
-------------------------------------------------------------------------------}

{-# COMPLETE ExactlyNil, ExactlyCons #-}
pattern ExactlyCons :: () => xs' ~ (x ': xs) => a -> Exactly xs a -> Exactly xs' a
pattern ExactlyCons x xs <- (Exactly (K x :* (Exactly -> xs)))
  where
    ExactlyCons x xs = Exactly (K x :* getExactly xs)

pattern ExactlyNil :: () => xs ~ '[] => Exactly xs a
pattern ExactlyNil = Exactly Nil

{-------------------------------------------------------------------------------
  Type class instances for 'Exactly'

  For 'AtMost' and 'NonEmpty' we can just derive these, but for 'Exactly'
  we need to do a bit more work.
-------------------------------------------------------------------------------}

instance Functor (Exactly xs) where
  fmap f (Exactly xs) = npToSListI xs $ Exactly $
      hmap (mapKK f) xs

instance Foldable (Exactly xs) where
  foldMap f (Exactly xs) = npToSListI xs $
      foldMap f (hcollapse xs)

instance Traversable (Exactly xs) where
  traverse f (Exactly xs) = npToSListI xs $ fmap Exactly $
      hsequence' $ hmap (\(K x) -> Comp $ K <$> f x) xs

instance Show a => Show (Exactly xs a) where
  show (Exactly xs) = npToSListI xs $
      case dict of
        Dict -> show xs
    where
      dict :: SListI xs => Dict (All (Compose Show (K a))) xs
      dict = all_NP (hpure Dict)

instance Eq a => Eq (Exactly xs a) where
  Exactly xs == Exactly xs' = npToSListI xs $
      case dict of
        Dict -> xs == xs'
    where
      dict :: SListI xs => Dict (All (Compose Eq (K a))) xs
      dict = all_NP (hpure Dict)

{-------------------------------------------------------------------------------
  Working with 'Exactly'
-------------------------------------------------------------------------------}

-- | Singleton
exactlyOne :: a -> Exactly '[x] a
exactlyOne a = Exactly $ K a :* Nil

-- | From a pair
exactlyTwo :: a -> a -> Exactly '[x, y] a
exactlyTwo a1 a2 = Exactly $ K a1 :* K a2 :* Nil

-- | Analogue of 'head'
exactlyHead :: Exactly (x ': xs) a -> a
exactlyHead = unK . hd . getExactly

-- | Analogue of 'tail'
exactlyTail :: Exactly (x ': xs) a -> Exactly xs a
exactlyTail = Exactly . tl . getExactly

-- | Analogue of 'zip'
exactlyZip :: Exactly xs a -> Exactly xs b -> Exactly xs (a, b)
exactlyZip (Exactly np) (Exactly np') = Exactly $
    npToSListI np $
      hzipWith (\(K x) (K y) -> K (x, y)) np np'

-- | Analogue of 'zip' where the length of second argument is unknown
exactlyZipFoldable :: Foldable t => Exactly xs a -> t b -> AtMost xs (a, b)
exactlyZipFoldable = \(Exactly as) bs -> go as (Foldable.toList bs)
  where
    go :: NP (K a) xs -> [b] -> AtMost xs (a, b)
    go _           []     = AtMostNil
    go Nil         _      = AtMostNil
    go (K a :* as) (b:bs) = AtMostCons (a, b) $ go as bs

exactlyWeaken :: Exactly xs a -> AtMost xs a
exactlyWeaken = go . getExactly
  where
    go :: NP (K a) xs -> AtMost xs a
    go Nil         = AtMostNil
    go (K x :* xs) = AtMostCons x (go xs)

exactlyWeakenNonEmpty :: Exactly (x ': xs) a -> NonEmpty (x ': xs) a
exactlyWeakenNonEmpty = go . getExactly
  where
    go :: NP (K a) (x ': xs) -> NonEmpty (x ': xs) a
    go (K x :* Nil)         = NonEmptyOne x
    go (K x :* xs@(_ :* _)) = NonEmptyCons x (go xs)

-- | Analogue of 'replicate'
--
-- In CPS style because the @xs@ type parameter is not statically known.
exactlyReplicate :: forall a r. Word -> a -> (forall xs. Exactly xs a -> r) -> r
exactlyReplicate = \n a k -> go n a (k . Exactly)
  where
    go :: Word -> a -> (forall xs. NP (K a) xs -> r) -> r
    go 0 _ k = k Nil
    go n a k = go (n - 1) a $ \xs -> k (K a :* xs)

{-------------------------------------------------------------------------------
  Working with 'AtMost'
-------------------------------------------------------------------------------}

-- | Singleton
atMostOne :: a -> AtMost (x ': xs) a
atMostOne x = AtMostCons x AtMostNil

-- | Analogue of 'init'
--
-- For simplicity we don't shrink the type-level index.
atMostInit :: AtMost xs a -> Maybe (AtMost xs a, a)
atMostInit = go
  where
    go :: AtMost xs a -> Maybe (AtMost xs a, a)
    go AtMostNil         = Nothing
    go (AtMostCons a as) = Just $
                             case go as of
                               Nothing        -> (AtMostNil, a)
                               Just (as', a') -> (AtMostCons a as', a')

-- | Analogue of 'head'
atMostHead :: AtMost xs a -> Maybe a
atMostHead AtMostNil        = Nothing
atMostHead (AtMostCons x _) = Just x

-- | Analogue of 'last'
atMostLast :: AtMost xs a -> Maybe a
atMostLast = fmap snd . atMostInit

atMostZipFoldable :: Foldable t => AtMost xs a -> t b -> AtMost xs (a, b)
atMostZipFoldable = \as bs -> go as (Foldable.toList bs)
  where
    go :: AtMost xs a -> [b] -> AtMost xs (a, b)
    go AtMostNil         _      = AtMostNil
    go _                 []     = AtMostNil
    go (AtMostCons a as) (b:bs) = AtMostCons (a, b) (go as bs)

atMostNonEmpty :: AtMost (x ': xs) a -> Maybe (NonEmpty (x ': xs) a)
atMostNonEmpty = \case
    AtMostNil       -> Nothing
    AtMostCons x xs -> Just $ go x xs
  where
    go :: a -> AtMost xs a -> NonEmpty (x ': xs) a
    go x AtMostNil         = NonEmptyOne  x
    go x (AtMostCons y zs) = NonEmptyCons x (go y zs)

atMostFromNonEmpty :: NonEmpty xs a -> AtMost xs a
atMostFromNonEmpty = go
  where
    go :: NonEmpty xs a -> AtMost xs a
    go (NonEmptyOne  x)    = AtMostCons x AtMostNil
    go (NonEmptyCons x xs) = AtMostCons x (go xs)
