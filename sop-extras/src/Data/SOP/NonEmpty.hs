{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Type-level non-empty lists
module Data.SOP.NonEmpty (
    NonEmpty (..)
    -- * Proofs
  , IsNonEmpty (..)
  , ProofNonEmpty (..)
  , checkIsNonEmpty
    -- * Working with 'NonEmpty'
  , nonEmptyFromList
  , nonEmptyHead
  , nonEmptyInit
  , nonEmptyLast
  , nonEmptyMapOne
  , nonEmptyMapTwo
  , nonEmptyStrictPrefixes
  , nonEmptyToList
  ) where

import           Control.Applicative
import qualified Data.Foldable as Foldable
import           Data.Kind (Type)
import           Data.Proxy
import           Data.SOP.Sing

{-------------------------------------------------------------------------------
  NonEmpty
-------------------------------------------------------------------------------}

-- | Non-empty variation on 'AtMost'
type NonEmpty :: [Type] -> Type -> Type
data NonEmpty xs a where
  NonEmptyOne  :: !a -> NonEmpty (x ': xs) a
  NonEmptyCons :: !a -> !(NonEmpty xs a) -> NonEmpty (x ': xs) a

deriving instance Eq a   => Eq          (NonEmpty xs a)
deriving instance Show a => Show        (NonEmpty xs a)
deriving instance           Functor     (NonEmpty xs)
deriving instance           Foldable    (NonEmpty xs)
deriving instance           Traversable (NonEmpty xs)

instance (IsNonEmpty xs, SListI xs) => Applicative (NonEmpty xs) where
  pure :: forall a. a -> NonEmpty xs a
  pure a = case isNonEmpty (Proxy @xs) of
             ProofNonEmpty{} -> go shape
    where
      go :: forall x xs'. Shape xs' -> NonEmpty (x : xs') a
      go ShapeNil        = NonEmptyOne a
      go (ShapeCons xs') = NonEmptyCons a (go xs')
  (<*>) = go
    where
      go :: NonEmpty xs' (a -> b) -> NonEmpty xs' a -> NonEmpty xs' b
      go (NonEmptyOne  f)    (NonEmptyOne  x)    = NonEmptyOne  (f x)
      go (NonEmptyCons f _)  (NonEmptyOne  x)    = NonEmptyOne  (f x)
      go (NonEmptyOne  f)    (NonEmptyCons x _)  = NonEmptyOne  (f x)
      go (NonEmptyCons f fs) (NonEmptyCons x xs) = NonEmptyCons (f x) (go fs xs)

{-------------------------------------------------------------------------------
  Working with 'NonEmpty'
-------------------------------------------------------------------------------}

-- | Analogue of 'head'
nonEmptyHead :: NonEmpty xs a -> a
nonEmptyHead (NonEmptyOne  x)   = x
nonEmptyHead (NonEmptyCons x _) = x

-- | Analogue of 'last'
nonEmptyLast :: NonEmpty xs a -> a
nonEmptyLast = snd . nonEmptyInit

-- | Analogue of 'init'
nonEmptyInit :: NonEmpty xs a -> (Maybe (NonEmpty xs a), a)
nonEmptyInit (NonEmptyOne  x)    = (Nothing, x)
nonEmptyInit (NonEmptyCons x xs) =
    case nonEmptyInit xs of
      (Nothing  , final) -> (Just (NonEmptyOne  x)     , final)
      (Just xs' , final) -> (Just (NonEmptyCons x xs') , final)

-- | Build a 'NonEmpty' from a list. Returns 'Nothing' when the list is empty
-- or when it's longer than @xs@.
nonEmptyFromList :: forall xs a. SListI xs => [a] -> Maybe (NonEmpty xs a)
nonEmptyFromList = go (sList @xs)
  where
    go :: forall xs'. SList xs' -> [a] -> Maybe (NonEmpty xs' a)
    go s ys = case (s, ys) of
        (SCons, [y])   -> Just $ NonEmptyOne y
        (SCons, y:ys') -> NonEmptyCons y <$> go sList ys'
        (SCons, [])    -> Nothing
        (SNil,  _)     -> Nothing

-- | Convert a 'NonEmpty' to a list.
nonEmptyToList :: forall xs a. NonEmpty xs a -> [a]
nonEmptyToList = go
  where
    go :: forall xs'. NonEmpty xs' a -> [a]
    go (NonEmptyOne  x)    = [x]
    go (NonEmptyCons x xs) = x : go xs

-- | A strict prefixes
--
-- >    nonEmptyStrictPrefixes (fromJust (nonEmptyFromList [1..4]))
-- > == [ NonEmptyOne  1
-- >    , NonEmptyCons 1 $ NonEmptyOne  2
-- >    , NonEmptyCons 1 $ NonEmptyCons 2 $ NonEmptyOne 3
-- >    ]
nonEmptyStrictPrefixes :: NonEmpty xs a -> [NonEmpty xs a]
nonEmptyStrictPrefixes = go
  where
    go :: NonEmpty xs a -> [NonEmpty xs a]
    go (NonEmptyOne  _)    = []
    go (NonEmptyCons x xs) = NonEmptyOne x : map (NonEmptyCons x) (go xs)

-- | Apply the specified function to exactly one element
nonEmptyMapOne :: forall m xs a. Alternative m
               => (a -> m a) -> NonEmpty xs a -> m (NonEmpty xs a)
nonEmptyMapOne f = go
  where
    go :: NonEmpty xs' a -> m (NonEmpty xs' a)
    go (NonEmptyOne  x)    = NonEmptyOne <$> f x
    go (NonEmptyCons x xs) = Foldable.asum [
          (`NonEmptyCons` xs) <$> f x
        , NonEmptyCons x <$> go xs
        ]

-- | Variation on 'nonEmptyMapOne' where we try to apply the function to
-- /pairs/ of elements
nonEmptyMapTwo :: forall m xs a. Alternative m
               => (a -> m a) -- Used when we reached the end of the list
               -> (a -> a -> m (a, a))
               -> NonEmpty xs a -> m (NonEmpty xs a)
nonEmptyMapTwo f g = go
  where
    go :: NonEmpty xs' a -> m (NonEmpty xs' a)
    go (NonEmptyOne x) =
        NonEmptyOne <$> f x
    go (NonEmptyCons x xs@(NonEmptyOne y)) = Foldable.asum [
          (\(x', y') -> NonEmptyCons x' (NonEmptyOne y')) <$> g x y
        , NonEmptyCons x <$> go xs
        ]
    go (NonEmptyCons x xs@(NonEmptyCons y zs)) = Foldable.asum [
          (\(x', y') -> NonEmptyCons x' (NonEmptyCons y' zs)) <$> g x y
        , NonEmptyCons x <$> go xs
        ]


{-------------------------------------------------------------------------------
  Proofs
-------------------------------------------------------------------------------}

type ProofNonEmpty :: [a] -> Type
data ProofNonEmpty xs where
  ProofNonEmpty :: Proxy x -> Proxy xs -> ProofNonEmpty (x ': xs)

class IsNonEmpty xs where
  isNonEmpty :: proxy xs -> ProofNonEmpty xs

instance IsNonEmpty (x ': xs) where
  isNonEmpty _ = ProofNonEmpty (Proxy @x) (Proxy @xs)

checkIsNonEmpty :: forall xs. SListI xs => Proxy xs -> Maybe (ProofNonEmpty xs)
checkIsNonEmpty _ = case sList @xs of
    SNil  -> Nothing
    SCons -> Just $ ProofNonEmpty Proxy Proxy
