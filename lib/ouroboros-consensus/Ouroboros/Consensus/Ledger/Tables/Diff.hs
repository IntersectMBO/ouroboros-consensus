{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.Ledger.Tables.Diff
  ( -- * Types
    Delta (..)
  , Diff (..)

    -- * Conversion
  , keysSet

    -- * Construction
  , diff

    -- ** Maps
  , fromMap
  , fromMapDeletes
  , fromMapInserts

    -- ** Set
  , fromSetDeletes

    -- ** Lists
  , fromList
  , fromListDeletes
  , fromListInserts

    -- * Query

    -- ** Size
  , null
  , numDeletes
  , numInserts
  , size

    -- * Applying diffs
  , applyDiff
  , applyDiffForKeys

    -- * Filter
  , filterWithKeyOnly
  , foldMapDelta
  , traverseDeltaWithKey_
  ) where

import Control.Monad (void)
import Data.Bifunctor
import Data.Foldable (foldMap')
import qualified Data.Map.Merge.Strict as Merge
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import NoThunks.Class
import Prelude hiding (null)

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

newtype Diff k v = Diff (Map k (Delta v))
  deriving stock (Show, Eq)
  deriving Generic
  deriving newtype NoThunks

-- | Custom 'Functor' instance, since @'Functor' ('Map' k)@ is actually the
-- 'Functor' instance for a lazy Map.
instance Functor (Diff k) where
  fmap f (Diff m) = Diff $ Map.map (fmap f) m

instance Ord k => Semigroup (Diff k v) where
  (Diff m1) <> (Diff m2) = Diff $ Map.unionWith (<>) m1 m2

instance Ord k => Monoid (Diff k v) where
  mempty = Diff mempty

data Delta v
  = Insert !v
  | Delete
  deriving stock (Show, Eq, Functor)
  deriving Generic
  deriving NoThunks

-- | Right-biased
instance Semigroup (Delta v) where
  _d1 <> d2 = d2

{------------------------------------------------------------------------------
  Conversion
------------------------------------------------------------------------------}

keysSet :: Diff k v -> Set k
keysSet (Diff m) = Map.keysSet m

{------------------------------------------------------------------------------
  Construction
------------------------------------------------------------------------------}

diff :: (Ord k, Eq v) => Map k v -> Map k v -> Diff k v
diff m1 m2 =
  Diff $
    Merge.merge
      (Merge.mapMissing $ \_k _v -> Delete)
      (Merge.mapMissing $ \_k v -> Insert v)
      ( Merge.zipWithMaybeMatched $ \_k v1 v2 ->
          if v1 == v2
            then Nothing
            else Just (Insert v2)
      )
      m1
      m2

fromMap :: Map k (Delta v) -> Diff k v
fromMap = Diff

fromMapInserts :: Map k v -> Diff k v
fromMapInserts = Diff . Map.map Insert

fromMapDeletes :: Map k v -> Diff k v
fromMapDeletes = Diff . Map.map (const Delete)

fromSetDeletes :: Set k -> Diff k v
fromSetDeletes = Diff . Map.fromSet (const Delete)

fromList :: Ord k => [(k, Delta v)] -> Diff k v
fromList = Diff . Map.fromList

fromListInserts :: Ord k => [(k, v)] -> Diff k v
fromListInserts = Diff . Map.fromList . fmap (second Insert)

fromListDeletes :: Ord k => [(k, v)] -> Diff k v
fromListDeletes = Diff . Map.fromList . fmap (second (const Delete))

{------------------------------------------------------------------------------
  Query
------------------------------------------------------------------------------}

null :: Diff k v -> Bool
null (Diff m) = Map.null m

size :: Diff k v -> Int
size (Diff m) = Map.size m

numInserts :: Diff k v -> Int
numInserts (Diff m) = getSum $ foldMap' f m
 where
  f (Insert _) = 1
  f Delete = 0

numDeletes :: Diff k v -> Int
numDeletes (Diff m) = getSum $ foldMap' f m
 where
  f (Insert _) = 0
  f Delete = 1

{------------------------------------------------------------------------------
  Applying diffs
------------------------------------------------------------------------------}

applyDiff ::
  Ord k =>
  Map k v ->
  Diff k v ->
  Map k v
applyDiff m (Diff diffs) =
  Merge.merge
    Merge.preserveMissing
    (Merge.mapMaybeMissing newKeys)
    (Merge.zipWithMaybeMatched oldKeys)
    m
    diffs
 where
  newKeys :: k -> Delta v -> Maybe v
  newKeys _k (Insert x) = Just x
  newKeys _k Delete = Nothing

  oldKeys :: k -> v -> Delta v -> Maybe v
  oldKeys _k _v1 (Insert x) = Just x
  oldKeys _k _v1 Delete = Nothing

applyDiffForKeys ::
  Ord k =>
  Map k v ->
  Set k ->
  Diff k v ->
  Map k v
applyDiffForKeys m ks (Diff diffs) =
  applyDiff
    m
    (Diff $ diffs `Map.restrictKeys` (Map.keysSet m `Set.union` ks))

{-------------------------------------------------------------------------------
  Filter
-------------------------------------------------------------------------------}

filterWithKeyOnly :: (k -> Bool) -> Diff k v -> Diff k v
filterWithKeyOnly f (Diff m) = Diff $ Map.filterWithKey (const . f) m

{-------------------------------------------------------------------------------
  Traversals and folds
-------------------------------------------------------------------------------}

-- | Traversal with keys over the deltas.
traverseDeltaWithKey_ ::
  Applicative t =>
  (k -> Delta v -> t a) ->
  Diff k v ->
  t ()
traverseDeltaWithKey_ f (Diff m) = void $ Map.traverseWithKey f m

-- | @'foldMap'@ over the deltas.
foldMapDelta :: Monoid m => (Delta v -> m) -> Diff k v -> m
foldMapDelta f (Diff m) = foldMap f m
