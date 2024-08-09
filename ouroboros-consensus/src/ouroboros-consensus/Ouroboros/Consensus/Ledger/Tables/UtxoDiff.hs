{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE ViewPatterns   #-}

module Ouroboros.Consensus.Ledger.Tables.UtxoDiff (
    -- * Types
    MaybeUtxoDiff (..)
  , UtxoDiff (..)
    -- * Conversion
  , keysSet
    -- * Construction
  , diff
  , empty
  , utxoProperty
  , utxoProperty2
    -- ** Maps
  , fromListDeletes
  , fromListInserts
  , fromMapDeletes
  , fromMapInserts
    -- * Query
    -- ** Size
  , null
  , numDeletes
  , numInserts
  , size
    -- * Applying diffs
  , applyUtxoDiff
  , applyUtxoDiffForKeys
    -- * Filter
  , filterOnlyKey
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid
import           Data.Semigroup.Cancellative
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.Generics
import           NoThunks.Class
import           Prelude hiding (null)

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

-- | A difference in the UTxO set.
--
-- INVARIANTS:
-- - *The UTxO Property*: the keys in the map in the first component *MUST* be
--   disjoint from the keys in the map in the second component.
--
--   This follows the UTxO flow, entries can only be created once and can only
--   be deleted once they were created.
data UtxoDiff k v =
    UtxoDiff
      (Map k v) -- ^ Deletes
      (Map k v) -- ^ Inserts
  deriving stock (Show, Eq)
  deriving Generic
  deriving NoThunks

-- | The type 'UtxoDiff' is **not** a Semigroup, but this type is.
--
-- The user can throw an error (with a nice CallStack) if 'NotAUtxoDiff' is
-- returned.
data MaybeUtxoDiff k v =
    NotAUtxoDiff
  | AUtxoDiff (UtxoDiff k v)
  deriving stock (Show, Eq)
  deriving Generic
  deriving NoThunks

instance Functor (UtxoDiff k) where
  fmap f (UtxoDiff m1 m2) = UtxoDiff (Map.map f m1) (Map.map f m2)

-- | PRECONDITION:
-- - @a@ satisfies the UTxO property
-- - @b@ satisfies the UTxO property
-- - let @UtxoDiff delA insA = a@ and @UtxoDiff delB insB = b@ then
--    - @delA ∩ delB = ∅@
--    - @insA ∩ insB = ∅@
--    - @delA ∩ insB = ∅@
--
-- POSTCONDITION: @a <> b@ satisfies the UTxO property
instance Ord k => Semigroup (MaybeUtxoDiff k v) where
  NotAUtxoDiff <> _ = NotAUtxoDiff
  _ <> NotAUtxoDiff = NotAUtxoDiff
  (AUtxoDiff u1@(UtxoDiff m1 m2)) <> (AUtxoDiff u2@(UtxoDiff m3 m4)) =
    if utxoProperty2 u1 u2
      then AUtxoDiff $ UtxoDiff
              (Map.union m1 (m3 `Map.difference` m2))
              (Map.union (m2 `Map.difference` m3) m4)
      else NotAUtxoDiff

instance Ord k => Monoid (MaybeUtxoDiff k v) where
  mempty = AUtxoDiff $ UtxoDiff mempty mempty

{------------------------------------------------------------------------------
  Conversion
------------------------------------------------------------------------------}

keysSet :: Ord k => UtxoDiff k v -> Set k
keysSet (UtxoDiff m1 m2) = Map.keysSet m1 <> Map.keysSet m2

{------------------------------------------------------------------------------
  Construction
------------------------------------------------------------------------------}

utxoProperty :: Ord k => UtxoDiff k v -> Bool
utxoProperty (UtxoDiff dels ins) =
  Map.keysSet dels `Set.disjoint` Map.keysSet ins

utxoProperty2 :: Ord k => UtxoDiff k v -> UtxoDiff k v -> Bool
utxoProperty2
  (UtxoDiff (Map.keysSet -> dels1) (Map.keysSet -> ins1))
  (UtxoDiff (Map.keysSet -> dels2) (Map.keysSet -> ins2)) =
     -- no double deletes
     dels1 `Set.disjoint` dels2
     -- no double inserts
  && ins1  `Set.disjoint` ins2
     -- no delete before insert
  && dels1 `Set.disjoint` ins2

empty :: Ord k => UtxoDiff k v
empty = UtxoDiff mempty mempty

-- | Compute a 'UtxoDiff'.
--
-- If you know that the arguments are key-disjoint then you should use the more
-- direct function 'utxoDiff', which skips computing the difference of the maps.
diff :: Ord k => Map k v -> Map k v -> UtxoDiff k v
diff m1 m2 = UtxoDiff (m1 `Map.difference` m2) (m2 `Map.difference` m1)

fromMapInserts :: Ord k => Map k v -> UtxoDiff k v
fromMapInserts = UtxoDiff mempty

fromMapDeletes :: Ord k => Map k v -> UtxoDiff k v
fromMapDeletes = flip UtxoDiff mempty

fromListInserts :: Ord k => [(k, v)] -> UtxoDiff k v
fromListInserts = UtxoDiff mempty . Map.fromList

fromListDeletes :: Ord k => [(k, v)] -> UtxoDiff k v
fromListDeletes = flip UtxoDiff mempty . Map.fromList

{------------------------------------------------------------------------------
  Query
------------------------------------------------------------------------------}

null :: UtxoDiff k v -> Bool
null (UtxoDiff m1 m2) = Map.null m1 && Map.null m2

size :: UtxoDiff k v -> Int
size (UtxoDiff m1 m2) = Map.size m1 + Map.size m2

numInserts :: UtxoDiff k v -> Int
numInserts (UtxoDiff _ m2) = Map.size m2

numDeletes :: UtxoDiff k v -> Int
numDeletes (UtxoDiff m1 _) = Map.size m1

{------------------------------------------------------------------------------
  Applying diffs
------------------------------------------------------------------------------}

applyUtxoDiff ::
     Ord k
  => Map k v
  -> UtxoDiff k v
  -> Map k v
applyUtxoDiff m (UtxoDiff deletes inserts) =
    (m `Map.difference` deletes) `Map.union` inserts

applyUtxoDiffForKeys ::
     Ord k
  => Map k v
  -> Set k
  -> UtxoDiff k v
  -> Map k v
applyUtxoDiffForKeys m ks (UtxoDiff deletes inserts) =
  applyUtxoDiff
    m
    (UtxoDiff (deletes `Map.restrictKeys` (Map.keysSet m `Set.union` ks))
          (inserts `Map.restrictKeys` (Map.keysSet m `Set.union` ks)))

{-------------------------------------------------------------------------------
  Filter
-------------------------------------------------------------------------------}

filterOnlyKey :: (k -> Bool) -> UtxoDiff k v -> UtxoDiff k v
filterOnlyKey f (UtxoDiff m1 m2) =
  UtxoDiff (Map.filterWithKey (const . f) m1) (Map.filterWithKey (const . f) m2)

{-------------------------------------------------------------------------------
  Reductive instances
-------------------------------------------------------------------------------}

instance (Ord k, Eq v) => LeftReductive (MaybeUtxoDiff k v) where
  stripPrefix NotAUtxoDiff _ = Just NotAUtxoDiff
  stripPrefix _ NotAUtxoDiff = Just NotAUtxoDiff
  stripPrefix (AUtxoDiff (UtxoDiff m1 m2)) (AUtxoDiff (UtxoDiff m3 m4)) =
    let
        -- m1 ⊆ m3 because of UTXO-property
        -- we cannot have added something that was deleted
        dels = stripPrefix m1 m3
    in
      case (dels, stripPrefix m2 m4) of
        -- Violation of UTXO-property
        (Nothing, _) -> Just NotAUtxoDiff
        -- m2 ⊄ m4 because some have been cancelled
        (Just dels', Nothing) ->
          let
            (notCancelled, cancelled) =
              (m2 `Map.intersection` m4, m2 `Map.difference` m4)
          in case stripPrefix notCancelled m4 of
              Nothing  ->  Just NotAUtxoDiff
              Just m4' -> Just $ AUtxoDiff $ UtxoDiff (cancelled <> dels') m4'
        -- m2 ⊆ m4 because none have been cancelled
        (Just dels', Just m5) -> Just $ AUtxoDiff $ UtxoDiff dels' m5

instance (Ord k, Eq v) => RightReductive (MaybeUtxoDiff k v) where
  stripSuffix NotAUtxoDiff _ = Just NotAUtxoDiff
  stripSuffix _ NotAUtxoDiff = Just NotAUtxoDiff
  stripSuffix (AUtxoDiff (UtxoDiff m1 m2)) (AUtxoDiff (UtxoDiff m3 m4)) =
    let
        -- m2 ⊇ m4 because of UTXO-property
        -- we cannot have deleted anything that didn't exist
        ins = stripSuffix m2 m4
    in case (ins, stripSuffix m1 m3) of
        (Nothing, _) -> Just NotAUtxoDiff
        (Just ins', Nothing) ->
          let
            (notCancelled, cancelled) =
              (m1 `Map.intersection` m3, m1 `Map.difference` m3)
          in case stripSuffix notCancelled m3 of
              Nothing  -> Just NotAUtxoDiff
              Just m3' -> Just $ AUtxoDiff $ UtxoDiff m3' (ins' <> cancelled)
        (Just ins', Just m5) ->
          -- m1 ⊇ m3 because none have been cancelled
          Just $ AUtxoDiff $ UtxoDiff m5 ins'
