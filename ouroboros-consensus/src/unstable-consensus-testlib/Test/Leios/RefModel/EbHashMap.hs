{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

-- | A type for managing maps over 'EbHash' but entries can't exist
-- without a 'Election' that justifies them
--
-- Note that there's at most two 'EbHash'es per 'Election'. That'll be
-- the first-announced and then the certified EB if it's different
-- (which should be impossible/rare due to L_hdr).
module EbHashMap (
    -- * Identifiers
    Slot (..)
  , PoolId (..)
  , Election (..)
  , electionSlot
  , EbHash (..)
    -- * Reference-counted bidirectional election/EB map
  , Refs (..)
  , InactiveRef (..)
  , RefCount (..)
  , EbHashMap (..)
  , activeRef
    -- * Construction
  , empty
  , upsert
  , supersede
    -- * Query
  , lookupEb
  , lookupElection
  , refCount
    -- * Update
  , updateEb
    -- * Deletion / GC
  , deleteElection
  , pruneElections
    -- * Invariant (for tests)
  , invariant
  ) where

import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word16, Word64)

newtype Slot   = Slot   Word64 deriving (Eq, Ord, Show)
newtype PoolId = PoolId Word64 deriving (Eq, Ord, Show)
newtype EbHash = EbHash Word64 deriving (Eq, Ord, Show)

data Election = Election Slot PoolId            -- election = (slot, pool)
  deriving (Eq, Ord, Show)

electionSlot :: Election -> Slot
electionSlot (Election s _) = s

-- | An election's references: the inactive (superseded) reference, the active
-- reference, and a per-election payload.
--
-- INVARIANT: the active reference is never equal to the inactive one.
data Refs b = Refs !InactiveRef !EbHash !b
  deriving (Eq, Functor, Show)

data InactiveRef =
    NoInactiveRefYet
  | -- | 'supersede' was called before 'insert'
    NoInactiveRef
  | InactiveRef !EbHash
  deriving (Eq, Show)

activeRef :: Refs b -> EbHash
activeRef (Refs _ act _) = act

-- | An EB's reference count (the number of elections naming it, active or
-- inactive) and a per-EB payload.
--
-- INVARIANT: the count is positive (i.e. strictly > 0); an entry exists iff
-- some election names it.
data RefCount a = RefCount !Word16 !a
  deriving (Eq, Functor, Show)

instance Semigroup a => Semigroup (RefCount a)
  where
    RefCount n x <> RefCount m y = RefCount (n + m) (x <> y)

refCount :: RefCount a -> Word16
refCount (RefCount n _) = n

-----

-- | A bidirectional, reference-counted map between elections and the EBs they
-- name: @a@ is the per-EB payload, @b@ the per-election payload.
--
-- INVARIANT: the EB map's keys are exactly the EBs named (active or inactive) by
-- some election, and each EB's count equals the number of elections naming it.
data EbHashMap a b =
    EbHashMap
      !(Map EbHash   (RefCount a))
      !(Map Election (Refs      b))
  deriving (Eq, Show)

empty :: EbHashMap a b
empty = EbHashMap Map.empty Map.empty

incRef :: Semigroup a => EbHash -> a -> Map EbHash (RefCount a) -> Map EbHash (RefCount a)
incRef eh a = Map.insertWith collide eh (RefCount 1 a)
  where
    collide new old = old <> new

decRef :: EbHash -> Map EbHash (RefCount a) -> Map EbHash (RefCount a)
decRef = Map.update step
  where
    step (RefCount n x)
      | n <= 1    = Nothing
      | otherwise = Just (RefCount (n - 1) x)

-- | This @b@ value is only the default; update it with 'supersede'
upsert :: Semigroup a => EbHash -> Election -> a -> b -> EbHashMap a b -> EbHashMap a b
upsert eh el a' b (EbHashMap ebs els) =
    case Map.lookup el els of
      Nothing ->
        EbHashMap (incRef eh a' ebs) (setEl (Refs NoInactiveRefYet eh b))
      Just (Refs _ act _)
        | act == eh -> EbHashMap (Map.adjust (fmap (<> a')) eh ebs) els
        | otherwise -> error "EbHashMap.insert: inserted the wrong EbHash"
  where
    setEl r = Map.insert el r els

-- | 'Nothing' if the lookup missed or the update returned 'Nothing'
updateEb :: EbHash -> (a -> Maybe a) -> EbHashMap a b -> Maybe (EbHashMap a b)
updateEb eh f (EbHashMap ebs els) =
    (\x -> EbHashMap x els) <$> 
    Map.alterF
        (\case
            Nothing -> Nothing
            Just (RefCount n x) -> do
                !y <- f x
                Just (Just (RefCount n y))
        )
        eh
        ebs

-- | This @a@ value is only the default; update it with 'upsert' or 'updateEb'
supersede :: Semigroup a => Election -> EbHash -> a -> b -> EbHashMap a b -> EbHashMap a b
supersede el eh a b (EbHashMap ebs els) =
    case Map.lookup el els of
      Nothing ->
        EbHashMap (incRef eh a ebs) (Map.insert el (Refs NoInactiveRef eh b) els)
      Just (Refs NoInactiveRefYet act _)
        | act == eh -> EbHashMap (Map.adjust (fmap (<> a)) eh ebs) (Map.insert el (Refs NoInactiveRef act b) els)
        | otherwise -> EbHashMap (incRef eh a ebs) (Map.insert el (Refs (InactiveRef act) eh b) els)
      Just (Refs NoInactiveRef _ _) -> error "EbHashMap.supersede: election already superseded"
      Just (Refs InactiveRef{} _ _) -> error "EbHashMap.supersede: election already superseded"

lookupEb :: EbHash -> EbHashMap a b -> Maybe (RefCount a)
lookupEb eh (EbHashMap ebs _) = Map.lookup eh ebs

lookupElection :: Election -> EbHashMap a b -> Maybe (Refs b)
lookupElection el (EbHashMap _ els) = Map.lookup el els

deleteElection :: Election -> EbHashMap a b -> EbHashMap a b
deleteElection el m@(EbHashMap ebs els) =
    case Map.lookup el els of
      Nothing             -> m
      Just (Refs inact act _) -> EbHashMap (dropRefs ebs inact act) (Map.delete el els)

pruneElections :: (Election -> Bool) -> EbHashMap a b -> EbHashMap a b
pruneElections p (EbHashMap ebs els) =
    EbHashMap (foldl' step ebs (Map.elems dropped)) kept
  where
    (dropped, kept) = Map.partitionWithKey (\el _ -> p el) els
    step acc (Refs inact act _) = dropRefs acc inact act

dropRefs :: Map EbHash (RefCount a) -> InactiveRef -> EbHash -> Map EbHash (RefCount a)
dropRefs ebs inact act =
    case inact of InactiveRef old -> decRef old (decRef act ebs); _ -> decRef act ebs

invariant :: EbHashMap a b -> Bool
invariant (EbHashMap ebs els) =
       all neverEqual (Map.elems els)
    && Map.keys tally == Map.keys ebs
    && and (Map.intersectionWith (\n (RefCount m _) -> n == m) tally ebs)
    && all (\(RefCount n _) -> 0 < n) (Map.elems ebs)
  where
    neverEqual (Refs inact act _) = case inact of InactiveRef eh -> eh /= act; _ -> True
    tally = foldl' count Map.empty (Map.elems els)
    count m (Refs inact act _) =
      let m1 = Map.insertWith (+) act (1 :: Word16) m
      in case inact of InactiveRef old -> Map.insertWith (+) old 1 m1; _ -> m1
