{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

-- | A type for managing maps over 'EbHash' but entries can't exist
-- without an 'Election' that justifies them. For example, a node only
-- ever wants some EB because some election(s) require that EB.
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
  , RefCounts (..)
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

-- | An election's references: the inactive (superseded) reference and the
-- active reference.
--
-- INVARIANT: the active reference is never equal to the inactive one.
data Refs = Refs !InactiveRef !EbHash
  deriving (Eq, Show)

data InactiveRef =
    NoInactiveRefYet
  | -- | 'supersede' was called before 'insert'
    NoInactiveRef
  | InactiveRef !EbHash
  deriving (Eq, Show)

activeRef :: Refs -> EbHash
activeRef (Refs _ act) = act

-- | An EB's active and inactive reference counts (how many elections name it as
-- their active EB, and how many as their inactive/superseded EB) and a per-EB
-- payload.
--
-- INVARIANT: the total count is positive (i.e. active + inactive > 0); an entry
-- exists iff some election names it.
data RefCounts a = RefCounts !Word16 !Word16 !a
  deriving (Eq, Functor, Show)

refCount :: RefCounts a -> Word16
refCount (RefCounts a i _) = a + i

-----

-- | A bidirectional, reference-counted map between elections and the EBs they
-- name: @a@ is the per-EB payload; the election side carries no payload.
--
-- INVARIANT: the EB map's keys are exactly the EBs named (active or inactive) by
-- some election, and each EB's count equals the number of elections naming it.
data EbHashMap a =
    EbHashMap
      !(Map EbHash   (RefCounts a))
      !(Map Election Refs)
  deriving (Eq, Show)

empty :: EbHashMap a
empty = EbHashMap Map.empty Map.empty

incRefActive :: Semigroup a => EbHash -> a -> Map EbHash (RefCounts a) -> Map EbHash (RefCounts a)
incRefActive eh a = Map.alter step eh
  where
    step Nothing                  = Just (RefCounts 1 0 a)
    step (Just (RefCounts n i x)) = Just (RefCounts (n + 1) i (x <> a))

-- | For one election: convert its reference to @old@ from active to inactive, and
-- add an active reference to @new@ (payload @a@). (@old@'s total is unchanged; @new@
-- gains one.) We never deactivate a reference without activating a different one.
deactivateRef :: Semigroup a => EbHash -> EbHash -> a -> Map EbHash (RefCounts a) -> Map EbHash (RefCounts a)
deactivateRef old new a = incRefActive new a . Map.adjust step old
  where
    step (RefCounts ac i x)
      | ac == 0   = error "EbHashMap.deactivateRef: no active reference"
      | otherwise = RefCounts (ac - 1) (i + 1) x

decRefActive :: EbHash -> Map EbHash (RefCounts a) -> Map EbHash (RefCounts a)
decRefActive = Map.update step
  where
    step (RefCounts a i x)
      | a == 0     = error "EbHashMap.decRefActive: no active reference"
      | a + i <= 1 = Nothing
      | otherwise  = Just (RefCounts (a - 1) i x)

decRefInactive :: EbHash -> Map EbHash (RefCounts a) -> Map EbHash (RefCounts a)
decRefInactive = Map.update step
  where
    step (RefCounts a i x)
      | i == 0     = error "EbHashMap.decRefInactive: no inactive reference"
      | a + i <= 1 = Nothing
      | otherwise  = Just (RefCounts a (i - 1) x)

-- | Add @el@'s active reference to @eh@, combining the per-EB payload via '<>'
upsert :: Semigroup a => EbHash -> Election -> a -> EbHashMap a -> EbHashMap a
upsert eh el a' (EbHashMap ebs els) =
    case Map.lookup el els of
      Nothing ->
        EbHashMap (incRefActive eh a' ebs) (setEl (Refs NoInactiveRefYet eh))
      Just (Refs _ act)
        | act == eh -> EbHashMap (Map.adjust (fmap (<> a')) eh ebs) els
        | otherwise -> error "EbHashMap.insert: inserted the wrong EbHash"
  where
    setEl r = Map.insert el r els

-- | 'Nothing' if the lookup missed or the update returned 'Nothing'
updateEb :: EbHash -> (a -> Maybe a) -> EbHashMap a -> Maybe (EbHashMap a)
updateEb eh f (EbHashMap ebs els) =
    (\x -> EbHashMap x els) <$> 
    Map.alterF
        (\case
            Nothing -> Nothing
            Just (RefCounts n i x) -> do
                !y <- f x
                Just (Just (RefCounts n i y))
        )
        eh
        ebs

-- | This @a@ value is only the default; update it with 'upsert' or 'updateEb'
supersede :: Semigroup a => Election -> EbHash -> a -> EbHashMap a -> EbHashMap a
supersede el eh a (EbHashMap ebs els) =
    case Map.lookup el els of
      Nothing ->
        EbHashMap (incRefActive eh a ebs) (Map.insert el (Refs NoInactiveRef eh) els)
      Just (Refs NoInactiveRefYet act)
        | act == eh -> EbHashMap (Map.adjust (fmap (<> a)) eh ebs) (Map.insert el (Refs NoInactiveRef act) els)
        | otherwise -> EbHashMap (deactivateRef act eh a ebs) (Map.insert el (Refs (InactiveRef act) eh) els)
      Just (Refs NoInactiveRef _) -> error "EbHashMap.supersede: election already superseded"
      Just (Refs InactiveRef{} _) -> error "EbHashMap.supersede: election already superseded"

lookupEb :: EbHash -> EbHashMap a -> Maybe (RefCounts a)
lookupEb eh (EbHashMap ebs _) = Map.lookup eh ebs

lookupElection :: Election -> EbHashMap a -> Maybe Refs
lookupElection el (EbHashMap _ els) = Map.lookup el els

deleteElection :: Election -> EbHashMap a -> EbHashMap a
deleteElection el m@(EbHashMap ebs els) =
    case Map.lookup el els of
      Nothing             -> m
      Just (Refs inact act) -> EbHashMap (dropRefs ebs inact act) (Map.delete el els)

pruneElections :: (Election -> Bool) -> EbHashMap a -> EbHashMap a
pruneElections p (EbHashMap ebs els) =
    EbHashMap (foldl' step ebs (Map.elems dropped)) kept
  where
    (dropped, kept) = Map.partitionWithKey (\el _ -> p el) els
    step acc (Refs inact act) = dropRefs acc inact act

dropRefs :: Map EbHash (RefCounts a) -> InactiveRef -> EbHash -> Map EbHash (RefCounts a)
dropRefs ebs inact act =
    case inact of InactiveRef old -> decRefInactive old (decRefActive act ebs); _ -> decRefActive act ebs

invariant :: EbHashMap a -> Bool
invariant (EbHashMap ebs els) =
       all neverEqual (Map.elems els)
    && Map.keys tally == Map.keys ebs
    && and (Map.intersectionWith (\(a, i) (RefCounts a' i' _) -> a == a' && i == i') tally ebs)
    && all (\(RefCounts a i _) -> 0 < a + i) (Map.elems ebs)
  where
    neverEqual (Refs inact act) = case inact of InactiveRef eh -> eh /= act; _ -> True
    tally = foldl' count Map.empty (Map.elems els)
    count m (Refs inact act) =
      let m1 = Map.insertWith addPair act ((1, 0) :: (Word16, Word16)) m
      in case inact of InactiveRef old -> Map.insertWith addPair old (0, 1) m1; _ -> m1
    addPair (a1, i1) (a2, i2) = (a1 + a2, i1 + i2)
