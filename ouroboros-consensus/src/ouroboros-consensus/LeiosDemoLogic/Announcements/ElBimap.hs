-- | A bidirectional map, both halves kept strict, relating each
-- `ElId` to a set of `a` and back.
--
module LeiosDemoLogic.Announcements.ElBimap (module LeiosDemoLogic.Announcements.ElBimap) where

import           Cardano.Slotting.Slot (SlotNo)
import           Data.ByteString.Short (ShortByteString)
import           Data.Foldable (foldl')
import qualified Data.Map.Strict as Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet

-- | The slot number and pool id of an election
data ElId =
    -- | The bytes are the hash of the pool's cold verification key
    --
    -- TODO should it be the hash, or just the key itself?
    MkElId !SlotNo !ShortByteString
  deriving (Eq)

-- | INVARIANT: lexicographically starts with 'SlotNo'
instance Ord ElId
  where
    compare (MkElId slL idL) (MkElId slR idR) =
        compare slL (slR :: SlotNo)
     <> compare idL idR

-- | A many-to-many relation between 'ElId' (left) and @a@ (right),
-- indexed in both directions
--
-- INVARIANT: the halves agree --- @r@ occurs in @forwardHalf ! l@ iff
-- @l@ occurs in @inverseHalf ! r@.
data ElBimap a =
    MkElBimap {
        forwardHalf :: !(Strict.Map ElId (NESet a))
      ,
        inverseHalf :: !(Strict.Map a (NESet ElId))
      }

emptyElBimap :: ElBimap a
emptyElBimap = MkElBimap Map.empty Map.empty

insertElBimap :: Ord a => ElId -> a -> ElBimap a -> ElBimap a
insertElBimap l r bm =
    MkElBimap {
        forwardHalf = Map.insertWith (<>) l (NESet.singleton r) (forwardHalf bm)
      ,
        inverseHalf = Map.insertWith (<>) r (NESet.singleton l) (inverseHalf bm)
      }

-- | Insert an election paired with each element of a set
insertElBimapLs :: Ord a => ElId -> Set a -> ElBimap a -> ElBimap a
insertElBimapLs l rs bm =
    case NESet.nonEmptySet rs of
        Nothing -> bm
        Just rs' -> MkElBimap {
            forwardHalf = Map.insertWith (<>) l rs' (forwardHalf bm)
          ,
            inverseHalf = foldl' addToInverse (inverseHalf bm) rs
          }
  where
    addToInverse inv r = Map.insertWith (<>) r (NESet.singleton l) inv

lookupElBimapL :: ElId -> ElBimap a -> Set a
lookupElBimapL l bm =
    maybe Set.empty NESet.toSet
  $ Map.lookup l
  $ forwardHalf bm

lookupElBimapR :: Ord a => a -> ElBimap a -> Set ElId
lookupElBimapR r bm =
    maybe Set.empty NESet.toSet
  $ Map.lookup r
  $ inverseHalf bm

-- | Remove an election and every pair it was part of
deleteElBimapL :: Ord a => ElId -> ElBimap a -> ElBimap a
deleteElBimapL l bm =
    case Map.lookup l (forwardHalf bm) of
        Nothing -> bm
        Just rs -> MkElBimap {
            forwardHalf = Map.delete l (forwardHalf bm)
          ,
            inverseHalf = foldl' dropFromInverse (inverseHalf bm) rs
          }
  where
    dropFromInverse inv r = Map.update (NESet.nonEmptySet . NESet.delete l) r inv

-- | Remove a right key and every pair it was part of
deleteElBimapR :: Ord a => a -> ElBimap a -> ElBimap a
deleteElBimapR r bm =
    case Map.lookup r (inverseHalf bm) of
        Nothing -> bm
        Just ls -> MkElBimap {
            forwardHalf = foldl' dropFromForward (forwardHalf bm) ls
          ,
            inverseHalf = Map.delete r (inverseHalf bm)
          }
  where
    dropFromForward fwd l = Map.update (NESet.nonEmptySet . NESet.delete r) l fwd

-- | Remove every election older than the given slot and every pair
-- those elections were part of
--
-- Called whenever the ChainDB's immutable tip advances to a new slot.
pruneElBimap :: Ord a => SlotNo -> ElBimap a -> ElBimap a
pruneElBimap immTipSlot bm =
    MkElBimap {
        forwardHalf = forwardHalf'
      ,
        inverseHalf = Map.foldlWithKey' dropElection (inverseHalf bm) pruned
      }
  where
    (pruned, forwardHalf') = Map.spanAntitone tooOld (forwardHalf bm)

    dropElection inv el rs = foldl' (dropPair el) inv rs

    dropPair el inv r = Map.update (NESet.nonEmptySet . NESet.delete el) r inv

    -- NB strict comparison, so that the immtip's announcement remains
    tooOld (MkElId elSlot _poolId) = elSlot < immTipSlot
