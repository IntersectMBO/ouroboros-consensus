module HeldEbHashSet (
    HeldEbHashSet
  , empty
  , insert
  , member
  , prune
  ) where

import           Data.Foldable (foldl', toList)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import           Data.Word (Word16)
import           EbHashMap (EbHash, Slot)

newtype RefCount =
    -- | INVARIANT positive
    RefCount Word16
  deriving (Eq, Show)

instance Semigroup RefCount where RefCount x <> RefCount y = RefCount (x + y)

data HeldEbHashSet =
    HeldEbHashSet
        !(Map EbHash RefCount)
        !(Map Slot (NESet EbHash))
  deriving (Eq, Show)

empty :: HeldEbHashSet
empty = HeldEbHashSet Map.empty Map.empty

-- | Under normal operation this is called at most twice for one slot: the first-announced EB and the certified EB. Because we reconstruct this structure on startup without the central state that deduplicates announcements/certs, a slot battle or a (bounded) sequence of restarts can drive it higher; we accept that laxity, so 'insert' never fails.
insert :: EbHash -> Slot -> HeldEbHashSet -> HeldEbHashSet
insert eh slot (HeldEbHashSet ms els) =
    HeldEbHashSet ms' els'
  where
    already = maybe False (NESet.member eh) (Map.lookup slot els)
    ms'     = if already then ms else Map.insertWith (<>) eh (RefCount 1) ms
    els'    = Map.insertWith NESet.union slot (NESet.singleton eh) els

member :: EbHash -> HeldEbHashSet -> Bool
member eh (HeldEbHashSet ms _els) = Map.member eh ms

prune :: (Slot -> Bool) -> HeldEbHashSet -> HeldEbHashSet
prune p (HeldEbHashSet ms els) =
    HeldEbHashSet
        (foldl' decr ms (concatMap toList (Map.elems dropped)))
        kept
  where
    (dropped, kept) = Map.partitionWithKey (\slot _ -> p slot) els
    decr m eh = Map.update step eh m
    step (RefCount n) | n <= 1    = Nothing
                      | otherwise = Just $! RefCount (n - 1)
