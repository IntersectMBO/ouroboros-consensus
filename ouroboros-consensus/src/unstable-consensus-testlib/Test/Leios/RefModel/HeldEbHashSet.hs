{-# LANGUAGE LambdaCase #-}

module HeldEbHashSet (
    HeldEbHashSet
  , empty
  , insert
  , member
  , prune
  ) where

import           Data.List (foldl')
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Word (Word16)
import           EbHashMap (EbHash, Election)

-- | One or two, but no meaningful order
data Refs = Ref1 !EbHash | Ref2 !EbHash !EbHash
  deriving (Eq, Show)

newtype RefCount =
    -- | INVARIANT positive
    RefCount Word16
  deriving (Eq, Show)

instance Semigroup RefCount where RefCount x <> RefCount y = RefCount (x + y)

data HeldEbHashSet = HeldEbHashSet !(Map EbHash RefCount) !(Map Election Refs)
  deriving (Eq, Show)

empty :: HeldEbHashSet
empty = HeldEbHashSet Map.empty Map.empty

insert :: EbHash -> Election -> HeldEbHashSet -> HeldEbHashSet
insert eh el (HeldEbHashSet ehs els) =
    HeldEbHashSet
        (Map.insertWith (<>) eh (RefCount 1) ehs)
        (Map.alter
            (\case
                Nothing -> Just $! Ref1 eh
                Just (Ref1 old) -> Just $! Ref2 old eh
                Just Ref2{} -> error "HeldEbHashSet.insert: third insert for one election"
            )
            el
            els
        )

member :: EbHash -> HeldEbHashSet -> Bool
member eh (HeldEbHashSet ehs _els) = Map.member eh ehs

prune :: (Election -> Bool) -> HeldEbHashSet -> HeldEbHashSet
prune p (HeldEbHashSet ehs els) =
    HeldEbHashSet
        (foldl' decr ehs (concatMap refsEbs (Map.elems dropped)))
        kept
  where
    (dropped, kept) = Map.partitionWithKey (\el _ -> p el) els
    decr m eh = Map.update step eh m
    step (RefCount n) | n <= 1    = Nothing
                      | otherwise = Just $! RefCount (n - 1)
    refsEbs (Ref1 a)   = [a]
    refsEbs (Ref2 a b) = [a, b]
