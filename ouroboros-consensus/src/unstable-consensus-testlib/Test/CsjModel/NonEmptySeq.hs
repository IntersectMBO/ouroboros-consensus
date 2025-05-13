{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A non-empty sequence type with some custom methods with types that are
-- convenient for "Test.CsjModel"
module Test.CsjModel.NonEmptySeq (module Test.CsjModel.NonEmptySeq) where

import           Control.Monad (guard)
import qualified Data.Maybe as L (Maybe (Just, Nothing))
-- TODO use AnchoredSeq?
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)
import           NoThunks.Class (NoThunks)

-- | A non-empty 'Seq'
newtype NonEmptySeq a = UnsafeNonEmptySeq (Seq a)
  deriving stock (Generic, Read, Show)
  deriving newtype (Functor)
  deriving anyclass (NoThunks)

neTrim :: NonEmptySeq a -> NonEmptySeq a
neTrim xs = UnsafeNonEmptySeq $ Seq.Empty Seq.|> neHead xs Seq.|> neLast xs

nonEmptySeq :: Seq a -> L.Maybe (NonEmptySeq a)
nonEmptySeq xs = UnsafeNonEmptySeq xs <$ guard (not (Seq.null xs))

consSeq :: a -> Seq a -> (NonEmptySeq a)
consSeq x xs = UnsafeNonEmptySeq $ x Seq.<| xs

neIndex :: NonEmptySeq a -> Int -> a
neIndex (UnsafeNonEmptySeq xs) i = Seq.index xs i

neLength :: NonEmptySeq a -> Int
neLength (UnsafeNonEmptySeq xs) = Seq.length xs

neHead :: NonEmptySeq a -> a
neHead xs = xs `neIndex` 0

neLast :: NonEmptySeq a -> a
neLast xs = xs `neIndex` (neLength xs - 1)

-- | The element at index @'div' n 2@
neMid :: NonEmptySeq a -> a
neMid (UnsafeNonEmptySeq xs) = Seq.index xs (Seq.length xs `div` 2)

-- | The elements before 'neMid'
neBeforeMid :: NonEmptySeq a -> Seq a
neBeforeMid (UnsafeNonEmptySeq xs) = Seq.take (Seq.length xs `div` 2) xs

neDrop :: Int -> NonEmptySeq a -> Seq a
neDrop n (UnsafeNonEmptySeq xs) = Seq.drop n xs

neTake :: Int -> NonEmptySeq a -> Seq a
neTake n (UnsafeNonEmptySeq xs) = Seq.take n xs

-- | 'neMid' is the rightmost element of the first half
neHalves :: NonEmptySeq a -> (NonEmptySeq a, Seq a)
neHalves (UnsafeNonEmptySeq xs) =
    case gte of
        Seq.Empty     -> error "impossible!"
        eq Seq.:<| gt -> (UnsafeNonEmptySeq $ lt Seq.|> eq, gt)
  where
    (lt, gte) = Seq.splitAt (Seq.length xs `div` 2) xs

neInit :: NonEmptySeq a -> Seq a
neInit (UnsafeNonEmptySeq xs) = case xs of
    Seq.Empty     -> error "impossible!"
    xs' Seq.:|> _ -> xs'

toSeq :: NonEmptySeq a -> Seq a
toSeq (UnsafeNonEmptySeq xs) = xs

data BinarySearchResult =
    -- | INVARIANT: both of these indices are @0 <=@ and @< n@.
    LastFalseFirstTrue {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  |
    -- | All of the elements have the same value for the predicate.
    Uniformly !Bool
  deriving (Eq, Show)

neBinarySearch :: (a -> Bool) -> NonEmptySeq a -> BinarySearchResult
neBinarySearch predicate (UnsafeNonEmptySeq xs) =
    finish $ go l0 r0
  where
    l0 = negate 1
    r0 = length xs

    go !l !r =
        if l + 1 == r then (l, r) else
        let !m = div (l + r) 2
        in
        if predicate $ Seq.index xs m then go l m else go m r

    finish (l, r) = case (l0 == l, r0 == r) of
        (True , True ) -> error "impossible!"
        (True , False) -> Uniformly True
        (False, True ) -> Uniformly False
        _              -> LastFalseFirstTrue l r

-- | Find the leftmost index of a point in a strictly ascending 'Seq'
leftmostInAscSeq :: Ord q => (p -> q) -> Seq p -> q -> L.Maybe Int
leftmostInAscSeq f xs =
    case nonEmptySeq xs of
        L.Nothing -> const L.Nothing
        L.Just ne -> \q -> do
            i <- case neBinarySearch ((> q) . f) ne of
                LastFalseFirstTrue i _j -> L.Just i
                Uniformly False         -> L.Just $ neLength ne - 1
                Uniformly True          -> L.Nothing
            guard $ q == f (neIndex ne i)
            pure i
