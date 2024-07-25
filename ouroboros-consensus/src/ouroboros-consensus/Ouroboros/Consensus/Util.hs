{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE StandaloneKindSignatures   #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Miscellaneous utilities
module Ouroboros.Consensus.Util (
    -- * Type-level utility
    ShowProxy (..)
  , Some (..)
  , SomePair (..)
  , SomeSecond (..)
    -- * Folding variations
  , foldlM'
  , nTimes
  , nTimesM
  , repeatedly
  , repeatedlyM
    -- * Lists
  , allEqual
  , dropLast
  , firstJust
  , split
  , splits
  , takeLast
  , takeUntil
    -- * Safe variants of existing base functions
  , lastMaybe
  , safeMaximum
  , safeMaximumBy
  , safeMaximumOn
    -- * Hashes
  , hashFromBytesShortE
    -- * Monadic utilities
  , whenJust
    -- * Composition
  , (......:)
  , (.....:)
  , (....:)
  , (...:)
  , (..:)
  , (.:)
    -- * Product
  , pairFst
  , pairSnd
    -- * Miscellaneous
  , eitherToMaybe
  , fib
    -- * Electric code
  , Electric
  , Fuse
  , FuseBlownException (..)
  , electric
  , newFuse
  , withFuse
  ) where

import           Cardano.Crypto.Hash (Hash, HashAlgorithm, hashFromBytesShort)
import           Control.Monad (unless)
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Trans.Class
import           Data.ByteString.Short (ShortByteString)
import           Data.Foldable (asum, toList)
import           Data.Function (on)
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Kind (Type)
import           Data.List as List (foldl', maximumBy)
import           Data.List.NonEmpty (NonEmpty (..), (<|))
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           GHC.Stack
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec (Some (..))
import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

{-------------------------------------------------------------------------------
  Type-level utility
-------------------------------------------------------------------------------}

-- | Pair of functors instantiated to the /same/ existential
data SomePair (f :: k -> Type) (g :: k -> Type) where
    SomePair :: f a -> g a -> SomePair f g

-- | Hide the second type argument of some functor
--
-- @SomeSecond f a@ is isomorphic to @Some (f a)@, but is more convenient in
-- partial applications.
type SomeSecond :: (k1 -> k2 -> Type) -> k1 -> Type
data SomeSecond f a where
  SomeSecond :: !(f a b) -> SomeSecond f a

{-------------------------------------------------------------------------------
  Folding variations
-------------------------------------------------------------------------------}

foldlM' :: forall m a b. Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldlM' f = go
  where
    go :: b -> [a] -> m b
    go !acc []     = return acc
    go !acc (x:xs) = f acc x >>= \acc' -> go acc' xs

repeatedly :: (a -> b -> b) -> ([a] -> b -> b)
repeatedly = flip . List.foldl' . flip

repeatedlyM :: Monad m => (a -> b -> m b) -> ([a] -> b -> m b)
repeatedlyM = flip . foldlM' . flip

-- | Apply a function n times. The value of each application is forced.
nTimes :: forall a. (a -> a) -> Word64 -> (a -> a)
nTimes f n = runIdentity . nTimesM (Identity . f) n

-- | Apply a function n times through a monadic bind. The value of each
-- application is forced.
nTimesM :: forall m a. Monad m => (a -> m a) -> Word64 -> (a -> m a)
nTimesM f = go
  where
    go :: Word64 -> (a -> m a)
    go 0 !x = return x
    go n !x = go (n - 1) =<< f x

{-------------------------------------------------------------------------------
  Lists
-------------------------------------------------------------------------------}

-- | Take the last @n@ elements
takeLast :: Word64 -> [a] -> [a]
takeLast n = reverse . take (fromIntegral n) . reverse

-- | Drop the last @n@ elements
dropLast :: Word64 -> [a] -> [a]
dropLast n = reverse . drop (fromIntegral n) . reverse

firstJust :: forall a b f. Foldable f => (a -> Maybe b) -> f a -> Maybe b
firstJust f = asum . fmap f . toList

allEqual :: Eq a => [a] -> Bool
allEqual []       = True
allEqual [_]      = True
allEqual (x:y:zs) = x == y && allEqual (y:zs)

-- | Take items until the condition is true. If the condition is true for an
-- item, include that item as the last item in the returned list. If the
-- condition was never true, the original list is returned.
--
-- > takeUntil (== 3) [1,2,3,4]
-- [1,2,3]
-- > takeUntil (== 2) [0,1,0]
-- [0,1,0]
-- > takeUntil (== 2) [2,2,3]
-- [2]
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = \case
    []
      -> []
    x:xs
      | p x
      -> [x]
      | otherwise
      -> x:takeUntil p xs

-- | Focus on one element in the list
--
-- E.g.
--
-- >    splits [1..3]
-- > == [ ([]    , 1 , [2,3])
-- >    , ([1]   , 2 , [3]  )
-- >    , ([1,2] , 3 , []   )
-- >    ]
splits :: [a] -> [([a], a, [a])]
splits []     = []
splits (a:as) = ([], a, as) : map (\(xs, y, zs) -> (a:xs, y, zs)) (splits as)

-- | Split a list given a delimiter predicate.
--
-- >>> split (`elem` "xy") "axbyxc"
-- "a" :| ["b","","c"]
--
-- We have the laws
--
-- > concat (split p as) === filter (not . p) as
-- > length (split p as) === length (filter p as) + 1
split :: (a -> Bool) -> [a] -> NonEmpty [a]
split p = \case
    []           -> pure []
    a : as | p a -> [] <| split p as
    a : as       -> let bs :| bss = split p as in (a : bs) :| bss

{-------------------------------------------------------------------------------
  Safe variants of existing base functions
-------------------------------------------------------------------------------}

lastMaybe :: [a] -> Maybe a
lastMaybe []     = Nothing
lastMaybe [x]    = Just x
lastMaybe (_:xs) = lastMaybe xs

safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum = safeMaximumBy compare

safeMaximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
safeMaximumBy _cmp [] = Nothing
safeMaximumBy cmp ls  = Just $ maximumBy cmp ls

safeMaximumOn :: Ord b => (a -> b) -> [a] -> Maybe a
safeMaximumOn f = safeMaximumBy (compare `on` f)

{-------------------------------------------------------------------------------
  Hashes
-------------------------------------------------------------------------------}

-- | Calls 'hashFromBytesShort' and throws an error if the input is of the
-- wrong length.
hashFromBytesShortE ::
     forall h a. (HashAlgorithm h, HasCallStack)
  => ShortByteString
  -> Hash h a
hashFromBytesShortE bs = fromMaybe (error msg) $ hashFromBytesShort bs
  where
    msg =
      "hashFromBytesShort called with ShortByteString of the wrong length: " <>
      show bs

{-------------------------------------------------------------------------------
  Monadic utilities
-------------------------------------------------------------------------------}

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust (Just x) f = f x
whenJust Nothing _  = pure ()

{-------------------------------------------------------------------------------
  Composition
-------------------------------------------------------------------------------}

(.:) :: (y -> z) -> (x0 -> x1 -> y) -> (x0 -> x1 -> z)
(f .: g) x0 x1 = f (g x0 x1)

(..:) :: (y -> z) -> (x0 -> x1 -> x2 -> y) -> (x0 -> x1 -> x2 -> z)
(f ..: g) x0 x1 x2 = f (g x0 x1 x2)

(...:) :: (y -> z) -> (x0 -> x1 -> x2 -> x3 -> y) -> (x0 -> x1 -> x2 -> x3 -> z)
(f ...: g) x0 x1 x2 x3 = f (g x0 x1 x2 x3)

(....:) :: (y -> z) -> (x0 -> x1 -> x2 -> x3 -> x4 -> y) -> (x0 -> x1 -> x2 -> x3 -> x4 -> z)
(f ....: g) x0 x1 x2 x3 x4 = f (g x0 x1 x2 x3 x4)

(.....:) :: (y -> z) -> (x0 -> x1 -> x2 -> x3 -> x4 -> x5 -> y) -> (x0 -> x1 -> x2 -> x3 -> x4 -> x5 -> z)
(f .....: g) x0 x1 x2 x3 x4 x5 = f (g x0 x1 x2 x3 x4 x5)

(......:) :: (y -> z) -> (x0 -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> y) -> (x0 -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> z)
(f ......: g) x0 x1 x2 x3 x4 x5 x6 = f (g x0 x1 x2 x3 x4 x5 x6)

{-------------------------------------------------------------------------------
  Product
-------------------------------------------------------------------------------}

pairFst :: Product f g a -> f a
pairFst (Pair a _) = a

pairSnd :: Product f g a -> g a
pairSnd (Pair _ b) = b

{-------------------------------------------------------------------------------
  Miscellaneous
-------------------------------------------------------------------------------}

-- | Fast Fibonacci computation, using Binet's formula
fib :: Word64 -> Word64
fib n = round $ phi ** fromIntegral n / sq5
  where
    sq5, phi :: Double
    sq5 = sqrt 5
    phi = (1 + sq5) / 2

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

{-------------------------------------------------------------------------------
  Electric code, i.e. regions of code that will throw an exception if accessed
  concurrently.
-------------------------------------------------------------------------------}

-- | An action that cannot be ran without drawing current through a 'Fuse'.
--
-- NOTE: using @Fuse m -> ...@ would suffice but the newtype wrapper is useful
-- for ensuring we don't make mistakes.
newtype Electric m a = Electric (m a)
  deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadCatch)

instance MonadTrans Electric where
  lift = Electric

-- | See 'Electric'
electric :: m a -> Electric m a
electric = Electric

-- | A simple semaphore, though instead of blocking a fatal exception is thrown.
data Fuse m = Fuse !Text !(StrictMVar m ()) deriving (Generic)

deriving instance NoThunks (StrictMVar m ()) => NoThunks (Fuse m)

newFuse :: MonadMVar m => Text -> m (Fuse m)
newFuse name = Fuse name <$> newMVar ()

-- | Put full load on the 'Fuse' while the 'Electric' is running.
--
-- Thus any two 'withFuse' calls with the same 'Fuse' will throw one fatal
-- exception.
--
-- NOTE The metaphor is: when I run at most one waffle iron concurrently, my
-- kitchen's fuse doesn't blow. But it blows if I run more than one waffle iron
-- concurrently.
--
-- WARNING If the given action throws its own exception, then it will never stop
-- putting load on the 'Fuse'.
withFuse ::
     (MonadThrow m, MonadMVar m)
  => Fuse m
  -> Electric m a
  -> m a
withFuse (Fuse name m) (Electric io) = do
  tryTakeMVar m >>= \case
    Nothing -> throwIO $ FuseBlownException name
    Just () -> pure ()
  a <- io
  tryPutMVar m () >>= \b -> unless b $ throwIO $ FuseBlownException name
  pure a

-- | Too much electrical load was put on the 'Fuse', see 'withFuse'.
newtype FuseBlownException = FuseBlownException Text
 deriving (Show)
 deriving anyclass (Exception)
