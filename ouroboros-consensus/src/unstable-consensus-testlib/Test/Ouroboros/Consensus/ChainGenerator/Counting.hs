{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Very strong types for working with indices, counts, etc within sequences.
module Test.Ouroboros.Consensus.ChainGenerator.Counting
  ( -- * general counts
    Count (Count)
  , forgetBase
  , forgetElem
  , getCount
  , (+)
  , (-)

    -- * indices and sizes
  , Index
  , Preds
  , Size
  , Total
  , forRange_
  , lastIndex
  , range
  , uniformIndex

    -- * windows
  , Contains (Contains, UnsafeContains)
  , Lbl (Lbl)
  , SomeWindow (SomeWindow)
  , Win
  , forgetWindow
  , fromWindow
  , fromWindowVar
  , joinWin
  , toWindow
  , toWindowVar
  , truncateWin
  , windowLast
  , windowSize
  , windowStart
  , withSuffixWindow
  , withTopWindow
  , withWindow
  , withWindowBetween

    -- * vectors
  , MVector (MVector)
  , Vector (Vector)
  , createV
  , getMVector
  , getVector
  , lengthMV
  , lengthV
  , modifyMV
  , readMV
  , readV
  , replicateMV
  , sliceMV
  , sliceV
  , unsafeThawV
  , writeMV

    -- * variables
  , Other
  , Var
  , joinVar
  , toIndex
  , toSize
  , toVar
  ) where

import Control.Monad.ST (ST)
import Data.Coerce (coerce)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Type.Equality as TypeEq
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import GHC.OverloadedLabels (IsLabel (fromLabel))
import qualified System.Random.Stateful as R
import qualified Test.Ouroboros.Consensus.ChainGenerator.Some as Some
import qualified Test.QuickCheck as QC
import Prelude hiding ((+), (-))
import qualified Prelude

-----

infixl 6 .+, .-

(.+) :: Int -> Int -> Int
(.+) = (Prelude.+)

(.-) :: Int -> Int -> Int
(.-) = (Prelude.-)

-----

-- | A type-indexed Int to represent counts of elements in containers
--
-- * @base@ is the type-level name of the container in which we are counting (e.g. @Win (Lbl HonestLbl) skolem1@)
-- * @elem@ is the type-level name of the elements in the container (e.g. 'Test.Ouroboros.Consensus.ChainGenerator.Slot.SlotE')
-- * @which@ is the type-level name of some property that identifies the
--   particular elements that we are counting (e.g. 'Pred', 'Total', or 'Other')
--
-- TODO: rename @base@ to @container@
newtype Count (base :: Type) (elem :: kelem) (which :: kwhich) = Count Int
  deriving (QC.Arbitrary, Eq, Ord, Read, Show)

getCount :: Count base elem which -> Int
getCount (Count n) = n

infixl 6 +, -

(+) :: Count base elem which -> Int -> Count base elem which
(+) (Count i) j = Count (i .+ j)

(-) :: Count base elem which -> Int -> Count base elem which
(-) (Count i) j = Count (i .- j)

forgetBase :: Count base elem which -> Some.Forgotten (Count () elem which)
forgetBase (Count x) = Some.forgotten $ Count x

forgetElem :: Count base elem which -> Some.Forgotten (Count base () which)
forgetElem (Count x) = Some.forgotten $ Count x

-----

-- | Type-level name for counting elements with an index smaller
-- than a given value
data Preds

-- | Type-level name for counting all elements in a container
data Total

type Index base elem = Count base elem Preds
type Size base elem = Count base elem Total

-- | The 'Index' of the rightmost element in the sequence of the given 'Size'
lastIndex :: Size base elem -> Index base elem
lastIndex (Count n) = Count (n .- 1)

range :: Size base elem -> [Index base elem]
range (Count n) = coerce [0 .. max 0 n .- 1]

forRange_ :: Applicative f => Size base elem -> (Index base elem -> f a) -> f ()
forRange_ c = for_ (range c)

uniformIndex :: R.StatefulGen g m => Size base elem -> g -> m (Index base elem)
uniformIndex n g = Count <$> R.uniformRM (0, getCount $ lastIndex n) g

-----

-- | A human-readable label for a 'Win'
type Lbl :: forall {k}. k -> Type
data Lbl lbl = Lbl

instance lbl TypeEq.~~ s => IsLabel s (Lbl lbl) where fromLabel = Lbl

-- | A type-level name for a window within some containing sequence
--
-- * @lbl@ is a name component that can be used in multiple names
-- * @skolem@ is a component to differentiate between names which use the
--   same @lbl@
--
-- TODO: rename Win to WinLabel
data Win (lbl :: klbl) (skolem :: Type)

-- | Values of this type describe a window in a sequence of elements.
--
-- A window is an infix of the sequence, and it is described with an
-- offset and a length or size (the number of elements in the window).
--
-- * @elem@ is a type-level name of the elements in the containing sequence (e.g. 'Test.Ouroboros.Consensus.ChainGenerator.Slot.SlotE')
-- * @outer@ is a type-level name identifying the containing sequence (e.g. @Win (Lbl HonestLbl) skolem1@)
-- * @inner@ is a type-level name for the window that the value describes (e.g. @Win (Lbl ScgLbl) skolem2@)
--
-- Note that nothing is said about the containing sequence other
-- than its type name.
--
-- TODO: rename Contains to Window
data Contains (elem :: kelem) (outer :: Type) (inner :: Type)
  = UnsafeContains
      -- | index of the start of the window as
      -- an offset in the containing sequence.
      !(Index outer elem)
      -- | size of the window
      -- INVARIANT: does not reach past the end of the containing
      -- sequence (whatever that end is)
      !(Size inner elem)
  deriving (Eq, Read, Show)

pattern Contains :: Index outer elem -> Size inner elem -> Contains elem outer inner
pattern Contains x y <- UnsafeContains x y

{-# COMPLETE Contains #-}

forgetWindow :: Contains elem outer inner -> Some.Forgotten (Index outer elem, Index outer elem)
forgetWindow win = Some.forgotten (windowStart win, windowLast win)

-- | Converts an index of a window into an index in the containing sequence.
fromWindow :: Contains elem outer inner -> Index inner elem -> Index outer elem
fromWindow (Contains (Count i) _n) (Count j) = Count (i .+ j)

-- | Converts a count of elements in a window to a count of elements in the
-- containing sequence.
fromWindowVar :: Contains elem outer inner -> Var inner x -> Var outer x
fromWindowVar _ (Count x) = Count x

toWindow :: Contains elem outer inner -> Index outer elem -> Maybe (Index inner elem)
{-# INLINE toWindow #-}
toWindow (Contains (Count i) (Count n)) (Count j) = if i <= j && j < i .+ n then Just (Count (j .- i)) else Nothing

toWindowVar :: Contains elem outer inner -> Var outer x -> Var inner x
toWindowVar _ (Count x) = Count x

windowSize :: Contains elem outer inner -> Size inner elem
windowSize (Contains _i (Count n)) = Count n

windowStart :: Contains elem outer inner -> Index outer elem
windowStart win = fromWindow win (Count 0)

windowLast :: Contains elem outer inner -> Index outer elem
windowLast win = fromWindow win $ lastIndex $ windowSize win

truncateWin :: Contains elem outer inner -> Size inner elem -> Contains elem outer inner
truncateWin (UnsafeContains start len) x = UnsafeContains start (min len x)

-- | 'Contains' is a 'Data.Semigroupoid.Semigroupoid'
joinWin :: Contains elem outer mid -> Contains elem mid inner -> Contains elem outer inner
{-# INLINE joinWin #-}
joinWin win win2 = UnsafeContains (fromWindow win $ windowStart win2) (windowSize win2)

data SomeWindow (lbl :: klbl) (outer :: Type) (elem :: kelem)
  = forall (skolem :: Type).
    SomeWindow
      !(Proxy skolem)
      !(Contains elem outer (Win lbl skolem))

instance Eq (SomeWindow lbl outer elem) where
  SomeWindow _l1 l2 == SomeWindow _r1 r2 =
    forgetWindow l2 == forgetWindow r2

instance Show (SomeWindow lbl outer elem) where
  showsPrec p (SomeWindow prx win) =
    Some.runShowsPrec p $
      Some.showCtor SomeWindow "SomeWindow"
        `Some.showArg` prx
        `Some.showArg` win

instance Read (SomeWindow lbl outer elem) where
  readPrec =
    Some.runReadPrec $
      Some.readCtor SomeWindow "SomeWindow"
        <*> Some.readArg
        <*> Some.readArg

-- | @withWindow outerSz lbl offset innerSz@ is a window of length @innerSz@
-- with name @lbl@ starting at @offset@ in a sequence with length @outerSz@.
--
-- If the window doesn't fit in the containing sequence, it is clipped so the
-- resulting (possibly empty) window is contained.
--
-- Note that the window can spill either on the right if @i + innerSz > outerSz@,
-- or it can spill on the left if @i < 0@, or it can spill on both sides
-- simultaneously.
withWindow ::
  Size outer elem -> Lbl lbl -> Index outer elem -> Size x elem -> SomeWindow lbl outer elem
withWindow (Count n) _lbl (Count i) (Count m) =
  SomeWindow Proxy $ UnsafeContains (Count i') (Count m')
 where
  i' = min n (max 0 i)

  -- we compute the elements that fall outside the containing sequence
  precedingElements = i' .- i
  trailingElements = max 0 $ i .+ m .- n

  m' = max 0 $ m .- precedingElements .- trailingElements

-- | @withWindowBetween outerSz lbl i j@ is the window between indices @i@
-- and @j@ with name @lbl@ in a containing sequence of length @outerSz@.
withWindowBetween ::
  Size outer elem -> Lbl lbl -> Index outer elem -> Index outer elem -> SomeWindow lbl outer elem
withWindowBetween n lbl (Count i) (Count j) = withWindow n lbl (Count i) (Count $ j .- i .+ 1)

-- | @withSuffixWindow outerSz lbl i@ is the window between indices @i@ and the
-- end of the containing sequence of length @outerSz@ with name @lbl@.
withSuffixWindow :: Size outer elem -> Lbl lbl -> Index outer elem -> SomeWindow lbl outer elem
withSuffixWindow n lbl i = withWindow n lbl i (Count $ getCount n .- getCount i)

-- | @withTopWindow lbl sz k@ passes to @k@ a window of size @sz@ with name
-- @lbl@ at offset @0@ of some containing sequence with a unique name @base@.
withTopWindow ::
  Lbl lbl ->
  Int ->
  (forall base. Proxy base -> SomeWindow lbl base elem -> ans) ->
  ans
withTopWindow _lbl n k =
  k Proxy $ SomeWindow Proxy $ UnsafeContains (Count 0) (Count n)

-----

-- | Same indices as 'Index' and 'Size'
newtype Vector base elem a = Vector (V.Vector a)
  deriving (Eq, Read, Show)

instance (QC.Arbitrary a, V.Unbox a) => QC.Arbitrary (Vector base elem a) where
  arbitrary = (Vector . V.fromList) <$> QC.arbitrary
  shrink = map (Vector . V.fromList) . QC.shrink . V.toList . getVector

getVector :: Vector base elem a -> V.Vector a
getVector (Vector v) = v

lengthV :: V.Unbox a => Vector base elem a -> Size base elem
lengthV = Count . V.length . getVector

sliceV :: MV.Unbox a => Contains elem outer inner -> Vector outer elem a -> Vector inner elem a
{-# INLINE sliceV #-}
sliceV win (Vector v) =
  Vector $ V.slice i n v
 where
  Count i = fromWindow win (Count 0)
  Count n = windowSize win

unsafeThawV :: MV.Unbox a => Vector base elem a -> ST s (MVector base elem s a)
unsafeThawV (Vector v) = MVector <$> V.unsafeThaw v

createV :: MV.Unbox a => (forall s. ST s (MVector base elem s a)) -> Vector base elem a
createV m = Vector $ V.create (getMVector <$> m)

-- | A type-indexed vector carrying values of a container
--
-- * @base@ is a type-level name identifying the container (e.g. @Win (Lbl HonestLbl) skolem1@)
-- * @elem@ is a type-level name of the elements in the container (e.g. 'Test.Ouroboros.Consensus.ChainGenerator.Slot.SlotE')
newtype MVector base elem s a = MVector (MV.MVector s a)

getMVector :: MVector base elem s a -> MV.MVector s a
getMVector (MVector mv) = mv

lengthMV :: MV.Unbox a => MVector base elem s a -> Size base elem
lengthMV = Count . MV.length . getMVector

sliceMV ::
  MV.Unbox a => Contains elem outer inner -> MVector outer elem s a -> MVector inner elem s a
{-# INLINE sliceMV #-}
sliceMV win (MVector mv) =
  MVector $ MV.slice i n mv
 where
  Count i = fromWindow win (Count 0)
  Count n = windowSize win

replicateMV :: MV.Unbox a => Size base elem -> ST s a -> ST s (MVector base elem s a)
replicateMV (Count n) m = fmap MVector $ MV.replicateM n m

readMV :: MV.Unbox a => MVector base elem s a -> Index base elem -> ST s a
writeMV :: MV.Unbox a => MVector base elem s a -> Index base elem -> a -> ST s ()
modifyMV :: MV.Unbox a => MVector base elem s a -> (a -> a) -> Index base elem -> ST s ()
readMV (MVector mv) (Count i) = MV.read mv i
writeMV (MVector mv) (Count i) x = MV.write mv i x
modifyMV (MVector mv) f (Count i) = MV.modify mv f i

readV :: MV.Unbox a => Vector base elem a -> Index base elem -> a
readV (Vector v) (Count i) = v V.! i

-----

-- | A type-level name for counting elements without a specific property
data Other

deriving instance which TypeEq.~~ Other => Enum (Count base elem which)
deriving instance which TypeEq.~~ Other => Num (Count base elem which)

type Var base elem = Count base elem Other

-- | For initializing a 'Var'
toVar :: Count base elem which -> Var base elem
toVar (Count n) = Count n

-- | When the 'Var' has become a 'Size'
toSize :: Var base elem -> Size base elem
toSize (Count n) = Count n

-- | When the 'Var' has become a 'Index'
toIndex :: Var base elem -> Index base elem
toIndex (Count i) = Count i

-- | A count of things in an element can be lifted to a count of things in a vector
joinVar :: MVector base elem s a -> Var a x -> Var base x
joinVar _ = \(Count n) -> Count n
