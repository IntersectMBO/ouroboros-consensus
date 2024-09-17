{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Ouroboros.Consensus.ChainGenerator.BitVector (
    -- * Finding
    MaybeFound (JustFound, NothingFound)
  , findIthActiveInV
  , findIthEmptyInMV
  , findIthEmptyInV
    -- * Counting
  , countActivesInMV
  , countActivesInV
    -- * Slots
  , setMV
  , testMV
  , testV
    -- * Generating
  , SomeDensityWindow (SomeDensityWindow)
  , fillInWindow
  ) where

import           Control.Monad.ST (ST, runST)
import           Data.Functor ((<&>))
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified System.Random.Stateful as R
import qualified Test.Ouroboros.Consensus.ChainGenerator.Counting as C
import qualified Test.Ouroboros.Consensus.ChainGenerator.Slot as S
import           Test.Ouroboros.Consensus.ChainGenerator.Slot
                     (E (ActiveSlotE, EmptySlotE, SlotE), POL, PreImage, S)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Some as Some

-----

data MaybeFound base =
    NothingFound
  |
    JustFound
      {-# UNPACK #-} !(C.Index base SlotE)
  deriving (Eq, Read, Show)

-- | Trivial wrapper around 'findIthEmptyInMV'
findIthEmptyInV ::
     POL   pol
  => proxy pol
  -> C.Vector base SlotE S
  -> C.Index base (PreImage pol EmptySlotE)
  -> MaybeFound base
findIthEmptyInV pol v i =
    runST $ C.unsafeThawV v >>= \mv -> findIthEmptyInMV pol mv i

-- | Find the (i+1)st empty slot in a window
--
-- * @findIthEmptyInMV notInverted v 0@ yields the first empty slot
-- * @findIthEmptyInMV notInverted v 1@ yields the second empty slot
-- * @findIthEmptyInMV notInverted v k@ yields the @k+1@st empty slot
--
-- > findIthEmptyInMV notInverted 01101 0 == JustFound 0
-- > findIthEmptyInMV notInverted 01101 1 == JustFound 3
-- > findIthEmptyInMV notInverted 01101 2 == NothingFound
--
findIthEmptyInMV ::
  forall proxy pol base s.
     POL   pol
  => proxy pol
  -> C.MVector base SlotE s S
  -> C.Index base (PreImage pol EmptySlotE)
  -> ST s (MaybeFound base)
findIthEmptyInMV pol mv i =
    go 0 (C.toVar i)
  where
    go !j !toSkip = if C.getCount (C.lengthMV mv) <= j then pure NothingFound else do
        w <- C.readMV mv (C.Count j)
        if
          | S.test pol w -> go (j + 1) toSkip
          | 0 == toSkip  -> pure $ JustFound (C.Count j)
          | otherwise    -> go (j + 1) (toSkip - 1)

findIthActiveInV ::
     C.Vector base SlotE S
  -> C.Index base ActiveSlotE
  -> MaybeFound base
findIthActiveInV =
    findIthEmptyInV S.inverted

-----

-- | Trivial wrapper around 'countActivesInMV'
countActivesInV ::
     POL   pol
  => proxy pol
  -> C.Vector base SlotE S
  -> C.Size base (PreImage pol ActiveSlotE)
countActivesInV pol v =
    C.toSize $ runST $ C.unsafeThawV v >>= \mv -> countActivesInMV pol mv

-- | The number of active slots in the vector
countActivesInMV ::
     POL   pol
  => proxy pol
  -> C.MVector base SlotE s S
  -> ST s (C.Var base (PreImage pol ActiveSlotE))
countActivesInMV pol mv =
    MV.foldl'
        (\acc w -> if S.test pol w then acc + 1 else acc)
        0
        mv'
  where
    C.MVector mv' = mv

-----

-- | A density of active slots in a given window
--
-- @pol@ is the polarity to use for the active slots
--
-- TODO: rename to SomeDensity
data SomeDensityWindow pol =
  forall slidingWindow.
    SomeDensityWindow
        !(C.Var  slidingWindow (PreImage pol ActiveSlotE)) -- ^ Numerator: The active slots
        !(C.Size slidingWindow SlotE)                      -- ^ Denominator: The total amount of slots

instance Eq (SomeDensityWindow pol) where
    SomeDensityWindow l1 l2 == SomeDensityWindow r1 r2 =
      C.forgetBase l1 == C.forgetBase r1 && C.forgetBase l2 == C.forgetBase r2

instance Show (SomeDensityWindow pol) where
    showsPrec p (SomeDensityWindow numer denom) =
        Some.runShowsPrec p
      $ Some.showCtor (SomeDensityWindow @pol) "SomeDensityWindow"
            `Some.showArg` numer
            `Some.showArg` denom

instance Read (SomeDensityWindow pol) where
    readPrec =
        Some.runReadPrec
      $ Some.readCtor SomeDensityWindow "SomeDensityWindow"
            <*> Some.readArg
            <*> Some.readArg

-- | @fillInWindow pol (SomeDensityWindow k s) g mv@ mutates @mv@ to ensure
-- that the vector @take s $ mv ++ repeat (mkActive pol)@ has at least @k@
-- slots polarizely active.
--
-- Preconditions:
--
-- > lengthMV mv <= s
-- > k <= s
--
fillInWindow ::
  forall proxy pol base g s.
     (POL pol, R.StatefulGen g (ST s))
  => proxy pol
  -> SomeDensityWindow pol
  -> g
  -> C.MVector base SlotE s S
  -> ST s (C.Var base (PreImage pol ActiveSlotE))   -- ^ the count after filling
fillInWindow pol (SomeDensityWindow k s) g mv
        | not (C.getCount k <= C.getCount s) =
            error $ "fillInWindow: assertion failure: k <= s: "
                    ++ show k ++ " <= " ++ show s
        | not (C.getCount sz <= C.getCount s) =
            error $ "fillInWindow: assertion failure: sz <= s: "
                    ++ show sz ++ " <= " ++ show s
        | otherwise = do
    -- how many active polarized slots @actual@ currently has
    initialActives <- countActivesInMV pol mv


    -- discount the numerator accordingly if @mv@ is smaller than @s@
    --
    -- EG when a full-size @mv@ would reach past the 'Len'.
    --
    -- This discount reflects that we (very conservatively!) assume every
    -- truncated slot would be an active polarized slot.
    let discountedK :: C.Var base (PreImage pol ActiveSlotE)
        discountedK = C.Count $ C.getCount k - (C.getCount s - C.getCount sz)

    -- how many active polarized slots need to be added to @mv@
    let adding = max 0 $ C.toVar discountedK - initialActives :: C.Var base (PreImage pol ActiveSlotE)

    -- draw from the empty polarized slots uniformly without replacement, a la Fisher-Yates shuffle
    C.forRange_ (C.toSize adding) $ \alreadyAdded -> do
        let currentActives = C.toSize $ initialActives + C.toVar alreadyAdded
            currentEmpties = S.complementActive pol sz currentActives

        whichEmptyToFlip <- C.uniformIndex currentEmpties g

        slot <- findIthEmptyInMV pol mv whichEmptyToFlip <&> \case
            JustFound i  -> i
            NothingFound -> error "impossible! fillInWindow"

        setMV pol mv slot

    pure $ initialActives + adding
  where
    sz = C.lengthMV mv :: C.Size base SlotE

-----

testV :: POL pol => proxy pol -> C.Vector base SlotE S -> C.Index base SlotE -> Bool
testV pol mv i = S.test pol (C.readV mv i)

testMV :: POL pol => proxy pol -> C.MVector base SlotE s S -> C.Index base SlotE -> ST s Bool
testMV pol mv i = do
     w <- C.readMV mv i
     pure $ S.test pol w

setMV :: POL pol => proxy pol -> C.MVector base SlotE s S -> C.Index base SlotE -> ST s ()
setMV pol mv i = C.writeMV mv i $ S.mkActive pol
