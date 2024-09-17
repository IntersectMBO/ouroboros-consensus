{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{- | These functions iteratively produce all race windows in a slot vector.

The first window is produced by 'init', which unconditionally starts the window at the first slot.
This window can then be passed to 'next', which starts the new window after the first active slot.

@
---X--X--X--X-- ...
^ start of window 1 from 'init'
    ^ start of window 2 from 'next'
       ^ start of window 3 from 'next'
@

Valid windows must have @k+1@ active slots.
If the vector doesn't have sufficient slots to meet this condition, 'init' and 'next' return 'Nothing' and we fall back
to 'initConservative' and 'nextConservative', which return windows truncated at the end of time.
-}
module Test.Ouroboros.Consensus.ChainGenerator.RaceIterator (
    Race (Race, UnsafeRace)
  , RaceLbl
  , init
  , initConservative
  , next
  , nextConservative
  ) where

import           Control.Monad (when)
import           Data.Proxy (Proxy (Proxy))
import           Prelude hiding (init)
import qualified Test.Ouroboros.Consensus.ChainGenerator.BitVector as BV
import qualified Test.Ouroboros.Consensus.ChainGenerator.Counting as C
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Kcp (Kcp))
import           Test.Ouroboros.Consensus.ChainGenerator.Slot
                     (E (ActiveSlotE, SlotE), S)

-----

-- | See 'Race'
data RaceLbl

-- | A window whose last slot contains the @k+1@st active slot in it
newtype Race base = UnsafeRace (C.SomeWindow RaceLbl base SlotE)
  deriving (Eq, Read, Show)

pattern Race :: C.SomeWindow RaceLbl base SlotE -> Race base
pattern Race x <- UnsafeRace x

{-# COMPLETE Race #-}

-----

-- | Find the nth active slot /in/ the given race window and return the slot number in the context of the entire chain.
--
-- Race windows are anchored in an active slot, and so could start with an empty or active slot.
nthActiveSlotIndex ::
  forall base adv.
     C.Index adv ActiveSlotE
  -> C.Vector base SlotE S
  -> C.Contains SlotE base adv
  -> Maybe (C.Index base SlotE)
nthActiveSlotIndex n v raceWin =
  -- the given race window has at least k+1 blocks in it and 0<=k, so this pattern can't fail
  -- TODO by invariant during construction of the honest chain?
  case BV.findIthActiveInV (C.sliceV raceWin v) n of
      BV.NothingFound   -> Nothing   -- would be impossible if we never called next after *Conservative
      BV.JustFound slot -> pure $! C.fromWindow raceWin slot

-- | Yields the race window starting at position 0 of the given
-- vector if the @k+1@ active slot exists.
init :: Kcp -> C.Vector base SlotE S -> Maybe (Race base)
init (Kcp k) v =
    -- find the @k+1@st active slot in the given race window
    case BV.findIthActiveInV v (C.Count k) of
        BV.NothingFound       -> Nothing
        BV.JustFound kPlus1st ->
            Just
         $! UnsafeRace
         $  C.withWindowBetween
                (C.lengthV v)
                (C.Lbl @RaceLbl)
                (C.Count 0)
                kPlus1st

-- | @initConservative@ creates a window for the whole vector.
initConservative :: C.Vector base SlotE S -> Race base
initConservative v =
    let sz = C.lengthV v
     in UnsafeRace
          $ C.withWindow
             sz
             (C.Lbl @RaceLbl)
             (C.Count 0)
             (C.Count $ C.getCount sz)

data RaceStepLbl

-- | @next v r@ yields the race window anchored at the first
-- active slot of @r@ if there is an active slot after @r@.
next ::
  forall base.
     C.Vector base SlotE S
  -> Race base
  -> Maybe (Race base)
next v (UnsafeRace (C.SomeWindow Proxy raceWin)) = do
    next0 <- nthActiveSlotIndex (C.Count 0) v raceWin

    -- find the first active slot /after/ the given race window
    --
    -- Race windows end in an active slot.
    nextK <- do
        C.SomeWindow Proxy searchWin <-
            pure
          $ C.withWindowBetween
                sz
                (C.Lbl @RaceStepLbl)
                (C.windowLast raceWin)
                (C.lastIndex sz)
        nthActiveSlotIndex (C.Count 1) v searchWin

    pure $! UnsafeRace $ C.withWindowBetween
        sz
        (C.Lbl @RaceLbl)
        (next0 C.+ 1)
        nextK
  where
    sz = C.lengthV v

-- | @nextConservative v r@ yields a window anchored at the first
-- active slot of @r@ if it exists, and extending until the end of @v@.
nextConservative ::
  forall base.
     C.Vector base SlotE S
  -> Race base
  -> Maybe (Race base)
nextConservative v (UnsafeRace (C.SomeWindow Proxy raceWin)) = do
    next0 <- nthActiveSlotIndex (C.Count 0) v raceWin

    -- do not return a Race Window that starts after 'Len'
    when (next0 == C.lastIndex sz) Nothing

    pure $! UnsafeRace $ C.withWindowBetween
        sz
        (C.Lbl @RaceLbl)
        (next0 C.+ 1)
        (C.lastIndex sz)
  where
    sz = C.lengthV v
