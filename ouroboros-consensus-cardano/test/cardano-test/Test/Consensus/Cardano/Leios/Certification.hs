{-# LANGUAGE NumericUnderscores #-}

-- | When a ranking block can certify the endorser block that its parent
-- announced.
module Test.Consensus.Cardano.Leios.Certification (tests) where

import Cardano.Ledger.BaseTypes (Milliseconds32 (..))
import Cardano.Slotting.Slot (SlotNo (..))
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( SlotLength
  , getSlotLength
  , slotLengthFromMillisec
  , slotLengthFromSec
  )
import Ouroboros.Consensus.Leios.Types
  ( LeiosPeriods (..)
  , certificationGapElapsed
  )
import Test.Tasty
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Leios certification"
    [ testCase
        "the CIP's feasible values give a gap of 14 slots"
        test_cipFeasibleValuesGive14SlotGap
    , testProperty
        "the first allowed slot rounds up"
        prop_firstAllowedSlotRoundsUp
    ]

-- | The worked example in the "Feasible Protocol Parameters" section of
-- CIP-0164. With @L_hdr@ = 1 s, @L_vote@ = 4 s, @L_diff@ = 7 s and 1 s slots,
-- the CIP says that a certificate can be included at least 14 slots after the
-- announcing block.
--
-- This test finds errors in the formula itself.
-- 'prop_firstAllowedSlotRoundsUp' compares 'certificationGapElapsed' with a
-- second copy of the formula. If both copies have the same error, the property
-- still passes. An example is the factor 3 on @L_vote@ in place of @L_hdr@.
-- This test does not use the formula. Its expected value is the 14 that the
-- CIP states.
test_cipFeasibleValuesGive14SlotGap :: Assertion
test_cipFeasibleValuesGive14SlotGap =
  map gapElapsed [113, 114] @?= [False, True]
 where
  gapElapsed =
    certificationGapElapsed (slotLengthFromSec 1) cipPeriods (SlotNo 100)
  cipPeriods =
    LeiosPeriods
      { announcementPeriod = Milliseconds32 1_000
      , votePeriod = Milliseconds32 4_000
      , diffusionPeriod = Milliseconds32 7_000
      }

-- | The first slot that can certify the EB is
-- @rbSlot + ceiling (t / slotLength)@, and every later slot can certify it too.
--
-- The generated periods are rarely a whole number of slots, so this property
-- fails if 'certificationGapElapsed' rounds down.
prop_firstAllowedSlotRoundsUp :: Property
prop_firstAllowedSlotRoundsUp =
  forAll genPeriods $ \periods ->
    forAll genSlotLength $ \slotLength ->
      forAll (SlotNo <$> choose (0, 1_000_000)) $ \rbSlot ->
        forAll (choose (0, 1_000)) $ \laterBy ->
          let gapElapsed = certificationGapElapsed slotLength periods rbSlot
              gapSlots =
                ceiling (cipTotal periods / toRational (getSlotLength slotLength))
              firstSlotThatCanCertify = rbSlot + SlotNo gapSlots
           in counterexample ("first allowed slot: " <> show firstSlotThatCanCertify) $
                conjoin
                  [ counterexample "the slot before the first allowed slot can certify" $
                      gapSlots == 0 || not (gapElapsed (firstSlotThatCanCertify - 1))
                  , counterexample "the first allowed slot cannot certify" $
                      gapElapsed firstSlotThatCanCertify
                  , counterexample "a later slot cannot certify" $
                      gapElapsed (firstSlotThatCanCertify + SlotNo laterBy)
                  ]

-- | @3 * L_hdr + L_vote + L_diff@ in seconds, as CIP-0164 writes it.
cipTotal :: LeiosPeriods -> Rational
cipTotal periods =
  ( 3 * ms (announcementPeriod periods)
      + ms (votePeriod periods)
      + ms (diffusionPeriod periods)
  )
    / 1000
 where
  ms = toRational . unMilliseconds32

-- | Each period is zero, the size of the CIP's values, or any 'Milliseconds32'.
-- If all three periods are zero, the gap is 0 slots. The large values make
-- @3 * L_hdr@ larger than the maximum 'Word32', so the property fails if
-- 'certificationGapElapsed' computes the sum in 'Word32'.
genPeriods :: Gen LeiosPeriods
genPeriods = LeiosPeriods <$> genPeriod <*> genPeriod <*> genPeriod
 where
  genPeriod =
    Milliseconds32
      <$> frequency
        [ (1, pure 0)
        , (2, choose (1, 20_000))
        , (1, choose (0, maxBound))
        ]

genSlotLength :: Gen SlotLength
genSlotLength = slotLengthFromMillisec <$> choose (1, 5_000)
