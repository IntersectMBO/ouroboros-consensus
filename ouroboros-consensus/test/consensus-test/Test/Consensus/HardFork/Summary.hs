{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Tests for the hard fork summary.
--
-- This module verifies the property that /no matter how the summary is
-- constructed/, as long as it satisfies its invariants, we should have
-- roundtrip properties:
--
-- * Converting time to a slot and then back to time should be an identity
--   (modulo the time spent in that slot).
-- * Converting a slot to time and then back should be an identity.
-- * Converting slot to an epoch and then back to a slot should be an identity
--   (modulo the time spent in that epoch).
-- * Converting an epoch to a slot and then back should be an identity.
-- * Converting a Peras round number to a slot and then back should be an identity.
module Test.Consensus.HardFork.Summary (tests) where

import Data.Time
import Data.Word
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime
import qualified Ouroboros.Consensus.HardFork.History as HF
import Test.Consensus.HardFork.Infra
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.Orphans.Arbitrary ()
import Test.Util.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Summary"
    [ testGroup
        "Sanity"
        [ testProperty "generator" $ checkGenerator $ \ArbitrarySummary{..} ->
            checkInvariant HF.invariantSummary arbitrarySummary
        , testProperty "shrinker" $ checkShrinker $ \ArbitrarySummary{..} ->
            checkInvariant HF.invariantSummary arbitrarySummary
        ]
    , testGroup
        "Conversions"
        [ testProperty "roundtripWallclockSlot" roundtripWallclockSlot
        , testProperty "roundtripSlotWallclock" roundtripSlotWallclock
        , testProperty "roundtripSlotEpoch" roundtripSlotEpoch
        , testProperty "roundtripEpochSlot" roundtripEpochSlot
        , testProperty "roundtripPerasRoundSlot" roundtripPerasRoundSlot
        , testProperty "reportsPastHorizonWallclockToSlot" reportsPastHorizonWallclockToSlot
        , testProperty "reportsPastHorizonSlotToWallclock" reportsPastHorizonSlotToWallclock
        , testProperty "reportsPastHorizonSlotToEpoch" reportsPastHorizonSlotToEpoch
        , testProperty "reportsPastHorizonEpochToSlot" reportsPastHorizonEpochToSlot
        , testProperty "reportsPastHorizonPerasRoundNoToSlot" reportsPastHorizonPerasRoundNoToSlot
        ]
    ]

{-------------------------------------------------------------------------------
  Dealing with the 'PastHorizonException'
-------------------------------------------------------------------------------}

noPastHorizonException ::
  ArbitrarySummary ->
  HF.Qry Property ->
  Property
noPastHorizonException ArbitrarySummary{..} p =
  case HF.runQuery p arbitrarySummary of
    Right prop -> prop
    Left ex ->
      counterexample ("Unexpected " ++ show ex) $
        property False

isPastHorizonException ::
  Show a =>
  ArbitrarySummary ->
  HF.Qry a ->
  Property
isPastHorizonException ArbitrarySummary{..} ma =
  case HF.runQuery ma arbitrarySummary of
    Left _ -> property True
    Right a ->
      counterexample ("Unexpected " ++ show a) $
        property False

{-------------------------------------------------------------------------------
  Tests using just 'Summary'
-------------------------------------------------------------------------------}

roundtripWallclockSlot :: ArbitrarySummary -> Property
roundtripWallclockSlot s@ArbitrarySummary{beforeHorizonTime = time} =
  noPastHorizonException s $ do
    (slot, inSlot, timeLeft) <- HF.wallclockToSlot time
    (time', slotLen) <- HF.slotToWallclock slot
    return $
      conjoin
        [ addRelTime inSlot time' === time
        , inSlot + timeLeft === getSlotLength slotLen
        ]

roundtripSlotWallclock :: ArbitrarySummary -> Property
roundtripSlotWallclock s@ArbitrarySummary{beforeHorizonSlot = slot} =
  noPastHorizonException s $ do
    (time, slotLen) <- HF.slotToWallclock slot
    (slot', inSlot, timeLeft) <- HF.wallclockToSlot time
    return $
      conjoin
        [ slot' === slot
        , inSlot === 0
        , inSlot + timeLeft === getSlotLength slotLen
        ]

roundtripSlotEpoch :: ArbitrarySummary -> Property
roundtripSlotEpoch s@ArbitrarySummary{beforeHorizonSlot = slot} =
  noPastHorizonException s $ do
    (epoch, inEpoch, slotsLeft) <- HF.slotToEpoch slot
    (slot', epochSize) <- HF.epochToSlot epoch
    return $
      conjoin
        [ HF.addSlots inEpoch slot' === slot
        , inEpoch + slotsLeft === unEpochSize epochSize
        ]

roundtripEpochSlot :: ArbitrarySummary -> Property
roundtripEpochSlot s@ArbitrarySummary{beforeHorizonEpoch = epoch} =
  noPastHorizonException s $ do
    (slot, epochSize) <- HF.epochToSlot epoch
    (epoch', inEpoch, slotsLeft) <- HF.slotToEpoch slot
    return $
      conjoin
        [ epoch' === epoch
        , inEpoch === 0
        , inEpoch + slotsLeft === unEpochSize epochSize
        ]

-- | Test that conversion between Peras rounds and slots roundtips.
--   Additionally, test that the relative slot in round and remaining
--   slots in round are withing the round length.
roundtripPerasRoundSlot :: ArbitrarySummary -> Property
roundtripPerasRoundSlot ArbitrarySummary{mBeforeHorizonPerasRoundNo = Nothing} =
  discard
roundtripPerasRoundSlot
  s@ArbitrarySummary{mBeforeHorizonPerasRoundNo = Just perasRoundNo} =
    noPastHorizonException s $
      HF.perasRoundNoToSlot perasRoundNo >>= \case
        HF.NoPerasEnabled ->
          pure $ counterexample "perasRoundNoToSlot returned NoPerasEnabled" False
        HF.PerasEnabled (slot, PerasRoundLength perasRoundLength) -> do
          HF.slotToPerasRoundNo slot >>= \case
            HF.NoPerasEnabled ->
              pure $ counterexample "slotToPerasRoundNo returned NoPerasEnabled" False
            HF.PerasEnabled (perasRoundNo', slotInRound, remainingSlotsInRound) ->
              pure $
                conjoin
                  [ perasRoundNo' === perasRoundNo
                  , slotInRound `lt` perasRoundLength
                  , remainingSlotsInRound `le` perasRoundLength
                  ]

reportsPastHorizonWallclockToSlot :: ArbitrarySummary -> Property
reportsPastHorizonWallclockToSlot s@ArbitrarySummary{..} =
  case mPastHorizonTime of
    Just x -> isPastHorizonException s $ HF.wallclockToSlot x
    Nothing -> discard

reportsPastHorizonSlotToWallclock :: ArbitrarySummary -> Property
reportsPastHorizonSlotToWallclock s@ArbitrarySummary{..} =
  case mPastHorizonSlot of
    Just x -> isPastHorizonException s $ HF.slotToWallclock x
    Nothing -> discard

reportsPastHorizonSlotToEpoch :: ArbitrarySummary -> Property
reportsPastHorizonSlotToEpoch s@ArbitrarySummary{..} =
  case mPastHorizonSlot of
    Just x -> isPastHorizonException s $ HF.slotToEpoch x
    Nothing -> discard

reportsPastHorizonEpochToSlot :: ArbitrarySummary -> Property
reportsPastHorizonEpochToSlot s@ArbitrarySummary{..} =
  case mPastHorizonEpoch of
    Just x -> isPastHorizonException s $ HF.epochToSlot x
    Nothing -> discard

reportsPastHorizonPerasRoundNoToSlot :: ArbitrarySummary -> Property
reportsPastHorizonPerasRoundNoToSlot s@ArbitrarySummary{..} =
  case mPastHorizonPerasRoundNo of
    Just x -> isPastHorizonException s $ HF.perasRoundNoToSlot x
    Nothing -> discard

{-------------------------------------------------------------------------------
  Arbitrary 'Summary'

  We should be able to show properties of the conversion functions independent
  of how the 'Summary' that they use is derived.
-------------------------------------------------------------------------------}

data ArbitrarySummary = forall xs. ArbitrarySummary
  { arbitrarySummary :: HF.Summary xs
  , beforeHorizonTime :: RelativeTime
  , beforeHorizonSlot :: SlotNo
  , beforeHorizonEpoch :: EpochNo
  , mBeforeHorizonPerasRoundNo :: Maybe PerasRoundNo
  , mPastHorizonTime :: Maybe RelativeTime
  , mPastHorizonSlot :: Maybe SlotNo
  , mPastHorizonEpoch :: Maybe EpochNo
  , mPastHorizonPerasRoundNo :: Maybe PerasRoundNo
  }

deriving instance Show ArbitrarySummary

instance Arbitrary ArbitrarySummary where
  arbitrary = chooseEras $ \is@(Eras _) -> do
    summary <- genSummary is

    let summaryStart :: HF.Bound
        mSummaryEnd :: HF.EraEnd
        (summaryStart, mSummaryEnd) = HF.summaryBounds summary

    -- Genuine Peras bounds cannot be computed in the same way as regular slot
    -- bounds using 'HF.summaryBounds', because some eras may not have Peras
    -- enabled. Instead, we rely on 'HF.summaryPerasBounds' and derive arbitrary
    -- before and past horizon round numbers from it.
    (mBeforeHorizonPerasRoundNo, mPastHorizonPerasRoundNo) <- do
      case HF.summaryPerasBounds summary of
        -- No era supports Peras, so every round number is past the horizon.
        NoPerasEnabled -> do
          pastHorizonPerasRound :: Word64 <- choose (0, 1_000)
          pure
            ( Nothing
            , Just (PerasRoundNo pastHorizonPerasRound)
            )
        -- Peras is enabled for /at least/ the (last unbounded) era, so there is
        -- no 'PerasRoundNo' that is past the horizon.
        PerasEnabled (summaryPerasStart, Nothing) -> do
          beforeHorizonNextPerasRounds <- choose (0, 1_000)
          pure
            ( Just (HF.addPerasRounds beforeHorizonNextPerasRounds summaryPerasStart)
            , Nothing
            )
        -- Peras is enabled for /at least/ one era, where the last one of those
        -- is bounded. Therefore, we have a definite horizon for 'PerasRoundNo'.
        PerasEnabled (summaryPerasStart, Just summaryPerasEnd) -> do
          let summaryPerasRounds = HF.countPerasRounds summaryPerasEnd summaryPerasStart
          beforeHorizonPerasRounds <- choose (0, summaryPerasRounds - 1)
          pastHorizonPerasRounds :: Word64 <- choose (0, 10)
          pure
            ( Just (HF.addPerasRounds beforeHorizonPerasRounds summaryPerasStart)
            , Just (HF.addPerasRounds pastHorizonPerasRounds summaryPerasEnd)
            )

    case mSummaryEnd of
      HF.EraUnbounded -> do
        -- Don't pick /too/ large numbers to avoid overflow
        beforeHorizonSlots <- choose (0, 100_000_000)
        beforeHorizonEpochs <- choose (0, 1_000_000)
        beforeHorizonSeconds <- choose (0, 1_000_000_000)

        let beforeHorizonSlot :: SlotNo
            beforeHorizonEpoch :: EpochNo
            beforeHorizonTime :: RelativeTime

            beforeHorizonSlot =
              HF.addSlots
                beforeHorizonSlots
                (HF.boundSlot summaryStart)
            beforeHorizonEpoch =
              HF.addEpochs
                beforeHorizonEpochs
                (HF.boundEpoch summaryStart)
            beforeHorizonTime =
              addRelTime
                (realToFrac (beforeHorizonSeconds :: Double))
                (HF.boundTime summaryStart)
        return
          ArbitrarySummary
            { arbitrarySummary = summary
            , beforeHorizonTime
            , beforeHorizonSlot
            , beforeHorizonEpoch
            , mBeforeHorizonPerasRoundNo
            , mPastHorizonTime = Nothing
            , mPastHorizonSlot = Nothing
            , mPastHorizonEpoch = Nothing
            , mPastHorizonPerasRoundNo = Nothing
            }
      HF.EraEnd summaryEnd -> do
        let summarySlots, summaryEpochs :: Word64
            summarySlots =
              HF.countSlots
                (HF.boundSlot summaryEnd)
                (HF.boundSlot summaryStart)
            summaryEpochs =
              HF.countEpochs
                (HF.boundEpoch summaryEnd)
                (HF.boundEpoch summaryStart)
            summaryTimeSpan :: NominalDiffTime
            summaryTimeSpan =
              diffRelTime
                (HF.boundTime summaryEnd)
                (HF.boundTime summaryStart)

            summaryTimeSpanSeconds :: Double
            summaryTimeSpanSeconds = realToFrac summaryTimeSpan

        -- Pick arbitrary values before the horizon

        beforeHorizonSlots <- choose (0, summarySlots - 1)
        beforeHorizonEpochs <- choose (0, summaryEpochs - 1)
        beforeHorizonSeconds <-
          choose (0, summaryTimeSpanSeconds)
            `suchThat` \x -> x /= summaryTimeSpanSeconds
        let beforeHorizonSlot :: SlotNo
            beforeHorizonEpoch :: EpochNo
            beforeHorizonTime :: RelativeTime

            beforeHorizonSlot =
              HF.addSlots
                beforeHorizonSlots
                (HF.boundSlot summaryStart)
            beforeHorizonEpoch =
              HF.addEpochs
                beforeHorizonEpochs
                (HF.boundEpoch summaryStart)
            beforeHorizonTime =
              addRelTime
                (realToFrac beforeHorizonSeconds)
                (HF.boundTime summaryStart)

        -- Pick arbitrary values past the horizon

        pastHorizonSlots :: Word64 <- choose (0, 10)
        pastHorizonEpochs :: Word64 <- choose (0, 10)
        pastHorizonSeconds :: Double <- choose (0, 10)

        let pastHorizonSlot :: SlotNo
            pastHorizonEpoch :: EpochNo
            pastHorizonTime :: RelativeTime

            pastHorizonSlot =
              HF.addSlots
                pastHorizonSlots
                (HF.boundSlot summaryEnd)
            pastHorizonEpoch =
              HF.addEpochs
                pastHorizonEpochs
                (HF.boundEpoch summaryEnd)
            pastHorizonTime =
              addRelTime
                (realToFrac pastHorizonSeconds)
                (HF.boundTime summaryEnd)
        return
          ArbitrarySummary
            { arbitrarySummary = summary
            , beforeHorizonTime
            , beforeHorizonSlot
            , beforeHorizonEpoch
            , mBeforeHorizonPerasRoundNo
            , mPastHorizonTime = Just pastHorizonTime
            , mPastHorizonSlot = Just pastHorizonSlot
            , mPastHorizonEpoch = Just pastHorizonEpoch
            , mPastHorizonPerasRoundNo = mPastHorizonPerasRoundNo
            }

  shrink summary@ArbitrarySummary{..} =
    concat
      [ -- Reduce before-horizon slot
        [ summary{beforeHorizonSlot = SlotNo s}
        | s <- shrink (unSlotNo beforeHorizonSlot)
        ]
      , -- Reduce before-horizon epoch
        [ summary{beforeHorizonEpoch = EpochNo e}
        | e <- shrink (unEpochNo beforeHorizonEpoch)
        ]
      , -- Reduce before-horizon time
        [ summary{beforeHorizonTime = RelativeTime t}
        | t <- shrink (getRelativeTime beforeHorizonTime)
        , t >= 0
        ]
      , -- Drop an era /provided/ this doesn't cause of any of the before
        -- horizon values to become past horizon
        [ ArbitrarySummary{arbitrarySummary = summary', ..}
        | (Just summary', lastEra) <- [HF.summaryInit arbitrarySummary]
        , beforeHorizonSlot < HF.boundSlot (HF.eraStart lastEra)
        , beforeHorizonEpoch < HF.boundEpoch (HF.eraStart lastEra)
        , beforeHorizonTime < HF.boundTime (HF.eraStart lastEra)
        ]
      ]
