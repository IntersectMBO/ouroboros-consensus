{-# LANGUAGE TypeApplications #-}

-- | Test properties for the weighted Fait-Accompli committee selection model
module Test.Consensus.Committee.WFALS.Model.Test (tests) where

import Data.List (mapAccumR)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Word (Word64)
import Numeric.Natural (minusNaturalMaybe)
import Test.Consensus.Committee.WFALS.Model
  ( IsStake (..)
  , NumSeats
  , Stake (..)
  , StakeDistr
  , StakeRole (..)
  , StakeType (..)
  , VoterId
  , adjustStake
  , coerceStake
  , stakeDistrToDecreasingStakes
  , stakeDistrTotalStake
  , weightedFaitAccompliPersistentSeats
  , weightedFaitAccompliThreshold
  )
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Positive (..)
  , Property
  , Testable (..)
  , choose
  , counterexample
  , forAll
  , sized
  , tabulate
  , vectorOf
  , (===)
  , (==>)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestEnv (adjustQuickCheckTests)

-- * Property tests

-- ** Generators

-- | Generate a random stake as a Rational number
genStake :: Gen (Stake Ledger Global)
genStake = do
  Positive num <- arbitrary
  Positive den <- arbitrary
  pure (StakeLedgerGlobal (num % den))

-- | Generate a non-empty list of unique voter IDs of a given size.
genUniqueVoterIds :: Int -> Gen [VoterId]
genUniqueVoterIds size =
  fmap show <$> go size Set.empty
 where
  go 0 acc =
    return (Set.toList acc)
  go k acc = do
    voterId <- choose (0, maxBound :: Word64)
    if voterId `Set.member` acc
      then
        go k acc
      else do
        rest <- go (k - 1) (Set.insert voterId acc)
        pure (voterId : rest)

-- | Generate a non-empty random stake distribution of a given size
--
-- NOTE: the size is shifted up by one to ensure non-emptiness.
genStakeDistr :: Int -> Gen (StakeDistr Ledger Global)
genStakeDistr size = do
  ids <- genUniqueVoterIds (succ size)
  stakes <- vectorOf (succ size) genStake
  pure (Map.fromList (zip ids stakes))

-- * Property helpers

-- | Helper to generate stake distributions along with a number of seats that
-- lies within the accepatable range [1, #{nodes with positive stake}]
forAllStakeDistrAndNumSeats ::
  (StakeDistr Ledger Global -> NumSeats Global -> Property) ->
  Property
forAllStakeDistrAndNumSeats p =
  forAll (sized genStakeDistr) $ \stakeDistr -> do
    let numPositiveStakeNodes =
          fromIntegral
            . length
            . filter ((> 0) . stakeToRational)
            . Map.elems
            $ stakeDistr
    let genNumSeats =
          fromInteger <$> choose (1, numPositiveStakeNodes)
    forAll genNumSeats $ \numSeats ->
      p stakeDistr numSeats

-- | Tabulate the target number of seats
tabulateTargetNumSeats :: NumSeats Global -> Property -> Property
tabulateTargetNumSeats numSeats =
  tabulate "Target number of seats" [bucket]
 where
  -- Divide in buckets of 10
  bucket
    | lower == upper = show lower
    | otherwise = show lower <> "-" <> show upper
  lower = (numSeats `div` 10) * 10
  upper = lower + 9

-- | Tabulate the size of a stake distribution
tabulateStakeDistrSize :: StakeDistr Ledger Global -> Property -> Property
tabulateStakeDistrSize stakeDistr =
  tabulate "Stake distribution size" [bucket]
 where
  -- Divide in buckets of 10
  bucket
    | lower == upper = show lower
    | otherwise = show lower <> "-" <> show upper
  lower = (size `div` 10) * 10
  upper = lower + 9
  size = Map.size stakeDistr

-- | Tabulate whether we are asking for more seats than nodes in the stake
-- distribution or not
tabulateMoreSeatsThanNodes ::
  StakeDistr Ledger Global ->
  NumSeats Global ->
  Property ->
  Property
tabulateMoreSeatsThanNodes stakeDistr numSeats =
  tabulate
    "More target seats than nodes"
    [ show (numSeats > fromIntegral (Map.size stakeDistr))
    ]

-- | Tabulate the ratio of persistent to non-persistent seats as the percentage
-- of persistent seats out of the total number of seats.
tabulatePersistentToNonPersistentRatio ::
  NumSeats Global ->
  NumSeats Global ->
  Property ->
  Property
tabulatePersistentToNonPersistentRatio
  numPersistentSeats
  numNonPersistentSeats =
    tabulate "Persistent to non-persistent seat ratio" [bucket]
   where
    -- Divide buckets in 10% increments
    bucket
      | lower == upper = show lower <> "%"
      | otherwise = show lower <> "%-" <> show upper <> "%"
    lower = (numPersistentSeats * 100) `div` totalSeats `div` 10 * 10
    upper = min 100 (lower + 10)
    totalSeats = numPersistentSeats + numNonPersistentSeats

-- ** Properties

-- | Stakes returned by 'stakeDistrToDecreasingStakes' are non-increasing
prop_stakeDistrDecreasingStakes :: Property
prop_stakeDistrDecreasingStakes =
  forAll (sized genStakeDistr) $ \stakeDistr -> do
    let tiebreaker = compare -- unfair tiebreaker
    let stakes = fmap snd (stakeDistrToDecreasingStakes tiebreaker stakeDistr)
    case stakes of
      [] -> True -- vacuously true for empty stake distributions
      (_ : rest) -> and (zipWith (>=) stakes rest)

-- | Persistent seats returned by 'weightedFaitAccompliPersistentSeats' never
-- exceed the requested number of seats.
prop_weightedFaitAccompliPersistentSeats_persistentSeatsRespectNumSeats :: Property
prop_weightedFaitAccompliPersistentSeats_persistentSeatsRespectNumSeats =
  forAllStakeDistrAndNumSeats $ \stakeDistr numSeats -> do
    let tiebreaker = compare -- unfair tiebreaker
    let (persistentSeats, numNonPersistentSeats, _) =
          weightedFaitAccompliPersistentSeats tiebreaker numSeats stakeDistr
    let numPersistentSeats = fromIntegral (Map.size persistentSeats)
    tabulateTargetNumSeats numSeats
      . tabulateStakeDistrSize stakeDistr
      . tabulatePersistentToNonPersistentRatio numPersistentSeats numNonPersistentSeats
      $ counterexample
        ( unlines
            [ "Total requested seats: " <> show numSeats
            , "Number of persistent seats selected: " <> show numPersistentSeats
            , "Persistent seats selected: " <> show persistentSeats
            ]
        )
      $ numPersistentSeats <= numSeats

-- | The stake of a persistent seat returned by
-- 'weightedFaitAccompliPersistentSeats' is never smaller than the stake of any
-- of the remaining the nodes in the residual stake distribution.
prop_weightedFaitAccompliPersistentSeats_persistentSeatsHaveLargeStake :: Property
prop_weightedFaitAccompliPersistentSeats_persistentSeatsHaveLargeStake =
  forAllStakeDistrAndNumSeats $ \stakeDistr numSeats -> do
    let tiebreaker = compare -- unfair tiebreaker
    let (persistentSeats, numNonPersistentSeats, residualStakeDistr) =
          weightedFaitAccompliPersistentSeats tiebreaker numSeats stakeDistr
    let numPersistentSeats = fromIntegral (Map.size persistentSeats)
    let persistentStakes = Map.elems persistentSeats
    let residualStakes = Map.elems residualStakeDistr
    tabulateStakeDistrSize stakeDistr
      . tabulateTargetNumSeats numSeats
      . tabulateMoreSeatsThanNodes stakeDistr numSeats
      . tabulatePersistentToNonPersistentRatio numPersistentSeats numNonPersistentSeats
      $ counterexample
        ( unlines
            [ "Persistent seats: " <> show persistentSeats
            , "Residual stake distribution: " <> show residualStakeDistr
            , "Min persistent stake: " <> show (minimum persistentStakes)
            , "Max residual stake: " <> show (maximum residualStakes)
            ]
        )
      $ not (null persistentStakes)
        && not (null residualStakes)
      ==> minimum (stakeToRational <$> persistentStakes)
        >= maximum (stakeToRational <$> residualStakes)

-- | The total stake for the persistent are residual stake distributions
-- returned by 'weightedFaitAccompliPersistentSeats' adds up to the original
-- total stake.
prop_weightedFaitAccompliPersistentSeats_totalStakePreserved :: Property
prop_weightedFaitAccompliPersistentSeats_totalStakePreserved =
  forAllStakeDistrAndNumSeats $ \stakeDistr numSeats -> do
    let tiebreaker = compare -- unfair tiebreaker
    let (persistentSeats, numNonPersistentSeats, residualStakeDistr) =
          weightedFaitAccompliPersistentSeats tiebreaker numSeats stakeDistr
    let numPersistentSeats = fromIntegral (Map.size persistentSeats)
    let totalOriginalStake = stakeDistrTotalStake stakeDistr
    let totalPersistentStake = stakeDistrTotalStake persistentSeats
    let totalResidualStake = stakeDistrTotalStake residualStakeDistr
    let totalCombinedStake =
          stakeToRational totalPersistentStake
            + stakeToRational totalResidualStake
    tabulateStakeDistrSize stakeDistr
      . tabulateTargetNumSeats numSeats
      . tabulateMoreSeatsThanNodes stakeDistr numSeats
      . tabulatePersistentToNonPersistentRatio numPersistentSeats numNonPersistentSeats
      $ counterexample
        ( unlines
            [ "Total original stake: " <> show totalOriginalStake
            , "Total persistent stake: " <> show totalPersistentStake
            , "Total residual stake: " <> show totalResidualStake
            , "Total combined stake: " <> show totalCombinedStake
            ]
        )
      $ stakeToRational totalOriginalStake === totalCombinedStake

-- | The number of non-persistent seats to be selected from the residual stake
-- distribution returned by 'weightedFaitAccompliPersistentSeats' adds up to the
-- original number of seats minus the number of persistent seats.
prop_weightedFaitAccompliPersistentSeats_nonPersistentSeatsCountCorrect :: Property
prop_weightedFaitAccompliPersistentSeats_nonPersistentSeatsCountCorrect =
  forAllStakeDistrAndNumSeats $ \stakeDistr numSeats -> do
    let tiebreaker = compare -- unfair tiebreaker
    let (persistentSeats, numNonPersistentSeats, _) =
          weightedFaitAccompliPersistentSeats tiebreaker numSeats stakeDistr
    let numPersistentSeats = fromIntegral (Map.size persistentSeats)
    tabulateStakeDistrSize stakeDistr
      . tabulateTargetNumSeats numSeats
      . tabulateMoreSeatsThanNodes stakeDistr numSeats
      . tabulatePersistentToNonPersistentRatio numPersistentSeats numNonPersistentSeats
      $ counterexample
        ( unlines
            [ "Total requested seats: " <> show numSeats
            , "Persistent seats selected: " <> show numPersistentSeats
            , "Non-persistent seats to select: " <> show numNonPersistentSeats
            ]
        )
      $ case minusNaturalMaybe numSeats numPersistentSeats of
        Just n -> n === numNonPersistentSeats
        Nothing -> property False

-- | None of the stakes in the residual stake distribution returned by
-- 'weightedFaitAccompliPersistentSeats' should qualify for the next persistent
-- seat according to the weighted Fait Accompli threshold.
prop_weightedFaitAccompliPersistentSeats_residualSeatsDoNotQualify :: Property
prop_weightedFaitAccompliPersistentSeats_residualSeatsDoNotQualify =
  forAllStakeDistrAndNumSeats $ \stakeDistr numSeats -> do
    let tiebreaker = compare -- unfair tiebreaker
    let (persistentSeats, numNonPersistentSeats, residualStakeDistr) =
          weightedFaitAccompliPersistentSeats tiebreaker numSeats stakeDistr
    let numPersistentSeats =
          fromIntegral (Map.size persistentSeats)
    let addSeatIndex =
          flip zipWith [numPersistentSeats ..] $
            \seatIndex (voterId, voterStake) ->
              (voterId, voterStake, seatIndex)
    let accumStakeR =
          fmap snd $
            flip mapAccumR 0 $
              \stakeAccR (voterId, voterStake, seatIndex) ->
                let
                  stakeAccR' =
                    adjustStake (+ stakeToRational voterStake) stakeAccR
                 in
                  ( stakeAccR'
                  ,
                    ( voterId
                    , voterStake
                    , seatIndex
                    , stakeAccR'
                    )
                  )
    let rightCumulativeDecreasingResidualStakes =
          accumStakeR $
            addSeatIndex $
              stakeDistrToDecreasingStakes tiebreaker residualStakeDistr

    let judgments =
          fmap
            ( \(voterIndex, voterStake, seatIndex, stakeAccum) ->
                ( voterIndex
                , voterStake
                , seatIndex
                , stakeAccum
                , weightedFaitAccompliThreshold
                    numSeats
                    seatIndex
                    -- Here we cast the stake from Residual to Global since
                    -- the Fait-Accompli threshold is supposed to be computed
                    -- on global stakes:
                    (coerceStake @(Stake Ledger Global) voterStake)
                    stakeAccum
                )
            )
            rightCumulativeDecreasingResidualStakes
    tabulateStakeDistrSize stakeDistr
      . tabulateTargetNumSeats numSeats
      . tabulateMoreSeatsThanNodes stakeDistr numSeats
      . tabulatePersistentToNonPersistentRatio numPersistentSeats numNonPersistentSeats
      $ counterexample
        ( unlines
            [ "Persistent seats selected: "
                <> show persistentSeats
            , "Next persistent seat index: "
                <> show numPersistentSeats
            , "Residual stakes with index, cumulative stake, and threshold judgments: "
                <> show judgments
            ]
        )
      $ all
        (\(_, _, _, _, aboveThreshold) -> not aboveThreshold)
        judgments

-- | Test suite
tests :: TestTree
tests =
  adjustQuickCheckTests (* 100) $
    testGroup
      "weighted Fait-Accompli committee selection model property tests"
      [ testProperty
          "prop_stakeDistrDecreasingStakes"
          prop_stakeDistrDecreasingStakes
      , testProperty
          "prop_weightedFaitAccompliPersistentSeats_persistentSeatsRespectNumSeats"
          prop_weightedFaitAccompliPersistentSeats_persistentSeatsRespectNumSeats
      , testProperty
          "prop_weightedFaitAccompliPersistentSeats_persistentSeatsHaveLargeStake"
          prop_weightedFaitAccompliPersistentSeats_persistentSeatsHaveLargeStake
      , testProperty
          "prop_weightedFaitAccompliPersistentSeats_totalStakePreserved"
          prop_weightedFaitAccompliPersistentSeats_totalStakePreserved
      , testProperty
          "prop_weightedFaitAccompliPersistentSeats_nonPersistentSeatsCountCorrect"
          prop_weightedFaitAccompliPersistentSeats_nonPersistentSeatsCountCorrect
      , testProperty
          "prop_weightedFaitAccompliPersistentSeats_residualSeatsDoNotQualify"
          prop_weightedFaitAccompliPersistentSeats_residualSeatsDoNotQualify
      ]
