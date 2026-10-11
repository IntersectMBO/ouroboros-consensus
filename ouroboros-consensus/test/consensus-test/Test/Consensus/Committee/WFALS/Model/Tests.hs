{-# LANGUAGE TypeApplications #-}

-- | Test properties for the weighted Fait-Accompli committee selection model
module Test.Consensus.Committee.WFALS.Model.Tests (tests) where

import Data.List (mapAccumR)
import qualified Data.Map.Strict as Map
import Numeric.Natural (minusNaturalMaybe)
import Test.Consensus.Committee.WFALS.Model
  ( IsStake (..)
  , Stake (..)
  , StakeRole (..)
  , StakeType (..)
  , WFAError
  , adjustStake
  , coerceStake
  , stakeDistrToDecreasingStakes
  , stakeDistrTotalStake
  , weightedFaitAccompliPersistentSeats
  , weightedFaitAccompliThreshold
  )
import Test.Consensus.Committee.WFALS.Model.Utils
  ( forAllValidStakeDistrAndNumSeats
  , genStakeDistr
  , tabulatePersistentToNonPersistentRatio
  , tabulateStakeDistrSize
  , tabulateTargetNumSeats
  )
import Test.QuickCheck
  ( Property
  , Testable (..)
  , counterexample
  , forAll
  , sized
  , (===)
  , (==>)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestEnv (adjustQuickCheckTests)

-- * Properties

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
  forAllValidStakeDistrAndNumSeats $ \stakeDistr numSeats -> do
    let tiebreaker = compare -- unfair tiebreaker
    let (persistentSeats, numNonPersistentSeats, _) =
          expectSuccess $
            weightedFaitAccompliPersistentSeats
              tiebreaker
              numSeats
              stakeDistr
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
  forAllValidStakeDistrAndNumSeats $ \stakeDistr numSeats -> do
    let tiebreaker = compare -- unfair tiebreaker
    let (persistentSeats, numNonPersistentSeats, residualStakeDistr) =
          expectSuccess $
            weightedFaitAccompliPersistentSeats
              tiebreaker
              numSeats
              stakeDistr
    let numPersistentSeats = fromIntegral (Map.size persistentSeats)
    let persistentStakes = Map.elems persistentSeats
    let residualStakes = Map.elems residualStakeDistr
    tabulateStakeDistrSize stakeDistr
      . tabulateTargetNumSeats numSeats
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
  forAllValidStakeDistrAndNumSeats $ \stakeDistr numSeats -> do
    let tiebreaker = compare -- unfair tiebreaker
    let (persistentSeats, numNonPersistentSeats, residualStakeDistr) =
          expectSuccess $
            weightedFaitAccompliPersistentSeats
              tiebreaker
              numSeats
              stakeDistr
    let numPersistentSeats = fromIntegral (Map.size persistentSeats)
    let totalOriginalStake = stakeDistrTotalStake stakeDistr
    let totalPersistentStake = stakeDistrTotalStake persistentSeats
    let totalResidualStake = stakeDistrTotalStake residualStakeDistr
    let totalCombinedStake =
          stakeToRational totalPersistentStake
            + stakeToRational totalResidualStake
    tabulateStakeDistrSize stakeDistr
      . tabulateTargetNumSeats numSeats
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
  forAllValidStakeDistrAndNumSeats $ \stakeDistr numSeats -> do
    let tiebreaker = compare -- unfair tiebreaker
    let (persistentSeats, numNonPersistentSeats, _) =
          expectSuccess $
            weightedFaitAccompliPersistentSeats
              tiebreaker
              numSeats
              stakeDistr
    let numPersistentSeats = fromIntegral (Map.size persistentSeats)
    tabulateStakeDistrSize stakeDistr
      . tabulateTargetNumSeats numSeats
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
  forAllValidStakeDistrAndNumSeats $ \stakeDistr numSeats -> do
    let tiebreaker = compare -- unfair tiebreaker
    let (persistentSeats, numNonPersistentSeats, residualStakeDistr) =
          expectSuccess $
            weightedFaitAccompliPersistentSeats
              tiebreaker
              numSeats
              stakeDistr
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

expectSuccess :: Either WFAError a -> a
expectSuccess result =
  case result of
    Left err -> error $ "Model failed unexpectedly: " <> err
    Right val -> val

-- | Test suite
tests :: TestTree
tests =
  adjustQuickCheckTests (* 100) $
    testGroup
      "Model property tests"
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
