{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Local sortition used by non-persistent members of the voting committee
module Ouroboros.Consensus.Committee.LS
  ( -- * Local sortition check
    localSortitionNumSeats
  ) where

import Cardano.Ledger.BaseTypes (FixedPoint)
import Control.Exception (assert)
import Data.Maybe (fromMaybe)
import Ouroboros.Consensus.Committee.Crypto (CryptoSupportsVRF (..))
import Ouroboros.Consensus.Committee.Types
  ( NonPersistentCommitteeSize (..)
  , NumSeats (..)
  , Stake (..)
  , StakeRole (..)
  , TotalNonPersistentStake (..)
  )

-- * Local sortition check

-- | Compute how many non-persistent seats can be granted by local sortition
-- to a voter given its VRF output and stake
localSortitionNumSeats ::
  forall c.
  CryptoSupportsVRF c =>
  -- | Expected number of non-persistent voters in the committee
  NonPersistentCommitteeSize ->
  -- | Total stake of non-persistent voters
  TotalNonPersistentStake ->
  -- | Stake of the voter
  Stake Ledger ->
  -- | VRF output from the participant
  VRFOutput c ->
  NumSeats
localSortitionNumSeats
  (NonPersistentCommitteeSize numNonPersistentVoters)
  (TotalNonPersistentStake (CumulativeStake totalNonPersistentStake))
  (LedgerStake ourStake)
  vrfOutput =
    assert (totalNonPersistentStake > 0) $
      assert (ourStake > 0) $ do
        -- Normalize the VRF output to a value in [0, 1]
        let normalizedVRFOutput :: Rational
            normalizedVRFOutput =
              normalizeVRFOutput vrfOutput

        -- Expected number of seats granted by local sortition
        let lambda :: FixedPoint
            lambda =
              fromRational $
                fromIntegral numNonPersistentVoters
                  * ourStake
                  / totalNonPersistentStake

        -- Compute the "orders" of the Poisson distribution with parameter
        -- lambda, which are used as thresholds to determine how many seats we
        -- get based on the normalized VRF output
        let orders :: [FixedPoint]
            orders =
              (fromRational normalizedVRFOutput / lambda)
                : zipWith
                  (\k prev -> k * prev / lambda)
                  [2 ..]
                  orders

        -- Estimate how many seats we get by comparing the normalized VRF output
        -- against the thresholds defined by the orders
        let expectedSeats :: Int
            expectedSeats =
              fromMaybe 0 $
                taylorExpCmpFirstNonLower
                  3
                  orders
                  (-lambda)

        NumSeats (fromIntegral expectedSeats)

-------------------------------------------------------------------------------
-- Helpers vendored from:
-- https://github.com/cardano-scaling/leios-wfa-ls-demo/blob/7bbd846d9765191ca83b58477dc1596f64ac80fd/leios-wfa-ls-demo/lib/Cardano/Leios/NonIntegral.hs#L227

data Step a
  = Stop
  | -- Here we have `Below n err acc divisor`
    Below Int a a a

-- Returns the index of the first element that is NOT certainly BELOW.
-- It evaluates cmps left-to-right, reusing the Taylor-expansion state
-- (acc/err/divisor/n) across elements so we don't redo work.
--
-- Behavior:
--   * If cmp_i is proven ABOVE -> return i
--   * If max iterations reached while testing cmp_i -> return i
--   * If every element is proven BELOW -> returns Nothing
--
-- IMPORTANT: boundX must be e^{|x|} for correct error bounds (see taylorExpCmp).
taylorExpCmpFirstNonLower ::
  RealFrac a =>
  -- | boundX = e^{|x|} for correct error estimation
  a ->
  -- | list of cmp thresholds (checked in order)
  [a] ->
  -- | x in e^x
  a ->
  Maybe Int
taylorExpCmpFirstNonLower boundX cmps x =
  goList 1000 0 x 1 1 0 cmps
 where
  -- State is: maxN, n, err, acc, divisor
  -- plus current index i and remaining cmps.
  goList _ _ _ _ _ _ [] = Nothing
  goList maxN n err acc divisor i (cmp : rest) =
    case decideOne maxN n err acc divisor cmp of
      Stop -> Just i
      Below n' err' acc' divisor' ->
        goList maxN n' err' acc' divisor' (i + 1) rest

  -- Decide current cmp by advancing the shared Taylor state as needed.
  -- If BELOW is established, returns the *advanced* state to continue with.
  -- If ABOVE is established or maxN reached, returns Stop.
  decideOne maxN n err acc divisor cmp
    | maxN == n = Stop
    | cmp >= acc' + errorTerm = Stop
    | cmp < acc' - errorTerm = Below (n + 1) err' acc' divisor'
    | otherwise = decideOne maxN (n + 1) err' acc' divisor' cmp
   where
    divisor' = divisor + 1
    nextX = err
    acc' = acc + nextX
    err' = (err * x) / divisor'
    errorTerm = abs (err' * boundX)
