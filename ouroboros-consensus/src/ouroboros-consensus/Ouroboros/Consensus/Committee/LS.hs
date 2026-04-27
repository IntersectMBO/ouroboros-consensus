{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Local sortition used by non-persistent members of the voting committee
-- Implements the @LS@ component of the wFA^LS scheme from the Fait-Accompli
-- Committee Selection paper (https://eprint.iacr.org/2023/1273.pdf, §2.3).
-- See also https://github.com/input-output-hk/ouroboros-leios/blob/c5658913221a7f58063bc4f82efaec0900e53dab/post-cip/weighted-fait-accompli.pdf
module Ouroboros.Consensus.Committee.LS
  ( -- * Local sortition check
    LocalSortitionNumSeats (..)
  , localSortitionNumSeats
  ) where

import Cardano.Ledger.BaseTypes (FixedPoint, HasZero)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Ouroboros.Consensus.Committee.Crypto (NormalizedVRFOutput (..))
import Ouroboros.Consensus.Committee.Types (Cumulative (..), LedgerStake (..))
import Ouroboros.Consensus.Committee.WFA
  ( NonPersistentCommitteeSize (..)
  , TotalNonPersistentStake (..)
  )

-- * Local sortition check

-- | Number of non-persistent seats granted by local sortition to a voter
newtype LocalSortitionNumSeats = LocalSortitionNumSeats
  { unLocalSortitionNumSeats :: Word64
  }
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, HasZero)

-- | Compute how many non-persistent seats can be granted by local sortition to
-- a voter given their normalized VRF output and stake
localSortitionNumSeats ::
  -- | Expected number of non-persistent voters in the committee
  NonPersistentCommitteeSize ->
  -- | Total stake of non-persistent voters
  TotalNonPersistentStake ->
  -- | Stake of the voter
  LedgerStake ->
  -- | Normalized VRF output from the participant
  NormalizedVRFOutput ->
  LocalSortitionNumSeats
localSortitionNumSeats
  (NonPersistentCommitteeSize numNonPersistentVoters)
  (TotalNonPersistentStake (Cumulative (LedgerStake totalNonPersistentStake)))
  (LedgerStake voterStake)
  (NormalizedVRFOutput normalizedVRFOutput)
    -- None of the non-persistent voters have any stake => nobody gets a seat.
    -- NOTE: this check also exists to prevent division by zero below.
    | totalNonPersistentStake <= 0 = LocalSortitionNumSeats 0
    -- This voter has no stake (but some others do) => it does not get any seat.
    -- NOTE: this check avoids the expensive computation below and also
    -- prevent division by zero in computing "orders"
    | voterStake <= 0 = LocalSortitionNumSeats 0
    -- If the voter has stake close to zero, the conversion from 'Rational' to
    -- 'FixedPoint' for 'lambda' might underflow to zero, which would cause the
    -- "orders" computation below to divide by zero.
    | lambda <= 0 = LocalSortitionNumSeats 0
    -- This voter might be entitled to some seats => run the local sortition.
    | otherwise = LocalSortitionNumSeats (fromIntegral expectedSeats)
   where
    -- Expected number of seats granted by local sortition
    lambda :: FixedPoint
    lambda =
      fromRational $
        fromIntegral numNonPersistentVoters
          * voterStake
          / totalNonPersistentStake

    -- Compute the "orders" of the Poisson distribution with parameter lambda,
    -- which are used as thresholds to determine how many seats we get based on
    -- the normalized VRF output
    orders :: [FixedPoint]
    orders =
      (fromRational normalizedVRFOutput / lambda)
        : zipWith
          (\k prev -> k * prev / lambda)
          [2 ..]
          orders

    -- Estimate how many seats we get by comparing the normalized VRF output
    -- against the thresholds defined by the orders.
    --
    -- TODO(peras): evaluate whether the limit used below (3) makes sense in
    -- this context. One possible starting point would be to understand why
    -- @checkLeaderNatValue@ (in Ledger) also uses 3 as its own limit when
    -- computing slot leadership proofs.
    expectedSeats :: Int
    expectedSeats =
      fromMaybe 0 $
        taylorExpCmpFirstNonLower
          3
          orders
          (-lambda)

-------------------------------------------------------------------------------
-- Helpers vendored from:
-- https://github.com/cardano-scaling/leios-wfa-ls-demo/blob/7bbd846d9765191ca83b58477dc1596f64ac80fd/leios-wfa-ls-demo/lib/Cardano/Leios/NonIntegral.hs#L227
--
-- TODO: merge these into @Cardano.Ledger.NonIntegral@ in @cardano-ledger@

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
  forall a.
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
  -- Traverse the list of cmps, advancing the Taylor state as needed while
  -- checking if the current cmp is ABOVE or BELOW. If ABOVE, return the index.
  goList ::
    Int -> -- maxN
    Int -> -- n
    a -> -- err
    a -> -- acc
    a -> -- divisor
    Int -> -- current index
    [a] -> -- remaining cmps
    Maybe Int
  goList _ _ _ _ _ _ [] = Nothing
  goList maxN n err acc divisor i (cmp : rest) =
    case decideOne maxN n err acc divisor cmp of
      Stop ->
        Just i
      Below n' err' acc' divisor' ->
        goList maxN n' err' acc' divisor' (i + 1) rest

  -- Decide current cmp by advancing the shared Taylor state as needed.
  -- If BELOW is established, returns the *advanced* state to continue with.
  -- If ABOVE is established or maxN reached, returns Stop.
  decideOne ::
    Int -> -- maxN
    Int -> -- n
    a -> -- err
    a -> -- acc
    a -> -- divisor
    a -> -- cmp
    Step a
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
