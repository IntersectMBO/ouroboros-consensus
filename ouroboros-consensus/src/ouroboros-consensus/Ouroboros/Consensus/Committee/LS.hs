{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Local sortition used by non-persistent members of the voting committee
module Ouroboros.Consensus.Committee.LS
  ( -- * Local sortition check
    NormalizedLocalSortitionVRFValue (..)
  , checkNonPersistentSeats
  ) where

import Cardano.Ledger.BaseTypes (FixedPoint)
import Data.Maybe (fromMaybe)
import Ouroboros.Consensus.Committee.Types
  ( CommitteeSize (..)
  , NumSeats (..)
  , Stake (..)
  , StakeRole (..)
  )

-- * Local sortition check

-- | VRF output of a voter for a given election, normalized to [0, 1]
newtype NormalizedLocalSortitionVRFValue = NormalizedLocalSortitionVRFValue
  { unNormalizedLocalSortitionVRFValue :: Rational
  }
  deriving (Eq, Show)

-- | Compute how many non-persistent seats can be granted by local sortition
-- to a given voter for a given election
checkNonPersistentSeats ::
  -- | Expected number of non-persistent voters in the committee
  CommitteeSize ->
  -- | Total stake of non-persistent voters
  Stake Cumulative ->
  -- | Stake of the voter
  Stake Ledger ->
  -- | Normalized VRF output of the voter for the given election
  NormalizedLocalSortitionVRFValue ->
  -- | Number of non-persistent seats granted to us by local sortition
  NumSeats
checkNonPersistentSeats
  (CommitteeSize numNonPersistentVoters)
  (CumulativeStake totalNonPersistentStake)
  (LedgerStake ourStake)
  (NormalizedLocalSortitionVRFValue vrfValue) =
    NumSeats (fromIntegral numSeats)
   where
    numSeats :: Int
    numSeats
      | totalNonPersistentStake == 0 = 0
      | ourStake == 0 = 0
      | otherwise = fromMaybe 0 expectedSeats

    expectedSeats :: Maybe Int
    expectedSeats =
      taylorExpCmpFirstNonLower 3 orders (-lambda)

    orders :: [FixedPoint]
    orders =
      (fromRational vrfValue / lambda)
        : zipWith
          (\k prev -> k * prev / lambda)
          [2 ..]
          orders

    lambda :: FixedPoint
    lambda =
      fromRational $
        fromIntegral numNonPersistentVoters
          * ourStake
          / totalNonPersistentStake

-- normalizedOutputVRF :: FixedPoint
-- normalizedOutputVRF =
--   fromRational $
--     toInteger vrfValue
--       % (2 ^ (8 * vrfSize))

-- normalizedOutputVRF :: FixedPoint
-- normalizedOutputVRF =
--   fromRational $
--     toInteger (VRF.getOutputVRFNatural outputVRF)
--       % (2 ^ (8 * VRF.sizeOutputVRF outputVRF))
--
-- outputVRF :: VRF.OutputVRF v
-- outputVRF = VRF.certifiedOutput certifiedVRF

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

-- * VRF construction

-- -- | Input to the verifiable random function used by Peras.
-- --
-- -- Consists of the hash of the Peras round number and the epoch nonce.
-- newtype PerasInputVRF = PerasInputVRF
--   { unPerasInputVRF :: Hash Blake2b_256 PerasInputVRF
--   }
--   deriving (Eq, Ord, Show, Generic)
--   deriving newtype (NoThunks, ToCBOR)
--
-- -- | Construct a unified VRF value for Peras local sortition.
-- --
-- -- The VRF input is derived from:
-- --   * the election identifier (Peras round number), and
-- --   * the Praos nonce for the epoch
-- --
-- -- TODO: which epoch? the current one? the one corresponding to the round?
-- mkPerasInputVRF ::
--   PerasRoundNo ->
--   Nonce ->
--   PerasInputVRF
-- mkPerasInputVRF roundNo epochNonce =
--   PerasInputVRF
--     . Hash.castHash
--     . Hash.hashWith id
--     . runByteBuilder (8 + 32)
--     $ roundNoBytes <> epochNonceBytes
--  where
--   roundNoBytes =
--     BS.word64BE (unPerasRoundNo roundNo)
--   epochNonceBytes =
--     case epochNonce of
--       NeutralNonce -> mempty
--       Nonce h -> BS.byteStringCopy (Hash.hashToBytes h)
--
-- -- | Construct a certified VRF value for Peras local sortition.
-- mkPerasCertifiedVRF ::
--   ( VRF.VRFAlgorithm v
--   , VRF.Signable v PerasInputVRF
--   , VRF.ContextVRF v ~ ()
--   ) =>
--   Nonce ->
--   PerasRoundNo ->
--   VRF.SignKeyVRF v ->
--   VRF.CertifiedVRF v PerasInputVRF
-- mkPerasCertifiedVRF epochNonce roundNo signKeyVRF =
--   VRF.evalCertified () inputVRF signKeyVRF
--  where
--   inputVRF = mkPerasInputVRF roundNo epochNonce

-- -- | Compute how many non-persistent seats can be granted by local sortition
-- checkNonPersistentSeats ::
--   forall v.
--   VRF.VRFAlgorithm v =>
--   -- | Certified VRF output for the given round and our VRF key
--   VRF.CertifiedVRF v PerasInputVRF ->
