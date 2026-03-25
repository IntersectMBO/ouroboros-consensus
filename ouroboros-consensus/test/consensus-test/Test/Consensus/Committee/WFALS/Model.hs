{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}

-- | Model implementing committee selection based on the Fait Accompli scheme.
--
-- This is based on the paper:
--
-- Peter Gaži, Aggelos Kiayias, and Alexander Russell. 2023. Fait Accompli
-- Committee Selection: Improving the Size-Security Tradeoff of Stake-Based
-- Committees. In Proceedings of the 2023 ACM SIGSAC Conference on Computer and
-- Communications Security (CCS '23). Association for Computing Machinery, New
-- York, NY, USA, 845–858. https://doi.org/10.1145/3576915.3623194
--
-- PDF: https://eprint.iacr.org/2023/1273.pdf
module Test.Consensus.Committee.WFALS.Model
  ( -- * Auxiliary types
    StakeRole (..)
  , StakeType (..)
  , Stake (..)
  , IsStake (..)
  , coerceStake
  , adjustStake
  , SeatIndex
  , VoterId
  , NumSeats

    -- * Stake Distribution
  , StakeDistr
  , stakeDistrToDecreasingStakes
  , stakeDistrTotalStake

    -- * Fait Accompli Committee Selection
  , WFAError
  , weightedFaitAccompliWith
  , weightedFaitAccompliPersistentSeats
  , weightedFaitAccompliThreshold
  , localSortition
  , makeStakeDistrVotingGlobal
  ) where

import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Traversable (mapAccumR)
import GHC.Generics (Generic)
import Numeric.Natural (Natural, minusNaturalMaybe)
import System.Random (RandomGen)
import qualified System.Random.MWC.Distributions as MWC
import System.Random.Stateful (runStateGen)

-------------------------------------------------------------------------------

-- * Auxiliary types

-- | The role of a given stake.
--
-- NOTE: used mostly to better document type signatures in this module.
type data StakeRole
  = -- | Stake as recorded on the ledger.
    -- Used for inputs to the committee selection algorithm.
    Ledger
  | -- | Stake used when casting a new vote.
    -- This may be derived from the ledger stake via some transformation.
    -- Used for outputs of the committee selection algorithm.
    Weight

-- | Extra type information about a given stake.
--
-- NOTE: used mostly to better document type signatures in this module.
type data StakeType
  = Global
  | Persistent
  | Residual
  | Cumulative

-- | Number of seats in the committee
type NumSeats :: StakeType -> Type
type family NumSeats st

-- | Seat index (0-based)
type SeatIndex = Natural

-- | Voter identifier
type VoterId = String

-- ** Ledger and voting stakes

-- | Stake types annotated with their role and type information
data family Stake :: StakeRole -> StakeType -> Type

-- | Type class with operations needed over stakes
class (Num stake, Ord stake) => IsStake stake where
  stakeToRational :: stake -> Rational
  rationalToStake :: Rational -> stake

-- | DerivingVia helper to implement 'IsStake' instances for different stake
-- types via a common underlying representation (Rational).
type RationalStake :: StakeRole -> StakeType -> Type
newtype RationalStake sr st
  = RationalStake Rational
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num

instance IsStake (RationalStake sr st) where
  stakeToRational (RationalStake r) = r
  rationalToStake = RationalStake

-- | Adjust a stake via a function on rationals
adjustStake ::
  forall stake' stake.
  ( IsStake stake
  , IsStake stake'
  ) =>
  (Rational -> Rational) ->
  stake ->
  stake'
adjustStake f stake =
  rationalToStake (f (stakeToRational stake))

-- | Retag a stake with a different origin and type.
--
-- NOTE: type parameters are arranged so that one can use type applications
-- to specify the parameters of the resulting type first.
coerceStake ::
  forall stake' stake.
  ( IsStake stake
  , IsStake stake'
  ) =>
  stake ->
  stake'
coerceStake =
  adjustStake id

-- ** Stake distributions

-- | Stake distribution mapping voters to their stakes
type StakeDistr (sr :: StakeRole) (st :: StakeType) = Map VoterId (Stake sr st)

-- | Get the total stake in a stake distribution
stakeDistrTotalStake ::
  forall sr st.
  IsStake (Stake sr st) =>
  StakeDistr sr st ->
  Stake sr st
stakeDistrTotalStake distr =
  sum (Map.elems distr)

-- | View a stake distribution as a non-increasing list of stakes.
--
-- Like the real implementation, this also uses a tiebreaker to control how
-- voters with the same stake are ordered in the resulting list.
stakeDistrToDecreasingStakes ::
  IsStake (Stake sr st) =>
  (VoterId -> VoterId -> Ordering) ->
  StakeDistr sr st ->
  [(VoterId, Stake sr st)]
stakeDistrToDecreasingStakes tiebreaker distr =
  sortBy
    descendingStakeWithTiebreaker
    (Map.toList distr)
 where
  descendingStakeWithTiebreaker
    (poolId1, stake1)
    (poolId2, stake2)
      -- The pools have different stake => sort them in descending order
      | stake1 /= stake2 = compare stake2 stake1
      -- The pools have the same stake => use the tiebreaker to sort them
      | otherwise = tiebreaker poolId1 poolId2

-------------------------------------------------------------------------------

-- * Fait Accompli Committee Selection

-- | Errors that can occur while computing a weighted Fait Accompli committee selection.
type WFAError = String

-- | Weighted Fait Accompli committee selection with a fallback mechanism.
--
-- This is mostly just plumbing to connect the result of
-- 'weightedFaitAccompliPersistentSeats' with a given fallback scheme.
weightedFaitAccompliWith ::
  -- | Tiebreaker to control the order of voters with the same stake
  (VoterId -> VoterId -> Ordering) ->
  -- | Fallback function for non-persistent seats
  ( NumSeats Residual ->
    StakeDistr Ledger Residual ->
    StakeDistr Weight Residual
  ) ->
  -- | Total number of seats
  NumSeats Global ->
  -- | Stake distribution
  StakeDistr Ledger Global ->
  -- | Selected persistent and non-persistent seats with their corresponding
  -- voting stakes
  Either
    WFAError
    ( StakeDistr Weight Persistent
    , StakeDistr Weight Residual
    )
weightedFaitAccompliWith tiebreaker fallback globalNumSeats stakeDistr = do
  (persistentSeats, numNonPersistentSeats, residualStakeDistr) <-
    weightedFaitAccompliPersistentSeats
      tiebreaker
      globalNumSeats
      stakeDistr
  let nonPersistentSeats =
        fallback
          numNonPersistentSeats
          residualStakeDistr
  pure (persistentSeats, nonPersistentSeats)

-- | First step of the weighted Fait Accompli committee selection.
--
-- This step selects the persistent seats deterministically via the weighted
-- Fait Accompli threshold, and returns the remaining stake distribution for the
-- non-persistent seats selection.
weightedFaitAccompliPersistentSeats ::
  (VoterId -> VoterId -> Ordering) ->
  NumSeats Global ->
  StakeDistr Ledger Global ->
  Either
    WFAError
    ( StakeDistr Weight Persistent
    , NumSeats Residual
    , StakeDistr Ledger Residual
    )
weightedFaitAccompliPersistentSeats tiebreaker globalNumSeats stakeDistr
  | globalNumSeats <= 0 =
      Left "Number of seats must be positive"
  | Map.size stakeDistr == 0 =
      Left "Stake distribution cannot be empty"
  | sum stakeDistr == 0 =
      Left "Total stake must be positive"
  | numPoolsWithPositiveStake < globalNumSeats =
      Left "Not enough voters with positive stake to fill all expected seats"
  | otherwise =
      Right (persistentSeats, numNonPersistentSeats, residualStakeDistr)
 where
  -- Number of voters with positive stake in the input distribution
  numPoolsWithPositiveStake =
    fromIntegral
      . length
      . filter (> 0)
      . Map.elems
      $ stakeDistr

  -- Persistent seats selected deterministically based on their ledger stake.
  -- NOTE: their voting stake is *exactly* their ledger stake.
  persistentSeats =
    voterListToStakeDistr persistentVotersWithStake

  -- Residual stake distribution for the non-persistent seats selection. This
  -- includes all the voters that did not qualify for a persistent seat, with
  -- their original ledger stake.
  residualStakeDistr =
    voterListToStakeDistr residualVotersWithStake

  -- Number of non-persistent seats to be selected via a fallback scheme
  numNonPersistentSeats =
    case globalNumSeats `minusNaturalMaybe` numPersistentSeats of
      Just n -> n
      Nothing -> error "Number of persistent seats exceeds total seats"
   where
    numPersistentSeats = fromIntegral (length persistentVotersWithStake)

  -- Split the cumulative stakes into persistent and non-persistent seats
  (persistentVotersWithStake, residualVotersWithStake) =
    bimap
      (mapStakes (coerceStake @(Stake Weight Persistent)))
      (mapStakes (coerceStake @(Stake Ledger Residual)))
      $ span
        ( \(_, voterStake, seatIndex, stakeAccum) ->
            weightedFaitAccompliThreshold
              globalNumSeats
              seatIndex
              voterStake
              stakeAccum
        )
        rightCumulativeDecreasingStakes
   where
    mapStakes f = fmap (\(x, s, y, z) -> (x, f s, y, z))

  -- Stake distribution in non-increasing order, including voter seat index and
  -- accumulated stakes from the right, i.e., from all the voters with smaller
  -- stake than the current one.
  rightCumulativeDecreasingStakes =
    accumStakeR $
      addSeatIndex $
        stakeDistrToDecreasingStakes tiebreaker $
          stakeDistr
   where
    -- Add an increasing seat index to each candidate
    addSeatIndex =
      zipWith
        (\seatIndex (vId, vStake) -> (vId, vStake, seatIndex))
        [0 ..]
    -- Accumulated stake of every candidate beyond (and including) this one
    accumStakeR xs =
      snd $
        mapAccumR
          ( \stakeAccR (voterId, voterStake, seatIndex) ->
              let stakeAccR' =
                    stakeAccR
                      + coerceStake @(Stake Ledger Cumulative) voterStake
               in ( stakeAccR'
                  , (voterId, voterStake, seatIndex, stakeAccR')
                  )
          )
          0
          xs

  -- Drop the accumulated information to get back to a stake distribution
  voterListToStakeDistr =
    Map.fromList . fmap (\(voterId, stake, _, _) -> (voterId, stake))

-- | Weighted Fait Accompli threshold for persistent seats
--
-- For more details, see (Section 4, Figure 7) of the paper:
-- https://eprint.iacr.org/2023/1273.pdf
weightedFaitAccompliThreshold ::
  -- | Total number of seats
  NumSeats Global ->
  -- | Voter seat index (0-based)
  SeatIndex ->
  -- | Voter ledger stake
  Stake Ledger Global ->
  -- | Cumulative ledger stake from the right (including the current voter stake)
  Stake Ledger Cumulative ->
  -- | Whether the voter qualifies for a persistent seat
  Bool
weightedFaitAccompliThreshold numSeats voterSeat voterStake stakeAccR
  | stakeToRational stakeAccR <= 0 =
      False -- Avoid division by zero in the right-hand side of the inequality
  | voterSeat >= numSeats =
      False -- Avoid underflow in the right-hand side of the inequality
  | otherwise =
      ( (1 - (stakeToRational voterStake / stakeToRational stakeAccR))
          ^ (2 :: Integer)
      )
        < toRational (numSeats - voterSeat - 1)
          / toRational (numSeats - voterSeat)

-- | Local sortition fallback
localSortition ::
  ( IsStake (Stake Ledger st)
  , IsStake (Stake Weight st)
  , Integral (NumSeats st)
  ) =>
  RandomGen rng =>
  -- | Random number generator
  rng ->
  -- | Expected number of non-persistent seats
  NumSeats st ->
  -- | Non-persistent stake distribution candidate (not necessarily normalized)
  StakeDistr Ledger st ->
  -- | Selected non-persistent seats with their correspoinding voting stakes
  StakeDistr Weight st
localSortition rng0 numSeats stakeDistr =
  Map.fromList $
    sampleNonPersistentVoters $
      Map.toList $
        stakeDistr
 where
  -- The total stake of the non-persistent candidate stake distribution
  totalStake = stakeDistrTotalStake stakeDistr
  -- All non-persistent voters share the same voting "weight"
  seatStake = rationalToStake (stakeToRational totalStake / fromIntegral numSeats)
  -- Sample non-persistent voter seats independently to emulate local sortition
  sampleNonPersistentVoters = snd . foldr sampleVoterStake (rng0, [])
  -- Sample voter seats for each voter independently to emulate local sortition
  sampleVoterStake (voterId, stake) (rng, acc) =
    let
      normalizedStake = stakeToRational stake / stakeToRational totalStake
      -- Expected number of seats for this voter
      lambda = fromIntegral numSeats * fromRational normalizedStake
      -- Sample number of seats using Poisson distribution
      (numSeatsForVoter, rng') = poisson lambda rng
      -- Voting stake of this voter equally divided
      --
      -- NOTE: if desired we could also scale this by the number of seats this
      -- voter was granted via the Poisson sampling.
      votingStake = fromIntegral numSeatsForVoter * seatStake
     in
      ( -- Thread the updated random generator
        rng'
      , -- Accumulate voter if they won any seats
        if numSeatsForVoter > 0 then (voterId, votingStake) : acc else acc
      )

-- | Combine the resulting persistent and non-persistent stake distributions
-- into a single global one
makeStakeDistrVotingGlobal ::
  StakeDistr Weight Persistent ->
  StakeDistr Weight Residual ->
  StakeDistr Weight Global
makeStakeDistrVotingGlobal persistent residual =
  Map.unionWith
    (+)
    (coerceStake @(Stake Weight Global) <$> persistent)
    (coerceStake @(Stake Weight Global) <$> residual)

-- ** Random sampling helpers

-- | Wrapper over 'mwc-random' to sample from a Poisson distribution using an
-- explicit seed.
poisson :: RandomGen rng => Double -> rng -> (Natural, rng)
poisson lambda rng0
  | lambda < 0 =
      error "Poisson's lambda parameter must be non-negative"
  | otherwise =
      first fromIntegral $
        runStateGen rng0 $
          MWC.poisson lambda

-------------------------------------------------------------------------------

-- * Boilerplate instances

type instance NumSeats Global = Natural

type instance NumSeats Persistent = Natural

type instance NumSeats Residual = Natural

newtype instance Stake Ledger Global = StakeLedgerGlobal Rational
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num

newtype instance Stake Ledger Persistent = StakeLedgerPersistent Rational
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num

newtype instance Stake Ledger Residual = StakeLedgerResidual Rational
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num

newtype instance Stake Ledger Cumulative = StakeLedgerCumulative Rational
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num

newtype instance Stake Weight Global = StakeWeightGlobal Rational
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num

newtype instance Stake Weight Persistent = StakeWeightPersistent Rational
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num

newtype instance Stake Weight Residual = StakeWeightResidual Rational
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype Num

deriving via (RationalStake Ledger Global) instance IsStake (Stake Ledger Global)
deriving via (RationalStake Ledger Persistent) instance IsStake (Stake Ledger Persistent)
deriving via (RationalStake Ledger Residual) instance IsStake (Stake Ledger Residual)
deriving via (RationalStake Ledger Cumulative) instance IsStake (Stake Ledger Cumulative)
deriving via (RationalStake Weight Global) instance IsStake (Stake Weight Global)
deriving via (RationalStake Weight Persistent) instance IsStake (Stake Weight Persistent)
deriving via (RationalStake Weight Residual) instance IsStake (Stake Weight Residual)
