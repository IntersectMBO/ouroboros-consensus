{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Deterministic portion of the Weighted Fait-Accompli committee selection scheme
module Ouroboros.Consensus.Committee.WFA
  ( weightedFaitAccompliSplitSeats
  , isAbovePersistentSeatThreshold
  ) where

import Control.Exception (assert)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Committee.Types
  ( CommitteeSize (..)
  , CumulativeStakeDistr
  , SeatIndex (..)
  , Stake (..)
  , StakeRole (..)
  , VoterId
  , cumulativeStakeDistrToList
  )

-- | Split a stake distrubution into persistent and non-persistent committee
-- seats according to the weighted Fait-Accompli scheme.
--
-- This function returns a map indexed by voter id, containing their committee
-- status (whether they were granted a persistent seat or not), as well as their
-- corresponding (ledger) stake. In addition, it also returns other metrics
-- related to the committee composition to be cached throughout the entire epoch:
--   * number of persistent seats granted via the weighted Fait-Accompli scheme
--   * number of non-persistent seats expected to vote via local sortition
--   * total persistent stake
--   * total non-persistent stake
weightedFaitAccompliSplitSeats ::
  -- | Cumulated stake distribution of the potential voters
  CumulativeStakeDistr ->
  -- | Expected total committee size (persistent + non-persistent)
  CommitteeSize ->
  ( Map VoterId ((Maybe SeatIndex), Stake Ledger)
  , CommitteeSize
  , CommitteeSize
  , Stake Cumulative
  , Stake Cumulative
  )
weightedFaitAccompliSplitSeats stakeDistr totalSeats =
  assert (unCommitteeSize totalSeats >= numPersistentVoters) $
    ( Map.fromList persistentVoters <> Map.fromList nonPersistentVoters
    , CommitteeSize numPersistentVoters
    , CommitteeSize numNonPersistentVoters
    , CumulativeStake persistentStake
    , CumulativeStake nonPersistentStake
    )
 where
  persistentVoters =
    [ (voterId, (Just seatIndex, voterStake))
    | (seatIndex, voterId, voterStake, _) <- persistentSeats
    ]

  nonPersistentVoters =
    [ (voterId, (Nothing, voterStake))
    | (_, voterId, voterStake, _) <- nonPersistentSeats
    ]

  numPersistentVoters =
    fromIntegral (length persistentSeats)

  -- NOTE: this is not computed from the length of 'nonPersistentSeats', but
  -- rather from the total committee size minus the number of persistent voters.
  numNonPersistentVoters =
    unCommitteeSize totalSeats - numPersistentVoters

  persistentStake =
    totalStake persistentSeats

  nonPersistentStake =
    totalStake nonPersistentSeats

  totalStake voters =
    sum [voterStake | (_, _, LedgerStake voterStake, _) <- voters]

  (persistentSeats, nonPersistentSeats) =
    List.span
      ( \(seatIndex, _, voterStake, cumulativeStake) ->
          isAbovePersistentSeatThreshold
            totalSeats
            seatIndex
            voterStake
            cumulativeStake
      )
      (cumulativeStakeDistrToList stakeDistr)

-- | Evaluate whether a voter with its give stake and relatile position in the
-- stake distribution can be granted a persistent seat in the voting committee.
isAbovePersistentSeatThreshold ::
  -- | Total committee size (persistent + non-persistent)
  CommitteeSize ->
  -- | Current voter seat index
  SeatIndex ->
  -- | Current voter stake
  Stake Ledger ->
  -- | Cumulated stake of voters with smaller or equal stake than the current one
  Stake Cumulative ->
  -- | Whether the current voter has a persistent seat or not
  Bool
isAbovePersistentSeatThreshold
  (CommitteeSize totalSeats)
  (SeatIndex voterSeat)
  (LedgerStake voterStake)
  (CumulativeStake cumulativeStake)
    | cumulativeStake <= 0 =
        False -- Avoid division by zero in the left-hand side of the inequality
    | voterSeat > totalSeats =
        False -- Avoid underflow in the right-hand side of the inequality
    | otherwise =
        ( (1 - (voterStake / cumulativeStake))
            ^ (2 :: Integer)
        )
          < ( toRational (totalSeats - voterSeat)
                / toRational (totalSeats - voterSeat + 1)
            )
