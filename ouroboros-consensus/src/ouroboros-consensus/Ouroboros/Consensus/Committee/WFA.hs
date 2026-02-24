{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Deterministic portion of the Weighted Fait-Accompli committee selection scheme
module Ouroboros.Consensus.Committee.WFA
  ( weightedFaitAccompliSplitSeats
  , isAbovePersistentSeatThreshold
  ) where

import Control.Exception (assert)
import qualified Data.Array as Array
import qualified Data.List as List
import Ouroboros.Consensus.Committee.Types
  ( CommitteeSize (..)
  , ExtCumulativeStakeDistr (..)
  , NonPersistentCommitteeSize (..)
  , PersistentCommitteeSize (..)
  , SeatIndex (..)
  , Stake (..)
  , StakeRole (..)
  , TotalNonPersistentStake (..)
  , TotalPersistentStake (..)
  )

-- | Split a stake distrubution into persistent and non-persistent committee
-- seats according to the weighted Fait-Accompli scheme.
--
-- This function returns:
--   * number of persistent seats granted via the weighted Fait-Accompli scheme
--   * number of non-persistent seats expected to vote via local sortition
--   * total persistent stake
--   * total non-persistent stake
--
-- TODO: this can be more efficient if we compute the values directly on the array.
weightedFaitAccompliSplitSeats ::
  -- | Extended cumulative stake distribution of the potential voters
  ExtCumulativeStakeDistr c ->
  -- | Expected total committee size (persistent + non-persistent)
  CommitteeSize ->
  ( PersistentCommitteeSize
  , NonPersistentCommitteeSize
  , TotalPersistentStake
  , TotalNonPersistentStake
  )
weightedFaitAccompliSplitSeats stakeDistr totalSeats =
  assert (unCommitteeSize totalSeats >= numPersistentVoters) $
    ( PersistentCommitteeSize numPersistentVoters
    , NonPersistentCommitteeSize numNonPersistentVoters
    , TotalPersistentStake persistentStake
    , TotalNonPersistentStake nonPersistentStake
    )
 where
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
    CumulativeStake $
      sum $
        [ voterStake
        | (_, (_, _, LedgerStake voterStake, _)) <- voters
        ]

  (persistentSeats, nonPersistentSeats) =
    List.span
      ( \(seatIndex, (_, _, voterStake, cumulativeStake)) ->
          isAbovePersistentSeatThreshold
            totalSeats
            seatIndex
            voterStake
            cumulativeStake
      )
      $ Array.assocs stakeDistrArray

  stakeDistrArray =
    unExtCumulativeStakeDistr stakeDistr

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
    | voterSeat >= totalSeats =
        False -- Avoid underflow in the right-hand side of the inequality
    | otherwise =
        ( (1 - (voterStake / cumulativeStake))
            ^ (2 :: Integer)
        )
          < ( toRational (totalSeats - voterSeat - 1)
                / toRational (totalSeats - voterSeat)
            )
