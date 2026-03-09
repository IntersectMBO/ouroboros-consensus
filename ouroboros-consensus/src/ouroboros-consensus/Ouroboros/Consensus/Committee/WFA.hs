{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Deterministic portion of the Weighted Fait-Accompli committee selection scheme
module Ouroboros.Consensus.Committee.WFA
  ( -- * Weighted Fait-Accompli committee selection scheme
    PersistentCommitteeSize (..)
  , NonPersistentCommitteeSize (..)
  , TotalPersistentStake (..)
  , TotalNonPersistentStake (..)
  , weightedFaitAccompliSplitSeats
  , isAbovePersistentSeatThreshold

    -- * Cumulative stake distributions
  , SeatIndex (..)
  , ExtWFAStakeDistr (..)
  , mkExtWFAStakeDistr
  ) where

import Control.Exception (assert)
import Data.Array (Array, Ix, listArray)
import qualified Data.Array as Array
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Ouroboros.Consensus.Committee.Types
  ( Cumulative (..)
  , LedgerStake (..)
  , PoolId
  , TargetCommitteeSize (..)
  )

-- * Weighted Fait-Accompli committee selection scheme

-- | Persistent committee size
newtype PersistentCommitteeSize = PersistentCommitteeSize
  { unPersistentCommitteeSize :: Word64
  }
  deriving (Show, Eq)

-- | Non-persistent committee size
newtype NonPersistentCommitteeSize = NonPersistentCommitteeSize
  { unNonPersistentCommitteeSize :: Word64
  }
  deriving (Show, Eq)

-- | Total persistent stake
newtype TotalPersistentStake = TotalPersistentStake
  { unTotalPersistentStake :: Cumulative LedgerStake
  }
  deriving (Show, Eq)

-- | Total non-persistent stake
newtype TotalNonPersistentStake = TotalNonPersistentStake
  { unTotalNonPersistentStake :: Cumulative LedgerStake
  }
  deriving (Show, Eq)

-- | Split a stake distrubution into persistent and non-persistent committee
-- seats according to the weighted Fait-Accompli scheme.
--
-- This function returns:
--   * number of persistent seats granted via the weighted Fait-Accompli scheme
--   * number of non-persistent seats expected to vote via local sortition
--   * total persistent stake
--   * total non-persistent stake
weightedFaitAccompliSplitSeats ::
  -- | Extended cumulative stake distribution of the potential voters
  ExtWFAStakeDistr c ->
  -- | Expected total committee size (persistent + non-persistent)
  TargetCommitteeSize ->
  ( PersistentCommitteeSize
  , NonPersistentCommitteeSize
  , TotalPersistentStake
  , TotalNonPersistentStake
  )
weightedFaitAccompliSplitSeats stakeDistr totalSeats =
  assert (numPersistentVoters <= unTargetCommitteeSize totalSeats) $
    ( PersistentCommitteeSize numPersistentVoters
    , NonPersistentCommitteeSize numNonPersistentVoters
    , TotalPersistentStake (Cumulative (LedgerStake persistentStake))
    , TotalNonPersistentStake (Cumulative (LedgerStake nonPersistentStake))
    )
 where
  stakeDistrArray =
    unExtWFAStakeDistr stakeDistr

  ( numPersistentVoters
    , persistentStake
    , nonPersistentStake
    ) =
      traverseSeats (Array.bounds stakeDistrArray) True 0 0 0

  numNonPersistentVoters =
    unTargetCommitteeSize totalSeats
      - numPersistentVoters

  traverseSeats
    (currSeatIndex, lastSeatIndex)
    checkPersistentSeatThreshold
    accNumPersistentVoters
    accPersistentStake
    accNonPersistentStake
      -- Reached the end
      | currSeatIndex > lastSeatIndex =
          ( accNumPersistentVoters
          , accPersistentStake
          , accNonPersistentStake
          )
      -- The current voter is persistent
      | isPersistent =
          traverseSeats
            (succ currSeatIndex, lastSeatIndex)
            True
            (accNumPersistentVoters + 1)
            (accPersistentStake + voterStake)
            accNonPersistentStake
      -- The current voter is non-persistent
      | otherwise =
          traverseSeats
            (succ currSeatIndex, lastSeatIndex)
            False
            accNumPersistentVoters
            accPersistentStake
            (accNonPersistentStake + voterStake)
     where
      -- Extract the entry in the array corresponding to the current seat index
      (_, _, LedgerStake voterStake, cumulativeStake) =
        (Array.!) stakeDistrArray currSeatIndex

      -- Check whether the current voter can be granted a persistent seat
      isPersistent =
        -- NOTE: because the check should behave monotonically, we can skip it
        -- entirely after the first non-persistent voter is found.
        checkPersistentSeatThreshold
          && isAbovePersistentSeatThreshold
            totalSeats
            currSeatIndex
            (LedgerStake voterStake)
            cumulativeStake

-- | Evaluate whether a voter with its give stake and relatile position in the
-- stake distribution can be granted a persistent seat in the voting committee.
isAbovePersistentSeatThreshold ::
  -- | Total committee size (persistent + non-persistent)
  TargetCommitteeSize ->
  -- | Current voter seat index
  SeatIndex ->
  -- | Current voter stake
  LedgerStake ->
  -- | Cumulated stake of voters with smaller or equal stake than the current one
  Cumulative LedgerStake ->
  -- | Whether the current voter has a persistent seat or not
  Bool
isAbovePersistentSeatThreshold
  (TargetCommitteeSize totalSeats)
  (SeatIndex voterSeat)
  (LedgerStake voterStake)
  (Cumulative (LedgerStake cumulativeStake))
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

-- * Cumulative stake distributions

-- | Seat index in the voting committee
newtype SeatIndex = SeatIndex
  { unSeatIndex :: Word64
  }
  deriving (Show, Eq, Ord, Enum, Ix)

-- | Extended cumulative stake distribution.
--
-- Stake distribution in descending order with precomputed right-cumulative
-- stake, i.e., the total stake of voters with smaller or equal stake than the
-- current one (including the current one itself). In addition, this wrapper
-- also allows the inclusion of an arbitrary payload of type @a@. This is useful
-- to keep track of anything else we might need to know about the voters in the
-- committee selection scheme (e.g. their public keys) in a single place.
--
-- E.g.: given the following stake distribution:
--
-- @
--   PoolId 1 -> (50, PK#1)
--   PoolId 2 -> (15, PK#2)
--   PoolId 3 -> (10, PK#3)
--   PoolId 4 -> (20, PK#4)
--   PoolId 5 -> (5,  PK#5)
-- @
--
--  We would have the following cumulative stake distribution:
--
-- @
--   Array.listArray
--     (SeatIndex 0, SeatIndex 4)
--     [ (PoolId 1, PK#1, LedgerStake 50, CumulativeStake 100)
--     , (PoolId 4, PK#4, LedgerStake 20, CumulativeStake 50)
--     , (PoolId 2, PK#2, LedgerStake 15, CumulativeStake 30)
--     , (PoolId 3, PK#3, LedgerStake 10, CumulativeStake 15)
--     , (PoolId 5, PK#5, LedgerStake 5,  CumulativeStake 5)
--     ]
-- @
--
-- NOTE: this wrapper exists to allow us to share the same cumulative stake
-- distribution across multiple committee selection instances derived from the
-- same underlying stake distribution (e.g. Leios and Peras voting committees
-- for the same epoch).
newtype ExtWFAStakeDistr a = ExtWFAStakeDistr
  { unExtWFAStakeDistr ::
      Array
        SeatIndex
        ( PoolId -- Voter ID of this voter
        , a -- Extra payload associated to this voter
        , LedgerStake -- Ledger stake of this voter
        , Cumulative LedgerStake -- Right-cumulative ledger stake of this voter
        )
  }

-- | Construct an extended cumulative stake distribution
mkExtWFAStakeDistr ::
  Map PoolId (LedgerStake, a) ->
  ExtWFAStakeDistr a
mkExtWFAStakeDistr pools =
  ExtWFAStakeDistr
    . listArray (SeatIndex 0, SeatIndex (numPools - 1))
    . snd
    . List.mapAccumR accumStake (Cumulative (LedgerStake 0))
    . List.sortBy descendingStake
    . Map.toList
    $ pools
 where
  numPools =
    fromIntegral (Map.size pools)

  descendingStake
    (_, (LedgerStake x, _))
    (_, (LedgerStake y, _)) =
      compare y x

  accumStake
    (Cumulative (LedgerStake stakeAccR))
    (poolId, (LedgerStake poolStake, poolPublicKey)) =
      let stakeAccR' = stakeAccR + poolStake
       in ( Cumulative (LedgerStake stakeAccR')
          ,
            ( poolId
            , poolPublicKey
            , LedgerStake poolStake
            , Cumulative (LedgerStake stakeAccR')
            )
          )
