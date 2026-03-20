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
  , NumPoolsWithPositiveStake (..)
  , WFAError (..)
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

-- | Errors that can occur when trying to split the stake distribution into
-- persistent and seats via weighted Fait-Accompli.
data WFAError
  = -- | The underlying stake distribution is empty
    EmptyStakeDistribution
  | -- | The target committee size is larger than the number of pools with positive
    -- stake in the underlying stake distribution, which would lead to incorrect
    -- results (e.g. granting persistent seats to voters with zero stake).
    NotEnoughPoolsWithPositiveStake
      TargetCommitteeSize
      NumPoolsWithPositiveStake
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
  Either
    WFAError
    ( PersistentCommitteeSize
    , NonPersistentCommitteeSize
    , TotalPersistentStake
    , TotalNonPersistentStake
    )
weightedFaitAccompliSplitSeats extWFAStakeDistr totalSeats
  -- The target committee size must not be not larger than the actual number of
  -- pools with positive stake in the underlying stake distribution. Otherwise,
  -- it could lead to incorrect/non-desirable results (e.g., granting persistent
  -- seats to voters with zero stake).
  | notEnoughPoolsWithPositiveStake =
      Left
        ( NotEnoughPoolsWithPositiveStake
            totalSeats
            (numPoolsWithPositiveStake extWFAStakeDistr)
        )
  | otherwise =
      -- We should have /at most/ as many persistent voters as the total
      -- committee size, but not more.
      assert (numPersistentVoters <= unTargetCommitteeSize totalSeats) $
        Right
          ( PersistentCommitteeSize numPersistentVoters
          , NonPersistentCommitteeSize numNonPersistentVoters
          , TotalPersistentStake (Cumulative (LedgerStake persistentStake))
          , TotalNonPersistentStake (Cumulative (LedgerStake nonPersistentStake))
          )
 where
  notEnoughPoolsWithPositiveStake =
    unNumPoolsWithPositiveStake (numPoolsWithPositiveStake extWFAStakeDistr)
      < unTargetCommitteeSize totalSeats

  stakeDistrArray =
    unExtWFAStakeDistr extWFAStakeDistr

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

-- | Number of pools with positive stake in the underlying stake distribution
newtype NumPoolsWithPositiveStake = NumPoolsWithPositiveStake
  { unNumPoolsWithPositiveStake :: Word64
  }
  deriving (Show, Eq)

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
data ExtWFAStakeDistr a = ExtWFAStakeDistr
  { unExtWFAStakeDistr ::
      Array
        SeatIndex
        ( PoolId -- Voter ID of this voter
        , a -- Extra payload associated to this voter
        , LedgerStake -- Ledger stake of this voter
        , Cumulative LedgerStake -- Right-cumulative ledger stake of this voter
        )
  , numPoolsWithPositiveStake :: NumPoolsWithPositiveStake
  -- ^ Number of pools with positive stake in the underlying stake distribution.
  -- This is also precomputed at the beginning of the epoch to prevent invalid
  -- weighted Fait-Accompli instantiations with a target committee size larger
  -- than the number of pools with positive stake, which would lead to incorrect
  -- results (e.g. granting persistent seats to voters with zero stake).
  }

-- | Construct an extended cumulative stake distribution.
--
-- Returns an error if the underlying stake distribution is empty.
mkExtWFAStakeDistr ::
  Map PoolId (LedgerStake, a) ->
  Either WFAError (ExtWFAStakeDistr a)
mkExtWFAStakeDistr pools
  | Map.null pools =
      Left
        EmptyStakeDistribution
  | otherwise =
      Right
        ExtWFAStakeDistr
          { unExtWFAStakeDistr = stakeDistrArray
          , numPoolsWithPositiveStake = numPoolsWithPositiveStakeAcc
          }
 where
  stakeDistrArray =
    listArray
      ( SeatIndex 0
      , SeatIndex (fromIntegral (Map.size pools) - 1)
      )
      cumulativeStakeAndPools

  ((_totalStake, numPoolsWithPositiveStakeAcc), cumulativeStakeAndPools) =
    List.mapAccumR
      accumStakeAndCountPoolsWithPositiveStake
      ( Cumulative (LedgerStake 0)
      , NumPoolsWithPositiveStake 0
      )
      . List.sortBy descendingStake
      . Map.toList
      $ pools

  descendingStake
    (_, (LedgerStake x, _))
    (_, (LedgerStake y, _)) =
      compare y x

  accumStakeAndCountPoolsWithPositiveStake
    (Cumulative (LedgerStake stakeAccR), NumPoolsWithPositiveStake numPoolsAccR)
    (poolId, (LedgerStake poolStake, poolPublicKey)) =
      let stakeAccR' =
            stakeAccR + poolStake
          numPoolsAccR'
            | poolStake > 0 = numPoolsAccR + 1
            | otherwise = numPoolsAccR
       in (
            ( Cumulative (LedgerStake stakeAccR')
            , NumPoolsWithPositiveStake numPoolsAccR'
            )
          ,
            ( poolId
            , poolPublicKey
            , LedgerStake poolStake
            , Cumulative (LedgerStake stakeAccR')
            )
          )
