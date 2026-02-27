{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Types related the generic committee selection scheme
module Ouroboros.Consensus.Committee.Types
  ( -- * Stake
    StakeRole (..)
  , Stake (..)

    -- * Voter IDs
  , VoterId (..)

    -- * Committee seats
  , SeatIndex (..)
  , NumSeats (..)

    -- * Committee size
  , CommitteeSize (..)

    -- * Cummulative stake distributions
  , CumulativeStakeDistr (..)
  , mkCummulativeStakeDistr
  , cumulativeStakeDistrToList
  , cumulativeStakeDistrLookup
  ) where

import Cardano.Ledger.Core (KeyHash, KeyRole (..))
import Cardano.Ledger.State (IndividualPoolStake (..), PoolDistr (..))
import Data.Kind (Type)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)

-- * Stake

-- | Role of a given stake
data StakeRole
  = -- | Stake as reflected by the ledger state
    Ledger
  | -- | Stake used when voting
    Vote
  | -- | Cummulative ledger used internally
    Cumulative
  deriving (Show, Eq)

-- | Stake of a given party, annotated with the role it might play
type Stake :: StakeRole -> Type
data family Stake r

newtype instance Stake Ledger = LedgerStake Rational
newtype instance Stake Vote = VoteStake Rational
newtype instance Stake Cumulative = CumulativeStake Rational

-- * Voter IDs

-- | Identifier of a given voter in the committee selection scheme
newtype VoterId = VoterId {unVoterId :: KeyHash StakePool}
  deriving (Show, Eq, Ord)

-- * Committee seats

-- | Persistent seat index in the voting committee
newtype SeatIndex = SeatIndex {unSeatIndex :: Word64}
  deriving (Show, Eq, Ord, Enum)

-- | Number of seats granted to a voter (used for non-persistent voters)
newtype NumSeats = NumSeats {unNumSeats :: Word64}
  deriving (Show, Eq, Ord)

-- * Committee size

-- | Size of a voting committee (used for both expected and actual sizes)
newtype CommitteeSize = CommitteeSize {unCommitteeSize :: Word64}
  deriving (Show, Eq, Ord)

-- * Cummulative stake distributions

-- | Cummulative stake distribution for a given role.
--
-- Stake distribution in descending order, annotated with both the voter's
-- (potential) persistent seat index, as well as their right-cumulative stake,
-- i.e., the total stake of voters with smaller or equal stake than the
-- current one (including the current one itself).
--
-- E.g.: given the following stake distribution:
--
-- @
--   VoterId 1 -> 50
--   VoterId 2 -> 15
--   VoterId 3 -> 10
--   VoterId 4 -> 20
--   VoterId 5 -> 5
-- @
--
--  We would have the following cummulative stake distribution:
--
-- @
--   Map.fromList
--     [ (VoterId 1, SeatIndex 0, LedgerStake 50, CumulativeStake 100)
--     , (VoterId 4, SeatIndex 1, LedgerStake 20, CumulativeStake 50)
--     , (VoterId 2, SeatIndex 2, LedgerStake 15, CumulativeStake 30)
--     , (VoterId 3, SeatIndex 3, LedgerStake 10, CumulativeStake 15)
--     , (VoterId 5, SeatIndex 4, LedgerStake 5,  CumulativeStake 5)
--     ]
-- @
--
-- NOTE: this wrapper exists to allow us to share the same cummulative stake
-- distribution across multiple committee selection instances derived from the
-- same underlying stake distribution (e.g. Leios and Peras voting committees
-- for the same epoch).
newtype CumulativeStakeDistr
  = CumulativeStakeDistr
  { unCummulativeStakeDistr ::
      Map
        VoterId
        ( SeatIndex
        , Stake Ledger -- Ledger stake of this voter
        , Stake Cumulative -- Right-cummulative ledger stake of this voter
        )
  }

-- | Build a 'CumulativeStakeDistr' from a regular ledger stake distribution
mkCummulativeStakeDistr ::
  PoolDistr ->
  CumulativeStakeDistr
mkCummulativeStakeDistr poolDistr =
  CumulativeStakeDistr $
    Map.fromList $
      snd $
        List.mapAccumR
          accum
          ( SeatIndex 0 -- initial seat index
          , CumulativeStake 0 -- initial right cumulative stake
          )
          (Map.toDescList (unPoolDistr poolDistr))
 where
  accum (SeatIndex idx, CumulativeStake stakeAccR) (poolId, poolStake) =
    ( (SeatIndex idx', CumulativeStake stakeAccR')
    ,
      ( VoterId poolId
      , (SeatIndex idx, LedgerStake voterStake, CumulativeStake stakeAccR')
      )
    )
   where
    voterStake = individualPoolStake poolStake
    stakeAccR' = stakeAccR + voterStake
    idx' = succ idx

-- | Convert a 'CumulativeStakeDistr' back to a list in descending order
cumulativeStakeDistrToList ::
  CumulativeStakeDistr ->
  [(SeatIndex, VoterId, Stake Ledger, Stake Cumulative)]
cumulativeStakeDistrToList (CumulativeStakeDistr distr) =
  [ (seatIndex, voterId, ledgerStake, cumulativeStake)
  | (voterId, (seatIndex, ledgerStake, cumulativeStake)) <-
      Map.toDescList distr
  ]

-- | Lookup a given voter in a 'CumulativeStakeDistr'
cumulativeStakeDistrLookup ::
  CumulativeStakeDistr ->
  VoterId ->
  Maybe (SeatIndex, Stake Ledger, Stake Cumulative)
cumulativeStakeDistrLookup (CumulativeStakeDistr distr) voterId =
  Map.lookup voterId distr
