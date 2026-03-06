{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Types related the generic committee selection scheme
module Ouroboros.Consensus.Committee.Types
  ( -- * Voter IDs
    PoolId (..)

    -- * Stake
  , StakeRole (..)
  , Stake (..)
  , TotalPersistentStake (..)
  , TotalNonPersistentStake (..)

    -- * Committee seats
  , SeatIndex (..)
  , NumSeats (..)

    -- * Committee size
  , CommitteeSize (..)
  , PersistentCommitteeSize (..)
  , NonPersistentCommitteeSize (..)

    -- * Cumulative stake distributions
  , ExtCumulativeStakeDistr (..)
  , mkExtCumulativeStakeDistr
  ) where

import Cardano.Ledger.Core (KeyHash, KeyRole (..))
import Data.Array (Array, Ix, listArray)
import Data.Kind (Type)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)

-- * Pool IDs

-- | Identifier of a given voter in the committee selection scheme
newtype PoolId = PoolId {unPoolId :: KeyHash StakePool}
  deriving (Show, Eq, Ord)

-- * Stake

-- | Role of a given stake
data StakeRole
  = -- | Stake as reflected by the ledger state
    Ledger
  | -- | Stake used when voting
    Weight
  | -- | Cumulative stake used internally
    Cumulative
  deriving (Show, Eq)

-- | Stake of a given party, annotated with the role it might play
type Stake :: StakeRole -> Type
data family Stake r

newtype instance Stake Ledger = LedgerStake Rational
newtype instance Stake Weight = VoteWeight Rational
newtype instance Stake Cumulative = CumulativeStake Rational

-- | Total persistent stake
newtype TotalPersistentStake = TotalPersistentStake
  { unTotalPersistentStake :: Stake Cumulative
  }

-- | Total non-persistent stake
newtype TotalNonPersistentStake = TotalNonPersistentStake
  { unTotalNonPersistentStake :: Stake Cumulative
  }

-- * Committee seats

-- | Persistent seat index in the voting committee
newtype SeatIndex = SeatIndex {unSeatIndex :: Word64}
  deriving (Show, Eq, Ord, Enum, Ix)

-- | Number of seats granted to a voter (used for non-persistent voters)
newtype NumSeats = NumSeats {unNumSeats :: Word64}
  deriving (Show, Eq, Ord)

-- * Committee size

-- | Target committee size
newtype CommitteeSize = CommitteeSize {unCommitteeSize :: Word64}
  deriving (Show, Eq, Ord)

-- | Persistent committee size
newtype PersistentCommitteeSize = PersistentCommitteeSize
  { unPersistentCommitteeSize :: Word64
  }
  deriving (Show, Eq, Ord)

-- | Non-persistent committee size
newtype NonPersistentCommitteeSize = NonPersistentCommitteeSize
  { unNonPersistentCommitteeSize :: Word64
  }
  deriving (Show, Eq, Ord)

-- * Cumulative stake distributions

-- | Extended cumulative stake distribution for a given role.
--
-- Stake distribution in descending order, extended with the voter's ID (both
-- PoolId and their public key) as well as their right-cumulative stake, i.e.,
-- the total stake of voters with smaller or equal stake than the current one
-- (including the current one itself).
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
newtype ExtCumulativeStakeDistr publicKey
  = ExtCumulativeStakeDistr
  { unExtCumulativeStakeDistr ::
      Array
        SeatIndex
        ( PoolId
        , publicKey
        , Stake Ledger -- Ledger stake of this voter
        , Stake Cumulative -- Right-cumulative ledger stake of this voter
        )
  }

-- | Construct an extended cumulative stake distribution
mkExtCumulativeStakeDistr ::
  Map PoolId (Stake Ledger, publicKey) ->
  ExtCumulativeStakeDistr publicKey
mkExtCumulativeStakeDistr pools =
  ExtCumulativeStakeDistr
    . listArray (SeatIndex 0, SeatIndex (numPools - 1))
    . snd
    . List.mapAccumR accumStake (CumulativeStake 0)
    . List.sortBy descendingStake
    . Map.toList
    $ pools
 where
  numPools =
    fromIntegral (Map.size pools)

  descendingStake
    (_, (LedgerStake x, _))
    (_, (LedgerStake y, _)) =
      flip compare y x

  accumStake
    (CumulativeStake stakeAccR)
    (poolId, (LedgerStake poolStake, poolPublicKey)) =
      let stakeAccR' = stakeAccR + poolStake
       in ( CumulativeStake stakeAccR'
          ,
            ( poolId
            , poolPublicKey
            , LedgerStake poolStake
            , CumulativeStake stakeAccR'
            )
          )
