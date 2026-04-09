{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Types common to any generic committee selection scheme
module Ouroboros.Consensus.Committee.Types
  ( PoolId (..)
  , LedgerStake (..)
  , VoteWeight (..)
  , TargetCommitteeSize (..)
  , Cumulative (..)
  ) where

import Cardano.Ledger.BaseTypes (HasZero)
import Cardano.Ledger.Core (KeyHash, KeyRole (..))
import Data.Word (Word64)

-- | Identifier of a given voter in the committee selection scheme
newtype PoolId = PoolId
  { unPoolId :: KeyHash StakePool
  }
  deriving (Show, Eq, Ord)

-- | Stake of a voter as reflected by the ledger state
newtype LedgerStake = LedgerStake
  { unLedgerStake :: Rational
  }
  deriving (Show, Eq)
  deriving newtype (Num, HasZero)

-- | Voting power of a voter in the committee selection scheme
newtype VoteWeight = VoteWeight
  { unVoteWeight :: Rational
  }
  deriving (Show, Eq)

-- | Target committee size
newtype TargetCommitteeSize = TargetCommitteeSize
  { unTargetCommitteeSize :: Word64
  }
  deriving (Show, Eq)

-- | Wrapper to tag accumulated resources
newtype Cumulative a = Cumulative
  { unCumulative :: a
  }
  deriving (Show, Eq)
