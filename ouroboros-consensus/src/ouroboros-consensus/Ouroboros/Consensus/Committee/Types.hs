{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Types common to any generic committee selection scheme
module Ouroboros.Consensus.Committee.Types
  ( PoolId (..)
  , LedgerStake (..)
  , VoteWeight (..)
  , TargetCommitteeSize (..)
  , Cumulative (..)
  ) where

import Cardano.Binary (FromCBOR (..), ToCBOR (..))
import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.BaseTypes (HasZero)
import Cardano.Ledger.Core (KeyHash (..), KeyRole (..))
import Cardano.Prelude (Generic)
import Control.DeepSeq (NFData)
import Data.Semigroup (Sum (..))
import Data.Word (Word64)
import NoThunks.Class (NoThunks)

-- | Identifier of a given voter in the committee selection scheme
newtype PoolId = PoolId
  { unPoolId :: KeyHash StakePool
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

instance FromCBOR PoolId where
  fromCBOR = do
    bytes <- fromCBOR
    case Hash.hashFromBytes bytes of
      Just hash ->
        return (PoolId (KeyHash hash))
      Nothing ->
        fail ("failed to decode PoolId, invalid hash bytes: " <> show bytes)

instance ToCBOR PoolId where
  toCBOR (PoolId hash) =
    toCBOR (Hash.hashToBytes (unKeyHash hash))

-- | Stake of a voter as reflected by the ledger state
newtype LedgerStake = LedgerStake
  { unLedgerStake :: Rational
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, HasZero, FromCBOR, ToCBOR)
  deriving anyclass NoThunks

-- | Relative voting power of a voter in the committee selection scheme
newtype VoteWeight = VoteWeight
  { unVoteWeight :: Rational
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Fractional, NFData, FromCBOR, ToCBOR)
  deriving anyclass NoThunks
  deriving Semigroup via Sum Rational
  deriving Monoid via Sum Rational

-- | Target committee size
newtype TargetCommitteeSize = TargetCommitteeSize
  { unTargetCommitteeSize :: Word64
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromCBOR, ToCBOR)
  deriving anyclass NoThunks

-- | Wrapper to tag accumulated resources
newtype Cumulative a = Cumulative
  { unCumulative :: a
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromCBOR, ToCBOR)
  deriving anyclass NoThunks
