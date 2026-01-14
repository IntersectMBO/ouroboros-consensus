{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Peras.Vote
  ( PerasVote (..)
  , PerasVoterId (..)
  , PerasVoteStake (..)
  , PerasVoteStakeDistr (..)
  , PerasVoteTarget (..)
  , PerasVoteId (..)
  , stakeAboveThreshold
  ) where

import qualified Cardano.Binary as KeyHash
import Cardano.Ledger.Core (KeyHash (..), KeyRole (..))
import Codec.Serialise (Serialise (..))
import Codec.Serialise.Decoding (decodeListLenOf)
import Codec.Serialise.Encoding (encodeListLen)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy (..))
import Data.Semigroup (Sum (..))
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (HeaderHash, Point)
import Ouroboros.Consensus.Peras.Params
  ( PerasParams (..)
  , PerasQuorumStakeThreshold (..)
  )
import Ouroboros.Consensus.Peras.Round (PerasRoundNo)
import Ouroboros.Consensus.Util (ShowProxy (..))
import Quiet (Quiet (..))

data PerasVote blk = PerasVote
  { pvVoteRound :: PerasRoundNo
  , pvVoteBlock :: Point blk
  , pvVoteVoterId :: PerasVoterId
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass NoThunks

instance ShowProxy blk => ShowProxy (PerasVote blk) where
  showProxy _ = "PerasVote " <> showProxy (Proxy @blk)

instance Serialise (HeaderHash blk) => Serialise (PerasVote blk) where
  encode PerasVote{pvVoteRound, pvVoteBlock, pvVoteVoterId} =
    encodeListLen 3
      <> encode pvVoteRound
      <> encode pvVoteBlock
      <> KeyHash.toCBOR (unPerasVoterId pvVoteVoterId)
  decode = do
    decodeListLenOf 3
    pvVoteRound <- decode
    pvVoteBlock <- decode
    pvVoteVoterId <- PerasVoterId <$> KeyHash.fromCBOR
    pure $ PerasVote{pvVoteRound, pvVoteBlock, pvVoteVoterId}

newtype PerasVoterId = PerasVoterId
  { unPerasVoterId :: KeyHash StakePool
  }
  deriving newtype NoThunks
  deriving stock (Eq, Ord, Generic)
  deriving Show via Quiet PerasVoterId

-- NOTE: At the moment there is no consensus from researchers/engineers on how
-- we go from the absolute stake of a voter in the ledger to the relative stake
-- of their vote in the voting commitee (given that the quorum is expressed as
-- a relative value of the voting commitee total stake).
--
-- So, for now you can consider this 'Rational' as the best approximation we
-- have at the moment of the concrete type for a relative vote stake that can be
-- compared to the quorum threshold value (also currently a 'Rational').
newtype PerasVoteStake = PerasVoteStake
  { unPerasVoteStake :: Rational
  }
  deriving newtype (Eq, Ord, Num, Fractional, NoThunks, Serialise)
  deriving stock Generic
  deriving Show via Quiet PerasVoteStake
  deriving Semigroup via Sum Rational
  deriving Monoid via Sum Rational

newtype PerasVoteStakeDistr = PerasVoteStakeDistr
  { unPerasVoteStakeDistr :: Map PerasVoterId PerasVoteStake
  }
  deriving newtype NoThunks
  deriving stock (Show, Eq, Generic)

data PerasVoteTarget blk = PerasVoteTarget
  { pvtRoundNo :: !PerasRoundNo
  , pvtBlock :: !(Point blk)
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

data PerasVoteId blk = PerasVoteId
  { pviRoundNo :: !PerasRoundNo
  , pviVoterId :: !PerasVoterId
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass NoThunks

-- | Check whether a given vote stake is above the quorum threshold
stakeAboveThreshold :: PerasParams -> PerasVoteStake -> Bool
stakeAboveThreshold params stake =
  unPerasVoteStake stake
    >= unPerasQuorumStakeThreshold (perasQuorumStakeThreshold params)
