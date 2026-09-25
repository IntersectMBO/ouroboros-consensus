{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The 'IsPerasVote' projection/injection class.
module Ouroboros.Consensus.Peras.Vote.Class
  ( IsPerasVote (..)
  , getPerasVoteId
  , getPerasVoteTarget
  ) where

import Ouroboros.Consensus.Block.Abstract (Point)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (WithArrivalTime (..))
import Ouroboros.Consensus.Peras.Types
  ( BoostedBlock
  , BoostedBlockCompatibleWithPoint (..)
  , PerasRoundNo
  , PerasSeatIndex
  , PerasVoteId (..)
  , PerasVoteTarget (..)
  )

-- | Types that support being treated as Peras votes
class
  BoostedBlockCompatibleWithPoint (BoostedBlock vote) blk =>
  IsPerasVote vote blk
    | vote -> blk
  where
  getPerasVoteRound :: vote -> PerasRoundNo
  getPerasVoteBlock :: vote -> BoostedBlock vote
  getPerasVoteSeatIndex :: vote -> PerasSeatIndex

  getPerasVotePoint :: vote -> Point blk
  getPerasVotePoint = boostedBlockToPoint . getPerasVoteBlock

-- | Extract the vote ID from a Peras vote container
getPerasVoteId :: IsPerasVote vote blk => vote -> PerasVoteId
getPerasVoteId vote =
  PerasVoteId
    { pviRoundNo = getPerasVoteRound vote
    , pviSeatIndex = getPerasVoteSeatIndex vote
    }

-- | Extract the vote target from a Peras vote container
getPerasVoteTarget :: IsPerasVote vote blk => vote -> PerasVoteTarget blk
getPerasVoteTarget vote =
  PerasVoteTarget
    { pvtRoundNo = getPerasVoteRound vote
    , pvtBlock = getPerasVotePoint vote
    }

instance
  IsPerasVote vote blk =>
  IsPerasVote (WithArrivalTime vote) blk
  where
  getPerasVoteRound = getPerasVoteRound . forgetArrivalTime
  getPerasVoteBlock = getPerasVoteBlock . forgetArrivalTime
  getPerasVoteSeatIndex = getPerasVoteSeatIndex . forgetArrivalTime
