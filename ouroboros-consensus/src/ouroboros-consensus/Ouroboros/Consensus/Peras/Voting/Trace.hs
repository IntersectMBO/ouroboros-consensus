{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Ouroboros.Consensus.Peras.Voting.Trace (TracePerasVotingEvent (..)) where

import Data.Word (Word64)

import Ouroboros.Consensus.Peras.Voting.Rules (PerasVotingRulesDecision)
import Ouroboros.Consensus.Block (PerasRoundNo, PerasVote, StandardHash, ValidatedPerasVote)
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime)

data TracePerasVotingEvent blk
  = TracePerasVotingRuleEvent (PerasVotingRulesDecision blk)
  | TracePerasVotingNoVoteAfterFirstSlotInRound PerasRoundNo Word64 -- slot index
  | TracePerasVotingNotAVoterInRound PerasRoundNo
  | TracePerasVotingForgedVote (ValidatedPerasVote blk)
  | TracePerasVotingAddedVoteToDB (WithArrivalTime (ValidatedPerasVote blk))
  -- | TODO: Maybe get rid of that when we no longer read crypto data from env variables
  | TracePerasVotingCantReadEnv String

deriving instance (Show (PerasVote blk), StandardHash blk) => Show (TracePerasVotingEvent blk)
