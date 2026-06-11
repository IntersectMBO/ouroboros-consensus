module Ouroboros.Consensus.Peras.Voting.Trace (TracePerasVotingEvent (..)) where

import Ouroboros.Consensus.Peras.Voting.Rules (PerasVotingRulesDecision)
import Ouroboros.Consensus.Block (PerasRoundNo)
import Data.Word (Word64)

data TracePerasVotingEvent blk
  = TraceVotingRuleEvent (PerasVotingRulesDecision blk)
  | TraceNoVoteAfterFirstSlotInRound PerasRoundNo Word64 -- slot index
  | TraceNotElectedInRound PerasRoundNo
  -- | TODO: Maybe get rid of that when we no longer read crypto data from env variables
  | TraceCantReadEnv String
  deriving Show
