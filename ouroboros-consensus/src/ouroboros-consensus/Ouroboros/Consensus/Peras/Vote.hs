module Ouroboros.Consensus.Peras.Vote
 ( module X
 , TraceVotingEvent (..)
 ) where

import Ouroboros.Consensus.Peras.Vote.Aggregation as X
import Ouroboros.Consensus.Peras.Voting.Rules (PerasVotingRulesDecision)
import Ouroboros.Consensus.Block (PerasRoundNo, SlotNo)
import Data.Word (Word64)

-- | TODO: replace with actual type parameter for committee selection when known
data TraceVotingEvent
  = TraceCommitteeSelectionEvent ()
  | TraceVotingRuleEvent PerasVotingRulesDecision
  | TraceNoPerasEnabledAtSlot SlotNo
  | TraceNoVoteAfterFirstSlotInRound PerasRoundNo Word64 -- slot index
  | TraceNotElectedInRound PerasRoundNo
  | TraceNoPerasCandidateBeforeSlot SlotNo
  deriving Show
