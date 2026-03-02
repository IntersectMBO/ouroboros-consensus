{-# LANGUAGE LambdaCase #-}

module Ouroboros.Consensus.Peras.Vote
 ( module X
 , TraceVotingEvent (..)
 ) where

import Ouroboros.Consensus.Peras.Vote.Aggregation as X
import Ouroboros.Consensus.Peras.Voting.Rules (PerasVotingRulesDecision)
import Ouroboros.Consensus.Util.Pred (Explainable (..), ExplanationMode (..), ShowExplain)
import Ouroboros.Consensus.Block (SlotNo)
import Data.Word (Word64)

-- | TODO: replace with actual type parameter for committee selection when known
data TraceVotingEvent
  = TraceCommitteeSelectionEvent (ShowExplain ())
  | TraceVotingRuleEvent PerasVotingRulesDecision
  | TraceNoPerasEnabledAtSlot SlotNo
  | TraceNoVoteAfterFirstSlotInRound Word64 -- slot index

instance Explainable TraceVotingEvent where
  explain mode = \case
    TraceCommitteeSelectionEvent evt -> "CommitteeSelection(" <> explain mode evt <> ")"
    TraceVotingRuleEvent evt -> "VotingRule(" <> explain mode evt <> ")"

instance Show TraceVotingEvent where
  show = explain Shallow
