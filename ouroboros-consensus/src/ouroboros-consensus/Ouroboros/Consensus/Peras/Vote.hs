{-# LANGUAGE LambdaCase #-}

module Ouroboros.Consensus.Peras.Vote
 ( module X
 , TraceVotingEvent (..)
 ) where

import Ouroboros.Consensus.Peras.Vote.Aggregation as X
import Ouroboros.Consensus.Peras.Voting.Rules (PerasVotingRulesDecision)
import Ouroboros.Consensus.Util.Pred (Explainable (..), ExplanationMode (..))

-- | TODO: remove type parameter when actual types are known for committee selection
data TraceVotingEvent electionEvt
  = TraceCommitteeSelectionEvent electionEvt
  | TraceVotingRuleEvent PerasVotingRulesDecision

instance Explainable electionEvt => Explainable (TraceVotingEvent electionEvt) where
  explain mode = \case
    TraceCommitteeSelectionEvent evt -> "CommitteeSelection(" <> explain mode evt <> ")"
    TraceVotingRuleEvent evt -> "VotingRule(" <> explain mode evt <> ")"

instance Explainable electionEvt => Show (TraceVotingEvent electionEvt) where
  show = explain Shallow
