{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Peras.Voting.Trace (TracePerasVoteForgingEvent (..)) where

import Data.Word (Word64)
import Ouroboros.Consensus.Block
  ( PerasCert
  , PerasRoundNo
  , PerasVote
  , StandardHash
  , ValidatedPerasVote
  )
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime)
import Ouroboros.Consensus.Peras.Voting.Rules (PerasVotingRulesDecision)
import Ouroboros.Consensus.Storage.ChainDB (AddPerasCertChainSelOutcome, AddPerasVoteResult)

data TracePerasVoteForgingEvent blk
  = TracePerasVotingRuleEvent (PerasVotingRulesDecision blk)
  | TracePerasVotingNoVoteAfterFirstSlotInRound PerasRoundNo Word64 -- slot index
  | TracePerasVotingNotAVoterInRound PerasRoundNo
  | TracePerasVotingForgedVote (WithArrivalTime (ValidatedPerasVote blk))
  | TracePerasVotingAddVoteResult (AddPerasVoteResult blk)
  | TracePerasVotingAddCertChainSelOutcome AddPerasCertChainSelOutcome
  | -- | TODO: Maybe get rid of that when we no longer read crypto data from env variables
    TracePerasVotingCantReadEnv String

deriving instance
  ( Show (PerasVote blk)
  , Show (PerasCert blk)
  , StandardHash blk
  ) =>
  Show (TracePerasVoteForgingEvent blk)
