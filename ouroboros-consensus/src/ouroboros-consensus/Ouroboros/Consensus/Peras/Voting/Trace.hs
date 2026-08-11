{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Peras.Voting.Trace
  ( TracePerasVoteForgingEvent (..)
  ) where

import Data.Word (Word64)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block
  ( PerasCert
  , PerasRoundNo
  , PerasVote
  , StandardHash
  , ValidatedPerasVote
  )
import Ouroboros.Consensus.BlockchainTime (WithArrivalTime)
import Ouroboros.Consensus.Peras.Voting.Rules (PerasVotingRulesDecision)
import Ouroboros.Consensus.Storage.ChainDB
  ( AddPerasCertChainSelOutcome
  , AddPerasVoteResult
  )

-- | Peras vote forging events.
data TracePerasVoteForgingEvent blk
  = -- | We do nothing unless we are on the first slot of a round
    TracePerasVotingNoVoteAfterFirstSlotInRound
      -- | The current round number
      PerasRoundNo
      -- | The current slot number within the round
      Word64
  | -- | We do nothing if we are not eligible to vote in the current round
    TracePerasVotingNotAVoterInRound
      -- | The current round number
      PerasRoundNo
  | -- | The decision made by the voting rules for a given round
    TracePerasVotingRulesDecision
      -- | The current round number
      PerasRoundNo
      -- | The reason to forge or not forge a vote
      (PerasVotingRulesDecision blk)
  | -- | We forged a vote for the current round
    TracePerasVotingForgedVote
      -- | The current round number
      PerasRoundNo
      -- | The forged vote
      (WithArrivalTime (ValidatedPerasVote blk))
  | -- | The result of adding the vote to the PerasVoteDB
    TracePerasVotingAddVoteResult
      -- | The current round number
      PerasRoundNo
      -- | The result of adding the vote to the chain
      (AddPerasVoteResult blk)
  | -- | The result of adding the certificate to the PerasCertDB
    TracePerasVotingAddCertChainSelOutcome
      -- | The current round number
      PerasRoundNo
      -- | The result of adding the certificate to the chain
      AddPerasCertChainSelOutcome
  | -- | TODO: get rid of this when we no longer read stuff from env variables
    TracePerasVotingCantReadEnv String

deriving instance
  ( StandardHash blk
  , Show (PerasVote blk)
  , Show (PerasCert blk)
  ) =>
  Show (TracePerasVoteForgingEvent blk)
deriving instance
  ( StandardHash blk
  , Eq (PerasVote blk)
  , Eq (PerasCert blk)
  ) =>
  Eq (TracePerasVoteForgingEvent blk)
deriving instance
  ( StandardHash blk
  , NoThunks (PerasVote blk)
  , NoThunks (PerasCert blk)
  ) =>
  NoThunks (TracePerasVoteForgingEvent blk)
deriving instance
  Generic (TracePerasVoteForgingEvent blk)
