{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Thin wrapper around vote forging in a voting committee
module Ouroboros.Consensus.Committee.Forging
  ( MkVoteForging (..)
  , VoteForging (..)
  , noVoteForging
  , voteForgingWith
  ) where

import Ouroboros.Consensus.Committee.Class
  ( CryptoSupportsVotingCommittee (Vote)
  , VotingCommittee
  , VotingCommitteeError
  )
import qualified Ouroboros.Consensus.Committee.Class as Class
import Ouroboros.Consensus.Committee.Crypto
  ( ElectionId
  , PrivateKey
  , VoteCandidate
  )
import Ouroboros.Consensus.Committee.Types (PoolId)

-- | Monadic wrapper around the action that creates a 'VoteForging'
newtype MkVoteForging m crypto committee
  = MkVoteForging
  { mkVoteForging :: m (VoteForging crypto committee)
  }

-- | Thin wrapper around vote forging in a voting committee.
--
-- This wrapper provides a way to hide internal vote-forging details such as:
--  * Pool ID (used to check eligibility)
--  * Private keys (used to check eligibility and sign votes)
-- Into a closure that only exposes the necessary parameters.
data VoteForging crypto committee
  = VoteForging
  { forgeVoteIfEligible ::
      VotingCommittee crypto committee -> -- Current committee composition
      ElectionId crypto -> -- Election for which we want to forge a vote
      VoteCandidate crypto -> -- Candidate we want to vote for
      Either
        (VotingCommitteeError crypto committee)
        (Maybe (Vote crypto committee))
  }

-- | 'VoteForging' that unconditionally rejects any attempt to forge a vote
noVoteForging :: VoteForging crypto committee
noVoteForging =
  VoteForging
    { forgeVoteIfEligible = \_ _ _ ->
        pure Nothing
    }

-- | 'VoteForging' that forges votes for a given candidate when eligible
voteForgingWith ::
  CryptoSupportsVotingCommittee crypto committee =>
  PoolId ->
  PrivateKey crypto ->
  VoteForging crypto committee
voteForgingWith poolId privateKey =
  VoteForging
    { forgeVoteIfEligible = \committee electionId candidate -> do
        shouldVote <-
          Class.checkShouldVote
            committee
            poolId
            privateKey
            electionId
        case shouldVote of
          Nothing ->
            pure Nothing
          Just witness -> do
            let vote =
                  Class.forgeVote
                    witness
                    privateKey
                    electionId
                    candidate
            pure (Just vote)
    }
