{-# LANGUAGE FlexibleContexts #-}

-- | Stateful wrapper around a voting committee instance
module Ouroboros.Consensus.Committee.Handle
  ( VotingCommitteeHandle
  , mkVotingCommitteeHandle
  ) where

import Control.Concurrent.Class.MonadSTM (STM)
import qualified Control.Concurrent.Class.MonadSTM as LazySTM
import Control.Monad.Class.MonadThrow (Exception, MonadThrow (..))
import Data.Containers.NonEmpty (HasNonEmpty (..))
import Data.Maybe.Strict (StrictMaybe, maybeToStrictMaybe)
import Ouroboros.Consensus.Committee.AcrossEpochs
  ( ElectionEpoch
  , InterEpochVotingCommittee
  , getVotingCommitteeForElection
  , mkInterEpochVotingCommittee
  )
import Ouroboros.Consensus.Committee.Class
  ( CryptoSupportsVotingCommittee (..)
  , VotesWithSameTarget
  )
import Ouroboros.Consensus.Committee.Crypto (ElectionId, PrivateKey, VoteCandidate)
import Ouroboros.Consensus.Committee.Types (PoolId, VoteWeight)
import Ouroboros.Consensus.Util.IOLike (IOLike)

-- | Stateful wrapper over the 'CryptoSupportsVotingCommittee' type class.
--
-- This wrapper keeps tracks of:
--
-- * The multiple inter-epoch voting committee compositions.
-- * The pool ID of this node.
-- * The private keys used for checking eligibility and forging votes.
data VotingCommitteeHandleInternal crypto committee
  = VotingCommitteeHandleInternal
  { vchiVotingCommittees :: !(InterEpochVotingCommittee crypto committee)
  -- ^ Voting committee compositions across epochs
  , vchiPoolId :: !PoolId
  -- ^ Pool ID of this node
  , vchiPrivateKeys :: !(StrictMaybe (PrivateKey crypto))
  -- ^ Private keys used for checking eligibility and forging votes
  }

newtype VotingCommitteeHandle crypto committee m
  = VotingCommitteeHandle
      ( LazySTM.TVar
          m
          (VotingCommitteeHandleInternal crypto committee)
      )

-- * Stateful public interface

-- | Construct a 'VotingCommitteeHandle' from the given input information.
mkVotingCommitteeHandle ::
  ( IOLike m
  , Exception (VotingCommitteeError crypto committee)
  , CryptoSupportsVotingCommittee crypto committee
  ) =>
  VotingCommitteeInput crypto committee ->
  (ElectionId crypto -> ElectionEpoch) ->
  PoolId ->
  Maybe (PrivateKey crypto) ->
  STM m (VotingCommitteeHandle crypto committee m)
mkVotingCommitteeHandle
  votingCommitteeInput
  getElectionEpoch
  poolId
  privateKeys = do
    votingCommittees <-
      either throwIO pure $
        mkInterEpochVotingCommittee
          votingCommitteeInput
          getElectionEpoch
    fmap VotingCommitteeHandle $
      LazySTM.newTVar $
        VotingCommitteeHandleInternal
          { vchiVotingCommittees =
              votingCommittees
          , vchiPoolId =
              poolId
          , vchiPrivateKeys =
              maybeToStrictMaybe privateKeys
          }

-- -- | Check whether we should vote in a given election
-- checkShouldVote ::
--   ( MonadThrow m
--   , Exception (VotingCommitteeError crypto committee)
--   , CryptoSupportsVotingCommittee crypto committee
--   ) =>
--   VotingCommitteeHandle crypto committee ->
--   ElectionId crypto ->
--   m (Maybe (EligibilityWitness crypto committee))
-- checkShouldVote vcHandle = do
--   votingCommittee <-
--     getVotingCommitteeForElection (vchGetElectionEpoch vcHandle) vcHandle
--   undefined
--
-- -- | Forge a vote for a given election and candidate
-- forgeVote ::
--   MonadThrow m =>
--   VotingCommitteeHandle crypto committee ->
--   EligibilityWitness crypto committee ->
--   ElectionId crypto ->
--   VoteCandidate crypto ->
--   m (Vote crypto committee)
-- forgeVote = undefined
--
-- -- | Verify a vote cast by a committee member in a given election
-- verifyVote ::
--   MonadThrow m =>
--   VotingCommitteeHandle crypto committee ->
--   Vote crypto committee ->
--   ElectionId crypto ->
--   m (EligibilityWitness crypto committee)
-- verifyVote = undefined
--
-- -- | Compute the voting weight of a eligibile party
-- eligiblePartyVoteWeight ::
--   MonadThrow m =>
--   VotingCommitteeHandle crypto committee ->
--   EligibilityWitness crypto committee ->
--   m VoteWeight
-- eligiblePartyVoteWeight = undefined
--
-- -- | Forge a certificate attesting the winner of a given election
-- forgeCert ::
--   MonadThrow m =>
--   VotingCommitteeHandle crypto committee ->
--   VotesWithSameTarget crypto committee ->
--   m (Cert crypto committee)
-- forgeCert = undefined
--
-- -- | Verify a certificate attesting the winner of a given election
-- verifyCert ::
--   MonadThrow m =>
--   VotingCommitteeHandle crypto committee ->
--   Cert crypto committee ->
--   ElectionId crypto ->
--   m (NE [EligibilityWitness crypto committee])
-- verifyCert = undefined
--
-- -- * Helpers
