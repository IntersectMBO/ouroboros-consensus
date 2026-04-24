{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Generic interface used by implementations of voting committees.
module Ouroboros.Consensus.Committee.Class
  ( -- * Voting committee interface
    CryptoSupportsVotingCommittee (..)

    -- * Votes with same target
  , VotesWithSameTarget
  , getElectionIdFromVotes
  , getVoteCandidateFromVotes
  , getRawVotes
  , VotesWithSameTargetError (..)
  , ensureSameTarget
  ) where

import Data.Containers.NonEmpty (HasNonEmpty (..))
import Data.Either (partitionEithers)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsVoteSigning
  , ElectionId
  , PrivateKey
  , VoteCandidate
  )
import Ouroboros.Consensus.Committee.Types (PoolId, VoteWeight)

-- * Voting committee interface

-- | Interface for voting committee schemes.
--
-- This class is parametrized by the crypto primitives and the committee
-- selection data structure. Instances define how to check whether a party
-- should vote and how to compute the voting weight of a committee member.
class
  CryptoSupportsVoteSigning crypto =>
  CryptoSupportsVotingCommittee crypto committee
  where
  -- | Structure storing the voting committee context
  data VotingCommittee crypto committee :: Type

  -- | Input information needed to construct a voting committee
  data VotingCommitteeInput crypto committee :: Type

  -- | Errors that can occur when operating on a voting committee
  data VotingCommitteeError crypto committee :: Type

  -- | Witness attesting that a party is eligible to vote in a given election
  --
  -- NOTE: this is not necessarily the same as the cryptographic proof of
  -- eligibility used in concrete votes and certificates sent over the wire.
  data EligibilityWitness crypto committee :: Type

  -- | Abstract vote cast by a committee member in a given election
  data Vote crypto committee :: Type

  -- | Abstract certificate attesting the winner of a given election
  data Cert crypto committee :: Type

  -- | Construct a voting committee
  mkVotingCommittee ::
    VotingCommitteeInput crypto committee ->
    Either
      (VotingCommitteeError crypto committee)
      (VotingCommittee crypto committee)

  -- | Check whether we should vote in a given election
  checkShouldVote ::
    VotingCommittee crypto committee ->
    PoolId ->
    PrivateKey crypto ->
    ElectionId crypto ->
    Either
      (VotingCommitteeError crypto committee)
      (Maybe (EligibilityWitness crypto committee))

  -- | Forge a vote for a given election and candidate
  forgeVote ::
    EligibilityWitness crypto committee ->
    PrivateKey crypto ->
    ElectionId crypto ->
    VoteCandidate crypto ->
    Vote crypto committee

  -- | Verify a vote cast by a committee member in a given election
  verifyVote ::
    VotingCommittee crypto committee ->
    Vote crypto committee ->
    Either
      (VotingCommitteeError crypto committee)
      (EligibilityWitness crypto committee)

  -- | Compute the voting weight of a eligibile party
  eligiblePartyVoteWeight ::
    VotingCommittee crypto committee ->
    EligibilityWitness crypto committee ->
    VoteWeight

  -- | Forge a certificate attesting the winner of a given election
  forgeCert ::
    VotesWithSameTarget crypto committee ->
    Either
      (VotingCommitteeError crypto committee)
      (Cert crypto committee)

  -- | Verify a certificate attesting the winner of a given election
  verifyCert ::
    VotingCommittee crypto committee ->
    Cert crypto committee ->
    Either
      (VotingCommitteeError crypto committee)
      (NE [EligibilityWitness crypto committee])

-- * Votes with same target

-- | Collection of votes all targeting the same election and candidate
data VotesWithSameTarget crypto committee
  = VotesWithSameTarget
      (ElectionId crypto)
      (VoteCandidate crypto)
      (NE [Vote crypto committee])

-- | Get the election identifier targeted by a collection of votes
getElectionIdFromVotes ::
  VotesWithSameTarget crypto committee ->
  ElectionId crypto
getElectionIdFromVotes (VotesWithSameTarget electionId _ _) =
  electionId

-- | Get the vote candidate targeted by a collection of votes
getVoteCandidateFromVotes ::
  VotesWithSameTarget crypto committee ->
  VoteCandidate crypto
getVoteCandidateFromVotes (VotesWithSameTarget _ candidate _) =
  candidate

-- | Get the raw votes from a collection of votes with the same target.
--
-- NOTE: this returns votes in ascending seat index order.
getRawVotes ::
  VotesWithSameTarget crypto committee ->
  NE [Vote crypto committee]
getRawVotes (VotesWithSameTarget _ _ votes) =
  votes

-- | Errors when votes do not all target the same election and candidate
data VotesWithSameTargetError crypto committee
  = EmptyVotes
  | TargetMismatch
      -- First vote and the rest of the votes that match its target
      (NE [Vote crypto committee])
      -- Votes that do not match the target of the first vote
      (NE [Vote crypto committee])

-- | Check that a list of votes all target the same election and candidate
ensureSameTarget ::
  ( Eq (ElectionId crypto)
  , Eq (VoteCandidate crypto)
  ) =>
  (Vote crypto committee -> (ElectionId crypto, VoteCandidate crypto)) ->
  [Vote crypto committee] ->
  Either
    (VotesWithSameTargetError crypto committee)
    (VotesWithSameTarget crypto committee)
ensureSameTarget getTarget = \case
  [] ->
    Left EmptyVotes
  (firstVote : nextVotes) -> do
    case partitionEithers (fmap matchesTarget nextVotes) of
      ([], matchingVotes) ->
        Right $
          VotesWithSameTarget
            electionId
            candidate
            (firstVote :| matchingVotes)
      (firstMismatchingVote : nextMismatchingVotes, matchingVotes) ->
        Left $
          TargetMismatch
            (firstVote :| matchingVotes)
            (firstMismatchingVote :| nextMismatchingVotes)
   where
    target@(electionId, candidate) =
      getTarget firstVote
    matchesTarget v'
      | getTarget v' /= target = Left v'
      | otherwise = Right v'
