{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Generic interface used by implementations of voting committees.
module Ouroboros.Consensus.Committee.Class
  ( -- * Voting committee interface
    CryptoSupportsVotingCommittee (..)

    -- * Votes with same target
  , VotesNoDupNonEmptySameTarget
  , getElectionIdFromVotes
  , getVoteCandidateFromVotes
  , getRawVotes
  , VotesNoDupNonEmptySameTargetError (..)
  , ensureNoDupNonEmptySameTarget
  ) where

import Data.Containers.NonEmpty (HasNonEmpty (..))
import Data.Either (partitionEithers)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsVoteSigning
  , ElectionId
  , PrivateKey
  , VoteCandidate
  )
import Ouroboros.Consensus.Committee.Types (PoolId, SeatIndex, VoteWeight)

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
    VotesNoDupNonEmptySameTarget crypto committee SeatIndex ->
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

-- | Non-empty collection of votes all targeting the same election and candidate
-- with unique voter IDs.
data VotesNoDupNonEmptySameTarget crypto committee voterId
  = VotesNoDupNonEmptySameTarget
      (ElectionId crypto)
      (VoteCandidate crypto)
      (NE [Vote crypto committee])

-- | Get the election identifier targeted by a collection of votes
getElectionIdFromVotes ::
  VotesNoDupNonEmptySameTarget crypto committee voterId ->
  ElectionId crypto
getElectionIdFromVotes (VotesNoDupNonEmptySameTarget electionId _ _) =
  electionId

-- | Get the vote candidate targeted by a collection of votes
getVoteCandidateFromVotes ::
  VotesNoDupNonEmptySameTarget crypto committee voterId ->
  VoteCandidate crypto
getVoteCandidateFromVotes (VotesNoDupNonEmptySameTarget _ candidate _) =
  candidate

-- | Get the raw votes from a collection of votes with the same target.
--
-- NOTE: this returns votes in ascending seat index order.
getRawVotes ::
  VotesNoDupNonEmptySameTarget crypto committee voterId ->
  NE [Vote crypto committee]
getRawVotes (VotesNoDupNonEmptySameTarget _ _ votes) =
  votes

-- | Errors when votes do not respect the requirements to be grouped together to
-- eventually forge a certificate.
data VotesNoDupNonEmptySameTargetError crypto committee voterId
  = EmptyVotes
  | TargetMismatch
      -- First vote and the rest of the votes that match its target
      (NE [Vote crypto committee])
      -- Votes that do not match the target of the first vote
      (NE [Vote crypto committee])
  | DuplicateVoter voterId

-- | Check:
--     + that a list of votes is non-empty,
--     + that all votes target the same election and candidate,
--     + and that no two votes come from the same voter.
ensureNoDupNonEmptySameTarget ::
  ( Eq (ElectionId crypto)
  , Eq (VoteCandidate crypto)
  , Ord voterId
  ) =>
  (Vote crypto committee -> (ElectionId crypto, VoteCandidate crypto, voterId)) ->
  [Vote crypto committee] ->
  Either
    (VotesNoDupNonEmptySameTargetError crypto committee voterId)
    (VotesNoDupNonEmptySameTarget crypto committee voterId)
ensureNoDupNonEmptySameTarget getVoteInfo = \case
  [] ->
    Left EmptyVotes
  (firstVote : nextVotes) -> do
    case partitionEithers (fmap matchesTarget nextVotes) of
      ([], matchingVotes) -> do
        let allVotes = firstVote :| matchingVotes
        case findDuplicate (\v -> let (_, _, vid) = getVoteInfo v in vid) allVotes of
          Just dup -> Left (DuplicateVoter dup)
          Nothing ->
            Right $
              VotesNoDupNonEmptySameTarget
                electionId
                candidate
                allVotes
      (firstMismatchingVote : nextMismatchingVotes, matchingVotes) ->
        Left $
          TargetMismatch
            (firstVote :| matchingVotes)
            (firstMismatchingVote :| nextMismatchingVotes)
   where
    (electionId, candidate, _) =
      getVoteInfo firstVote
    target = (electionId, candidate)
    matchesTarget v'
      | let (eid, vc, _) = getVoteInfo v'
      , (eid, vc) /= target =
          Left v'
      | otherwise = Right v'

-- | Find the first duplicate voter ID in a non-empty list of votes.
findDuplicate ::
  Ord voterId =>
  (vote -> voterId) ->
  NonEmpty vote ->
  Maybe voterId
findDuplicate getId =
  go Set.empty
 where
  go !seen (v :| rest) =
    let vid = getId v
     in if Set.member vid seen
          then Just vid
          else case rest of
            [] -> Nothing
            (next : more) -> go (Set.insert vid seen) (next :| more)
