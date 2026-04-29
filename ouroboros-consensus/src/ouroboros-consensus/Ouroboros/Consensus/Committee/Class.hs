{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Generic interface used by implementations of voting committees.
module Ouroboros.Consensus.Committee.Class
  ( -- * Voting committee interface
    CryptoSupportsVotingCommittee (..)

    -- * Votes with same target
  , UniqueVotesWithSameTarget
  , getElectionIdFromVotes
  , getVoteCandidateFromVotes
  , getRawVotes
  , UniqueVotesWithSameTargetError (..)
  , ensureUniqueVotesWithSameTarget
  , unsafeUniqueVotesWithSameTarget
  , checkUniqueVotesWithSameTarget -- for testing purposes only
  ) where

import Control.Exception (assert)
import Data.Containers.NonEmpty (HasNonEmpty (..))
import Data.Either (isRight)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Proxy (Proxy (..))
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
    UniqueVotesWithSameTarget crypto committee ->
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

-- | Collection of unique votes all targeting the same election and candidate
data UniqueVotesWithSameTarget crypto committee
  = UniqueVotesWithSameTarget
      (ElectionId crypto)
      (VoteCandidate crypto)
      (NE [Vote crypto committee])

-- | Get the election identifier targeted by a collection of votes
getElectionIdFromVotes ::
  UniqueVotesWithSameTarget crypto committee ->
  ElectionId crypto
getElectionIdFromVotes (UniqueVotesWithSameTarget electionId _ _) =
  electionId

-- | Get the vote candidate targeted by a collection of votes
getVoteCandidateFromVotes ::
  UniqueVotesWithSameTarget crypto committee ->
  VoteCandidate crypto
getVoteCandidateFromVotes (UniqueVotesWithSameTarget _ candidate _) =
  candidate

-- | Get the raw votes from a collection of votes with the same target.
--
-- NOTE: this returns votes in ascending seat index order.
getRawVotes ::
  UniqueVotesWithSameTarget crypto committee ->
  NE [Vote crypto committee]
getRawVotes (UniqueVotesWithSameTarget _ _ votes) =
  votes

-- | Errors when votes do not all target the same election and candidate
data UniqueVotesWithSameTargetError vote
  = DuplicateVotes
      -- A cluster of votes equal under the supplied ordering, i.e.,
      -- either true duplicates or equivocating votes (same id, different
      -- target).
      (NE [vote])
  | TargetMismatch
      -- First vote (under the supplied ordering) whose target is treated
      -- as the canonical target for the comparison
      vote
      -- Votes that do not match the target of the first vote
      (NE [vote])

-- | Check that a non-empty list of votes all target the same election and
-- candidate and there are no duplicates.
--
-- NOTE: duplicates are reported in preference to target mismatches.
ensureUniqueVotesWithSameTarget ::
  forall crypto committee.
  ( Eq (ElectionId crypto)
  , Eq (VoteCandidate crypto)
  ) =>
  -- | How to project the target from an abstract vote
  (Vote crypto committee -> (ElectionId crypto, VoteCandidate crypto)) ->
  -- | How to compare votes by ID, where EQ means that two votes have the same
  -- ID and are either total duplicates, or are equivocating (i.e., they have
  -- the same ID but a different target)
  (Vote crypto committee -> Vote crypto committee -> Ordering) ->
  -- | Collection of votes to check
  NE [Vote crypto committee] ->
  Either
    (UniqueVotesWithSameTargetError (Vote crypto committee))
    (UniqueVotesWithSameTarget crypto committee)
ensureUniqueVotesWithSameTarget getTarget cmpVotes votes =
  fmap
    ( const
        ( UniqueVotesWithSameTarget
            electionId
            candidate
            (firstVote :| nextVotes)
        )
    )
    $ checkUniqueVotesWithSameTarget
      (Proxy @crypto)
      getTarget
      cmpVotes
      votes
 where
  firstVote :| nextVotes = votes
  (electionId, candidate) = getTarget firstVote

-- | Same as 'ensureUniqueVotesWithSameTarget' but turns the invariant
-- checks into assertions.
--
-- WARNING: asserts become a no-op if the code is compiled with optimizations,
-- thus this function should only be used in production when the caller can
-- guarantee that the input votes satisfy the contract.
unsafeUniqueVotesWithSameTarget ::
  forall crypto committee.
  ( Eq (ElectionId crypto)
  , Eq (VoteCandidate crypto)
  ) =>
  -- | How to project the target from an abstract vote
  (Vote crypto committee -> (ElectionId crypto, VoteCandidate crypto)) ->
  -- | How to compare votes by ID, where EQ means that two votes have the same
  -- ID and are either total duplicates, or are equivocating (i.e., they have
  -- the same ID but a different target)
  (Vote crypto committee -> Vote crypto committee -> Ordering) ->
  -- | Collection of votes to check
  NE [Vote crypto committee] ->
  UniqueVotesWithSameTarget crypto committee
unsafeUniqueVotesWithSameTarget getTarget cmpVotes votes =
  assert
    ( isRight
        ( checkUniqueVotesWithSameTarget
            (Proxy @crypto)
            getTarget
            cmpVotes
            votes
        )
    )
    $ UniqueVotesWithSameTarget
      electionId
      candidate
      (firstVote :| nextVotes)
 where
  firstVote :| nextVotes = votes
  (electionId, candidate) = getTarget firstVote

-- | Validate that a non-empty collection of votes is well-formed for
-- certificate forging: all votes target the same election and candidate
-- (per @getTarget@), and no two votes are equal under @cmpVotes@.
--
-- Equality (@EQ@) is treated as evidence of a duplicate or equivocating vote
-- and is reported via 'DuplicateVotes' in preference to any 'TargetMismatch'.
--
-- NOTE: this is exposed for testing; production code should use
-- 'ensureUniqueVotesWithSameTarget' or 'unsafeUniqueVotesWithSameTarget'.
checkUniqueVotesWithSameTarget ::
  ( Eq (ElectionId crypto)
  , Eq (VoteCandidate crypto)
  ) =>
  Proxy crypto ->
  -- | How to project the target an abstract vote
  (vote -> (ElectionId crypto, VoteCandidate crypto)) ->
  -- | How to compare votes by ID, where EQ means that two votes have the same
  -- ID and are either total duplicates, or are equivocating (i.e., they have
  -- the same ID but a different target)
  (vote -> vote -> Ordering) ->
  -- | Collection of votes to check
  NE [vote] ->
  Either (UniqueVotesWithSameTargetError vote) ()
checkUniqueVotesWithSameTarget _ getTarget cmpVotes votes =
  go firstVote nextVotes
 where
  -- Sort the votes so checking for duplicates can be done in the same pass as
  -- checking for target mismatches
  firstVote :| nextVotes =
    NonEmpty.sortBy cmpVotes votes

  -- Check that all votes have the same target and there are no duplicates. The
  -- first argument is the last vote we have checked, passed sequentially to the
  -- next step to check against duplicates (the input must be sorted for this).
  go _ [] =
    Right ()
  go v (v' : vs)
    | isDuplicateOf v v' = do
        -- NOTE: duplicates are contiguous because the input is sorted by
        -- @cmpVotes@, thus we can stop at the first non-duplicate vote.
        let duplicateVotes = v :| (v' : takeWhile (isDuplicateOf v) vs)
        Left (DuplicateVotes duplicateVotes)
    | doesNotMatchFirstVoteTarget v' = do
        -- NOTE: mismatches are /not/ necessarily contiguous, thus we cannot
        -- stop at the first matching vote.
        let mismatchingVotes = v' :| filter doesNotMatchFirstVoteTarget vs
        Left (TargetMismatch firstVote mismatchingVotes)
    | otherwise =
        go v' vs

  -- Check if a vote does not match the target of the first vote
  doesNotMatchFirstVoteTarget v =
    getTarget v /= getTarget firstVote

  -- Check if two votes are duplicates of each other acording to @cmpVotes@
  isDuplicateOf v v' =
    cmpVotes v v' == EQ
