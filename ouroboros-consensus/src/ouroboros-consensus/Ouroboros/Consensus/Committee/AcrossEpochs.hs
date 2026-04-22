-- | This module extends a given voting committee to work across epochs.
--
-- This is needed to support the case of validating an old vote or certificate
-- from a previous epoch arriving too late. In the general case, this means we
-- would need to store an arbitrary number of past voting committee selections.
-- However, since:
--   1. the length of an epoch is much larger than the immutability window, and
--   2. we don't care about validating votes older than the immutability window,
--      it follows that we only need to store the voting committee selection for
--      the current and previous epochs.
--  NOTE: this rationale might need to be revisited if we ever want to support
--  validating votes and certificates older than the immutability window, e.g.,
--  for historical queries.
module Ouroboros.Consensus.Committee.AcrossEpochs
  ( ElectionEpoch (..)
  , InterEpochVotingCommittee (..)
  , mkInterEpochVotingCommittee
  , newEpoch
  , getVotingCommitteeForElection
  ) where

import Data.Maybe.Strict (StrictMaybe (..), strictMaybeToMaybe)
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee (..))
import Ouroboros.Consensus.Committee.Crypto (ElectionId)

-- | Tag to identify to which epoch an election belongs to.
data ElectionEpoch
  = -- | The election belongs to the current epoch.
    CurrentEpoch
  | -- | The election belongs to the previous epoch.
    PreviousEpoch
  | -- | The election belongs to an epoch other than the last two ones.
    UnknownEpoch
  deriving (Eq, Show)

-- | Voting committee composition across epochs
data InterEpochVotingCommittee crypto committee
  = InterEpochVotingCommittee
  { currEpochVotingCommittee :: !(VotingCommittee crypto committee)
  -- ^ Voting committee composition for the current epoch
  , prevEpochVotingCommittee :: !(StrictMaybe (VotingCommittee crypto committee))
  -- ^ Voting committee composition for the previous epoch, if known
  , getElectionEpoch :: ElectionId crypto -> ElectionEpoch
  -- ^ Mapping election IDs to their corresponding epochs
  }

-- | Construct an inter-epoch committee selection for the first epoch
mkInterEpochVotingCommittee ::
  CryptoSupportsVotingCommittee crypto committee =>
  VotingCommitteeInput crypto committee ->
  (ElectionId crypto -> ElectionEpoch) ->
  Either
    (VotingCommitteeError crypto committee)
    (InterEpochVotingCommittee crypto committee)
mkInterEpochVotingCommittee votingCommitteeInput electionEpoch = do
  votingCommittee <-
    mkVotingCommittee votingCommitteeInput
  pure $
    InterEpochVotingCommittee
      { currEpochVotingCommittee =
          votingCommittee
      , prevEpochVotingCommittee =
          SNothing
      , getElectionEpoch =
          electionEpoch
      }

-- | Update an inter-epoch committee selection at the beginning of a new epoch
newEpoch ::
  CryptoSupportsVotingCommittee crypto committee =>
  VotingCommitteeInput crypto committee ->
  InterEpochVotingCommittee crypto committee ->
  Either
    (VotingCommitteeError crypto committee)
    (InterEpochVotingCommittee crypto committee)
newEpoch newEpochVotingCommitteeInput interEpochVotingCommittee = do
  newEpochVotingCommittee <-
    mkVotingCommittee newEpochVotingCommitteeInput
  pure $
    interEpochVotingCommittee
      { currEpochVotingCommittee =
          newEpochVotingCommittee
      , prevEpochVotingCommittee =
          SJust (currEpochVotingCommittee interEpochVotingCommittee)
      }

-- | Get the voting committee corresponding to an election, if any
getVotingCommitteeForElection ::
  ElectionEpoch ->
  InterEpochVotingCommittee crypto committee ->
  Maybe (VotingCommittee crypto committee)
getVotingCommitteeForElection electionEpoch interEpochVotingCommittee = do
  case electionEpoch of
    CurrentEpoch ->
      Just (currEpochVotingCommittee interEpochVotingCommittee)
    PreviousEpoch ->
      strictMaybeToMaybe (prevEpochVotingCommittee interEpochVotingCommittee)
    UnknownEpoch ->
      Nothing
