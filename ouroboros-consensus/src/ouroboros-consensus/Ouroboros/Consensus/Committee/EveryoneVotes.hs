{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | A simple voting committee where pools with positive stake can vote.
module Ouroboros.Consensus.Committee.EveryoneVotes
  ( -- * Voting committee interface
    EveryoneVotes
  , VotingCommittee -- opaque
  , VotingCommitteeInput (..)
  , VotingCommitteeError (..)
  , EligibilityWitness (..)
  , Vote (..)
  , Cert (..)

    -- * Metrics about the voting committee composition
  , candidateSeats
  , numActiveVoters
  ) where

import Cardano.Ledger.BaseTypes (HasZero (..), NonZero)
import Cardano.Ledger.BaseTypes.NonZero (NonZero (..), nonZero)
import Control.Exception (assert)
import Control.Monad.Zip (MonadZip (..))
import qualified Data.Array as Array
import Data.Bifunctor (Bifunctor (..))
import Data.Containers.NonEmpty (HasNonEmpty (..))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set.NonEmpty as NESet
import Ouroboros.Consensus.Committee.Class
  ( CryptoSupportsVotingCommittee (..)
  , VotesWithSameTarget
  , getElectionIdFromVotes
  , getRawVotes
  , getVoteCandidateFromVotes
  )
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsAggregateVoteSigning (..)
  , CryptoSupportsVoteSigning (..)
  , ElectionId
  , PrivateKey
  , PublicKey
  , VoteCandidate
  )
import Ouroboros.Consensus.Committee.Types
  ( LedgerStake (..)
  , PoolId
  , VoteWeight (..)
  )
import Ouroboros.Consensus.Committee.WFA
  ( ExtWFAStakeDistr (..)
  , NumPoolsWithPositiveStake (..)
  , SeatIndex
  , WFAError
  , getCandidateInSeat
  , seatIndexWithinBounds
  )

-- | Tag for a simple voting committee where pools with positive stake can vote.
data EveryoneVotes

instance
  ( CryptoSupportsVoteSigning crypto
  , CryptoSupportsAggregateVoteSigning crypto
  ) =>
  CryptoSupportsVotingCommittee crypto EveryoneVotes
  where
  data VotingCommittee crypto EveryoneVotes
    = EveryoneVotesVotingCommittee
    { -- Preaccumulated stake distribution used to compute committee composition
      extWFAStakeDistr :: !(ExtWFAStakeDistr (PublicKey crypto))
    , -- Index of a given candidate in the cumulative stake distribution
      candidateSeats :: !(Map PoolId SeatIndex)
    , -- Number of active voters (i.e., those with non-zero stake)
      numActiveVoters :: !NumPoolsWithPositiveStake
    }

  data VotingCommitteeInput crypto EveryoneVotes
    = EveryoneVotesVotingCommitteeInput
        -- Extended cumulative stake distribution of the potential voters
        !(ExtWFAStakeDistr (PublicKey crypto))

  data VotingCommitteeError crypto EveryoneVotes
    = -- An error occurred during the computation of the committee selection
      WFAError WFAError
    | -- Pool ID is missing from the voting committee
      MissingPoolId PoolId
    | -- Seat index is out of bounds for the voting committee
      MissingSeatIndex SeatIndex
    | -- Pool has no stake and thus is not entitled to vote
      PoolHasNoStake SeatIndex
    | -- The vote signature is invalid
      InvalidVoteSignature String
    | -- The certificate signature is invalid
      InvalidCertSignature String
    | -- We triggered an unexpected cryptographic error
      CryptoError String
    deriving (Show, Eq)

  data EligibilityWitness crypto EveryoneVotes
    = EveryoneVotesMember
        !SeatIndex
        !(NonZero LedgerStake)

  data Vote crypto EveryoneVotes
    = EveryoneVotesVote
        !SeatIndex
        !(ElectionId crypto)
        !(VoteCandidate crypto)
        !(VoteSignature crypto)

  data Cert crypto EveryoneVotes
    = EveryoneVotesCert
        !(ElectionId crypto)
        !(VoteCandidate crypto)
        !(NE (Set SeatIndex))
        !(AggregateVoteSignature crypto)

  mkVotingCommittee = mkEveryoneVotesVotingCommittee
  checkShouldVote = implCheckShouldVote
  forgeVote = implForgeVote
  verifyVote = implVerifyVote
  eligiblePartyVoteWeight = implEligiblePartyVoteWeight
  forgeCert = implForgeCert
  verifyCert = implVerifyCert

-- | Construct a 'EveryoneVotesVotingCommittee' for a given epoch
mkEveryoneVotesVotingCommittee ::
  VotingCommitteeInput crypto EveryoneVotes ->
  Either
    (VotingCommitteeError crypto EveryoneVotes)
    (VotingCommittee crypto EveryoneVotes)
mkEveryoneVotesVotingCommittee
  ( EveryoneVotesVotingCommitteeInput
      stakeDistr
    ) = do
    let seats =
          Map.fromList
            . fmap (\(seatIndex, (poolId, _, _, _)) -> (poolId, seatIndex))
            . Array.assocs
            . unExtWFAStakeDistr
            $ stakeDistr

    pure $
      EveryoneVotesVotingCommittee
        { extWFAStakeDistr = stakeDistr
        , candidateSeats = seats
        , numActiveVoters = numPoolsWithPositiveStake stakeDistr
        }

-- | Check whether we should vote in a given election
implCheckShouldVote ::
  forall crypto.
  VotingCommittee crypto EveryoneVotes ->
  PoolId ->
  PrivateKey crypto ->
  ElectionId crypto ->
  Either
    (VotingCommitteeError crypto EveryoneVotes)
    (Maybe (EligibilityWitness crypto EveryoneVotes))
implCheckShouldVote committee ourId _ourPrivateKey _electionId
  | Just seatIndex <- Map.lookup ourId (candidateSeats committee) =
      assert (seatIndexWithinBounds seatIndex (extWFAStakeDistr committee)) $ do
        let (_, _, ourStake, _) =
              getCandidateInSeat seatIndex (extWFAStakeDistr committee)
        case nonZero ourStake of
          Nothing ->
            Right Nothing
          Just nonZeroOurStake ->
            Right $
              Just $
                EveryoneVotesMember
                  seatIndex
                  nonZeroOurStake
  | otherwise =
      Left (MissingPoolId ourId)

-- | Forge a vote for a given election and candidate
implForgeVote ::
  forall crypto.
  CryptoSupportsVoteSigning crypto =>
  EligibilityWitness crypto EveryoneVotes ->
  PrivateKey crypto ->
  ElectionId crypto ->
  VoteCandidate crypto ->
  Vote crypto EveryoneVotes
implForgeVote member ourPrivateKey electionId candidate =
  EveryoneVotesVote seatIndex electionId candidate sig
 where
  EveryoneVotesMember seatIndex _ =
    member
  ourVoteSigningKey =
    getVoteSigningKey (Proxy @crypto) ourPrivateKey
  sig =
    signVote ourVoteSigningKey electionId candidate

-- | Verify a vote cast by a committee member in a given election
implVerifyVote ::
  forall crypto.
  CryptoSupportsVoteSigning crypto =>
  VotingCommittee crypto EveryoneVotes ->
  Vote crypto EveryoneVotes ->
  Either
    (VotingCommitteeError crypto EveryoneVotes)
    (EligibilityWitness crypto EveryoneVotes)
implVerifyVote committee = \case
  EveryoneVotesVote seatIndex electionId candidate sig
    | seatIndexWithinBounds seatIndex (extWFAStakeDistr committee) -> do
        let (_, voterPublicKey, voterStake, _) =
              getCandidateInSeat seatIndex (extWFAStakeDistr committee)
        let voterVerificationKey =
              getVoteVerificationKey (Proxy @crypto) voterPublicKey
        bimap InvalidVoteSignature id $ do
          verifyVoteSignature
            voterVerificationKey
            electionId
            candidate
            sig
        case nonZero voterStake of
          Nothing ->
            Left (PoolHasNoStake seatIndex)
          Just nonZeroVoterStake ->
            pure $
              EveryoneVotesMember
                seatIndex
                nonZeroVoterStake
    | otherwise ->
        Left (MissingSeatIndex seatIndex)

-- | Compute the voting power of an eligible committee member.
--
-- In this simple voting committee, the vote weight of a member is equal to
-- their ledger stake, as long as it is positive.
implEligiblePartyVoteWeight ::
  VotingCommittee crypto EveryoneVotes ->
  EligibilityWitness crypto EveryoneVotes ->
  VoteWeight
implEligiblePartyVoteWeight _committee member =
  VoteWeight (unLedgerStake (unNonZero voterStake))
 where
  EveryoneVotesMember _ voterStake = member

-- | Forge a certificate attesting the winner of a given election
implForgeCert ::
  forall crypto.
  CryptoSupportsAggregateVoteSigning crypto =>
  VotesWithSameTarget crypto EveryoneVotes ->
  Either
    (VotingCommitteeError crypto EveryoneVotes)
    (Cert crypto EveryoneVotes)
implForgeCert votes = do
  aggSig <-
    bimap CryptoError id $ do
      aggregateVoteSignatures
        (Proxy @crypto)
        voteSignatures
  pure $
    EveryoneVotesCert
      (getElectionIdFromVotes votes)
      (getVoteCandidateFromVotes votes)
      (NESet.fromList voters)
      aggSig
 where
  (voters, voteSignatures) =
    munzip $ flip fmap votesInAscendingSeatIndexOrder $ \case
      EveryoneVotesVote seatIndex _ _ sig ->
        ( seatIndex
        , sig
        )

  -- Make sure we have votes in ascending seat index order, which is something
  -- 'VotesWithSameTarget' cannot guarantee by itself, since seat indices are
  -- an implementation detail of this voting committee scheme.
  votesInAscendingSeatIndexOrder =
    flip NonEmpty.sortWith (getRawVotes votes) $ \case
      EveryoneVotesVote seatIndex _ _ _ -> seatIndex

-- | Verify a certificate attesting the winner of a given election
implVerifyCert ::
  forall crypto.
  ( CryptoSupportsVoteSigning crypto
  , CryptoSupportsAggregateVoteSigning crypto
  ) =>
  VotingCommittee crypto EveryoneVotes ->
  Cert crypto EveryoneVotes ->
  Either
    (VotingCommitteeError crypto EveryoneVotes)
    (NE [EligibilityWitness crypto EveryoneVotes])
implVerifyCert committee = \case
  EveryoneVotesCert electionId candidate voters aggSig -> do
    -- Traverse the list of voters in ascending seat index order, collecting:
    -- 1. their membership status
    -- 2. their vote verification keys (to verify the aggregate vote signature)
    (members, voteVerificationKeys) <-
      fmap munzip . flip traverse (NESet.toAscList voters) $ \case
        seatIndex
          | seatIndexWithinBounds seatIndex (extWFAStakeDistr committee) -> do
              let (_, voterPublicKey, voterStake, _) =
                    getCandidateInSeat seatIndex (extWFAStakeDistr committee)
              let voterVerificationKey =
                    getVoteVerificationKey (Proxy @crypto) voterPublicKey
              case nonZero voterStake of
                Nothing ->
                  Left (PoolHasNoStake seatIndex)
                Just nonZeroVoterStake ->
                  pure
                    ( EveryoneVotesMember
                        seatIndex
                        nonZeroVoterStake
                    , voterVerificationKey
                    )
          | otherwise ->
              Left (MissingSeatIndex seatIndex)
    -- Verify aggregate signature
    aggVerificationKey <-
      bimap CryptoError id $ do
        aggregateVoteVerificationKeys
          (Proxy @crypto)
          voteVerificationKeys
    bimap InvalidCertSignature id $
      verifyAggregateVoteSignature
        (Proxy @crypto)
        aggVerificationKey
        electionId
        candidate
        aggSig

    -- Return the list of voters attesting the election winner
    pure members
