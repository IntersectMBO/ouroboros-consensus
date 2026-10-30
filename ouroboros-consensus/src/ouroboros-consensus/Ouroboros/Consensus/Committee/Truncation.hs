{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A voting committee consisting of the highest-stake pools.
module Ouroboros.Consensus.Committee.Truncation
  ( Truncation
  , VotingCommittee
  , VotingCommitteeInput (..)
  , VotingCommitteeError (..)
  , EligibilityWitness (..)
  , Vote (..)
  , Cert (..)
  , candidateSeats
  ) where

import Cardano.Binary
  ( FromCBOR (..)
  , ToCBOR (..)
  , decodeListLenOf
  , encodeListLen
  )
import Control.Exception (Exception, assert)
import Control.Monad.Zip (MonadZip (..))
import qualified Data.Array as Array
import Data.Bifunctor (Bifunctor (..))
import Data.Containers.NonEmpty (HasNonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set.NonEmpty as NESet
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Committee.Class
  ( CryptoSupportsVotingCommittee (..)
  , UniqueVotesWithSameTarget
  , VotingCommittee
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
  ( Cumulative (..)
  , LedgerStake (..)
  , PoolId
  , TargetCommitteeSize (..)
  , VoteWeight (..)
  )
import Ouroboros.Consensus.Committee.WFA
  ( ExtWFAStakeDistr (..)
  , SeatIndex (..)
  , TotalStake (..)
  , WFAError
  , WFATiebreaker (..)
  , getCandidateIfSeatWithinBounds
  , mkExtWFAStakeDistr
  , unsafeGetCandidateInSeat
  )

-- | Tag for the deterministic top-stake committee scheme.
data Truncation
  deriving (Show, Eq, Generic, NoThunks)

data instance VotingCommittee crypto Truncation
  = TruncationVotingCommittee
  { -- Preaccumulated stake distribution used to compute committee composition
    extWFAStakeDistr :: !(ExtWFAStakeDistr (PublicKey crypto))
  , -- Index of a given candidate in the cumulative stake distribution
    candidateSeats :: !(Map PoolId SeatIndex)
  , -- Targeted number of eligible seats
    targetCommitteeSize :: !TargetCommitteeSize
  , -- Total stake of all voters (i.e., the sum of the stakes)
    totalActiveStake :: !TotalStake
  }

instance
  ( Typeable crypto
  , FromCBOR (PublicKey crypto)
  ) =>
  FromCBOR (VotingCommittee crypto Truncation)
  where
  fromCBOR = do
    decodeListLenOf 4
    extWFAStakeDistr <- fromCBOR
    candidateSeats <- fromCBOR
    targetCommitteeSize <- fromCBOR
    totalActiveStake <- fromCBOR
    pure
      TruncationVotingCommittee
        { extWFAStakeDistr
        , candidateSeats
        , targetCommitteeSize
        , totalActiveStake
        }

instance
  ( Typeable crypto
  , ToCBOR (PublicKey crypto)
  ) =>
  ToCBOR (VotingCommittee crypto Truncation)
  where
  toCBOR
    TruncationVotingCommittee
      { extWFAStakeDistr
      , candidateSeats
      , targetCommitteeSize
      , totalActiveStake
      } =
      encodeListLen 4
        <> toCBOR extWFAStakeDistr
        <> toCBOR candidateSeats
        <> toCBOR targetCommitteeSize
        <> toCBOR totalActiveStake

instance
  (Ord (ElectionId crypto), CryptoSupportsAggregateVoteSigning crypto) =>
  CryptoSupportsVotingCommittee crypto Truncation
  where
  data VotingCommitteeInput crypto Truncation
    = TruncationVotingCommitteeInput
        !TargetCommitteeSize
        !(Map PoolId (LedgerStake, PublicKey crypto))

  data VotingCommitteeError crypto Truncation
    = -- An error occurred during the computation of the committee selection
      WFAError WFAError
    | -- Target committee size must be positive
      ZeroCommitteeSize
    | -- Pool ID is missing from the voting committee
      MissingPoolId PoolId
    | -- Seat index is out of bounds for the voting committee
      MissingSeatIndex SeatIndex
    | -- Voter claims to be a committee member, but is not eligible
      NotACommitteeMember SeatIndex
    | -- Pool has no stake and thus is not entitled to vote
      PoolHasNoStake SeatIndex
    | -- The vote signature is invalid
      InvalidVoteSignature String
    | -- The certificate signature is invalid
      InvalidCertSignature String
    | -- We triggered an unexpected cryptographic error
      CryptoError String

  data EligibilityWitness crypto Truncation
    = -- A member of the deterministic top-stake committee
      TruncationMember
        !SeatIndex
        !LedgerStake

  data Vote crypto Truncation
    = TruncationVote
        !SeatIndex
        !(ElectionId crypto)
        !(VoteCandidate crypto)
        !(VoteSignature crypto)

  data Cert crypto Truncation
    = TruncationCert
        !(ElectionId crypto)
        !(VoteCandidate crypto)
        !(NE (Set SeatIndex))
        !(AggregateVoteSignature crypto)

  mkVotingCommittee = mkTruncationVotingCommittee
  checkShouldVote = implCheckShouldVote
  forgeVote = implForgeVote
  verifyVote = implVerifyVote
  eligiblePartyVoteWeight = implEligiblePartyVoteWeight
  forgeCert = implForgeCert
  verifyCert = implVerifyCert

  voteTarget (TruncationVote _ electionId candidate _) = (electionId, candidate)
  compareVotesById
    (TruncationVote seatIndex1 electionId1 _ _)
    (TruncationVote seatIndex2 electionId2 _ _) =
      compare (electionId1, seatIndex1) (electionId2, seatIndex2)

deriving instance Show (PublicKey crypto) => Show (VotingCommittee crypto Truncation)
deriving instance
  Eq (PublicKey crypto) =>
  Eq (VotingCommittee crypto Truncation)
deriving instance
  NoThunks (PublicKey crypto) =>
  NoThunks (VotingCommittee crypto Truncation)
deriving instance Generic (VotingCommittee crypto Truncation)

deriving instance
  Show (PublicKey crypto) =>
  Show (VotingCommitteeInput crypto Truncation)
deriving instance
  Eq (PublicKey crypto) =>
  Eq (VotingCommitteeInput crypto Truncation)
deriving instance
  NoThunks (PublicKey crypto) =>
  NoThunks (VotingCommitteeInput crypto Truncation)
deriving instance Generic (VotingCommitteeInput crypto Truncation)

deriving instance Show (VotingCommitteeError crypto Truncation)
deriving instance Eq (VotingCommitteeError crypto Truncation)
deriving instance NoThunks (VotingCommitteeError crypto Truncation)
deriving instance Generic (VotingCommitteeError crypto Truncation)
deriving instance
  Typeable crypto =>
  Exception (VotingCommitteeError crypto Truncation)

-- | Construct a 'TruncationVotingCommittee' for a given epoch
mkTruncationVotingCommittee ::
  VotingCommitteeInput crypto Truncation ->
  Either
    (VotingCommitteeError crypto Truncation)
    (VotingCommittee crypto Truncation)
mkTruncationVotingCommittee
  ( TruncationVotingCommitteeInput
      targetSize@(TargetCommitteeSize size)
      pools
    ) = do
    -- Preserve the shared stake-distribution error precedence for empty input.
    stakeDistr <- bimap WFAError id $ mkExtWFAStakeDistr (WFATiebreaker compare) pools
    if size == 0
      then Left ZeroCommitteeSize
      else do
        let seats =
              Map.fromList
                [ (poolId, seatIndex)
                | (seatIndex, (poolId, _, _, _)) <-
                    Array.assocs (unExtWFAStakeDistr stakeDistr)
                ]
        pure $
          TruncationVotingCommittee
            { extWFAStakeDistr = stakeDistr
            , candidateSeats = seats
            , targetCommitteeSize = targetSize
            , totalActiveStake =
                TotalStake . Cumulative . LedgerStake $
                  Map.foldl' positiveStake 0 pools
            }
   where
    -- Ledger stake distributions are normally nonnegative. The shared
    -- constructor accepts negative values, so exclude them from vote weights.
    positiveStake total (LedgerStake stake, _)
      | stake > 0 = total + stake
      | otherwise = total

-- | Check whether we should vote in a given election
implCheckShouldVote ::
  forall crypto.
  VotingCommittee crypto Truncation ->
  PoolId ->
  PrivateKey crypto ->
  ElectionId crypto ->
  Either (VotingCommitteeError crypto Truncation) (Maybe (EligibilityWitness crypto Truncation))
implCheckShouldVote committee poolId _ _ =
  case Map.lookup poolId (candidateSeats committee) of
    Nothing -> Left (MissingPoolId poolId)
    Just seatIndex
      | not (isCommitteeMember seatIndex committee) -> Right Nothing
      | otherwise -> do
          let (_, _, stake, _) =
                unsafeGetCandidateInSeat seatIndex (extWFAStakeDistr committee)
          pure $
            if hasPositiveStake stake
              then Just (TruncationMember seatIndex stake)
              else Nothing

-- | Forge a vote for a given election and candidate
implForgeVote ::
  forall crypto.
  CryptoSupportsVoteSigning crypto =>
  EligibilityWitness crypto Truncation ->
  PrivateKey crypto ->
  ElectionId crypto ->
  VoteCandidate crypto ->
  Vote crypto Truncation
implForgeVote (TruncationMember seatIndex _) privateKey electionId candidate =
  TruncationVote seatIndex electionId candidate (signVote signingKey electionId candidate)
 where
  signingKey = getVoteSigningKey (Proxy @crypto) privateKey

-- | Verify a vote cast by a committee member in a given election
implVerifyVote ::
  forall crypto.
  CryptoSupportsVoteSigning crypto =>
  VotingCommittee crypto Truncation ->
  Vote crypto Truncation ->
  Either (VotingCommitteeError crypto Truncation) (EligibilityWitness crypto Truncation)
implVerifyVote committee = \case
  TruncationVote seatIndex electionId candidate sig ->
    case getCandidateIfSeatWithinBounds seatIndex (extWFAStakeDistr committee) of
      Just (_, publicKey, stake, _)
        | isCommitteeMember seatIndex committee -> do
            if hasPositiveStake stake
              then do
                bimap InvalidVoteSignature id $
                  verifyVoteSignature (getVoteVerificationKey (Proxy @crypto) publicKey) electionId candidate sig
                pure (TruncationMember seatIndex stake)
              else Left (PoolHasNoStake seatIndex)
      Just _ -> Left (NotACommitteeMember seatIndex)
      Nothing -> Left (MissingSeatIndex seatIndex)

-- | Compute the voting power of an eligible committee member.
implEligiblePartyVoteWeight ::
  VotingCommittee crypto Truncation ->
  EligibilityWitness crypto Truncation ->
  VoteWeight
implEligiblePartyVoteWeight committee (TruncationMember _ (LedgerStake stake)) =
  VoteWeight
    ( if stake <= 0 || totalActiveStakeValue <= 0
        then 0
        else stake / totalActiveStakeValue
    )
 where
  TotalStake (Cumulative (LedgerStake totalActiveStakeValue)) = totalActiveStake committee

-- | Forge a certificate attesting the winner of a given election
implForgeCert ::
  forall crypto.
  CryptoSupportsAggregateVoteSigning crypto =>
  UniqueVotesWithSameTarget crypto Truncation ->
  Either (VotingCommitteeError crypto Truncation) (Cert crypto Truncation)
implForgeCert votes =
  assert allUniqueVoterSeats $ do
    sig <- bimap CryptoError id $ aggregateVoteSignatures (Proxy @crypto) voteSignatures
    pure $ TruncationCert (getElectionIdFromVotes votes) (getVoteCandidateFromVotes votes) voterSet sig
 where
  voterSet = NESet.fromList voters
  (voters, voteSignatures) = munzip $ fmap unpack votesInSeatOrder
  unpack (TruncationVote seatIndex _ _ sig) = (seatIndex, sig)
  votesInSeatOrder = NonEmpty.sortWith voteSeatIndex (getRawVotes votes)
  voteSeatIndex (TruncationVote seatIndex _ _ _) = seatIndex
  allUniqueVoterSeats = NESet.size voterSet == length voters && NESet.size voterSet == length voteSignatures

-- | Verify a certificate attesting the winner of a given election
implVerifyCert ::
  forall crypto.
  CryptoSupportsAggregateVoteSigning crypto =>
  VotingCommittee crypto Truncation ->
  Cert crypto Truncation ->
  Either (VotingCommitteeError crypto Truncation) (NE [EligibilityWitness crypto Truncation])
implVerifyCert committee = \case
  TruncationCert electionId candidate voters sig -> do
    (members, keys) <- fmap munzip . traverse checkMember $ NESet.toAscList voters
    aggKey <- bimap CryptoError id $ aggregateVoteVerificationKeys (Proxy @crypto) keys
    bimap InvalidCertSignature id $
      verifyAggregateVoteSignature (Proxy @crypto) aggKey electionId candidate sig
    pure members
 where
  checkMember seatIndex =
    case getCandidateIfSeatWithinBounds seatIndex (extWFAStakeDistr committee) of
      Nothing -> Left (MissingSeatIndex seatIndex)
      Just (_, publicKey, stake, _)
        | isCommitteeMember seatIndex committee ->
            if hasPositiveStake stake
              then
                pure (TruncationMember seatIndex stake, getVoteVerificationKey (Proxy @crypto) publicKey)
              else Left (PoolHasNoStake seatIndex)
        | otherwise -> Left (NotACommitteeMember seatIndex)

-- | Whether a pool has stake that entitles it to vote.
hasPositiveStake :: LedgerStake -> Bool
hasPositiveStake (LedgerStake stake) = stake > 0

-- * Helpers

-- | Check whether a seat is within the targeted committee size
isCommitteeMember ::
  SeatIndex ->
  VotingCommittee crypto Truncation ->
  Bool
isCommitteeMember (SeatIndex seatIndex) committee =
  seatIndex < unTargetCommitteeSize (targetCommitteeSize committee)
