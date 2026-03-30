{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Weighted Fait-Accompli with Local Sortition (wFA^LS) committee selection.
--
-- This module implements a generic committee selection scheme based the on
-- Weighted Fait-Accompli with Local Sortition (wFA^LS) algorithm
-- from the paper:
--
-- Peter Gaži, Aggelos Kiayias, and Alexander Russell. 2023. Fait Accompli
-- Committee Selection: Improving the Size-Security Tradeoff of Stake-Based
-- Committees. In Proceedings of the 2023 ACM SIGSAC Conference on Computer and
-- Communications Security (CCS '23). Association for Computing Machinery, New
-- York, NY, USA, 845–858. https://doi.org/10.1145/3576915.3623194
--
-- PDF: https://eprint.iacr.org/2023/1273.pdf
--
-- For this, we combine the deterministic portion of the weighted Fait-Accompli
-- scheme (defined in @Ouroboros.Consensus.Committee.WFA@) with local sortition
-- (defined in @Ouroboros.Consensus.Committee.LS@) as a fallback scheme.
--
-- NOTE: this module is meant to be imported qualified.
module Ouroboros.Consensus.Committee.WFALS
  ( -- * Voting committee membership
    WFALSCommitteeMember (..)
  , WFALSVote (..)

    -- * Committee membership interface
  , CryptoSupportsWFALS
  , WFALSCommitteeSelection (..)
  , mkWFALSCommitteeSelection
  , WFALSCommitteeSelectionError (..)
  ) where

import Cardano.Ledger.BaseTypes (Nonce)
import Cardano.Ledger.BaseTypes.NonZero (NonZero, nonZero)
import Control.Exception (assert)
import Control.Monad (void)
import qualified Data.Array as Array
import Data.Bifunctor (Bifunctor (..))
import Data.Either (partitionEithers)
import Data.Foldable (Foldable (foldl'))
import Data.Kind (Type)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import Ouroboros.Consensus.Committee.BitMap (BitMap, bitmapFromIndices, bitmapToIndices)
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsGroupVRF (..)
  , CryptoSupportsGroupVoteSigning (..)
  , CryptoSupportsNaiveGroupVRF
  , CryptoSupportsVRF (..)
  , CryptoSupportsVoteSigning (..)
  , ElectionId
  , PrivateKeys
  , PublicKeys
  , VRFPoolContext (..)
  , VoteMessage
  )
import Ouroboros.Consensus.Committee.LS
  ( LocalSortitionNumSeats (..)
  , localSortitionNumSeats
  )
import Ouroboros.Consensus.Committee.Types
  ( CryptoSupportsCommitteeSelection (..)
  , Cumulative (..)
  , LedgerStake (..)
  , PoolId
  , TargetCommitteeSize (..)
  , VoteWeight (..)
  , VotesWithSameTarget
  , VotingWithAggregation (..)
  , VotingWithCommitteeSelection (..)
  , getElectionIdFromVotes
  , getRawVotes
  , getVoteMessageFromVotes
  )
import Ouroboros.Consensus.Committee.WFA
  ( Candidate
  , ExtWFAStakeDistr (..)
  , NonPersistentCommitteeSize
  , PersistentCommitteeSize (..)
  , SeatIndex (..)
  , TotalNonPersistentStake (..)
  , TotalPersistentStake
  , WFAError
  , getCandidateInSeat
  , seatIndexWithinBounds
  , weightedFaitAccompliSplitSeats
  )

-- * Committee membership interface

-- | View of a committee selection vote.
--
-- This is a projection of a vote containing only the information needed to
-- validate it against a given committee selection.
type WFALSVote :: Type -> Type
data WFALSVote crypto where
  PersistentVote ::
    SeatIndex ->
    ElectionId crypto ->
    VoteMessage crypto ->
    VoteSignature crypto ->
    WFALSVote crypto
  NonPersistentVote ::
    SeatIndex ->
    ElectionId crypto ->
    VoteMessage crypto ->
    VRFOutput crypto ->
    VoteSignature crypto ->
    WFALSVote crypto

deriving instance
  ( Eq (ElectionId crypto)
  , Eq (VoteMessage crypto)
  , Eq (VoteSignature crypto)
  , Eq (VRFOutput crypto)
  ) =>
  Eq (WFALSVote crypto)

deriving instance
  ( Show (ElectionId crypto)
  , Show (VoteMessage crypto)
  , Show (VoteSignature crypto)
  , Show (VRFOutput crypto)
  ) =>
  Show (WFALSVote crypto)

-- * Voting committee membership

class
  ( CryptoSupportsVoteSigning crypto
  , CryptoSupportsVRF crypto
  , Eq (VoteSignature crypto)
  , Show (VoteSignature crypto)
  , Eq (VRFOutput crypto)
  , Show (VRFOutput crypto)
  ) =>
  CryptoSupportsWFALS crypto

data WFALSCommitteeSelection crypto
  = WFALSCommitteeSelection
  { csElectionId :: !(ElectionId crypto)
  -- ^ Election ID for this committee selection
  , wfaStakeDistr :: !(ExtWFAStakeDistr (PublicKeys crypto))
  -- ^ Preaccumulated stake distrubution used to compute committee composition
  , candidateSeats :: !(Map PoolId SeatIndex)
  -- ^ Index of a given candidate in the cumulative stake distribution
  , persistentCommitteeSize :: !PersistentCommitteeSize
  -- ^ Number of persistent seats granted by the weighted Fait-Accompli scheme
  , nonPersistentCommitteeSize :: !NonPersistentCommitteeSize
  -- ^ Expected number of non-persistent voters
  , totalPersistentStake :: !TotalPersistentStake
  -- ^ Total stake of persistent voters
  , totalNonPersistentStake :: !TotalNonPersistentStake
  -- ^ Total stake of non-persistent voters
  , epochNonce :: !Nonce
  -- ^ Epoch nonce of the epoch where this committee selection takes place
  }

deriving instance
  (Eq (PublicKeys crypto), Eq (ElectionId crypto)) => Eq (WFALSCommitteeSelection crypto)

deriving instance
  (Show (PublicKeys crypto), Show (ElectionId crypto)) => Show (WFALSCommitteeSelection crypto)

data WFALSCommitteeSelectionError crypto
  = -- | A voter ID is missing from the committee selection
    MissingPoolId PoolId
  | -- | A voter claims to be a persistent member of the committee, but it's not
    NotAPersistentMember SeatIndex
  | -- | A voter claims to be a non-persistent member of the committee, but it's not
    NotANonPersistentMember SeatIndex
  | -- | A non-persistent voter was assigned zero seats by local sortition
    NonPersistentMemberWithZeroSeats SeatIndex
  | -- | The seat index is out of bounds
    InvalidSeatIndex SeatIndex
  | -- | The VRF evaluation for local sortition failed (e.g. due to invalid proof)
    LocalSortitionError String
  | -- | The vote signature is invalid
    InvalidVoteSignature String
  | -- | The cert's election ID does not match the committee selection's election ID
    CertElectionIdMismatch
  | -- | The cert contains no votes
    EmptyCert
  | -- | The group VRF verification failed
    InvalidGroupVRF String
  | -- | The group vote signature verification failed
    InvalidGroupVoteSignature String
  deriving (Show, Eq)

data WFALSCommitteeMember crypto
  = -- | A persistent member of the voting committee
    PersistentCommitteeMember
      (Candidate (PublicKeys crypto))
      VoteWeight
  | -- | A (realized) non-persistent member of the voting committee.
    NonPersistentCommitteeMember
      (Candidate (PublicKeys crypto))
      (VRFOutput crypto)
      (NonZero LocalSortitionNumSeats)
      VoteWeight

deriving instance
  (Show (VRFOutput crypto), Show (PublicKeys crypto)) => Show (WFALSCommitteeMember crypto)

deriving instance
  (Eq (VRFOutput crypto), Eq (PublicKeys crypto)) => Eq (WFALSCommitteeMember crypto)

committeeMemberCandidate :: WFALSCommitteeMember crypto -> Candidate (PublicKeys crypto)
committeeMemberCandidate = \case
  PersistentCommitteeMember candidate _ -> candidate
  NonPersistentCommitteeMember candidate _ _ _ -> candidate

instance
  CryptoSupportsWFALS crypto =>
  CryptoSupportsCommitteeSelection crypto (WFALSCommitteeSelection crypto)
  where
  type CryptoOf (WFALSCommitteeSelection crypto) = crypto
  type CommitteeSelectionError (WFALSCommitteeSelection crypto) = WFALSCommitteeSelectionError crypto
  type CommitteeMember (WFALSCommitteeSelection crypto) = WFALSCommitteeMember crypto

  checkShouldVote ::
    WFALSCommitteeSelection crypto ->
    PoolId ->
    PrivateKeys crypto ->
    ElectionId crypto ->
    Either
      (WFALSCommitteeSelectionError crypto)
      (Maybe (WFALSCommitteeMember crypto))
  checkShouldVote selection ourId ourPrivateKeys electionId
    | Just seatIndex <- Map.lookup ourId (candidateSeats selection) =
        case lookupPersistentCommitteeMember selection seatIndex of
          Right member -> pure (Just member)
          Left (NotAPersistentMember _) -> do
            let ourVRFSigningKey =
                  getVRFSigningKey (Proxy @crypto) ourPrivateKeys
                vrfContext =
                  VRFSignContext ourVRFSigningKey
            vrfOutput <- checkVRFOutput vrfContext electionId selection
            case lookupNonPersistentCommitteeMember selection seatIndex vrfOutput of
              -- For checkShouldVote, we shouldn't error out if local sortition gives us zero seats. Instead, we should just return Nothing to indicate that we're not a member of the committee.
              Left (NonPersistentMemberWithZeroSeats _) -> pure Nothing
              Right res -> pure (Just res)
              Left err -> Left err
          Left err -> Left err
    | otherwise =
        Left (MissingPoolId ourId)

  committeeMemberWeight ::
    WFALSCommitteeMember crypto ->
    VoteWeight
  committeeMemberWeight = \case
    PersistentCommitteeMember _ weight -> weight
    NonPersistentCommitteeMember _ _ _ weight -> weight

instance
  CryptoSupportsWFALS crypto =>
  VotingWithCommitteeSelection crypto (WFALSCommitteeSelection crypto)
  where
  type Vote (WFALSCommitteeSelection crypto) = WFALSVote crypto

  forgeVote ::
    WFALSCommitteeMember crypto ->
    PrivateKeys crypto ->
    ElectionId crypto ->
    VoteMessage crypto ->
    WFALSVote crypto
  forgeVote member ourPrivateKeys electionId message =
    let sig = signVote (getVoteSignaturePrivateKey (Proxy @crypto) ourPrivateKeys) electionId message
     in case member of
          PersistentCommitteeMember
            candidate
            _weight ->
              let (seatIndex, _, _, _, _) = candidate
               in PersistentVote seatIndex electionId message sig
          NonPersistentCommitteeMember
            candidate
            vrfOutput
            _numSeats
            _weight ->
              let (seatIndex, _, _, _, _) = candidate
               in NonPersistentVote seatIndex electionId message vrfOutput sig

  verifyVote ::
    WFALSCommitteeSelection crypto ->
    WFALSVote crypto ->
    Either
      (WFALSCommitteeSelectionError crypto)
      (WFALSCommitteeMember crypto)
  verifyVote selection =
    \case
      PersistentVote seatIndex electionId message sig -> do
        member <- lookupPersistentCommitteeMember selection seatIndex
        let (_, _, voterPublicKeys, _, _) = committeeMemberCandidate member
            voterSignaturePublicKeys =
              getVoteSignaturePublicKey (Proxy @crypto) voterPublicKeys
        checkVoteSignature voterSignaturePublicKeys electionId message sig
        pure member
      NonPersistentVote seatIndex electionId message vrfOutput sig -> do
        member <- lookupNonPersistentCommitteeMember selection seatIndex vrfOutput
        let (_, _, voterPublicKeys, _, _) = committeeMemberCandidate member
            voterSignaturePublicKeys =
              getVoteSignaturePublicKey (Proxy @crypto) voterPublicKeys
        let voterVRFVerifyKey =
              getVRFVerifyKey (Proxy @crypto) voterPublicKeys
        let vrfContext =
              VRFVerifyContext voterVRFVerifyKey vrfOutput
        checkVoteSignature voterSignaturePublicKeys electionId message sig
        void $ checkVRFOutput vrfContext electionId selection
        pure member

  getElectionIdFromVote = \case
    PersistentVote _ electionId _ _ -> electionId
    NonPersistentVote _ electionId _ _ _ -> electionId

  getVoteMessageFromVote = \case
    PersistentVote _ _ message _ -> message
    NonPersistentVote _ _ message _ _ -> message

-- | Construct a 'CommitteeSelection' for a given epoch
mkWFALSCommitteeSelection ::
  -- | Epoch nonce
  Nonce ->
  -- | Election ID
  ElectionId crypto ->
  -- | Expected committee size
  TargetCommitteeSize ->
  -- | Extended cumulative stake distribution of the potential voters
  ExtWFAStakeDistr (PublicKeys crypto) ->
  Either WFAError (WFALSCommitteeSelection crypto)
mkWFALSCommitteeSelection nonce electionId totalSeats stakeDistr = do
  ( numPersistentVoters
    , numNonPersistentVoters
    , persistentStake
    , nonPersistentStake
    ) <-
    weightedFaitAccompliSplitSeats stakeDistr totalSeats

  let seats =
        Map.fromList
          [ (poolId, seatIndex)
          | (seatIndex, (poolId, _, _, _)) <-
              Array.assocs (unExtWFAStakeDistr stakeDistr)
          ]

  pure $
    WFALSCommitteeSelection
      { csElectionId = electionId
      , wfaStakeDistr = stakeDistr
      , candidateSeats = seats
      , persistentCommitteeSize = numPersistentVoters
      , nonPersistentCommitteeSize = numNonPersistentVoters
      , totalPersistentStake = persistentStake
      , totalNonPersistentStake = nonPersistentStake
      , epochNonce = nonce
      }

-- | Check if a voter is a persistent member of a committee
isPersistentMember ::
  SeatIndex ->
  WFALSCommitteeSelection crypto ->
  Bool
isPersistentMember seatIndex selection =
  unSeatIndex seatIndex
    < unPersistentCommitteeSize (persistentCommitteeSize selection)

-- | Look up a persistent committee member by seat index.
--
-- Checks that the seat index is within bounds and that the voter is a
-- persistent member of the committee. Does NOT verify signatures.
lookupPersistentCommitteeMember ::
  WFALSCommitteeSelection crypto ->
  SeatIndex ->
  Either (WFALSCommitteeSelectionError crypto) (WFALSCommitteeMember crypto)
lookupPersistentCommitteeMember selection seatIndex
  | not (seatIndexWithinBounds seatIndex (wfaStakeDistr selection)) =
      Left (InvalidSeatIndex seatIndex)
  | isPersistentMember seatIndex selection =
      let candidate = getCandidateInSeat seatIndex (wfaStakeDistr selection)
          (_, _, _, LedgerStake stakeVal, _) = candidate
       in Right $ PersistentCommitteeMember candidate (VoteWeight stakeVal)
  | otherwise =
      Left (NotAPersistentMember seatIndex)

-- Checks that the seat index is within bounds and that the voter is a
-- non-persistent member of the committee based on the presented VRFOutput.
--
-- Returns 'NonPersistentMemberWithZeroSeats' if local sortition assigns zero seats.
-- Does NOT verify signatures or VRF
-- output, so the user must ensure that the provided VRF output is correct
-- outside of this function.
lookupNonPersistentCommitteeMember ::
  CryptoSupportsVRF crypto =>
  WFALSCommitteeSelection crypto ->
  SeatIndex ->
  VRFOutput crypto ->
  Either (WFALSCommitteeSelectionError crypto) (WFALSCommitteeMember crypto)
lookupNonPersistentCommitteeMember selection seatIndex vrfOutput
  | not (seatIndexWithinBounds seatIndex (wfaStakeDistr selection)) =
      Left (InvalidSeatIndex seatIndex)
  | not (isPersistentMember seatIndex selection) =
      let candidate = getCandidateInSeat seatIndex (wfaStakeDistr selection)
          (_, _, _, voterStake, _) = candidate
          numSeats =
            localSortitionNumSeats
              (nonPersistentCommitteeSize selection)
              (totalNonPersistentStake selection)
              voterStake
              (normalizeVRFOutput vrfOutput)
       in case nonZero numSeats of
            Nothing -> Left (NonPersistentMemberWithZeroSeats seatIndex)
            Just nzNumSeats ->
              let LedgerStake voterStakeVal = voterStake
                  TotalNonPersistentStake (Cumulative (LedgerStake nonPersistentStake)) =
                    totalNonPersistentStake selection
                  voterWeight =
                    VoteWeight $
                      fromIntegral (unLocalSortitionNumSeats numSeats)
                        * voterStakeVal
                        / nonPersistentStake
               in Right $
                    NonPersistentCommitteeMember
                      candidate
                      vrfOutput
                      nzNumSeats
                      voterWeight
  | otherwise =
      Left (NotANonPersistentMember seatIndex)

-- | Check the validity of a vote signature
checkVoteSignature ::
  forall crypto.
  CryptoSupportsVoteSigning crypto =>
  VoteSignaturePublicKey crypto ->
  ElectionId crypto ->
  VoteMessage crypto ->
  VoteSignature crypto ->
  Either (CommitteeSelectionError (WFALSCommitteeSelection crypto)) ()
checkVoteSignature voterPublicKey electionId message sig =
  first InvalidVoteSignature $ do
    verifyVoteSignature
      voterPublicKey
      electionId
      message
      sig

-- | Get the VRF output associated to a given context
checkVRFOutput ::
  forall crypto.
  CryptoSupportsVRF crypto =>
  VRFPoolContext crypto ->
  ElectionId crypto ->
  WFALSCommitteeSelection crypto ->
  Either (CommitteeSelectionError (WFALSCommitteeSelection crypto)) (VRFOutput crypto)
checkVRFOutput context electionId selection =
  first LocalSortitionError $ do
    evalVRF
      context
      ( mkVRFElectionInput
          @crypto
          (epochNonce selection)
          electionId
      )

partitionVotes :: [WFALSVote crypto] -> ([WFALSVote crypto], [WFALSVote crypto])
partitionVotes =
  partitionEithers
    . fmap
      ( \case
          v@PersistentVote{} -> Left v
          v@NonPersistentVote{} -> Right v
      )

data WFALSCert crypto = WFALSCert
  { certElectionId :: ElectionId crypto
  , certVoteMessage :: VoteMessage crypto
  , persistentVoters :: BitMap SeatIndex
  , nonPersistentVotersToEligibility :: Map SeatIndex (VRFOutput crypto)
  , groupSignature :: GroupVoteSignature crypto
  }

deriving instance
  ( Eq (ElectionId crypto)
  , Eq (VoteMessage crypto)
  , Eq (VRFOutput crypto)
  , Eq (GroupVoteSignature crypto)
  ) =>
  Eq (WFALSCert crypto)

deriving instance
  ( Show (ElectionId crypto)
  , Show (VoteMessage crypto)
  , Show (VRFOutput crypto)
  , Show (GroupVoteSignature crypto)
  ) =>
  Show (WFALSCert crypto)

instance
  (CryptoSupportsWFALS crypto, CryptoSupportsGroupVoteSigning crypto, CryptoSupportsGroupVRF crypto) =>
  VotingWithAggregation crypto (WFALSCommitteeSelection crypto)
  where
  type Cert (WFALSCommitteeSelection crypto) = WFALSCert crypto

  getElectionIdFromCert = certElectionId
  getVoteMessageFromCert = certVoteMessage

  forgeCert ::
    VotesWithSameTarget (WFALSCommitteeSelection crypto) ->
    Cert (WFALSCommitteeSelection crypto)
  forgeCert votes =
    let certElectionId = getElectionIdFromVotes votes
        certVoteMessage = getVoteMessageFromVotes votes
        (pvs, npvs) = partitionVotes (NE.toList $ getRawVotes votes)
        sortedPvs = sortOn voteSeatIndex pvs
        sortedNpvs = sortOn voteSeatIndex npvs
        maxPvIndex = maxWithDefault 0 voteSeatIndex sortedPvs
        pvIndices = voteSeatIndex <$> sortedPvs
        persistentVoters = bitmapFromIndices maxPvIndex pvIndices
        nonPersistentVotersToEligibility = Map.fromList $ npvIdAndVRFOutput <$> sortedNpvs
        orderedVotes =
          -- ensure deterministic ordering for group signature
          let allVotes = sortedPvs ++ sortedNpvs
           in assert (sortOn voteSeatIndex allVotes == allVotes) $
                case NE.nonEmpty allVotes of
                  Just ne -> ne
                  Nothing ->
                    error "We've just re-ordered a `NonEmpty` list of votes, so the result should still be `NonEmpty`"
        groupSignature = sconcat $ liftVoteSignature (Proxy @crypto) . voterSignature <$> orderedVotes
     in WFALSCert
          { certElectionId
          , certVoteMessage
          , persistentVoters
          , nonPersistentVotersToEligibility
          , groupSignature
          }
   where
    voterSignature (PersistentVote _ _ _ sig) = sig
    voterSignature (NonPersistentVote _ _ _ _ sig) = sig

    npvIdAndVRFOutput (NonPersistentVote seatIndex _ _ vrfOutput _) = (seatIndex, vrfOutput)
    npvIdAndVRFOutput _ = error "This function should only be called on non-persistent votes"

    voteSeatIndex (PersistentVote seatIndex _ _ _) = seatIndex
    voteSeatIndex (NonPersistentVote seatIndex _ _ _ _) = seatIndex

    maxWithDefault def f xs = foldl' (\acc x -> max acc (f x)) def xs

  verifyCert ::
    WFALSCommitteeSelection crypto ->
    Cert (WFALSCommitteeSelection crypto) ->
    Either (WFALSCommitteeSelectionError crypto) (NonEmpty (WFALSCommitteeMember crypto))
  verifyCert selection WFALSCert
                         { certElectionId
                         , certVoteMessage
                         , persistentVoters
                         , nonPersistentVotersToEligibility
                         , groupSignature
                         } = do
    -- Check that the cert's election ID matches the committee selection's election ID
    if certElectionId /= csElectionId selection
      then Left CertElectionIdMismatch
      else pure ()

    let pvIndices = bitmapToIndices persistentVoters

    -- Look up persistent members
    sortedPvMembers <-
      sortOn memberSeatIndex
        <$> mapM (\seatIndex -> lookupPersistentCommitteeMember selection seatIndex) pvIndices

    -- Look up non-persistent members
    sortedNpvMembers <-
      sortOn memberSeatIndex
        <$> mapM
          (\(seatIndex, vrfOut) -> lookupNonPersistentCommitteeMember selection seatIndex vrfOut)
          (Map.toList nonPersistentVotersToEligibility)

    let allMembers = sortedPvMembers ++ sortedNpvMembers
    members <- assert (sortOn memberSeatIndex allMembers == allMembers) $
      case NE.nonEmpty allMembers of
        Nothing -> Left EmptyCert
        Just ne -> Right ne

    -- Extract keys for verification
    let
      sortedPvSignPubKeys = getVoteSignaturePublicKey (Proxy @crypto) . memberPubKeys <$> sortedPvMembers

      sortedNpvPubKeys = memberPubKeys <$> sortedNpvMembers
      sortedNpvSignPubKeys = getVoteSignaturePublicKey (Proxy @crypto) <$> sortedNpvPubKeys
      sortedNpvVRFVerifyKeys = getVRFVerifyKey (Proxy @crypto) <$> sortedNpvPubKeys

      sortedNpvVRFOutputs = nonPersistentMemberVRFOutput <$> sortedNpvMembers

      vrfElectionInput = mkVRFElectionInput @crypto (epochNonce selection) certElectionId

    -- Group VRF verification
    () <- assert (length sortedNpvVRFVerifyKeys == length sortedNpvVRFOutputs) $
      case (NE.nonEmpty sortedNpvVRFVerifyKeys, NE.nonEmpty sortedNpvVRFOutputs) of
        (Just vrfVerifyKeys, Just vrfOutputs) ->
          -- We do group verification of the VRF output
          -- Crypto schemes can use the trivial CryptoSupportsNaiveGroupVRF instance, which under the hood just verifies each VRF output individually, if they want to opt out of this optimization
          let groupVerifyKey = sconcat $ liftVRFVerifyKey (Proxy @crypto) <$> vrfVerifyKeys
              groupVRFOutput = sconcat $ liftVRFOutput (Proxy @crypto) <$> vrfOutputs
           in first InvalidGroupVRF $
                verifyGroupVRF (Proxy @crypto) groupVerifyKey vrfElectionInput groupVRFOutput
        (Nothing, Nothing) -> pure ()
        _ ->
          error
            "The two lists have initially the same length, so they should both be empty or both be non-empty"

    -- Group signature verification
    let sortedSignPubKeys = sortedPvSignPubKeys ++ sortedNpvSignPubKeys
    () <- assert (length sortedSignPubKeys == length allMembers) $
      case NE.nonEmpty sortedSignPubKeys of
        Just signPubKeys ->
          let groupPublicKey = sconcat $ liftVoteSignaturePublicKey (Proxy @crypto) <$> signPubKeys
           in first InvalidGroupVoteSignature $
                verifyGroupVoteSignature
                  (Proxy @crypto)
                  groupPublicKey
                  certElectionId
                  certVoteMessage
                  groupSignature
        Nothing -> pure ()

    Right members
   where
    memberPubKeys m = let (_, _, pk, _, _) = committeeMemberCandidate m in pk
    memberSeatIndex m = let (seatIndex, _, _, _, _) = committeeMemberCandidate m in seatIndex

    nonPersistentMemberVRFOutput (NonPersistentCommitteeMember _ vrfOutput _ _) = vrfOutput
    nonPersistentMemberVRFOutput _ = error "This function should only be called on non-persistent members"
