{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
    MembershipType (..)
  , MemberhipProof
  , CommitteeMember (..)

    -- * Committee votes
  , VoteSupportsWFALS (..)
  , VoteView (..)

    -- * Committee membership interface
  , CryptoSupportsWFALS (..)
  , CommitteeSelection
  , mkCommitteeSelection
  , CommitteeSelectionError (..)
  , committeeMemberWeight
  , checkShouldVote
  , verifyVote
  ) where

import Cardano.Ledger.BaseTypes (Nonce)
import Control.Exception (assert)
import Control.Monad (void)
import qualified Data.Array as Array
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsVRF (..)
  , CryptoSupportsVoteSigning (..)
  , ElectionId
  , VRFPoolContext (..)
  )
import Ouroboros.Consensus.Committee.LS
  ( LocalSortitionNumSeats (..)
  , localSortitionNumSeats
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
  , NonPersistentCommitteeSize
  , PersistentCommitteeSize (..)
  , SeatIndex (..)
  , TotalNonPersistentStake (..)
  , TotalPersistentStake
  , WFAError
  , weightedFaitAccompliSplitSeats
  )

-- * Voting committee membership

-- | Type of committee membership
data MembershipType
  = -- | Persistent membership to the voting committee
    Persistent
  | -- | Non-persistent membership to the voting committee
    NonPersistent
  deriving Show

-- | Proof of committee membership for a given voter
type MemberhipProof :: Type -> MembershipType -> Type
data MemberhipProof crypto membership where
  PersistentMemberProof ::
    SeatIndex ->
    VoteSignature crypto ->
    MemberhipProof crypto Persistent
  NonPersistentMemberProof ::
    LocalSortitionNumSeats ->
    VRFOutput crypto ->
    VoteSignature crypto ->
    MemberhipProof crypto NonPersistent

-- | Committee members (i.e., no longer candidates)
type CommitteeMember :: Type -> Type
data CommitteeMember crypto
  = -- | A persistent member of the voting committee
    PersistentCommitteeMember
      (MemberhipProof crypto Persistent)
      LedgerStake
  | -- | A (realized) non-persistent member of the voting committee
    NonPersistentCommitteeMember
      (MemberhipProof crypto NonPersistent)
      LedgerStake

-- * Committee votes

-- | Interface for votes that can be validated under the wFA^LS scheme
class VoteSupportsWFALS crypto vote where
  -- | Project a vote to its wFA^LS view, which contains only the information
  -- needed to validate it against a given committee selection
  getVoteView ::
    vote ->
    (forall membership. VoteView crypto membership -> r) ->
    r

-- | View of a committee selection vote.
--
-- This is a projection of a vote containing only the information needed to
-- validate it against a given committee selection.
type VoteView :: Type -> MembershipType -> Type
data VoteView crypto membership where
  PersistentVote ::
    SeatIndex ->
    ElectionId crypto ->
    VoteMessage crypto ->
    VoteSignature crypto ->
    VoteView crypto Persistent
  NonPersistentVote ::
    PoolId ->
    ElectionId crypto ->
    VoteMessage crypto ->
    VRFOutput crypto ->
    VoteSignature crypto ->
    VoteView crypto NonPersistent

-- * Committee membership interface

-- | Crypto interface needed for the wFA^LS scheme.
--
-- This class exists to tie the same key pairs both for signing/verifying votes
-- and for computing/verifying VRF outputs. Under this interface, it could still
-- be possible to use different crypto primitives for each of these operations
-- if needed.
class
  ( CryptoSupportsVoteSigning crypto
  , CryptoSupportsVRF crypto
  ) =>
  CryptoSupportsWFALS crypto
  where
  -- | Private key type for wFA^LS committee membership
  type PrivateKey crypto

  -- | Public key type for wFA^LS committee membership
  type PublicKey crypto

  -- | Cast a committee public key into a vote signature public key
  getVoteSignaturePublicKey ::
    Proxy crypto ->
    PublicKey crypto ->
    VoteSignaturePublicKey crypto

  -- | Cast a committee private key into a vote signature private key
  getVoteSignaturePrivateKey ::
    Proxy crypto ->
    PrivateKey crypto ->
    VoteSignaturePrivateKey crypto

  -- | Cast a committee public key into a VRF verification key
  getVRFVerifyKey ::
    Proxy crypto ->
    PublicKey crypto ->
    VRFVerifyKey crypto

  -- | Cast a committee private key into a VRF signing key
  getVRFSigningKey ::
    Proxy crypto ->
    PrivateKey crypto ->
    VRFSigningKey crypto

-- | Interface used to evaluate the membership of a party in a voting committee.
--
-- According to the weighted Fait-Accompli committee selection scheme, voting
-- committees are composed of two parts:
--  1. a deterministic set of "persistent" members that are assigned at the
--   beginning of the epoch according to the weighted Fait-Accompli scheme, and
--  2. a non-deterministic set of "non-persistent" members that are selected on
--   each election within such epoch via local sortition among the candidates
--   that were not granted a persistent seat.
--
-- Due to 1., this interface is temporarily anchored to a given epoch, allowing
-- us partially apply much of the relevant information about the committee
-- composition at the beginning of such epoch.
type CommitteeSelection :: Type -> Type
data CommitteeSelection crypto = CommitteeSelection
  { wfaStakeDistr :: !(ExtWFAStakeDistr (PublicKey crypto))
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

-- | Construct a 'CommitteeSelection' for a given epoch
mkCommitteeSelection ::
  -- | Epoch nonce
  Nonce ->
  -- | Expected committee size
  TargetCommitteeSize ->
  -- | Extended cumulative stake distribution of the potential voters
  ExtWFAStakeDistr (PublicKey crypto) ->
  Either WFAError (CommitteeSelection crypto)
mkCommitteeSelection nonce totalSeats stakeDistr = do
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
    CommitteeSelection
      { wfaStakeDistr = stakeDistr
      , candidateSeats = seats
      , persistentCommitteeSize = numPersistentVoters
      , nonPersistentCommitteeSize = numNonPersistentVoters
      , totalPersistentStake = persistentStake
      , totalNonPersistentStake = nonPersistentStake
      , epochNonce = nonce
      }

-- | Errors that can occur when checking the committee membership of a given
-- voter (including ourselves)
data CommitteeSelectionError
  = -- | A voter ID is missing from the committee selection
    MissingPoolId PoolId
  | -- | A voter claims to be a persistent member of the committe, but it's not
    NotAPersistentMember SeatIndex
  | -- | A voter claims to be a non-persistent member of the committe, but it's not
    NotANonPersistentMember PoolId
  | -- | The VRF evaluation for local sortition failed (e.g. due to invalid proof)
    LocalSortitionError String
  | -- | The vote signature is invalid
    InvalidVoteSignature String
  deriving (Show, Eq)

-- | Compute the voting power of a committee member
--
-- NOTE: theres is a subtle difference between the "Ledger stake" and the "Vote
-- weight" of a given voter. On one hand, the ledger stake is the stake as
-- reflected directly by the ledger stake distribution under consideration. On
-- the other hand, the "Vote" weight refers to the voting power of that voter,
-- i.e., the stake that a voter can effectively contribute to an election,
-- which might be different from their ledger stake depending on their committee
-- membership type:
--   * for a persistent committee member, their vote weight is equal to their
--     ledger stake throughout their entire tenure in the committee, whereas
--   * for a non-persistent committee member, their vote weight (provided that
--     they are actually selected to vote via local sortition) is equal to their
--     ledger stake normalized by the total non-persistent stake.
committeeMemberWeight ::
  CommitteeSelection crypto ->
  CommitteeMember crypto ->
  VoteWeight
committeeMemberWeight selection = \case
  -- Persistent members have their voting power equal to their stake
  PersistentCommitteeMember
    (PersistentMemberProof _seatIndex _sig)
    (LedgerStake stake) ->
      VoteWeight stake
  -- Non-persistent members have their voting power proportional to their
  -- number of seats granted by local sortition and their stake (normalized
  -- by the total non-persistent stake)
  NonPersistentCommitteeMember
    (NonPersistentMemberProof numSeats _sig _vrfOutput)
    (LedgerStake stake) ->
      VoteWeight $
        fromIntegral (unLocalSortitionNumSeats numSeats)
          * stake
          / nonPersistentStake
     where
      TotalNonPersistentStake (Cumulative (LedgerStake nonPersistentStake)) =
        totalNonPersistentStake selection

-- | Check if a voter is a persistent member of a committee
isPersistentMember ::
  SeatIndex ->
  CommitteeSelection crypto ->
  Bool
isPersistentMember seatIndex selection =
  unSeatIndex seatIndex
    < unPersistentCommitteeSize (persistentCommitteeSize selection)

-- | Check that a seat index is within bounds in a committee selection
seatIndexWithinBounds ::
  SeatIndex ->
  CommitteeSelection crypto ->
  Bool
seatIndexWithinBounds seatIndex selection =
  unSeatIndex seatIndex >= unSeatIndex lowerBound
    && unSeatIndex seatIndex <= unSeatIndex upperBound
 where
  (lowerBound, upperBound) =
    Array.bounds $
      unExtWFAStakeDistr $
        wfaStakeDistr $
          selection

-- | Retrieve the candidate information associated to a given seat index.
--
-- PRECONDITION: the seat index must be within bounds in the committee selection
getCandidateInSeat ::
  SeatIndex ->
  CommitteeSelection crypto ->
  (PoolId, PublicKey crypto, LedgerStake, Cumulative LedgerStake)
getCandidateInSeat seatIndex selection =
  (Array.!) distrArray seatIndex
 where
  distrArray = unExtWFAStakeDistr (wfaStakeDistr selection)

-- | Check the validity of a vote signature
checkVoteSignature ::
  forall crypto.
  CryptoSupportsVoteSigning crypto =>
  VoteSignaturePublicKey crypto ->
  ElectionId crypto ->
  VoteMessage crypto ->
  VoteSignature crypto ->
  Either CommitteeSelectionError ()
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
  CommitteeSelection crypto ->
  Either CommitteeSelectionError (VRFOutput crypto)
checkVRFOutput context electionId selection =
  first LocalSortitionError $ do
    evalVRF
      context
      ( mkVRFElectionInput
          @crypto
          (epochNonce selection)
          electionId
      )

-- | Check whether we should vote in a given election.
checkShouldVote ::
  forall crypto.
  CryptoSupportsWFALS crypto =>
  PoolId ->
  PrivateKey crypto ->
  ElectionId crypto ->
  VoteMessage crypto ->
  CommitteeSelection crypto ->
  Either CommitteeSelectionError (Maybe (CommitteeMember crypto))
checkShouldVote ourId ourPrivateKey electionId message selection
  | Just seatIndex <- Map.lookup ourId (candidateSeats selection) =
      assert (seatIndexWithinBounds seatIndex selection) $ do
        let (_, _, ourStake, _) =
              getCandidateInSeat seatIndex selection
        let ourSignaturePrivateKey =
              getVoteSignaturePrivateKey (Proxy @crypto) ourPrivateKey
        let ourVRFSigningKey =
              getVRFSigningKey (Proxy @crypto) ourPrivateKey
        let sig =
              signVote ourSignaturePrivateKey electionId message
        case isPersistentMember seatIndex selection of
          True -> do
            pure $
              Just $
                PersistentCommitteeMember
                  (PersistentMemberProof seatIndex sig)
                  ourStake
          False -> do
            let vrfContext =
                  VRFSignContext ourVRFSigningKey
            vrfOutput <- checkVRFOutput vrfContext electionId selection
            let numSeats =
                  localSortitionNumSeats
                    (nonPersistentCommitteeSize selection)
                    (totalNonPersistentStake selection)
                    ourStake
                    (normalizeVRFOutput vrfOutput)
            case unLocalSortitionNumSeats numSeats of
              0 ->
                pure Nothing
              _ ->
                pure $
                  Just $
                    NonPersistentCommitteeMember
                      (NonPersistentMemberProof numSeats vrfOutput sig)
                      ourStake
  | otherwise =
      Left (MissingPoolId ourId)

-- | Check the validity of a vote in a given election.
verifyVote ::
  forall crypto vote.
  ( CryptoSupportsWFALS crypto
  , VoteSupportsWFALS crypto vote
  ) =>
  vote ->
  CommitteeSelection crypto ->
  Either CommitteeSelectionError (Maybe (CommitteeMember crypto))
verifyVote vote selection =
  getVoteView @crypto vote $ \case
    PersistentVote seatIndex electionId message sig
      | seatIndexWithinBounds seatIndex selection
      , isPersistentMember seatIndex selection -> do
          let (_, voterPublicKey, voterStake, _) =
                getCandidateInSeat seatIndex selection
          let voterSignaturePublicKey =
                getVoteSignaturePublicKey (Proxy @crypto) voterPublicKey
          checkVoteSignature voterSignaturePublicKey electionId message sig
          pure $
            Just $
              PersistentCommitteeMember
                (PersistentMemberProof seatIndex sig)
                voterStake
      | otherwise -> do
          Left (NotAPersistentMember seatIndex)
    NonPersistentVote poolId electionId message vrfOutput sig
      | Just seatIndex <- Map.lookup poolId (candidateSeats selection)
      , not (isPersistentMember seatIndex selection) ->
          assert (seatIndexWithinBounds seatIndex selection) $ do
            let (_, voterPublicKey, voterStake, _) =
                  getCandidateInSeat seatIndex selection
            let voterSignaturePublicKey =
                  getVoteSignaturePublicKey (Proxy @crypto) voterPublicKey
            let voterVRFVerifyKey =
                  getVRFVerifyKey (Proxy @crypto) voterPublicKey
            let vrfContext =
                  VRFVerifyContext voterVRFVerifyKey vrfOutput
            checkVoteSignature voterSignaturePublicKey electionId message sig
            void $ checkVRFOutput vrfContext electionId selection
            let numSeats =
                  localSortitionNumSeats
                    (nonPersistentCommitteeSize selection)
                    (totalNonPersistentStake selection)
                    voterStake
                    (normalizeVRFOutput vrfOutput)
            case unLocalSortitionNumSeats numSeats of
              0 ->
                pure Nothing
              _ ->
                pure $
                  Just $
                    NonPersistentCommitteeMember
                      (NonPersistentMemberProof numSeats vrfOutput sig)
                      voterStake
      | otherwise ->
          Left (NotANonPersistentMember poolId)
