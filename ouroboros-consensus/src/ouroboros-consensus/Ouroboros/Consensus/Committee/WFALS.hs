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
  , CommitteeSelection (..)
  , mkCommitteeSelection
  , CommitteeSelectionError (..)
  , committeeMemberWeight
  , checkShouldVote
  , verifyVote
  ) where

import Cardano.Ledger.BaseTypes (Nonce)
import Control.Exception (assert)
import qualified Data.Array as Array
import Data.Bifunctor (Bifunctor (..))
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsSignature (..)
  , CryptoSupportsVRF (..)
  , ElectionId
  , VRFPoolContext (..)
  )
import Ouroboros.Consensus.Committee.LS
  ( localSortitionNumSeats
  )
import Ouroboros.Consensus.Committee.Types
  ( CommitteeSize (..)
  , ExtCumulativeStakeDistr (..)
  , NonPersistentCommitteeSize
  , NumSeats (..)
  , PersistentCommitteeSize (..)
  , PoolId
  , SeatIndex (..)
  , Stake (..)
  , StakeRole (..)
  , TotalNonPersistentStake (..)
  , TotalPersistentStake
  )
import Ouroboros.Consensus.Committee.WFA (weightedFaitAccompliSplitSeats)

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
data MemberhipProof c m where
  PersistentMemberProof ::
    SeatIndex ->
    Proxy c -> -- To be replaced with some crypto material
    MemberhipProof c Persistent
  NonPersistentMemberProof ::
    NumSeats ->
    Proxy c -> -- To be replaced with some crypto material
    MemberhipProof c NonPersistent

-- | Committee members (i.e., no longer candidates)
type CommitteeMember :: Type -> MembershipType -> Type
data CommitteeMember c m where
  CommitteeMember ::
    { committeeMemberhipProof :: MemberhipProof c m
    , committeeMemberStake :: Stake Ledger
    } ->
    CommitteeMember c m

-- | Construct a persistent committee member
mkPersistentMember ::
  forall c.
  SeatIndex ->
  Stake Ledger ->
  CommitteeMember c Persistent
mkPersistentMember seatIndex stake =
  CommitteeMember
    { committeeMemberhipProof = PersistentMemberProof seatIndex (Proxy @c)
    , committeeMemberStake = stake
    }

-- | Construct a non-persistent committee member
mkNonPersistentMember ::
  forall c.
  NumSeats ->
  Stake Ledger ->
  CommitteeMember c NonPersistent
mkNonPersistentMember numSeats stake =
  CommitteeMember
    { committeeMemberhipProof = NonPersistentMemberProof numSeats (Proxy @c)
    , committeeMemberStake = stake
    }

-- * Committee votes

-- | Interface for votes that can be validated under the wFA^LS scheme
class VoteSupportsWFALS c vote where
  -- | Project a vote to its wFA^LS view, which contains only the information
  -- needed to validate it against a given committee selection
  getVoteView ::
    vote ->
    (forall m. VoteView c m -> r) ->
    r

-- | View of a committee selection vote.
--
-- This is a projection of a vote containing only the information needed to
-- validate it against a given committee selection.
type VoteView :: Type -> MembershipType -> Type
data VoteView c m where
  PersistentVote ::
    SeatIndex ->
    ElectionId c ->
    Payload c ->
    Signature c ->
    VoteView c Persistent
  NonPersistentVote ::
    PoolId -> -- TODO: decided whether we want this or a SeatIndex
    ElectionId c ->
    VRFOutput c ->
    Payload c ->
    Signature c ->
    VoteView c NonPersistent

-- * Committee membership interface

-- | Crypto interface needed for validating votes under the wFA^LS scheme.
--
-- This class exists to allows to use the same key pair to derive keys both for
-- signing/verifying votes and for computing/verifying VRF outputs. Under this
-- interface, it could be possible to use different crypto primitives for each
-- of these operations if needed.
class
  ( CryptoSupportsSignature c
  , CryptoSupportsVRF c
  ) =>
  CryptoSupportsWFALS c
  where
  -- | Private key type for wFA^LS committee membership
  type WFALSPrivateKey c

  -- | Public key type for wFA^LS committee membership
  type WFALSPublicKey c

  -- | Cast a committee public key into a signature public key
  getSignaturePublicKey :: Proxy c -> WFALSPublicKey c -> SignaturePublicKey c

  -- | Cast a committee private key into a signature private key
  getSignaturePrivateKey :: Proxy c -> WFALSPrivateKey c -> SignaturePrivateKey c

  -- | Cast a committee public key into a VRF verification key
  getVRFVerifyKey :: Proxy c -> WFALSPublicKey c -> VRFVerifyKey c

  -- | Cast a committee private key into a VRF signing key
  getVRFSigningKey :: Proxy c -> WFALSPrivateKey c -> VRFSigningKey c

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
data CommitteeSelection c = CommitteeSelection
  { extCumulativeStakeDistr :: !(ExtCumulativeStakeDistr (WFALSPublicKey c))
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
  CommitteeSize ->
  -- | Extended cumulative stake distribution of the potential voters
  ExtCumulativeStakeDistr (WFALSPublicKey c) ->
  CommitteeSelection c
mkCommitteeSelection nonce totalSeats stakeDistr =
  CommitteeSelection
    { extCumulativeStakeDistr = stakeDistr
    , candidateSeats = seats
    , persistentCommitteeSize = numPersistentVoters
    , nonPersistentCommitteeSize = numNonPersistentVoters
    , totalPersistentStake = persistentStake
    , totalNonPersistentStake = nonPersistentStake
    , epochNonce = nonce
    }
 where
  ( numPersistentVoters
    , numNonPersistentVoters
    , persistentStake
    , nonPersistentStake
    ) = weightedFaitAccompliSplitSeats stakeDistr totalSeats

  seats =
    Map.fromList
      [ (poolId, seatIndex)
      | (seatIndex, (poolId, _, _, _)) <-
          Array.assocs (unExtCumulativeStakeDistr stakeDistr)
      ]

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
-- NOTE: theres is a subtle difference between the "Ledger" and the "Vote"
-- stakes of a given voter. On one hand, the "Ledger" stake is the stake as
-- reflected directly by the ledger stake distribution under consideration. On
-- the other hand, the "Vote" stake refers to the voting power of that voter,
-- i.e., the weight that a voter can effectively contribute to an election,
-- which might be different from the "Loter" stake depending on their committee
-- membership type:
--   * for a persistent committee member, their "Vote" stake is equal to their
--     "Ledger" stake throughout their entire tenure in the committee, whereas
--   * for a non-persistent committee member, their "Vote" stake (provided that
--     they are actually selected to vote via local sortition) is equal to their
--     "Ledger" stake normalized by the total non-persistent stake.
committeeMemberWeight ::
  CommitteeSelection c ->
  CommitteeMember c m ->
  Stake Weight
committeeMemberWeight selection member =
  case member of
    -- Persistent members have their voting power equal to their stake
    CommitteeMember
      (PersistentMemberProof _seatIndex _proxy)
      (LedgerStake stake) ->
        VoteWeight stake
    -- Non-persistent members have their voting power proportional to their
    -- number of seats granted by local sortition and their stake (normalized
    -- by the total non-persistent stake)
    CommitteeMember
      (NonPersistentMemberProof numSeats _proxy)
      (LedgerStake stake) ->
        VoteWeight $
          fromIntegral (unNumSeats numSeats)
            * stake
            / nonPersistentStake
       where
        TotalNonPersistentStake (CumulativeStake nonPersistentStake) =
          totalNonPersistentStake selection

-- | Check if a voter is a persistent member of a committee
isPersistentMember ::
  SeatIndex ->
  CommitteeSelection c ->
  Bool
isPersistentMember seatIndex selection =
  unSeatIndex seatIndex
    < unPersistentCommitteeSize (persistentCommitteeSize selection)

-- | Check that a seat index is within bounds in a committee selection
seatIndexWithinBounds ::
  SeatIndex ->
  CommitteeSelection c ->
  Bool
seatIndexWithinBounds seatIndex selection =
  unSeatIndex seatIndex >= unSeatIndex lowerBound
    && unSeatIndex seatIndex <= unSeatIndex upperBound
 where
  (lowerBound, upperBound) =
    Array.bounds $
      unExtCumulativeStakeDistr $
        extCumulativeStakeDistr $
          selection

-- | Retrieve the candidate information associated to a given seat index
--
-- PRECONDITION: the seat index must be within bounds in the committee selection
getCandidateInSeat ::
  SeatIndex ->
  CommitteeSelection c ->
  (PoolId, WFALSPublicKey c, Stake Ledger, Stake Cumulative)
getCandidateInSeat seatIndex selection =
  (Array.!)
    (unExtCumulativeStakeDistr (extCumulativeStakeDistr selection))
    seatIndex

-- | Check the validity of a vote payload
checkVotePayload ::
  forall c.
  CryptoSupportsSignature c =>
  SignaturePublicKey c ->
  Payload c ->
  Signature c ->
  Either CommitteeSelectionError ()
checkVotePayload voterPublicKey payload sig =
  bimap InvalidVoteSignature id $ do
    verifyPayloadSignature voterPublicKey payload sig

-- | Compute the number of non-persistent seats granted to a voter via local sortition
nonPersistentNumSeats ::
  forall c.
  CryptoSupportsVRF c =>
  VRFPoolContext c ->
  Stake Ledger ->
  ElectionId c ->
  CommitteeSelection c ->
  Either CommitteeSelectionError NumSeats
nonPersistentNumSeats context stake electionId selection =
  bimap LocalSortitionError id $ do
    let vrfElectionInput = mkVRFElectionInput @c (epochNonce selection) electionId
    vrfOutput <- evalVRF context vrfElectionInput
    let numNonPersistentSeats = nonPersistentCommitteeSize selection
    let nonPersistentStake = totalNonPersistentStake selection
    pure $
      localSortitionNumSeats
        numNonPersistentSeats
        nonPersistentStake
        stake
        vrfOutput

-- | Check whether we should vote in a given election
checkShouldVote ::
  forall c a.
  CryptoSupportsVRF c =>
  PoolId ->
  VRFSigningKey c ->
  ElectionId c ->
  CommitteeSelection c ->
  (forall r. Maybe (CommitteeMember c r) -> a) ->
  Either CommitteeSelectionError a
checkShouldVote ourId ourPrivateKey electionId selection k
  | Just seatIndex <- Map.lookup ourId (candidateSeats selection) =
      assert (seatIndexWithinBounds seatIndex selection) $ do
        let (_, _, ourStake, _) = getCandidateInSeat seatIndex selection
        if isPersistentMember seatIndex selection
          then do
            pure (k (Just (mkPersistentMember seatIndex ourStake)))
          else do
            numSeats <-
              nonPersistentNumSeats
                (VRFSignContext ourPrivateKey)
                ourStake
                electionId
                selection
            if unNumSeats numSeats > 0
              then pure (k (Just (mkNonPersistentMember @c numSeats ourStake)))
              else pure (k Nothing)
  | otherwise =
      Left (MissingPoolId ourId)

-- | Check the validity of a vote in a given election
verifyVote ::
  forall c vote a.
  CryptoSupportsWFALS c =>
  VoteSupportsWFALS c vote =>
  vote ->
  CommitteeSelection c ->
  (forall r. Maybe (CommitteeMember c r) -> a) ->
  Either CommitteeSelectionError a
verifyVote vote selection k =
  getVoteView @c vote $ \case
    PersistentVote seatIndex _electionId payload sig
      | seatIndexWithinBounds seatIndex selection -> do
          let (_, voterCommitteePublicKey, voterStake, _) =
                getCandidateInSeat seatIndex selection
          let voterSignaturePublicKey =
                getSignaturePublicKey (Proxy @c) voterCommitteePublicKey
          checkVotePayload voterSignaturePublicKey payload sig
          pure (k (Just (mkPersistentMember seatIndex voterStake)))
      | otherwise -> do
          Left (NotAPersistentMember seatIndex)
    NonPersistentVote poolId electionId vrfOutput payload sig
      | Just seatIndex <- Map.lookup poolId (candidateSeats selection) ->
          assert (seatIndexWithinBounds seatIndex selection) $ do
            let (_, voterCommitteePublicKey, voterStake, _) =
                  getCandidateInSeat seatIndex selection
            let voterSignaturePublicKey =
                  getSignaturePublicKey (Proxy @c) voterCommitteePublicKey
            let voterVRFVerifyKey =
                  getVRFVerifyKey (Proxy @c) voterCommitteePublicKey
            checkVotePayload voterSignaturePublicKey payload sig
            numSeats <-
              nonPersistentNumSeats
                (VRFVerifyContext voterVRFVerifyKey vrfOutput)
                voterStake
                electionId
                selection
            if unNumSeats numSeats > 0
              then pure (k (Just (mkNonPersistentMember @c numSeats voterStake)))
              else pure (k Nothing)
      | otherwise ->
          Left (NotANonPersistentMember poolId)
