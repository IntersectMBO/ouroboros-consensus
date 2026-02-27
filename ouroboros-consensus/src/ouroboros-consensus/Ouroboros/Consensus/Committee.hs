{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | Generic committee selection scheme
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
module Ouroboros.Consensus.Committee
  ( -- * Voting committee membership
    MembershipType (..)
  , MembershipProof (..)
  , CommitteeMember (..)

    -- * Committee membership interface
  , CommitteeMemberView (..)
  , CommitteeView (..)
  , mkCommitteeView
  , committeeMemberVoteStake
  , CheckCommitteeMemberError (..)
  , checkCommitteeMember
  ) where

import Cardano.Ledger.BaseTypes (Nonce)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Committee.LS (checkNonPersistentSeats)
import Ouroboros.Consensus.Committee.Types
  ( CommitteeSize
  , NumSeats (..)
  , SeatIndex
  , Stake (..)
  , StakeRole (..)
  , VoterId
  )
import Ouroboros.Consensus.Util (Some (..))

-- * Voting committee membership

-- | Type of committee membership
data MembershipType
  = -- | Persistent membership to the voting committee
    Persistent
  | -- | Non-persistent membership to the voting committee
    NonPersistent
  deriving Show

type MembershipProof :: MembershipType -> Type
data family MembershipProof m

newtype instance MembershipProof Persistent = PersistentMemberProof SeatIndex
newtype instance MembershipProof NonPersistent = NonPersistentMemberProof NumSeats

-- | Committee members
type CommitteeMember :: Type -> MembershipType -> Type
data CommitteeMember c m where
  -- | A persistent member of the voting committee
  PersistentMember ::
    MembershipProof Persistent ->
    Stake Ledger ->
    CommitteeMember c Persistent
  -- | A non-persistent member of the voting committee
  NonPersistentMember ::
    MembershipProof NonPersistent ->
    Stake Ledger ->
    CommitteeMember c NonPersistent

-- * Committee membership interface

type CommitteeMemberView :: Type -> Type
data CommitteeMemberView c = CommitteeMemberView
  { committeeMemberPersistentSeat :: Maybe SeatIndex
  -- ^ Whether the member was granted a persistent seat in the committee
  , committeeMemberStake :: Stake Ledger
  -- ^ Stake of the member as reflected by the ledger state
  }

-- | Interface used to evaluate the membership of a party in avoting committee.
--
-- NOTE: this record is meant to be instantiated at the beginning of its
-- corresponding epoch in order to cache as much of the relevant information
-- about the committee composition as possible.
type CommitteeView :: Type -> Type
data CommitteeView c = CommitteeView
  { committeeMembers :: !(Map VoterId (CommitteeMemberView c))
  -- ^ Status and stake of every candidate member of the voting committee
  , persistentCommitteeSize :: !CommitteeSize
  -- ^ Number of persistent seats granted by the weighted Fait-Accompli scheme
  , nonPersistentCommiteeSize :: !CommitteeSize
  -- ^ Expected number of non-persistent voters
  , totalPersistentStake :: !(Stake Cumulative)
  -- ^ Total stake of persistent voters
  , totalNonPersistentStake :: !(Stake Cumulative)
  -- ^ Total stake of non-persistent voters
  , epochNonce :: !Nonce
  -- ^ Epoch nonce corresponding to the epoch where the election takes place
  }

-- | Construct a 'CommitteeView' for a given epoch
mkCommitteeView :: CommitteeView c
mkCommitteeView =
  error "mkCommitteeView: not implemented yet"

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
committeeMemberVoteStake ::
  CommitteeView c ->
  CommitteeMember c m ->
  Stake Vote
committeeMemberVoteStake cv = \case
  -- Persistent members have their voting power equal to their stake
  PersistentMember _ (LedgerStake stake) ->
    VoteStake stake
  -- Non-persistent members have their voting power proportional to their
  -- number of seats granted by local sortition and their stake (normalized
  -- by the total non-persistent stake)
  NonPersistentMember proof (LedgerStake stake) ->
    VoteStake $ fromIntegral (unNumSeats numSeats) * stake / nonPersistentStake
   where
    NonPersistentMemberProof numSeats = proof
    CumulativeStake nonPersistentStake = totalNonPersistentStake cv

-- | Errors that can occur when checking the committee membership of a given voter
data CheckCommitteeMemberError
  = -- | The voter is not part of the original stake distribution
    MissingVoterId VoterId
  deriving (Show, Eq)

-- | Check whether a committee member should cast a vote on a given election
checkCommitteeMember ::
  VoterId ->
  electionId ->
  CommitteeView c ->
  Either CheckCommitteeMemberError (Maybe (Some (CommitteeMember c)))
checkCommitteeMember voterId electionId committeeView = do
  case Map.lookup voterId (committeeMembers committeeView) of
    Nothing ->
      Left (MissingVoterId voterId)
    Just memberView
      -- Persistent members always vote during their entire tenure in the committee
      | Just seatIndex <- committeeMemberPersistentSeat memberView -> do
          let proof = PersistentMemberProof seatIndex
          let stake = committeeMemberStake memberView
          Right (Just (Some (PersistentMember proof stake)))

      -- Non-persistent members only vote if they are selected via local sortition
      | NumSeats numSeats <- localSortitionSeats memberView
      , numSeats > 0 -> do
          let proof = NonPersistentMemberProof (NumSeats numSeats)
          let stake = committeeMemberStake memberView
          Right (Just (Some (NonPersistentMember proof stake)))

      -- Non-persistent members that are not selected via local sortition do not vote
      | otherwise -> do
          Right Nothing
 where
  localSortitionSeats memberView =
    checkNonPersistentSeats
      (persistentCommitteeSize committeeView)
      (totalPersistentStake committeeView)
      (committeeMemberStake memberView)
      (error "missing: VRF output for local sortition")

--
-- -- | Get the number of non-persistent committee seats (if any) corresponding to a given voter in a certain round
-- nonPersistentCommitteeSeatsFor ::
--   PerasVoterId ->
--   PerasRoundNo ->
--   PerasCommitteeView v ->
--   Maybe (NonPersistent NumPerasCommitteeSeats)
-- nonPersistentCommitteeSeatsFor voterId roundNo pcv = do
--   epochNonce <- epochNonceFor roundNo pcv
--   voterStake <- voterStakeFor voterId pcv
--   let signKeyVRF = error "missing"
--   let certifiedVRF = mkPerasCertifiedVRF epochNonce roundNo signKeyVRF
--   let numNonPersistentSeats = unNonPersistent (nonPersistentCommiteeSize pcv)
--   let totalNonPersistentStake = unNonPersistent (nonPersistentStake pcv)
--   pure $
--     checkNonPersistentSeats
--       certifiedVRF
--       numNonPersistentSeats
--       totalNonPersistentStake
--       voterStake
--
-- -- | Get the voter stake corresponding to a given voter
-- voterStakeFor ::
--   PerasVoterId ->
--   PerasCommitteeView v ->
--   Maybe PerasVoterStake
-- voterStakeFor voterId pcv =
--   Map.lookup voterId (committeeMembers pcv) >>= Just . snd
--
-- -- | Get the epoch nonce corresponding to a given Peras round
-- epochNonceFor ::
--   PerasRoundNo ->
--   PerasCommitteeView v ->
--   Maybe Nonce
-- epochNonceFor pvv roundNo =
--   error "epochNonceForRound: not implemented yet"
--
-- -- -- | Construct a certified VRF value for a given Peras round using a given VRF signing key
-- -- certifiedVRFForRound ::
-- --   ( VRF.VRFAlgorithm v
-- --   , VRF.Signable v PerasInputVRF
-- --   , VRF.ContextVRF v ~ ()
-- --   ) =>
-- --   PerasCommitteeView ->
-- --   PerasRoundNo ->
-- --   VRF.SignKeyVRF v ->
-- --   VRF.CertifiedVRF v PerasInputVRF
-- -- certifiedVRFForRound pvv roundNo signKeyVRF =
-- --   mkPerasCertifiedVRF epochNonce roundNo signKeyVRF
-- --  where
-- --   epochNonce = epochNonceForRound pvv roundNo

--
--
