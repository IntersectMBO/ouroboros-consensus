{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | "Everyone Votes" committee selection scheme.
--
-- In this simplified scheme, every registered stake pool operator is a member
-- of the voting committee. There is no sortition or persistent/non-persistent
-- distinction. This is useful as a prototype or baseline implementation.
module Ouroboros.Consensus.Committee.EveryoneVotes
  ( -- * Crypto constraint
    CryptoSupportsEveryoneVotes (..)

    -- * Committee selection data
  , EveryoneVotesCommitteeSelection (..)
  , EveryoneVotesCommitteeSelectionError (..)

    -- * Vote type
  , EveryoneVotesVote (..)
  ) where

import Cardano.Ledger.BaseTypes.NonZero (NonZero, nonZero)
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (Foldable (foldl'))
import Data.Function (on)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import Ouroboros.Consensus.Committee.BitMap (BitMap, bitmapFromIndices, bitmapToIndices)
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsGroupVoteSigning (..)
  , CryptoSupportsVoteSigning (..)
  , ElectionId
  , PrivateKeys
  , PublicKeys
  , VoteMessage
  )
import Ouroboros.Consensus.Committee.Types
  ( CryptoSupportsCommitteeSelection (..)
  , LedgerStake (..)
  , PoolId
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
  , SeatIndex (..)
  , getCandidateInSeat
  , seatIndexWithinBounds
  )

-- | Crypto constraint for the "Everyone Votes" scheme.
--
-- Only vote signing is required (no VRF).
class CryptoSupportsVoteSigning crypto => CryptoSupportsEveryoneVotes crypto

-- | Committee selection data for the "Everyone Votes" scheme.
data EveryoneVotesCommitteeSelection crypto = EveryoneVotesCommitteeSelection
  { evcsElectionId :: !(ElectionId crypto)
  -- ^ Election ID for this committee selection
  , evStakeDistr :: !(ExtWFAStakeDistr (PublicKeys crypto))
  -- ^ Reusing the stake distr type from WFA because it's easier in this prototype
  , evCandidateSeats :: !(Map PoolId SeatIndex)
  }

deriving instance
  (Eq (PublicKeys crypto), Eq (ElectionId crypto)) => Eq (EveryoneVotesCommitteeSelection crypto)

deriving instance
  (Show (PublicKeys crypto), Show (ElectionId crypto)) =>
  Show (EveryoneVotesCommitteeSelection crypto)

-- | Errors that can occur during "Everyone Votes" committee selection.
data EveryoneVotesCommitteeSelectionError crypto
  = EVMissingPoolId PoolId
  | EVInvalidSeatIndex SeatIndex
  | EVZeroStake SeatIndex
  | EVInvalidVoteSignature String
  | EVCertElectionIdMismatch
  | EVEmptyCert
  | EVInvalidGroupVoteSignature String
  deriving (Show, Eq)

-- | A vote in the "Everyone Votes" scheme.
data EveryoneVotesVote crypto = EveryoneVotesVote
  { evSeatIndex :: SeatIndex
  , evElectionId :: ElectionId crypto
  , evMessage :: VoteMessage crypto
  , evSignature :: VoteSignature crypto
  }

deriving instance
  ( Eq (ElectionId crypto)
  , Eq (VoteMessage crypto)
  , Eq (VoteSignature crypto)
  ) =>
  Eq (EveryoneVotesVote crypto)

deriving instance
  ( Show (ElectionId crypto)
  , Show (VoteMessage crypto)
  , Show (VoteSignature crypto)
  ) =>
  Show (EveryoneVotesVote crypto)

-- * Instances

data EveryoneVotesCommitteeMember crypto = EveryoneVotesCommitteeMember
  { evcmCandidate :: Candidate (PublicKeys crypto)
  , evcmInvariant :: NonZero (LedgerStake)
  , evcmVoteWeight :: VoteWeight
  }

deriving instance
  Show (PublicKeys crypto) => Show (EveryoneVotesCommitteeMember crypto)

deriving instance
  Eq (PublicKeys crypto) => Eq (EveryoneVotesCommitteeMember crypto)

instance
  CryptoSupportsEveryoneVotes crypto =>
  CryptoSupportsCommitteeSelection crypto (EveryoneVotesCommitteeSelection crypto)
  where
  type CryptoOf (EveryoneVotesCommitteeSelection crypto) = crypto
  type
    CommitteeSelectionError (EveryoneVotesCommitteeSelection crypto) =
      EveryoneVotesCommitteeSelectionError crypto
  type CommitteeMember (EveryoneVotesCommitteeSelection crypto) = EveryoneVotesCommitteeMember crypto

  checkShouldVote ::
    EveryoneVotesCommitteeSelection crypto ->
    PoolId ->
    PrivateKeys crypto ->
    ElectionId crypto ->
    Either
      (EveryoneVotesCommitteeSelectionError crypto)
      (Maybe (EveryoneVotesCommitteeMember crypto))
  checkShouldVote selection ourId _ _
    | Just seatIndex <- Map.lookup ourId (evCandidateSeats selection) =
        case lookupCommitteeMember selection seatIndex of
          Left (EVZeroStake _) -> pure Nothing
          Right member -> pure (Just member)
          Left err -> Left err
    | otherwise =
        Left (EVMissingPoolId ourId)

  committeeMemberWeight ::
    EveryoneVotesCommitteeMember crypto ->
    VoteWeight
  committeeMemberWeight = evcmVoteWeight

instance
  CryptoSupportsEveryoneVotes crypto =>
  VotingWithCommitteeSelection
    crypto
    (EveryoneVotesCommitteeSelection crypto)
  where
  type Vote (EveryoneVotesCommitteeSelection crypto) = EveryoneVotesVote crypto

  forgeVote member ourPrivateKeys electionId message =
    let (seatIndex, _, _, _, _) = evcmCandidate member
        sig = signVote (getVoteSignaturePrivateKey (Proxy @crypto) ourPrivateKeys) electionId message
     in EveryoneVotesVote
          { evSeatIndex = seatIndex
          , evElectionId = electionId
          , evMessage = message
          , evSignature = sig
          }

  verifyVote selection vote =
    let EveryoneVotesVote seatIndex electionId message sig = vote
     in do
          member <- lookupCommitteeMember selection seatIndex
          let (_, _, voterPublicKeys, _, _) = evcmCandidate member
              voterSignaturePublicKey =
                getVoteSignaturePublicKey (Proxy @crypto) voterPublicKeys
          first EVInvalidVoteSignature $
            verifyVoteSignature
              voterSignaturePublicKey
              electionId
              message
              sig
          pure member

  getElectionIdFromVote (EveryoneVotesVote _ electionId _ _) = electionId
  getVoteMessageFromVote (EveryoneVotesVote _ _ message _) = message

-- | Look up a committee member by seat index.
--
-- Checks that the seat index is within bounds and that the voter has
-- non-zero stake. Does NOT verify signatures.
lookupCommitteeMember ::
  EveryoneVotesCommitteeSelection crypto ->
  SeatIndex ->
  Either (EveryoneVotesCommitteeSelectionError crypto) (EveryoneVotesCommitteeMember crypto)
lookupCommitteeMember selection seatIndex
  | seatIndexWithinBounds seatIndex (evStakeDistr selection) =
      let candidate = getCandidateInSeat seatIndex (evStakeDistr selection)
          (_, _, _, voterStake, _) = candidate
       in case nonZero voterStake of
            Nothing -> Left (EVZeroStake seatIndex)
            Just nzStake ->
              Right
                EveryoneVotesCommitteeMember
                  { evcmCandidate = candidate
                  , evcmInvariant = nzStake
                  , evcmVoteWeight = VoteWeight (unLedgerStake voterStake)
                  }
  | otherwise =
      Left (EVInvalidSeatIndex seatIndex)

data EveryoneVotesCert crypto = EveryoneVotesCert
  { certElectionId :: ElectionId crypto
  , certVoteMessage :: VoteMessage crypto
  , certVoters :: BitMap SeatIndex
  , groupSignature :: GroupVoteSignature crypto
  }

deriving instance
  ( Eq (ElectionId crypto)
  , Eq (VoteMessage crypto)
  , Eq (GroupVoteSignature crypto)
  ) =>
  Eq (EveryoneVotesCert crypto)

deriving instance
  ( Show (ElectionId crypto)
  , Show (VoteMessage crypto)
  , Show (GroupVoteSignature crypto)
  ) =>
  Show (EveryoneVotesCert crypto)

instance
  (CryptoSupportsEveryoneVotes crypto, CryptoSupportsGroupVoteSigning crypto) =>
  VotingWithAggregation crypto (EveryoneVotesCommitteeSelection crypto)
  where
  type Cert (EveryoneVotesCommitteeSelection crypto) = EveryoneVotesCert crypto

  getElectionIdFromCert = certElectionId
  getVoteMessageFromCert = certVoteMessage

  forgeCert ::
    VotesWithSameTarget (EveryoneVotesCommitteeSelection crypto) ->
    Cert (EveryoneVotesCommitteeSelection crypto)
  forgeCert votes =
    let certElectionId = getElectionIdFromVotes votes
        certVoteMessage = getVoteMessageFromVotes votes
        sortedVotes = NE.sortBy (compare `on` evSeatIndex) $ getRawVotes votes
        maxIndex = maxWithDefault 0 evSeatIndex (NE.toList sortedVotes)
        indices = evSeatIndex <$> NE.toList sortedVotes
        certVoters = bitmapFromIndices maxIndex indices
        groupSignature = sconcat $ liftVoteSignature (Proxy @crypto) . evSignature <$> sortedVotes
     in EveryoneVotesCert
          { certElectionId
          , certVoteMessage
          , certVoters
          , groupSignature
          }
   where
    maxWithDefault def f xs = foldl' (\acc x -> max acc (f x)) def xs

  verifyCert ::
    EveryoneVotesCommitteeSelection crypto ->
    Cert (EveryoneVotesCommitteeSelection crypto) ->
    Either
      (EveryoneVotesCommitteeSelectionError crypto)
      (NonEmpty (EveryoneVotesCommitteeMember crypto))
  verifyCert selection EveryoneVotesCert{certElectionId, certVoteMessage, certVoters, groupSignature} = do
    -- Check that the cert's election ID matches the committee selection's election ID
    if certElectionId /= evcsElectionId selection
      then Left EVCertElectionIdMismatch
      else pure ()

    let voterIndices = bitmapToIndices certVoters

    -- Look up members
    sortedMembers <-
      sortOn memberSeatIndex
        <$> mapM (\seatIndex -> lookupCommitteeMember selection seatIndex) voterIndices

    members <- case NE.nonEmpty sortedMembers of
      Nothing -> Left EVEmptyCert
      Just ne -> Right ne

    -- Group signature verification
    let signPubKeys = getVoteSignaturePublicKey (Proxy @crypto) . memberPubKeys <$> sortedMembers
    () <- case NE.nonEmpty signPubKeys of
      Just pubKeys ->
        let groupPublicKey = sconcat $ liftVoteSignaturePublicKey (Proxy @crypto) <$> pubKeys
         in first EVInvalidGroupVoteSignature $
              verifyGroupVoteSignature
                (Proxy @crypto)
                groupPublicKey
                certElectionId
                certVoteMessage
                groupSignature
      Nothing -> pure ()

    Right members
   where
    memberPubKeys m = let (_, _, pk, _, _) = evcmCandidate m in pk
    memberSeatIndex m = let (seatIndex, _, _, _, _) = evcmCandidate m in seatIndex
