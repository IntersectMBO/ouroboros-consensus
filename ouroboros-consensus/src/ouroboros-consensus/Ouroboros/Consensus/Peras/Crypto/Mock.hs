{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Mocked crypto for Peras.
--
-- This crypto scheme doesn't use any real cryptography, and should only be used
-- for testing purposes. The only possible error thrown by this implementation
-- is when a seat index is out of bounds for a given voting committee.
module Ouroboros.Consensus.Peras.Crypto.Mock
  ( -- * Mocked crypto for Peras
    MockPerasVotingCommitteeScheme
  , MockPerasCrypto
  , VotingCommittee (..)
  , VotingCommitteeInput (..)
  , VotingCommitteeError (..)
  , EligibilityWitness (..)
  , Vote (..)
  , Cert (..)

    -- * Helpers
  , seatIndexToInt
  , unsafeIntToSeatIndex
  , getEligibilityWitness
  ) where

import Cardano.Prelude (Bifunctor (second))
import Codec.Serialise (Serialise)
import Control.Exception.Base (Exception)
import Data.Either.Extra (maybeToEither)
import qualified Data.List as List
import Data.List.Extra ((!?))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set.NonEmpty as NESet
import Data.Typeable (Typeable)
import Data.Word (Word16)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.Block.Abstract (HeaderHash, Point, StandardHash)
import Ouroboros.Consensus.Committee.Class
  ( CryptoSupportsVotingCommittee (..)
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
import Ouroboros.Consensus.Committee.Types (LedgerStake (..), PoolId)
import Ouroboros.Consensus.Peras.Cert.Mock (MockPerasCert (..))
import Ouroboros.Consensus.Peras.Types
  ( PerasRoundNo
  , PerasSeatIndex (..)
  , VoteWeight (..)
  )
import Ouroboros.Consensus.Peras.Vote.Mock (MockPerasVote (..))
import Ouroboros.Consensus.Peras.Voting.Adapter
  ( PerasCertCompatibleWithVotingCommittee (..)
  , PerasVoteCompatibleWithVotingCommittee (..)
  )

-- * | A mock crypto scheme for Peras.

-- | A mock crypto scheme for Peras
data MockPerasCrypto blk
  deriving (Show, Eq, Generic, NoThunks)

type instance ElectionId (MockPerasCrypto blk) = PerasRoundNo
type instance VoteCandidate (MockPerasCrypto blk) = Point blk

type instance PrivateKey (MockPerasCrypto blk) = ()
type instance PublicKey (MockPerasCrypto blk) = ()

instance CryptoSupportsVoteSigning (MockPerasCrypto blk) where
  type VoteSigningKey (MockPerasCrypto blk) = ()
  type VoteVerificationKey (MockPerasCrypto blk) = ()
  data VoteSignature (MockPerasCrypto blk) = MockVoteSignature
  getVoteSigningKey _proxy privateKey = privateKey
  getVoteVerificationKey _proxy publicKey = publicKey
  signVote _ _ _ = MockVoteSignature
  verifyVoteSignature _ _ _ _ = Right ()

instance CryptoSupportsAggregateVoteSigning (MockPerasCrypto blk) where
  type AggregateVoteSignature (MockPerasCrypto blk) = ()
  type AggregateVoteVerificationKey (MockPerasCrypto blk) = ()
  aggregateVoteSignatures _ _ = Right ()
  verifyAggregateVoteSignature _ _ _ _ _ = Right ()
  aggregateVoteVerificationKeys _ _ = Right ()

-- | A mock voting committee scheme for Peras
data MockPerasVotingCommitteeScheme blk
  deriving (Show, Eq, Generic, NoThunks)

newtype instance VotingCommittee crypto (MockPerasVotingCommitteeScheme blk)
  = MockPerasVotingCommittee
  { -- Stake distribution
    weightDistr :: NonEmpty (PoolId, VoteWeight)
  }

instance
  ( Ord (ElectionId crypto)
  , ElectionId crypto ~ PerasRoundNo
  , VoteCandidate crypto ~ Point blk
  , CryptoSupportsAggregateVoteSigning crypto
  ) =>
  CryptoSupportsVotingCommittee crypto (MockPerasVotingCommitteeScheme blk)
  where
  newtype VotingCommitteeInput crypto (MockPerasVotingCommitteeScheme blk)
    = MockPerasVotingCommitteeInput (NonEmpty (PoolId, LedgerStake))

  newtype VotingCommitteeError crypto (MockPerasVotingCommitteeScheme blk)
    = -- Seat index is out of bounds for the voting committee
      MissingSeatIndex PerasSeatIndex

  data EligibilityWitness crypto (MockPerasVotingCommitteeScheme blk)
    = MockPerasVotingCommitteeSchemeMember
        !PerasSeatIndex
        !VoteWeight

  newtype Vote crypto (MockPerasVotingCommitteeScheme blk)
    = MockPerasVotingCommitteeSchemeVote (MockPerasVote blk)

  newtype Cert crypto (MockPerasVotingCommitteeScheme blk)
    = MockPerasVotingCommitteeSchemeCert (MockPerasCert blk)

  mkVotingCommittee (MockPerasVotingCommitteeInput stakeDistr) =
    Right
      MockPerasVotingCommittee
        { weightDistr = second normalize <$> stakeDistr
        }
   where
    LedgerStake totalStake = sum (snd <$> stakeDistr)
    normalize (LedgerStake stake)
      | totalStake == 0 = 0
      | otherwise = VoteWeight (stake / totalStake)

  checkShouldVote MockPerasVotingCommittee{weightDistr} poolId _ _ =
    case findWithIndex (\(pid, _) -> pid == poolId) weightDistr of
      Just (rawIndex, (_pid, voteWeight)) ->
        Right . Just $
          MockPerasVotingCommitteeSchemeMember
            (unsafeIntToSeatIndex rawIndex)
            voteWeight
      _ ->
        Right Nothing
   where
    findWithIndex :: (a -> Bool) -> NonEmpty a -> Maybe (Int, a)
    findWithIndex p xs = List.find (p . snd) (zip [0 ..] (NonEmpty.toList xs))

  forgeVote (MockPerasVotingCommitteeSchemeMember seatIndex _) _ roundNo block =
    MockPerasVotingCommitteeSchemeVote $
      MockPerasVote
        { mockVoteRound = roundNo
        , mockVoteBlock = block
        , mockVoteSeatIndex = seatIndex
        }

  verifyVote
    MockPerasVotingCommittee{weightDistr}
    (MockPerasVotingCommitteeSchemeVote mockVote) =
      case NonEmpty.toList weightDistr !? seatIndexToInt seatIndex of
        Just (_pid, voteWeight) ->
          Right (MockPerasVotingCommitteeSchemeMember seatIndex voteWeight)
        _ ->
          Left (MissingSeatIndex seatIndex)
     where
      seatIndex = mockVoteSeatIndex mockVote

  eligiblePartyVoteWeight
    _
    (MockPerasVotingCommitteeSchemeMember _seatIndex voteWeight) =
      voteWeight

  forgeCert uniqueVoteWithSameTarget = do
    pure $
      MockPerasVotingCommitteeSchemeCert $
        MockPerasCert
          { mockCertRound = roundNo
          , mockCertBlock = block
          , mockCertVoters = voters
          }
   where
    roundNo = getElectionIdFromVotes uniqueVoteWithSameTarget
    block = getVoteCandidateFromVotes uniqueVoteWithSameTarget
    rawVotes = getRawVotes uniqueVoteWithSameTarget
    voters = NESet.fromList (getSeatIndex <$> rawVotes)
    getSeatIndex (MockPerasVotingCommitteeSchemeVote mockVote) =
      mockVoteSeatIndex mockVote

  verifyCert committee (MockPerasVotingCommitteeSchemeCert mockCert) =
    traverse getProof voterList
   where
    voterList = NESet.toList (mockCertVoters mockCert)
    getProof seatIndex =
      maybeToEither (MissingSeatIndex seatIndex) $
        getEligibilityWitness committee seatIndex

  voteTarget
    ( MockPerasVotingCommitteeSchemeVote
        MockPerasVote{mockVoteRound, mockVoteBlock}
      ) =
      (mockVoteRound, mockVoteBlock)

  compareVotesById
    ( MockPerasVotingCommitteeSchemeVote
        MockPerasVote
          { mockVoteRound = mockVoteRound1
          , mockVoteSeatIndex = mockVoteSeatIndex1
          }
      )
    ( MockPerasVotingCommitteeSchemeVote
        MockPerasVote
          { mockVoteRound = mockVoteRound2
          , mockVoteSeatIndex = mockVoteSeatIndex2
          }
      ) =
      compare
        (mockVoteRound1, mockVoteSeatIndex1)
        (mockVoteRound2, mockVoteSeatIndex2)

deriving instance
  Show (VotingCommittee crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  Eq (VotingCommittee crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  NoThunks (VotingCommittee crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  Serialise (VotingCommittee crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  Generic (VotingCommittee crypto (MockPerasVotingCommitteeScheme blk))

deriving instance
  Show (VotingCommitteeInput crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  Eq (VotingCommitteeInput crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  NoThunks (VotingCommitteeInput crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  Serialise (VotingCommitteeInput crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  Generic (VotingCommitteeInput crypto (MockPerasVotingCommitteeScheme blk))

deriving instance
  Show (VotingCommitteeError crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  Eq (VotingCommitteeError crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  NoThunks (VotingCommitteeError crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  Serialise (VotingCommitteeError crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  Generic (VotingCommitteeError crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  ( Typeable crypto
  , Typeable blk
  ) =>
  Exception (VotingCommitteeError crypto (MockPerasVotingCommitteeScheme blk))

deriving instance
  StandardHash blk =>
  Show (Vote crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  StandardHash blk =>
  Eq (Vote crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  StandardHash blk =>
  NoThunks (Vote crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  Serialise
    (HeaderHash blk) =>
  Serialise (Vote crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  Generic (Vote crypto (MockPerasVotingCommitteeScheme blk))

deriving instance
  StandardHash blk =>
  Show (Cert crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  StandardHash blk =>
  Eq (Cert crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  StandardHash blk =>
  NoThunks (Cert crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  Serialise (HeaderHash blk) =>
  Serialise (Cert crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  Generic (Cert crypto (MockPerasVotingCommitteeScheme blk))

deriving instance
  Show (EligibilityWitness crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  Eq (EligibilityWitness crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  NoThunks (EligibilityWitness crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  Serialise (EligibilityWitness crypto (MockPerasVotingCommitteeScheme blk))
deriving instance
  Generic (EligibilityWitness crypto (MockPerasVotingCommitteeScheme blk))

instance
  PerasVoteCompatibleWithVotingCommittee
    (MockPerasVote blk)
    (MockPerasCrypto blk)
    (MockPerasVotingCommitteeScheme blk)
  where
  toPerasVote (MockPerasVotingCommitteeSchemeVote mockVote) =
    Right mockVote
  fromPerasVote mockVote =
    Right (MockPerasVotingCommitteeSchemeVote mockVote)

instance
  PerasCertCompatibleWithVotingCommittee
    (MockPerasCert blk)
    (MockPerasCrypto blk)
    (MockPerasVotingCommitteeScheme blk)
  where
  toPerasCert (MockPerasVotingCommitteeSchemeCert mockCert) =
    Right mockCert
  fromPerasCert mockCert =
    Right (MockPerasVotingCommitteeSchemeCert mockCert)

-- * Helpers

seatIndexToInt :: PerasSeatIndex -> Int
seatIndexToInt (PerasSeatIndex seatIndex) =
  fromIntegral @Word16 @Int seatIndex

unsafeIntToSeatIndex :: Int -> PerasSeatIndex
unsafeIntToSeatIndex int
  | int >= 0 && int <= fromIntegral @Word16 @Int maxBound =
      PerasSeatIndex (fromIntegral @Int @Word16 int)
  | otherwise =
      error $ "unsafeIntToSeatIndex: Int out of bounds for PerasSeatIndex: " <> show int

getEligibilityWitness ::
  VotingCommittee crypto (MockPerasVotingCommitteeScheme blk) ->
  PerasSeatIndex ->
  Maybe (EligibilityWitness crypto (MockPerasVotingCommitteeScheme blk))
getEligibilityWitness MockPerasVotingCommittee{weightDistr} seatIndex = do
  (_poolId, voteWeight) <- NonEmpty.toList weightDistr !? seatIndexToInt seatIndex
  pure $ MockPerasVotingCommitteeSchemeMember seatIndex voteWeight
