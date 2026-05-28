{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Peras.Crypto.Mock
  ( MockPerasVotingCommitteeScheme
  , MockPerasCrypto
  , MockPerasCommittee
  , VotingCommittee (..)
  , VotingCommitteeInput (..)
  , VotingCommitteeError (..)
  , EligibilityWitness (..)
  , Vote (..)
  , Cert (..)
  , seatIndexToInt
  , unsafeIntToSeatIndex
  , getEligibility
  ) where

import Cardano.Prelude (Bifunctor (second))
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
import Ouroboros.Consensus.Block.Abstract (Point)
import Ouroboros.Consensus.Block.SupportsPeras
  ( PerasCertCompatibleWithVotingCommittee (..)
  , PerasRoundNo
  , PerasSeatIndex
  , PerasVoteCompatibleWithVotingCommittee (..)
  , VoteWeight (..)
  )
import Ouroboros.Consensus.Committee.Class
  ( CryptoSupportsVotingCommittee (..)
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
import Ouroboros.Consensus.Peras.Types (PerasSeatIndex (..))
import Ouroboros.Consensus.Peras.Vote.Mock (MockPerasVote (..))

data MockPerasVotingCommitteeScheme

data MockPerasCrypto blk

type instance ElectionId (MockPerasCrypto blk) = PerasRoundNo
type instance VoteCandidate (MockPerasCrypto blk) = Point blk

type instance PrivateKey (MockPerasCrypto blk) = ()
type instance PublicKey (MockPerasCrypto blk) = ()

instance CryptoSupportsVoteSigning (MockPerasCrypto blk) where
  type VoteSigningKey (MockPerasCrypto blk) = ()

  -- \| Key used for verifying votes
  type VoteVerificationKey (MockPerasCrypto blk) = ()

  -- \| Cryptographic signature of a vote
  data VoteSignature (MockPerasCrypto blk) = MockVoteSignature
  getVoteSigningKey _proxy privateKey = privateKey
  getVoteVerificationKey _proxy publicKey = publicKey
  signVote _ _ _ = MockVoteSignature
  verifyVoteSignature _ _ _ _ = Right ()

type instance ElectionId (MockPerasCrypto blk) = PerasRoundNo
type instance VoteCandidate (MockPerasCrypto blk) = Point blk

instance CryptoSupportsAggregateVoteSigning (MockPerasCrypto blk) where
  type AggregateVoteSignature (MockPerasCrypto blk) = ()
  type AggregateVoteVerificationKey (MockPerasCrypto blk) = ()
  aggregateVoteSignatures _ _ = Right ()
  verifyAggregateVoteSignature _ _ _ _ _ = Right ()
  aggregateVoteVerificationKeys _ _ = Right ()

--------------------------------------------------------------------------------
-- Uncomment if we ever need to use 'MockPeras{Vote,Cert}' with 'EveryoneVotes' scheme
--------------------------------------------------------------------------------
-- instance
--   PerasVoteCompatibleWithVotingCommittee
--     (MockPerasVote blk)
--     (MockPerasCrypto blk)
--     EveryoneVotes
--   where
--   toPerasVote (EveryoneVotesVote seatIndex roundNo point _signature) = do
--     perasSeatIndex <- toPerasSeatIndex seatIndex
--     pure $
--       MockPerasVote
--         { mockVoteRound = roundNo
--         , mockVoteBlock = point
--         , mockVoteSeatIndex = perasSeatIndex
--         }
--   fromPerasVote MockPerasVote{mockVoteRound, mockVoteBlock, mockVoteSeatIndex} =
--     Right $
--       EveryoneVotesVote
--         (fromPerasSeatIndex mockVoteSeatIndex)
--         mockVoteRound
--         mockVoteBlock
--         MockVoteSignature

-- instance
--   PerasCertCompatibleWithVotingCommittee
--     (MockPerasCert blk)
--     (MockPerasCrypto blk)
--     EveryoneVotes
--   where
--   toPerasCert (EveryoneVotesCert roundNo point voters _certSignature) = do
--     perasVoters <- do
--       perasSeatIndices <- traverse toPerasSeatIndex (NonEmpty.toList $ NESet.toList voters)
--       case NonEmpty.nonEmpty perasSeatIndices of
--         Nothing -> error "We started with a non-empty set of voters, so this should never happen"
--         Just neList -> pure $ NESet.fromList neList
--     pure $
--       MockPerasCert
--         { mockCertRound = roundNo
--         , mockCertBlock = point
--         , mockCertVoters = perasVoters
--         }
--   fromPerasCert MockPerasCert{mockCertRound, mockCertBlock, mockCertVoters} =
--     Right $
--       EveryoneVotesCert
--         mockCertRound
--         mockCertBlock
--         (NESet.mapMonotonic fromPerasSeatIndex mockCertVoters)
--         ()
--------------------------------------------------------------------------------

data MockPerasCommittee blk

instance
  ( Ord (ElectionId crypto)
  , ElectionId crypto ~ PerasRoundNo
  , VoteCandidate crypto ~ Point blk
  , CryptoSupportsAggregateVoteSigning crypto
  ) =>
  CryptoSupportsVotingCommittee crypto (MockPerasCommittee blk)
  where
  newtype VotingCommittee crypto (MockPerasCommittee blk)
    = MockPerasVotingCommittee
    { -- Stake distribution
      weightDistr :: NonEmpty (PoolId, VoteWeight)
    }
    deriving stock (Show, Eq, Generic, Typeable)
    deriving anyclass NoThunks

  newtype VotingCommitteeInput crypto (MockPerasCommittee blk)
    = MockPerasVotingCommitteeInput (NonEmpty (PoolId, LedgerStake))
    deriving stock (Show, Eq, Generic, Typeable)
    deriving anyclass NoThunks

  newtype VotingCommitteeError crypto (MockPerasCommittee blk)
    = -- Seat index is out of bounds for the voting committee
      MissingSeatIndex PerasSeatIndex
    deriving stock (Show, Eq, Generic, Typeable)
    deriving anyclass (NoThunks, Exception)

  data EligibilityWitness crypto (MockPerasCommittee blk)
    = MockPerasCommitteeMember
        !PerasSeatIndex
        !VoteWeight
    deriving stock (Show, Eq, Generic, Typeable)
    deriving anyclass NoThunks

  newtype Vote crypto (MockPerasCommittee blk)
    = MockPerasCommitteeVote (MockPerasVote blk)
    deriving stock (Show, Eq, Generic, Typeable)
    deriving anyclass NoThunks

  newtype Cert crypto (MockPerasCommittee blk)
    = MockPerasCommitteeCert (MockPerasCert blk)
    deriving stock (Show, Eq, Generic, Typeable)
    deriving anyclass NoThunks

  mkVotingCommittee (MockPerasVotingCommitteeInput stakeDistr) =
    let LedgerStake totalStake = sum (snd <$> stakeDistr)
        normalize (LedgerStake stake) = if totalStake == 0 then 0 else VoteWeight (stake / totalStake)
     in Right MockPerasVotingCommittee{weightDistr = second normalize <$> stakeDistr}
  checkShouldVote MockPerasVotingCommittee{weightDistr} poolId _ _ =
    case findWithIndex (\(pid, _) -> pid == poolId) weightDistr of
      Just (rawIndex, (_pid, voteWeight)) -> Right . Just $ MockPerasCommitteeMember (unsafeIntToSeatIndex rawIndex) voteWeight
      _ -> Right Nothing
   where
    findWithIndex :: (a -> Bool) -> NonEmpty a -> Maybe (Int, a)
    findWithIndex p xs = List.find (p . snd) (zip [0 ..] (NonEmpty.toList xs))
  forgeVote (MockPerasCommitteeMember seatIndex _) _ roundNo block =
    MockPerasCommitteeVote $
      MockPerasVote
        { mockVoteRound = roundNo
        , mockVoteBlock = block
        , mockVoteSeatIndex = seatIndex
        }
  verifyVote MockPerasVotingCommittee{weightDistr} (MockPerasCommitteeVote mockVote) =
    let seatIndex = mockVoteSeatIndex mockVote
     in case NonEmpty.toList weightDistr !? seatIndexToInt seatIndex of
          Just (_pid, voteWeight) -> Right $ MockPerasCommitteeMember seatIndex voteWeight
          _ -> Left (MissingSeatIndex seatIndex)
  eligiblePartyVoteWeight _ (MockPerasCommitteeMember _seatIndex voteWeight) = voteWeight
  forgeCert uniqueVoteWithSameTarget = do
    let roundNo = getElectionIdFromVotes uniqueVoteWithSameTarget
        block = getVoteCandidateFromVotes uniqueVoteWithSameTarget
        rawVotes = getRawVotes uniqueVoteWithSameTarget
    let voters = NESet.fromList $ (\(MockPerasCommitteeVote mockVote) -> mockVoteSeatIndex mockVote) <$> rawVotes
    pure $
      MockPerasCommitteeCert $
        MockPerasCert
          { mockCertRound = roundNo
          , mockCertBlock = block
          , mockCertVoters = voters
          }
  verifyCert committee (MockPerasCommitteeCert mockCert) = do
    let voterList = NESet.toList $ mockCertVoters mockCert
    traverse
      (\seatIndex -> maybeToEither (MissingSeatIndex seatIndex) (getEligibility committee seatIndex))
      voterList

  voteTarget (MockPerasCommitteeVote MockPerasVote{mockVoteRound, mockVoteBlock}) =
    (mockVoteRound, mockVoteBlock)
  compareVotesById
    ( MockPerasCommitteeVote
        MockPerasVote{mockVoteRound = mockVoteRound1, mockVoteSeatIndex = mockVoteSeatIndex1}
      )
    ( MockPerasCommitteeVote
        MockPerasVote{mockVoteRound = mockVoteRound2, mockVoteSeatIndex = mockVoteSeatIndex2}
      ) =
      compare (mockVoteRound1, mockVoteSeatIndex1) (mockVoteRound2, mockVoteSeatIndex2)

instance
  PerasVoteCompatibleWithVotingCommittee
    (MockPerasVote blk)
    (MockPerasCrypto blk) -- We can theoretically use an arbitrary crypto scheme, but we must abide by 'vote -> crypto' fun dep
    (MockPerasCommittee blk)
  where
  toPerasVote (MockPerasCommitteeVote mockVote) =
    Right $ mockVote
  fromPerasVote mockVote =
    Right $ MockPerasCommitteeVote mockVote

instance
  PerasCertCompatibleWithVotingCommittee
    (MockPerasCert blk)
    (MockPerasCrypto blk) -- We can theoretically use an arbitrary crypto scheme, but we must abide by 'vote -> crypto' fun dep
    (MockPerasCommittee blk)
  where
  toPerasCert (MockPerasCommitteeCert mockCert) =
    Right $ mockCert
  fromPerasCert mockCert =
    Right $ MockPerasCommitteeCert mockCert

seatIndexToInt :: PerasSeatIndex -> Int
seatIndexToInt (PerasSeatIndex seatIndex) = fromIntegral @Word16 @Int seatIndex

unsafeIntToSeatIndex :: Int -> PerasSeatIndex
unsafeIntToSeatIndex int
  | int >= 0 && int <= fromIntegral @Word16 @Int maxBound =
      PerasSeatIndex (fromIntegral @Int @Word16 int)
  | otherwise = error $ "unsafeIntToSeatIndex: Int out of bounds for PerasSeatIndex: " <> show int

getEligibility ::
  VotingCommittee crypto (MockPerasCommittee blk) ->
  PerasSeatIndex ->
  Maybe (EligibilityWitness crypto (MockPerasCommittee blk))
getEligibility MockPerasVotingCommittee{weightDistr} seatIndex = do
  (_poolId, voteWeight) <- NonEmpty.toList weightDistr !? seatIndexToInt seatIndex
  pure $ MockPerasCommitteeMember seatIndex voteWeight
