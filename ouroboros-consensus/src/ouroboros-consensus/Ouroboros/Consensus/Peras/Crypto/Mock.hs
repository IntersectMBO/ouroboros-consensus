{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Peras.Crypto.Mock where
import Ouroboros.Consensus.Committee.Crypto
import Ouroboros.Consensus.Block.SupportsPeras
import Ouroboros.Consensus.Block (Point)
import Ouroboros.Consensus.Peras.Cert.Mock (MockPerasCert (..))
import Ouroboros.Consensus.Peras.Vote.Mock (MockPerasVote (..))
import qualified Data.Set.NonEmpty as NESet
import Ouroboros.Consensus.Committee.EveryoneVotes (EveryoneVotes, Vote (..), Cert (..))
import qualified Data.List.NonEmpty as NonEmpty

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

--   data Vote crypto EveryoneVotes
--     = EveryoneVotesVote
--         !SeatIndex
--         !(ElectionId crypto)
--         !(VoteCandidate crypto)
--         !(VoteSignature crypto)

--   data Cert crypto EveryoneVotes
--     = EveryoneVotesCert
--         !(ElectionId crypto)
--         !(VoteCandidate crypto)
--         !(NE (Set SeatIndex))
--         !(AggregateVoteSignature crypto)

-- data MockPerasCert blk
--   = MockPerasCert
--   { mockCertRound :: PerasRoundNo
--   , mockCertBlock :: Point blk
--   }

-- data MockPerasVote blk
--   = MockPerasVote
--   { mockVoteRound :: PerasRoundNo
--   , mockVoteBlock :: Point blk
--   , mockVoteSeatIndex :: PerasSeatIndex
--   }

type instance ElectionId (MockPerasCrypto blk) = PerasRoundNo
type instance VoteCandidate (MockPerasCrypto blk) = Point blk

instance CryptoSupportsAggregateVoteSigning (MockPerasCrypto blk) where
  type AggregateVoteSignature (MockPerasCrypto blk) = ()
  type AggregateVoteVerificationKey (MockPerasCrypto blk) = ()
  aggregateVoteSignatures _ _ = Right ()
  verifyAggregateVoteSignature _ _ _ _ _ = Right ()
  aggregateVoteVerificationKeys _ _ = Right ()

instance
  PerasVoteCompatibleWithVotingCommittee
    (MockPerasVote blk)
    (MockPerasCrypto blk)
    EveryoneVotes
  where
  toPerasVote (EveryoneVotesVote seatIndex roundNo point _signature) = do
    perasSeatIndex <- toPerasSeatIndex seatIndex
    pure $ MockPerasVote
      { mockVoteRound = roundNo
      , mockVoteBlock = point
      , mockVoteSeatIndex = perasSeatIndex
      }
  fromPerasVote MockPerasVote { mockVoteRound, mockVoteBlock, mockVoteSeatIndex } =
    Right $ EveryoneVotesVote (fromPerasSeatIndex mockVoteSeatIndex) mockVoteRound mockVoteBlock MockVoteSignature

instance
  PerasCertCompatibleWithVotingCommittee
    (MockPerasCert blk)
    (MockPerasCrypto blk)
    EveryoneVotes
  where
  toPerasCert (EveryoneVotesCert roundNo point voters _certSignature) = do
    perasVoters <- do
        perasSeatIndices <- traverse toPerasSeatIndex (NonEmpty.toList $ NESet.toList voters)
        case NonEmpty.nonEmpty perasSeatIndices of
            Nothing -> error "We started with a non-empty set of voters, so this should never happen"
            Just neList -> pure $ NESet.fromList neList
    pure $ MockPerasCert
      { mockCertRound = roundNo
      , mockCertBlock = point
      , mockCertVoters = perasVoters
      }
  fromPerasCert MockPerasCert { mockCertRound, mockCertBlock, mockCertVoters } =
    Right $ EveryoneVotesCert mockCertRound mockCertBlock (NESet.mapMonotonic fromPerasSeatIndex mockCertVoters) ()
