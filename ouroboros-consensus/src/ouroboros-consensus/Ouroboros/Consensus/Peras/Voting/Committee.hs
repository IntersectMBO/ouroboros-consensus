{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Support for using concrete votes and certificates with multiple voting
-- committee implementations.
module Ouroboros.Consensus.Peras.Voting.Committee
  ( -- * Peras support for multiple voting committee implementations
    PerasConversionError (..)
  , PerasVoteCompatibleWithVotingCommiittee (..)
  , PerasCertCompatibleWithVotingCommiittee (..)
  ) where

import Data.Containers.NonEmpty (HasNonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import Data.Maybe (isJust)
import Data.Word (Word16, Word64)
import qualified Ouroboros.Consensus.Committee.Class as Committee
import Ouroboros.Consensus.Committee.Crypto (CryptoSupportsVRF (..))
import Ouroboros.Consensus.Committee.EveryoneVotes
  ( Cert (..)
  , EveryoneVotes
  , Vote (..)
  )
import Ouroboros.Consensus.Committee.WFA (SeatIndex (..))
import Ouroboros.Consensus.Committee.WFALS (Cert (..), Vote (..), WFALS)
import qualified Ouroboros.Consensus.Peras.Cert.V1 as V1
import Ouroboros.Consensus.Peras.Crypto.BLS (PerasBLSCrypto)
import Ouroboros.Consensus.Peras.Types (PerasSeatIndex (..))
import qualified Ouroboros.Consensus.Peras.Vote.V1 as V1

-- * Peras support for multiple voting committee implementations

-- | Errors that can occur when converting between Peras and committee types
data PerasConversionError
  = EveryoneVotesButFoundNonPersistentVoterInVote SeatIndex
  | EveryoneVotesButFoundNonPersistentVotersInCert (NE [SeatIndex])
  | SeatIndexOverflowError Word64
  | CryptoError String
  deriving stock (Eq, Show)

-- | Conversion between (concrete) Peras votes and (abstract) committee votes.
--
-- NOTE: the functional dependency @vote -> crypto@ explicitly ties each
-- concrete Peras vote type to a specific crypto scheme.
class
  PerasVoteCompatibleWithVotingCommiittee vote crypto committee
    | vote -> crypto
  where
  toPerasVote ::
    Committee.Vote crypto committee ->
    Either PerasConversionError vote
  fromPerasVote ::
    vote ->
    Either PerasConversionError (Committee.Vote crypto committee)

-- | Conversion between (concrete) Peras certificates and (abstract) committee
-- certificates.
--
-- NOTE: the functional dependency @cert -> crypto@ explicitly ties each
-- concrete Peras certificate type to a specific crypto scheme.
class
  PerasCertCompatibleWithVotingCommiittee cert crypto committee
    | cert -> crypto
  where
  toPerasCert ::
    Committee.Cert crypto committee ->
    Either PerasConversionError cert
  fromPerasCert ::
    cert ->
    Either PerasConversionError (Committee.Cert crypto committee)

-- 'V1.PerasVote's are compatible with 'WFALS' as long as we make sure to avoid
-- overflowing their `Word16` seat index.
instance
  PerasVoteCompatibleWithVotingCommiittee
    V1.PerasVote
    PerasBLSCrypto
    WFALS
  where
  toPerasVote = \case
    WFALSPersistentVote seatIndex electionId candidate sig -> do
      perasSeatIndex <- toPerasSeatIndex seatIndex
      pure $
        V1.PerasVote
          { V1.pvRoundNo = electionId
          , V1.pvBoostedBlock = candidate
          , V1.pvSeatIndex = perasSeatIndex
          , V1.pvEligibilityProof = V1.PersistentPerasVoteEligibilityProof
          , V1.pvSignature = sig
          }
    WFALSNonPersistentVote seatIndex electionId candidate vrfOutput sig -> do
      perasSeatIndex <- toPerasSeatIndex seatIndex
      let proof = V1.NonPersistentPerasVoteEligibilityProof vrfOutput
      pure $
        V1.PerasVote
          { V1.pvRoundNo = electionId
          , V1.pvBoostedBlock = candidate
          , V1.pvSeatIndex = perasSeatIndex
          , V1.pvEligibilityProof = proof
          , V1.pvSignature = sig
          }

  fromPerasVote = \case
    V1.PerasVote electionId candidate seatIndex proof sig -> do
      let seatIndex' = fromPerasSeatIndex seatIndex
      case proof of
        V1.PersistentPerasVoteEligibilityProof ->
          pure $
            WFALSPersistentVote
              seatIndex'
              electionId
              candidate
              sig
        V1.NonPersistentPerasVoteEligibilityProof vrfOutput ->
          pure $
            WFALSNonPersistentVote
              seatIndex'
              electionId
              candidate
              vrfOutput
              sig

-- 'V1.PerasCert's are compatible with 'WFALS' as long as we make sure to avoid
-- overflowing the `Word16` seat index of each voter.
instance
  PerasCertCompatibleWithVotingCommiittee
    V1.PerasCert
    PerasBLSCrypto
    WFALS
  where
  toPerasCert = \case
    WFALSCert electionId candidate voters sig -> do
      voters' <- toPerasCertVoters voters
      pure $
        V1.PerasCert
          { V1.pcRoundNo = electionId
          , V1.pcBoostedBlock = candidate
          , V1.pcVoters = voters'
          , V1.pcSignature = sig
          }

  fromPerasCert = \case
    V1.PerasCert electionId candidate voters sig -> do
      let voters' = fromPerasCertVoters voters
      pure $
        WFALSCert
          electionId
          candidate
          voters'
          sig

-- 'V1.PerasVote's are compatible with 'EveryoneVotes' as long as we make sure
-- to only accept votes with persistent elegibility proofs (in addition to
-- avoiding overflowing their `Word16` seat index).
instance
  PerasVoteCompatibleWithVotingCommiittee
    V1.PerasVote
    PerasBLSCrypto
    EveryoneVotes
  where
  toPerasVote = \case
    EveryoneVotesVote seatIndex electionId candidate sig -> do
      perasSeatIndex <- toPerasSeatIndex seatIndex
      pure $
        V1.PerasVote
          { V1.pvRoundNo = electionId
          , V1.pvBoostedBlock = candidate
          , V1.pvSeatIndex = perasSeatIndex
          , V1.pvEligibilityProof = V1.PersistentPerasVoteEligibilityProof
          , V1.pvSignature = sig
          }

  fromPerasVote = \case
    V1.PerasVote electionId candidate seatIndex proof sig -> do
      let seatIndex' = fromPerasSeatIndex seatIndex
      case proof of
        V1.PersistentPerasVoteEligibilityProof ->
          pure $
            EveryoneVotesVote
              seatIndex'
              electionId
              candidate
              sig
        V1.NonPersistentPerasVoteEligibilityProof _ ->
          Left $
            EveryoneVotesButFoundNonPersistentVoterInVote seatIndex'

-- 'V1.PerasCert's are compatible with 'EveryoneVotes' as long as we make sure
-- to only accept certificates containing only persistent elegibility proofs
-- (in addition to avoiding overflowing the `Word16` seat index of each voter).
instance
  PerasCertCompatibleWithVotingCommiittee
    V1.PerasCert
    PerasBLSCrypto
    EveryoneVotes
  where
  toPerasCert = \case
    EveryoneVotesCert electionId candidate voters sig -> do
      voters' <-
        toPerasCertVoters
          . NEMap.fromSet (const Nothing)
          $ voters
      pure $
        V1.PerasCert
          { V1.pcRoundNo = electionId
          , V1.pcBoostedBlock = candidate
          , V1.pcVoters = voters'
          , V1.pcSignature = sig
          }

  fromPerasCert = \case
    V1.PerasCert electionId candidate voters sig -> do
      let voters' = fromPerasCertVoters voters
      case nonPersistentVoters voters' of
        Nothing ->
          pure $
            EveryoneVotesCert
              electionId
              candidate
              (NEMap.keysSet voters')
              sig
        Just nonPersistentSeatIndices ->
          Left $
            EveryoneVotesButFoundNonPersistentVotersInCert
              nonPersistentSeatIndices
   where
    nonPersistentVoters voters' =
      case Map.keys (NEMap.filter isJust voters') of
        [] ->
          Nothing
        nonPersistentSeats ->
          Just (NonEmpty.fromList nonPersistentSeats)

-- * Helpers

-- | Convert a Peras seat index to a committee seat index.
fromPerasSeatIndex ::
  PerasSeatIndex ->
  SeatIndex
fromPerasSeatIndex (PerasSeatIndex seatIndex) =
  SeatIndex (fromIntegral @Word16 @Word64 seatIndex)

-- | Convert a committee seat index to a Peras seat index
--
-- NOTE: this can fail if the seat index in the committee vote or certificate
-- overflows the smaller 'Word16' type used by Peras votes and certificates.
-- In practice, this should never happen unless there is a bug in the voting
-- committee logic.
toPerasSeatIndex ::
  SeatIndex ->
  Either PerasConversionError PerasSeatIndex
toPerasSeatIndex (SeatIndex seatIndex)
  | seatIndex <= fromIntegral @Word16 @Word64 maxBound =
      Right (PerasSeatIndex (fromIntegral @Word64 @Word16 seatIndex))
  | otherwise =
      Left (SeatIndexOverflowError seatIndex)

-- | Convert concrete Peras certificate voters to abstract committee voters
fromPerasCertVoters ::
  V1.PerasCertVoters ->
  NE (Map SeatIndex (Maybe (VRFOutput PerasBLSCrypto)))
fromPerasCertVoters voters =
  NEMap.fromAscList
    . NonEmpty.map
      ( \(seatIndex, proof) ->
          ( fromPerasSeatIndex seatIndex
          , fromPerasVoteEligibilityProof proof
          )
      )
    . NEMap.toAscList
    . V1.unPerasCertVoters
    $ voters
 where
  fromPerasVoteEligibilityProof = \case
    V1.PersistentPerasVoteEligibilityProof -> Nothing
    V1.NonPersistentPerasVoteEligibilityProof vrfOutput -> Just vrfOutput

-- | Convert abstract committee voters to concrete Peras certificate voters
toPerasCertVoters ::
  NE (Map SeatIndex (Maybe (VRFOutput PerasBLSCrypto))) ->
  Either PerasConversionError V1.PerasCertVoters
toPerasCertVoters voters =
  fmap V1.PerasCertVoters
    . fmap NEMap.fromAscList
    . traverse
      ( \(seatIndex, proof) -> do
          seatIndex' <- toPerasSeatIndex seatIndex
          let proof' = toPerasVoteEligibilityProof proof
          pure (seatIndex', proof')
      )
    . NEMap.toAscList
    $ voters
 where
  toPerasVoteEligibilityProof = \case
    Nothing -> V1.PersistentPerasVoteEligibilityProof
    Just vrfOutput -> V1.NonPersistentPerasVoteEligibilityProof vrfOutput
