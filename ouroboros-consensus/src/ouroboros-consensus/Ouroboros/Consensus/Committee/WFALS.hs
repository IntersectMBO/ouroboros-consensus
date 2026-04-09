{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
module Ouroboros.Consensus.Committee.WFALS
  ( -- * Voting committee interface
    WFALS
  , VotingCommittee -- VotingCommittee internals are not exported
  , VotingCommitteeInput (..)
  , VotingCommitteeError (..)
  , EligibilityWitness (..)
  , Vote (..)
  , Cert (..)

    -- * Metrics about the voting committee composition
  , candidateSeats
  , persistentCommitteeSize
  , nonPersistentCommitteeSize
  , totalPersistentStake
  , totalNonPersistentStake
  ) where

import Cardano.Ledger.BaseTypes (NonZero (..), Nonce, nonZero)
import Control.Monad (void)
import Control.Monad.Zip (MonadZip (..))
import qualified Data.Array as Array
import Data.Bifunctor (Bifunctor (..))
import Data.Containers.NonEmpty (HasNonEmpty (..))
import Data.Data (Proxy (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.NonEmpty as NEMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Ouroboros.Consensus.Committee.Class
  ( CryptoSupportsVotingCommittee (..)
  , UniqueVotesWithSameTarget
  , getElectionIdFromVotes
  , getRawVotes
  , getVoteCandidateFromVotes
  )
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsAggregateVoteSigning (..)
  , CryptoSupportsBatchVRFVerification (..)
  , CryptoSupportsVRF (..)
  , CryptoSupportsVoteSigning (..)
  , ElectionId
  , PrivateKey
  , PublicKey
  , VRFPoolContext (..)
  , VoteCandidate
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
  , getCandidateIfSeatWithinBounds
  , unsafeGetCandidateInSeat
  , weightedFaitAccompliSplitSeats
  )

-- | Tag for weighted Fait-Accompli with Local Sortition (wFA^LS)
data WFALS

instance
  ( CryptoSupportsAggregateVoteSigning crypto
  , CryptoSupportsBatchVRFVerification crypto
  ) =>
  CryptoSupportsVotingCommittee crypto WFALS
  where
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
  data VotingCommittee crypto WFALS
    = WFALSVotingCommittee
    { -- Preaccumulated stake distribution used to compute committee composition
      extWFAStakeDistr :: !(ExtWFAStakeDistr (PublicKey crypto))
    , -- Index of a given candidate in the cumulative stake distribution
      candidateSeats :: !(Map PoolId SeatIndex)
    , -- Number of persistent seats granted by the weighted Fait-Accompli scheme
      persistentCommitteeSize :: !PersistentCommitteeSize
    , -- Expected number of non-persistent voters
      nonPersistentCommitteeSize :: !NonPersistentCommitteeSize
    , --  Total stake of persistent voters
      totalPersistentStake :: !TotalPersistentStake
    , -- Total stake of non-persistent voters
      totalNonPersistentStake :: !TotalNonPersistentStake
    , --  Epoch nonce of the epoch where this committee selection takes place
      epochNonce :: !Nonce
    }

  data VotingCommitteeInput crypto WFALS
    = WFALSVotingCommitteeInput
        -- Epoch nonce for the epoch where this voting committee takes place
        !Nonce
        -- Expected committee size for this voting committee
        !TargetCommitteeSize
        -- Extended cumulative stake distribution of the potential voters
        !(ExtWFAStakeDistr (PublicKey crypto))

  data VotingCommitteeError crypto WFALS
    = -- An error occurred during the computation of the committee selection
      WFAError WFAError
    | -- Pool ID is missing from the voting committee
      MissingPoolId PoolId
    | -- Voter claims to be a persistent member of the committee, but it's not
      NotAPersistentMember SeatIndex
    | -- Voter claims to be a non-persistent member of the committee, but it's not
      NotANonPersistentMember SeatIndex
    | -- The VRF evaluation returned zero non-persistent seats
      ZeroNonPersistentSeats SeatIndex
    | -- The vote signature is invalid
      InvalidVoteSignature String
    | -- The voter eligibility is invalid
      InvalidVoterEligibilityProof String
    | -- The certificate signature is invalid
      InvalidCertSignature String
    | -- We triggered an unexpected cryptographic error
      CryptoError String
    deriving (Show, Eq)

  data EligibilityWitness crypto WFALS
    = -- A persistent member of the voting committee
      WFALSPersistentMember
        !SeatIndex
        !LedgerStake
    | -- A realized non-persistent member of the voting committee
      WFALSNonPersistentMember
        !SeatIndex
        !LedgerStake
        !(VRFOutput crypto)
        !(NonZero LocalSortitionNumSeats)

  data Vote crypto WFALS
    = WFALSPersistentVote
        !SeatIndex
        !(ElectionId crypto)
        !(VoteCandidate crypto)
        !(VoteSignature crypto)
    | WFALSNonPersistentVote
        !SeatIndex
        !(ElectionId crypto)
        !(VoteCandidate crypto)
        !(VRFOutput crypto)
        !(VoteSignature crypto)

  data Cert crypto WFALS
    = WFALSCert
        !(ElectionId crypto)
        !(VoteCandidate crypto)
        !(NE (Map SeatIndex (Maybe (VRFOutput crypto))))
        !(AggregateVoteSignature crypto)

  mkVotingCommittee = mkWFALSVotingCommittee
  checkShouldVote = implCheckShouldVote
  forgeVote = implForgeVote
  verifyVote = implVerifyVote
  eligiblePartyVoteWeight = implEligiblePartyVoteWeight
  forgeCert = implForgeCert
  verifyCert = implVerifyCert

-- | Construct a 'WFALSVotingCommittee' for a given epoch
mkWFALSVotingCommittee ::
  VotingCommitteeInput crypto WFALS ->
  Either
    (VotingCommitteeError crypto WFALS)
    (VotingCommittee crypto WFALS)
mkWFALSVotingCommittee
  ( WFALSVotingCommitteeInput
      nonce
      totalSeats
      stakeDistr
    ) = do
    ( numPersistentVoters
      , numNonPersistentVoters
      , persistentStake
      , nonPersistentStake
      ) <-
      bimap WFAError id $
        weightedFaitAccompliSplitSeats
          stakeDistr
          totalSeats

    let seats =
          Map.fromList
            [ (poolId, seatIndex)
            | (seatIndex, (poolId, _, _, _)) <-
                Array.assocs (unExtWFAStakeDistr stakeDistr)
            ]

    pure $
      WFALSVotingCommittee
        { extWFAStakeDistr = stakeDistr
        , candidateSeats = seats
        , persistentCommitteeSize = numPersistentVoters
        , nonPersistentCommitteeSize = numNonPersistentVoters
        , totalPersistentStake = persistentStake
        , totalNonPersistentStake = nonPersistentStake
        , epochNonce = nonce
        }

-- | Check whether we should vote in a given election
implCheckShouldVote ::
  forall crypto.
  CryptoSupportsVRF crypto =>
  VotingCommittee crypto WFALS ->
  PoolId ->
  PrivateKey crypto ->
  ElectionId crypto ->
  Either
    (VotingCommitteeError crypto WFALS)
    (Maybe (EligibilityWitness crypto WFALS))
implCheckShouldVote committee ourId ourPrivateKey electionId
  | Just seatIndex <- Map.lookup ourId (candidateSeats committee) = do
      let (_, _, ourStake, _) =
            unsafeGetCandidateInSeat seatIndex (extWFAStakeDistr committee)
      let ourVRFSigningKey =
            getVRFSigningKey (Proxy @crypto) ourPrivateKey
      case isPersistentMember seatIndex committee of
        True -> do
          pure $
            Just $
              WFALSPersistentMember
                seatIndex
                ourStake
        False -> do
          let vrfContext =
                VRFSignContext ourVRFSigningKey
          vrfOutput <-
            -- Here we are using @evalVRF@ to compute our own VRF output. If
            -- that fails, it means something went wrong on the crypto side.
            bimap CryptoError id $ do
              evalVRF
                vrfContext
                ( mkVRFElectionInput
                    @crypto
                    (epochNonce committee)
                    electionId
                )
          let numSeats =
                localSortitionNumSeats
                  (nonPersistentCommitteeSize committee)
                  (totalNonPersistentStake committee)
                  ourStake
                  (normalizeVRFOutput vrfOutput)
          case nonZero numSeats of
            Nothing ->
              pure Nothing
            Just nonZeroNumSeats ->
              pure $
                Just $
                  WFALSNonPersistentMember
                    seatIndex
                    ourStake
                    vrfOutput
                    nonZeroNumSeats
  | otherwise =
      Left (MissingPoolId ourId)

-- | Forge a vote for a given election and candidate
implForgeVote ::
  forall crypto.
  CryptoSupportsVoteSigning crypto =>
  EligibilityWitness crypto WFALS ->
  PrivateKey crypto ->
  ElectionId crypto ->
  VoteCandidate crypto ->
  Vote crypto WFALS
implForgeVote member ourPrivateKey electionId candidate =
  case member of
    WFALSPersistentMember seatIndex _ ->
      WFALSPersistentVote seatIndex electionId candidate sig
    WFALSNonPersistentMember seatIndex _ vrfOutput _ ->
      WFALSNonPersistentVote seatIndex electionId candidate vrfOutput sig
 where
  ourVoteSigningKey =
    getVoteSigningKey (Proxy @crypto) ourPrivateKey
  sig =
    signVote ourVoteSigningKey electionId candidate

-- | Verify a vote cast by a committee member in a given election
implVerifyVote ::
  forall crypto.
  ( CryptoSupportsVoteSigning crypto
  , CryptoSupportsVRF crypto
  ) =>
  VotingCommittee crypto WFALS ->
  Vote crypto WFALS ->
  Either
    (VotingCommitteeError crypto WFALS)
    (EligibilityWitness crypto WFALS)
implVerifyVote committee = \case
  WFALSPersistentVote seatIndex electionId candidate sig
    | Just (_, voterPublicKey, voterStake, _) <-
        getCandidateIfSeatWithinBounds seatIndex (extWFAStakeDistr committee)
    , isPersistentMember seatIndex committee -> do
        let voterVerificationKey =
              getVoteVerificationKey (Proxy @crypto) voterPublicKey
        checkVoteSignature voterVerificationKey electionId candidate sig
        pure $
          WFALSPersistentMember
            seatIndex
            voterStake
    | otherwise -> do
        Left (NotAPersistentMember seatIndex)
  WFALSNonPersistentVote seatIndex electionId message vrfOutput sig
    | Just (_, voterPublicKey, voterStake, _) <-
        getCandidateIfSeatWithinBounds seatIndex (extWFAStakeDistr committee)
    , not (isPersistentMember seatIndex committee) -> do
        let voterVoteVerificationKey =
              getVoteVerificationKey (Proxy @crypto) voterPublicKey
        bimap InvalidVoteSignature id $ do
          verifyVoteSignature
            voterVoteVerificationKey
            electionId
            message
            sig
        let voterVRFVerificationKey =
              getVRFVerificationKey (Proxy @crypto) voterPublicKey
        let vrfContext =
              VRFVerifyContext voterVRFVerificationKey vrfOutput
        void $ bimap InvalidVoterEligibilityProof id $ do
          evalVRF
            vrfContext
            ( mkVRFElectionInput
                @crypto
                (epochNonce committee)
                electionId
            )
        let numSeats =
              localSortitionNumSeats
                (nonPersistentCommitteeSize committee)
                (totalNonPersistentStake committee)
                voterStake
                (normalizeVRFOutput vrfOutput)
        case nonZero numSeats of
          Nothing ->
            Left (ZeroNonPersistentSeats seatIndex)
          Just nonZeroNumSeats ->
            pure $
              WFALSNonPersistentMember
                seatIndex
                voterStake
                vrfOutput
                nonZeroNumSeats
    | otherwise ->
        Left (NotANonPersistentMember seatIndex)

-- | Compute the voting power of an eligible committee member
--
-- NOTE: there is a subtle difference between the "Ledger stake" and the "Vote
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
implEligiblePartyVoteWeight ::
  VotingCommittee crypto WFALS ->
  EligibilityWitness crypto WFALS ->
  VoteWeight
implEligiblePartyVoteWeight committee = \case
  -- Persistent members have their voting power equal to their stake
  WFALSPersistentMember
    _seatIndex
    (LedgerStake stake) ->
      VoteWeight stake
  -- Non-persistent members have their voting power proportional to their
  -- number of seats granted by local sortition and their stake (normalized
  -- by the total non-persistent stake)
  WFALSNonPersistentMember
    _seatIndex
    (LedgerStake stake)
    _vrfOutput
    numSeats ->
      VoteWeight $
        fromIntegral (unLocalSortitionNumSeats (unNonZero numSeats))
          * stake
          / nonPersistentStake
     where
      TotalNonPersistentStake (Cumulative (LedgerStake nonPersistentStake)) =
        totalNonPersistentStake committee

-- | Forge a certificate attesting the winner of a given election
implForgeCert ::
  forall crypto.
  CryptoSupportsAggregateVoteSigning crypto =>
  UniqueVotesWithSameTarget crypto WFALS ->
  Either
    (VotingCommitteeError crypto WFALS)
    (Cert crypto WFALS)
implForgeCert votes = do
  aggSig <-
    bimap CryptoError id $
      aggregateVoteSignatures
        (Proxy @crypto)
        voteSignatures
  pure $
    WFALSCert
      (getElectionIdFromVotes votes)
      (getVoteCandidateFromVotes votes)
      (NEMap.fromAscList voters)
      aggSig
 where
  (voters, voteSignatures) =
    munzip $ flip fmap votesInAscendingSeatIndexOrder $ \case
      WFALSPersistentVote seatIndex _ _ sig ->
        ( (seatIndex, Nothing)
        , sig
        )
      WFALSNonPersistentVote seatIndex _ _ vrfOutput sig ->
        ( (seatIndex, Just vrfOutput)
        , sig
        )

  -- Make sure we have votes in ascending seat index order, which is something
  -- 'VotesWithSameTarget' cannot guarantee by itself, since seat indices are
  -- an implementation detail of this voting committee scheme.
  votesInAscendingSeatIndexOrder =
    flip NonEmpty.sortWith (getRawVotes votes) $ \case
      WFALSPersistentVote seatIndex _ _ _ -> seatIndex
      WFALSNonPersistentVote seatIndex _ _ _ _ -> seatIndex

-- | Verify a certificate attesting the winner of a given election
implVerifyCert ::
  forall crypto.
  ( CryptoSupportsAggregateVoteSigning crypto
  , CryptoSupportsBatchVRFVerification crypto
  ) =>
  VotingCommittee crypto WFALS ->
  Cert crypto WFALS ->
  Either
    (VotingCommitteeError crypto WFALS)
    (NE [EligibilityWitness crypto WFALS])
implVerifyCert committee = \case
  WFALSCert electionId candidate voters aggSig -> do
    -- Traverse the list of voters in ascending seat index order, collecting:
    -- 1. their membership status
    -- 2. their vote verification keys (to verify the aggregate vote signature)
    -- 3. optionally, their VRF verification keys and outputs (to verify the
    --    aggregate VRF output for non-persistent voters, if any)
    (members, voteVerificationKeys, optionalVRFKeysAndOutputs) <-
      fmap nonEmptyUnzip3 . flip traverse (NEMap.toAscList voters) $ \case
        -- Persistent voter
        (seatIndex, Nothing)
          | Just (_, voterPublicKey, voterStake, _) <-
              getCandidateIfSeatWithinBounds seatIndex (extWFAStakeDistr committee)
          , isPersistentMember seatIndex committee -> do
              let voterVoteVerificationKey =
                    getVoteVerificationKey (Proxy @crypto) voterPublicKey
              pure
                ( WFALSPersistentMember
                    seatIndex
                    voterStake
                , voterVoteVerificationKey
                , Nothing
                )
          | otherwise ->
              Left (NotAPersistentMember seatIndex)
        -- Non-persistent voter
        (seatIndex, Just vrfOutput)
          | Just (_, voterPublicKey, voterStake, _) <-
              getCandidateIfSeatWithinBounds seatIndex (extWFAStakeDistr committee)
          , not (isPersistentMember seatIndex committee) -> do
              let voterVoteVerificationKey =
                    getVoteVerificationKey (Proxy @crypto) voterPublicKey
              let voterVRFVerificationKey =
                    getVRFVerificationKey (Proxy @crypto) voterPublicKey
              let numSeats =
                    localSortitionNumSeats
                      (nonPersistentCommitteeSize committee)
                      (totalNonPersistentStake committee)
                      voterStake
                      (normalizeVRFOutput vrfOutput)
              case nonZero numSeats of
                Nothing ->
                  Left (ZeroNonPersistentSeats seatIndex)
                Just nonZeroNumSeats ->
                  pure
                    ( WFALSNonPersistentMember
                        seatIndex
                        voterStake
                        vrfOutput
                        nonZeroNumSeats
                    , voterVoteVerificationKey
                    , Just (voterVRFVerificationKey, vrfOutput)
                    )
          | otherwise ->
              Left (NotANonPersistentMember seatIndex)

    -- Verify aggregate signature
    aggVerificationKey <-
      bimap CryptoError id $
        aggregateVoteVerificationKeys
          (Proxy @crypto)
          voteVerificationKeys
    bimap InvalidCertSignature id $
      verifyAggregateVoteSignature
        (Proxy @crypto)
        aggVerificationKey
        electionId
        candidate
        aggSig

    -- Verify VRF outputs for non-persistent voters (if any)
    case catMaybes (NonEmpty.toList optionalVRFKeysAndOutputs) of
      -- No non-persistent voters => no VRF outputs to verify
      [] -> do
        pure ()
      -- Some non-persistent voters => verify their aggregate VRF outputs
      vrfKeysAndOutputs -> do
        let (vrfVerificationKeys, vrfOutputs) =
              munzip
                . NonEmpty.fromList -- safe 'vrfKeysAndOutputs' /= []
                $ vrfKeysAndOutputs
        bimap InvalidCertSignature id $
          batchVerifyVRFOutputs
            vrfVerificationKeys
            ( mkVRFElectionInput
                @crypto
                (epochNonce committee)
                electionId
            )
            vrfOutputs

    -- Return the list of voters attesting the election winner
    pure members

-- * Helpers

-- | Check if a voter is a persistent member of in a voting committee
isPersistentMember ::
  SeatIndex ->
  VotingCommittee crypto WFALS ->
  Bool
isPersistentMember seatIndex committee =
  unSeatIndex seatIndex
    < unPersistentCommitteeSize (persistentCommitteeSize committee)

-- | Check the validity of a vote signature
checkVoteSignature ::
  forall crypto.
  CryptoSupportsVoteSigning crypto =>
  VoteVerificationKey crypto ->
  ElectionId crypto ->
  VoteCandidate crypto ->
  VoteSignature crypto ->
  Either
    (VotingCommitteeError crypto WFALS)
    ()
checkVoteSignature verificationKey electionId message sig =
  bimap InvalidVoteSignature id $ do
    verifyVoteSignature
      verificationKey
      electionId
      message
      sig

-- | Extended unzip3 for 'NonEmpty' lists
nonEmptyUnzip3 ::
  NE [(a, b, c)] ->
  (NE [a], NE [b], NE [c])
nonEmptyUnzip3 ((a, b, c) :| rest) =
  ( a :| restA
  , b :| restB
  , c :| restC
  )
 where
  (restA, restB, restC) =
    unzip3 rest
