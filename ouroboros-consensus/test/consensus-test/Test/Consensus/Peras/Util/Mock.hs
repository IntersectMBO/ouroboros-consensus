{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Test.Consensus.Peras.Util.Mock where

import Data.Containers.NonEmpty (NE)
import Data.Either (fromRight)
import Data.Set (Set)
import qualified Data.Set.NonEmpty as NESet
import Ouroboros.Consensus.Block (BlockSupportsPeras (PerasEpochContext), PerasParams (perasWeight))
import Ouroboros.Consensus.Block.SupportsPeras (PerasParams, ValidatedPerasCert (..), ValidatedPerasVote (..), mkPerasParams)
import Ouroboros.Consensus.Committee.Class
import Ouroboros.Consensus.Peras.Cert.Mock (MockPerasCert (..))
import Ouroboros.Consensus.Peras.Crypto.Mock
  ( MockPerasCommittee
  , MockPerasCrypto
  , VotingCommittee (MockPerasVotingCommittee)
  , VotingCommitteeInput (MockPerasVotingCommitteeInput)
  , getEligibility
  , unsafeIntToSeatIndex
  )
import Ouroboros.Consensus.Peras.Types (PerasSeatIndex (..))
import Ouroboros.Consensus.Peras.Vote.Mock (MockPerasVote (..))
import Test.Consensus.Peras.Util.Internal
import Test.QuickCheck (Gen, choose)
import Test.Util.TestBlock (TestBlock)

genMockPerasVotingCommitteeInput ::
  Gen (VotingCommitteeInput (MockPerasCrypto TestBlock) (MockPerasCommittee TestBlock))
genMockPerasVotingCommitteeInput = do
  NonEmptyListWithUniqueIds poolIds <- genNonEmptyListWithUniqueIds id genPoolId
  poolIdsWithStakes <- traverse (\poolId -> (poolId,) <$> genLedgerStake) poolIds
  pure $ MockPerasVotingCommitteeInput poolIdsWithStakes

genMockPerasVotingCommittee ::
  Gen (VotingCommittee (MockPerasCrypto TestBlock) (MockPerasCommittee TestBlock))
genMockPerasVotingCommittee =
  fromRight (error "mkVotingCommittee of O.C.Peras.Crypto.Mock can't fail") . mkVotingCommittee
    <$> genMockPerasVotingCommitteeInput

genPerasParams :: Gen (PerasParams TestBlock)
genPerasParams = pure mkPerasParams

genMockPerasEpochContext :: Gen (PerasEpochContext TestBlock)
genMockPerasEpochContext = (,) <$> genMockPerasVotingCommittee <*> genPerasParams

pickSeatIndexFromCommittee ::
  VotingCommittee (MockPerasCrypto TestBlock) (MockPerasCommittee TestBlock) -> Gen PerasSeatIndex
pickSeatIndexFromCommittee (MockPerasVotingCommittee weightDistr) = do
  let maxIndex = length weightDistr - 1
  unsafeIntToSeatIndex <$> choose (0, maxIndex)

genVotersSubset ::
  VotingCommittee (MockPerasCrypto TestBlock) (MockPerasCommittee TestBlock) ->
  Gen (NE (Set PerasSeatIndex))
genVotersSubset committee = do
  NonEmptyListWithUniqueIds seatIndices <-
    genNonEmptyListWithUniqueIds id (pickSeatIndexFromCommittee committee)
  pure $ NESet.fromList seatIndices

genMockPerasVote ::
  VotingCommittee (MockPerasCrypto TestBlock) (MockPerasCommittee TestBlock) ->
  Gen (MockPerasVote TestBlock)
genMockPerasVote committee = do
  seatIndex <- pickSeatIndexFromCommittee committee
  roundNo <- genRoundNo
  block <- genPointTestBlock
  pure
    MockPerasVote
      { mockVoteSeatIndex = seatIndex
      , mockVoteRound = roundNo
      , mockVoteBlock = block
      }

genMockValidatedPerasVote :: PerasEpochContext TestBlock -> Gen (ValidatedPerasVote TestBlock)
genMockValidatedPerasVote (committee, _params) = do
  vote <- genMockPerasVote committee
  let eligibilityWitness =
        maybe
          ( error
              "genValidatedPerasVote: seatIndex of vote generated from the committee should be part of the committee"
          )
          id
          (getEligibility committee (mockVoteSeatIndex vote))
  pure
    ValidatedPerasVote
      { vpvVote = vote
      , vpvVoteWeight = eligiblePartyVoteWeight committee eligibilityWitness
      }

genMockPerasCert ::
  VotingCommittee (MockPerasCrypto TestBlock) (MockPerasCommittee TestBlock) ->
  Gen (MockPerasCert TestBlock)
genMockPerasCert committee = do
  votersSubset <- genVotersSubset committee
  roundNo <- genRoundNo
  block <- genPointTestBlock
  pure
    MockPerasCert
      { mockCertVoters = votersSubset
      , mockCertRound = roundNo
      , mockCertBlock = block
      }

genMockValidatedPerasCert :: PerasEpochContext TestBlock -> Gen (ValidatedPerasCert TestBlock)
genMockValidatedPerasCert (committee, params) = do
  cert <- genMockPerasCert committee
  pure $
    ValidatedPerasCert
      { vpcCert = cert
      , vpcCertBoost = perasWeight params
      }
