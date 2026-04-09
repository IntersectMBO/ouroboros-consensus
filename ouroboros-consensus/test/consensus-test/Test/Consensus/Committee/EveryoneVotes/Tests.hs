{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Test properties for the EveryoneVotes implementation using TestCrypto.
module Test.Consensus.Committee.EveryoneVotes.Tests (tests) where

import Cardano.Ledger.BaseTypes
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee (..))
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsVoteSigning (..)
  , ElectionId
  , PrivateKey
  , PublicKey
  , VoteCandidate
  )
import Ouroboros.Consensus.Committee.EveryoneVotes
  ( EligibilityWitness (..)
  , EveryoneVotes
  , Vote (..)
  , VotingCommitteeInput (..)
  , candidateSeats
  , numActiveVoters
  )
import Ouroboros.Consensus.Committee.Types
  ( LedgerStake (..)
  , PoolId
  )
import Ouroboros.Consensus.Committee.WFA
  ( NumPoolsWithPositiveStake (..)
  , SeatIndex (..)
  , mkExtWFAStakeDistr
  )
import Test.Consensus.Committee.TestCrypto (TestCrypto)
import qualified Test.Consensus.Committee.TestCrypto as TestCrypto
import Test.Consensus.Committee.Utils
  ( eqWithShowCmp
  , genEpochNonce
  , genPools
  , genPositiveStake
  , onError
  , tabulateNumPools
  , tabulatePoolStake
  , unfairWFATiebreaker
  )
import Test.QuickCheck
  ( Gen
  , Property
  , Testable (..)
  , choose
  , counterexample
  , elements
  , forAll
  , forAllShow
  , frequency
  , tabulate
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup
    "Implementation tests using TestCrypto"
    [ adjustQuickCheckTests (* 10) $
        testProperty
          "prop_checkShouldVote_verifyVote"
          prop_checkShouldVote_verifyVote
    , adjustQuickCheckTests (* 10) $
        testProperty
          "prop_fakeVotesDontVerify"
          prop_fakeVotesDontVerify
    ]

-- | If a pool is entitled to vote in a given committee, the vote it casts
-- should be verifiable under the same committee.
prop_checkShouldVote_verifyVote :: Property
prop_checkShouldVote_verifyVote =
  forAll (genPools 1000 TestCrypto.genKeyPair) $ \pools ->
    forAll (samplePoolWithNonZeroStake pools) $ \(poolId, poolPrivateKey, poolStake) ->
      forAll genEpochNonce $ \epochNonce ->
        forAll TestCrypto.genElectionId $ \electionId ->
          forAll TestCrypto.genVoteCandidate $ \candidate -> do
            let extWFAStakeDistr =
                  mkExtWFAStakeDistr
                    unfairWFATiebreaker
                    (Map.map (\(_, pubKey, stake) -> (stake, pubKey)) pools)
                    `onError` \err ->
                      error ("mkExtWFAStakeDistr failed: " <> show err)
            let committee =
                  mkVotingCommittee @TestCrypto @EveryoneVotes
                    ( EveryoneVotesVotingCommitteeInput
                        epochNonce
                        extWFAStakeDistr
                    )
                    `onError` \err ->
                      error ("mkVotingCommittee failed: " <> show err)
            let shouldVote =
                  checkShouldVote
                    committee
                    poolId
                    poolPrivateKey
                    electionId
                    `onError` \err ->
                      error ("checkShouldVote failed: " <> show err)
            tabulateNumPools pools
              . tabulatePoolStake poolStake
              . tabulateShouldVote shouldVote
              $ case shouldVote of
                -- The pool is eligible to vote => cast a vote using their
                -- eligibility witness and make sure it verifies under the
                -- same voting committee.
                Just witness -> do
                  let vote =
                        castVote
                          poolPrivateKey
                          electionId
                          candidate
                          witness
                  case verifyVote committee vote of
                    Left err ->
                      counterexample
                        ("vote verification failed: " <> show err)
                        $ property False
                    Right witness' ->
                      counterexample
                        "vote verification mismatch"
                        $ eqWithShowCmp
                          showWitness
                          cmpWitness
                          witness
                          witness'
                -- The pool is not eligible to vote => do nothing
                Nothing ->
                  property True

-- | Votes cast using fake eligibility witnesses should fail verification.
prop_fakeVotesDontVerify :: Property
prop_fakeVotesDontVerify =
  forAll (genPools 1000 TestCrypto.genKeyPair) $ \pools -> do
    forAll (samplePoolWithNonZeroStake pools) $ \(_, poolPrivateKey, poolStake) ->
      forAll genEpochNonce $ \epochNonce ->
        forAll TestCrypto.genElectionId $ \electionId ->
          forAll TestCrypto.genVoteCandidate $ \candidate -> do
            let extWFAStakeDistr =
                  mkExtWFAStakeDistr
                    unfairWFATiebreaker
                    (Map.map (\(_, pubKey, stake) -> (stake, pubKey)) pools)
                    `onError` \err ->
                      error ("mkExtWFAStakeDistr failed: " <> show err)
            let committee =
                  mkVotingCommittee @TestCrypto @EveryoneVotes
                    ( EveryoneVotesVotingCommitteeInput
                        epochNonce
                        extWFAStakeDistr
                    )
                    `onError` \err ->
                      error ("mkVotingCommitee failed: " <> show err)
            forAllShow
              ( genFakeEligibilityWitness
                  committee
                  poolStake
              )
              (showWitness . snd)
              $ \(fakeWitnessType, fakeWitness) -> do
                let fakeVote =
                      castVote
                        poolPrivateKey
                        electionId
                        candidate
                        fakeWitness
                tabulateNumPools pools
                  . tabulateFakeWitnessType fakeWitnessType
                  $ case verifyVote committee fakeVote of
                    Left _ ->
                      property True
                    Right _ ->
                      counterexample
                        ( unlines
                            [ "vote verification succeeded but should have failed:"
                            , "fake witness type: " <> show fakeWitnessType
                            ]
                        )
                        $ property False

-- * EveryoneVotes helpers

castVote ::
  PrivateKey TestCrypto ->
  ElectionId TestCrypto ->
  VoteCandidate TestCrypto ->
  EligibilityWitness TestCrypto EveryoneVotes ->
  Vote TestCrypto EveryoneVotes
castVote privateKey electionId candidate witness =
  case witness of
    EveryoneVotesMember seatIndex _ ->
      EveryoneVotesVote
        seatIndex
        electionId
        candidate
        sig
 where
  sig =
    signVote @TestCrypto
      (getVoteSigningKey (Proxy @TestCrypto) privateKey)
      electionId
      candidate

-- * Generators

samplePoolWithNonZeroStake ::
  Map PoolId (PrivateKey TestCrypto, PublicKey TestCrypto, LedgerStake) ->
  Gen (PoolId, PrivateKey TestCrypto, LedgerStake)
samplePoolWithNonZeroStake pools =
  elements
    [ (poolId, privateKey, ledgerStake)
    | (poolId, (privateKey, _, ledgerStake)) <- Map.toList pools
    , not (isZero ledgerStake)
    ]

data FakeEligibilityWitnessType
  = PoolWithZeroStake
  | SeatIndexOutOfBounds
  deriving Show

-- | Generate a fake eligibility witness that would fail verification if used
-- to cast a vote.
--
-- This breaks the structure of a valid witness in two ways:
--  1. generating a witness for a pool with zero stake, or
--  2. generating a witness with a seat index outside of the valid range.
genFakeEligibilityWitness ::
  VotingCommittee TestCrypto EveryoneVotes ->
  LedgerStake ->
  Gen (FakeEligibilityWitnessType, EligibilityWitness TestCrypto EveryoneVotes)
genFakeEligibilityWitness
  committee
  poolStake = do
    frequency
      [
        ( if totalSeats > nonZeroStakeSeats then 1 else 0
        , genPoolWithZeroStakeWitness
        )
      ,
        ( 1
        , genSeatIndexOutOfBoundsWitness
        )
      ]
   where
    totalSeats = fromIntegral (Map.size (candidateSeats committee))
    nonZeroStakeSeats = unNumPoolsWithPositiveStake (numActiveVoters committee)

    -- This witness points to a valid seat index but it fakes a non-zero stake
    -- for a pool that actually has zero stake (case 1).
    genPoolWithZeroStakeWitness = do
      seatIndex <- SeatIndex <$> choose (nonZeroStakeSeats, totalSeats - 1)
      ledgerStake <- genPositiveStake
      -- traceShow (">>>>>", nonZeroStakeSeats, totalSeats, seatIndex) $
      pure
        ( PoolWithZeroStake
        , EveryoneVotesMember
            seatIndex
            (unsafeNonZero ledgerStake)
        )

    -- This witness has a seat index that lies outside the valid range (case 2).
    genSeatIndexOutOfBoundsWitness = do
      seatIndex <- SeatIndex <$> choose (totalSeats, totalSeats + 100)
      pure
        ( SeatIndexOutOfBounds
        , EveryoneVotesMember
            seatIndex
            (unsafeNonZero poolStake)
        )

-- * Property helpers

showWitness ::
  EligibilityWitness TestCrypto EveryoneVotes ->
  String
showWitness witness =
  case witness of
    EveryoneVotesMember seatIndex stake ->
      "Member(SeatIndex="
        <> show seatIndex
        <> ", Stake="
        <> show (unLedgerStake (unNonZero stake))
        <> ")"

cmpWitness ::
  EligibilityWitness TestCrypto EveryoneVotes ->
  EligibilityWitness TestCrypto EveryoneVotes ->
  Bool
cmpWitness w1 w2 =
  case (w1, w2) of
    ( EveryoneVotesMember seatIndex1 stake1
      , EveryoneVotesMember seatIndex2 stake2
      ) ->
        seatIndex1 == seatIndex2
          && stake1 == stake2

-- * Tabulation helpers

tabulateShouldVote ::
  Maybe (EligibilityWitness TestCrypto EveryoneVotes) ->
  Property ->
  Property
tabulateShouldVote shouldVote =
  tabulate
    "Should vote"
    [ case shouldVote of
        Nothing ->
          "NoVote"
        Just (EveryoneVotesMember _ _) ->
          "Vote"
    ]

tabulateFakeWitnessType ::
  FakeEligibilityWitnessType ->
  Property ->
  Property
tabulateFakeWitnessType fakeWitnessType =
  tabulate
    "Fake witness type"
    [show fakeWitnessType]
