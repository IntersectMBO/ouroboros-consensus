{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Test properties for the EveryoneVotes implementation using TestCrypto.
module Test.Consensus.Committee.EveryoneVotes.Tests (tests) where

import Cardano.Ledger.BaseTypes
import Data.Function (on)
import Data.List (intercalate, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Ouroboros.Consensus.Committee.Class
  ( CryptoSupportsVotingCommittee (..)
  , ensureUniqueVotesWithSameTarget
  )
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
  , genPools
  , genPositiveStake
  , mkBucket
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
    , testProperty
        "prop_forgeCert_verifyCert"
        prop_forgeCert_verifyCert
    ]

-- | If a pool is entitled to vote in a given committee, the vote it casts
-- should be verifiable under the same committee.
prop_checkShouldVote_verifyVote :: Property
prop_checkShouldVote_verifyVote =
  forAll (genPools 1000 TestCrypto.genKeyPair) $ \pools ->
    forAll (samplePool pools) $ \(poolId, poolPrivateKey, poolStake) ->
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
    forAll (samplePool pools) $ \(_, poolPrivateKey, poolStake) ->
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
                      extWFAStakeDistr
                  )
                  `onError` \err ->
                    error ("mkVotingCommittee failed: " <> show err)
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

-- | If we forge a certificate from many votes with the same target, verifying
-- it should succeed and return the eligibility witnesses.
prop_forgeCert_verifyCert :: Property
prop_forgeCert_verifyCert =
  forAll (genPools 1000 TestCrypto.genKeyPair) $ \pools ->
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
                    extWFAStakeDistr
                )
                `onError` \err ->
                  error ("mkVotingCommittee failed: " <> show err)
        -- Forge votes from all eligible pools
        let (votes, originalWitnesses) =
              unzip $
                [ ( castVote privateKey electionId candidate witness
                  , witness
                  )
                | (poolId, (privateKey, _, _)) <- Map.toList pools
                , Just witness <-
                    [ checkShouldVote
                        committee
                        poolId
                        privateKey
                        electionId
                        `onError` \err ->
                          error ("checkShouldVote failed: " <> show err)
                    ]
                ]
        tabulateNumPools pools
          . tabulateNumVotes (length votes)
          $ case votes of
            [] ->
              -- No eligible voters, nothing to test
              property True
            firstVote : nextVotes -> do
              let uniqueVotesWithSameTarget =
                    ensureUniqueVotesWithSameTarget
                      ( \case
                          EveryoneVotesVote _ eid cand _ -> (eid, cand)
                      )
                      ( compare `on` \case
                          EveryoneVotesVote seatIndex _ _ _ -> seatIndex
                      )
                      (firstVote :| nextVotes)
                      `onError` \_ ->
                        error "votes don't have the same target!"
              let cert =
                    forgeCert uniqueVotesWithSameTarget
                      `onError` \err ->
                        error ("forgeCert failed: " <> show err)
              case verifyCert committee cert of
                Left err ->
                  counterexample
                    ("certificate verification failed: " <> show err)
                    $ property False
                Right witnesses -> do
                  let witnessesFromCert = NonEmpty.toList witnesses
                  counterexample
                    ( unlines
                        [ "witnesses mismatch!"
                        , "expected these " <> show (length originalWitnesses) <> " witnesses:"
                        , intercalate "\n" (fmap showWitness originalWitnesses)
                        , "but got these " <> show (length witnessesFromCert) <> " witnesses:"
                        , intercalate "\n" (fmap showWitness witnessesFromCert)
                        ]
                    )
                    $ cmpWitnesses originalWitnesses witnessesFromCert

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

samplePool ::
  Map PoolId (PrivateKey TestCrypto, PublicKey TestCrypto, LedgerStake) ->
  Gen (PoolId, PrivateKey TestCrypto, LedgerStake)
samplePool pools =
  elements
    [ (poolId, privateKey, ledgerStake)
    | (poolId, (privateKey, _, ledgerStake)) <- Map.toList pools
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

cmpWitnesses ::
  [EligibilityWitness TestCrypto EveryoneVotes] ->
  [EligibilityWitness TestCrypto EveryoneVotes] ->
  Bool
cmpWitnesses ws1 ws2 =
  length ws1 == length ws2
    && all
      (uncurry cmpWitness)
      (zip (sortBySeatIndex ws1) (sortBySeatIndex ws2))
 where
  sortBySeatIndex =
    sortOn $ \case
      EveryoneVotesMember seatIndex _ -> seatIndex

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

tabulateNumVotes ::
  Int ->
  Property ->
  Property
tabulateNumVotes numVotes =
  tabulate
    "Number of votes"
    [mkBucket 100 (fromIntegral numVotes)]
