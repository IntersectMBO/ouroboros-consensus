{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Test properties for the weighted Fait-Accompli implementation using TestCrypto.
module Test.Consensus.Committee.WFALS.Tests (tests) where

import Cardano.Ledger.BaseTypes
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Ouroboros.Consensus.Committee.Class (CryptoSupportsVotingCommittee (..))
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsVRF (..)
  , CryptoSupportsVoteSigning (..)
  , ElectionId
  , PrivateKey
  , PublicKey
  , VRFPoolContext (..)
  , VoteCandidate
  , evalVRF
  , mkVRFElectionInput
  )
import Ouroboros.Consensus.Committee.LS (LocalSortitionNumSeats (..))
import qualified Ouroboros.Consensus.Committee.LS as LS
import Ouroboros.Consensus.Committee.Types
  ( LedgerStake (..)
  , PoolId
  , TargetCommitteeSize (..)
  )
import Ouroboros.Consensus.Committee.WFA
  ( NonPersistentCommitteeSize (..)
  , PersistentCommitteeSize (..)
  , SeatIndex (..)
  , mkExtWFAStakeDistr
  )
import Ouroboros.Consensus.Committee.WFALS
  ( EligibilityWitness (..)
  , Vote (..)
  , VotingCommitteeInput (..)
  , WFALS
  , nonPersistentCommitteeSize
  , persistentCommitteeSize
  , totalNonPersistentStake
  )
import Test.Consensus.Committee.TestCrypto (TestCrypto)
import qualified Test.Consensus.Committee.TestCrypto as TestCrypto
import Test.Consensus.Committee.Utils
  ( eqWithShowCmp
  , genEpochNonce
  , genPools
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
    ]

-- | If a pool is entitled to vote in a given committee, the vote it casts
-- should be verifiable under the same committee.
prop_checkShouldVote_verifyVote :: Property
prop_checkShouldVote_verifyVote =
  forAll (genPools 1000 TestCrypto.genKeyPair) $ \pools ->
    forAll (genTargetCommitteeSize pools) $ \targetCommitteeSize ->
      forAll (samplePool pools) $ \(poolId, poolPrivateKey, poolStake) ->
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
                    mkVotingCommittee @TestCrypto @WFALS
                      ( WFALSVotingCommitteeInput
                          epochNonce
                          targetCommitteeSize
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
                . tabulateTargetCommitteeSize targetCommitteeSize
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
                          "vote verificateion mismatch"
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
  forAll (genPools 1000 TestCrypto.genKeyPair) $ \pools ->
    forAll (genTargetCommitteeSize pools) $ \targetCommitteeSize -> do
      forAll (samplePool pools) $ \(_, poolPrivateKey, poolStake) ->
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
                    mkVotingCommittee @TestCrypto @WFALS
                      ( WFALSVotingCommitteeInput
                          epochNonce
                          targetCommitteeSize
                          extWFAStakeDistr
                      )
                      `onError` \err ->
                        error ("mkVotingCommitee failed: " <> show err)
              forAllShow
                ( genFakeEligibilityWitness
                    committee
                    poolPrivateKey
                    poolStake
                    epochNonce
                    electionId
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
                    . tabulateTargetCommitteeSize targetCommitteeSize
                    . tabulateFakePersistentWitnessType fakeWitnessType
                    $ do
                      case verifyVote committee fakeVote of
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

-- * WFALS helpers

castVote ::
  PrivateKey TestCrypto ->
  ElectionId TestCrypto ->
  VoteCandidate TestCrypto ->
  EligibilityWitness TestCrypto WFALS ->
  Vote TestCrypto WFALS
castVote privateKey electionId candidate witness =
  case witness of
    WFALSPersistentMember seatIndex _ ->
      WFALSPersistentVote
        seatIndex
        electionId
        candidate
        sig
    WFALSNonPersistentMember seatIndex _ vrfOutput _ ->
      WFALSNonPersistentVote
        seatIndex
        electionId
        candidate
        vrfOutput
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

genTargetCommitteeSize ::
  Map PoolId (PrivateKey TestCrypto, PublicKey TestCrypto, LedgerStake) ->
  Gen TargetCommitteeSize
genTargetCommitteeSize pools = do
  let hasPositiveStake (_, _, LedgerStake stake) =
        stake > 0
  let poolsWithPositiveStake =
        Map.filter hasPositiveStake pools
  numPools <-
    choose (1, Map.size poolsWithPositiveStake)
  pure $ TargetCommitteeSize (fromIntegral numPools)

data FakeEligibilityWitnessType
  = NotAPersistentMember
  | NotANonPersistentMember
  | NonPersistentMemberWithZeroSeats
  deriving Show

-- | Generate a fake eligibility witness that would fail verification if used
-- to cast a vote.
--
-- This breaks the structure of a valid witnesses in three ways:
--  1. generating a persistent member witness with an index outside of the
--     persistent members range,
--  2. generating a non-persistent member witness with an index outside of the
--     non-persistent members range, and
--  3. generating a non-persistent member witness with a VRF output that leads
--     to zero non-persistent seats.
genFakeEligibilityWitness ::
  VotingCommittee TestCrypto WFALS ->
  PrivateKey TestCrypto ->
  LedgerStake ->
  Nonce ->
  ElectionId TestCrypto ->
  Gen (FakeEligibilityWitnessType, EligibilityWitness TestCrypto WFALS)
genFakeEligibilityWitness
  committee
  poolPrivateKey
  poolStake
  epochNonce
  electionId = do
    frequency
      [ (1, genFakePersistentMemberWitness)
      , (1, genFakeNonPersistentMemberWitness)
      ]
   where
    numPersistentSeats =
      unPersistentCommitteeSize (persistentCommitteeSize committee)

    numNonPersistentSeats =
      unNonPersistentCommitteeSize (nonPersistentCommitteeSize committee)

    persistentRange =
      (0, numPersistentSeats - 1)

    nonPersistentRange =
      (numPersistentSeats, numPersistentSeats + numNonPersistentSeats - 1)

    -- This witness looks like a persistent member, but its seat index lies
    -- outside the persistent members range (case 1).
    genFakePersistentMemberWitness = do
      seatIndex <- SeatIndex <$> choose nonPersistentRange
      pure
        ( NotAPersistentMember
        , WFALSPersistentMember
            seatIndex
            poolStake
        )

    -- This witness looks like a non-persistent member, but depending on whether
    -- the VRF output leads to a positive number of seats or not, it breaks
    -- the structure of a valid witness either by having an index outside of the
    -- non-persistent members range (case 2) or by failing to claim a non-zero
    -- number of non-persistent seats (case 3).
    genFakeNonPersistentMemberWitness = do
      let vrfOutput =
            evalVRF @TestCrypto
              (VRFSignContext (getVRFSigningKey (Proxy @TestCrypto) poolPrivateKey))
              (mkVRFElectionInput epochNonce electionId)
              `onError` \err ->
                error ("evalVRF failed: " <> show err)
      let numSeats =
            LS.localSortitionNumSeats
              (nonPersistentCommitteeSize committee)
              (totalNonPersistentStake committee)
              poolStake
              (normalizeVRFOutput vrfOutput)

      case nonZero numSeats of
        Just nonZeroNumSeats -> do
          seatIndex <- SeatIndex <$> choose persistentRange
          pure
            ( NotANonPersistentMember
            , WFALSNonPersistentMember
                seatIndex
                poolStake
                vrfOutput
                nonZeroNumSeats
            )
        Nothing -> do
          seatIndex <- SeatIndex <$> choose nonPersistentRange
          pure
            ( NonPersistentMemberWithZeroSeats
            , WFALSNonPersistentMember
                seatIndex
                poolStake
                vrfOutput
                (unsafeNonZero numSeats)
            )

-- * Property helpers

showWitness ::
  EligibilityWitness TestCrypto WFALS ->
  String
showWitness witness =
  case witness of
    WFALSPersistentMember seatIndex _ ->
      "PersistentMember(SeatIndex=" <> show seatIndex <> ")"
    WFALSNonPersistentMember seatIndex _ vrfOutput numSeats ->
      "NonPersistentMember(SeatIndex="
        <> show seatIndex
        <> ", VRFOutput="
        <> show vrfOutput
        <> ", NumSeats="
        <> show (unLocalSortitionNumSeats (unNonZero numSeats))
        <> ")"

cmpWitness ::
  EligibilityWitness TestCrypto WFALS ->
  EligibilityWitness TestCrypto WFALS ->
  Bool
cmpWitness w1 w2 =
  case (w1, w2) of
    ( WFALSPersistentMember seatIndex1 _
      , WFALSPersistentMember seatIndex2 _
      ) ->
        seatIndex1 == seatIndex2
    ( WFALSNonPersistentMember seatIndex1 _ vrfOutput1 numSeats1
      , WFALSNonPersistentMember seatIndex2 _ vrfOutput2 numSeats2
      ) ->
        seatIndex1 == seatIndex2
          && vrfOutput1 == vrfOutput2
          && numSeats1 == numSeats2
    _ ->
      False

-- * Tabulation helpers

tabulateTargetCommitteeSize ::
  TargetCommitteeSize ->
  Property ->
  Property
tabulateTargetCommitteeSize (TargetCommitteeSize size) =
  tabulate
    "Target committee size"
    [mkBucket 100 (fromIntegral size)]

tabulateShouldVote ::
  Maybe (EligibilityWitness TestCrypto WFALS) ->
  Property ->
  Property
tabulateShouldVote shouldVote =
  tabulate
    "Should vote"
    [ case shouldVote of
        Nothing ->
          "NoVote"
        Just (WFALSPersistentMember _ _) ->
          "PersistentVote"
        Just (WFALSNonPersistentMember _ _ _ numSeats) ->
          "NonPersistentVote(NumSeats="
            <> show (unLocalSortitionNumSeats (unNonZero numSeats))
            <> ")"
    ]

tabulateFakePersistentWitnessType ::
  FakeEligibilityWitnessType ->
  Property ->
  Property
tabulateFakePersistentWitnessType fakeWitnessType =
  tabulate
    "Fake witness type"
    [show fakeWitnessType]
