{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Test properties for the weighted Fait-Accompli implementation using TestCrypto.
module Test.Consensus.Committee.WFALS.Tests (tests) where

import Cardano.Ledger.BaseTypes hiding ((==>))
import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import Data.List (intercalate, partition, sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Ouroboros.Consensus.Committee.Class
  ( CryptoSupportsVotingCommittee (..)
  , VotingCommittee
  , ensureUniqueVotesWithSameTarget
  )
import Ouroboros.Consensus.Committee.Crypto
  ( CryptoSupportsVRF (..)
  , ElectionId
  , PrivateKey
  , PublicKey
  , VRFPoolContext (..)
  , evalVRF
  , mkVRFElectionInput
  )
import Ouroboros.Consensus.Committee.LS (LocalSortitionNumSeats (..))
import qualified Ouroboros.Consensus.Committee.LS as LS
import Ouroboros.Consensus.Committee.Types
  ( LedgerStake (..)
  , PoolId
  , TargetCommitteeSize (..)
  , VoteWeight (..)
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
  , once
  , tabulate
  , (.&&.)
  , (===)
  , (==>)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Committee
  ( eqWithShowCmp
  , genEpochNonce
  , genPools
  , mkBucket
  , mkPoolId
  , onError
  , tabulateNumPools
  , tabulatePoolStake
  , unfairWFATiebreaker
  )
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
    , testProperty
        "prop_eligiblePartyVoteWeight_example"
        (once prop_eligiblePartyVoteWeight_example)
    , testProperty
        "prop_nonPersistentVoteWeightProportionalToNumSeats"
        prop_nonPersistentVoteWeightProportionalToNumSeats
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
                          forgeVote
                            witness
                            poolPrivateKey
                            electionId
                            candidate
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
                        error ("mkVotingCommittee failed: " <> show err)
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
                        forgeVote
                          fakeWitness
                          poolPrivateKey
                          electionId
                          candidate
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

-- | If we forge a certificate from many votes with the same target, verifying
-- it should succeed and return the eligibility witnesses.
prop_forgeCert_verifyCert :: Property
prop_forgeCert_verifyCert =
  forAll (genPools 1000 TestCrypto.genKeyPair) $ \pools ->
    forAll (genTargetCommitteeSize pools) $ \targetCommitteeSize ->
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
            -- Forge votes from all eligible pools
            let (votes, originalWitnesses) =
                  unzip $
                    [ ( forgeVote witness privateKey electionId candidate
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
              . tabulateTargetCommitteeSize targetCommitteeSize
              $ case votes of
                [] ->
                  -- No eligible voters, nothing to test
                  property True
                firstVote : nextVotes -> do
                  let uniqueVotesWithSameTarget =
                        ensureUniqueVotesWithSameTarget
                          ( \case
                              WFALSPersistentVote _ eid cand _ -> (eid, cand)
                              WFALSNonPersistentVote _ eid cand _ _ -> (eid, cand)
                          )
                          ( compare `on` \case
                              WFALSPersistentVote seatIndex _ _ _ -> seatIndex
                              WFALSNonPersistentVote seatIndex _ _ _ _ -> seatIndex
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

-- | Regression test for the vote weight of committee members
--
--  * Total stake: 1000
--  * Persistent voter A has 600.
--  * Non-persistent candidates have 400 in total, among them B has 50 and J
--    has 20.
--  * The target is 4 non-persistent seats, so each seat is worth 400 / 4 = 100.
--  * The lottery gives B 3 seats, J 1 seat, and no seat to anyone else.
--
-- The vote weights should then be:
--
--  * A (persistent): 600 / 1000 = 0.6
--  * B (3 seats):    3 * 100 / 1000 = 0.3
--  * J (1 seat):     1 * 100 / 1000 = 0.1
--
-- adding up to exactly 1, since the lottery granted (in that case) exactly
-- the targeted number of non-persistent seats.
--
-- The committee membership and seats are not chosen by hand: they result from
-- the stake distribution and from the VRF outputs of the voters. The keys,
-- epoch nonce and election ID used below were found by searching for an
-- election producing the lottery outcome above.
--
-- If this outcome changes (e.g. after a change in the crypto internals or in
-- the local sortition), a new suitable election ID can be found by running the
-- following script in @cabal repl ouroboros-consensus:test:consensus-test@
-- (keep the stakes, key seeds, epoch nonce and target committee size in sync
-- with the test):
--
-- > :set -XTypeApplications -XDataKinds
-- > import Data.Word (Word64)
-- > import qualified Data.Map.Strict as Map
-- > import Cardano.Ledger.BaseTypes (mkNonceFromNumber, unNonZero)
-- > import Ouroboros.Consensus.Committee.Class
-- > import Ouroboros.Consensus.Committee.LS
-- > import Ouroboros.Consensus.Committee.Types
-- > import Ouroboros.Consensus.Committee.WFA
-- > import Ouroboros.Consensus.Committee.WFALS
-- > import Test.Util.Committee
-- > import qualified Test.Consensus.Committee.TestCrypto as TestCrypto
-- > import Test.Consensus.Committee.TestCrypto (TestCrypto)
-- > :{
-- > let stakes = [("A", 600), ("B", 50), ("C", 50), ("D", 50), ("E", 50), ("F", 50), ("G", 50), ("H", 50), ("I", 30), ("J", 20)] :: [(String, Rational)]
-- >     pools = Map.fromList [ (mkPoolId name, (sk, pk, LedgerStake stake)) | (seed, (name, stake)) <- zip [0 ..] stakes, let (sk, pk) = TestCrypto.mkDeterministicKeyPair seed ]
-- >     Right stakeDistr = mkExtWFAStakeDistr unfairWFATiebreaker (Map.map (\(_, pk, stake) -> (stake, pk)) pools)
-- >     Right committee = mkVotingCommittee @TestCrypto @WFALS (WFALSVotingCommitteeInput (mkNonceFromNumber 0) (TargetCommitteeSize 5) stakeDistr)
-- >     membership electionId name =
-- >       let (sk, _, _) = pools Map.! mkPoolId name
-- >        in case checkShouldVote committee (mkPoolId name) sk electionId of
-- >             Right Nothing -> "none"
-- >             Right (Just WFALSPersistentMember{}) -> "persistent"
-- >             Right (Just (WFALSNonPersistentMember _ _ _ numSeats)) -> show (unLocalSortitionNumSeats (unNonZero numSeats))
-- >             Left err -> error (show err)
-- >     expected = [("A", "persistent"), ("B", "3"), ("J", "1")] <> [ (name, "none") | name <- ["C", "D", "E", "F", "G", "H", "I"] ]
-- > :}
-- > take 1 [ electionId | electionId <- [0 ..] :: [Word64], all (\(name, m) -> membership electionId name == m) expected ]
--
-- NOTE: the non-persistent stake is purposely spread over many candidates.
-- If a single candidate held a large share of it (e.g. 300 out of 400), the
-- weighted Fait-Accompli scheme would grant it a persistent seat instead.
prop_eligiblePartyVoteWeight_example :: Property
prop_eligiblePartyVoteWeight_example = do
  let pools =
        Map.fromList
          [ (mkPoolId name, (privateKey, publicKey, LedgerStake stake))
          | (seed, (name, stake)) <- zip [0 ..] stakes
          , let (privateKey, publicKey) = TestCrypto.mkDeterministicKeyPair seed
          ]
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
              (TargetCommitteeSize 5)
              extWFAStakeDistr
          )
          `onError` \err ->
            error ("mkVotingCommittee failed: " <> show err)
  let witnesses =
        Map.fromList
          [ (name, witness)
          | (name, _) <- stakes
          , let (privateKey, _, _) = pools Map.! mkPoolId name
          , Just witness <-
              [ checkShouldVote
                  committee
                  (mkPoolId name)
                  privateKey
                  electionId
                  `onError` \err ->
                    error ("checkShouldVote failed: " <> show err)
              ]
          ]
  let weights =
        Map.map (eligiblePartyVoteWeight committee) witnesses
  (persistentCommitteeSize committee === PersistentCommitteeSize 1)
    .&&. (nonPersistentCommitteeSize committee === NonPersistentCommitteeSize 4)
    .&&. ( Map.map showMembership witnesses
             === Map.fromList
               [ ("A", "persistent")
               , ("B", "non-persistent with 3 seat(s)")
               , ("J", "non-persistent with 1 seat(s)")
               ]
         )
    .&&. ( weights
             === Map.fromList
               [ ("A", VoteWeight (6 / 10))
               , ("B", VoteWeight (3 / 10))
               , ("J", VoteWeight (1 / 10))
               ]
         )
    .&&. (sum (Map.elems weights) === VoteWeight 1)
 where
  epochNonce = mkNonceFromNumber 0
  electionId = 4669

  stakes =
    [ ("A", 600)
    , ("B", 50)
    , ("C", 50)
    , ("D", 50)
    , ("E", 50)
    , ("F", 50)
    , ("G", 50)
    , ("H", 50)
    , ("I", 30)
    , ("J", 20)
    ]

  showMembership :: EligibilityWitness TestCrypto WFALS -> String
  showMembership = \case
    WFALSPersistentMember{} ->
      "persistent"
    WFALSNonPersistentMember _ _ _ numSeats ->
      "non-persistent with "
        <> show (unLocalSortitionNumSeats (unNonZero numSeats))
        <> " seat(s)"

-- | The vote weight of a non-persistent voter is proportional to the number of
-- non-persistent seats it obtained, regardless of its stake: all non-persistent
-- voters of a given election get the same vote weight per seat.
--
-- The only exception are /outstanding/ voters, who obtained so many seats that
-- their vote weight would exceed 1. Their vote weight must be clamped to
-- exactly 1 instead.
--
-- For the proportionality check to be meaningful, we require at least two
-- regular (i.e. non-outstanding) non-persistent voters.
prop_nonPersistentVoteWeightProportionalToNumSeats :: Property
prop_nonPersistentVoteWeightProportionalToNumSeats =
  forAll (genPools 1000 TestCrypto.genKeyPair) $ \pools ->
    forAll (genTargetCommitteeSize pools) $ \targetCommitteeSize ->
      forAll genEpochNonce $ \epochNonce ->
        forAll TestCrypto.genElectionId $ \electionId -> do
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
          -- Stake, number of seats and vote weight of the non-persistent voters
          -- elected by local sortition
          let nonPersistentVoters =
                [ ( stake
                  , unLocalSortitionNumSeats (unNonZero numSeats)
                  , eligiblePartyVoteWeight committee witness
                  )
                | (poolId, (privateKey, _, _)) <- Map.toList pools
                , Just witness@(WFALSNonPersistentMember _ stake _ numSeats) <-
                    [ checkShouldVote
                        committee
                        poolId
                        privateKey
                        electionId
                        `onError` \err ->
                          error ("checkShouldVote failed: " <> show err)
                    ]
                ]
          let (outstandingVoters, regularVoters) =
                partition (\(_, _, weight) -> weight >= 1) nonPersistentVoters
          let numDistinct f =
                length . nubOrd . fmap f
          let weightPerSeat (_, numSeats, VoteWeight weight) =
                weight / fromIntegral numSeats
          let showVoters voters =
                show
                  [ (stake, numSeats, weight, weightPerSeat voter)
                  | voter@(stake, numSeats, weight) <- voters
                  ]
          tabulateNumPools pools
            . tabulateTargetCommitteeSize targetCommitteeSize
            . tabulate
              "Distinct numbers of seats among non-persistent voters"
              [show (numDistinct (\(_, numSeats, _) -> numSeats) nonPersistentVoters)]
            . tabulate
              "Non-persistent voters with different stakes"
              [show (numDistinct (\(stake, _, _) -> stake) nonPersistentVoters > 1)]
            . tabulate
              "Outstanding non-persistent voters"
              [show (not (null outstandingVoters))]
            -- Outstanding voters are extremely rare, so we report everything
            -- needed to reproduce such a case whenever we encounter one.
            . tabulate
              "Parameters reproducing outstanding non-persistent voters"
              [ unlines
                  [ "epoch nonce: " <> show epochNonce
                  , "election ID: " <> show electionId
                  , "target committee size: " <> show targetCommitteeSize
                  , "outstanding voters [(stake, seats, weight, weight per seat)]: "
                      <> showVoters outstandingVoters
                  , "pools [(pool ID, (private key, public key, stake))]: "
                      <> show (Map.toList pools)
                  ]
              | not (null outstandingVoters)
              ]
            $ (length regularVoters >= 2)
            ==> counterexample
              ( "outstanding voters with a vote weight different from 1 "
                  <> "[(stake, seats, weight, weight per seat)]: "
                  <> showVoters outstandingVoters
              )
              (all (\(_, _, weight) -> weight == 1) outstandingVoters)
              .&&. counterexample
                ( "regular voters with different vote weights per seat "
                    <> "[(stake, seats, weight, weight per seat)]: "
                    <> showVoters regularVoters
                )
                (numDistinct weightPerSeat regularVoters <= 1)

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
      [
        ( if numNonPersistentSeats > 0 then 1 else 0
        , genFakePersistentMemberWitness
        )
      ,
        ( if numPersistentSeats > 0 then 1 else 0
        , genFakeNonPersistentMemberWitness
        )
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

cmpWitnesses ::
  [EligibilityWitness TestCrypto WFALS] ->
  [EligibilityWitness TestCrypto WFALS] ->
  Bool
cmpWitnesses ws1 ws2 =
  length ws1 == length ws2
    && all
      (uncurry cmpWitness)
      (zip (sortBySeatIndex ws1) (sortBySeatIndex ws2))
 where
  sortBySeatIndex =
    sortOn $ \case
      WFALSPersistentMember seatIndex _ -> seatIndex
      WFALSNonPersistentMember seatIndex _ _ _ -> seatIndex

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

tabulateNumVotes ::
  Int ->
  Property ->
  Property
tabulateNumVotes numVotes =
  tabulate
    "Number of votes"
    [mkBucket 100 (fromIntegral numVotes)]
