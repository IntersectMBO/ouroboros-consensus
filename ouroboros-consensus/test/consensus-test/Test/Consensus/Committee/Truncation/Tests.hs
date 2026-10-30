{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Consensus.Committee.Truncation.Tests (tests) where

import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Ouroboros.Consensus.Committee.Class
  ( CryptoSupportsVotingCommittee (..)
  , ensureUniqueVotesWithSameTarget
  )
import Ouroboros.Consensus.Committee.Truncation
  ( EligibilityWitness (..)
  , Truncation
  , Vote (..)
  , VotingCommitteeError (..)
  , VotingCommitteeInput (..)
  , candidateSeats
  )
import Ouroboros.Consensus.Committee.Types
  ( LedgerStake (..)
  , TargetCommitteeSize (..)
  , VoteWeight (..)
  )
import Ouroboros.Consensus.Committee.WFA (SeatIndex (..), WFAError (..))
import Test.Consensus.Committee.TestCrypto (TestCrypto)
import qualified Test.Consensus.Committee.TestCrypto as TestCrypto
import Test.QuickCheck
  ( Property
  , choose
  , counterexample
  , forAll
  , property
  , (.&&.)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Committee (genPools, mkPoolId, onError)

tests :: TestTree
tests =
  testGroup
    "Truncation committee selection"
    [ testProperty "selects the top pools by stake and PoolId" prop_selectsTopPools
    , testProperty "rejects a zero committee size" prop_rejectsZeroSize
    , testProperty "empty input takes precedence over zero committee size" prop_emptyInput
    , testProperty "zero-stake pools are ineligible within the limit" prop_zeroStakeMember
    , testProperty "negative-stake pools are ineligible within the limit" prop_negativeStakeMember
    , testProperty
        "positive-stake pools outside the limit are ineligible and retained"
        prop_rejectsOutsideTopN
    ]

prop_selectsTopPools :: Property
prop_selectsTopPools =
  forAll (genPools 40 TestCrypto.genKeyPair) $ \pools ->
    forAll (choose (1, Map.size pools)) $ \size ->
      let stakeDistr = Map.map (\(_, publicKey, stake) -> (stake, publicKey)) pools
          committee =
            mkVotingCommittee @TestCrypto @Truncation
              (TruncationVotingCommitteeInput (TargetCommitteeSize (fromIntegral size)) stakeDistr)
              `onError` (error . show)
          expected =
            Map.fromList
              [ (poolId, SeatIndex (fromIntegral seat))
              | (seat, (poolId, _)) <-
                  zip [0 :: Int ..] (sortOn rank (Map.toList pools))
              ]
          rank (poolId, (_, _, LedgerStake stake)) = (negate stake, poolId)
       in counterexample (show (candidateSeats committee, expected)) $
            property (candidateSeats committee == expected)

prop_rejectsZeroSize :: Property
prop_rejectsZeroSize =
  forAll (genPools 5 TestCrypto.genKeyPair) $ \pools ->
    let stakeDistr = Map.map (\(_, publicKey, stake) -> (stake, publicKey)) pools
     in case mkVotingCommittee @TestCrypto @Truncation
          (TruncationVotingCommitteeInput (TargetCommitteeSize 0) stakeDistr) of
          Left ZeroCommitteeSize -> property True
          result -> counterexample (showResult result) False
 where
  showResult = either show (const "unexpected success")

prop_emptyInput :: Property
prop_emptyInput =
  case mkVotingCommittee @TestCrypto @Truncation
    (TruncationVotingCommitteeInput (TargetCommitteeSize 0) Map.empty) of
    Left (WFAError EmptyStakeDistribution) -> property True
    result -> counterexample (showResult result) False
 where
  showResult = either show (const "unexpected success")

prop_zeroStakeMember :: Property
prop_zeroStakeMember =
  forAll (genPools 1 TestCrypto.genKeyPair) $ \pools ->
    let (_, (privateKey, publicKey, _)) = Map.findMin pools
        positivePoolId = mkPoolId "truncation-positive-pool"
        zeroStakePoolId = mkPoolId "truncation-zero-stake-pool"
        stakeDistr =
          Map.fromList
            [ (positivePoolId, (LedgerStake 7, publicKey))
            , (zeroStakePoolId, (LedgerStake 0, publicKey))
            ]
        committee =
          mkVotingCommittee @TestCrypto @Truncation
            (TruncationVotingCommitteeInput (TargetCommitteeSize 2) stakeDistr)
            `onError` (error . show)
        zeroSeat = candidateSeats committee Map.! zeroStakePoolId
     in counterexample
          "zero-stake candidate was not retained at its ranked seat"
          (property (zeroSeat == SeatIndex 1))
          .&&. forAll
            TestCrypto.genElectionId
            ( \electionId ->
                forAll
                  TestCrypto.genVoteCandidate
                  ( \candidate ->
                      case checkShouldVote committee zeroStakePoolId privateKey electionId of
                        Right Nothing ->
                          let vote =
                                forgeVote
                                  (TruncationMember zeroSeat (LedgerStake 0))
                                  privateKey
                                  electionId
                                  candidate
                           in case verifyVote committee vote of
                                Left (PoolHasNoStake _) ->
                                  case ensureUniqueVotesWithSameTarget
                                    (\case TruncationVote _ eid cand _ -> (eid, cand))
                                    (\vote1 vote2 -> compare (voteSeat vote1) (voteSeat vote2))
                                    (vote :| []) of
                                    Left _ -> counterexample "could not construct zero-stake certificate" False
                                    Right votes ->
                                      case forgeCert votes of
                                        Left err -> counterexample (show err) False
                                        Right cert ->
                                          case verifyCert committee cert of
                                            Left (PoolHasNoStake _) -> property True
                                            Left err -> counterexample (show err) False
                                            Right _ ->
                                              counterexample "zero-stake certificate unexpectedly verified" False
                                Left err -> counterexample (show err) False
                                Right _ -> counterexample "zero-stake vote unexpectedly verified" False
                        Left err -> counterexample (show err) False
                        Right (Just _) -> counterexample "zero-stake pool unexpectedly eligible" False
                  )
            )
 where
  voteSeat (TruncationVote seatIndex _ _ _) = seatIndex

prop_negativeStakeMember :: Property
prop_negativeStakeMember =
  forAll TestCrypto.genKeyPair $ \(privateKey, publicKey) ->
    let positivePoolId = mkPoolId "truncation-positive-pool"
        negativePoolId = mkPoolId "truncation-negative-stake-pool"
        negativeStake = LedgerStake (-7)
        positiveStake = LedgerStake 7
        -- Ledger stake distributions are normally nonnegative; exercise the
        -- shared constructor's behavior for an invalid negative stake anyway.
        stakeDistr =
          Map.fromList
            [ (positivePoolId, (positiveStake, publicKey))
            , (negativePoolId, (negativeStake, publicKey))
            ]
        committee =
          mkVotingCommittee @TestCrypto @Truncation
            (TruncationVotingCommitteeInput (TargetCommitteeSize 2) stakeDistr)
            `onError` (error . show)
        negativeSeat = candidateSeats committee Map.! negativePoolId
        positiveSeat = candidateSeats committee Map.! positivePoolId
     in counterexample
          "negative-stake candidate was not retained at its ranked seat"
          (property (negativeSeat == SeatIndex 1))
          .&&. property (positiveSeat == SeatIndex 0)
          .&&. property
            ( eligiblePartyVoteWeight committee (TruncationMember positiveSeat positiveStake)
                == VoteWeight 1
            )
          .&&. property
            ( eligiblePartyVoteWeight committee (TruncationMember negativeSeat negativeStake)
                == VoteWeight 0
            )
          .&&. ( forAll TestCrypto.genElectionId $ \electionId ->
                   forAll TestCrypto.genVoteCandidate $ \candidate ->
                     case checkShouldVote committee negativePoolId privateKey electionId of
                       Right Nothing ->
                         let forgedVote =
                               forgeVote
                                 (TruncationMember negativeSeat negativeStake)
                                 privateKey
                                 electionId
                                 candidate
                             forgedVotes =
                               ensureUniqueVotesWithSameTarget
                                 (\case TruncationVote _ eid cand _ -> (eid, cand))
                                 (\vote1 vote2 -> compare (voteSeat vote1) (voteSeat vote2))
                                 (forgedVote :| [])
                          in case (verifyVote committee forgedVote, forgedVotes) of
                               (Left (PoolHasNoStake _), Right uniqueVotes) ->
                                 case forgeCert uniqueVotes of
                                   Left err -> counterexample (show err) False
                                   Right forgedCert ->
                                     case verifyCert committee forgedCert of
                                       Left (PoolHasNoStake _) -> property True
                                       Left err -> counterexample (show err) False
                                       Right _ ->
                                         counterexample "negative-stake certificate unexpectedly verified" False
                               (Left err, _) -> counterexample (show err) False
                               (_, Left _) -> counterexample "could not construct negative-stake certificate" False
                               (Right _, Right _) -> counterexample "negative-stake vote unexpectedly verified" False
                       Left err -> counterexample (show err) False
                       Right (Just _) -> counterexample "negative-stake pool unexpectedly eligible" False
               )
 where
  voteSeat (TruncationVote seatIndex _ _ _) = seatIndex

prop_rejectsOutsideTopN :: Property
prop_rejectsOutsideTopN =
  forAll TestCrypto.genKeyPair $ \(privateKey, publicKey) ->
    let topPoolId = mkPoolId "truncation-top-pool"
        outsidePoolId = mkPoolId "truncation-outside-pool"
        outsideStake = LedgerStake 5
        stakeDistr =
          Map.fromList
            [ (topPoolId, (LedgerStake 10, publicKey))
            , (outsidePoolId, (outsideStake, publicKey))
            ]
        committee =
          mkVotingCommittee @TestCrypto @Truncation
            (TruncationVotingCommitteeInput (TargetCommitteeSize 1) stakeDistr)
            `onError` (error . show)
        outsideSeat = candidateSeats committee Map.! outsidePoolId
        expectedSeats =
          Map.fromList
            [ (topPoolId, SeatIndex 0)
            , (outsidePoolId, SeatIndex 1)
            ]
     in counterexample
          "committee representation did not retain every ranked pool"
          (property (candidateSeats committee == expectedSeats))
          .&&. forAll
            TestCrypto.genElectionId
            ( \electionId ->
                forAll TestCrypto.genVoteCandidate $ \candidate ->
                  case checkShouldVote committee outsidePoolId privateKey electionId of
                    Right Nothing ->
                      let forgedVote =
                            forgeVote
                              (TruncationMember outsideSeat outsideStake)
                              privateKey
                              electionId
                              candidate
                          forgedVotes =
                            ensureUniqueVotesWithSameTarget
                              (\case TruncationVote _ eid cand _ -> (eid, cand))
                              (\vote1 vote2 -> compare (voteSeat vote1) (voteSeat vote2))
                              (forgedVote :| [])
                       in case (verifyVote committee forgedVote, forgedVotes) of
                            (Left (NotACommitteeMember _), Right uniqueVotes) ->
                              case forgeCert uniqueVotes of
                                Left err -> counterexample (show err) False
                                Right forgedCert ->
                                  case verifyCert committee forgedCert of
                                    Left (NotACommitteeMember _) -> property True
                                    Left err -> counterexample (show err) False
                                    Right _ ->
                                      counterexample
                                        "outside-top-N certificate unexpectedly verified"
                                        False
                            (Left err, _) -> counterexample (show err) False
                            (_, Left _) ->
                              counterexample
                                "could not construct outside-top-N certificate"
                                False
                            (Right _, Right _) ->
                              counterexample "outside-top-N vote unexpectedly verified" False
                    Left err -> counterexample (show err) False
                    Right (Just _) ->
                      counterexample "outside-top-N pool unexpectedly eligible" False
            )
 where
  voteSeat (TruncationVote seatIndex _ _ _) = seatIndex
