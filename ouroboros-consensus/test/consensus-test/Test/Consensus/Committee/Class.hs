{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Test properties for the generic voting committee class helpers
module Test.Consensus.Committee.Class (tests) where

import Data.Containers.NonEmpty (HasNonEmpty (..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Ouroboros.Consensus.Committee.Class
  ( CryptoSupportsVotingCommittee (..)
  , VotesWithSameTarget
  , VotesWithSameTargetError (..)
  , ensureSameTarget
  , getRawVotes
  )
import Ouroboros.Consensus.Committee.Crypto
  ( ElectionId
  , VoteCandidate
  )
import Ouroboros.Consensus.Committee.Types (VoteWeight (..))
import Test.Consensus.Committee.TestCrypto (TestCrypto)
import qualified Test.Consensus.Committee.TestCrypto as TestCrypto
import Test.Consensus.Committee.Utils (mkBucket)
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Positive (..)
  , Property
  , Small (..)
  , Testable (..)
  , counterexample
  , elements
  , forAll
  , frequency
  , suchThat
  , tabulate
  , vectorOf
  , (.&&.)
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup
    "ensureSameTarget properties"
    [ adjustQuickCheckTests (* 10) $
        testProperty
          "prop_ensureSameTarget"
          prop_ensureSameTarget
    ]

-- * Mock committee implementation for testing

data MockCommittee

instance CryptoSupportsVotingCommittee TestCrypto MockCommittee where
  data VotingCommittee TestCrypto MockCommittee = MockVotingCommittee
  data VotingCommitteeInput TestCrypto MockCommittee = MockVotingCommitteeInput
  data VotingCommitteeError TestCrypto MockCommittee = MockError
  data EligibilityWitness TestCrypto MockCommittee = MockWitness
  data Vote TestCrypto MockCommittee = MockVote (ElectionId TestCrypto) (VoteCandidate TestCrypto)
  data Cert TestCrypto MockCommittee = MockCert

  mkVotingCommittee _ = Right MockVotingCommittee
  checkShouldVote _ _ _ _ = Right (Just MockWitness)
  forgeVote _ _ electionId candidate = MockVote electionId candidate
  verifyVote _ _ = Right MockWitness
  eligiblePartyVoteWeight _ _ = VoteWeight 1
  forgeCert _ = Right MockCert
  verifyCert _ _ = Right (MockWitness :| [])

deriving instance Show (Vote TestCrypto MockCommittee)
deriving instance Eq (Vote TestCrypto MockCommittee)

-- * Test property

prop_ensureSameTarget :: Property
prop_ensureSameTarget =
  forAll genVoteList $ \voteList -> do
    let result = ensureSameTarget @TestCrypto @MockCommittee getVoteTarget voteList
    tabulate "Outcome" [showOutcome result]
      . tabulate "List length" [mkBucket 10 (fromIntegral (length voteList))]
      $ case (voteList, result) of
        ([], Left EmptyVotes) ->
          property True
        (v : vs, Left (TargetMismatch matchingVotes mismatchingVotes)) ->
          (allSameTarget (v :| vs) === Nothing)
            .&&. (all (\v' -> getVoteTarget v' == getVoteTarget v) matchingVotes)
            .&&. (all (\v' -> getVoteTarget v' /= getVoteTarget v) mismatchingVotes)
        (v : vs, Right votesWithSameTarget) ->
          (allSameTarget (v :| vs) === Just (getVoteTarget v))
            .&&. (all (\v' -> getVoteTarget v' == getVoteTarget v) (getRawVotes votesWithSameTarget))
        _ ->
          counterexample
            ( unlines
                [ "Unexpected outcome:"
                , "Vote list: " <> show voteList
                , "Result: " <> showOutcome result
                ]
            )
            $ property False

-- * Generators

-- | Generate a vote list (empty, all same target, or mixed targets)
genVoteList :: Gen [Vote TestCrypto MockCommittee]
genVoteList =
  frequency
    [ (1, pure [])
    , (5, genSameTarget =<< genTarget)
    , (4, genMixedTarget)
    ]
 where
  genNumVotes = do
    getSmall . getPositive
      <$> arbitrary @(Positive (Small Int))

  genTarget = do
    (,)
      <$> TestCrypto.genElectionId
      <*> TestCrypto.genVoteCandidate

  genSameTarget (electionId, candidate) = do
    numVotes <- genNumVotes
    vectorOf numVotes (pure (MockVote electionId candidate))

  genMixedTarget = do
    target1 <- genTarget
    target2 <- genTarget `suchThat` (/= target1)
    votes1 <- genSameTarget target1
    votes2 <- genSameTarget target2
    interleaveVotes votes1 votes2

  interleaveVotes [] ys = pure ys
  interleaveVotes xs [] = pure xs
  interleaveVotes (x : xs) (y : ys) = do
    useFirst <- elements [True, False]
    rest <- interleaveVotes xs ys
    pure (if useFirst then x : rest else y : rest)

-- * Helpers

-- | Get the target from a vote
getVoteTarget ::
  Vote TestCrypto MockCommittee ->
  (ElectionId TestCrypto, VoteCandidate TestCrypto)
getVoteTarget (MockVote electionId candidate) =
  (electionId, candidate)

-- | Check if all votes in a non-empty list have the same target
allSameTarget ::
  NE [Vote TestCrypto MockCommittee] ->
  Maybe (ElectionId TestCrypto, VoteCandidate TestCrypto)
allSameTarget (v :| vs)
  | all (\v' -> getVoteTarget v == getVoteTarget v') vs =
      Just (getVoteTarget v)
  | otherwise =
      Nothing

-- | Show the outcome of ensureSameTarget
showOutcome ::
  Either
    (VotesWithSameTargetError TestCrypto MockCommittee)
    (VotesWithSameTarget TestCrypto MockCommittee) ->
  String
showOutcome (Left EmptyVotes) = "EmptyVotes"
showOutcome (Left (TargetMismatch _ _)) = "TargetMismatch"
showOutcome (Right _) = "Success"
