{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Test properties for the generic voting committee class helpers
module Test.Consensus.Committee.Class (tests) where

import Data.Containers.NonEmpty (HasNonEmpty (..))
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Proxy (Proxy (..))
import GHC.Word (Word64)
import Ouroboros.Consensus.Committee.Class
  ( UniqueVotesWithSameTargetError (..)
  , checkUniqueVotesWithSameTarget
  )
import Ouroboros.Consensus.Committee.Crypto
  ( ElectionId
  , VoteCandidate
  )
import Test.Consensus.Committee.TestCrypto (TestCrypto)
import qualified Test.Consensus.Committee.TestCrypto as TestCrypto
import Test.Consensus.Committee.Utils (mkBucket)
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , Positive (..)
  , Property
  , Small (..)
  , counterexample
  , forAll
  , tabulate
  , vectorOf
  , (.&&.)
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Testable (..), testProperty)
import Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup
    "UniqueVotesWithSameTarget properties"
    [ adjustQuickCheckTests (* 10) $
        testProperty
          "prop_checkUniqueVotesWithSameTarget"
          prop_checkUniqueVotesWithSameTarget
    ]

-- * Mock committee implementation for testing

type TestVoterId = Word64

data TestVote
  = TestVote
  { tvElectionId :: ElectionId TestCrypto
  , tvCandidate :: VoteCandidate TestCrypto
  , tvVoterId :: TestVoterId
  }
  deriving (Eq, Show)

-- * Test property

prop_checkUniqueVotesWithSameTarget :: Property
prop_checkUniqueVotesWithSameTarget =
  forAll genVotes $ \votes -> do
    let result =
          checkUniqueVotesWithSameTarget
            (Proxy @TestCrypto)
            getVoteTarget
            cmpVoteId
            votes
    tabulate "Outcome" [showOutcome result]
      . tabulate "List length" [mkBucket 10 (fromIntegral (length votes))]
      . counterexample
        ( unlines
            [ "Unexpected outcome:"
            , "Votes: " <> show votes
            , "Result: " <> showOutcome result
            ]
        )
      $ case result of
        Left (DuplicateVotes duplicateVotes) -> do
          counterexample
            ( unlines
                [ "Duplicate votes: " <> show duplicateVotes
                ]
            )
            $ (length duplicateVotes > 1)
              .&&. (property (uniqueVoterIdsCount duplicateVotes == 1))
        Left (TargetMismatch firstVote mismatchingVotes) -> do
          counterexample
            ( unlines
                [ "First vote: " <> show firstVote
                , "Mismatching votes: " <> show mismatchingVotes
                ]
            )
            $ (allSameTarget votes === Nothing)
              .&&. (all (\v' -> getVoteTarget v' /= getVoteTarget firstVote) mismatchingVotes)
        Right () -> do
          let firstVote = NonEmpty.head votes
          counterexample
            ( unlines
                [ "First votes: " <> show votes
                ]
            )
            $ (allSameTarget votes === Just (getVoteTarget firstVote))
              .&&. (uniqueVoterIdsCount votes === length votes)

-- * Generators

-- | Generate a vote list (empty, all same target, or mixed targets)
genVotes :: Gen (NE [TestVote])
genVotes = do
  numVotes <- genNumVotes
  NonEmpty.fromList <$> vectorOf numVotes genVote
 where
  genNumVotes = do
    getSmall . getPositive
      <$> arbitrary @(Positive (Small Int))

  genVote = do
    (electionId, candidate) <- genTarget
    voterId <- genVoterId
    pure (TestVote electionId candidate voterId)

  genTarget = do
    electionId <- TestCrypto.genElectionId
    candidate <- TestCrypto.genVoteCandidate
    pure (electionId, candidate)

  genVoterId = do
    getSmall <$> arbitrary @(Small TestVoterId)

-- * Helpers

-- | Get the target from a vote
getVoteTarget ::
  TestVote ->
  (ElectionId TestCrypto, VoteCandidate TestCrypto)
getVoteTarget = \case
  TestVote electionId candidate _ -> (electionId, candidate)

-- | Compare two votes by their ID
cmpVoteId ::
  TestVote ->
  TestVote ->
  Ordering
cmpVoteId =
  compare `on` \case
    TestVote _ _ voteId -> voteId

-- | Check if all votes in a non-empty list have the same target
allSameTarget ::
  NE [TestVote] ->
  Maybe (ElectionId TestCrypto, VoteCandidate TestCrypto)
allSameTarget (v :| vs)
  | all (\v' -> getVoteTarget v == getVoteTarget v') vs =
      Just (getVoteTarget v)
  | otherwise =
      Nothing

-- | Get the number of unique voter IDs in a non-empty list of votes
uniqueVoterIdsCount ::
  NE [TestVote] ->
  Int
uniqueVoterIdsCount votes =
  length (NonEmpty.nubBy ((==) `on` tvVoterId) votes)

-- | Show the outcome of ensureSameTarget
showOutcome ::
  Either (UniqueVotesWithSameTargetError TestVote) () ->
  String
showOutcome (Left (TargetMismatch _ _)) = "TargetMismatch"
showOutcome (Left (DuplicateVotes _)) = "DuplicateVotes"
showOutcome (Right ()) = "Success"
