{-# LANGUAGE NamedFieldPuns #-}

module Test.LeiosVoteState (tests) where

import Control.Concurrent.Class.MonadSTM.Strict (atomically)
import Control.Monad (forM_)
import Control.Monad.Class.MonadTimer.SI (timeout)
import Control.Monad.IOSim (runSimOrThrow)
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import LeiosDemoTypes
  ( Committee (MkCommitee)
  , EbHash (MkEbHash)
  , LeiosVote (..)
  , VoteInvalid (..)
  , VoterId (MkVoterId)
  )
import LeiosVoteState
  ( addVote
  , getNextVote
  , newLeiosVoteState
  , subscribeVotes
  )
import Test.LeiosDemoDb (genPoint)
import Test.QuickCheck
  ( Gen
  , Property
  , arbitrary
  , counterexample
  , forAll
  , listOf1
  , property
  , (.&&.)
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "LeiosVoteState"
    [ testProperty "subscriber receives added vote" prop_subscriberReceivesVote
    , testProperty "subscriber receives all distinct votes" prop_subscriberReceivesAll
    , testProperty "duplicate vote is reported as AlreadyKnown" prop_deduplicateVotes
    , testProperty "late subscriber does not see prior votes" prop_lateSubscriber
    , testProperty "invalid vote is rejected and not published" prop_invalidVoteRejected
    , testProperty "no committee rejects vote" prop_noCommitteeRejected
    ]

-- | Always-empty committee. Validation currently only inspects 'voteSignature',
-- so the committee contents don't matter for these state-machine tests.
dummyCommittee :: Committee
dummyCommittee = MkCommitee Set.empty

-- | Generate a vote that 'validateLeiosVote' currently accepts
-- (i.e. 'voteSignature' is 'True').
genVote :: Gen LeiosVote
genVote = do
  point <- genPoint
  voterId <- MkVoterId <$> arbitrary
  voteSignature <- arbitrary
  pure $ MkLeiosVote{point, voterId, voteSignature}

-- | A subscriber should receive a vote that was added after subscribing.
prop_subscriberReceivesVote :: Property
prop_subscriberReceivesVote =
  forAll genVote $ \vote -> property $ runSimOrThrow $ do
    st <- newLeiosVoteState (pure (Just dummyCommittee))
    sub <- subscribeVotes st
    _ <- addVote st vote
    received <- atomically $ getNextVote sub
    pure $ received === vote

-- | A subscriber should receive all distinct votes in order.
prop_subscriberReceivesAll :: Property
prop_subscriberReceivesAll =
  forAll (listOf1 genVote) $ \votes -> property $ runSimOrThrow $ do
    st <- newLeiosVoteState (pure (Just dummyCommittee))
    sub <- subscribeVotes st
    forM_ votes (addVote st)
    received <- mapM (\_ -> atomically $ getNextVote sub) votes
    pure $ received === votes

-- | Adding the same vote twice should only deliver it once and report
-- 'AlreadyKnown' on the second add.
prop_deduplicateVotes :: Property
prop_deduplicateVotes =
  forAll genVote $ \vote -> property $ runSimOrThrow $ do
    st <- newLeiosVoteState (pure (Just dummyCommittee))
    sub <- subscribeVotes st
    r1 <- addVote st vote
    r2 <- addVote st vote
    received <- atomically $ getNextVote sub
    -- The second read should block (no second notification).
    mSecond <- timeout 0.1 $ atomically $ getNextVote sub
    pure $
      counterexample "first add" (r1 === Added)
        .&&. counterexample "second add" (r2 === AlreadyKnown)
        .&&. counterexample "first vote" (received === vote)
        .&&. counterexample "second read should timeout" (isNothing mSecond === True)

-- | A subscriber that subscribes after a vote was added should not see it.
prop_lateSubscriber :: Property
prop_lateSubscriber =
  forAll genVote $ \vote -> property $ runSimOrThrow $ do
    st <- newLeiosVoteState (pure (Just dummyCommittee))
    _ <- addVote st vote
    sub <- subscribeVotes st
    mVote <- timeout 0.1 $ atomically $ getNextVote sub
    pure $ counterexample "late subscriber should not see prior vote" (isNothing mVote === True)

-- | A vote rejected by validation must not be published to subscribers.
-- Exhaustive coverage of validation reasons belongs in tests for
-- 'validateLeiosVote'; here we just exercise one rejection path to ensure
-- the state machine short-circuits.
prop_invalidVoteRejected :: Property
prop_invalidVoteRejected =
  forAll genVote $ \v0 -> property $ runSimOrThrow $ do
    let badVote = v0{voteSignature = False}
    st <- newLeiosVoteState (pure (Just dummyCommittee))
    sub <- subscribeVotes st
    r <- addVote st badVote
    mVote <- timeout 0.1 $ atomically $ getNextVote sub
    pure $
      r === VoteInvalid InvalidSignature
        .&&. counterexample "subscriber should not see invalid vote" (isNothing mVote === True)

-- | When no committee is selected, votes should be rejected with 'NoCommittee'
-- and not published to subscribers.
prop_noCommitteeRejected :: Property
prop_noCommitteeRejected =
  forAll genVote $ \vote -> property $ runSimOrThrow $ do
    st <- newLeiosVoteState (pure Nothing)
    sub <- subscribeVotes st
    r <- addVote st vote
    mVote <- timeout 0.1 $ atomically $ getNextVote sub
    pure $
      r === NoCommittee
        .&&. counterexample "subscriber should not see vote" (isNothing mVote === True)
