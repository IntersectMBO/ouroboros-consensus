{-# LANGUAGE NamedFieldPuns #-}

module Test.LeiosVoteState (tests) where

import Control.Concurrent.Class.MonadSTM.Strict (atomically)
import Control.Monad (forM_)
import Control.Monad.Class.MonadTimer.SI (timeout)
import Control.Monad.IOSim (runSimOrThrow)
import qualified Data.ByteString as BS
import Data.Maybe (fromJust, isNothing)
import qualified Data.Set as Set
import LeiosDemoTypes
  ( Committee (..)
  , LeiosVote (..)
  , VoteInvalid (..)
  , VoterId (MkVoterId)
  , getVoterId
  , signLeiosVote
  , voteSignature
  , voters
  )
import LeiosVoteState
  ( addVote
  , getNextVote
  , newLeiosVoteState
  , subscribeVotes
  )
import Ouroboros.Consensus.Config (VotingKey)
import Test.LeiosDemoDb (genPoint)
import Test.QuickCheck
  ( Gen
  , Property
  , chooseInt
  , counterexample
  , elements
  , forAll
  , listOf1
  , property
  , vectorOf
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
    , testProperty "vote signed with key not on committee is rejected" prop_signerNotInCommittee
    ]

genVotingKey :: Gen VotingKey
genVotingKey = BS.pack <$> vectorOf 32 (fromIntegral <$> chooseInt (0, 255))

-- | A non-empty committee.
genCommittee :: Gen Committee
genCommittee = do
  n <- chooseInt (1, 10)
  ks <- Set.fromList <$> vectorOf n genVotingKey
  pure $ MkCommittee ks

-- | A 'VotingKey' that is *not* a member of the given committee.
genKeyNotIn :: Committee -> Gen VotingKey
genKeyNotIn committee = do
  k <- genVotingKey
  if Set.member k (voters committee)
    then genKeyNotIn committee
    else pure k

-- | A vote produced by 'signLeiosVote' with an arbitrary point and a key from the
-- committee.
genVoteFor :: Committee -> Gen LeiosVote
genVoteFor committee = do
  key <- elements (Set.toList (voters committee))
  let voterId = fromJust $ getVoterId key committee
  signLeiosVote key voterId <$> genPoint

-- | A subscriber should receive a vote that was added after subscribing.
prop_subscriberReceivesVote :: Property
prop_subscriberReceivesVote =
  forAll genCommittee $ \committee ->
    forAll (genVoteFor committee) $ \vote -> property $ runSimOrThrow $ do
      st <- newLeiosVoteState (pure (Just committee))
      sub <- subscribeVotes st
      _ <- addVote st vote
      received <- atomically $ getNextVote sub
      pure $ received === vote

-- | A subscriber should receive all distinct votes in order.
prop_subscriberReceivesAll :: Property
prop_subscriberReceivesAll =
  forAll genCommittee $ \committee ->
    forAll (listOf1 (genVoteFor committee)) $ \votes -> property $ runSimOrThrow $ do
      st <- newLeiosVoteState (pure (Just committee))
      sub <- subscribeVotes st
      forM_ votes (addVote st)
      received <- mapM (\_ -> atomically $ getNextVote sub) votes
      pure $ received === votes

-- | Adding the same vote twice should only deliver it once and report
-- 'AlreadyKnown' on the second add.
prop_deduplicateVotes :: Property
prop_deduplicateVotes =
  forAll genCommittee $ \committee ->
    forAll (genVoteFor committee) $ \vote -> property $ runSimOrThrow $ do
      st <- newLeiosVoteState (pure (Just committee))
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
  forAll genCommittee $ \committee ->
    forAll (genVoteFor committee) $ \vote -> property $ runSimOrThrow $ do
      st <- newLeiosVoteState (pure (Just committee))
      _ <- addVote st vote
      sub <- subscribeVotes st
      mVote <- timeout 0.1 $ atomically $ getNextVote sub
      pure $ counterexample "late subscriber should not see prior vote" (isNothing mVote === True)

-- | A vote rejected by validation must not be published to subscribers.
prop_invalidVoteRejected :: Property
prop_invalidVoteRejected =
  forAll genCommittee $ \committee ->
    forAll (genVoteFor committee) $ \v0 -> property $ runSimOrThrow $ do
      let badVote = v0{voteSignature = False}
      st <- newLeiosVoteState (pure (Just committee))
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
  forAll genCommittee $ \committee ->
    forAll (genVoteFor committee) $ \vote -> property $ runSimOrThrow $ do
      st <- newLeiosVoteState (pure Nothing)
      sub <- subscribeVotes st
      r <- addVote st vote
      mVote <- timeout 0.1 $ atomically $ getNextVote sub
      pure $
        r === NoCommittee
          .&&. counterexample "subscriber should not see vote" (isNothing mVote === True)

-- | A vote produced with a key that is not a member of the committee must be
-- rejected with 'SignerNotInCommittee' and not published.
prop_signerNotInCommittee :: Property
prop_signerNotInCommittee =
  forAll genCommittee $ \committee ->
    forAll (genKeyNotIn committee) $ \key ->
      forAll genPoint $ \point -> property $ runSimOrThrow $ do
        -- Claim to have a seat on the committee
        let vote = signLeiosVote key (MkVoterId 0) point
        st <- newLeiosVoteState (pure (Just committee))
        sub <- subscribeVotes st
        r <- addVote st vote
        mVote <- timeout 0.1 $ atomically $ getNextVote sub
        pure $
          r === VoteInvalid SignerNotInCommittee
            .&&. counterexample "subscriber should not see invalid vote" (isNothing mVote === True)
