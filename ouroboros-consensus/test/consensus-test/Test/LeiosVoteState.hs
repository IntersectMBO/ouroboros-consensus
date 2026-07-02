{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications #-}

module Test.LeiosVoteState (tests) where

import Cardano.Crypto.DSIGN
  ( DSIGNAlgorithm (deriveVerKeyDSIGN)
  , genKeyDSIGN
  , seedSizeDSIGN
  )
import Control.Concurrent.Class.MonadSTM.Strict
  ( atomically
  , newTVar
  , readTVar
  , writeTVar
  )
import Control.Monad (forM_)
import Control.Monad.Class.MonadTimer.SI (timeout)
import Control.Monad.IOSim (runSimOrThrow)
import Data.Data (Proxy (..))
import Data.Maybe (fromJust, isNothing)
import LeiosDemoTypes
  ( LeiosCommittee (..)
  , LeiosDSIGN
  , LeiosSigningKey
  , LeiosVote (..)
  , LeiosVoterId (..)
  , VoteInvalid (..)
  , getLeiosVoterId
  , leiosCommitteeSize
  , mkCommitteeEveryoneVotes
  , signLeiosVote
  )
import LeiosVoteState
  ( AddVoteResult (..)
  , addVote
  , getNextVote
  , newLeiosVoteState
  , subscribeVotes
  )
import Test.Crypto.Util (arbitrarySeedOfSize)
import Test.LeiosDemoDb (genRbHash)
import Test.QuickCheck
  ( Gen
  , Property
  , chooseInt
  , counterexample
  , elements
  , forAll
  , listOf1
  , property
  , suchThat
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
    , testProperty "duplicate check before validation" prop_deduplicateBeforeValidation
    , testProperty "late subscriber does not see prior votes" prop_lateSubscriber
    , testProperty "invalid vote is rejected and not published" prop_invalidVoteRejected
    , testProperty "no committee rejects vote" prop_noCommitteeRejected
    , testProperty "vote signed with key not on committee is rejected" prop_signerNotInCommittee
    ]

genLeiosSigningKey :: Gen LeiosSigningKey
genLeiosSigningKey = do
  seed <- arbitrarySeedOfSize (seedSizeDSIGN (Proxy @LeiosDSIGN))
  pure $ genKeyDSIGN seed

data TestCommittee = TestCommittee
  { committee :: LeiosCommittee
  , allKeys :: [LeiosSigningKey]
  }
  deriving Show

-- | A non-empty committee.
genCommittee :: Gen TestCommittee
genCommittee = do
  n <- chooseInt (1, 10)
  allKeys <- vectorOf n genLeiosSigningKey
  weights <- vectorOf n (chooseInt (1, 100))
  pure
    TestCommittee
      { committee = mkCommitteeEveryoneVotes $ zip (deriveVerKeyDSIGN <$> allKeys) weights
      , allKeys
      }

-- | A 'VotingKey' that is *not* a member of the given committee.
genKeyNotIn :: TestCommittee -> Gen LeiosSigningKey
genKeyNotIn c = do
  genLeiosSigningKey `suchThat` \sk ->
    not $ elem sk c.allKeys

-- | A vote produced by 'signLeiosVote' with an arbitrary point and a key from the
-- committee.
genVoteFor :: TestCommittee -> Gen LeiosVote
genVoteFor c = do
  key <- elements c.allKeys
  let vid = fromJust $ getLeiosVoterId (deriveVerKeyDSIGN key) c.committee
  signLeiosVote key vid <$> genRbHash

-- | A subscriber should receive a vote that was added after subscribing.
prop_subscriberReceivesVote :: Property
prop_subscriberReceivesVote =
  forAll genCommittee $ \testCommittee ->
    forAll (genVoteFor testCommittee) $ \vote -> property $ runSimOrThrow $ do
      st <- newLeiosVoteState (pure (Just testCommittee.committee))
      sub <- subscribeVotes st
      _ <- addVote st vote
      received <- atomically $ getNextVote sub
      pure $ received === vote

-- | A subscriber should receive all distinct votes in order.
prop_subscriberReceivesAll :: Property
prop_subscriberReceivesAll =
  forAll genCommittee $ \testCommittee ->
    forAll (listOf1 (genVoteFor testCommittee)) $ \votes -> property $ runSimOrThrow $ do
      st <- newLeiosVoteState (pure (Just testCommittee.committee))
      sub <- subscribeVotes st
      forM_ votes (addVote st)
      received <- mapM (\_ -> atomically $ getNextVote sub) votes
      pure $ received === votes

-- | Adding the same vote twice should only deliver it once and report
-- 'AlreadyKnown' on the second add.
prop_deduplicateVotes :: Property
prop_deduplicateVotes =
  forAll genCommittee $ \testCommittee ->
    forAll (genVoteFor testCommittee) $ \vote -> property $ runSimOrThrow $ do
      st <- newLeiosVoteState (pure (Just testCommittee.committee))
      sub <- subscribeVotes st
      r1 <- addVote st vote
      r2 <- addVote st vote
      received <- atomically $ getNextVote sub
      -- The second read should block (no second notification).
      mSecond <- timeout 0.1 $ atomically $ getNextVote sub
      pure $
        counterexample "first add" (isAdded r1)
          .&&. counterexample "second add" (r2 === AlreadyKnown)
          .&&. counterexample "first vote" (received === vote)
          .&&. counterexample "second read should timeout" (isNothing mSecond === True)

-- | Re-adding a vote must short-circuit as 'AlreadyKnown' even when the vote
-- would no longer pass validation under the current committee. This pins
-- ordering: the deduplication check runs before validation.
prop_deduplicateBeforeValidation :: Property
prop_deduplicateBeforeValidation =
  forAll genCommittee $ \initialCommittee ->
    forAll (genVoteFor initialCommittee) $ \vote ->
      forAll genCommittee $ \otherCommittee -> property $ runSimOrThrow $ do
        committeeVar <- atomically $ newTVar (Just initialCommittee.committee)
        st <- newLeiosVoteState (readTVar committeeVar)
        r1 <- addVote st vote
        -- Swap to a fresh committee for which the vote does not validate
        -- (different voter keys, so the signature check fails).
        atomically $ writeTVar committeeVar (Just otherCommittee.committee)
        r2 <- addVote st vote
        pure $
          counterexample "first add" (isAdded r1)
            .&&. counterexample "second add" (r2 === AlreadyKnown)

-- | A subscriber that subscribes after a vote was added should not see it.
prop_lateSubscriber :: Property
prop_lateSubscriber =
  forAll genCommittee $ \testCommittee ->
    forAll (genVoteFor testCommittee) $ \vote -> property $ runSimOrThrow $ do
      st <- newLeiosVoteState (pure (Just testCommittee.committee))
      _ <- addVote st vote
      sub <- subscribeVotes st
      mVote <- timeout 0.1 $ atomically $ getNextVote sub
      pure $ counterexample "late subscriber should not see prior vote" (isNothing mVote === True)

-- | A vote rejected by validation must not be published to subscribers.
prop_invalidVoteRejected :: Property
prop_invalidVoteRejected =
  forAll genCommittee $ \testCommittee ->
    forAll (genVoteFor testCommittee) $ \vote ->
      forAll genLeiosSigningKey $ \someKey ->
        property $ runSimOrThrow $ do
          let badVote = signLeiosVote someKey vote.voterId vote.announcingRbHash
          st <- newLeiosVoteState (pure (Just testCommittee.committee))
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
  forAll genCommittee $ \testCommittee ->
    forAll (genVoteFor testCommittee) $ \vote -> property $ runSimOrThrow $ do
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
  forAll genCommittee $ \testCommittee ->
    forAll (genKeyNotIn testCommittee) $ \key ->
      forAll genRbHash $ \announcingRbHash -> property $ runSimOrThrow $ do
        -- VoterId must be outside of committe, otherwise this is just a bad signature
        let n = leiosCommitteeSize testCommittee.committee
        let vote = signLeiosVote key (LeiosVoterId $ fromIntegral n) announcingRbHash
        st <- newLeiosVoteState (pure (Just testCommittee.committee))
        sub <- subscribeVotes st
        r <- addVote st vote
        mVote <- timeout 0.1 $ atomically $ getNextVote sub
        pure $
          r === VoteInvalid SignerNotInCommittee
            .&&. counterexample "subscriber should not see invalid vote" (isNothing mVote === True)

isAdded :: AddVoteResult -> Property
isAdded Added{} = property True
isAdded r = counterexample ("expected Added, got " ++ show r) False
