{-# LANGUAGE OverloadedRecordDot #-}

module Test.LeiosVoteState (tests) where

import Cardano.Crypto.DSIGN
  ( DSIGNAlgorithm (deriveVerKeyDSIGN)
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
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Ratio ((%))
import LeiosDemoTypes
  ( LeiosSeatId (..)
  , LeiosSigningKey
  , LeiosVote (..)
  , RbHash
  , VoteInvalid (..)
  , Weight
  , getLeiosSeatId
  , leiosCommitteeSize
  , signLeiosVote
  , validateLeiosVote
  )
import LeiosVoteState
  ( AddVoteResult (..)
  , addVote
  , getNextVote
  , newLeiosVoteState
  , queryCert
  , subscribeVotes
  )
import Test.Cardano.Crypto.Leios.Gen (TestCommittee (..), genCommittee, genLeiosSigningKey)
import Test.LeiosDemoDb (genRbHash)
import Test.QuickCheck
  ( Gen
  , Property
  , counterexample
  , elements
  , forAll
  , listOf1
  , property
  , suchThat
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
    , testProperty "certification follows the threshold parameter" prop_certificationFollowsThreshold
    ]

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
  let vid = fromJust $ getLeiosSeatId (deriveVerKeyDSIGN key) c.committee
  signLeiosVote key vid <$> genRbHash

-- | A subscriber should receive a vote that was added after subscribing.
prop_subscriberReceivesVote :: Property
prop_subscriberReceivesVote =
  forAll genCommittee $ \testCommittee ->
    forAll (genVoteFor testCommittee) $ \vote -> property $ runSimOrThrow $ do
      st <- newLeiosVoteState (pure (Just (testCommittee.committee, testQuorumThreshold)))
      sub <- subscribeVotes st
      _ <- addVote st vote
      received <- atomically $ getNextVote sub
      pure $ received === vote

-- | A subscriber should receive all distinct votes in order.
prop_subscriberReceivesAll :: Property
prop_subscriberReceivesAll =
  forAll genCommittee $ \testCommittee ->
    forAll (listOf1 (genVoteFor testCommittee)) $ \votes -> property $ runSimOrThrow $ do
      st <- newLeiosVoteState (pure (Just (testCommittee.committee, testQuorumThreshold)))
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
      st <- newLeiosVoteState (pure (Just (testCommittee.committee, testQuorumThreshold)))
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
        committeeVar <- atomically $ newTVar (Just (initialCommittee.committee, testQuorumThreshold))
        st <- newLeiosVoteState (readTVar committeeVar)
        r1 <- addVote st vote
        -- Swap to a fresh committee for which the vote does not validate
        -- (different voter keys, so the signature check fails).
        atomically $ writeTVar committeeVar (Just (otherCommittee.committee, testQuorumThreshold))
        r2 <- addVote st vote
        pure $
          counterexample "first add" (isAdded r1)
            .&&. counterexample "second add" (r2 === AlreadyKnown)

-- | A subscriber that subscribes after a vote was added should not see it.
prop_lateSubscriber :: Property
prop_lateSubscriber =
  forAll genCommittee $ \testCommittee ->
    forAll (genVoteFor testCommittee) $ \vote -> property $ runSimOrThrow $ do
      st <- newLeiosVoteState (pure (Just (testCommittee.committee, testQuorumThreshold)))
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
          st <- newLeiosVoteState (pure (Just (testCommittee.committee, testQuorumThreshold)))
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
      forAll genRbHash $ \announcement -> property $ runSimOrThrow $ do
        -- VoterId must be outside of committe, otherwise this is just a bad signature
        let n = leiosCommitteeSize testCommittee.committee
        let vote = signLeiosVote key (LeiosSeatId $ fromIntegral n) announcement
        st <- newLeiosVoteState (pure (Just (testCommittee.committee, testQuorumThreshold)))
        sub <- subscribeVotes st
        r <- addVote st vote
        mVote <- timeout 0.1 $ atomically $ getNextVote sub
        pure $
          r === VoteInvalid SignerNotInCommittee
            .&&. counterexample "subscriber should not see invalid vote" (isNothing mVote === True)

isAdded :: AddVoteResult -> Property
isAdded Added{} = property True
isAdded r = counterexample ("expected Added, got " ++ show r) False

-- | The quorum threshold these tests run with. Certification used to be pinned
-- to a hardcoded 3/4, so keeping that value here leaves the plumbing properties
-- above behaving exactly as they did before the parameter was introduced.
testQuorumThreshold :: Weight
testQuorumThreshold = 3 % 4

-- | Certification is driven by the threshold parameter rather than a constant.
--
-- Pinned at the two ends, which holds whatever weights the generator picks: at a
-- zero threshold the first vote already certifies, and at a threshold above the
-- committee's total weight even a vote from every seat does not.
prop_certificationFollowsThreshold :: Property
prop_certificationFollowsThreshold =
  forAll genCommittee $ \testCommittee ->
    forAll genRbHash $ \rbHash ->
      let votes = votesForEverySeat testCommittee rbHash
          totalWeight = sum (voteWeight testCommittee <$> votes)
       in property $ runSimOrThrow $ do
            certAtZero <- certifyWith testCommittee.committee 0 votes rbHash
            certAboveTotal <- certifyWith testCommittee.committee (totalWeight + 1) votes rbHash
            pure $
              counterexample
                ("total weight: " <> show totalWeight <> ", votes: " <> show (length votes))
                ( counterexample "a zero threshold should certify on the first vote" (isJust certAtZero)
                    .&&. counterexample
                      "a threshold above the committee's total weight must never certify"
                      (isNothing certAboveTotal)
                )
 where
  certifyWith c threshold votes rbHash = do
    st <- newLeiosVoteState (pure (Just (c, threshold)))
    forM_ votes (addVote st)
    queryCert st rbHash

-- | One vote per seat that has a key, all on the same point so they tally
-- together.
votesForEverySeat :: TestCommittee -> RbHash -> [LeiosVote]
votesForEverySeat c rbHash =
  [ signLeiosVote key vid rbHash
  | key <- c.allKeys
  , Just vid <- [getLeiosSeatId (deriveVerKeyDSIGN key) c.committee]
  ]

-- | The weight a vote carries, as the committee accounts for it.
voteWeight :: TestCommittee -> LeiosVote -> Weight
voteWeight c vote = either (const 0) id $ validateLeiosVote c.committee vote
