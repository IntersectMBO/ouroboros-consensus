{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

#if __GLASGOW_HASKELL__ >= 910
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

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
import qualified Data.ByteString.Char8 as BS8
import Data.Function (on)
import Data.List (nub, nubBy)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Ratio ((%))
import qualified Data.Set as Set
import LeiosDemoTypes
  ( LeiosSeatId (..)
  , LeiosSigningKey
  , LeiosVote (..)
  , RbHash (..)
  , VoteInvalid (..)
  , Weight
  , getLeiosSeatId
  , leiosCommitteeSize
  , signLeiosVote
  , validateLeiosVote
  )
import LeiosTxCache.API (maxAnnouncementCount)
import LeiosVoteState
  ( AddVoteResult (..)
  , VoteTally (..)
  , addVote
  , getNextVote
  , leiosVoteWindow
  , newLeiosVoteState
  , queryCert
  , subscribeVotes
  , withinVoteWindow
  )
import Test.Cardano.Crypto.Leios.Gen (TestCommittee (..), genCommittee, genLeiosSigningKey)
import Test.QuickCheck
  ( Gen
  , Property
  , choose
  , counterexample
  , elements
  , forAll
  , forAllShrink
  , listOf1
  , property
  , shrink
  , shrinkIntegral
  , shrinkList
  , shrinkNothing
  , shuffle
  , sublistOf
  , suchThat
  , vectorOf
  , (.&&.)
  , (===)
  , (==>)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.LeiosHash (genRbHash)
import Text.Printf (printf)

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
    , testProperty "reported tally accumulates the reported weights" prop_tallyAccumulates
    , testProperty
        "the oldest point is forgotten once the window is full"
        prop_pointWindowEvictsOldest
    , testProperty
        "a replayed vote is taken once, and only for a votable point"
        prop_replayTakesEachVoteOnce
    , testProperty "the vote window is the newest blocks" prop_voteWindowIsNewestBlocks
    ]

-- | Votes arriving in any order and any number of times, as a fixed set of them
-- did when every node kept relaying votes for long-gone points.
data Replay = Replay
  { rpPoints :: !Int
  -- ^ Distinct points, one vote each; the vote for point @i@ is vote @i@.
  , rpVotable :: ![Int]
  -- ^ The points the vote state is told it may take votes for.
  , rpBad :: ![Int]
  -- ^ The votes that carry a signature the committee rejects.
  , rpArrivals :: ![Int]
  -- ^ The order the votes arrive in, repeats included.
  }
  deriving Show

-- | Up to twice 'maxAnnouncementCount' points, so that arrivals cycle through
-- more points than the state keeps, at most 'leiosVoteWindow' of them votable,
-- and one to three rounds of every vote in a fresh order.
genReplay :: Gen Replay
genReplay = do
  n <- choose (1, 2 * maxAnnouncementCount + leiosVoteWindow)
  votable <- take leiosVoteWindow <$> (shuffle =<< sublistOf [0 .. n - 1])
  bad <- sublistOf [0 .. n - 1]
  rounds <- choose (1, 3)
  arrivals <- concat <$> vectorOf rounds (shuffle [0 .. n - 1])
  pure $ Replay n votable bad arrivals

-- | Fewer points (dropping every mention of the removed ones), fewer votable or
-- bad points, or fewer arrivals; each step makes the replay strictly smaller.
shrinkReplay :: Replay -> [Replay]
shrinkReplay (Replay n votable bad arrivals) =
  [ Replay n' (filter (< n') votable) (filter (< n') bad) (filter (< n') arrivals)
  | n' <- shrinkIntegral n
  , n' >= 1
  ]
    ++ [Replay n votable' bad arrivals | votable' <- shrinkList shrinkNothing votable]
    ++ [Replay n votable bad' arrivals | bad' <- shrinkList shrinkNothing bad]
    ++ [Replay n votable bad arrivals' | arrivals' <- shrinkList shrinkNothing arrivals]

-- | What 'addVote' did with one arrival, without the tally it reports.
data Outcome = OAdded | OKnown | ONotVotable | OInvalid VoteInvalid | ONoCommittee
  deriving (Eq, Show)

outcome :: AddVoteResult -> Outcome
outcome = \case
  Added{} -> OAdded
  AlreadyKnown -> OKnown
  NotVotable -> ONotVotable
  VoteInvalid r -> OInvalid r
  NoCommittee -> ONoCommittee

-- | The exact outcome of every arrival: a vote for a point outside the window is
-- dropped before validation, whatever its signature; inside it a bad vote is
-- rejected each time, and a good one is taken on its first arrival and known
-- after that.
replayOracle :: Replay -> [Outcome]
replayOracle (Replay _ votable bad arrivals) = go Set.empty arrivals
 where
  go _ [] = []
  go taken (i : is)
    | i `notElem` votable = ONotVotable : go taken is
    | i `elem` bad = OInvalid InvalidSignature : go taken is
    | i `Set.member` taken = OKnown : go taken is
    | otherwise = OAdded : go (Set.insert i taken) is

-- | Every vote is taken at most once and only for a votable point, and only
-- the votes taken reach subscribers, however often and in whatever order a
-- fixed set of votes keeps arriving.
prop_replayTakesEachVoteOnce :: Property
prop_replayTakesEachVoteOnce =
  forAll genCommittee $ \testCommittee ->
    forAll (genKeyNotIn testCommittee) $ \outsider ->
      forAllShrink genReplay shrinkReplay $ \replay ->
        let key = head testCommittee.allKeys
            vid = fromJust $ getLeiosSeatId (deriveVerKeyDSIGN key) testCommittee.committee
            point i = MkRbHash (BS8.pack (printf "%032d" i))
            vote i
              | i `elem` replay.rpBad = signLeiosVote outsider vid (point i)
              | otherwise = signLeiosVote key vid (point i)
            votable = Set.fromList (point <$> replay.rpVotable)
            expected = replayOracle replay
         in runSimOrThrow $ do
              st <-
                newLeiosVoteState
                  (pure (Just (testCommittee.committee, testQuorumThreshold)))
                  (pure (`Set.member` votable))
              sub <- subscribeVotes st
              results <- mapM (fmap outcome . addVote st . vote) replay.rpArrivals
              published <- drain sub
              let taken = [vote i | (i, OAdded) <- zip replay.rpArrivals expected]
              pure $
                counterexample "outcomes differ from the oracle" (results === expected)
                  .&&. counterexample "published votes differ from those taken" (published === taken)
 where
  drain sub =
    timeout 0.1 (atomically (getNextVote sub)) >>= \case
      Nothing -> pure []
      Just v -> (v :) <$> drain sub

-- | A point is votable exactly when it is among the newest 'leiosVoteWindow'
-- blocks, whatever the chain's length and wherever the point sits on it.
prop_voteWindowIsNewestBlocks :: Property
prop_voteWindowIsNewestBlocks =
  forAllShrink genChainQuery shrinkChainQuery $ \(len, q) ->
    let point i = MkRbHash (BS8.pack (printf "%032d" i))
        newestFirst = point <$> [0 .. len - 1]
     in counterexample (show (len, q)) $
          withinVoteWindow newestFirst (point q) === (q < len && q < leiosVoteWindow)
 where
  genChainQuery = do
    len <- choose (0, 3 * leiosVoteWindow)
    q <- choose (0, len + 2)
    pure (len, q)
  shrinkChainQuery (len, q) =
    [(len', q') | (len', q') <- shrink (len, q), len' >= 0, q' >= 0, q' <= len' + 2]

-- | Past 'maxAnnouncementCount' distinct points, the first one seen is dropped:
-- its vote is accepted again rather than reported as already known, while a
-- point still inside the window keeps rejecting its duplicate.
--
-- This is the bound being load-bearing, not a property anyone should want --
-- see 'boundPoints' on why first-seen order is the wrong order.
prop_pointWindowEvictsOldest :: Property
prop_pointWindowEvictsOldest =
  forAll genCommittee $ \testCommittee ->
    forAll (vectorOf (maxAnnouncementCount + 1) genRbHash) $ \rbHashes ->
      -- 32 random bytes apiece; a collision would only weaken the test.
      length (nub rbHashes) == length rbHashes ==>
        property $
          runSimOrThrow $ do
            st <- newLeiosVoteState (pure (Just (testCommittee.committee, testQuorumThreshold))) allVotable
            let key = head testCommittee.allKeys
                vid = fromJust $ getLeiosSeatId (deriveVerKeyDSIGN key) testCommittee.committee
                votes = [signLeiosVote key vid h | h <- rbHashes]
            mapM_ (addVote st) votes
            evicted <- addVote st (head votes)
            retained <- addVote st (last votes)
            pure $
              counterexample "oldest point should have been evicted" (isAdded evicted)
                .&&. counterexample "newest point should still be known" (retained === AlreadyKnown)

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
      st <- newLeiosVoteState (pure (Just (testCommittee.committee, testQuorumThreshold))) allVotable
      sub <- subscribeVotes st
      _ <- addVote st vote
      received <- atomically $ getNextVote sub
      pure $ received === vote

-- | A subscriber should receive all distinct votes in order.
prop_subscriberReceivesAll :: Property
prop_subscriberReceivesAll =
  forAll genCommittee $ \testCommittee ->
    forAll (listOf1 (genVoteFor testCommittee)) $ \votes -> property $ runSimOrThrow $ do
      st <- newLeiosVoteState (pure (Just (testCommittee.committee, testQuorumThreshold))) allVotable
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
      st <- newLeiosVoteState (pure (Just (testCommittee.committee, testQuorumThreshold))) allVotable
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
        st <- newLeiosVoteState (readTVar committeeVar) allVotable
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
      st <- newLeiosVoteState (pure (Just (testCommittee.committee, testQuorumThreshold))) allVotable
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
          st <- newLeiosVoteState (pure (Just (testCommittee.committee, testQuorumThreshold))) allVotable
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
      st <- newLeiosVoteState (pure Nothing) allVotable
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
        st <- newLeiosVoteState (pure (Just (testCommittee.committee, testQuorumThreshold))) allVotable
        sub <- subscribeVotes st
        r <- addVote st vote
        mVote <- timeout 0.1 $ atomically $ getNextVote sub
        pure $
          r === VoteInvalid SignerNotInCommittee
            .&&. counterexample "subscriber should not see invalid vote" (isNothing mVote === True)

-- | Every point is votable: the window plays no part in these properties.
allVotable :: Applicative f => f (RbHash -> Bool)
allVotable = pure (const True)

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
    st <- newLeiosVoteState (pure (Just (c, threshold))) allVotable
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

-- | Votes for one point, one per distinct committee seat.
--
-- Deduplicated on seat rather than on key: two keys mapping to the same seat
-- would have the second replace the first in the tally rather than add to it,
-- which is a different property from the one below.
genOneVotePerSeat :: TestCommittee -> RbHash -> Gen [LeiosVote]
genOneVotePerSeat c rbHash = do
  keys <- sublistOf c.allKeys `suchThat` (not . null)
  pure [signLeiosVote key (seatOf key) rbHash | key <- nubBy ((==) `on` seatOf) keys]
 where
  seatOf key = fromJust $ getLeiosSeatId (deriveVerKeyDSIGN key) c.committee

-- | The tally reported with each accepted vote must be the running sum of the
-- individual weights reported so far.
--
-- This pins the incrementally maintained 'psTotal' against the sum over the
-- voter map that it replaced. Nothing else in this suite asserts anything about
-- the tally value, so without this a wrong total is invisible to the tests and
-- shows up only as wrong telemetry.
prop_tallyAccumulates :: Property
prop_tallyAccumulates =
  forAll genCommittee $ \testCommittee ->
    forAll genRbHash $ \rbHash ->
      forAll (genOneVotePerSeat testCommittee rbHash) $ \votes ->
        property $ runSimOrThrow $ do
          st <- newLeiosVoteState (pure (Just (testCommittee.committee, testQuorumThreshold))) allVotable
          results <- mapM (addVote st) votes
          pure $ case traverse addedWeights results of
            Nothing ->
              counterexample ("expected every add to be Added, got " ++ show results) False
            Just weighted ->
              let (ownWeights, tallies) = unzip weighted
               in counterexample
                    ("own weights " ++ show ownWeights ++ ", tallies " ++ show tallies)
                    (tallies === scanl1 (+) ownWeights)

-- | The own weight and running tally carried by 'Added', or 'Nothing' for any
-- other result.
addedWeights :: AddVoteResult -> Maybe (Weight, Weight)
addedWeights (Added VoteTally{vtWeight, vtTally} _) = Just (vtWeight, vtTally)
addedWeights _ = Nothing
