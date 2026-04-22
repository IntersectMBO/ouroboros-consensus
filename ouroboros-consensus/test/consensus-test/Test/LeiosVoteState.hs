module Test.LeiosVoteState (tests) where

import Cardano.Slotting.Slot (SlotNo (SlotNo))
import Control.Concurrent.Class.MonadSTM.Strict (atomically)
import Control.Monad (forM_)
import Control.Monad.Class.MonadTimer.SI (timeout)
import Control.Monad.IOSim (runSimOrThrow)
import Data.Maybe (isNothing)
import LeiosDemoTypes
  ( EbHash (MkEbHash)
  , LeiosVote (..)
  , VoterId (MkVoterId)
  )
import LeiosVoteState
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck
  ( Gen
  , Property
  , arbitrary
  , chooseInt
  , counterexample
  , forAll
  , listOf1
  , property
  , vectorOf
  , (.&&.)
  , (===)
  )
import qualified Data.ByteString as BS

tests :: TestTree
tests =
  testGroup
    "LeiosVoteState"
    [ testProperty "subscriber receives added vote" prop_subscriberReceivesVote
    , testProperty "subscriber receives all distinct votes" prop_subscriberReceivesAll
    , testProperty "duplicate votes are not delivered" prop_deduplicateVotes
    , testProperty "late subscriber does not see prior votes" prop_lateSubscriber
    ]

genVote :: Gen LeiosVote
genVote = do
  slot <- SlotNo <$> arbitrary
  voter <- MkVoterId <$> arbitrary
  hash <- MkEbHash . BS.pack <$> vectorOf 32 (fromIntegral <$> chooseInt (0, 255))
  sig <- arbitrary
  pure $ MkLeiosVote slot voter hash sig

-- | A subscriber should receive a vote that was added after subscribing.
prop_subscriberReceivesVote :: Property
prop_subscriberReceivesVote =
  forAll genVote $ \vote -> property $ runSimOrThrow $ do
    st <- newLeiosVoteState
    sub <- subscribeVotes st
    addVote st vote
    received <- atomically $ getNextVote sub
    pure $ received === vote

-- | A subscriber should receive all distinct votes in order.
prop_subscriberReceivesAll :: Property
prop_subscriberReceivesAll =
  forAll (listOf1 genVote) $ \votes -> property $ runSimOrThrow $ do
    st <- newLeiosVoteState
    sub <- subscribeVotes st
    forM_ votes (addVote st)
    received <- mapM (\_ -> atomically $ getNextVote sub) votes
    pure $ received === votes

-- | Adding the same vote twice should only deliver it once.
prop_deduplicateVotes :: Property
prop_deduplicateVotes =
  forAll genVote $ \vote -> property $ runSimOrThrow $ do
    st <- newLeiosVoteState
    sub <- subscribeVotes st
    addVote st vote
    addVote st vote
    received <- atomically $ getNextVote sub
    -- The second read should block (no second notification).
    mSecond <- timeout 0.1 $ atomically $ getNextVote sub
    pure $
      counterexample "first vote" (received === vote)
        .&&. counterexample "second read should timeout" (isNothing mSecond === True)

-- | A subscriber that subscribes after a vote was added should not see it.
prop_lateSubscriber :: Property
prop_lateSubscriber =
  forAll genVote $ \vote -> property $ runSimOrThrow $ do
    st <- newLeiosVoteState
    addVote st vote
    sub <- subscribeVotes st
    mVote <- timeout 0.1 $ atomically $ getNextVote sub
    pure $ counterexample "late subscriber should not see prior vote" (isNothing mVote === True)
