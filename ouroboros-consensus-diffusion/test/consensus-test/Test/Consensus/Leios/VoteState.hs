{-# LANGUAGE TypeApplications #-}

-- | Tests of which votes the node's 'VoteState' keeps.
module Test.Consensus.Leios.VoteState (tests) where

import Control.Monad (forM)
import Control.Monad.IOSim (runSimOrThrow)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.Leios.AnnounceState (newAnnounceState)
import Ouroboros.Consensus.Leios.NotifyLog (newNotifyLog)
import Ouroboros.Consensus.Leios.Types (LeiosVote (..), RbHash (..))
import Ouroboros.Consensus.Leios.VoteState
  ( maxVoteElections
  , newVoteState
  , readVotes
  , recordVote
  , seenVote
  )
import Ouroboros.Consensus.Util.IOLike (atomically)
import Test.Consensus.Leios.LeiosNotify.Codec (signedVote)
import Test.Consensus.Leios.TestBlock (LeiosTestBlock)
import Test.QuickCheck
  ( Gen
  , Property
  , choose
  , conjoin
  , counterexample
  , elements
  , forAll
  , vectorOf
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "VoteState"
    [ testProperty
        "the node keeps the first vote of each seat in the latest election slots"
        prop_keepsLatest
    ]

-- | Recording every vote the node has not seen keeps exactly the latest
-- 'maxVoteElections' slots seen, each with the first vote of each seat, and
-- never records a vote of one seat in one slot twice, so the node never relays
-- it twice either.
prop_keepsLatest :: Property
prop_keepsLatest = forAll genVotes $ \vs ->
  let (kept, recorded) = runSimOrThrow $ do
        notifyLog <- newNotifyLog @_ @LeiosTestBlock
        announceState <- newAnnounceState notifyLog
        st <- newVoteState announceState notifyLog
        recorded' <- fmap catMaybes $ forM vs $ \(slot, vote) -> atomically $ do
          seen <- seenVote st slot (voteSeatId vote)
          if seen
            then pure Nothing
            else Just (slot, voteSeatId vote) <$ recordVote st slot vote
        kept' <- atomically (readVotes st)
        pure (kept', recorded')
   in conjoin
        [ counterexample "kept" $
            kept === Map.drop (Map.size (firstVotes vs) - maxVoteElections) (firstVotes vs)
        , counterexample "recorded twice" $ length recorded === Set.size (Set.fromList recorded)
        ]
 where
  firstVotes vs = Map.fromListWith (flip Map.union) [(slot, Map.singleton (voteSeatId v) v) | (slot, v) <- vs]

-- | Enough votes, over enough slots, that the node forgets some.
genVotes :: Gen [(SlotNo, LeiosVote)]
genVotes = do
  n <- choose (0, 10 * maxVoteElections)
  vectorOf n $
    (,) <$> (SlotNo <$> choose (0, 3 * fromIntegral maxVoteElections)) <*> elements someVotes

-- | Several votes of each of a few seats, so that the first one is told from
-- the others.
someVotes :: [LeiosVote]
someVotes = [signedVote (MkRbHash (BS.replicate 32 tag)) seat | tag <- [0 .. 3], seat <- [0 .. 3]]
