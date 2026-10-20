{-# LANGUAGE TypeApplications #-}

-- | Tests of which votes the node's 'VoteState' keeps.
module Test.Consensus.Leios.VoteState (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Ledger.State (emptyLeiosCommittee)
import Control.Monad (forM)
import Control.Monad.IOSim (runSimOrThrow)
import qualified Data.ByteString.Short as SBS
import Data.List (scanl')
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Time.Clock (NominalDiffTime)
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..), addRelTime)
import Ouroboros.Consensus.Leios.AnnounceState (newAnnounceState)
import Ouroboros.Consensus.Leios.NotifyLog (newNotifyLog)
import Ouroboros.Consensus.Leios.Types (LeiosVote (..), RankingBlockHash (..))
import Ouroboros.Consensus.Leios.VoteState
  ( ElectionVotes (..)
  , newVoteState
  , readVotes
  , recordVote
  , seenVote
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
  ( AcceptedVote (..)
  , VoteRelayWindow (..)
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
  , listOf
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
        "the node keeps the first vote of each seat in every election still worth relaying"
        prop_keepsUnexpired
    ]

-- | Recording every vote that has neither expired nor been seen, as 'addVote'
-- does, keeps the first vote of each seat in exactly the elections still worth
-- relaying when it last recorded one, and never records a vote of one seat in
-- one election twice, so the node never relays it twice either.
prop_keepsUnexpired :: Property
prop_keepsUnexpired = forAll genVotes $ \vs ->
  let (kept, recorded) = runSimOrThrow $ do
        notifyLog <- newNotifyLog @_ @LeiosTestBlock
        announceState <- newAnnounceState notifyLog
        st <- newVoteState announceState notifyLog
        recorded' <- fmap catMaybes $ forM vs $ \(at, slot, vote) -> atomically $ do
          seen <- seenVote st slot (voteSeatId vote)
          if seen || at > relayUntilOf slot
            then pure Nothing
            else Just (at, slot, voteSeatId vote) <$ recordVote st at (accepted slot vote)
        kept' <- atomically (readVotes st)
        pure (kept', recorded')
      lastRecorded = maximum (RelativeTime 0 : [at | (at, _, _) <- recorded])
      elections = [(slot, seat) | (_, slot, seat) <- recorded]
   in conjoin
        [ counterexample "kept" $
            Map.map electionVotes kept
              === Map.filterWithKey (\slot _ -> lastRecorded <= relayUntilOf slot) (firstVotes vs)
        , counterexample "recorded twice" $ length elections === Set.size (Set.fromList elections)
        ]
 where
  firstVotes vs =
    Map.fromListWith
      (flip Map.union)
      [(slot, Map.singleton (voteSeatId v) v) | (at, slot, v) <- vs, at <= relayUntilOf slot]

  accepted slot vote =
    MkAcceptedVote
      { acceptedVote = vote
      , voteElectionSlot = slot
      , voteCommittee = emptyLeiosCommittee
      , voteRelayWindow = MkVoteRelayWindow (slotStart slot) (relayUntilOf slot)
      }

-- | Votes arriving over time, each for an election slot that began at most two
-- windows earlier, so that some arrive after their window closed.
genVotes :: Gen [(RelativeTime, SlotNo, LeiosVote)]
genVotes = do
  gaps <- listOf (choose (0, 2 :: Double))
  forM (drop 1 (scanl' (+) 0 gaps)) $ \t -> do
    age <- choose (0, 2 * realToFrac window)
    (,,) (RelativeTime (realToFrac t)) (SlotNo (floor (max 0 (t - age))))
      <$> elements someVotes

-- | One-second slots, the first starting at 0.
slotStart :: SlotNo -> RelativeTime
slotStart (SlotNo s) = RelativeTime (fromIntegral s)

-- | Votes are worth relaying for 'window' seconds after their slot began.
relayUntilOf :: SlotNo -> RelativeTime
relayUntilOf slot = addRelTime window (slotStart slot)

window :: NominalDiffTime
window = 10

-- | Several votes of each of a few seats, so that the first one is told from
-- the others.
someVotes :: [LeiosVote]
someVotes =
  [ signedVote (MkRankingBlockHash (UnsafeHash (SBS.replicate 32 tag))) seat
  | tag <- [0 .. 3]
  , seat <- [0 .. 3]
  ]
