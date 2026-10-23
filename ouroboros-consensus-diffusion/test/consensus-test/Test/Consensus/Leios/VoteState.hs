{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests of which votes the node's 'VoteState' keeps.
module Test.Consensus.Leios.VoteState (tests) where

import Cardano.Crypto.DSIGN.Class (signDSIGN)
import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Crypto.Leios (LeiosSeatId (..), leiosSignContext)
import Cardano.Ledger.BaseTypes (Milliseconds32 (..), knownNonZeroBounded)
import Cardano.Ledger.State (emptyLeiosCommittee)
import Control.Monad (forM)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (Tracer, mkTracer, nullTracer)
import qualified Data.ByteString.Short as SBS
import Data.List (scanl')
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Time.Clock (NominalDiffTime)
import Ouroboros.Consensus.Block (ConvertRawHash (..), SlotNo (..), blockHash, blockSlot)
import Ouroboros.Consensus.BlockchainTime (slotLengthFromSec)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types
  ( RelativeTime (..)
  , SystemTime (..)
  , addRelTime
  )
import Ouroboros.Consensus.Config (SecurityParam (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Leios.AnnounceState (newAnnounceState, onValidAnnouncement)
import Ouroboros.Consensus.Leios.NotifyLog
  ( Notification (..)
  , NotifyLog
  , newNotifyLog
  , nextNotifyLog
  , notifyLogStart
  )
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..), OCINStaleness (FreshOCIN))
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlockHash
  , LeiosVote (..)
  , RankingBlockHash
  , toRankingBlockHash
  )
import Ouroboros.Consensus.Leios.VoteState
  ( ElectionVotes (..)
  , TraceLeiosVoteState (..)
  , addVote
  , newVoteState
  , readVotes
  , recordVote
  , seenVote
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( ValidAnnouncement (..)
  , mkAnnouncingHeader
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
  ( AcceptedVote (..)
  , VoteRelayWindow (..)
  )
import Ouroboros.Consensus.Util.IOLike
  ( MonadAsync (..)
  , MonadDelay (..)
  , MonadMonotonicTime (..)
  , STM
  , Time (..)
  , atomically
  , orElse
  )
import Test.Cardano.Crypto.Leios.Gen (TestSeatType (..), mkCommitteeFromTestSeats)
import Test.Consensus.Leios.LeiosNotify.Codec (signedVote)
import Test.Consensus.Leios.TestBlock
  ( LeiosTestBlock (..)
  , LeiosTestLedgerConfig (..)
  , firstLeiosBlock
  , setAnnouncement
  , singleNodeLeiosTestConfig
  )
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
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "VoteState"
    [ testProperty
        "the node keeps the first vote of each seat in every election still worth relaying"
        prop_keepsUnexpired
    , testCase
        "the node relays a vote just once when two peers deliver it just before its window closes"
        $ relayedCopies VerifyingTakesNoTime @?= 1
    , testCase
        "the node relays a vote just once when two peers deliver it and its window closes while the node verifies it"
        $ relayedCopies VerifyingOutlastsWindow @?= 1
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
        st <- newVoteState nullTracer announceState notifyLog
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

-- | How the node verifying a vote in 'relayedCopies' interleaves with the vote's
-- window closing.
data VoteVerificationInterleavingScenarios
  = VerifyingTakesNoTime
  | -- | Long enough for the vote's window to close meanwhile.
    VerifyingOutlastsWindow

-- | How many times the node relays a vote that two peers deliver at once, 0.05 s
-- before its window closes.
relayedCopies :: VoteVerificationInterleavingScenarios -> Int
relayedCopies scenario = runSimOrThrow $ do
  notifyLog <- newNotifyLog
  announceState <- newAnnounceState notifyLog
  st <- newVoteState verifyingTakesTime announceState notifyLog
  atomically $ onValidAnnouncement announceState valid
  threadDelay 8.95
  concurrently_ (addVote st systemTime accepted) (addVote st systemTime accepted)
  length . filter isVote <$> atomically (notifications notifyLog)
 where
  verifyingTakesTime :: MonadDelay m => Tracer m TraceLeiosVoteState
  verifyingTakesTime = case scenario of
    VerifyingTakesNoTime -> nullTracer
    -- The window closes 0.05 s after the vote arrives, so during these 0.1 s.
    VerifyingOutlastsWindow -> mkTracer $ \(TraceLeiosVoteVerified _) -> threadDelay 0.1

  systemTime :: MonadMonotonicTime m => SystemTime m
  systemTime =
    SystemTime
      { systemTimeCurrent = (\(Time t) -> RelativeTime (realToFrac t)) <$> getMonotonicTime
      , systemTimeWait = pure ()
      }

  valid =
    MkValidAnnouncement
      { announcingHeader =
          fromMaybe (error "relayedCopies: an announcement") $
            mkAnnouncingHeader topLevelConfig (ltbHeader announcingBlock)
      , announcementOnset = RelativeTime 1
      , announcementAge = 0
      , announcementStaleness = FreshOCIN
      }

  accepted =
    MkAcceptedVote
      { acceptedVote = MkLeiosVote rbHash (LeiosSeatId 0) (signDSIGN leiosSignContext rbHash sk)
      , voteElectionSlot = blockSlot (ltbHeader announcingBlock)
      , voteCommittee = committee
      , voteRelayWindow = MkVoteRelayWindow (RelativeTime 4) (RelativeTime 9)
      }

  rbHash = toRankingBlockHash (Proxy @LeiosTestBlock) (blockHash (ltbHeader announcingBlock))

  (sk, committee) = case mkCommitteeFromTestSeats [(WithKey, 1)] of
    ([sk'], committee') -> (sk', committee')
    _ -> error "relayedCopies: one seat"

  announcingBlock = setAnnouncement (UnsafeHash (SBS.replicate 32 1)) 1000 (firstLeiosBlock 0)

  isVote = \case
    NotifyVote _ -> True
    NotifyAnnouncement _ -> False

  k = SecurityParam $ knownNonZeroBounded @10

  topLevelConfig =
    singleNodeLeiosTestConfig
      LeiosTestLedgerConfig
        { ltlcHardForkParams =
            HardFork.defaultEraParams k (slotLengthFromSec 1) HardFork.NoPerasEnabled
        , ltlcForecastRange = 100000
        , ltlcLeios =
            MkLeiosLedgerView
              { llvCommittee = committee
              , llvQuorumStakeThreshold = maxBound
              , llvAnnouncementPeriod = Milliseconds32 1000
              , llvVotePeriod = Milliseconds32 4000
              , llvDiffusionPeriod = Milliseconds32 1000
              , llvMaxEbBodySize = 5000
              , llvMaxEbTxsSize = 5000
              }
        }
      k

-- | Every notification in the log.
notifications :: NotifyLog (IOSim s) LeiosTestBlock -> STM (IOSim s) [Notification LeiosTestBlock]
notifications notifyLog = go notifyLogStart
 where
  go cursor =
    (nextNotifyLog notifyLog cursor >>= \(n, cursor') -> (n :) <$> go cursor')
      `orElse` pure []
