{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Properties of how the node kernel runs its components.
module Test.Consensus.NodeKernel (tests) where

import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Control.Monad.Class.MonadAsync (wait, withAsync)
import Control.Monad.Class.MonadTest (MonadTest (..))
import Control.Monad.Class.MonadTime.SI (Time (..), getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Control.Monad.IOSim (IOSim, exploreSimTrace, traceResult)
import Control.Tracer (nullTracer)
import qualified Data.ByteString.Short as SBS
import Ouroboros.Consensus.Block (SlotNo (..))
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..), SystemTime (..))
import Ouroboros.Consensus.Leios.AnnounceState (newAnnounceState)
import Ouroboros.Consensus.Leios.NotifyLog
  ( Notification (NotifyVote)
  , appendNotifyLog
  , newNotifyLog
  , nextNotifyLog
  , notifyLogStart
  )
import Ouroboros.Consensus.Leios.Types (RankingBlockHash (..))
import Ouroboros.Consensus.Leios.VoteState (ValidatedVote (..), newVoteState, recordVote)
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( defaultLeiosAnnouncementAgeLimits
  , maxAnnouncementAgeRecv
  , maxAnnouncementAgeSend
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State (voteLateGrace)
import Ouroboros.Consensus.NodeKernel (leiosDeadlinePruner, leiosDeadlinePrunerInterval)
import Ouroboros.Consensus.Util.IOLike (atomically, orElse)
import Ouroboros.Consensus.Util.Time (nominalDelay)
import Test.Consensus.Leios.LeiosNotify.Codec (signedVote)
import Test.Consensus.Leios.TestBlock (LeiosTestBlock)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Property, conjoin, counterexample, once, testProperty)
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "NodeKernel"
    [ testProperty
        "the deadline pruner runs far more often than a downstream peer's grace period"
        prop_leiosDeadlinePrunerInterval
    , testProperty
        "the node relays every vote it keeps, however the vote races the deadline pruner"
        prop_leiosDeadlinePrunerRace
    ]

-- | The node relays a notification at most 'leiosDeadlinePrunerInterval' after it
-- is no longer worth relaying, which must leave most of a downstream peer's
-- grace period for transmission and queueing: 'voteLateGrace' for a vote, and
-- the margin between 'maxAnnouncementAgeSend' and 'maxAnnouncementAgeRecv' of
-- 'defaultLeiosAnnouncementAgeLimits' for an announcement.
prop_leiosDeadlinePrunerInterval :: Property
prop_leiosDeadlinePrunerInterval =
  once . conjoin $
    [ counterexample "the vote grace period" $
        leiosDeadlinePrunerInterval `farBelow` voteLateGrace
    , counterexample "the announcement grace period" $
        leiosDeadlinePrunerInterval
          `farBelow` ( maxAnnouncementAgeRecv defaultLeiosAnnouncementAgeLimits
                         - maxAnnouncementAgeSend defaultLeiosAnnouncementAgeLimits
                     )
    ]
 where
  -- Leaving at least 90% of the grace period to transmission and queueing.
  farBelow a b = 10 * a <= b

-- | A vote past its deadline, recorded and relayed as 'addVote' does just as the
-- deadline pruner runs, is either rejected or relayed, in every interleaving. So
-- the pruner must prune the votes before the notify log.
prop_leiosDeadlinePrunerRace :: Property
prop_leiosDeadlinePrunerRace =
  once $ exploreSimTrace id (exploreRaces *> raceVote) $ \_ tr ->
    case traceResult False tr of
      Right relayed -> counterexample "kept a vote it does not relay" relayed
      Left e -> counterexample (show e) False

-- | Whether the node relays the vote if it keeps it.
raceVote :: IOSim s Bool
raceVote = do
  notifyLog <- newNotifyLog @_ @LeiosTestBlock
  announceState <- newAnnounceState defaultLeiosAnnouncementAgeLimits notifyLog
  st <- newVoteState nullTracer announceState notifyLog
  withAsync (leiosDeadlinePruner systemTime notifyLog st) $ \_ ->
    withAsync (addAtPrunerRun st notifyLog) $ \adding -> do
      -- Let the pruner run whichever way the vote raced it.
      threadDelay (2 * nominalDelay leiosDeadlinePrunerInterval)
      wait adding
 where
  systemTime =
    SystemTime
      { systemTimeCurrent = (\(Time t) -> RelativeTime (realToFrac t)) <$> getMonotonicTime
      , systemTimeWait = pure ()
      }

  -- When the pruner runs the second time, record the vote and relay it, and
  -- say whether the node relays it if it keeps it.
  addAtPrunerRun st notifyLog = do
    threadDelay (nominalDelay leiosDeadlinePrunerInterval)
    atomically $
      recordVote st validated >>= \case
        Left _ -> pure True
        Right _ -> do
          appendNotifyLog notifyLog (NotifyVote vote) deadline
          (True <$ nextNotifyLog notifyLog notifyLogStart) `orElse` pure False

  vote = signedVote (MkRankingBlockHash (UnsafeHash (SBS.replicate 32 0))) 0

  -- Between the pruner's first run and its second.
  deadline = RelativeTime (leiosDeadlinePrunerInterval / 2)

  validated =
    MkValidatedVote
      { validatedVote = vote
      , validatedElectionSlot = SlotNo 0
      , validatedSeatWeight = 0
      , validatedQuorum = 1
      , voteValidUntil = deadline
      }
