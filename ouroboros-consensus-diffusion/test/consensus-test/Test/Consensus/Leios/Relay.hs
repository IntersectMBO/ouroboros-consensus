{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests of the node relaying announcements, against peers run by the Leios
-- node-vs-environment tests.
--
-- Every peer both sends the node a schedule of notifications and records what
-- the node relays to it.
module Test.Consensus.Leios.Relay (tests) where

import Cardano.Ledger.BaseTypes (Milliseconds32 (..), knownNonZeroBounded)
import Cardano.Ledger.State (emptyLeiosCommittee)
import Control.Concurrent.Class.MonadSTM.Strict
  ( STM
  , StrictTQueue
  , StrictTVar
  , atomically
  , modifyTVar
  , newTQueueIO
  , newTVarIO
  , orElse
  , readTQueue
  , readTVarIO
  , writeTQueue
  )
import Control.Monad.Class.MonadAsync (forConcurrently, withAsync)
import Control.Monad.Class.MonadTime.SI (DiffTime, Time (..), getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (nullTracer)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (for_)
import qualified Data.Foldable as Foldable
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Ouroboros.Consensus.Block (Header, SlotNo (..), blockSlot)
import Ouroboros.Consensus.BlockchainTime (slotLengthFromSec)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..))
import Ouroboros.Consensus.Config (SecurityParam (..), TopLevelConfig)
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Leios.AnnounceState
  ( newAnnounceState
  , onValidAnnouncement
  , pruneAnnounceState
  )
import Ouroboros.Consensus.Leios.NotifyLog (newNotifyLog, nextNotifyLog, notifyLogStart)
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..), OCINStaleness (FreshOCIN))
import Ouroboros.Consensus.Leios.Types (EbHash (..), LeiosPoint, LeiosVote)
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client
  ( leiosNotifyClientPeerPipelined
  , toLeiosNotifyClientPeerPipelined
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements as Announcements
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader (ancElectionId)
  , ValidAnnouncement (..)
  , maxAnnouncementAgeRecv
  , mkAnnouncingHeader
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Server
  ( WhetherExcessiveRequests (..)
  , leiosNotifyServerPeerLookahead
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type
  ( LeiosNotify (..)
  , Message (..)
  , byteLimitsLeiosNotify
  , timeLimitsLeiosNotify
  )
import Ouroboros.Consensus.Network.LeiosNotify
  ( leiosNotifyPipelineDepth
  , runLookaheadFixedSenderPeerWithLimits
  )
import Ouroboros.Consensus.Storage.LeiosDB (newLeiosDBInMemory)
import Ouroboros.Network.Channel (Channel)
import Ouroboros.Network.Driver.Limits (runPipelinedPeerWithLimits)
import Ouroboros.Network.Protocol.Limits (ProtocolSizeLimits)
import Test.Consensus.Leios.Environment
  ( EnvPeer (..)
  , LeiosNotifyCodec
  , envLeiosNotifyCodec
  , refusingLeiosFetchServer
  , withNodeVsEnvConnection
  )
import Test.Consensus.Leios.NodeUnderTest
  ( Blk
  , NodeUnderTest
  , PeerAddr (..)
  , defaultNodeUnderTestConfig
  , withNodeUnderTest
  )
import Test.Consensus.Leios.TestBlock
  ( LeiosTestBlock (..)
  , LeiosTestLedgerConfig (..)
  , firstLeiosBlock
  , forkLeiosBlock
  , setAnnouncement
  , setCounterBelowImmTips
  , setSlot
  , singleNodeLeiosTestConfig
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))
import Test.Util.ChainDB (emptyNodeDBs)

tests :: TestTree
tests =
  testGroup
    "Relay"
    [ testCase "the node relays an announcement to a peer" $ do
        down <- relayToOne 0 [[announce someAnnouncingBlock]]
        announcements down @?= [ltbHeader someAnnouncingBlock]
    , testCase "the node relays an announcement it hears from two peers once" $ do
        down <- relayToOne 0 [[announce someAnnouncingBlock], [announce someAnnouncingBlock]]
        announcements down @?= [ltbHeader someAnnouncingBlock]
    , testCase "the node relays both announcements of an election, but no third" $ do
        down <-
          relayToOne
            0
            [[announce someAnnouncingBlock, announce equivocation], [announceAt 1 thirdAnnouncement]]
        announcements down @?= map ltbHeader [someAnnouncingBlock, equivocation]
    , testCase "the node does not relay an announcement too old to relay" $ do
        -- connecting late, since a connection without KeepAlive times out
        (_, downs) <-
          relayPeers
            400
            [PeerSpec 340 [announceAt 350 someAnnouncingBlock, announceAt 350 lateAnnouncingBlock]]
            [340]
        map announcements downs @?= [[ltbHeader lateAnnouncingBlock]]
    , testCase "the node does not relay an announcement of a revoked operational certificate" $ do
        down <- relayToOne 0 [[announce (setCounterBelowImmTips someAnnouncingBlock)]]
        announcements down @?= []
    , testCase "a peer that connects later receives the announcement, until it is too old to relay" $ do
        (_, downs) <- relay 400 [[announce someAnnouncingBlock]] [0, 10, 350]
        map announcements downs @?= [[ltbHeader someAnnouncingBlock], [ltbHeader someAnnouncingBlock], []]
    , testCase "the node does not relay an announcement of an election pruned while validating it" $
        prunedWhileValidating @?= []
    ]

announcements :: PeerOutcome -> [Header Blk]
announcements o = [hdr | (_, NotifyAnnouncement hdr) <- poReceived o]

{-------------------------------------------------------------------------------
  Running a scenario
-------------------------------------------------------------------------------}

-- | What a peer sends the node, or what the node relays to a peer.
data Notification
  = NotifyAnnouncement (Header Blk)
  | -- | an offer or votes, which these tests neither send nor check
    NotifyOther
  deriving Show

-- | A peer: when it connects, and what it sends the node when.
data PeerSpec = PeerSpec
  { psConnectAt :: DiffTime
  , psSends :: [(DiffTime, Notification)]
  }

newtype PeerOutcome = PeerOutcome
  { poReceived :: [(Time, Notification)]
  -- ^ what the node relayed to the peer, and when
  }

-- | Run peers against one node until the given time: upstream peers, which
-- connect at once and send the node their schedules, and downstream peers,
-- which connect at the given times and send nothing. Every peer records what
-- the node relays to it, and the test fails if an honest peer's client would
-- disconnect the node for it.
relay :: DiffTime -> [[(DiffTime, Notification)]] -> [DiffTime] -> IO ([PeerOutcome], [PeerOutcome])
relay end ups = relayPeers end (map (PeerSpec 0) ups)

-- | 'relay', with upstream peers that connect when they say.
relayPeers :: DiffTime -> [PeerSpec] -> [DiffTime] -> IO ([PeerOutcome], [PeerOutcome])
relayPeers end ups downs = do
  for_ (zip [1 :: Int ..] outcomes) $ \(i, o) -> case honestViolations (poReceived o) of
    [] -> pure ()
    violations -> assertFailure $ "peer " <> show i <> ":\n" <> unlines violations
  pure $ splitAt (length ups) outcomes
 where
  specs = ups <> [PeerSpec at [] | at <- downs]
  outcomes = runSimOrThrow $ withNode $ \nut ->
    forConcurrently (zip [1 ..] specs) $ \(i, spec) -> runPeer nut end (PeerAddr i) spec

-- | 'relay' for 20 seconds to one downstream peer connecting at the given time.
relayToOne :: DiffTime -> [[(DiffTime, Notification)]] -> IO PeerOutcome
relayToOne at ups =
  relay 20 ups [at] >>= \case
    (_, [down]) -> pure down
    _ -> error "relayToOne: one downstream peer"

runPeer :: NodeUnderTest (IOSim s) -> DiffTime -> PeerAddr -> PeerSpec -> IOSim s PeerOutcome
runPeer nut end addr PeerSpec{psConnectAt, psSends} = do
  threadDelay psConnectAt
  queue <- newTQueueIO
  received <- newTVarIO []
  let c = envLeiosNotifyCodec nut
      envPeer =
        EnvPeer
          { epLeiosNotifyServer = runFeedingServer c (readTQueue queue)
          , epLeiosNotifyClient = Just $ runRecordingClient c received
          , epLeiosFetchServer = refusingLeiosFetchServer nut
          , epLeiosFetchClient = Nothing
          }
  withAsync (feedAt queue psSends) $ \_feeder ->
    withNodeVsEnvConnection nut addr envPeer $ \_conn -> do
      Time now <- getMonotonicTime
      threadDelay (end - now)
      PeerOutcome . reverse <$> readTVarIO received

-- | Put each notification in the queue at its time since the run began.
feedAt :: StrictTQueue (IOSim s) Notification -> [(DiffTime, Notification)] -> IOSim s ()
feedAt queue = mapM_ $ \(at, n) -> do
  Time now <- getMonotonicTime
  threadDelay (max 0 (at - now))
  atomically $ writeTQueue queue n

-- | The peer's server: each request gets the next notification from the queue.
runFeedingServer ::
  LeiosNotifyCodec s -> STM (IOSim s) Notification -> Channel (IOSim s) BSL.ByteString -> IOSim s ()
runFeedingServer c next chan = do
  peer <- leiosNotifyServerPeerLookahead (pure NotExcessiveRequests) (toMessage <$> next)
  fst
    <$> runLookaheadFixedSenderPeerWithLimits nullTracer c sizeLimits timeLimitsLeiosNotify chan peer
 where
  toMessage :: Notification -> Message (LeiosNotify LeiosPoint (Header Blk) LeiosVote) StBusy StIdle
  toMessage = \case
    NotifyAnnouncement hdr -> MsgLeiosBlockAnnouncement hdr
    NotifyOther -> error "runFeedingServer: the tests send only announcements"

-- | The peer's client: keep the node's server's pipeline full, and record each
-- notification it relays.
runRecordingClient ::
  forall s.
  LeiosNotifyCodec s ->
  StrictTVar (IOSim s) [(Time, Notification)] ->
  Channel (IOSim s) BSL.ByteString ->
  IOSim s ()
runRecordingClient c received chan =
  fst
    <$> runPipelinedPeerWithLimits
      nullTracer
      c
      sizeLimits
      timeLimitsLeiosNotify
      chan
      ( toLeiosNotifyClientPeerPipelined $
          leiosNotifyClientPeerPipelined (pure (Right leiosNotifyPipelineDepth)) (pure record)
      )
 where
  record :: Message (LeiosNotify LeiosPoint (Header Blk) LeiosVote) StBusy StIdle -> IOSim s ()
  record msg = do
    now <- getMonotonicTime
    let n = case msg of
          MsgLeiosBlockAnnouncement hdr -> NotifyAnnouncement hdr
          _ -> NotifyOther
    atomically $ modifyTVar received ((now, n) :)

sizeLimits :: ProtocolSizeLimits (LeiosNotify point announcement vote) BSL.ByteString
sizeLimits = byteLimitsLeiosNotify (fromIntegral . BSL.length)

withNode :: (NodeUnderTest (IOSim s) -> IOSim s a) -> IOSim s a
withNode body = do
  nodeDBs <- emptyNodeDBs
  leiosDB <- newLeiosDBInMemory
  withNodeUnderTest (defaultNodeUnderTestConfig k ledgerConfig) nodeDBs leiosDB body

k :: SecurityParam
k = SecurityParam $ knownNonZeroBounded @10

-- | One-second slots, the first starting when the run begins.
ledgerConfig :: LeiosTestLedgerConfig
ledgerConfig =
  LeiosTestLedgerConfig
    { ltlcHardForkParams =
        HardFork.defaultEraParams k (slotLengthFromSec 1) HardFork.NoPerasEnabled
    , ltlcForecastRange = 100000
    , ltlcLeios =
        MkLeiosLedgerView
          { llvCommittee = emptyLeiosCommittee
          , llvQuorumStakeThreshold = maxBound
          , llvAnnouncementPeriod = Milliseconds32 1000
          , llvVotePeriod = Milliseconds32 4000
          , llvDiffusionPeriod = Milliseconds32 1000
          , llvMaxEbBodySize = 5000
          , llvMaxEbTxsSize = 5000
          }
    }

{-------------------------------------------------------------------------------
  Checking what a peer receives
-------------------------------------------------------------------------------}

-- | Why an honest peer's client would disconnect the node for this stream, if
-- it would: an announcement must be neither a repeat nor the third of its
-- election, nor older than 'maxAnnouncementAgeRecv'.
honestViolations :: [(Time, Notification)] -> [String]
honestViolations = snd . Foldable.foldl' step (Announcements.empty, [])
 where
  step (seen, errs) (at, n) = case n of
    NotifyAnnouncement hdr -> case mkAnnouncingHeader topLevelConfig hdr of
      Nothing -> (seen, errs <> ["not an announcement: " <> show hdr])
      Just anc -> case Announcements.insert @_ @Void (ancElectionId anc) anc seen of
        Left err -> (seen, errs <> [show err <> ": " <> show hdr])
        Right seen' ->
          ( seen'
          , errs
              <> [ "too old, at " <> show at <> ": " <> show hdr
                 | toSeconds at - slotStart (blockSlot hdr) > realToFrac maxAnnouncementAgeRecv
                 ]
          )
    NotifyOther -> (seen, errs)

  toSeconds (Time t) = realToFrac t :: Double
  slotStart (SlotNo s) = fromIntegral s -- one-second slots

topLevelConfig :: TopLevelConfig Blk
topLevelConfig = singleNodeLeiosTestConfig ledgerConfig k

{-------------------------------------------------------------------------------
  The node's announcement state on its own
-------------------------------------------------------------------------------}

-- | What the node relays of a valid announcement of an election that its
-- announcement state pruned while the announcement was validated.
prunedWhileValidating :: [()]
prunedWhileValidating = runSimOrThrow $ do
  notifyLog <- newNotifyLog
  st <- newAnnounceState notifyLog
  atomically $ pruneAnnounceState st (blockSlot someAnnouncingBlock + 1)
  atomically $ onValidAnnouncement st valid
  atomically $ ([()] <$ nextNotifyLog notifyLog notifyLogStart) `orElse` pure []
 where
  valid =
    MkValidAnnouncement
      { announcingHeader =
          fromMaybe (error "prunedWhileValidating: an announcement") $
            mkAnnouncingHeader topLevelConfig (ltbHeader someAnnouncingBlock)
      , announcementOnset = RelativeTime 1
      , announcementAge = 0
      , announcementStaleness = FreshOCIN
      }

{-------------------------------------------------------------------------------
  Fixtures
-------------------------------------------------------------------------------}

announce :: LeiosTestBlock -> (DiffTime, Notification)
announce = announceAt 0

announceAt :: DiffTime -> LeiosTestBlock -> (DiffTime, Notification)
announceAt at blk = (at, NotifyAnnouncement (ltbHeader blk))

-- | Announces an endorser block in slot 1.
someAnnouncingBlock :: LeiosTestBlock
someAnnouncingBlock = setAnnouncement (MkEbHash (BS.replicate 32 1)) 1000 (firstLeiosBlock 0)

-- | Another announcement in the election of 'someAnnouncingBlock'.
equivocation :: LeiosTestBlock
equivocation = setAnnouncement (MkEbHash (BS.replicate 32 2)) 1000 (forkLeiosBlock (firstLeiosBlock 0))

-- | A third announcement in the election of 'someAnnouncingBlock'.
thirdAnnouncement :: LeiosTestBlock
thirdAnnouncement =
  setAnnouncement
    (MkEbHash (BS.replicate 32 3))
    1000
    (forkLeiosBlock (forkLeiosBlock (firstLeiosBlock 0)))

-- | Announces an endorser block in slot 349.
lateAnnouncingBlock :: LeiosTestBlock
lateAnnouncingBlock = setAnnouncement (MkEbHash (BS.replicate 32 4)) 1000 (setSlot 349 (firstLeiosBlock 1))
