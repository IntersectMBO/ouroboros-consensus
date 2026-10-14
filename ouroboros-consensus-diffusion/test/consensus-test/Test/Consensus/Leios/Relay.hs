{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests of the node relaying announcements and votes, against peers run by
-- the Leios node-vs-environment tests.
--
-- Every peer both sends the node a schedule of notifications and records what
-- the node relays to it. The node relays a vote only once it has checked it
-- against the committee, and only after relaying the announcement it is for,
-- since a peer disconnects a peer that votes for an announcement it never sent.
module Test.Consensus.Leios.Relay (tests) where

import Cardano.Crypto.DSIGN.Class (signDSIGN, verifyDSIGN)
import Cardano.Crypto.Leios
  ( LeiosCommittee
  , LeiosSeat (..)
  , LeiosSeatId (..)
  , LeiosSigningKey
  , leiosCommitteeSize
  , resolveLeiosSeat
  )
import Cardano.Ledger.BaseTypes (Milliseconds32 (..), knownNonZeroBounded)
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
import Data.Either (isRight)
import Data.Foldable (for_, toList)
import qualified Data.Foldable as Foldable
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Word (Word16)
import Ouroboros.Consensus.Block (ConvertRawHash (..), Header, SlotNo (..), blockHash, blockSlot)
import Ouroboros.Consensus.BlockchainTime (slotLengthFromSec)
import Ouroboros.Consensus.Config (SecurityParam (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..))
import Ouroboros.Consensus.Leios.Types
  ( EbHash (..)
  , LeiosPoint
  , LeiosVote (..)
  , RbHash (..)
  , milliseconds32ToNominalDiffTime
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client
  ( leiosNotifyClientPeerPipelined
  , toLeiosNotifyClientPeerPipelined
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State (voteLateGrace)
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
  ( leiosNotifyMiniProtocolNum
  , leiosNotifyPipelineDepth
  , runLookaheadFixedSenderPeerWithLimits
  )
import Ouroboros.Network.Channel (Channel)
import Ouroboros.Network.Driver.Limits (runPipelinedPeerWithLimits)
import Ouroboros.Network.Protocol.Limits (ProtocolSizeLimits)
import Test.Cardano.Crypto.Leios.Gen (TestSeatType (..), mkCommitteeFromTestSeats)
import Test.Consensus.Leios.Environment
  ( EnvPeer (..)
  , LeiosNotifyCodec
  , NodeVsEnvConnection (..)
  , envLeiosNotifyCodec
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
  , setSlot
  )
import Test.QuickCheck
  ( Gen
  , Property
  , choose
  , conjoin
  , counterexample
  , elements
  , forAllShow
  , property
  , sublistOf
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.ChainDB (emptyNodeDBs)

tests :: TestTree
tests =
  testGroup
    "Relay"
    [ testCase "the node relays an announcement to a peer" $ do
        let down = relayToOne 0 [[announce someAnnouncingBlock]]
        announcements down @?= [ltbHeader someAnnouncingBlock]
    , testCase "the node relays an announcement it hears from two peers once" $ do
        let down = relayToOne 0 [[announce someAnnouncingBlock], [announce someAnnouncingBlock]]
        announcements down @?= [ltbHeader someAnnouncingBlock]
    , testCase "the node relays both announcements of an election, but no third" $ do
        let down =
              relayToOne
                0
                [[announce someAnnouncingBlock, announce equivocation], [announceAt 1 thirdAnnouncement]]
        announcements down @?= map ltbHeader [someAnnouncingBlock, equivocation]
    , testCase "the node does not relay an announcement too old to relay" $ do
        -- connecting late, since a connection without KeepAlive times out
        let (_, downs) =
              relayPeers
                400
                [PeerSpec 340 [announceAt 350 someAnnouncingBlock, announceAt 350 lateAnnouncingBlock]]
                [340]
        map announcements downs @?= [[ltbHeader lateAnnouncingBlock]]
    , testCase "a peer that connects later receives the announcement, until it is too old to relay" $ do
        let (_, downs) = relay 400 [[announce someAnnouncingBlock]] [0, 10, 350]
        map announcements downs @?= [[ltbHeader someAnnouncingBlock], [ltbHeader someAnnouncingBlock], []]
    , testCase "the node relays a vote to every other peer, after the announcement it is for" $ do
        let (_, downs) = relay 20 [votedFor0] [0, 0]
        for_ downs $ \down -> do
          votes down @?= [voteOf someAnnouncingBlock 0]
          assertHonest down
    , testCase "the node relays a vote promptly" $ do
        let down = relayToOne 0 [votedFor0]
        assertBool ("relayed at " <> show (voteTimes down)) $
          not (null (voteTimes down)) && all (<= Time 6) (voteTimes down)
    , testCase "the node relays a vote it hears from two peers once" $ do
        let down = relayToOne 0 [votedFor0, [announce someAnnouncingBlock, (5.5, vote someAnnouncingBlock 0)]]
        votes down @?= [voteOf someAnnouncingBlock 0]
        assertHonest down
    , testCase "the node relays only the first vote of a seat in an election" $ do
        let down = relayToOne 0 [votedFor0, [announce equivocation, (5.5, vote equivocation 0)]]
        votes down @?= [voteOf someAnnouncingBlock 0]
        assertHonest down
    , testCase "the node does not relay a badly signed vote, nor disconnect the peer that sent it" $ do
        let (ups, downs) = relay 20 [withControl [(5, badlySignedVote someAnnouncingBlock 0)]] [0]
        concatMap votes downs @?= [control]
        map poNodeClientEnded ups @?= [Nothing]
    , testCase "the node does not relay a vote from a seat outside the committee" $ do
        let down = relayToOne 0 [withControl [(5, badlySignedVote someAnnouncingBlock seatOutside)]]
        votes down @?= [control]
    , testCase "the node does not relay a vote from a seat without a key" $ do
        let down =
              relayToOne
                0
                [withControl [(5, vote someAnnouncingBlock seatNoKey), (5, vote someAnnouncingBlock seatBadPoP)]]
        votes down @?= [control]
    , testCase "a peer that connects during a vote's window receives the vote" $ do
        let down = relayToOne 6 [votedFor0]
        votes down @?= [voteOf someAnnouncingBlock 0]
        assertHonest down
    , testCase "a peer that connects once a peer would reject the vote does not receive it" $ do
        let (_, downs) = relay 100 [votedFor0] [0, 90]
        map votes downs @?= [[voteOf someAnnouncingBlock 0], []]
    , testProperty
        "the node relays exactly the first valid vote of each seat, and only what a peer accepts"
        $ forAllShow genVotingPeers showVotingPeers prop_relay
    ]
 where
  -- seat 0 votes for 'someAnnouncingBlock' 5 seconds in, a second into its
  -- election's window
  votedFor0 = [announce someAnnouncingBlock, (5, vote someAnnouncingBlock 0)]

  -- A valid vote sent alongside the ones a test expects the node to hold back,
  -- so that the test cannot pass merely because the node relays nothing.
  withControl sends = [announce someAnnouncingBlock] <> sends <> [(6, vote someAnnouncingBlock 1)]
  control = voteOf someAnnouncingBlock 1

{-------------------------------------------------------------------------------
  The property
-------------------------------------------------------------------------------}

-- | Each upstream peer announces, then votes, as the node's client requires of
-- it: only for announcements it sent, at most once per seat in an election,
-- within the window. A last peer only listens.
genVotingPeers :: Gen [[(DiffTime, Notification)]]
genVotingPeers = do
  n <- choose (1, 3)
  sequence (replicate n genUpstream)
 where
  genUpstream = do
    blks <- elements [[someAnnouncingBlock], [equivocation], [someAnnouncingBlock, equivocation]]
    -- both blocks are in one election, so a seat votes for one of them
    seats <- sublistOf [0 .. seatOutside]
    vs <- mapM (genVote blks) seats
    pure $ map announce blks <> sortOn fst vs
  genVote blks seat = do
    blk <- elements blks
    at <- realToFrac <$> choose (4.1 :: Double, 7.9)
    -- a seat outside the committee has no key to sign with
    signed <- if seat < seatOutside then elements [vote, badlySignedVote] else pure badlySignedVote
    pure (at, signed blk seat)

showVotingPeers :: [[(DiffTime, Notification)]] -> String
showVotingPeers = unlines . map (unwords . map showSend)
 where
  showSend (at, n) = show at <> ":" <> showNotification n

prop_relay :: [[(DiffTime, Notification)]] -> Property
prop_relay ups =
  conjoin $
    [ poNodeClientEnded up === Nothing
    | up <- upOutcomes
    ]
      <> [ counterexample ("peer " <> show i) $ honest (poReceived o)
         | (i, o) <- zip [1 :: Int ..] outcomes
         ]
      <> [ counterexample "relayed votes" $
             Set.fromList [electionSeat v | v <- votes down] === firstValid
         ]
 where
  (upOutcomes, downOutcomes) = relay 20 ups [0]
  outcomes = upOutcomes <> downOutcomes
  down = head downOutcomes

  -- every election and seat of which some peer sent a valid vote
  firstValid =
    Set.fromList
      [ electionSeat v
      | sends <- ups
      , (_, NotifyVotes vs) <- sends
      , v <- toList vs
      , validVote v
      ]

  electionSeat v = (announcedSlot Map.! voteAnnouncingRbHash v, leiosSeatIndex (voteSeatId v))

  announcedSlot =
    Map.fromList
      [ (rbHashOf hdr, blockSlot hdr)
      | sends <- ups
      , (_, NotifyAnnouncement hdr) <- sends
      ]

{-------------------------------------------------------------------------------
  Checking what a peer receives
-------------------------------------------------------------------------------}

-- | Why an honest peer's client would disconnect the node for this stream, if
-- it would: a vote must follow the announcement it is for, be the first of its
-- seat in that election, verify against the committee, and
-- arrive within its window.
honest :: [(Time, Notification)] -> Property
honest received = case honestViolations received of
  [] -> property True
  violations -> counterexample (unlines violations) False

honestViolations :: [(Time, Notification)] -> [String]
honestViolations = snd . Foldable.foldl' step ((Map.empty, Set.empty), [])
 where
  step ((announced, voted), errs) (at, n) = case n of
    NotifyAnnouncement hdr ->
      ((Map.insert (rbHashOf hdr) (blockSlot hdr) announced, voted), errs)
    NotifyVotes vs -> Foldable.foldl' (checkVote at) ((announced, voted), errs) vs
    NotifyOther -> ((announced, voted), errs)

  checkVote at ((announced, voted), errs) v = case Map.lookup (voteAnnouncingRbHash v) announced of
    Nothing -> ((announced, voted), errs <> ["a vote for an announcement not relayed before: " <> show v])
    Just slot ->
      ( (announced, Set.insert (slot, seat) voted)
      , errs
          <> [ "a second vote of seat " <> show seat <> " in slot " <> show slot
             | Set.member (slot, seat) voted
             ]
          <> ["an invalid vote: " <> show v | not (validVote v)]
          <> [ "a vote outside its window, at " <> show at
             | let age = toSeconds at - slotStart slot
             , age < 3 * lHdr || age > 3 * lHdr + lVote + grace
             ]
      )
   where
    seat = leiosSeatIndex (voteSeatId v)

  toSeconds (Time t) = realToFrac t :: Double
  slotStart (SlotNo s) = fromIntegral s -- one-second slots
  lHdr = realToFrac (milliseconds32ToNominalDiffTime (llvAnnouncementPeriod leiosLedgerView))
  lVote = realToFrac (milliseconds32ToNominalDiffTime (llvVotePeriod leiosLedgerView))
  grace = realToFrac voteLateGrace

assertHonest :: PeerOutcome -> IO ()
assertHonest o = case honestViolations (poReceived o) of
  [] -> pure ()
  violations -> assertBool (unlines violations) False

-- | Whether the vote's seat has a key in the committee, and its signature
-- verifies under it.
validVote :: LeiosVote -> Bool
validVote MkLeiosVote{voteAnnouncingRbHash, voteSeatId, voteSignature} =
  case resolveLeiosSeat committee voteSeatId of
    Just LeiosSeat{seatVKey = SJust vk} ->
      isRight $ verifyDSIGN () vk voteAnnouncingRbHash voteSignature
    _ -> False

announcements :: PeerOutcome -> [Header Blk]
announcements o = [hdr | (_, NotifyAnnouncement hdr) <- poReceived o]

votes :: PeerOutcome -> [LeiosVote]
votes o = concat [toList vs | (_, NotifyVotes vs) <- poReceived o]

-- | The vote of this seat for the announcement this block carries.
voteOf :: LeiosTestBlock -> Word16 -> LeiosVote
voteOf blk seat = signedBy (seatKeys !! fromIntegral seat) blk seat

voteTimes :: PeerOutcome -> [Time]
voteTimes o = [at | (at, NotifyVotes _) <- poReceived o]

{-------------------------------------------------------------------------------
  Running a scenario
-------------------------------------------------------------------------------}

-- | What a peer sends the node, or what the node relays to a peer.
data Notification
  = NotifyAnnouncement (Header Blk)
  | NotifyVotes (NonEmpty LeiosVote)
  | -- | an offer, which these tests neither send nor check
    NotifyOther
  deriving Show

showNotification :: Notification -> String
showNotification = \case
  NotifyAnnouncement hdr -> "announce@" <> show (blockSlot hdr)
  NotifyVotes vs -> "votes" <> show [(leiosSeatIndex (voteSeatId v), validVote v) | v <- toList vs]
  NotifyOther -> "other"

-- | A peer: when it connects, and what it sends the node when.
data PeerSpec = PeerSpec
  { psConnectAt :: DiffTime
  , psSends :: [(DiffTime, Notification)]
  }

data PeerOutcome = PeerOutcome
  { poReceived :: [(Time, Notification)]
  -- ^ what the node relayed to the peer, and when
  , poNodeClientEnded :: Maybe String
  -- ^ how the node's client of this peer ended, if it did
  }

-- | Run peers against one node until the given time: upstream peers, which
-- connect at once and send the node their schedules, and downstream peers,
-- which connect at the given times and send nothing. Every peer records what
-- the node relays to it.
relay :: DiffTime -> [[(DiffTime, Notification)]] -> [DiffTime] -> ([PeerOutcome], [PeerOutcome])
relay end ups = relayPeers end (map (PeerSpec 0) ups)

-- | 'relay', with upstream peers that connect when they say.
relayPeers :: DiffTime -> [PeerSpec] -> [DiffTime] -> ([PeerOutcome], [PeerOutcome])
relayPeers end ups downs = splitAt (length ups) $ runSimOrThrow $ withVotingNode $ \nut ->
  forConcurrently (zip [1 ..] specs) $ \(i, spec) -> runPeer nut end (PeerAddr i) spec
 where
  specs = ups <> [PeerSpec at [] | at <- downs]

-- | 'relay' for 20 seconds, past every vote's window, to one downstream peer
-- connecting at the given time.
relayToOne :: DiffTime -> [[(DiffTime, Notification)]] -> PeerOutcome
relayToOne at ups = case relay 20 ups [at] of
  (_, [down]) -> down
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
          }
  withAsync (feedAt queue psSends) $ \_feeder ->
    withNodeVsEnvConnection nut addr envPeer $ \conn -> do
      Time now <- getMonotonicTime
      threadDelay (end - now)
      ended <- case Map.lookup leiosNotifyMiniProtocolNum (connNodeInitiators conn) of
        Nothing -> pure (Just "the node runs no LeiosNotify client")
        Just client -> atomically $ (Just . either show (const "ended") <$> client) `orElse` pure Nothing
      PeerOutcome <$> (reverse <$> readTVarIO received) <*> pure ended

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
    NotifyVotes vs -> MsgLeiosVotes vs
    NotifyOther -> error "runFeedingServer: the tests send no offers"

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
          MsgLeiosVotes vs -> NotifyVotes vs
          _ -> NotifyOther
    atomically $ modifyTVar received ((now, n) :)

sizeLimits :: ProtocolSizeLimits (LeiosNotify point announcement vote) BSL.ByteString
sizeLimits = byteLimitsLeiosNotify (fromIntegral . BSL.length)

-- | A node of the committee 'committee', with the periods of 'leiosLedgerView'.
withVotingNode :: (NodeUnderTest (IOSim s) -> IOSim s a) -> IOSim s a
withVotingNode body = do
  nodeDBs <- emptyNodeDBs
  withNodeUnderTest cfg nodeDBs body
 where
  k = SecurityParam $ knownNonZeroBounded @10
  cfg =
    defaultNodeUnderTestConfig
      k
      LeiosTestLedgerConfig
        { ltlcHardForkParams =
            HardFork.defaultEraParams k (slotLengthFromSec 1) HardFork.NoPerasEnabled
        , ltlcForecastRange = 100000
        , ltlcLeios = leiosLedgerView
        }

{-------------------------------------------------------------------------------
  Fixtures
-------------------------------------------------------------------------------}

-- | The committee 'committee'. A vote is relayed between 3 s and 7 s after its
-- election slot began.
leiosLedgerView :: LeiosLedgerView
leiosLedgerView =
  MkLeiosLedgerView
    { llvCommittee = committee
    , llvQuorumStakeThreshold = maxBound
    , llvAnnouncementPeriod = Milliseconds32 1000
    , llvVotePeriod = Milliseconds32 4000
    , llvDiffusionPeriod = Milliseconds32 1000
    , llvMaxEbBodySize = 5000
    , llvMaxEbTxsSize = 5000
    }

-- | Three seats with keys, then one without a key and one whose key's proof of
-- possession does not verify.
seatKeys :: [LeiosSigningKey]
committee :: LeiosCommittee
(seatKeys, committee) =
  mkCommitteeFromTestSeats [(kind, 1 % 5) | kind <- [WithKey, WithKey, WithKey, NoKey, BadPoP]]

seatNoKey, seatBadPoP, seatOutside :: Word16
seatNoKey = 3
seatBadPoP = 4
seatOutside = fromIntegral (leiosCommitteeSize committee)

-- | The vote of this seat for the announcement this block carries, signed with
-- the seat's key.
vote :: LeiosTestBlock -> Word16 -> Notification
vote blk seat = NotifyVotes (voteOf blk seat :| [])

-- | As 'vote', but signed with a key of no seat.
badlySignedVote :: LeiosTestBlock -> Word16 -> Notification
badlySignedVote blk seat = NotifyVotes (signedBy (last seatKeys') blk seat :| [])
 where
  (seatKeys', _) = mkCommitteeFromTestSeats [(WithKey, 1) | _ <- [0 .. leiosCommitteeSize committee + 1]]

signedBy :: LeiosSigningKey -> LeiosTestBlock -> Word16 -> LeiosVote
signedBy sk blk seat =
  MkLeiosVote
    { voteAnnouncingRbHash = rbHash
    , voteSeatId = LeiosSeatId seat
    , voteSignature = signDSIGN () rbHash sk
    }
 where
  rbHash = rbHashOf (ltbHeader blk)

announce :: LeiosTestBlock -> (DiffTime, Notification)
announce = announceAt 0

announceAt :: DiffTime -> LeiosTestBlock -> (DiffTime, Notification)
announceAt at blk = (at, NotifyAnnouncement (ltbHeader blk))

rbHashOf :: Header Blk -> RbHash
rbHashOf = MkRbHash . toRawHash (Proxy @LeiosTestBlock) . blockHash

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
