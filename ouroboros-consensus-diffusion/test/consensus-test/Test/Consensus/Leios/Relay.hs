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
import Cardano.Crypto.Hash (Hash (UnsafeHash))
import Cardano.Crypto.Leios
  ( LeiosCert
  , LeiosCommittee
  , LeiosSeat (..)
  , LeiosSeatId (..)
  , LeiosSigningKey
  , Weight
  , leiosCommitteeSize
  , leiosSignContext
  , resolveLeiosSeat
  , verifyLeiosCert
  )
import Cardano.Ledger.BaseTypes (Milliseconds32 (..), boundRational, knownNonZeroBounded)
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
import Control.Monad (replicateM)
import Control.Monad.Class.MonadAsync (forConcurrently, withAsync)
import Control.Monad.Class.MonadTime.SI (DiffTime, Time (..), getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (mkTracer, nullTracer)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Short as SBS
import Data.Either (isRight)
import Data.Foldable (for_, toList)
import qualified Data.Foldable as Foldable
import Data.List (isInfixOf, mapAccumL, sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Maybe.Strict (StrictMaybe (..))
import Data.Proxy (Proxy (..))
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Time.Clock (NominalDiffTime)
import Data.Void (Void)
import Data.Word (Word16)
import Ouroboros.Consensus.Block (Header, SlotNo (..), blockHash, blockSlot)
import Ouroboros.Consensus.BlockchainTime (slotLengthFromSec)
import Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..), addRelTime)
import Ouroboros.Consensus.Config (SecurityParam (..), TopLevelConfig)
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Leios.AnnounceState
  ( newAnnounceState
  , onValidAnnouncement
  , pruneAnnounceState
  )
import Ouroboros.Consensus.Leios.NotifyLog
  ( NotifyLog
  , appendNotifyLog
  , newNotifyLog
  , nextNotifyLog
  , notifyLogPruneCadence
  , notifyLogStart
  , pruneNotifyLog
  )
import qualified Ouroboros.Consensus.Leios.NotifyLog as NotifyLog
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..), OCINStaleness (FreshOCIN))
import Ouroboros.Consensus.Leios.Types
  ( LeiosPoint
  , LeiosVote (..)
  , RankingBlockHash
  , milliseconds32ToNominalDiffTime
  , toRankingBlockHash
  )
import Ouroboros.Consensus.Leios.VoteState (TraceLeiosVoteState (..), getCertificate)
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client
  ( leiosNotifyClientPeerPipelined
  , toLeiosNotifyClientPeerPipelined
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements as Announcements
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.Announcements.Validation
  ( AnnouncingHeader (ancElectionId)
  , ValidAnnouncement (..)
  , maxAnnouncementAgeRecv
  , maxAnnouncementAgeSend
  , mkAnnouncingHeader
  )
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client.State
  ( VoteWindow (..)
  , leiosVoteWindow
  , voteLateGrace
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
  ( leiosNotifyMiniProtocolNum
  , leiosNotifyPipelineDepth
  , runLookaheadFixedSenderPeerWithLimits
  )
import Ouroboros.Consensus.Node.Tracers (Tracers' (..), nullTracers)
import Ouroboros.Consensus.NodeKernel (NodeKernel (..))
import Ouroboros.Network.Channel (Channel)
import Ouroboros.Network.Driver.Limits (runPipelinedPeerWithLimits)
import Ouroboros.Network.Protocol.Limits (ProtocolSizeLimits)
import Test.Cardano.Crypto.Leios.Gen (TestSeatType (..), mkCommitteeFromTestSeats)
import Test.Consensus.Leios.Environment
  ( EnvPeer (..)
  , LeiosNotifyCodec
  , NodeVsEnvConnection (..)
  , envLeiosNotifyCodec
  , refusingLeiosFetchServer
  , withNodeVsEnvConnection
  )
import Test.Consensus.Leios.NodeUnderTest
  ( Blk
  , NodeUnderTest (..)
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
import Test.QuickCheck
  ( Gen
  , Property
  , choose
  , conjoin
  , counterexample
  , cover
  , elements
  , forAllShow
  , frequency
  , once
  , oneof
  , property
  , sublistOf
  , (===)
  )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty)
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
    , testCase "a vote is relayed no longer than its announcement, however long its diffusion period" $
        voteUntil
          (leiosVoteWindow leiosLedgerView{llvDiffusionPeriod = Milliseconds32 1000000} (RelativeTime 1))
          @?= addRelTime maxAnnouncementAgeSend (RelativeTime 1)
    , testCase "the log prunes each notification once it is no longer worth relaying" $
        notifyLogAt [(9, 'a'), (300, 'b'), (5, 'c')] 6 @?= "ab"
    , testCase "the log does not take a notification it was pruned past already" $
        notifyLogPrunedAt 6 [(9, 'a'), (5, 'c'), (6, 'd')] @?= "ad"
    , testProperty
        "the log is pruned far more often than a downstream peer's grace"
        prop_notifyLogPruneCadence
    , testCase "the node relays a vote to every other peer, after the announcement it is for" $ do
        (_, downs) <- relay 20 [votedFor0] [0, 0]
        for_ downs $ \down -> votes down @?= [voteOf someAnnouncingBlock 0]
    , testCase "the node relays a vote promptly" $ do
        down <- relayToOne 0 [votedFor0]
        assertBool ("relayed at " <> show (voteTimes down)) $
          not (null (voteTimes down)) && all (<= Time 6) (voteTimes down)
    , testCase "the node relays a vote it hears from two peers once" $ do
        down <- relayToOne 0 [votedFor0, [announce someAnnouncingBlock, (5.5, vote someAnnouncingBlock 0)]]
        votes down @?= [voteOf someAnnouncingBlock 0]
    , testCase "the node relays only the first vote of a seat in an election" $ do
        down <- relayToOne 0 [votedFor0, [announce equivocation, (5.5, vote equivocation 0)]]
        votes down @?= [voteOf someAnnouncingBlock 0]
    , testCase "the node disconnects a peer that sends a badly signed vote, and does not relay it" $ do
        (ups, downs) <- relay 20 (withControl [[(5, badlySignedVote someAnnouncingBlock 0)]]) [0]
        concatMap votes downs @?= [control]
        map invalidVoteDisconnected ups @?= [True, False]
    , testCase "the node disconnects a peer that sends a vote from a seat outside the committee" $ do
        (ups, downs) <-
          relay 20 (withControl [[(5, badlySignedVote someAnnouncingBlock seatOutside)]]) [0]
        concatMap votes downs @?= [control]
        map invalidVoteDisconnected ups @?= [True, False]
    , testCase "the node disconnects a peer that sends a vote from a seat without a key" $ do
        (ups, downs) <-
          relay
            20
            ( withControl
                [[(5, vote someAnnouncingBlock seatNoKey)], [(5, vote someAnnouncingBlock seatBadPoP)]]
            )
            [0]
        concatMap votes downs @?= [control]
        map invalidVoteDisconnected ups @?= [True, True, False]
    , testCase "the node does not verify a vote of a seat it already has a vote of" $ do
        (ups, downs) <-
          relay
            20
            [votedFor0, [announce someAnnouncingBlock, (5.5, badlySignedVote someAnnouncingBlock 0)]]
            [0]
        concatMap votes downs @?= [voteOf someAnnouncingBlock 0]
        map poNodeClientEnded ups @?= [Nothing, Nothing]
    , testCase "the node does not relay a vote for an announcement it did not relay" $ do
        down <-
          relayToOne
            0
            ( withControl
                [[announce equivocation], [announceAt 1 thirdAnnouncement, (5, vote thirdAnnouncement 0)]]
            )
        votes down @?= [control]
    , testCase "the node relays a vote that arrives after its voting period, within its diffusion period" $ do
        -- Voting ends 8 s in, and diffusion 9 s in.
        down <- relayToOne 0 [[announce someAnnouncingBlock, (8.5, vote someAnnouncingBlock 0)]]
        votes down @?= [voteOf someAnnouncingBlock 0]
    , testCase "the node accepts a vote late within the grace, but does not relay it" $ do
        -- Diffusion ends 9 s in; the client accepts the vote until 69 s in.
        (ups, downs) <- relay 70 (withControl [[(68.5, vote someAnnouncingBlock 0)]]) [0]
        concatMap votes downs @?= [control]
        map poNodeClientEnded ups @?= [Nothing, Nothing]
    , testCase "the node accepts a vote early within the clock skew, but relays it once its window opens" $ do
        -- The window opens 4 s in; the client accepts the vote from 2 s in.
        (ups, downs) <- relay 20 [[announce someAnnouncingBlock, (2.5, vote someAnnouncingBlock 0)]] [0]
        map votes downs @?= [[voteOf someAnnouncingBlock 0]]
        map voteTimes downs @?= [[Time 4]]
        map poNodeClientEnded ups @?= [Nothing]
    , testCase "the node relays the early votes of a message once their window opens" $ do
        let early = voteOf someAnnouncingBlock 0 :| map (voteOf someAnnouncingBlock) [1, 2]
        down <- relayToOne 0 [[announce someAnnouncingBlock, (2.5, NotifyVotes early)]]
        votes down @?= toList early
        voteTimes down @?= map (const (Time 4)) (toList early)
    , testCase "a peer that connects during a vote's window receives the vote" $ do
        down <- relayToOne 6 [votedFor0]
        votes down @?= [voteOf someAnnouncingBlock 0]
    , testCase "a peer that connects once a peer would reject the vote does not receive it" $ do
        (_, downs) <- relay 100 [votedFor0] [0, 90]
        map votes downs @?= [[voteOf someAnnouncingBlock 0], []]
    , testProperty
        "the node relays the first valid vote of each seat it has by its deadline, no later than the log is pruned past it, only what a peer accepts, and certifies what reaches quorum"
        $ forAllShow genVotingPeers showVotingPeers prop_relay
    ]
 where
  -- Seat 0 votes for 'someAnnouncingBlock' 5 seconds in, a second into its
  -- election's window.
  votedFor0 = [announce someAnnouncingBlock, (5, vote someAnnouncingBlock 0)]

  -- Peers that announce 'someAnnouncingBlock' and send what a test expects the
  -- node to hold back, and a last one that sends a valid vote, so that the test
  -- cannot pass merely because the node relays nothing.
  withControl peers =
    map (announce someAnnouncingBlock :) peers
      <> [[announce someAnnouncingBlock, (6, vote someAnnouncingBlock 1)]]
  control = voteOf someAnnouncingBlock 1

-- | Whether the node's client disconnected the peer for an invalid vote.
invalidVoteDisconnected :: PeerOutcome -> Bool
invalidVoteDisconnected o = maybe False ("ExnLeiosInvalidVote" `isInfixOf`) (poNodeClientEnded o)

{-------------------------------------------------------------------------------
  The property
-------------------------------------------------------------------------------}

-- | Upstream peers, which each announce, then vote, as the node's client
-- requires of them: only for announcements they sent, at most once per seat in
-- an election, within the window. A last peer only listens.
data VotingPeers = VotingPeers
  { honestPeers :: [[(DiffTime, Notification)]]
  -- ^ Peers that send only valid votes.
  , invalidPeer :: Maybe [(DiffTime, Notification)]
  -- ^ A peer that sends only invalid votes.
  , nodeVerifying :: DiffTime
  -- ^ How long the node takes to verify a vote.
  }

genVotingPeers :: Gen VotingPeers
genVotingPeers = do
  n <- choose (1, 3)
  VotingPeers
    <$> replicateM n (genUpstream [0 .. seatNoKey - 1] vote)
    <*> oneof [pure Nothing, Just <$> genUpstream [0 .. seatOutside] invalidVote]
    <*> (realToFrac <$> choose (0, 1 :: Double))
 where
  genUpstream seats signed = do
    blks <- elements [[someAnnouncingBlock], [equivocation], [someAnnouncingBlock, equivocation]]
    -- Both blocks are in one election, so a seat votes for one of them.
    voters <- sublistOf seats
    vs <- mapM (genVote blks signed) voters
    pure $ map announce blks <> sortOn fst vs
  genVote blks signed seat = do
    blk <- elements blks
    -- From as early as the clock skew lets the node's client accept, until
    -- the deadline, often within the verifying time of it.
    at <-
      realToFrac
        <$> frequency [(3, choose (2.1 :: Double, 8.99)), (1, choose (8, 8.99))]
    pure (at, signed blk seat)
  -- A seat without a key cannot sign validly, and others sign with a wrong key.
  invalidVote blk seat
    | seat == seatNoKey || seat == seatBadPoP = vote blk seat
    | otherwise = badlySignedVote blk seat

allPeers :: VotingPeers -> [[(DiffTime, Notification)]]
allPeers VotingPeers{honestPeers, invalidPeer} = honestPeers <> maybeToList invalidPeer

showVotingPeers :: VotingPeers -> String
showVotingPeers peers =
  unlines $
    map showPeer (honestPeers peers)
      <> ["invalid: " <> showPeer p | p <- maybeToList (invalidPeer peers)]
      <> ["verifying: " <> show (nodeVerifying peers)]
 where
  showPeer = unwords . map showSend
  showSend (at, n) = show at <> ":" <> showNotification n

prop_relay :: VotingPeers -> Property
prop_relay peers =
  cover 10 (verifiedBy earliestFinish /= allValid) "a vote misses its deadline" $
    cover 10 (not (Set.null quorate)) "an announcement is certified" $
      conjoin $
        [ counterexample "an honest peer was disconnected" $ poNodeClientEnded up === Nothing
        | up <- honestOutcomes
        ]
          <> [ counterexample ("the invalid peer was disconnected: " <> show ended) $
                 maybe True ("ExnLeiosInvalidVote" `isInfixOf`) ended
             | up <- invalidOutcomes
             , let ended = poNodeClientEnded up
             ]
          <> [ counterexample ("peer " <> show i) $ honest (poReceived o)
             | (i, o) <- zip [1 :: Int ..] outcomes
             ]
          <> [ counterexample ("relayed after its deadline, at " <> show at <> ": " <> show (electionSeat v)) False
             | o <- outcomes
             , (at, NotifyVotes vs) <- poReceived o
             , v <- toList vs
             , at > Time (relayedUntilOf (fst (electionSeat v)))
             ]
          <> [ counterexample ("not relayed, though verified by its deadline: " <> show missed) $
                 Set.null missed
             , counterexample ("relayed, though verified only after the log was pruned past it: " <> show extra) $
                 Set.null extra
             , counterexample "certified announcements, against those whose relayed votes reach quorum" $
                 Map.keysSet certified === quorate
             ]
          <> [ counterexample ("a certificate that does not verify: " <> show err) False
             | (rbHash, cert) <- Map.toList certified
             , Left err <- [verifyLeiosCert committee quorum rbHash cert]
             ]
 where
  ups = allPeers peers
  (upOutcomes, down, certified) =
    case runRelay (VerifyingTime (nodeVerifying peers)) 20 (map (PeerSpec 0) ups) [0] of
      (ups', [down'], certified') -> (ups', down', certified')
      _ -> error "prop_relay: one downstream peer"
  (honestOutcomes, invalidOutcomes) = splitAt (length (honestPeers peers)) upOutcomes
  outcomes = upOutcomes <> [down]

  relayed = Set.fromList [electionSeat v | v <- votes down]

  -- Every announcement whose relayed votes reach quorum.
  quorate =
    Map.keysSet . Map.filter (quorum <=) $
      Map.fromListWith (+) [(voteAnnouncingRbHash v, seatWeightOf v) | v <- votes down]
  seatWeightOf v = maybe 0 seatWeight (resolveLeiosSeat committee (voteSeatId v))
  allValid = verifiedBy (map (\(_, v) -> (0, v)))
  missed = Set.difference (verifiedBy latestFinish) relayed
  extra = Set.difference relayed (verifiedBy' relayedUntilOf earliestFinish)

  -- Every election and seat of which the node verified some peer's valid vote
  -- by its deadline, if each verification finished as given. Its client of a
  -- peer verifies one vote after the other, so a vote waits for the ones the
  -- peer sent before it.
  verifiedBy = verifiedBy' deadlineOf
  verifiedBy' latest finish =
    Set.fromList
      [ electionSeat v
      | sends <- ups
      , (done, v) <- finish [(at, v) | (at, NotifyVotes vs) <- sends, v <- toList vs]
      , validVote v
      , done <= latest (fst (electionSeat v))
      ]

  -- At the latest, the node verifies every vote, each taking the whole
  -- verifying time.
  latestFinish =
    snd
      . mapAccumL (\done (at, v) -> let done' = max done (start at v) + verifying in (done', (done', v))) 0
  -- At the earliest, a vote the node relays was verified as soon as it arrived.
  earliestFinish = map (\(at, v) -> (start at v + verifying, v))
  -- The node waits for a vote's window to open.
  start at v = max at (fst (windowOf (fst (electionSeat v))))
  verifying = nodeVerifying peers

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

-- | The deadline of the votes of an election slot.
deadlineOf :: SlotNo -> DiffTime
deadlineOf = snd . windowOf

-- | The latest the node may relay a vote of an election slot: until the log is
-- pruned past its deadline.
relayedUntilOf :: SlotNo -> DiffTime
relayedUntilOf slot = deadlineOf slot + realToFrac notifyLogPruneCadence

-- | When the window of the votes of an election slot opens, and their
-- deadline.
windowOf :: SlotNo -> (DiffTime, DiffTime)
windowOf slot = (realToFrac opens, realToFrac deadline)
 where
  MkVoteWindow (RelativeTime opens) (RelativeTime deadline) =
    -- One-second slots, the first starting when the run begins.
    leiosVoteWindow leiosLedgerView (RelativeTime (fromIntegral (unSlotNo slot)))

-- | Why an honest peer's client would disconnect the node for this stream, if
-- it would: an announcement must be neither a repeat nor the third of its
-- election, nor older than 'maxAnnouncementAgeRecv'; a vote must follow the
-- announcement it is for, be the first of its seat in that election, verify
-- against the committee, and arrive between its window opening and its
-- diffusion period ending, even by a clock as far behind the node's as the
-- clock skew allows.
honest :: [(Time, Notification)] -> Property
honest received = case honestViolations received of
  [] -> property True
  violations -> counterexample (unlines violations) False

honestViolations :: [(Time, Notification)] -> [String]
honestViolations = snd . Foldable.foldl' step ((Announcements.empty, Map.empty, Set.empty), [])
 where
  step ((seen, announced, voted), errs) (at, n) = case n of
    NotifyAnnouncement hdr -> case mkAnnouncingHeader topLevelConfig hdr of
      Nothing -> ((seen, announced, voted), errs <> ["not an announcement: " <> show hdr])
      Just anc -> case Announcements.insert @_ @Void (ancElectionId anc) anc seen of
        Left err -> ((seen, announced, voted), errs <> [show err <> ": " <> show hdr])
        Right seen' ->
          ( (seen', Map.insert (rbHashOf hdr) (blockSlot hdr) announced, voted)
          , errs
              <> [ "too old, at " <> show at <> ": " <> show hdr
                 | toSeconds at - slotStart (blockSlot hdr) > realToFrac maxAnnouncementAgeRecv
                 ]
          )
    NotifyVotes vs -> Foldable.foldl' (checkVote at) ((seen, announced, voted), errs) vs
    NotifyOther -> ((seen, announced, voted), errs)

  checkVote at ((seen, announced, voted), errs) v = case Map.lookup (voteAnnouncingRbHash v) announced of
    Nothing ->
      ((seen, announced, voted), errs <> ["a vote for an announcement not relayed before: " <> show v])
    Just slot ->
      ( (seen, announced, Set.insert (slot, seat) voted)
      , errs
          <> [ "a second vote of seat " <> show seat <> " in slot " <> show slot
             | Set.member (slot, seat) voted
             ]
          <> ["an invalid vote: " <> show v | not (validVote v)]
          <> [ "a vote outside its window, at " <> show at
             | let age = toSeconds at - slotStart slot
             , age < 3 * lHdr || age > 3 * lHdr + lVote + lDiff + grace
             ]
      )
   where
    seat = leiosSeatIndex (voteSeatId v)

  toSeconds (Time t) = realToFrac t :: Double
  slotStart (SlotNo s) = fromIntegral s -- One-second slots.
  lHdr = realToFrac (milliseconds32ToNominalDiffTime (llvAnnouncementPeriod leiosLedgerView))
  lVote = realToFrac (milliseconds32ToNominalDiffTime (llvVotePeriod leiosLedgerView))
  lDiff = realToFrac (milliseconds32ToNominalDiffTime (llvDiffusionPeriod leiosLedgerView))
  grace = realToFrac voteLateGrace

-- | Whether the vote's seat has a key in the committee, and its signature
-- verifies under it.
validVote :: LeiosVote -> Bool
validVote MkLeiosVote{voteAnnouncingRbHash, voteSeatId, voteSignature} =
  case resolveLeiosSeat committee voteSeatId of
    Just LeiosSeat{seatVKey = SJust vk} ->
      isRight $ verifyDSIGN leiosSignContext vk voteAnnouncingRbHash voteSignature
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
  | -- | An offer, which these tests neither send nor check.
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
  -- ^ How the node's client of this peer ended, if it did.
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
  for_ (zip [1 :: Int ..] (ups' <> downs')) $ \(i, o) -> case honestViolations (poReceived o) of
    [] -> pure ()
    violations -> assertFailure $ "peer " <> show i <> ":\n" <> unlines violations
  pure (ups', downs')
 where
  (ups', downs', _) = runRelay (VerifyingTime 0) end ups downs

-- | 'relayPeers', with a node that takes this long to verify each vote, and
-- without failing for what the node relays. Also the certificates the node
-- has in the end, of the announcements the peers sent.
runRelay ::
  VerifyingTime ->
  DiffTime ->
  [PeerSpec] ->
  [DiffTime] ->
  ([PeerOutcome], [PeerOutcome], Map RankingBlockHash LeiosCert)
runRelay verifying end ups downs = runSimOrThrow $ withNode verifying $ \nut -> do
  outcomes <- forConcurrently (zip [1 ..] specs) $ \(i, spec) -> runPeer nut end (PeerAddr i) spec
  certified <-
    atomically $
      fmap (Map.mapMaybe id) $
        sequence $
          Map.fromSet (getCertificate (getLeiosVoteState (nutKernel nut))) announced
  let (ups', downs') = splitAt (length ups) outcomes
  pure (ups', downs', certified)
 where
  specs = ups <> [PeerSpec at [] | at <- downs]
  announced = Set.fromList [rbHashOf hdr | spec <- ups, (_, NotifyAnnouncement hdr) <- psSends spec]

-- | 'relay' for 20 seconds, past every vote's diffusion period, to one downstream peer
-- connecting at the given time.
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

-- | How long the node takes to verify a vote, beyond what verifying takes in the
-- simulation, which is no time at all.
newtype VerifyingTime = VerifyingTime DiffTime

-- | A node of the committee 'committee', with the periods of 'leiosLedgerView'.
withNode :: VerifyingTime -> (NodeUnderTest (IOSim s) -> IOSim s a) -> IOSim s a
withNode (VerifyingTime verifying) body = do
  nodeDBs <- emptyNodeDBs
  withNodeUnderTest (defaultNodeUnderTestConfig k ledgerConfig) tracers nodeDBs body
 where
  tracers =
    nullTracers
      { leiosVoteStateTracer = mkTracer $ \case
          TraceLeiosVoteValidated _ -> threadDelay verifying
          _ -> pure ()
      }

k :: SecurityParam
k = SecurityParam $ knownNonZeroBounded @10

-- | One-second slots, the first starting when the run begins.
ledgerConfig :: LeiosTestLedgerConfig
ledgerConfig =
  LeiosTestLedgerConfig
    { ltlcHardForkParams =
        HardFork.defaultEraParams k (slotLengthFromSec 1) HardFork.NoPerasEnabled
    , ltlcForecastRange = 100000
    , ltlcLeios = leiosLedgerView
    }

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

-- | The log's notifications once pruned at the given time, of entries worth
-- relaying until the given times, each a vote of the seat its name stands for.
notifyLogAt :: [(NominalDiffTime, Char)] -> NominalDiffTime -> String
notifyLogAt appends now = runSimOrThrow $ do
  notifyLog <- newNotifyLog @_ @Blk
  for_ appends $ \(expires, name) ->
    atomically $
      appendNotifyLog
        notifyLog
        (NotifyLog.NotifyVote (voteOf someAnnouncingBlock (seatOf name)))
        (RelativeTime expires)
  atomically $ pruneNotifyLog notifyLog (RelativeTime now)
  atomically $ map nameOf <$> notifyLogNotifications notifyLog

-- | The log's notifications, of entries worth relaying until the given times,
-- appended once it was pruned at the given time.
notifyLogPrunedAt :: NominalDiffTime -> [(NominalDiffTime, Char)] -> String
notifyLogPrunedAt now appends = runSimOrThrow $ do
  notifyLog <- newNotifyLog @_ @Blk
  atomically $ pruneNotifyLog notifyLog (RelativeTime now)
  for_ appends $ \(expires, name) ->
    atomically $
      appendNotifyLog
        notifyLog
        (NotifyLog.NotifyVote (voteOf someAnnouncingBlock (seatOf name)))
        (RelativeTime expires)
  atomically $ map nameOf <$> notifyLogNotifications notifyLog

-- | The node relays a notification at most 'notifyLogPruneCadence' after it is
-- no longer worth relaying, which must leave a downstream peer's grace for
-- transmission and queueing: 'voteLateGrace' for a vote, and the margin between
-- 'maxAnnouncementAgeSend' and 'maxAnnouncementAgeRecv' for an announcement.
prop_notifyLogPruneCadence :: Property
prop_notifyLogPruneCadence =
  once . conjoin $
    [ counterexample "the vote grace" $
        notifyLogPruneCadence `farBelow` voteLateGrace
    , counterexample "the announcement grace" $
        notifyLogPruneCadence `farBelow` (maxAnnouncementAgeRecv - maxAnnouncementAgeSend)
    ]
 where
  farBelow a b = 10 * a <= b

-- | The seat a name stands for in a log, 'a' for seat 0.
seatOf :: Char -> Word16
seatOf name = fromIntegral (fromEnum name - fromEnum 'a')

-- | The name of the seat of a vote in a log.
nameOf :: NotifyLog.Notification Blk -> Char
nameOf = \case
  NotifyLog.NotifyVote v -> toEnum (fromEnum 'a' + fromIntegral (leiosSeatIndex (voteSeatId v)))
  NotifyLog.NotifyAnnouncement _ -> '?'

-- | Every notification in the log.
notifyLogNotifications :: NotifyLog (IOSim s) Blk -> STM (IOSim s) [NotifyLog.Notification Blk]
notifyLogNotifications notifyLog = go notifyLogStart
 where
  go cursor =
    (nextNotifyLog notifyLog cursor >>= \(n, cursor') -> (n :) <$> go cursor')
      `orElse` pure []

{-------------------------------------------------------------------------------
  Fixtures
-------------------------------------------------------------------------------}

-- | The committee 'committee'. A vote is relayed between 3 s and 7 s after its
-- election slot began.
leiosLedgerView :: LeiosLedgerView
leiosLedgerView =
  MkLeiosLedgerView
    { llvCommittee = committee
    , llvQuorumStakeThreshold = fromMaybe (error "leiosLedgerView: a quorum") (boundRational quorum)
    , llvAnnouncementPeriod = Milliseconds32 1000
    , llvVotePeriod = Milliseconds32 4000
    , llvDiffusionPeriod = Milliseconds32 1000
    , llvMaxEbBodySize = 5000
    , llvMaxEbTxsSize = 5000
    }

-- | The votes of all three seats with keys certify an announcement.
quorum :: Weight
quorum = 3 % 5

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
    , voteSignature = signDSIGN leiosSignContext rbHash sk
    }
 where
  rbHash = rbHashOf (ltbHeader blk)

announce :: LeiosTestBlock -> (DiffTime, Notification)
announce = announceAt 0

announceAt :: DiffTime -> LeiosTestBlock -> (DiffTime, Notification)
announceAt at blk = (at, NotifyAnnouncement (ltbHeader blk))

rbHashOf :: Header Blk -> RankingBlockHash
rbHashOf = toRankingBlockHash (Proxy @LeiosTestBlock) . blockHash

-- | Announces an endorser block in slot 1.
someAnnouncingBlock :: LeiosTestBlock
someAnnouncingBlock = setAnnouncement (UnsafeHash (SBS.replicate 32 1)) 1000 (firstLeiosBlock 0)

-- | Another announcement in the election of 'someAnnouncingBlock'.
equivocation :: LeiosTestBlock
equivocation = setAnnouncement (UnsafeHash (SBS.replicate 32 2)) 1000 (forkLeiosBlock (firstLeiosBlock 0))

-- | A third announcement in the election of 'someAnnouncingBlock'.
thirdAnnouncement :: LeiosTestBlock
thirdAnnouncement =
  setAnnouncement
    (UnsafeHash (SBS.replicate 32 3))
    1000
    (forkLeiosBlock (forkLeiosBlock (firstLeiosBlock 0)))

-- | Announces an endorser block in slot 349.
lateAnnouncingBlock :: LeiosTestBlock
lateAnnouncingBlock = setAnnouncement (UnsafeHash (SBS.replicate 32 4)) 1000 (setSlot 349 (firstLeiosBlock 1))
