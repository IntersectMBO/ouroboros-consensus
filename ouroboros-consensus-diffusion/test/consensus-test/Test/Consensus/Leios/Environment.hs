{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | The mocked environment around the node under test.
--
-- A peer here is not scheduled: it holds a chain and a set of endorser blocks,
-- and answers whatever the node asks for. A test changes what a peer holds ---
-- extending its chain, or planting an endorser block --- and the node reacts.
-- That keeps a test written in terms of what exists in the network rather than
-- in terms of message timing.
--
-- An offer is not a promise: the node takes a CertRB header as an offer of the
-- endorser block it certifies, whether or not that peer holds it, and
-- LeiosFetch has no message for refusing. So a request for an endorser block
-- the peer does not hold stays outstanding until the test plants it.
--
-- A header is not a promise either. A peer serves every header of its chain
-- but hands over blocks only as far as 'serveChainThrough' says, so a test can
-- have a peer tell the node about blocks --- a certificate it claims to carry,
-- say --- that it never delivers.
module Test.Consensus.Leios.Environment
  ( PeerEnv (..)
  , WhetherToAwaitAtTip (..)
  , announceEb
  , connectPeer
  , heardBodyOffers
  , heardClosureOffers
  , requestEb
  , requestEbTxs
  , heardOffers
  , heardUnannouncedOffers
  , newPeerEnv
  , setAwaitAtTip
  , offerEb
  , offerEbTxs
  , plantEb
  , serveChain
  , serveChainThrough
  ) where

import Cardano.Network.NodeToNode
  ( ExpandedInitiatorContext (..)
  , IsBigLedgerPeer (..)
  , NodeToNodeVersion
  )
import Cardano.Network.PeerSelection (PeerTrustable (..))
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Codec.CBOR.Read (DeserialiseFailure)
import Codec.Serialise (decode, encode)
import qualified Control.Concurrent.Class.MonadSTM.Strict as PlainSTM
import Control.Monad (unless, void)
import Control.Monad.Class.MonadSay (say)
import Control.Monad.IOSim (IOSim)
import Control.ResourceRegistry (ResourceRegistry, forkLinkedThread)
import Control.Tracer (Tracer (..), emit)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Strict as V
import Data.Word (Word16, Word64)
import LeiosDemoLogic (bitmapOffsets)
import LeiosDemoOnlyTestFetch
  ( LeiosFetchRequestHandler (..)
  , Message (..)
  , SomeLeiosFetchJob (..)
  , leiosFetchClientPeer
  , leiosFetchServerPeer
  )
import LeiosDemoOnlyTestNotify (LeiosNotify (StBusy, StIdle))
import qualified LeiosDemoOnlyTestNotify as Notify
import LeiosDemoTypes
  ( BytesSize
  , EbHash
  , LeiosEb (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , LeiosVote
  , TxHash
  , hashLeiosEb
  )
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config (configCodec)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as CSClient
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import Ouroboros.Consensus.Node.ExitPolicy (NodeToNodeInitiatorResult)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Block (Tip)
import Ouroboros.Network.Channel (createConnectedChannels)
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.Context (ResponderContext (..))
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Driver.Simple (runPeer, runPipelinedPeer)
import Ouroboros.Network.Mock.Chain (Chain, ChainUpdate (..))
import qualified Ouroboros.Network.Mock.Chain as Chain
import Ouroboros.Network.Mock.ProducerState
  ( ChainProducerState (..)
  , findFirstPoint
  , followerInstruction
  , initChainProducerState
  , initFollower
  , switchFork
  , updateFollower
  )
import Ouroboros.Network.PeerSelection.PeerMetric (nullMetric)
import Ouroboros.Network.Protocol.BlockFetch.Server
  ( BlockFetchBlockSender (..)
  , BlockFetchSendBlocks (..)
  , BlockFetchServer (..)
  , blockFetchServerPeer
  )
import Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange (..))
import Ouroboros.Network.Protocol.ChainSync.Server
  ( ChainSyncServer (..)
  , ServerStIdle (..)
  , ServerStIntersect (..)
  , ServerStNext (..)
  , chainSyncServerPeer
  )
import Ouroboros.Network.Protocol.KeepAlive.Server
  ( KeepAliveServer (..)
  , keepAliveServerPeer
  )
import Ouroboros.Network.Protocol.Limits (ProtocolTimeLimitsWithRnd (..), waitForever)
import System.Random (mkStdGen)
import Test.Consensus.Leios.NodeUnderTest
import Test.Util.LeiosTestBlock
import Test.Util.Orphans.IOLike ()

type Blk = LeiosTestBlock

-- | What one peer holds. Both are mutable, since a test reveals things to the
-- node over time.
data PeerEnv m = PeerEnv
  { peChain :: PlainSTM.StrictTVar m (ChainProducerState Blk)
  , peServeThrough :: StrictTVar m (Point Blk)
  -- ^ The last block of 'peChain' this peer hands over; see
  -- 'serveChainThrough'.
  , peAwaitAtTip :: PlainSTM.StrictTVar m WhetherToAwaitAtTip
  -- ^ What this peer does once the node holds every header it has; see
  -- 'setAwaitAtTip'.
  , peEbs :: StrictTVar m (Map EbHash (LeiosEb, Map TxHash BS.ByteString))
  , peNotifications :: PlainSTM.StrictTVar m [LeiosNotification]
  -- ^ What this peer has yet to say over LeiosNotify, in order.
  , peHeard :: PlainSTM.StrictTVar m [LeiosNotification]
  -- ^ What the node has said to this peer over LeiosNotify, in order. The
  -- node is the upstream peer on this second LeiosNotify connection, which is
  -- how a test sees what it relays.
  , peFetchRequests :: PlainSTM.StrictTVar m [SomeLeiosFetchJob LeiosPoint LeiosEb LeiosTx m]
  -- ^ What this peer has yet to ask the node for over LeiosFetch, in order.
  -- The node is the server on that second LeiosFetch connection, so this is
  -- how a test says what a downstream peer requests of it --- including things
  -- no honest peer would ask for.
  }

-- | One thing a peer says over LeiosNotify.
type LeiosNotification =
  Notify.Message (LeiosNotify LeiosPoint (Header Blk) LeiosVote) StBusy StIdle

-- | See 'setAwaitAtTip'.
data WhetherToAwaitAtTip
  = -- | Send @MsgAwaitReply@, as a real peer does.
    AwaitAtTip
  | -- | Say nothing, leaving the node's request unanswered until this peer's
    -- chain grows.
    StayQuietAtTip
  deriving (Eq, Show)

newPeerEnv :: IOSim s (PeerEnv (IOSim s))
newPeerEnv = do
  peChain <- PlainSTM.newTVarIO (initChainProducerState Chain.Genesis)
  peServeThrough <- newTVarIO GenesisPoint
  peAwaitAtTip <- PlainSTM.newTVarIO AwaitAtTip
  peEbs <- newTVarIO Map.empty
  peNotifications <- PlainSTM.newTVarIO []
  peHeard <- PlainSTM.newTVarIO []
  peFetchRequests <- PlainSTM.newTVarIO []
  pure
    PeerEnv
      { peChain
      , peServeThrough
      , peAwaitAtTip
      , peEbs
      , peNotifications
      , peHeard
      , peFetchRequests
      }

-- | The endorser blocks the node offered this peer without having first
-- announced them to it.
--
-- That is the rule the node itself enforces on its upstream peers, so any
-- answer but the empty list is the node doing what it would disconnect a peer
-- for.
heardUnannouncedOffers :: PeerEnv (IOSim s) -> IOSim s [LeiosPoint]
heardUnannouncedOffers PeerEnv{peHeard} =
  go [] <$> atomically (PlainSTM.readTVar peHeard)
 where
  go :: [LeiosPoint] -> [LeiosNotification] -> [LeiosPoint]
  go _announced [] = []
  go announced (msg : rest) = case msg of
    Notify.MsgLeiosBlockAnnouncement hdr ->
      go (maybe id ((:) . fst) (lthAnnouncement hdr) announced) rest
    Notify.MsgLeiosBlockOffer point _size
      | point `notElem` announced -> point : go announced rest
    Notify.MsgLeiosBlockTxsOffer point
      | point `notElem` announced -> point : go announced rest
    _ -> go announced rest

-- | Make this the chain the peer serves, all of it. Switching to one that is
-- not an extension rolls the node back, as a real peer's would.
serveChain :: PeerEnv (IOSim s) -> Chain Blk -> IOSim s ()
serveChain penv chain = serveChainThrough penv (Chain.headPoint chain) chain

-- | As 'serveChain', but the peer hands over only the blocks up to and
-- including the given point: it still serves every header, and a request that
-- reaches past that point simply never gets an answer.
--
-- Not answering is the one thing no peer can be caught at --- it looks like a
-- slow peer --- and it is how the environment withholds an endorser block too.
-- A later call whose point is further along releases a request that is waiting
-- on those blocks.
serveChainThrough :: PeerEnv (IOSim s) -> Point Blk -> Chain Blk -> IOSim s ()
serveChainThrough PeerEnv{peChain, peServeThrough} through chain =
  atomically $ do
    PlainSTM.modifyTVar peChain $ switchFork chain
    writeTVar peServeThrough through

-- | Choose what this peer does once the node holds every header it has.
--
-- ChainSync jumping disengages a peer that says it has no more headers, and
-- every peer here runs out almost at once, so a test that needs CSJ to
-- still be steering the node when something else happens sets
-- 'StayQuietAtTip'.
setAwaitAtTip :: PeerEnv (IOSim s) -> WhetherToAwaitAtTip -> IOSim s ()
setAwaitAtTip PeerEnv{peAwaitAtTip} = atomically . PlainSTM.writeTVar peAwaitAtTip

-- | Have this peer announce, over LeiosNotify, the endorser block this header
-- announces.
--
-- This is what an offer from this peer has to be backed by; the peer's own
-- chain has nothing to do with it.
announceEb :: PeerEnv (IOSim s) -> Header Blk -> IOSim s ()
announceEb PeerEnv{peNotifications} hdr =
  atomically $
    PlainSTM.modifyTVar peNotifications (<> [Notify.MsgLeiosBlockAnnouncement hdr])

-- | Have this peer offer this endorser block over LeiosNotify.
--
-- Offering is not announcing: this says only that the peer has the body, and
-- an honest peer says it only after its own 'Notify.MsgLeiosBlockAnnouncement'
-- for that endorser block.
offerEb :: PeerEnv (IOSim s) -> LeiosPoint -> BytesSize -> IOSim s ()
offerEb PeerEnv{peNotifications} point size =
  atomically $
    PlainSTM.modifyTVar peNotifications (<> [Notify.MsgLeiosBlockOffer point size])

-- | Have this peer offer this endorser block's closure over LeiosNotify.
--
-- Independent of 'offerEb': either may be sent first, or alone. Like it, this
-- is not an announcement, so an honest peer says it only after its own
-- 'Notify.MsgLeiosBlockAnnouncement' for that endorser block.
offerEbTxs :: PeerEnv (IOSim s) -> LeiosPoint -> IOSim s ()
offerEbTxs PeerEnv{peNotifications} point =
  atomically $
    PlainSTM.modifyTVar peNotifications (<> [Notify.MsgLeiosBlockTxsOffer point])

-- | The endorser blocks the node has offered this peer, in order.
heardOffers :: PeerEnv (IOSim s) -> IOSim s [LeiosPoint]
heardOffers PeerEnv{peHeard} =
  atomically $
    foldMap offered <$> PlainSTM.readTVar peHeard
 where
  offered :: LeiosNotification -> [LeiosPoint]
  offered = \case
    Notify.MsgLeiosBlockOffer point _size -> [point]
    Notify.MsgLeiosBlockTxsOffer point -> [point]
    _ -> []

-- | The endorser blocks whose /body/ the node has told this peer it holds.
heardBodyOffers :: PeerEnv (IOSim s) -> IOSim s [LeiosPoint]
heardBodyOffers PeerEnv{peHeard} =
  atomically $
    foldMap offered <$> PlainSTM.readTVar peHeard
 where
  offered :: LeiosNotification -> [LeiosPoint]
  offered = \case
    Notify.MsgLeiosBlockOffer point _size -> [point]
    _ -> []

-- | The endorser blocks whose /closure/ the node has told this peer it holds.
--
-- Narrower than 'heardOffers' on purpose: offering a body says only that the
-- node has the reference list, which it checked against the announced size,
-- whereas offering the closure says it holds every transaction the block names
-- at the size the block names it at.
heardClosureOffers :: PeerEnv (IOSim s) -> IOSim s [LeiosPoint]
heardClosureOffers PeerEnv{peHeard} =
  atomically $
    foldMap offered <$> PlainSTM.readTVar peHeard
 where
  offered :: LeiosNotification -> [LeiosPoint]
  offered = \case
    Notify.MsgLeiosBlockTxsOffer point -> [point]
    _ -> []

-- | Have this peer ask the node for this endorser block's body.
requestEb :: PeerEnv (IOSim s) -> LeiosPoint -> IOSim s ()
requestEb PeerEnv{peFetchRequests} point =
  atomically $
    PlainSTM.modifyTVar peFetchRequests (<> [job])
 where
  job =
    MkSomeLeiosFetchJob
      (MsgLeiosBlockRequest point)
      (pure (\_reply -> pure ()))

-- | Have this peer ask the node for these offsets of this endorser block's
-- closure.
--
-- The bitmaps are passed through as given, so a test can ask for more than any
-- honest peer would: the node's own fetch logic batches its requests well
-- inside the bound the server enforces, and nothing else would produce one
-- that breaches it.
requestEbTxs :: PeerEnv (IOSim s) -> LeiosPoint -> [(Word16, Word64)] -> IOSim s ()
requestEbTxs PeerEnv{peFetchRequests} point bitmaps =
  atomically $
    PlainSTM.modifyTVar peFetchRequests (<> [job])
 where
  job =
    MkSomeLeiosFetchJob
      (MsgLeiosBlockTxsRequest point bitmaps)
      (pure (\_reply -> pure ()))

-- | The next thing this peer has to ask for, blocking until it has something.
--
-- Never finishes: like the LeiosNotify client, this peer simply goes on asking
-- for as long as the test runs.
nextFetchRequest ::
  PeerEnv (IOSim s) ->
  IOSim s (Either () (SomeLeiosFetchJob LeiosPoint LeiosEb LeiosTx (IOSim s)))
nextFetchRequest PeerEnv{peFetchRequests} = atomically $ do
  PlainSTM.readTVar peFetchRequests >>= \case
    [] -> retry
    job : rest -> do
      PlainSTM.writeTVar peFetchRequests rest
      pure (Right job)

-- | Make this endorser block, and its closure, available from this peer.
--
-- An empty closure is a peer that has the body and withholds the transactions:
-- it will answer body requests but stalls on a closure request forever.
plantEb ::
  PeerEnv (IOSim s) -> LeiosEb -> [(TxHash, BS.ByteString)] -> IOSim s ()
plantEb PeerEnv{peEbs} eb closure =
  atomically $
    modifyTVar peEbs $
      Map.insert (hashLeiosEb eb) (eb, Map.fromList closure)

-- | Run one peer against the node: the node's ChainSync, BlockFetch,
-- LeiosNotify, and LeiosFetch clients, each against this environment's server
-- AND the node's LeiosNotify server against this environment's client (to
-- observe what the node relays).
--
-- The other mini-protocols are simply never started. Nothing in the node
-- starts them on its own, and a protocol that is not running cannot time out.
connectPeer ::
  forall s.
  NodeUnderTest (IOSim s) ->
  ResourceRegistry (IOSim s) ->
  PeerAddr ->
  PeerEnv (IOSim s) ->
  IOSim s ()
connectPeer nut registry addr penv = do
  (csClient, csServer) <- createConnectedChannels
  (bfClient, bfServer) <- createConnectedChannels
  (kaClient, kaServer) <- createConnectedChannels
  (lnClient, lnServer) <- createConnectedChannels
  (lnDownClient, lnDownServer) <- createConnectedChannels
  (lfClient, lfServer) <- createConnectedChannels
  (lfDownClient, lfDownServer) <- createConnectedChannels

  let codecs = peerCodecs nut
      apps = peerApps nut

      record :: LeiosNotification -> IOSim s ()
      record msg = atomically $ PlainSTM.modifyTVar (peHeard penv) (<> [msg])

      fork name action = void $ forkLinkedThread registry name $ do
        say (name <> " starting")
        r <- action
        say (name <> " finished")
        pure r

  fork ("ChainSync client " <> show addr) $
    void $
      NTN.aChainSyncClient apps version (initiatorCtx addr) csClient
  fork ("ChainSync server " <> show addr) $
    void $
      runPeer (sayTracer ("cs " <> show addr)) (NTN.cChainSyncCodec codecs) csServer $
        chainSyncServerPeer $
          chainSyncServerOf penv

  fork ("BlockFetch client " <> show addr) $
    void $
      NTN.aBlockFetchClient apps version (initiatorCtx addr) bfClient
  fork ("BlockFetch server " <> show addr) $
    void $
      runPeer (sayTracer ("bf " <> show addr)) (NTN.cBlockFetchCodec codecs) bfServer $
        blockFetchServerPeer $
          blockFetchServerOf penv

  -- The BlockFetch client will not start until the keep-alive registry knows
  -- this peer, so this protocol has to run even though nothing here cares
  -- about liveness.
  fork ("KeepAlive client " <> show addr) $
    void $
      NTN.aKeepAliveClient apps version (initiatorCtx addr) kaClient
  fork ("KeepAlive server " <> show addr) $
    void $
      runPeer (sayTracer ("ka " <> show addr)) (NTN.cKeepAliveCodec codecs) kaServer $
        keepAliveServerPeer keepAliveServer

  fork ("LeiosNotify client " <> show addr) $
    void $
      NTN.aLeiosNotifyClient apps version (initiatorCtx addr) lnClient
  fork ("LeiosNotify server " <> show addr) $
    void $
      runPeer (sayTracer ("ln " <> show addr)) (NTN.cLeiosNotifyCodec codecs) lnServer $
        Notify.leiosNotifyServerPeer (nextNotification penv)

  -- The other direction of LeiosNotify: the node is the upstream peer, and
  -- this environment client records everything it says. Nothing here ever
  -- stops asking for more.
  fork ("LeiosNotify server (node) " <> show addr) $
    void $
      NTN.aLeiosNotifyServer apps version (responderCtx addr) lnDownServer
  -- Pipelined, to the same depth the node's own client uses. The node drops a
  -- notification it has no credit to send, by design, so a downstream peer
  -- that does not keep its requests outstanding silently misses offers --- and
  -- a test built on one would conclude the node never made them.
  fork ("LeiosNotify client (env) " <> show addr) $
    void $
      runPipelinedPeer (sayTracer ("ln-down " <> show addr)) (NTN.cLeiosNotifyCodec codecs) lnDownClient $
        Notify.toLeiosNotifyClientPeerPipelined $
          Notify.leiosNotifyClientPeerPipelined
            (pure (Right NTN.leiosNotifyPipelineDepth) :: IOSim s (Either () Int))
            (pure record)

  fork ("LeiosFetch client " <> show addr) $
    void $
      NTN.aLeiosFetchClient apps version (initiatorCtx addr) lfClient
  fork ("LeiosFetch server " <> show addr) $
    void $
      runPeer (sayTracer ("lf " <> show addr)) (NTN.cLeiosFetchCodec codecs) lfServer $
        leiosFetchServerPeer (pure (leiosFetchHandlerOf penv))

  -- The other direction of LeiosFetch: the node is the server, and this
  -- environment client asks it for whatever 'requestEbTxs' has queued.
  fork ("LeiosFetch server (node) " <> show addr) $
    void $
      NTN.aLeiosFetchServer apps version (responderCtx addr) lfDownServer
  fork ("LeiosFetch client (env) " <> show addr) $
    void $
      runPeer (sayTracer ("lf-down " <> show addr)) (NTN.cLeiosFetchCodec codecs) lfDownClient $
        leiosFetchClientPeer (nextFetchRequest penv)

{-------------------------------------------------------------------------------
  The servers
-------------------------------------------------------------------------------}

-- | The next thing this peer has to say, blocking until it has something.
nextNotification :: PeerEnv (IOSim s) -> IOSim s LeiosNotification
nextNotification PeerEnv{peNotifications} = atomically $ do
  PlainSTM.readTVar peNotifications >>= \case
    [] -> retry
    notification : rest -> do
      PlainSTM.writeTVar peNotifications rest
      pure notification

-- | Answers every keep-alive, forever.
keepAliveServer :: KeepAliveServer (IOSim s) ()
keepAliveServer =
  KeepAliveServer
    { recvMsgKeepAlive = pure keepAliveServer
    , recvMsgDone = pure ()
    }

-- | Serves whatever range of the peer's current chain is asked for, as far as
-- the peer hands blocks over (see 'serveChainThrough'): a batch reaching past
-- that point sends the blocks before it and then waits, forever unless a later
-- call moves the point. A range that is not on this peer's chain at all ---
-- what a fork switch leaves behind --- gets the honest "I do not have those".
blockFetchServerOf ::
  PeerEnv (IOSim s) -> BlockFetchServer Blk (Point Blk) (IOSim s) ()
blockFetchServerOf penv = go
 where
  go = BlockFetchServer handleRequest ()

  -- Both ends of the range are inclusive, unlike
  -- 'Chain.selectBlockRange', whose lower end is the anchor.
  handleRequest (ChainRange from to) = do
    blocks <- atomically chainBlocks
    let inRange = takeToPoint to $ dropWhile ((/= from) . blockPoint) blocks
    pure $ case inRange of
      blk : blks | holds to inRange -> SendMsgStartBatch (sendBlocks blk blks)
      _ -> SendMsgNoBlocks (pure go)

  takeToPoint to = \case
    [] -> []
    blk : blks
      | blockPoint blk == to -> [blk]
      | otherwise -> blk : takeToPoint to blks

  -- Each block waits until this peer hands it over, so a batch reaching into
  -- the withheld suffix delivers the blocks before it and then stops mid-batch.
  sendBlocks blk blks = atomically $ do
    served <- servedBlocks
    unless (holds (blockPoint blk) served) retry
    pure $ SendMsgBlock blk $ case blks of
      [] -> pure (SendMsgBatchDone (pure go))
      next : rest -> sendBlocks next rest

  chainBlocks = Chain.toOldestFirst . chainState <$> PlainSTM.readTVar (peChain penv)

  -- The prefix of this peer's chain it hands over, empty if 'peServeThrough'
  -- is not on the chain at all.
  servedBlocks = do
    blocks <- chainBlocks
    through <- readTVar (peServeThrough penv)
    pure $ case break ((== through) . blockPoint) blocks of
      (before, blk : _) -> before <> [blk]
      (_, []) -> []

  holds p = any ((== p) . blockPoint)

-- | Serves this peer's chain, as @chainSyncServerExample@ does, except that
-- reaching the tip need not be announced; see 'setAwaitAtTip'.
chainSyncServerOf ::
  PeerEnv (IOSim s) ->
  ChainSyncServer (Header Blk) (Point Blk) (Tip Blk) (IOSim s) ()
chainSyncServerOf PeerEnv{peChain, peAwaitAtTip} =
  ChainSyncServer $ idle <$> newFollower
 where
  idle r =
    ServerStIdle
      { recvMsgRequestNext = handleRequestNext r
      , recvMsgFindIntersect = handleFindIntersect r
      , recvMsgDoneClient = pure ()
      }

  idle' = ChainSyncServer . pure . idle

  -- The @Right@ is what puts @MsgAwaitReply@ on the wire. Blocking first and
  -- answering @Left@ sends the roll-forward alone, whenever it comes.
  handleRequestNext r = do
    tryReadChainUpdate r >>= \case
      Just update -> pure $ Left $ sendNext r update
      Nothing -> do
        -- This blocks on the flag as well as on the chain, so telling a peer
        -- that is already waiting here to announce its tip wakes it.
        mbUpdate <- atomically $ do
          PlainSTM.readTVar peAwaitAtTip >>= \case
            AwaitAtTip -> pure Nothing
            StayQuietAtTip -> Just <$> awaitInstruction r
        pure $ case mbUpdate of
          Just update -> Left $ sendNext r update
          Nothing -> Right $ sendNext r <$> readChainUpdate r

  sendNext r (tip, update) = case update of
    AddBlock blk -> SendMsgRollForward (getHeader blk) tip (idle' r)
    RollBack point -> SendMsgRollBackward point tip (idle' r)

  handleFindIntersect r points = do
    (mbPoint, tip) <- atomically $ do
      cps <- PlainSTM.readTVar peChain
      case findFirstPoint points cps of
        Nothing -> pure (Nothing, tipOf cps)
        Just point -> do
          let cps' = updateFollower r point cps
          PlainSTM.writeTVar peChain cps'
          pure (Just point, tipOf cps')
    pure $ case mbPoint of
      Just point -> SendMsgIntersectFound point tip (idle' r)
      Nothing -> SendMsgIntersectNotFound tip (idle' r)

  newFollower = atomically $ do
    cps <- PlainSTM.readTVar peChain
    let (cps', r) = initFollower GenesisPoint cps
    PlainSTM.writeTVar peChain cps'
    pure r

  tryReadChainUpdate r = atomically $ do
    cps <- PlainSTM.readTVar peChain
    case followerInstruction r cps of
      Nothing -> pure Nothing
      Just (update, cps') -> do
        PlainSTM.writeTVar peChain cps'
        pure $ Just (tipOf cps', update)

  readChainUpdate = atomically . awaitInstruction

  awaitInstruction r = do
    cps <- PlainSTM.readTVar peChain
    case followerInstruction r cps of
      Nothing -> retry
      Just (update, cps') -> do
        PlainSTM.writeTVar peChain cps'
        pure (tipOf cps', update)

  tipOf = Chain.headTip . chainState

-- | Answers a request for an endorser block, or for its closure's
-- transactions, blocking until this peer holds it.
leiosFetchHandlerOf ::
  PeerEnv (IOSim s) ->
  LeiosFetchRequestHandler LeiosPoint LeiosEb LeiosTx (IOSim s)
leiosFetchHandlerOf PeerEnv{peEbs} = MkLeiosFetchRequestHandler $ \case
  MsgLeiosBlockRequest point -> do
    (eb, _closure) <- atomically $ awaitEb point
    pure $ MsgLeiosBlock eb
  -- A peer that holds the body but not (all of) the closure simply does not
  -- answer, which is the one thing no peer can be caught at. Answering with
  -- fewer txs than were asked for would instead be a protocol violation, and
  -- the node would rightly kill the connection over it.
  MsgLeiosBlockTxsRequest point bitmaps -> do
    txs <- atomically $ do
      (eb, closure) <- awaitEb point
      let ebTxs = leiosEbTxs eb
          asked =
            [ Map.lookup txHash closure
            | offset <- bitmapOffsets bitmaps
            , Just (txHash, _size) <- [ebTxs V.!? offset]
            ]
      case sequence asked of
        Nothing -> retry
        Just bytess -> pure $ V.fromList (map MkLeiosTx bytess)
    pure $ MsgLeiosBlockTxs point bitmaps txs
 where
  awaitEb point = do
    ebs <- readTVar peEbs
    case Map.lookup (pointEbHash point) ebs of
      Nothing -> retry
      Just held -> pure held

{-------------------------------------------------------------------------------
  The node's side of each connection
-------------------------------------------------------------------------------}

-- | The real codecs. 'NTN.mkApps' takes a @Codecs@ whose ChainSync and
-- ChainSync-of-serialised-headers share one wire type, and likewise for
-- BlockFetch; 'NTN.identityCodecs' makes each protocol's wire type its own
-- message type, so the pairs differ and it does not fit. Hence these tests pay
-- for serialisation after all.
peerCodecs ::
  NodeUnderTest (IOSim s) ->
  NTN.Codecs
    Blk
    PeerAddr
    DeserialiseFailure
    (IOSim s)
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
peerCodecs nut =
  NTN.defaultCodecs
    (configCodec (nutTopLevelConfig nut))
    ()
    (const encodePeerAddr)
    (const decodePeerAddr)
    version

peerApps ::
  NodeUnderTest (IOSim s) ->
  NTN.Apps
    (IOSim s)
    PeerAddr
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    NodeToNodeInitiatorResult
    ()
peerApps nut =
  NTN.mkApps
    (nutKernel nut)
    (mkStdGen 17)
    NTN.nullTracers
    (const (peerCodecs nut))
    NTN.noByteLimits
    (\_ -> ProtocolTimeLimitsWithRnd $ \_state -> (waitForever,))
    CSClient.ChainSyncLoPBucketDisabled
    (nutCsjConfig nut)
    nullMetric
    (nutHandlers nut)

encodePeerAddr :: PeerAddr -> CBOR.Encoding
encodePeerAddr (PeerAddr n) = encode n

decodePeerAddr :: CBOR.Decoder s PeerAddr
decodePeerAddr = PeerAddr <$> decode

-- | Everything the peers see, into the simulation's say log, so a test that
-- gets stuck can show what the node last asked for.
sayTracer :: Show a => String -> Tracer (IOSim s) a
sayTracer prefix = Tracer . emit $ \ev -> say (prefix <> ": " <> show ev)

version :: NodeToNodeVersion
version = maxBound

-- | Every peer is a big ledger peer, which is the case the Leios logic treats
-- most generously; a test that wants the other case will have to say so.
initiatorCtx ::
  PeerAddr -> ExpandedInitiatorContext PeerAddr PeerTrustable (IOSim s)
initiatorCtx addr =
  ExpandedInitiatorContext
    { eicConnectionId = ConnectionId addr addr
    , eicControlMessage = pure Continue
    , eicIsBigLedgerPeer = IsBigLedgerPeer
    , eicExtraFlags = IsNotTrustable
    }

-- | The node's side of a connection on which the node is the upstream peer.
responderCtx :: PeerAddr -> ResponderContext PeerAddr
responderCtx addr = ResponderContext{rcConnectionId = ConnectionId addr addr}
