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
  , connectPeer
  , newPeerEnv
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
import LeiosDemoOnlyTestFetch
  ( LeiosFetchRequestHandler (..)
  , Message (..)
  , leiosFetchServerPeer
  )
import LeiosDemoTypes
  ( EbHash
  , LeiosEb (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , TxHash
  , hashLeiosEb
  )
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config (configCodec)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as CSClient
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import Ouroboros.Consensus.Node.ExitPolicy (NodeToNodeInitiatorResult)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Channel (createConnectedChannels)
import Ouroboros.Network.ConnectionId (ConnectionId (..))
import Ouroboros.Network.ControlMessage (ControlMessage (..))
import Ouroboros.Network.Driver.Simple (runPeer)
import Ouroboros.Network.Mock.Chain (Chain)
import qualified Ouroboros.Network.Mock.Chain as Chain
import Ouroboros.Network.Mock.ProducerState
  ( ChainProducerState (..)
  , initChainProducerState
  , switchFork
  )
import Ouroboros.Network.PeerSelection.PeerMetric (nullMetric)
import Ouroboros.Network.Protocol.BlockFetch.Server
  ( BlockFetchBlockSender (..)
  , BlockFetchSendBlocks (..)
  , BlockFetchServer (..)
  , blockFetchServerPeer
  )
import Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange (..))
import Ouroboros.Network.Protocol.ChainSync.Examples
  ( chainSyncServerExample
  )
import Ouroboros.Network.Protocol.ChainSync.Server (chainSyncServerPeer)
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
  , peEbs :: StrictTVar m (Map EbHash (LeiosEb, Map TxHash BS.ByteString))
  }

newPeerEnv :: IOSim s (PeerEnv (IOSim s))
newPeerEnv = do
  peChain <- PlainSTM.newTVarIO (initChainProducerState Chain.Genesis)
  peServeThrough <- newTVarIO GenesisPoint
  peEbs <- newTVarIO Map.empty
  pure PeerEnv{peChain, peServeThrough, peEbs}

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

-- | Make this endorser block, and its closure, available from this peer.
plantEb ::
  PeerEnv (IOSim s) -> LeiosEb -> [(TxHash, BS.ByteString)] -> IOSim s ()
plantEb PeerEnv{peEbs} eb closure =
  atomically $
    modifyTVar peEbs $
      Map.insert (hashLeiosEb eb) (eb, Map.fromList closure)

-- | Run one peer against the node: the node's ChainSync, BlockFetch and Leios
-- fetch clients, each against this environment's server.
--
-- The other mini-protocols are simply never started. Nothing in the node
-- starts them on its own, and a protocol that is not running cannot time out.
-- LeiosNotify is one of them: the node learns of an endorser block from the
-- CertRB header that certifies it, so this environment has nothing to say
-- over that protocol.
connectPeer ::
  NodeUnderTest (IOSim s) ->
  ResourceRegistry (IOSim s) ->
  PeerAddr ->
  PeerEnv (IOSim s) ->
  IOSim s ()
connectPeer nut registry addr penv = do
  (csClient, csServer) <- createConnectedChannels
  (bfClient, bfServer) <- createConnectedChannels
  (kaClient, kaServer) <- createConnectedChannels
  (lfClient, lfServer) <- createConnectedChannels

  let codecs = peerCodecs nut
      apps = peerApps nut

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
          chainSyncServerExample () (peChain penv) getHeader

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

  fork ("LeiosFetch client " <> show addr) $
    void $
      NTN.aLeiosFetchClient apps version (initiatorCtx addr) lfClient
  fork ("LeiosFetch server " <> show addr) $
    void $
      runPeer (sayTracer ("lf " <> show addr)) (NTN.cLeiosFetchCodec codecs) lfServer $
        leiosFetchServerPeer (pure (leiosFetchHandlerOf penv))

{-------------------------------------------------------------------------------
  The servers
-------------------------------------------------------------------------------}

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

-- | Answers a request for an endorser block, or for its closure's
-- transactions, blocking until this peer holds it.
leiosFetchHandlerOf ::
  PeerEnv (IOSim s) ->
  LeiosFetchRequestHandler LeiosPoint LeiosEb LeiosTx (IOSim s)
leiosFetchHandlerOf PeerEnv{peEbs} = MkLeiosFetchRequestHandler $ \case
  MsgLeiosBlockRequest point -> do
    (eb, _closure) <- atomically $ awaitEb point
    pure $ MsgLeiosBlock eb
  MsgLeiosBlockTxsRequest point bitmaps -> do
    (eb, closure) <- atomically $ awaitEb point
    let txs =
          V.fromList
            [ MkLeiosTx bytes
            | (txHash, _size) <- V.toList (leiosEbTxs eb)
            , Just bytes <- [Map.lookup txHash closure]
            ]
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
    CSClient.CSJDisabled
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
