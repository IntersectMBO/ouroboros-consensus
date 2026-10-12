{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests of LeiosNotify's client and server. Some run the two against each
-- other over connected channels with a real codec. The others run the node's
-- client and server, over real connections, against peers run by the Leios
-- node-vs-environment tests.
module Test.Consensus.Leios.LeiosNotify (tests) where

import Cardano.Ledger.BaseTypes (knownNonZeroBounded)
import Cardano.Network.NodeToNode (defaultMiniProtocolParameters)
import Cardano.Network.NodeToNode.Version (NodeToNodeVersion (..))
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI (threadDelay, timeout)
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (for_)
import qualified Data.Map.Strict as Map
import Network.TypedProtocol.Codec (Codec)
import Ouroboros.Consensus.BlockchainTime (slotLengthFromSec)
import Ouroboros.Consensus.Config (SecurityParam (..))
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Server
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type
import Ouroboros.Consensus.Network.LeiosNotify
  ( leiosNotifyMiniProtocolNum
  , leiosNotifyPipelineDepth
  , runLookaheadFixedSenderPeerWithLimits
  )
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import Ouroboros.Consensus.Node.ExitPolicy (NodeToNodeInitiatorResult)
import Ouroboros.Network.Channel (Channel, createConnectedBufferedChannels)
import Ouroboros.Network.Driver.Limits (runPipelinedPeerWithLimits)
import Ouroboros.Network.Mux
  ( MiniProtocol (..)
  , TemperatureBundle (..)
  , WithProtocolTemperature (..)
  )
import Ouroboros.Network.Protocol.Limits (ProtocolSizeLimits)
import Ouroboros.Network.Util.ShowProxy (ShowProxy)
import Test.Consensus.Leios.Environment
import Test.Consensus.Leios.NodeUnderTest
import Test.Consensus.Leios.TestBlock (LeiosTestLedgerConfig (..))
import Test.Tasty
import Test.Tasty.HUnit
import Test.Util.ChainDB (emptyNodeDBs)

tests :: TestTree
tests =
  testGroup
    "LeiosNotify"
    [ testGroup
        "client and server"
        [ testCase "the server cancels every outstanding request when the client quits" $
            runSimOrThrow (outstandingCanceled 3) @?= OutstandingOutcome 3 0
        , testCase "the server disconnects a client that exceeds its pipelining bound" $
            runSimOrThrow excessive @?= ExcessiveOutcome Disconnected
        ]
    , testGroup
        "node vs environment"
        [ testCase "the node's client requests nothing, and quits when the node demotes the peer" $
            runSimOrThrow (timeout 100 nodeClientQuits) @?= Just (NodeClientOutcome Running Succeeded 0)
        , testCase "the node's server cancels a peer's outstanding requests when the peer quits" $
            runSimOrThrow (timeout 100 nodeServerCancels) @?= Just (NodeServerOutcome 0)
        , testCase "the node's server disconnects a peer that exceeds the pipelining bound" $
            runSimOrThrow (timeout 100 nodeServerExcessive) @?= Just (ExcessiveOutcome Disconnected)
        , testCase "the node runs LeiosNotify from NodeToNodeV_16 on" $
            runSimOrThrow (timeout 100 (nodeRunsLeiosNotify NodeToNodeV_16)) @?= Just (True, True)
        , testCase "the node does not run LeiosNotify before NodeToNodeV_16" $
            runSimOrThrow (timeout 100 (nodeRunsLeiosNotify NodeToNodeV_15)) @?= Just (False, False)
        ]
    ]

{-------------------------------------------------------------------------------
  Client and server on their own
-------------------------------------------------------------------------------}

type P = LeiosNotify Int Int Int

codec :: Codec P CBOR.DeserialiseFailure (IOSim s) BSL.ByteString
codec =
  codecLeiosNotify
    CBOR.encodeInt
    CBOR.decodeInt
    CBOR.encodeInt
    CBOR.decodeInt
    CBOR.encodeInt
    CBOR.decodeInt

sizeLimits :: ProtocolSizeLimits (LeiosNotify point announcement vote) BSL.ByteString
sizeLimits = byteLimitsLeiosNotify (fromIntegral . BSL.length)

-- | Run a server that never replies, counting requests and refusing more than
-- @bound@.
runServer ::
  (ShowProxy point, ShowProxy announcement, ShowProxy vote) =>
  Codec (LeiosNotify point announcement vote) CBOR.DeserialiseFailure (IOSim s) BSL.ByteString ->
  StrictTVar (IOSim s) Int ->
  Int ->
  Channel (IOSim s) BSL.ByteString ->
  IOSim s ()
runServer c requests bound chan = do
  peer <-
    leiosNotifyServerPeerLookahead
      ( atomically $ do
          n <- readTVar requests
          if n >= bound
            then pure ExcessiveRequests
            else do
              writeTVar requests (n + 1)
              pure NotExcessiveRequests
      )
      retry
  fst
    <$> runLookaheadFixedSenderPeerWithLimits nullTracer c sizeLimits timeLimitsLeiosNotify chan peer

-- | Run the pipelined client at the given depth until @stop@ is filled,
-- counting the replies it handles.
runPipelinedClient ::
  (ShowProxy point, ShowProxy announcement, ShowProxy vote) =>
  Codec (LeiosNotify point announcement vote) CBOR.DeserialiseFailure (IOSim s) BSL.ByteString ->
  StrictTVar (IOSim s) Int ->
  Int ->
  StrictTMVar (IOSim s) () ->
  Channel (IOSim s) BSL.ByteString ->
  IOSim s ()
runPipelinedClient c replies depth stop chan =
  fst
    <$> runPipelinedPeerWithLimits
      nullTracer
      c
      sizeLimits
      timeLimitsLeiosNotify
      chan
      ( toLeiosNotifyClientPeerPipelined $
          leiosNotifyClientPeerPipelined
            ((Left <$> readTMVar stop) `orElse` pure (Right depth))
            (pure $ \_msg -> atomically $ modifyTVar replies (+ 1))
      )

data OutstandingOutcome = OutstandingOutcome Int Int
  deriving (Eq, Show)

-- | Fill the pipeline, then quit: the server sees every request, and the
-- client handles none of the cancellations, since it is already quitting.
outstandingCanceled :: Int -> IOSim s OutstandingOutcome
outstandingCanceled depth = do
  (clientChan, serverChan) <- createConnectedBufferedChannels 10
  requests <- newTVarIO 0
  replies <- newTVarIO 0
  stop <- newEmptyTMVarIO
  withAsync (runServer codec requests depth serverChan) $ \server ->
    withAsync (runPipelinedClient codec replies depth stop clientChan) $ \client -> do
      atomically $ readTVar requests >>= check . (== depth)
      atomically $ putTMVar stop ()
      wait client
      wait server
      OutstandingOutcome <$> readTVarIO requests <*> readTVarIO replies

data WhetherDisconnected = Disconnected | NotDisconnected
  deriving (Eq, Show)

newtype ExcessiveOutcome = ExcessiveOutcome WhetherDisconnected
  deriving (Eq, Show)

-- | A client pipelining one request more than the server allows.
excessive :: IOSim s ExcessiveOutcome
excessive = do
  (clientChan, serverChan) <- createConnectedBufferedChannels 10
  requests <- newTVarIO 0
  replies <- newTVarIO 0
  stop <- newEmptyTMVarIO
  withAsync (runPipelinedClient codec replies 3 stop clientChan) $ \_client ->
    disconnectedBy $ runServer codec requests 2 serverChan

disconnectedBy :: IOSim s () -> IOSim s ExcessiveOutcome
disconnectedBy server =
  try server >>= \case
    Left MkExnLeiosNotifyExcessiveRequests -> pure $ ExcessiveOutcome Disconnected
    Right () -> pure $ ExcessiveOutcome NotDisconnected

{-------------------------------------------------------------------------------
  The node, connected to peers run by the test
-------------------------------------------------------------------------------}

withNode :: (NodeUnderTest (IOSim s) -> IOSim s a) -> IOSim s a
withNode body = do
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
        }

-- | The node's LeiosNotify client on the connection.
nodeLeiosNotifyClient ::
  NodeVsEnvConnection (IOSim s) ->
  IOSim s (STM (IOSim s) (Either SomeException NodeToNodeInitiatorResult))
nodeLeiosNotifyClient conn =
  maybe (throwIO $ userError "the node does not run a LeiosNotify client") pure $
    Map.lookup leiosNotifyMiniProtocolNum (connNodeInitiators conn)

-- | The node's LeiosNotify server on the connection.
nodeLeiosNotifyServer ::
  NodeVsEnvConnection (IOSim s) -> IOSim s (STM (IOSim s) (Either SomeException ()))
nodeLeiosNotifyServer conn =
  maybe (throwIO $ userError "the node does not run a LeiosNotify server") pure $
    Map.lookup leiosNotifyMiniProtocolNum (connNodeResponders conn)

data WhetherRunning = Running | Finished
  deriving (Eq, Show)

data WhetherSucceeded = Succeeded | Failed
  deriving (Eq, Show)

succeeded :: Either e a -> WhetherSucceeded
succeeded = either (const Failed) (const Succeeded)

data NodeClientOutcome = NodeClientOutcome WhetherRunning WhetherSucceeded Int
  deriving (Eq, Show)

-- | The peer's server rejects every request, so that the node sending one fails
-- the test.
nodeClientQuits :: IOSim s NodeClientOutcome
nodeClientQuits = withNode $ \nut -> do
  requests <- newTVarIO 0
  let envPeer =
        EnvPeer
          { epLeiosNotifyServer = runServer (envLeiosNotifyCodec nut) requests 0
          , epLeiosNotifyClient = Nothing
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \conn -> do
    client <- nodeLeiosNotifyClient conn
    threadDelay 10
    before <- atomically $ (Finished <$ client) `orElse` pure Running
    connTerminate conn
    ended <- atomically client
    NodeClientOutcome before (succeeded ended) <$> readTVarIO requests

newtype NodeServerOutcome = NodeServerOutcome Int
  deriving (Eq, Show)

-- | The node never has a notification, so the peer's requests are all
-- outstanding when it quits.
nodeServerCancels :: IOSim s NodeServerOutcome
nodeServerCancels = withNode $ \nut -> do
  requests <- newTVarIO 0
  replies <- newTVarIO 0
  stop <- newEmptyTMVarIO
  let codec' = envLeiosNotifyCodec nut
      envPeer =
        EnvPeer
          { epLeiosNotifyServer = runServer codec' requests 0
          , epLeiosNotifyClient = Just $ runPipelinedClient codec' replies 3 stop
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \conn -> do
    threadDelay 10
    atomically $ putTMVar stop ()
    for_ (connPeerLeiosNotifyClient conn) $ \client -> atomically client >>= either throwIO pure
    server <- nodeLeiosNotifyServer conn
    atomically server >>= either throwIO pure
    NodeServerOutcome <$> readTVarIO replies

-- | A peer pipelining one request more than the node's server allows, so the
-- node disconnects it.
nodeServerExcessive :: IOSim s ExcessiveOutcome
nodeServerExcessive = withNode $ \nut -> do
  requests <- newTVarIO 0
  replies <- newTVarIO 0
  stop <- newEmptyTMVarIO
  let codec' = envLeiosNotifyCodec nut
      envPeer =
        EnvPeer
          { epLeiosNotifyServer = runServer codec' requests 0
          , epLeiosNotifyClient =
              Just $ runPipelinedClient codec' replies (leiosNotifyPipelineDepth + 1) stop
          }
  withNodeVsEnvConnection nut (PeerAddr 1) envPeer $ \conn -> do
    server <- nodeLeiosNotifyServer conn
    atomically server >>= \case
      Left e | Just MkExnLeiosNotifyExcessiveRequests <- fromException e -> pure $ ExcessiveOutcome Disconnected
      _ -> pure $ ExcessiveOutcome NotDisconnected

-- | Whether the node's initiator bundle, and its initiator-and-responder bundle,
-- include LeiosNotify among their hot mini-protocols for this version.
nodeRunsLeiosNotify :: NodeToNodeVersion -> IOSim s (Bool, Bool)
nodeRunsLeiosNotify ntnVersion = withNode $ \nut ->
  pure
    ( runsLeiosNotify $
        NTN.initiator mempty defaultMiniProtocolParameters ntnVersion versionData (nutApps nut ())
    , runsLeiosNotify $
        NTN.initiatorAndResponder
          mempty
          defaultMiniProtocolParameters
          ntnVersion
          versionData
          (nutApps nut ())
    )
 where
  versionData = nodeVersionData ntnVersion
  runsLeiosNotify bundle = case withHot bundle of
    WithHot protocols -> leiosNotifyMiniProtocolNum `elem` map miniProtocolNum protocols
