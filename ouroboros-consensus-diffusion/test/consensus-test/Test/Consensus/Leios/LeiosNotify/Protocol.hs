{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- | Tests of LeiosNotify's client and server, run against each other over
-- connected channels with a real codec.
module Test.Consensus.Leios.LeiosNotify.Protocol
  ( ExcessiveOutcome (..)
  , P
  , WhetherDisconnected (..)
  , codec
  , runPipelinedClient
  , sizeLimits
  , tests
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad.Class.MonadAsync
import Control.Monad.Class.MonadThrow
import Control.Monad.IOSim (IOSim, runSimOrThrow)
import Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as BSL
import Network.TypedProtocol.Codec (Codec)
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Server
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type
import Ouroboros.Consensus.Network.LeiosNotify (runLookaheadFixedSenderPeerWithLimits)
import Ouroboros.Network.Channel (Channel, createConnectedBufferedChannels)
import Ouroboros.Network.Driver.Limits (runPipelinedPeerWithLimits)
import Ouroboros.Network.Protocol.Limits (ProtocolSizeLimits)
import Ouroboros.Network.Util.ShowProxy (ShowProxy)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "client and server"
    [ testCase "the server cancels every outstanding request when the client quits" $
        runSimOrThrow (outstandingCanceled 3) @?= OutstandingOutcome 3 0
    , testCase "the server disconnects a client that exceeds its pipelining bound" $
        runSimOrThrow excessive @?= ExcessiveOutcome Disconnected
    ]

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
    Left ExnLeiosNotifyExcessiveRequests -> pure $ ExcessiveOutcome Disconnected
    Right () -> pure $ ExcessiveOutcome NotDisconnected
