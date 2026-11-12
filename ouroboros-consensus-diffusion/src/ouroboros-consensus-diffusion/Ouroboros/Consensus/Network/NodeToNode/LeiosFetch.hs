{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The node's LeiosFetch client and server, and what the node-to-node wiring
-- needs to run them.
module Ouroboros.Consensus.Network.NodeToNode.LeiosFetch
  ( runLeiosFetchClient
  , leiosFetchClient
  , leiosFetchMiniProtocol
  , leiosFetchServer
  , catchAndRethrow
  ) where

import Cardano.Network.NodeToNode (addSafetyMargin)
import Cardano.Network.PeerSelection (PeerTrustable)
import Control.DeepSeq (NFData)
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntSet as IntSet
import qualified Data.Vector.Strict as V
import Network.Mux.Timeout (withTimeoutSerial)
import Network.TypedProtocol.Codec (AnnotatedCodec, AnyMessage (..))
import Network.TypedProtocol.Driver (runPipelinedPeerWithDriver)
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , LeiosPoint (..)
  , LeiosTx (..)
  , maxEndorserBlockBytesSize
  , temporaryEndorserBlockFromBytes
  , unsafeEndorserBlockForEncodeOnly
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client
  ( LeiosFetchClientPeerPipelined
  , leiosFetchClientPeerPipelined
  , toLeiosFetchClientPeerPipelined
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies (nextLeiosFetchClientCommand)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel
  ( LeiosFetchKernel (..)
  , LeiosPeerLifetimeId
  )
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState as L
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server
  ( ExnLeiosInvalidRequest (..)
  , LeiosFetchRequestHandler (..)
  , LeiosFetchServerPeer
  , leiosFetchServerPeer
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace (TraceLeiosFetchServerEvent (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
  ( LeiosFetch (..)
  , Message (..)
  , SingLeiosFetch
  , TxBitmaps
  , bitmapToOffsets
  , timeLimitsLeiosFetch
  )
import Ouroboros.Consensus.Network.LeiosFetch
  ( leiosFetchMiniProtocolNum
  , leiosFetchPipelineDepth
  , maxLeiosFetchIngressBytes
  )
import Ouroboros.Consensus.Storage.LeiosDB.API (LeiosDbReader (..))
import Ouroboros.Consensus.Util.IOLike
  ( Exception
  , IOLike
  , MonadCatch (..)
  , MonadThrow (..)
  , atomically
  , finally
  )
import Ouroboros.Network.Channel (Channel)
import Ouroboros.Network.ControlMessage (ControlMessageSTM)
import Ouroboros.Network.Driver.Limits (ProtocolSizeLimits, TraceSendRecv)
import Ouroboros.Network.Driver.Progress
import Ouroboros.Network.Mux
  ( MiniProtocol (..)
  , MiniProtocolLimits (..)
  , MiniProtocolWithExpandedCtx
  , RunMiniProtocolWithExpandedCtx
  , StartOnDemandOrEagerly (..)
  )

-- | The node's LeiosFetch client, which sends the requests the node's
-- decision loop assigns to the peer, and ends when the node demotes the peer.
leiosFetchClient ::
  (IOLike m, Ord peer) =>
  LeiosFetchKernel m peer blk ->
  ControlMessageSTM m ->
  R.Client m (LeiosPeerLifetimeId peer) ->
  LeiosFetchClientPeerPipelined LeiosPoint EndorserBlock LeiosTx m ()
leiosFetchClient kernel controlMessageSTM client =
  toLeiosFetchClientPeerPipelined $
    leiosFetchClientPeerPipelined leiosFetchPipelineDepth $
      nextLeiosFetchClientCommand
        (lfkContext kernel)
        controlMessageSTM
        client

-- | The ordinary legacy-codec client with connection-bound FIFO observations.
-- Complete-message original arrival times survive decoder and handler delays;
-- no incremental payload salvage or cancellation is added to the protocol.
runLeiosFetchClient ::
  forall m peer failure.
  (IOLike m, MonadTimer m, Ord peer, NFData failure, Show failure) =>
  Tracer m (TraceSendRecv (LeiosFetch LeiosPoint EndorserBlock LeiosTx)) ->
  AnnotatedCodec (LeiosFetch LeiosPoint EndorserBlock LeiosTx) failure m BL.ByteString ->
  ProtocolSizeLimits (LeiosFetch LeiosPoint EndorserBlock LeiosTx) BL.ByteString ->
  R.Client m peer ->
  LeiosFetchClientPeerPipelined LeiosPoint EndorserBlock LeiosTx m () ->
  Channel m BL.ByteString ->
  m ((), Maybe BL.ByteString)
runLeiosFetchClient tracer codec sizes client peer channel =
  ( withTimeoutSerial $ \timeoutFn -> do
      let scope ::
            forall (st :: LeiosFetch LeiosPoint EndorserBlock LeiosTx).
            SingLeiosFetch st -> m (ReceiveScope m ())
          scope _ = do
            (key, attempt) <- atomically (R.receiverAttempt client)
            let bound = case R.attemptWork attempt of L.Body{} -> maxEndorserBlockBytesSize + 3; L.Job{} -> 550000
            pure
              ReceiveScope
                { receiveLimits = ProgressLimits bound 0 0
                , receiveStarted = pure ()
                , receiveInputStarting = atomically (R.publishInput client True)
                , receiveInput = \_ -> atomically (R.publishInput client False)
                , receiveAwaitInput = pure ()
                , receiveProgress = \_ -> throwIO R.MissingAttempt
                , receiveComplete = \receipt _ -> atomically (R.completeWire client key (receiptObservedAt receipt))
                , receiveAbort = \_ -> atomically (R.publishInput client False)
                }
          observe (AnyMessage msg) at = case msg of
            MsgLeiosBlockRequest{} -> atomically (R.observeSend client at)
            MsgLeiosBlockTxsRequest{} -> atomically (R.observeSend client at)
            _ -> pure ()
          driver =
            progressDriverWithLimits
              tracer
              timeoutFn
              (const codec)
              sizes
              timeLimitsLeiosFetch
              (fromIntegral maxLeiosFetchIngressBytes)
              scope
              observe
              channel
      ((), trailing) <- runPipelinedPeerWithDriver driver peer
      pure ((), progressTrailingBytes trailing)
  )
    `finally` atomically (R.closeClient client)

-- | The node's LeiosFetch server, which serves endorser blocks and their
-- transactions from the LeiosDB, through the connection's own reader.
leiosFetchServer ::
  MonadCatch m =>
  Tracer m TraceLeiosFetchServerEvent ->
  LeiosDbReader m ->
  LeiosFetchServerPeer LeiosPoint EndorserBlock LeiosTx m ()
leiosFetchServer tracer reader =
  leiosFetchServerPeer $ MkLeiosFetchRequestHandler $ \case
    MsgLeiosBlockRequest point ->
      fmap MsgLeiosBlock $
        catchAndRethrow tracer TraceLeiosFetchServerInvalidRequest $
          catchAndRethrow tracer (TraceLeiosFetchServerBlockDbFailure point) $
            serveEb tracer reader point
    MsgLeiosBlockTxsRequest point bitmaps ->
      fmap MsgLeiosBlockTxs $
        catchAndRethrow tracer TraceLeiosFetchServerInvalidRequest $
          catchAndRethrow tracer (TraceLeiosFetchServerTxsDbFailure point bitmaps) $
            serveTxs tracer reader point bitmaps

serveEb ::
  MonadThrow m =>
  Tracer m TraceLeiosFetchServerEvent ->
  LeiosDbReader m ->
  LeiosPoint ->
  m EndorserBlock
serveEb tracer reader point = do
  -- TODO Disconnect a client that requests what the node never offered it.
  traceWith tracer $ TraceLeiosFetchServerBlockRequested point
  lookupEbBody reader point >>= \case
    Nothing -> throwIO $ ExnLeiosUnknownBlockRequested point
    Just bytes -> do
      traceWith tracer $ TraceLeiosFetchServerBlockServed point (BS.length bytes)
      pure $ unsafeEndorserBlockForEncodeOnly $ temporaryEndorserBlockFromBytes bytes

serveTxs ::
  MonadThrow m =>
  Tracer m TraceLeiosFetchServerEvent ->
  LeiosDbReader m ->
  LeiosPoint ->
  TxBitmaps ->
  m (V.Vector LeiosTx)
serveTxs tracer reader point bitmaps = do
  -- TODO Disconnect a client that requests what the node never offered it.
  traceWith tracer $ TraceLeiosFetchServerTxsRequested point (length offsets)
  -- One row for each requested offset that the endorser block at this point
  -- has, with its bytes if the node holds them there.
  rows <- batchRetrieveTxs reader point offsets
  case traverse (\(_offset, _txHash, mbBytes) -> MkLeiosTx <$> mbBytes) rows of
    Just txs | length txs == length offsets -> do
      let n = length txs
      traceWith tracer $
        TraceLeiosFetchServerTxsServed point n (sum [BS.length (leiosTxBytes tx) | tx <- txs])
      pure $ V.fromListN n txs
    _ -> do
      let held = IntSet.fromList [offset | (offset, _txHash, Just _bytes) <- rows]
      throwIO $ ExnLeiosUnknownTxsRequested point (filter (`IntSet.notMember` held) offsets)
 where
  offsets = bitmapToOffsets bitmaps

-- | Trace an exception of the event's type that the action throws, then
-- rethrow it.
catchAndRethrow :: (MonadCatch m, Exception e) => Tracer m ev -> (e -> ev) -> m a -> m a
catchAndRethrow tracer mkEvent action =
  action `catch` \e -> do
    traceWith tracer (mkEvent e)
    throwIO e

-- | LeiosFetch as a node-to-node mini-protocol.
leiosFetchMiniProtocol ::
  RunMiniProtocolWithExpandedCtx mode addr PeerTrustable b m a c ->
  MiniProtocolWithExpandedCtx mode addr PeerTrustable b m a c
leiosFetchMiniProtocol run =
  MiniProtocol
    { miniProtocolNum = leiosFetchMiniProtocolNum
    , miniProtocolStart = StartOnDemand
    , miniProtocolLimits = leiosFetchProtocolLimits
    , miniProtocolRun = run
    }

-- | The ingress queue must hold every reply the client may have requested and
-- not yet consumed, 'maxLeiosFetchIngressBytes' of them; the margin covers the
-- framing around those bytes (see
-- 'Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type.byteLimitsLeiosFetch'). The
-- server's, which only receives requests, needs far less.
leiosFetchProtocolLimits :: MiniProtocolLimits
leiosFetchProtocolLimits =
  MiniProtocolLimits
    { maximumIngressQueue = addSafetyMargin maxLeiosFetchIngressBytes
    }
