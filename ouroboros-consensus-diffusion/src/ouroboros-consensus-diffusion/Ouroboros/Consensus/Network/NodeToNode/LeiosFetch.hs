{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The node's LeiosFetch client and server, and what the node-to-node wiring
-- needs to run them.
module Ouroboros.Consensus.Network.NodeToNode.LeiosFetch
  ( leiosFetchClient
  , leiosFetchMiniProtocol
  , leiosFetchServer
  , catchAndRethrow
  ) where

import Cardano.Network.NodeToNode (addSafetyMargin)
import Cardano.Network.PeerSelection (PeerTrustable)
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import qualified Data.IntSet as IntSet
import qualified Data.Vector.Strict as V
import Ouroboros.Consensus.Leios.Types
  ( LeiosPoint (..)
  , LeiosTx (..)
  , SerialisedEndorserBlock
  , mkEndorserBlock
  , serialiseEndorserBlock
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client
  ( LeiosFetchClientPeerPipelined
  , leiosFetchClientPeerPipelined
  , toLeiosFetchClientPeerPipelined
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client.Replies (nextLeiosFetchClientCommand)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Kernel
  ( LeiosFetchKernel (..)
  , LeiosFetchPeer (..)
  , LeiosPeerLifetimeId
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server
  ( ExnLeiosInvalidRequest (..)
  , LeiosFetchRequestHandler (..)
  , LeiosFetchServerPeer
  , leiosFetchServerPeer
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace (TraceLeiosFetchServerEvent (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
  ( Message (..)
  , TxBitmaps
  , bitmapOffsets
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
  )
import Ouroboros.Network.ControlMessage (ControlMessageSTM)
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
  LeiosPeerLifetimeId peer ->
  LeiosFetchPeer m blk ->
  LeiosFetchClientPeerPipelined LeiosPoint SerialisedEndorserBlock LeiosTx m ()
leiosFetchClient kernel controlMessageSTM lifetimeId peerVars =
  toLeiosFetchClientPeerPipelined $
    leiosFetchClientPeerPipelined leiosFetchPipelineDepth $
      nextLeiosFetchClientCommand
        (lfkContext kernel)
        controlMessageSTM
        lifetimeId
        (lfpRequestsToSend peerVars)

-- | The node's LeiosFetch server, which serves endorser blocks and their
-- transactions from the LeiosDB, through the connection's own reader.
leiosFetchServer ::
  MonadCatch m =>
  Tracer m TraceLeiosFetchServerEvent ->
  LeiosDbReader m ->
  LeiosFetchServerPeer LeiosPoint SerialisedEndorserBlock LeiosTx m ()
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
  m SerialisedEndorserBlock
serveEb tracer reader point = do
  -- TODO Disconnect a client that requests what the node never offered it.
  traceWith tracer $ TraceLeiosFetchServerBlockRequested point
  -- The one body's tx references, in order. A stored body is never empty, so
  -- no references means the node does not hold the body.
  lookupEbBody reader (pointEbHash point) >>= \case
    [] -> throwIO $ ExnLeiosUnknownBlockRequested point
    refs -> do
      let n = length refs
      traceWith tracer $ TraceLeiosFetchServerBlockServed point n
      -- In this node's preferred encoding; see the TODO on 'lookupEbBody'.
      pure $ serialiseEndorserBlock (mkEndorserBlock refs)

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
  -- One row for each requested offset that the endorser block has, with its
  -- bytes if the node holds them.
  rows <- batchRetrieveTxs reader (pointEbHash point) offsets
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
  offsets = bitmapOffsets bitmaps

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
