{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The node's LeiosFetch client and server, and what the node-to-node wiring
-- needs to run them.
module Ouroboros.Consensus.Network.NodeToNode.LeiosFetch
  ( ExnLeiosInvalidRequest (..)
  , leiosFetchClient
  , leiosFetchMiniProtocol
  , leiosFetchServer
  ) where

import Cardano.Network.NodeToNode (addSafetyMargin)
import Cardano.Network.PeerSelection (PeerTrustable)
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import Data.Either (partitionEithers)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Strict as V
import Ouroboros.Consensus.Leios.Types
  ( LeiosEb (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client
  ( LeiosFetchClientPeer
  , leiosFetchClientPeerIdle
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Server
  ( LeiosFetchRequestHandler (..)
  , LeiosFetchServerPeer
  , leiosFetchServerPeer
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Trace (TraceLeiosFetchServerEvent (..))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
  ( Message (..)
  , TxBitmaps
  , bitmapOffsets
  )
import Ouroboros.Consensus.Network.LeiosFetch (leiosFetchMiniProtocolNum, maxLeiosFetchIngressBytes)
import Ouroboros.Consensus.Storage.LeiosDB.API (LeiosDbReader (..))
import Ouroboros.Consensus.Storage.LeiosDB.Exception (LeiosDbException)
import Ouroboros.Consensus.Util.IOLike
  ( Exception (..)
  , MonadCatch (..)
  , MonadSTM (..)
  , MonadThrow (..)
  )
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.Mux
  ( MiniProtocol (..)
  , MiniProtocolLimits (..)
  , MiniProtocolWithExpandedCtx
  , RunMiniProtocolWithExpandedCtx
  , StartOnDemandOrEagerly (..)
  )

-- | The node's LeiosFetch client, which requests nothing yet and ends when the
-- node demotes the peer.
leiosFetchClient ::
  MonadSTM m =>
  ControlMessageSTM m ->
  LeiosFetchClientPeer LeiosPoint LeiosEb LeiosTx m ()
leiosFetchClient controlMessageSTM =
  leiosFetchClientPeerIdle $
    controlMessageSTM >>= \case
      Continue -> retry
      Quiesce -> retry
      Terminate -> pure ()

-- | The node's LeiosFetch server, which serves endorser blocks and their
-- transactions from the LeiosDB, through the connection's own reader.
leiosFetchServer ::
  MonadCatch m =>
  Tracer m TraceLeiosFetchServerEvent ->
  LeiosDbReader m ->
  LeiosFetchServerPeer LeiosPoint LeiosEb LeiosTx m ()
leiosFetchServer tracer reader =
  leiosFetchServerPeer $ MkLeiosFetchRequestHandler $ \case
    MsgLeiosBlockRequest point -> MsgLeiosBlock <$> serveEb tracer reader point
    MsgLeiosBlockTxsRequest point bitmaps -> MsgLeiosBlockTxs <$> serveTxs tracer reader point bitmaps

serveEb ::
  MonadCatch m =>
  Tracer m TraceLeiosFetchServerEvent ->
  LeiosDbReader m ->
  LeiosPoint ->
  m LeiosEb
serveEb tracer reader point = do
  traceWith tracer $ TraceLeiosFetchServerBlockRequested point
  -- A stored body is never empty.
  traceDbExceptionAndThrow tracer point (lookupEbBody reader (pointEbHash point)) >>= \case
    [] -> do
      traceWith tracer $ TraceLeiosFetchServerUnknownBlock point
      throwIO $ ExnLeiosUnknownBlockRequested point
    refs -> do
      let n = length refs
      traceWith tracer $ TraceLeiosFetchServerBlockServed point n
      pure $ MkLeiosEb $ V.fromListN n refs

serveTxs ::
  MonadCatch m =>
  Tracer m TraceLeiosFetchServerEvent ->
  LeiosDbReader m ->
  LeiosPoint ->
  TxBitmaps ->
  m (V.Vector LeiosTx)
serveTxs tracer reader point bitmaps = do
  traceWith tracer $ TraceLeiosFetchServerTxsRequested point (length offsets)
  rows <- traceDbExceptionAndThrow tracer point (batchRetrieveTxs reader (pointEbHash point) offsets)
  let held = IntMap.fromList [(offset, MkLeiosTx bytes) | (offset, _txHash, Just bytes) <- rows]
  case partitionEithers [maybe (Left offset) Right (IntMap.lookup offset held) | offset <- offsets] of
    ([], txs) -> do
      let n = length txs
      traceWith tracer $
        TraceLeiosFetchServerTxsServed point n (sum [BS.length (leiosTxBytes tx) | tx <- txs])
      pure $ V.fromListN n txs
    (missing, _txs) -> do
      traceWith tracer $ TraceLeiosFetchServerUnknownTxs point missing
      throwIO $ ExnLeiosUnknownTxsRequested point missing
 where
  offsets = bitmapOffsets bitmaps

-- | Trace a 'LeiosDbException' that the action throws, then rethrow it.
traceDbExceptionAndThrow ::
  MonadCatch m =>
  Tracer m TraceLeiosFetchServerEvent ->
  LeiosPoint ->
  m a ->
  m a
traceDbExceptionAndThrow tracer point action =
  action `catch` \(e :: LeiosDbException) -> do
    traceWith tracer $ TraceLeiosFetchServerDbException point (displayException e)
    throwIO e

-- | Thrown by the node's LeiosFetch server, which disconnects a client that
-- requests what the node does not hold.
--
-- TODO Disconnect it also for requesting what the node never offered it.
data ExnLeiosInvalidRequest
  = -- | An endorser block whose body the node does not hold.
    ExnLeiosUnknownBlockRequested !LeiosPoint
  | -- | Offsets into an endorser block that it does not have, or whose
    -- transactions the node does not hold.
    ExnLeiosUnknownTxsRequested !LeiosPoint ![Int]
  deriving Show

instance Exception ExnLeiosInvalidRequest

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
