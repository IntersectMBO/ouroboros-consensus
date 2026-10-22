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
import Codec.CBOR.Read (deserialiseFromBytes)
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntSet as IntSet
import qualified Data.Vector.Strict as V
import Ouroboros.Consensus.Leios.Types
  ( LeiosEb (..)
  , LeiosPoint (..)
  , LeiosTx (..)
  , decodeLeiosEb
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client
  ( LeiosFetchClientPeer
  , leiosFetchClientPeerIdle
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
import Ouroboros.Consensus.Network.LeiosFetch (leiosFetchMiniProtocolNum, maxLeiosFetchIngressBytes)
import Ouroboros.Consensus.Storage.LeiosDB.API (LeiosDbReader (..))
import Ouroboros.Consensus.Storage.LeiosDB.Exception (throwLeiosDbException)
import Ouroboros.Consensus.Util.IOLike
  ( Exception
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
  m LeiosEb
serveEb tracer reader point = do
  -- TODO Disconnect a client that requests what the node never offered it.
  traceWith tracer $ TraceLeiosFetchServerBlockRequested point
  lookupEbBody reader (pointEbHash point) >>= \case
    Nothing -> throwIO $ ExnLeiosUnknownBlockRequested point
    Just bytes -> do
      -- TODO Serve the stored bytes as they are, instead of decoding them only
      -- for the codec to encode them again.
      eb <- case deserialiseFromBytes decodeLeiosEb (BSL.fromStrict bytes) of
        Right (_, eb) -> pure eb
        Left err -> throwLeiosDbException ("stored endorser block body does not decode: " <> show err)
      traceWith tracer $ TraceLeiosFetchServerBlockServed point (V.length (leiosEbTxs eb))
      pure eb

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
  -- One row for each requested offset that the endorser block has. A tx the
  -- node does not hold yet reads as zeroes: what the node offered, it holds.
  rows <- batchRetrieveTxs reader (pointEbHash point) offsets
  if length rows == length offsets
    then do
      let txs = [MkLeiosTx bytes | (_offset, bytes) <- rows]
          n = length txs
      traceWith tracer $
        TraceLeiosFetchServerTxsServed point n (sum [BS.length (leiosTxBytes tx) | tx <- txs])
      pure $ V.fromListN n txs
    else do
      let declared = IntSet.fromList [offset | (offset, _bytes) <- rows]
      throwIO $ ExnLeiosUnknownTxsRequested point (filter (`IntSet.notMember` declared) offsets)
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
