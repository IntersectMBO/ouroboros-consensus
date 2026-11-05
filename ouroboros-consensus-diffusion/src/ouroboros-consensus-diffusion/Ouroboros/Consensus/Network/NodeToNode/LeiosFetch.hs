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
  , withLeiosFetchAtomicServer
  , catchAndRethrow
  ) where

import Cardano.Network.NodeToNode (addSafetyMargin)
import Cardano.Network.PeerSelection (PeerTrustable)
import qualified Codec.CBOR.Read as CBOR
import Control.Monad (unless, when)
import Control.Tracer (Tracer, traceWith)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Strict as V
import Data.Word (Word32, Word64)
import Ouroboros.Consensus.Leios.Types
  ( EndorserBlock
  , LeiosPoint (..)
  , LeiosTx (..)
  , decodeEndorserBlock
  , endorserBlockBytes
  , endorserBlockReferences
  , maxEndorserBlockBytesSize
  , temporaryEndorserBlockFromBytes
  , unsafeEndorserBlockForEncodeOnly
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server.State
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
  , maxLeiosTxsRequestBytesSize
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
  LeiosFetchClientPeerPipelined LeiosPoint EndorserBlock LeiosTx m ()
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

-- | Construct the opt-in atomic server around this connection's exclusive
-- reader. Only its one preparation task touches the reader. Run/join the
-- interpreter inside the action before releasing this scope or the reader.
-- Legacy scheduling and normal version advertisement are unchanged.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#deduplication-and-durable-acquisition@.
withLeiosFetchAtomicServer ::
  forall m a.
  IOLike m =>
  Tracer m TraceLeiosFetchServerEvent ->
  ServerLimits ->
  LeiosDbReader m ->
  (AtomicServer LeiosPoint EndorserBlock LeiosTx m -> m a) ->
  m a
withLeiosFetchAtomicServer tracer limits reader action = do
  let required = defaultServerLimits (serverDataLimit limits)
  unless
    ( serverBodyReplyLimit limits >= serverBodyReplyLimit required
        && serverTxReplyLimit limits >= serverTxReplyLimit required
    )
    $ throwIO InvalidServerLimits
  withAtomicServer limits (AtomicRequestHandler prepare) action
 where
  prepare ::
    forall busy.
    AtomicDataRequest LeiosPoint EndorserBlock LeiosTx busy ->
    m (PreparedReply LeiosPoint EndorserBlock LeiosTx busy)
  prepare = \case
    ServeBody _ point ->
      catchAndRethrow tracer TraceLeiosFetchServerInvalidRequest $
        catchAndRethrow tracer (TraceLeiosFetchServerBlockDbFailure point) $ do
          body <- serveEb tracer reader point
          let bytes = fromIntegral (BS.length (endorserBlockBytes body)) :: Word64
              encoded = 2 + bytes
          when (bytes > fromIntegral maxEndorserBlockBytesSize) $
            throwIO (ExnLeiosResponseTooLarge point encoded (maxEndorserBlockBytesSize + 2))
          checkEncoded point encoded (serverBodyReplyLimit limits)
          pure (PreparedBody (fromIntegral encoded) body)
    ServeTransactions _ point runs ->
      let bitmaps = NE.toList runs
          offsets = bitmapOffsets bitmaps
       in catchAndRethrow tracer TraceLeiosFetchServerInvalidRequest $
            catchAndRethrow tracer (TraceLeiosFetchServerTxsDbFailure point bitmaps) $ do
              -- Read bounded metadata before requesting payloads, so a bitmap cannot
              -- allocate a large vector and only then discover its byte overrun.
              raw <- lookupEbBody reader point >>= maybe (throwIO (ExnLeiosUnknownBlockRequested point)) pure
              when (toInteger (BS.length raw) > toInteger maxEndorserBlockBytesSize) $
                throwIO
                  (ExnLeiosResponseTooLarge point (fromIntegral (BS.length raw) + 2) (maxEndorserBlockBytesSize + 2))
              let input = BL.fromStrict raw
              body <- case CBOR.deserialiseFromBytes decodeEndorserBlock input of
                Right (rest, annotate) | BL.null rest -> pure (annotate input)
                Left err -> throwIO err
                _ -> throwIO ServerInvalidReply
              let refs = IntMap.fromDistinctAscList (zip [0 ..] (endorserBlockReferences body))
              sizes <-
                traverse
                  ( \off -> case IntMap.lookup off refs of
                      Nothing -> throwIO (ExnLeiosUnknownTxsRequested point [off])
                      Just (_, size) -> pure size
                  )
                  offsets
              let payload = sum (map fromIntegral sizes) :: Word64
                  encoded = txResponseBytes sizes
              when (payload > fromIntegral maxLeiosTxsRequestBytesSize) $
                throwIO (ExnLeiosTxsRequestTooLarge point payload maxLeiosTxsRequestBytesSize)
              checkEncoded point encoded (serverTxReplyLimit limits)
              txs <- serveTxs tracer reader point bitmaps
              unless (map (fromIntegral . BS.length . leiosTxBytes) (V.toList txs) == sizes) $
                throwIO ServerInvalidReply
              pure (PreparedTransactions (fromIntegral encoded) txs)
  checkEncoded point encoded maximumBytes =
    when (encoded > fromIntegral maximumBytes) $
      throwIO (ExnLeiosResponseTooLarge point encoded maximumBytes)

-- Tight CBOR: outer array/tag, inner array header, then tag 24 and a byte-string
-- header around each TX. Widen before sums; narrow only after checking the cap.
txResponseBytes :: [Word32] -> Word64
txResponseBytes sizes =
  2
    + cborHeaderBytes (fromIntegral (length sizes))
    + sum [fromIntegral n + 2 + cborHeaderBytes (fromIntegral n) | n <- sizes]

cborHeaderBytes :: Word64 -> Word64
cborHeaderBytes n
  | n < 24 = 1
  | n <= 0xff = 2
  | n <= 0xffff = 3
  | n <= 0xffffffff = 5
  | otherwise = 9

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
