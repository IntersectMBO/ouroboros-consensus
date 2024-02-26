{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.PeerSimulator.ScheduledBlockFetchServer (
    BlockFetch (..)
  , BlockFetchServerHandlers (..)
  , ScheduledBlockFetchServer (..)
  , SendBlocks (..)
  , runScheduledBlockFetchServer
  ) where

import           Control.Tracer
import           Ouroboros.Consensus.Block (Point)
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadSTM (STM))
import           Ouroboros.Network.BlockFetch.ClientState (ChainRange)
import           Ouroboros.Network.Protocol.BlockFetch.Server
import           Test.Consensus.PeerSimulator.ScheduledServer
                     (ScheduledServer (..), awaitOnlineState, runHandler)
import           Test.Consensus.PeerSimulator.Trace
import           Test.Consensus.PointSchedule (NodeState)
import           Test.Consensus.PointSchedule.Peers (PeerId)
import           Test.Util.TestBlock (TestBlock)

-- | Return values for the 'handlerSendBlocks'.
data SendBlocks =
  SendBlock TestBlock [TestBlock]
  |
  BatchDone

-- | Return values for the 'handlerBlockFetch'.
data BlockFetch =
  StartBatch [TestBlock]
  -- ^ As a response to the client request, we should send the blocks in the
  -- given batch.
  |
  NoBlocks
  -- ^ Negative response to the client's request for blocks.
  deriving (Eq, Show)

-- | Handlers for the scheduled BlockFetch server.
data BlockFetchServerHandlers m state =
  BlockFetchServerHandlers {
    bfshBlockFetch :: ChainRange (Point TestBlock) -> state -> STM m (Maybe BlockFetch, [TraceScheduledBlockFetchServerEvent state TestBlock]),
    bfshSendBlocks :: [TestBlock] -> state -> STM m (Maybe SendBlocks, [TraceScheduledBlockFetchServerEvent state TestBlock])
  }

-- | Resources used by a scheduled BlockFetch server. This comprises a generic
-- 'ScheduledServer' and BlockFetch-specific handlers.
data ScheduledBlockFetchServer m state =
  ScheduledBlockFetchServer {
    sbfsServer   :: ScheduledServer m state,
    sbfsTracer   :: Tracer m (TraceScheduledBlockFetchServerEvent state TestBlock),
    sbfsHandlers :: BlockFetchServerHandlers m state
  }

-- | Make a 'BlockFetchServer' able to run with the normal infrastructure from a
-- 'ScheduledBlockFetchServer'.
scheduledBlockFetchServer ::
  forall m a .
  IOLike m =>
  ScheduledBlockFetchServer m a ->
  BlockFetchServer TestBlock (Point TestBlock) m ()
scheduledBlockFetchServer ScheduledBlockFetchServer {sbfsServer, sbfsTracer, sbfsHandlers} =
  server
  where
    server = BlockFetchServer blockFetch ()

    BlockFetchServerHandlers {bfshBlockFetch, bfshSendBlocks} = sbfsHandlers

    blockFetch range =
      runHandler sbfsServer "BlockFetch" (bfshBlockFetch range) sbfsTracer $ \case
        StartBatch blocks -> do
          trace $ TraceSendingBlocks blocks
          pure $ SendMsgStartBatch (sendBlocks blocks)
        NoBlocks -> do
          trace $ TraceNoBlocks
          pure (SendMsgNoBlocks (server <$ awaitOnlineState sbfsServer))

    sendBlocks bs =
      runHandler sbfsServer "SendBlocks" (bfshSendBlocks bs) sbfsTracer $ \case
        SendBlock blk blks -> pure (SendMsgBlock blk (sendBlocks blks))
        BatchDone -> pure (SendMsgBatchDone (pure server))

    trace = traceWith sbfsTracer

-- | Construct a BlockFetch server for the peer simulator.
--
-- See 'scheduledBlockFetchServer'.
runScheduledBlockFetchServer ::
  IOLike m =>
  PeerId ->
  STM m () ->
  STM m (Maybe (NodeState TestBlock)) ->
  Tracer m (TraceEvent TestBlock) ->
  BlockFetchServerHandlers m (NodeState TestBlock) ->
  BlockFetchServer TestBlock (Point TestBlock) m ()
runScheduledBlockFetchServer ssPeerId ssTickStarted ssCurrentState tracer sbfsHandlers =
  scheduledBlockFetchServer ScheduledBlockFetchServer {
    sbfsServer = ScheduledServer {
      ssPeerId,
      ssTickStarted,
      ssCurrentState,
      ssCommonTracer = Tracer (traceWith tracer . TraceScheduledBlockFetchServerEvent ssPeerId . TraceHandlerEventBF)
    },
    sbfsTracer = Tracer (traceWith tracer . TraceScheduledBlockFetchServerEvent ssPeerId),
    sbfsHandlers
  }
