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

data SendBlocks blk =
  SendBlock blk [blk]
  |
  BatchDone

data BlockFetch blk =
  StartBatch [blk]
  |
  NoBlocks

data BlockFetchServerHandlers m state blk =
  BlockFetchServerHandlers {
    bfshBlockFetch :: ChainRange (Point blk) -> state -> STM m (Maybe (BlockFetch blk), [TraceScheduledBlockFetchServerEvent state blk]),
    bfshSendBlocks :: [blk] -> state -> STM m (Maybe (SendBlocks blk), [TraceScheduledBlockFetchServerEvent state blk])
  }

-- | Resources used by a BlockFetch server mock.
data ScheduledBlockFetchServer m state blk =
  ScheduledBlockFetchServer {
    sbfsServer   :: ScheduledServer m state blk,
    sbfsTracer   :: Tracer m (TraceScheduledBlockFetchServerEvent state blk),
    sbfsHandlers :: BlockFetchServerHandlers m state blk
  }

scheduledBlockFetchServer ::
  forall m state blk.
  IOLike m =>
  ScheduledBlockFetchServer m state blk ->
  BlockFetchServer blk (Point blk) m ()
scheduledBlockFetchServer ScheduledBlockFetchServer {sbfsServer, sbfsTracer, sbfsHandlers} =
  server
  where
    server = BlockFetchServer blockFetch ()

    BlockFetchServerHandlers {bfshBlockFetch, bfshSendBlocks} = sbfsHandlers

    blockFetch range =
      runHandler sbfsServer "BlockFetch" (bfshBlockFetch range) sbfsTracer $ \case
        StartBatch blocks -> do
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
  STM m (Maybe (NodeState blk)) ->
  Tracer m (TraceEvent blk) ->
  BlockFetchServerHandlers m (NodeState blk) blk ->
  BlockFetchServer blk (Point blk) m ()
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
