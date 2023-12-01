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
import           Data.List (intercalate)
import           Ouroboros.Consensus.Block (Point)
import           Ouroboros.Consensus.Util.Condense (Condense)
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadSTM (STM))
import           Ouroboros.Network.BlockFetch.ClientState (ChainRange)
import           Ouroboros.Network.Protocol.BlockFetch.Server
import           Test.Consensus.PeerSimulator.ScheduledServer
                     (ScheduledServer (..), awaitOnlineState, runHandler)
import           Test.Consensus.PeerSimulator.Trace
import           Test.Util.TestBlock (TestBlock)

-- | Return values for the 'handlerSendBlocks'.
data SendBlocks =
  SendBlock TestBlock [TestBlock]
  |
  BatchDone
  deriving (Eq, Show)

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
data BlockFetchServerHandlers m a =
  BlockFetchServerHandlers {
    bfshBlockFetch :: ChainRange (Point TestBlock) -> a -> STM m (Maybe BlockFetch, [String]),
    bfshSendBlocks :: [TestBlock] -> a -> STM m (Maybe SendBlocks, [String])
  }

-- | Resources used by a scheduled BlockFetch server. This comprises a generic
-- 'ScheduledServer' and BlockFetch-specific handlers.
data ScheduledBlockFetchServer m a =
  ScheduledBlockFetchServer {
    sbfsServer   :: ScheduledServer m a,
    sbfsHandlers :: BlockFetchServerHandlers m a
  }

-- | Make a 'BlockFetchServer' able to run with the normal infrastructure from a
-- 'ScheduledBlockFetchServer'.
scheduledBlockFetchServer ::
  forall m a .
  Condense a =>
  IOLike m =>
  ScheduledBlockFetchServer m a ->
  BlockFetchServer TestBlock (Point TestBlock) m ()
scheduledBlockFetchServer ScheduledBlockFetchServer {sbfsServer, sbfsHandlers} =
  server
  where
    server = BlockFetchServer blockFetch ()

    BlockFetchServerHandlers {bfshBlockFetch, bfshSendBlocks} = sbfsHandlers

    blockFetch range =
      runHandler sbfsServer "BlockFetch" (bfshBlockFetch range) $ \case
        StartBatch blocks -> do
          trace $ "  sending blocks: " ++ intercalate " " (terseBlock <$> blocks)
          trace "done handling BlockFetch"
          pure $ SendMsgStartBatch (sendBlocks blocks)
        NoBlocks -> do
          trace "  no blocks available"
          trace "done handling BlockFetch"
          pure (SendMsgNoBlocks (server <$ awaitOnlineState sbfsServer))

    sendBlocks bs =
      runHandler sbfsServer "SendBlocks" (bfshSendBlocks bs) $ \case
        SendBlock blk blks -> pure (SendMsgBlock blk (sendBlocks blks))
        BatchDone -> pure (SendMsgBatchDone (pure server))

    trace = traceWith (ssTracer sbfsServer)

-- | Construct a BlockFetch server for the peer simulator.
--
-- See 'scheduledBlockFetchServer'.
runScheduledBlockFetchServer ::
  Condense a =>
  IOLike m =>
  String ->
  STM m () ->
  STM m (Maybe a) ->
  Tracer m String ->
  BlockFetchServerHandlers m a ->
  BlockFetchServer TestBlock (Point TestBlock) m ()
runScheduledBlockFetchServer ssPeerId ssTickStarted ssCurrentState tracer sbfsHandlers =
  scheduledBlockFetchServer ScheduledBlockFetchServer {
    sbfsServer = ScheduledServer {
      ssPeerId,
      ssTickStarted,
      ssCurrentState,
      ssTracer = Tracer (traceUnitWith tracer ("ScheduledBlockFetchServer " ++ ssPeerId))
    },
    sbfsHandlers
  }
