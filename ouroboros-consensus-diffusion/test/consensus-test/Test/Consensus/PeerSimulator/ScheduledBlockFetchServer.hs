{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.PeerSimulator.ScheduledBlockFetchServer (
    BlockFetch (..)
  , ScheduledBlockFetchServer (..)
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

-- | Return values for the 'handlerBlockFetch'.
data BlockFetch =
  StartBatch [TestBlock]
  -- ^ As a response to the client request, we should send the blocks in the
  -- given batch.
  |
  NoBlocks
  -- ^ Negative response to the client's request for blocks.
  deriving (Eq, Show)

-- | Resources used by a BlockFetch server mock.
data ScheduledBlockFetchServer m a =
  ScheduledBlockFetchServer {
    sbfsServer  :: ScheduledServer m a,
    sbfsHandler :: ChainRange (Point TestBlock) -> a -> STM m (Maybe BlockFetch, [String])
  }

scheduledBlockFetchServer ::
  forall m a .
  Condense a =>
  IOLike m =>
  ScheduledBlockFetchServer m a ->
  BlockFetchServer TestBlock (Point TestBlock) m ()
scheduledBlockFetchServer ScheduledBlockFetchServer {sbfsServer, sbfsHandler} =
  server
  where
    server = BlockFetchServer blockFetch ()

    blockFetch range =
      runHandler sbfsServer "BlockFetch" (sbfsHandler range) $ \case
        StartBatch blocks -> do
          trace $ "  sending blocks: " ++ intercalate " " (terseBlock <$> blocks)
          trace "done handling BlockFetch"
          pure $ SendMsgStartBatch (sendBlocks blocks)
        NoBlocks -> do
          trace "  no blocks available"
          trace "done handling BlockFetch"
          pure (SendMsgNoBlocks (server <$ awaitOnlineState sbfsServer))

    sendBlocks :: [TestBlock] -> m (BlockFetchSendBlocks TestBlock (Point TestBlock) m ())
    sendBlocks = \case
      []         -> do
        trace "Batch done"
        pure (SendMsgBatchDone (pure server))
      blk : blks -> do
        trace ("Sending " ++ terseBlock blk)
        pure (SendMsgBlock blk (sendBlocks blks))

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
  (ChainRange (Point TestBlock) -> a -> STM m (Maybe BlockFetch, [String])) ->
  BlockFetchServer TestBlock (Point TestBlock) m ()
runScheduledBlockFetchServer ssPeerId ssTickStarted ssCurrentState tracer sbfsHandler =
  scheduledBlockFetchServer ScheduledBlockFetchServer {
    sbfsServer = ScheduledServer {
      ssPeerId,
      ssTickStarted,
      ssCurrentState,
      ssTracer = Tracer (traceUnitWith tracer ("ScheduledBlockFetchServer " ++ ssPeerId))
    },
    sbfsHandler
  }
