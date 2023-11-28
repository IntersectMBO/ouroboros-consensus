{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Functions that call to the BlockFetch API to start clients and servers
module Test.Consensus.PeerSimulator.BlockFetch (
    runBlockFetchClient
  , startBlockFetchLogic
  , startKeepAliveThread
  ) where

import           Control.Monad (void)
import           Control.Monad.Class.MonadTime
import           Control.Tracer (Tracer, nullTracer, traceWith)
import           Data.Hashable (Hashable)
import           Data.Map.Strict (Map)
import           Network.TypedProtocol.Channel (createConnectedChannels)
import           Network.TypedProtocol.Driver.Simple
                     (runConnectedPeersPipelined)
import           Ouroboros.Consensus.Block.Abstract (Header, Point (..))
import qualified Ouroboros.Consensus.MiniProtocol.BlockFetch.ClientInterface as BlockFetchClientInterface
import           Ouroboros.Consensus.Node.ProtocolInfo
                     (NumCoreNodes (NumCoreNodes))
import           Ouroboros.Consensus.Storage.ChainDB.API
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.IOLike (IOLike, STM, atomically,
                     retry)
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..),
                     FetchClientRegistry, FetchMode (..), blockFetchLogic,
                     bracketFetchClient, bracketKeepAliveClient)
import           Ouroboros.Network.BlockFetch.Client (blockFetchClient)
import           Ouroboros.Network.ControlMessage (ControlMessageSTM)
import           Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion,
                     isPipeliningEnabled)
import           Ouroboros.Network.Protocol.BlockFetch.Codec (codecBlockFetchId)
import           Ouroboros.Network.Protocol.BlockFetch.Server
                     (BlockFetchBlockSender (SendMsgStartBatch),
                     BlockFetchSendBlocks (SendMsgBatchDone, SendMsgBlock),
                     BlockFetchServer (..), blockFetchServerPeer)
import           Ouroboros.Network.Protocol.BlockFetch.Type (ChainRange (..))
import           Test.Consensus.PeerSimulator.Trace (terseFrag)
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (BlockConfig (TestBlockConfig), TestBlock)
import           Test.Util.Time (dawnOfTime)


startBlockFetchLogic
  :: forall m peer.
     (Hashable peer, Ord peer, IOLike m)
  => ResourceRegistry m
  -> ChainDB m TestBlock
  -> FetchClientRegistry peer (Header TestBlock) TestBlock m
  -> STM m (Map peer (AnchoredFragment (Header TestBlock)))
  -> m ()
startBlockFetchLogic registry chainDb fetchClientRegistry getCandidates = do
    let slotForgeTime :: BlockFetchClientInterface.SlotForgeTimeOracle m TestBlock
        slotForgeTime _ = pure dawnOfTime

        blockFetchConsensusInterface =
          BlockFetchClientInterface.mkBlockFetchConsensusInterface
            (TestBlockConfig $ NumCoreNodes 0) -- Only needed when minting blocks
            (BlockFetchClientInterface.defaultChainDbView chainDb)
            getCandidates
            (\_hdr -> 1000)
            slotForgeTime
            (pure FetchModeBulkSync)

        blockFetchCfg = BlockFetchConfiguration
          { bfcMaxConcurrencyBulkSync = 2
          , bfcMaxConcurrencyDeadline = 2
          , bfcMaxRequestsInflight = 4
          , bfcDecisionLoopInterval = 0
          , bfcSalt = 0
          }

    void $ forkLinkedThread registry "BlockFetchLogic" $
      blockFetchLogic
        nullTracer
        nullTracer
        blockFetchConsensusInterface
        fetchClientRegistry
        blockFetchCfg

startKeepAliveThread
  :: forall m peer.
     (Ord peer, IOLike m)
  => ResourceRegistry m
  -> FetchClientRegistry peer (Header TestBlock) TestBlock m
  -> peer
  -> m ()
startKeepAliveThread registry fetchClientRegistry peerId =
    void $ forkLinkedThread registry "KeepAlive" $
      bracketKeepAliveClient fetchClientRegistry peerId $ \_ ->
        atomically retry

runBlockFetchClient
  :: (Ord peer, IOLike m, MonadTime m)
  => peer
  -> Tracer m String
  -> STM m ()
  -> FetchClientRegistry peer (Header TestBlock) TestBlock m
  -> ControlMessageSTM m
  -> m (AnchoredFragment TestBlock)
  -> m ()
runBlockFetchClient peerId tracer wait fetchClientRegistry controlMsgSTM getCurrentServerChain =
    bracketFetchClient fetchClientRegistry ntnVersion isPipeliningEnabled peerId $ \clientCtx -> do
      let bfClient = blockFetchClient ntnVersion controlMsgSTM nullTracer clientCtx
          bfServer = blockFetchServerPeer $ mockBlockFetchServer wait tracer getCurrentServerChain

      fst <$> runConnectedPeersPipelined
              createConnectedChannels
              nullTracer
              codecBlockFetchId
              bfClient
              bfServer
  where
    ntnVersion :: NodeToNodeVersion
    ntnVersion = maxBound

mockBlockFetchServer ::
     forall m.
     (IOLike m)
  => STM m ()
  -> Tracer m String
  -> m (AnchoredFragment TestBlock)
  -> BlockFetchServer TestBlock (Point TestBlock) m ()
mockBlockFetchServer wait tracer getCurrentChain = idle
  where
    idle :: BlockFetchServer TestBlock (Point TestBlock) m ()
    idle = BlockFetchServer check ()

    -- REVIEW: When the range does not match the current chain, we block until it doesn't.
    -- This means that the client will never be able to change the range, like when rolling back.
    check :: ChainRange (Point TestBlock) -> m (BlockFetchBlockSender TestBlock (Point TestBlock) m ())
    check (ChainRange from to) = do
      curChain <- getCurrentChain
      case AF.sliceRange curChain from to of
        Nothing    -> do
          trace ("Waiting for next tick for range: " ++ condense from)
          atomically wait
          trace "unblocked"
          check (ChainRange from to)
        Just slice -> do
          trace ("Sending slice " ++ terseFrag slice)
          pure (SendMsgStartBatch $ sendBlocks (AF.toOldestFirst slice))

    sendBlocks :: [TestBlock] -> m (BlockFetchSendBlocks TestBlock (Point TestBlock) m ())
    sendBlocks = \case
      []         -> do
        trace "Batch done"
        pure (SendMsgBatchDone (pure idle))
      blk : blks -> do
        trace ("Sending " ++ condense blk)
        pure (SendMsgBlock blk (sendBlocks blks))

    trace = traceWith tracer
