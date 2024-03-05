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
  , runBlockFetchServer
  , startBlockFetchLogic
  , startKeepAliveThread
  ) where

import           Control.Exception (SomeException)
import           Control.Monad (void)
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer.SI (MonadTimer)
import           Control.Tracer (Tracer, nullTracer, traceWith)
import           Data.Hashable (Hashable)
import           Data.Map.Strict (Map)
import           Network.TypedProtocol.Codec (AnyMessage)
import           Ouroboros.Consensus.Block (HasHeader)
import           Ouroboros.Consensus.Block.Abstract (Header, Point (..))
import qualified Ouroboros.Consensus.MiniProtocol.BlockFetch.ClientInterface as BlockFetchClientInterface
import           Ouroboros.Consensus.Node.ProtocolInfo
                     (NumCoreNodes (NumCoreNodes))
import           Ouroboros.Consensus.Storage.ChainDB.API
import           Ouroboros.Consensus.Util (ShowProxy)
import           Ouroboros.Consensus.Util.IOLike (Exception (fromException),
                     IOLike, STM, atomically, retry, try)
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..),
                     FetchClientRegistry, FetchMode (..), blockFetchLogic,
                     bracketFetchClient, bracketKeepAliveClient)
import           Ouroboros.Network.BlockFetch.Client (blockFetchClient)
import           Ouroboros.Network.Channel (Channel)
import           Ouroboros.Network.ControlMessage (ControlMessageSTM)
import           Ouroboros.Network.Driver (runPeer, runPipelinedPeer)
import           Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion,
                     isPipeliningEnabled)
import           Ouroboros.Network.Protocol.BlockFetch.Codec (codecBlockFetchId)
import           Ouroboros.Network.Protocol.BlockFetch.Server
                     (BlockFetchServer (..), blockFetchServerPeer)
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import           Test.Consensus.PeerSimulator.StateView
                     (PeerSimulatorComponentResult (..),
                     PeerSimulatorResult (..),
                     StateViewTracers (StateViewTracers, svtPeerSimulatorResultsTracer))
import           Test.Consensus.PeerSimulator.Trace (TraceEvent)
import           Test.Consensus.PointSchedule.Peers (PeerId)
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (BlockConfig (TestBlockConfig), TestBlock)
import           Test.Util.Time (dawnOfTime)

startBlockFetchLogic ::
     forall m peer.
     (Hashable peer, Ord peer, IOLike m)
  => ResourceRegistry m
  -> ChainDB m TestBlock
  -> FetchClientRegistry peer (Header TestBlock) TestBlock m
  -> STM m (Map peer (AnchoredFragment (Header TestBlock)))
  -> m ()
startBlockFetchLogic registry chainDb fetchClientRegistry getCandidates = do
    let slotForgeTime :: BlockFetchClientInterface.SlotForgeTimeOracle m blk
        slotForgeTime _ = pure dawnOfTime

        blockFetchConsensusInterface =
          BlockFetchClientInterface.mkBlockFetchConsensusInterface
            (TestBlockConfig $ NumCoreNodes 0) -- Only needed when minting blocks
            (BlockFetchClientInterface.defaultChainDbView chainDb)
            getCandidates
            -- The size of headers in bytes is irrelevant because our tests
            -- do not serialize the blocks.
            (\_hdr -> 1000)
            slotForgeTime
            (pure FetchModeBulkSync)

        -- Values taken from
        -- ouroboros-consensus-diffusion/src/unstable-diffusion-testlib/Test/ThreadNet/Network.hs
        blockFetchCfg = BlockFetchConfiguration
          { bfcMaxConcurrencyBulkSync = 1
          , bfcMaxConcurrencyDeadline = 2
          , bfcMaxRequestsInflight = 10
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

startKeepAliveThread ::
     forall m peer blk.
     (Ord peer, IOLike m)
  => ResourceRegistry m
  -> FetchClientRegistry peer (Header blk) blk m
  -> peer
  -> m ()
startKeepAliveThread registry fetchClientRegistry peerId =
    void $ forkLinkedThread registry "KeepAlive" $
      bracketKeepAliveClient fetchClientRegistry peerId $ \_ ->
        atomically retry

runBlockFetchClient ::
     (IOLike m, MonadTime m, MonadTimer m, HasHeader blk, HasHeader (Header blk), ShowProxy blk)
  => Tracer m (TraceEvent blk)
  -> PeerId
  -> StateViewTracers blk m
  -> FetchClientRegistry PeerId (Header blk) blk m
  -> ControlMessageSTM m
  -> Channel m (AnyMessage (BlockFetch blk (Point blk)))
  -> m ()
runBlockFetchClient _tracer peerId StateViewTracers {svtPeerSimulatorResultsTracer} fetchClientRegistry controlMsgSTM channel = do
    bracketFetchClient fetchClientRegistry ntnVersion isPipeliningEnabled peerId $ \clientCtx -> do
      res <- try $ runPipelinedPeer nullTracer codecBlockFetchId channel
        (blockFetchClient ntnVersion controlMsgSTM nullTracer clientCtx)
      case res of
        Right ((), msgRes) -> traceWith svtPeerSimulatorResultsTracer $
          PeerSimulatorResult peerId $ SomeBlockFetchClientResult $ Right msgRes
        Left exn -> do
          traceWith svtPeerSimulatorResultsTracer $
            PeerSimulatorResult peerId $ SomeBlockFetchClientResult $ Left exn
          -- NOTE: here we are able to trace exceptions, as what is done in `runChainSyncClient`
          case fromException exn of
            (_ :: Maybe SomeException) -> pure ()
  where
    ntnVersion :: NodeToNodeVersion
    ntnVersion = maxBound

runBlockFetchServer ::
  (IOLike m, ShowProxy blk) =>
  Tracer m (TraceEvent blk) ->
  PeerId ->
  StateViewTracers blk m ->
  BlockFetchServer blk (Point blk) m () ->
  Channel m (AnyMessage (BlockFetch blk (Point blk))) ->
  m ()
runBlockFetchServer _tracer peerId StateViewTracers {svtPeerSimulatorResultsTracer} server channel = do
  res <- try $ runPeer nullTracer codecBlockFetchId channel (blockFetchServerPeer server)
  case res of
    Right ((), msgRes) -> traceWith svtPeerSimulatorResultsTracer $
      PeerSimulatorResult peerId $ SomeBlockFetchServerResult $ Right msgRes
    Left exn -> do
      traceWith svtPeerSimulatorResultsTracer $
        PeerSimulatorResult peerId $ SomeBlockFetchServerResult $ Left exn
      -- NOTE: here we are able to trace exceptions, as what is done in `runChainSyncClient`
      case fromException exn of
        (_ :: Maybe SomeException) -> pure ()
