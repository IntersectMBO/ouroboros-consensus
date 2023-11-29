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
import           Control.Tracer (nullTracer)
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
import           Ouroboros.Consensus.Util.IOLike (IOLike, STM, atomically,
                     retry)
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..),
                     FetchClientRegistry, FetchMode (..), blockFetchLogic,
                     bracketFetchClient, bracketKeepAliveClient)
import           Ouroboros.Network.BlockFetch.Client (blockFetchClient)
import           Ouroboros.Network.ControlMessage (ControlMessageSTM)
import           Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion,
                     isPipeliningEnabled)
import           Ouroboros.Network.Protocol.BlockFetch.Codec (codecBlockFetchId)
import           Ouroboros.Network.Protocol.BlockFetch.Server
                     (BlockFetchServer (..), blockFetchServerPeer)
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
  -> FetchClientRegistry peer (Header TestBlock) TestBlock m
  -> ControlMessageSTM m
  -> BlockFetchServer TestBlock (Point TestBlock) m ()
  -> m ()
runBlockFetchClient peerId fetchClientRegistry controlMsgSTM server =
    bracketFetchClient fetchClientRegistry ntnVersion isPipeliningEnabled peerId $ \clientCtx -> do
      let bfClient = blockFetchClient ntnVersion controlMsgSTM nullTracer clientCtx
          bfServer = blockFetchServerPeer server

      fst <$> runConnectedPeersPipelined
              createConnectedChannels
              nullTracer
              codecBlockFetchId
              bfClient
              bfServer
  where
    ntnVersion :: NodeToNodeVersion
    ntnVersion = maxBound
