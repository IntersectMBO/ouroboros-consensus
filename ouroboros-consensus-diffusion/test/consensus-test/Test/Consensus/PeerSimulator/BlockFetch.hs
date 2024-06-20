{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- | Functions that call to the BlockFetch API to start clients and servers
module Test.Consensus.PeerSimulator.BlockFetch (
    blockFetchNoTimeouts
  , runBlockFetchClient
  , runBlockFetchServer
  , startBlockFetchLogic
  , startKeepAliveThread
  ) where

import           Control.Exception (SomeException)
import           Control.Monad (void)
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer.SI (MonadTimer)
import           Control.ResourceRegistry
import           Control.Tracer (Tracer, nullTracer, traceWith)
import           Data.Functor.Contravariant ((>$<))
import           Data.Map.Strict (Map)
import           Network.TypedProtocol.Codec (AnyMessage, PeerHasAgency (..),
                     PeerRole)
import           Ouroboros.Consensus.Block (HasHeader)
import           Ouroboros.Consensus.Block.Abstract (Header, Point (..))
import qualified Ouroboros.Consensus.MiniProtocol.BlockFetch.ClientInterface as BlockFetchClientInterface
import           Ouroboros.Consensus.Node.ProtocolInfo
                     (NumCoreNodes (NumCoreNodes))
import           Ouroboros.Consensus.Storage.ChainDB.API
import           Ouroboros.Consensus.Util (ShowProxy)
import           Ouroboros.Consensus.Util.IOLike (DiffTime,
                     Exception (fromException), IOLike, STM, atomically, retry,
                     try)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..),
                     FetchClientRegistry, FetchMode (..), blockFetchLogic,
                     bracketFetchClient, bracketKeepAliveClient)
import           Ouroboros.Network.BlockFetch.Client (blockFetchClient)
import           Ouroboros.Network.Channel (Channel)
import           Ouroboros.Network.ControlMessage (ControlMessageSTM)
import           Ouroboros.Network.Driver (runPeer)
import           Ouroboros.Network.Driver.Limits
                     (ProtocolLimitFailure (ExceededSizeLimit, ExceededTimeLimit),
                     runPipelinedPeerWithLimits)
import           Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion,
                     isPipeliningEnabled)
import           Ouroboros.Network.Protocol.BlockFetch.Codec
                     (byteLimitsBlockFetch, codecBlockFetchId)
import           Ouroboros.Network.Protocol.BlockFetch.Server
                     (BlockFetchServer (..), blockFetchServerPeer)
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch (..),
                     ClientHasAgency (..), ServerHasAgency (..))
import           Ouroboros.Network.Protocol.Limits (ProtocolSizeLimits (..),
                     ProtocolTimeLimits (..), waitForever)
import           Test.Consensus.PeerSimulator.StateView
                     (PeerSimulatorComponentResult (..),
                     PeerSimulatorResult (..),
                     StateViewTracers (StateViewTracers, svtPeerSimulatorResultsTracer))
import           Test.Consensus.PeerSimulator.Trace
                     (TraceBlockFetchClientTerminationEvent (..),
                     TraceEvent (..))
import           Test.Consensus.PointSchedule (BlockFetchTimeout (..))
import           Test.Consensus.PointSchedule.Peers (PeerId)
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (BlockConfig (TestBlockConfig), TestBlock)
import           Test.Util.Time (dawnOfTime)

startBlockFetchLogic ::
     forall m.
     (IOLike m)
  => ResourceRegistry m
  -> Tracer m (TraceEvent TestBlock)
  -> ChainDB m TestBlock
  -> FetchClientRegistry PeerId (Header TestBlock) TestBlock m
  -> STM m (Map PeerId (AnchoredFragment (Header TestBlock)))
  -> m ()
startBlockFetchLogic registry tracer chainDb fetchClientRegistry getCandidates = do
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
            -- Initially, we tried FetchModeBulkSync, but adversaries had the
            -- opportunity to delay syncing by not responding to block requests.
            -- The BlockFetch logic would then wait for the timeout to expire
            -- before trying to download the block from another peer.
            (pure FetchModeDeadline)

        -- Values taken from
        -- ouroboros-consensus-diffusion/src/unstable-diffusion-testlib/Test/ThreadNet/Network.hs
        blockFetchCfg = BlockFetchConfiguration
          { -- We set a higher value here to allow downloading blocks from all
            -- peers.
            --
            -- If the value is too low, block downloads from a peer may prevent
            -- blocks from being downloaded from other peers. This can be
            -- problematic, since the batch download of a simulated BlockFetch
            -- server can last serveral ticks if the block pointer is not
            -- advanced to allow completion of the batch.
            --
            bfcMaxConcurrencyBulkSync = 50
          , bfcMaxConcurrencyDeadline = 50
          , bfcMaxRequestsInflight = 10
          , bfcDecisionLoopInterval = 0
          , bfcSalt = 0
          }

    void $ forkLinkedThread registry "BlockFetchLogic" $
      blockFetchLogic
        decisionTracer
        nullTracer
        blockFetchConsensusInterface
        fetchClientRegistry
        blockFetchCfg
  where
    decisionTracer = TraceOther . ("BlockFetchLogic | " ++) . show >$< tracer

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
  -> BlockFetchTimeout
  -> StateViewTracers blk m
  -> FetchClientRegistry PeerId (Header blk) blk m
  -> ControlMessageSTM m
  -> Channel m (AnyMessage (BlockFetch blk (Point blk)))
     -- ^ Send and receive message via the given 'Channel'.
  -> m ()
runBlockFetchClient tracer peerId blockFetchTimeouts StateViewTracers {svtPeerSimulatorResultsTracer} fetchClientRegistry controlMsgSTM channel = do
    bracketFetchClient fetchClientRegistry ntnVersion isPipeliningEnabled peerId $ \clientCtx -> do
      res <-
        try $
          runPipelinedPeerWithLimits
            nullTracer
            codecBlockFetchId
            blockFetchNoSizeLimits
            (timeLimitsBlockFetch blockFetchTimeouts)
            channel
            (blockFetchClient ntnVersion controlMsgSTM nullTracer clientCtx)
      case res of
        Right ((), msgRes) -> traceWith svtPeerSimulatorResultsTracer $
          PeerSimulatorResult peerId $ SomeBlockFetchClientResult $ Right msgRes
        Left exn -> do
          traceWith svtPeerSimulatorResultsTracer $
            PeerSimulatorResult peerId $ SomeBlockFetchClientResult $ Left exn
          case fromException exn of
            Just (ExceededSizeLimit _) ->
              traceWith tracer $ TraceBlockFetchClientTerminationEvent peerId TraceExceededSizeLimitBF
            Just (ExceededTimeLimit _) ->
              traceWith tracer $ TraceBlockFetchClientTerminationEvent peerId TraceExceededTimeLimitBF
            Nothing -> pure ()
  where
    ntnVersion :: NodeToNodeVersion
    ntnVersion = maxBound

blockFetchNoSizeLimits :: ProtocolSizeLimits (BlockFetch block point) bytes
blockFetchNoSizeLimits = byteLimitsBlockFetch (const 0)

-- | Same as 'timeLimitsChainSync' for BlockFetch. NOTE: There exists a
-- @timeLimitsBlockFetch@ in 'Ouroboros.Network.Protocol.BlockFetch.Codec' but
-- it does not allow customising the values as 'timeLimitsChainSync' does.
-- REVIEW: Should this be upstreamed to `ouroboros-network-protocols`?
timeLimitsBlockFetch :: forall block point. BlockFetchTimeout -> ProtocolTimeLimits (BlockFetch block point)
timeLimitsBlockFetch BlockFetchTimeout{busyTimeout, streamingTimeout} =
  ProtocolTimeLimits stateToLimit
  where
    stateToLimit :: forall (pr :: PeerRole) (st :: BlockFetch block point).
                    PeerHasAgency pr st -> Maybe DiffTime
    stateToLimit (ClientAgency TokIdle)      = waitForever
    stateToLimit (ServerAgency TokBusy)      = busyTimeout
    stateToLimit (ServerAgency TokStreaming) = streamingTimeout

blockFetchNoTimeouts :: BlockFetchTimeout
blockFetchNoTimeouts =
  BlockFetchTimeout
    { busyTimeout = Nothing,
      streamingTimeout = Nothing
    }

runBlockFetchServer ::
  (IOLike m, ShowProxy blk) =>
  Tracer m (TraceEvent blk) ->
  PeerId ->
  StateViewTracers blk m ->
  BlockFetchServer blk (Point blk) m () ->
  Channel m (AnyMessage (BlockFetch blk (Point blk))) ->
  -- ^ Send and receive message via the given 'Channel'.
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
