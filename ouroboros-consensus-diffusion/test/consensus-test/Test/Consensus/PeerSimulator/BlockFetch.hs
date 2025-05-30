{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functions that call to the BlockFetch API to start clients and servers
module Test.Consensus.PeerSimulator.BlockFetch
  ( blockFetchNoTimeouts
  , runBlockFetchClient
  , runBlockFetchServer
  , startBlockFetchLogic
  , startKeepAliveThread
  ) where

import Control.Monad (void)
import Control.Monad.Class.MonadTime
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.ResourceRegistry
import Control.Tracer (Tracer, nullTracer, traceWith)
import Data.Functor.Contravariant ((>$<))
import Network.TypedProtocol.Codec
  ( ActiveState
  , AnyMessage
  , StateToken
  , notActiveState
  )
import Ouroboros.Consensus.Block (HasHeader)
import Ouroboros.Consensus.Block.Abstract (Header, Point (..))
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.HeaderValidation (HeaderWithTime (..))
import qualified Ouroboros.Consensus.MiniProtocol.BlockFetch.ClientInterface as BlockFetchClientInterface
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( ChainSyncClientHandleCollection
  )
import Ouroboros.Consensus.Node.Genesis
  ( GenesisConfig (..)
  , enableGenesisConfigDefault
  )
import Ouroboros.Consensus.Node.ProtocolInfo
  ( NumCoreNodes (NumCoreNodes)
  )
import Ouroboros.Consensus.Storage.ChainDB.API
import Ouroboros.Consensus.Util (ShowProxy)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.BlockFetch
  ( BlockFetchConfiguration (..)
  , FetchClientRegistry
  , GenesisBlockFetchConfiguration (..)
  , blockFetchLogic
  , bracketFetchClient
  , bracketKeepAliveClient
  )
import Ouroboros.Network.BlockFetch.Client (blockFetchClient)
import Ouroboros.Network.BlockFetch.ConsensusInterface
  ( FetchMode (..)
  )
import Ouroboros.Network.Channel (Channel)
import Ouroboros.Network.ControlMessage (ControlMessageSTM)
import Ouroboros.Network.Driver (runPeer)
import Ouroboros.Network.Driver.Limits
  ( ProtocolLimitFailure (ExceededSizeLimit, ExceededTimeLimit)
  , runPipelinedPeerWithLimits
  )
import Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion)
import Ouroboros.Network.Protocol.BlockFetch.Codec
  ( byteLimitsBlockFetch
  , codecBlockFetchId
  )
import Ouroboros.Network.Protocol.BlockFetch.Server
  ( BlockFetchServer (..)
  , blockFetchServerPeer
  )
import Ouroboros.Network.Protocol.BlockFetch.Type
  ( BlockFetch (..)
  , SingBlockFetch (..)
  )
import Ouroboros.Network.Protocol.Limits
  ( ProtocolSizeLimits (..)
  , ProtocolTimeLimits (..)
  , waitForever
  )
import Test.Consensus.PeerSimulator.StateView
  ( PeerSimulatorComponentResult (..)
  , PeerSimulatorResult (..)
  , StateViewTracers (StateViewTracers, svtPeerSimulatorResultsTracer)
  )
import Test.Consensus.PeerSimulator.Trace
  ( TraceBlockFetchClientTerminationEvent (..)
  , TraceEvent (..)
  )
import Test.Consensus.PointSchedule (BlockFetchTimeout (..))
import Test.Consensus.PointSchedule.Peers (PeerId)
import Test.Util.Orphans.IOLike ()
import Test.Util.TestBlock (BlockConfig (TestBlockConfig), TestBlock)

startBlockFetchLogic ::
  forall m.
  (IOLike m, MonadTimer m) =>
  -- | Whether to enable chain selection starvation
  Bool ->
  ResourceRegistry m ->
  Tracer m (TraceEvent TestBlock) ->
  ChainDB m TestBlock ->
  FetchClientRegistry PeerId (HeaderWithTime TestBlock) TestBlock m ->
  ChainSyncClientHandleCollection PeerId m TestBlock ->
  m ()
startBlockFetchLogic enableChainSelStarvation registry tracer chainDb fetchClientRegistry csHandlesCol = do
  let blockFetchConsensusInterface =
        BlockFetchClientInterface.mkBlockFetchConsensusInterface
          nullTracer -- FIXME
          (TestBlockConfig $ NumCoreNodes 0) -- Only needed when minting blocks
          (BlockFetchClientInterface.defaultChainDbView chainDb)
          csHandlesCol
          -- The size of headers in bytes is irrelevant because our tests
          -- do not serialize the blocks.
          (\_hdr -> 1000)
          -- This is a syncing test, so we use 'FetchModeGenesis'.
          (pure FetchModeGenesis)
          DiffusionPipeliningOn

      bfcGenesisBFConfig =
        if enableChainSelStarvation
          then
            GenesisBlockFetchConfiguration
              { gbfcGracePeriod =
                  if enableChainSelStarvation
                    then
                      10 -- default value for cardano-node at the time of writing
                    else
                      1000000 -- (more than 11 days)
              }
          else gcBlockFetchConfig enableGenesisConfigDefault

      -- Values taken from
      -- ouroboros-consensus-diffusion/src/unstable-diffusion-testlib/Test/ThreadNet/Network.hs
      blockFetchCfg =
        BlockFetchConfiguration
          { bfcMaxConcurrencyBulkSync = 50
          , bfcMaxConcurrencyDeadline = 50 -- unused because of @pure FetchModeBulkSync@ above
          , bfcMaxRequestsInflight = 10
          , bfcDecisionLoopIntervalPraos = 0
          , bfcDecisionLoopIntervalGenesis = 0
          , bfcSalt = 0
          , bfcGenesisBFConfig
          }

  void $
    forkLinkedThread registry "BlockFetchLogic" $
      blockFetchLogic
        decisionTracer
        nullTracer
        blockFetchConsensusInterface
        fetchClientRegistry
        blockFetchCfg
 where
  decisionTracer = TraceOther . ("BlockFetchLogic | " ++) . show >$< tracer

startKeepAliveThread ::
  forall m peer blk hdr.
  (Ord peer, IOLike m) =>
  ResourceRegistry m ->
  FetchClientRegistry peer hdr blk m ->
  peer ->
  m ()
startKeepAliveThread registry fetchClientRegistry peerId =
  void $
    forkLinkedThread registry "KeepAlive" $
      bracketKeepAliveClient fetchClientRegistry peerId $ \_ ->
        atomically retry

runBlockFetchClient ::
  (IOLike m, MonadTime m, MonadTimer m, HasHeader blk, HasHeader (Header blk), ShowProxy blk) =>
  Tracer m (TraceEvent blk) ->
  PeerId ->
  BlockFetchTimeout ->
  StateViewTracers blk m ->
  FetchClientRegistry PeerId (HeaderWithTime blk) blk m ->
  ControlMessageSTM m ->
  -- | Send and receive message via the given 'Channel'.
  Channel m (AnyMessage (BlockFetch blk (Point blk))) ->
  m ()
runBlockFetchClient tracer peerId blockFetchTimeouts StateViewTracers{svtPeerSimulatorResultsTracer} fetchClientRegistry controlMsgSTM channel = do
  bracketFetchClient fetchClientRegistry ntnVersion peerId $ \clientCtx -> do
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
      Right ((), msgRes) ->
        traceWith svtPeerSimulatorResultsTracer $
          PeerSimulatorResult peerId $
            SomeBlockFetchClientResult $
              Right msgRes
      Left exn -> do
        traceWith svtPeerSimulatorResultsTracer $
          PeerSimulatorResult peerId $
            SomeBlockFetchClientResult $
              Left exn
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
timeLimitsBlockFetch ::
  forall block point. BlockFetchTimeout -> ProtocolTimeLimits (BlockFetch block point)
timeLimitsBlockFetch BlockFetchTimeout{busyTimeout, streamingTimeout} =
  ProtocolTimeLimits stateToLimit
 where
  stateToLimit ::
    forall (st :: BlockFetch block point).
    ActiveState st => StateToken st -> Maybe DiffTime
  stateToLimit SingBFIdle = waitForever
  stateToLimit SingBFBusy = busyTimeout
  stateToLimit SingBFStreaming = streamingTimeout
  stateToLimit a@SingBFDone = notActiveState a

blockFetchNoTimeouts :: BlockFetchTimeout
blockFetchNoTimeouts =
  BlockFetchTimeout
    { busyTimeout = Nothing
    , streamingTimeout = Nothing
    }

runBlockFetchServer ::
  (IOLike m, ShowProxy blk) =>
  Tracer m (TraceEvent blk) ->
  PeerId ->
  StateViewTracers blk m ->
  BlockFetchServer blk (Point blk) m () ->
  -- | Send and receive message via the given 'Channel'.
  Channel m (AnyMessage (BlockFetch blk (Point blk))) ->
  m ()
runBlockFetchServer _tracer peerId StateViewTracers{svtPeerSimulatorResultsTracer} server channel = do
  res <- try $ runPeer nullTracer codecBlockFetchId channel (blockFetchServerPeer server)
  case res of
    Right ((), msgRes) ->
      traceWith svtPeerSimulatorResultsTracer $
        PeerSimulatorResult peerId $
          SomeBlockFetchServerResult $
            Right msgRes
    Left exn -> do
      traceWith svtPeerSimulatorResultsTracer $
        PeerSimulatorResult peerId $
          SomeBlockFetchServerResult $
            Left exn
      -- NOTE: here we are able to trace exceptions, as what is done in `runChainSyncClient`
      case fromException exn of
        (_ :: Maybe SomeException) -> pure ()
