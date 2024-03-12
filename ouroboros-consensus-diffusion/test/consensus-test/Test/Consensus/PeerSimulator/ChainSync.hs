{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.PeerSimulator.ChainSync (
    chainSyncNoSizeLimits
  , chainSyncNoTimeouts
  , runChainSyncClient
  , runChainSyncServer
  ) where

import           Control.Exception (AsyncException (ThreadKilled),
                     SomeException)
import           Control.Monad.Class.MonadTimer.SI (MonadTimer)
import           Control.Tracer (Tracer (Tracer), nullTracer, traceWith)
import           Data.Map.Strict (Map)
import           Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import           Network.TypedProtocol.Codec (AnyMessage)
import           Ouroboros.Consensus.Block (Header, Point)
import           Ouroboros.Consensus.Config (TopLevelConfig (..))
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (ChainDbView,
                     ChainSyncLoPBucketConfig, Consensus,
                     bracketChainSyncClient, chainSyncClient)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as CSClient
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import           Ouroboros.Consensus.Util (ShowProxy)
import           Ouroboros.Consensus.Util.IOLike (Exception (fromException),
                     IOLike, MonadCatch (try), StrictTVar, uncheckedNewTVarM)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.Block (Tip)
import           Ouroboros.Network.Channel (Channel)
import           Ouroboros.Network.ControlMessage (ControlMessage (..))
import           Ouroboros.Network.Driver (runPeer)
import           Ouroboros.Network.Driver.Limits
                     (ProtocolLimitFailure (ExceededSizeLimit, ExceededTimeLimit),
                     runPipelinedPeerWithLimits)
import           Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion)
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
                     (ChainSyncClientPipelined, chainSyncClientPeerPipelined)
import           Ouroboros.Network.Protocol.ChainSync.Codec
                     (ChainSyncTimeout (..), byteLimitsChainSync,
                     codecChainSyncId, timeLimitsChainSync)
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Protocol.ChainSync.Server (ChainSyncServer,
                     chainSyncServerPeer)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.Limits (ProtocolSizeLimits (..))
import           Test.Consensus.PeerSimulator.StateView
                     (PeerSimulatorComponentResult (..),
                     PeerSimulatorResult (..),
                     StateViewTracers (StateViewTracers, svtPeerSimulatorResultsTracer))
import           Test.Consensus.PeerSimulator.Trace
                     (TraceChainSyncClientTerminationEvent (..),
                     TraceEvent (..))
import           Test.Consensus.PointSchedule.Peers (PeerId)
import           Test.Util.Orphans.IOLike ()

-- | A basic ChainSync client. It wraps around 'chainSyncClient', but simplifies
-- quite a few aspects. In particular, the size of the pipeline cannot exceed 20
-- messages and the “in future” checks are disabled.
basicChainSyncClient ::
  forall m blk.
  (IOLike m, LedgerSupportsProtocol blk) =>
  PeerId ->
  Tracer m (TraceEvent blk) ->
  TopLevelConfig blk ->
  ChainDbView m blk ->
  StrictTVar m (AnchoredFragment (Header blk)) ->
  -- ^ A TVar containing the fragment of headers for that peer, kept up to date
  -- by the ChainSync client.
  (m (), m ()) ->
  -- ^ Two monadic actions called when reaching and leaving @StIdle@.
  (m (), m (), m ()) ->
  -- ^ Three monadic actions called to pause and resume the LoP bucket and to
  -- add a token to the LoP bucket.
  Consensus ChainSyncClientPipelined blk m
basicChainSyncClient peerId tracer cfg chainDbView varCandidate (startIdling, stopIdling) (pauseLoPBucket, resumeLoPBucket, grantLoPToken) =
  chainSyncClient
    CSClient.ConfigEnv {
        CSClient.mkPipelineDecision0     = pipelineDecisionLowHighMark 10 20
      , CSClient.tracer                  = Tracer (traceWith tracer . TraceChainSyncClientEvent peerId)
      , CSClient.cfg
      , CSClient.chainDbView
      , CSClient.someHeaderInFutureCheck = dummyHeaderInFutureCheck
      }
    CSClient.DynamicEnv {
        CSClient.version             = maxBound
      , CSClient.controlMessageSTM   = return Continue
      , CSClient.headerMetricsTracer = nullTracer
      , CSClient.varCandidate
      , CSClient.startIdling
      , CSClient.stopIdling
      , CSClient.pauseLoPBucket
      , CSClient.resumeLoPBucket
      , CSClient.grantLoPToken
      }
  where
    dummyHeaderInFutureCheck ::
      InFutureCheck.SomeHeaderInFutureCheck m blk
    dummyHeaderInFutureCheck =
      InFutureCheck.SomeHeaderInFutureCheck InFutureCheck.HeaderInFutureCheck
      { InFutureCheck.proxyArrival = Proxy
      , InFutureCheck.recordHeaderArrival = \_ -> pure ()
      , InFutureCheck.judgeHeaderArrival = \_ _ _ -> pure ()
      , InFutureCheck.handleHeaderArrival = \_ -> pure Nothing
      }

-- | Create and run a ChainSync client using 'bracketChainSyncClient' and
-- 'basicChainSyncClient', synchronously. Exceptions are caught, sent to the
-- 'StateViewTracers' and logged.
runChainSyncClient ::
  (IOLike m, MonadTimer m, LedgerSupportsProtocol blk, ShowProxy blk, ShowProxy (Header blk)) =>
  Tracer m (TraceEvent blk) ->
  TopLevelConfig blk ->
  ChainDbView m blk ->
  PeerId ->
  -- ^ The id of the peer to which the client connects.
  ChainSyncTimeout ->
  -- ^ Timeouts for this client.
  ChainSyncLoPBucketConfig ->
  -- ^ Configuration for the LoP bucket.
  StateViewTracers blk m ->
  -- ^ Tracers used to record information for the future 'StateView'.
  StrictTVar m (Map PeerId (StrictTVar m (AnchoredFragment (Header blk)))) ->
  -- ^ A TVar containing a map of fragments of headers for each peer. This
  -- function will (via 'bracketChainSyncClient') register and de-register a
  -- TVar for the fragment of the peer.
  Channel m (AnyMessage (ChainSync (Header blk) (Point blk) (Tip blk))) ->
  m ()
runChainSyncClient
  tracer
  cfg
  chainDbView
  peerId
  chainSyncTimeouts
  lopBucketConfig
  StateViewTracers {svtPeerSimulatorResultsTracer}
  varCandidates
  channel = do
    -- We don't need this shared Set yet. If we need it at some point,
    -- it ought to be passed to `runChainSyncClient`.
    varIdling <- uncheckedNewTVarM $ Set.empty
    bracketChainSyncClient
      nullTracer
      chainDbView
      varCandidates
      varIdling
      peerId
      (maxBound :: NodeToNodeVersion)
      lopBucketConfig
      $ \varCandidate idleManagers lopBucket -> do
        res <-
          try $
            runPipelinedPeerWithLimits
              nullTracer
              codecChainSyncId
              chainSyncNoSizeLimits
              (timeLimitsChainSync chainSyncTimeouts)
              channel
              (chainSyncClientPeerPipelined (basicChainSyncClient peerId tracer cfg chainDbView varCandidate idleManagers lopBucket))
        case res of
          Right res' -> traceWith svtPeerSimulatorResultsTracer $
            PeerSimulatorResult peerId $ SomeChainSyncClientResult $ Right res'
          Left exn -> traceException exn
    where
      traceException exn = do
        traceWith svtPeerSimulatorResultsTracer $
          PeerSimulatorResult peerId $ SomeChainSyncClientResult $ Left exn
        case fromException exn of
          Just (ExceededSizeLimit _) ->
            traceWith tracer $ TraceChainSyncClientTerminationEvent peerId TraceExceededSizeLimitCS
          Just (ExceededTimeLimit _) ->
            traceWith tracer $ TraceChainSyncClientTerminationEvent peerId TraceExceededTimeLimitCS
          Nothing -> pure ()
        case fromException exn of
          Just ThreadKilled ->
            traceWith tracer $ TraceChainSyncClientTerminationEvent peerId TraceTerminatedByGDDGovernor
          _ -> pure ()
        case fromException exn of
          Just CSClient.EmptyBucket ->
            traceWith tracer $ TraceChainSyncClientTerminationEvent peerId TraceTerminatedByLoP
          _ -> pure ()

chainSyncNoSizeLimits :: ProtocolSizeLimits (ChainSync header point tip) bytes
chainSyncNoSizeLimits = byteLimitsChainSync (const 0)

chainSyncNoTimeouts :: ChainSyncTimeout
chainSyncNoTimeouts =
  ChainSyncTimeout
    { canAwaitTimeout = Nothing,
      intersectTimeout = Nothing,
      mustReplyTimeout = Nothing,
      idleTimeout = Nothing
    }

runChainSyncServer ::
  (IOLike m, ShowProxy blk, ShowProxy (Header blk)) =>
  Tracer m (TraceEvent blk) ->
  PeerId ->
  StateViewTracers blk m ->
  ChainSyncServer (Header blk) (Point blk) (Tip blk) m () ->
  Channel m (AnyMessage (ChainSync (Header blk) (Point blk) (Tip blk))) ->
  m ()
runChainSyncServer _tracer peerId StateViewTracers {svtPeerSimulatorResultsTracer} server channel =
  (try $ runPeer nullTracer codecChainSyncId channel (chainSyncServerPeer server)) >>= \case
    Right ((), msgRes) -> traceWith svtPeerSimulatorResultsTracer $
      PeerSimulatorResult peerId $ SomeChainSyncServerResult $ Right msgRes
    Left exn -> do
      traceWith svtPeerSimulatorResultsTracer $
        PeerSimulatorResult peerId $ SomeChainSyncServerResult $ Left exn
      -- NOTE: here we are able to trace exceptions, as what is done in `runChainSyncClient`
      case fromException exn of
        (_ :: Maybe SomeException) -> pure ()
