{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.PeerSimulator.ChainSync (runChainSyncClient) where

import           Control.Exception (AsyncException (ThreadKilled))
import           Control.Monad.Class.MonadTimer.SI (MonadTimer)
import           Control.Tracer (Tracer, nullTracer, traceWith)
import           Data.Map.Strict (Map)
import           Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import           Ouroboros.Consensus.Block (Header, Point)
import           Ouroboros.Consensus.Config (TopLevelConfig (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client (ChainDbView,
                     ChainSyncLoPBucketConfig, Consensus,
                     bracketChainSyncClient, chainSyncClient)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client as CSClient
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (Exception (fromException),
                     IOLike, MonadCatch (try), StrictTVar, uncheckedNewTVarM)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import           Ouroboros.Network.Block (Tip)
import           Ouroboros.Network.Channel (createConnectedChannels)
import           Ouroboros.Network.ControlMessage (ControlMessage (..))
import           Ouroboros.Network.Driver.Limits
                     (ProtocolLimitFailure (ExceededSizeLimit, ExceededTimeLimit))
import           Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion)
import           Ouroboros.Network.Protocol.ChainSync.ClientPipelined
                     (ChainSyncClientPipelined, chainSyncClientPeerPipelined)
import           Ouroboros.Network.Protocol.ChainSync.Codec (ChainSyncTimeout,
                     codecChainSyncId, timeLimitsChainSync)
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Protocol.ChainSync.Server (ChainSyncServer,
                     chainSyncServerPeer)
import           Test.Consensus.Network.Driver.Limits.Extras
                     (chainSyncNoSizeLimits,
                     runConnectedPeersPipelinedWithLimits)
import           Test.Consensus.PeerSimulator.StateView
                     (ChainSyncException (ChainSyncException),
                     StateViewTracers (StateViewTracers, svtChainSyncExceptionsTracer))
import           Test.Consensus.PeerSimulator.Trace (mkChainSyncClientTracer,
                     traceUnitWith)
import           Test.Consensus.PointSchedule.Peers (PeerId)
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock)

basicChainSyncClient :: forall m.
  IOLike m =>
  PeerId ->
  Tracer m String ->
  TopLevelConfig TestBlock ->
  ChainDbView m TestBlock ->
  StrictTVar m (AnchoredFragment (Header TestBlock)) ->
  (m (), m ()) ->
  (m (), m (), m ()) ->
  Consensus ChainSyncClientPipelined TestBlock m
basicChainSyncClient peerId tracer cfg chainDbView varCandidate (startIdling, stopIdling) (pauseLoPBucket, resumeLoPBucket, grantLoPToken) =
  chainSyncClient
    CSClient.ConfigEnv {
        CSClient.mkPipelineDecision0     = pipelineDecisionLowHighMark 10 20
      , CSClient.tracer                  = mkChainSyncClientTracer peerId tracer
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
      InFutureCheck.SomeHeaderInFutureCheck m TestBlock
    dummyHeaderInFutureCheck =
      InFutureCheck.SomeHeaderInFutureCheck InFutureCheck.HeaderInFutureCheck
      { InFutureCheck.proxyArrival = Proxy
      , InFutureCheck.recordHeaderArrival = \_ -> pure ()
      , InFutureCheck.judgeHeaderArrival = \_ _ _ -> pure ()
      , InFutureCheck.handleHeaderArrival = \_ -> pure Nothing
      }

runChainSyncClient ::
  (IOLike m, MonadTimer m) =>
  Tracer m String ->
  TopLevelConfig TestBlock ->
  ChainDbView m TestBlock ->
  PeerId ->
  ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m () ->
  ChainSyncTimeout ->
  ChainSyncLoPBucketConfig ->
  StateViewTracers m ->
  StrictTVar m (Map PeerId (StrictTVar m (AnchoredFragment (Header TestBlock)))) ->
  m ()
runChainSyncClient
  tracer
  cfg
  chainDbView
  peerId
  server
  chainSyncTimeouts
  lopBucketConfig
  StateViewTracers {svtChainSyncExceptionsTracer}
  varCandidates
  = do
    -- We don't need this shared Set yet. If we need it at some point,
    -- it ought to be passed to `runChainSyncClient`.
    varIdling <- uncheckedNewTVarM $ Set.empty
    bracketChainSyncClient nullTracer chainDbView varCandidates varIdling peerId ntnVersion lopBucketConfig $ \ varCandidate idleManagers lopBucket -> do
      res <- try $ runConnectedPeersPipelinedWithLimits
        createConnectedChannels
        nullTracer
        codecChainSyncId
        chainSyncNoSizeLimits
        (timeLimitsChainSync chainSyncTimeouts)
        (chainSyncClientPeerPipelined (basicChainSyncClient peerId tracer cfg chainDbView varCandidate idleManagers lopBucket))
        (chainSyncServerPeer server)
      case res of
        Right _ -> pure ()
        Left exn -> do
          traceWith svtChainSyncExceptionsTracer $ ChainSyncException peerId exn
          case fromException exn of
            Just (ExceededSizeLimit _) -> trace "Terminating because of size limit exceeded."
            Just (ExceededTimeLimit _) -> trace "Terminating because of time limit exceeded."
            Nothing -> pure ()
          case fromException exn of
            Just ThreadKilled -> trace "Terminated by GDD governor."
            _                 -> pure ()
          case fromException exn of
            Just CSClient.EmptyBucket -> trace "Terminating because of empty bucket."
            _ -> pure ()
  where
    ntnVersion :: NodeToNodeVersion
    ntnVersion = maxBound
    trace = traceUnitWith tracer $ "ChainSyncClient " ++ condense peerId
