-- | Options related to tracing
module Cardano.Node.Configuration.File.Tracing where

import Cardano.BM.Data.Tracer (TracingVerbosity)
import Cardano.Node.Configuration.Basics
import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Aeson.Key as Aeson
import Data.Aeson.Types
import Data.Proxy
import Data.Text
import qualified Data.Text as Text
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

type TraceAcceptPolicy = "TraceAcceptPolicy" :: Symbol
type TraceBlockchainTime = "TraceBlockchainTime" :: Symbol
type TraceBlockFetchClient = "TraceBlockFetchClient" :: Symbol
type TraceBlockFetchDecisions = "TraceBlockFetchDecisions" :: Symbol
type TraceBlockFetchProtocol = "TraceBlockFetchProtocol" :: Symbol
type TraceBlockFetchProtocolSerialised = "TraceBlockFetchProtocolSerialised" :: Symbol
type TraceBlockFetchServer = "TraceBlockFetchServer" :: Symbol
type TraceChainDB = "TraceChainDb" :: Symbol
type TraceChainSyncClient = "TraceChainSyncClient" :: Symbol
type TraceChainSyncBlockServer = "TraceChainSyncBlockServer" :: Symbol
type TraceChainSyncHeaderServer = "TraceChainSyncHeaderServer" :: Symbol
type TraceChainSyncProtocol = "TraceChainSyncProtocol" :: Symbol
type TraceConnectionManager = "TraceConnectionManager" :: Symbol
type TraceConnectionManagerCounters = "TraceConnectionManagerCounters" :: Symbol
type TraceConnectionManagerTransitions = "TraceConnectionManagerTransitions" :: Symbol
type DebugPeerSelectionInitiator = "DebugPeerSelectionInitiator" :: Symbol
type DebugPeerSelectionInitiatorResponder = "DebugPeerSelectionInitiatorResponder" :: Symbol
type TraceDiffusionInitialization = "TraceDiffusionInitialization" :: Symbol
type TraceDnsResolver = "TraceDnsResolver" :: Symbol
type TraceDnsSubscription = "TraceDnsSubscription" :: Symbol
type TraceErrorPolicy = "TraceErrorPolicy" :: Symbol
type TraceForge = "TraceForge" :: Symbol
type TraceForgeStateInfo = "TraceForgeStateInfo" :: Symbol
type TraceGDD = "TraceGDD" :: Symbol
type TraceHandshake = "TraceHandshake" :: Symbol
type TraceIpSubscription = "TraceIpSubscription" :: Symbol
type TraceKeepAliveClient = "TraceKeepAliveClient" :: Symbol
type TraceLedgerPeers = "TraceLedgerPeers" :: Symbol
type TraceLocalChainSyncProtocol = "TraceLocalChainSyncProtocol" :: Symbol
type TraceLocalConnectionManager = "TraceLocalConnectionManager" :: Symbol
type TraceLocalErrorPolicy = "TraceLocalErrorPolicy" :: Symbol
type TraceLocalHandshake = "TraceLocalHandshake" :: Symbol
type TraceLocalInboundGovernor = "TraceLocalInboundGovernor" :: Symbol
type TraceLocalRootPeers = "TraceLocalRootPeers" :: Symbol
type TraceLocalServer = "TraceLocalServer" :: Symbol
type TraceLocalStateQueryProtocol = "TraceLocalStateQueryProtocol" :: Symbol
type TraceLocalTxMonitorProtocol = "TraceLocalTxMonitorProtocol" :: Symbol
type TraceLocalTxSubmissionProtocol = "TraceLocalTxSubmissionProtocol" :: Symbol
type TraceLocalTxSubmissionServer = "TraceLocalTxSubmissionServer" :: Symbol
type TraceMempool = "TraceMempool" :: Symbol
type TraceBackingStore = "TraceBackingStore" :: Symbol
type TraceMux = "TraceMux" :: Symbol
type TraceMuxBearer = "TraceMuxBearer" :: Symbol
type TraceMuxChannel = "TraceMuxChannel" :: Symbol
type TraceLocalMux = "TraceLocalMux" :: Symbol
type TraceLocalMuxBearer = "TraceLocalMuxBearer" :: Symbol
type TraceLocalMuxChannel = "TraceLocalMuxChannel" :: Symbol
type TracePeerSelection = "TracePeerSelection" :: Symbol
type TracePeerSelectionCounters = "TracePeerSelectionCounters" :: Symbol
type TracePeerSelectionActions = "TracePeerSelectionActions" :: Symbol
type TracePublicRootPeers = "TracePublicRootPeers" :: Symbol
type TraceSanityCheckIssue = "TraceSanityCheckIssue" :: Symbol
type TraceServer = "TraceServer" :: Symbol
type TraceInboundGovernor = "TraceInboundGovernor" :: Symbol
type TraceInboundGovernorCounters = "TraceInboundGovernorCounters" :: Symbol
type TraceInboundGovernorTransitions = "TraceInboundGovernorTransitions" :: Symbol
type TraceTxInbound = "TraceTxInbound" :: Symbol
type TraceTxOutbound = "TraceTxOutbound" :: Symbol
type TraceTxSubmissionProtocol = "TraceTxSubmissionProtocol" :: Symbol
type TraceTxSubmission2Protocol = "TraceTxSubmission2Protocol" :: Symbol
type TraceKeepAliveProtocol = "TraceKeepAliveProtocol" :: Symbol
type TracePeerSharingProtocol = "TracePeerSharingProtocol" :: Symbol
type TraceGsm = "TraceGsm" :: Symbol
type TraceCsj = "TraceCsj" :: Symbol
type TraceKesAgent = "TraceKesAgent" :: Symbol
type TraceDevotedBlockFetch = "TraceDevotedBlockFetch" :: Symbol
type TraceChurnMode = "TraceChurnMode" :: Symbol
type TraceDNS = "TraceDNS" :: Symbol

newtype OnOff (name :: Symbol) = OnOff {isOn :: Bool} deriving (Eq, Show)

instance KnownSymbol a => FromJSON (OnOff a) where
  parseJSON b = withBool (symbolVal (Proxy @a)) (pure . OnOff) b <|> pure (OnOff False)

proxyName :: KnownSymbol name => Proxy name -> Text
proxyName p = Text.pack (symbolVal p)

data TraceSelection
  = TraceSelection
  { traceVerbosity :: Override TracingVerbosity
  , -- Per-trace toggles, alpha-sorted.
    traceAcceptPolicy :: Override (OnOff TraceAcceptPolicy)
  , traceBackingStore :: Override (OnOff TraceBackingStore)
  , traceBlockFetchClient :: Override (OnOff TraceBlockFetchClient)
  , traceBlockFetchDecisions :: Override (OnOff TraceBlockFetchDecisions)
  , traceBlockFetchProtocol :: Override (OnOff TraceBlockFetchProtocol)
  , traceBlockFetchProtocolSerialised :: Override (OnOff TraceBlockFetchProtocolSerialised)
  , traceBlockFetchServer :: Override (OnOff TraceBlockFetchServer)
  , traceBlockchainTime :: Override (OnOff TraceBlockchainTime)
  , traceChainDB :: Override (OnOff TraceChainDB)
  , traceChainSyncBlockServer :: Override (OnOff TraceChainSyncBlockServer)
  , traceChainSyncClient :: Override (OnOff TraceChainSyncClient)
  , traceChainSyncHeaderServer :: Override (OnOff TraceChainSyncHeaderServer)
  , traceChainSyncProtocol :: Override (OnOff TraceChainSyncProtocol)
  , traceChurnMode :: Override (OnOff TraceChurnMode)
  , traceConnectionManager :: Override (OnOff TraceConnectionManager)
  , traceConnectionManagerCounters :: Override (OnOff TraceConnectionManagerCounters)
  , traceConnectionManagerTransitions :: Override (OnOff TraceConnectionManagerTransitions)
  , traceCsj :: Override (OnOff TraceCsj)
  , traceDNS :: Override (OnOff TraceDNS)
  , traceDebugPeerSelectionInitiatorResponderTracer ::
      Override (OnOff DebugPeerSelectionInitiatorResponder)
  , traceDebugPeerSelectionInitiatorTracer :: Override (OnOff DebugPeerSelectionInitiator)
  , traceDevotedBlockFetch :: Override (OnOff TraceDevotedBlockFetch)
  , traceDiffusionInitialization :: Override (OnOff TraceDiffusionInitialization)
  , traceDnsResolver :: Override (OnOff TraceDnsResolver)
  , traceDnsSubscription :: Override (OnOff TraceDnsSubscription)
  , traceErrorPolicy :: Override (OnOff TraceErrorPolicy)
  , traceForge :: Override (OnOff TraceForge)
  , traceForgeStateInfo :: Override (OnOff TraceForgeStateInfo)
  , traceGDD :: Override (OnOff TraceGDD)
  , traceGsm :: Override (OnOff TraceGsm)
  , traceHandshake :: Override (OnOff TraceHandshake)
  , traceInboundGovernor :: Override (OnOff TraceInboundGovernor)
  , traceInboundGovernorCounters :: Override (OnOff TraceInboundGovernorCounters)
  , traceInboundGovernorTransitions :: Override (OnOff TraceInboundGovernorTransitions)
  , traceIpSubscription :: Override (OnOff TraceIpSubscription)
  , traceKeepAliveClient :: Override (OnOff TraceKeepAliveClient)
  , traceKeepAliveProtocol :: Override (OnOff TraceKeepAliveProtocol)
  , traceKesAgent :: Override (OnOff TraceKesAgent)
  , traceLedgerPeers :: Override (OnOff TraceLedgerPeers)
  , traceLocalChainSyncProtocol :: Override (OnOff TraceLocalChainSyncProtocol)
  , traceLocalConnectionManager :: Override (OnOff TraceLocalConnectionManager)
  , traceLocalErrorPolicy :: Override (OnOff TraceLocalErrorPolicy)
  , traceLocalHandshake :: Override (OnOff TraceLocalHandshake)
  , traceLocalInboundGovernor :: Override (OnOff TraceLocalInboundGovernor)
  , traceLocalMux :: Override (OnOff TraceLocalMux)
  , traceLocalMuxBearer :: Override (OnOff TraceLocalMuxBearer)
  , traceLocalMuxChannel :: Override (OnOff TraceLocalMuxChannel)
  , traceLocalRootPeers :: Override (OnOff TraceLocalRootPeers)
  , traceLocalServer :: Override (OnOff TraceLocalServer)
  , traceLocalStateQueryProtocol :: Override (OnOff TraceLocalStateQueryProtocol)
  , traceLocalTxMonitorProtocol :: Override (OnOff TraceLocalTxMonitorProtocol)
  , traceLocalTxSubmissionProtocol :: Override (OnOff TraceLocalTxSubmissionProtocol)
  , traceLocalTxSubmissionServer :: Override (OnOff TraceLocalTxSubmissionServer)
  , traceMempool :: Override (OnOff TraceMempool)
  , traceMux :: Override (OnOff TraceMux)
  , traceMuxBearer :: Override (OnOff TraceMuxBearer)
  , traceMuxChannel :: Override (OnOff TraceMuxChannel)
  , tracePeerSelection :: Override (OnOff TracePeerSelection)
  , tracePeerSelectionActions :: Override (OnOff TracePeerSelectionActions)
  , tracePeerSelectionCounters :: Override (OnOff TracePeerSelectionCounters)
  , tracePeerSharingProtocol :: Override (OnOff TracePeerSharingProtocol)
  , tracePublicRootPeers :: Override (OnOff TracePublicRootPeers)
  , traceSanityCheckIssue :: Override (OnOff TraceSanityCheckIssue)
  , traceServer :: Override (OnOff TraceServer)
  , traceTxInbound :: Override (OnOff TraceTxInbound)
  , traceTxOutbound :: Override (OnOff TraceTxOutbound)
  , traceTxSubmission2Protocol :: Override (OnOff TraceTxSubmission2Protocol)
  , traceTxSubmissionProtocol :: Override (OnOff TraceTxSubmissionProtocol)
  }
  deriving (Eq, Show)

instance FromJSON TraceSelection where
  parseJSON = withObject "TraceSelection" $ \v -> do
    TraceSelection
      <$> (v .:= "TracingVerbosity")
      <*> parseTracer (Proxy @TraceAcceptPolicy) v
      <*> parseTracer (Proxy @TraceBackingStore) v
      <*> parseTracer (Proxy @TraceBlockFetchClient) v
      <*> parseTracer (Proxy @TraceBlockFetchDecisions) v
      <*> parseTracer (Proxy @TraceBlockFetchProtocol) v
      <*> parseTracer (Proxy @TraceBlockFetchProtocolSerialised) v
      <*> parseTracer (Proxy @TraceBlockFetchServer) v
      <*> parseTracer (Proxy @TraceBlockchainTime) v
      <*> parseTracer (Proxy @TraceChainDB) v
      <*> parseTracer (Proxy @TraceChainSyncBlockServer) v
      <*> parseTracer (Proxy @TraceChainSyncClient) v
      <*> parseTracer (Proxy @TraceChainSyncHeaderServer) v
      <*> parseTracer (Proxy @TraceChainSyncProtocol) v
      <*> parseTracer (Proxy @TraceChurnMode) v
      <*> parseTracer (Proxy @TraceConnectionManager) v
      <*> parseTracer (Proxy @TraceConnectionManagerCounters) v
      <*> parseTracer (Proxy @TraceConnectionManagerTransitions) v
      <*> parseTracer (Proxy @TraceCsj) v
      <*> parseTracer (Proxy @TraceDNS) v
      <*> parseTracer (Proxy @DebugPeerSelectionInitiatorResponder) v
      <*> parseTracer (Proxy @DebugPeerSelectionInitiator) v
      <*> parseTracer (Proxy @TraceDevotedBlockFetch) v
      <*> parseTracer (Proxy @TraceDiffusionInitialization) v
      <*> parseTracer (Proxy @TraceDnsResolver) v
      <*> parseTracer (Proxy @TraceDnsSubscription) v
      <*> parseTracer (Proxy @TraceErrorPolicy) v
      <*> parseTracer (Proxy @TraceForge) v
      <*> parseTracer (Proxy @TraceForgeStateInfo) v
      <*> parseTracer (Proxy @TraceGDD) v
      <*> parseTracer (Proxy @TraceGsm) v
      <*> parseTracer (Proxy @TraceHandshake) v
      <*> parseTracer (Proxy @TraceInboundGovernor) v
      <*> parseTracer (Proxy @TraceInboundGovernorCounters) v
      <*> parseTracer (Proxy @TraceInboundGovernorTransitions) v
      <*> parseTracer (Proxy @TraceIpSubscription) v
      <*> parseTracer (Proxy @TraceKeepAliveClient) v
      <*> parseTracer (Proxy @TraceKeepAliveProtocol) v
      <*> parseTracer (Proxy @TraceKesAgent) v
      <*> parseTracer (Proxy @TraceLedgerPeers) v
      <*> parseTracer (Proxy @TraceLocalChainSyncProtocol) v
      <*> parseTracer (Proxy @TraceLocalConnectionManager) v
      <*> parseTracer (Proxy @TraceLocalErrorPolicy) v
      <*> parseTracer (Proxy @TraceLocalHandshake) v
      <*> parseTracer (Proxy @TraceLocalInboundGovernor) v
      <*> parseTracer (Proxy @TraceLocalMux) v
      <*> parseTracer (Proxy @TraceLocalMuxBearer) v
      <*> parseTracer (Proxy @TraceLocalMuxChannel) v
      <*> parseTracer (Proxy @TraceLocalRootPeers) v
      <*> parseTracer (Proxy @TraceLocalServer) v
      <*> parseTracer (Proxy @TraceLocalStateQueryProtocol) v
      <*> parseTracer (Proxy @TraceLocalTxMonitorProtocol) v
      <*> parseTracer (Proxy @TraceLocalTxSubmissionProtocol) v
      <*> parseTracer (Proxy @TraceLocalTxSubmissionServer) v
      <*> parseTracer (Proxy @TraceMempool) v
      <*> parseTracer (Proxy @TraceMux) v
      <*> parseTracer (Proxy @TraceMuxBearer) v
      <*> parseTracer (Proxy @TraceMuxChannel) v
      <*> parseTracer (Proxy @TracePeerSelection) v
      <*> parseTracer (Proxy @TracePeerSelectionActions) v
      <*> parseTracer (Proxy @TracePeerSelectionCounters) v
      <*> parseTracer (Proxy @TracePeerSharingProtocol) v
      <*> parseTracer (Proxy @TracePublicRootPeers) v
      <*> parseTracer (Proxy @TraceSanityCheckIssue) v
      <*> parseTracer (Proxy @TraceServer) v
      <*> parseTracer (Proxy @TraceTxInbound) v
      <*> parseTracer (Proxy @TraceTxOutbound) v
      <*> parseTracer (Proxy @TraceTxSubmission2Protocol) v
      <*> parseTracer (Proxy @TraceTxSubmissionProtocol) v

parseTracer :: KnownSymbol name => Proxy name -> Object -> Parser (Override (OnOff name))
parseTracer p obj = obj .:= Aeson.fromText (proxyName p)

data TracingSystem
  = LegacyTracing TraceSelection
  | TraceDispatcher TraceSelection
  deriving (Generic, Show)

data TracingConfiguration = TracingConfiguration
  { tracingSystem :: Maybe TracingSystem
  , traceMetrics :: Bool
  }
  deriving (Generic, Show)

instance FromJSON TracingConfiguration where
  parseJSON =
    withObject "TracingConfiguration" $ \v -> do
      logging <- v .:? "TurnOnLogging"
      met <- v .:? "TurnOnLogMetrics" .!= False
      trSystem <-
        case logging of
          Just True -> do
            newSystem <- v .:? "UseTraceDispatcher"
            let tr = case newSystem of
                  Just True -> TraceDispatcher
                  _ -> LegacyTracing
            Just . tr <$> parseJSON (Object v)
          _ ->
            pure Nothing
      pure $ TracingConfiguration trSystem met
