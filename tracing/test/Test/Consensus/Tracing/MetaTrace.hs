{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Consistency checks over the 'MetaTrace' instances.
--
-- These need no trace values: everything here is derived from 'allNamespaces'
-- and the namespace-indexed methods. That makes them cheap enough to run over
-- every traced type, and they catch the mistakes that are easy to make when
-- writing an instance by hand -- a namespace left out of 'allNamespaces', a
-- typo that makes a namespace unreachable, a missing severity (which silently
-- makes the message unconfigurable) or missing documentation.
module Test.Consensus.Tracing.MetaTrace (tests) where

import Cardano.Logging
import Cardano.Protocol.Crypto (StandardCrypto)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime)
import Ouroboros.Consensus.Block (Header)
import Ouroboros.Consensus.Block.SupportsSanityCheck (SanityCheckIssue)
import Ouroboros.Consensus.BlockchainTime.WallClock.Util (TraceBlockchainTimeEvent)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock)
import Ouroboros.Consensus.Genesis.Governor (TraceGDDEvent)
import Ouroboros.Consensus.Mempool (TraceEventMempool)
import Ouroboros.Consensus.MiniProtocol.BlockFetch.Server (TraceBlockFetchServerEvent)
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client (TraceChainSyncClientEvent)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.Jumping as Jumping
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server (TraceChainSyncServerEvent)
import Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
  ( TraceLocalTxSubmissionServerEvent
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.PerasCert
  ( TracePerasCertDiffusionInbound
  , TracePerasCertDiffusionOutbound
  )
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.PerasVote
  ( TracePerasVoteDiffusionInbound
  , TracePerasVoteDiffusionOutbound
  )
import Ouroboros.Consensus.Node.GSM (TraceGsmEvent)
import Ouroboros.Consensus.Node.Tracers
  ( TraceForgeEvent
  , TracePerasCertInclusionEvent
  , TracePerasVoteForgingEvent
  )
import qualified Ouroboros.Consensus.Protocol.Ledger.HotKey as HotKey
import Ouroboros.Consensus.Protocol.Praos.AgentClient (KESAgentClientTrace)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import Ouroboros.Consensus.Tracing
  ( ClientMetrics
  , ConsensusStartupException
  , ReplayBlockStats
  )
import Ouroboros.Network.Block (Tip)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import Ouroboros.Network.BlockFetch.Decision.Trace (TraceDecisionEvent)
import Test.Tasty
import Test.Tasty.HUnit

type Blk = CardanoBlock StandardCrypto

-- | Stand-in for the peer type of the per-peer tracers.
--
-- No 'MetaTrace' method looks at it -- namespaces, severities and documentation
-- are the same whatever a peer is -- so this picks the simplest inhabitant
-- rather than dragging in the node's address types.
type Peer = ()

tests :: TestTree
tests =
  testGroup
    "MetaTrace"
    [ testGroup
        "consensus"
        [ metaTrace @(TraceForgeEvent Blk) "TraceForgeEvent"
        , metaTrace @(TraceEventMempool Blk) "TraceEventMempool"
        , metaTrace @(TraceChainSyncClientEvent Blk) "TraceChainSyncClientEvent"
        , metaTrace @(TraceChainSyncServerEvent Blk) "TraceChainSyncServerEvent"
        , metaTrace @(TraceBlockFetchServerEvent Blk) "TraceBlockFetchServerEvent"
        , metaTrace @(TraceLocalTxSubmissionServerEvent Blk)
            "TraceLocalTxSubmissionServerEvent"
        , metaTrace @(TraceGsmEvent (Tip Blk)) "TraceGsmEvent"
        , metaTrace @(TraceBlockchainTimeEvent UTCTime) "TraceBlockchainTimeEvent"
        , metaTrace @SanityCheckIssue "SanityCheckIssue"
        , metaTrace @HotKey.KESInfo "KESInfo"
        , metaTrace @ConsensusStartupException "ConsensusStartupException"
        , metaTrace @ReplayBlockStats "ReplayBlockStats"
        , metaTrace @ClientMetrics "ClientMetrics"
        , metaTrace @(TraceGDDEvent Peer Blk) "TraceGDDEvent"
        , metaTrace @(Jumping.TraceEventCsj Peer Blk) "Jumping.TraceEventCsj"
        , metaTrace @(Jumping.TraceEventDbf Peer) "Jumping.TraceEventDbf"
        , metaTrace @(BlockFetch.TraceFetchClientState (Header Blk))
            "BlockFetch.TraceFetchClientState"
        , metaTrace @(TraceDecisionEvent Peer (Header Blk)) "TraceDecisionEvent"
        , metaTrace @KESAgentClientTrace "KESAgentClientTrace"
        ]
    , -- Only ChainDB. The LedgerDB, ImmutableDB, VolatileDB, PerasCertDB and
      -- PerasVoteDB tracers are not separate: ChainDbArgs derives each of them
      -- from the ChainDB tracer, and ChainDB.TraceEvent's allNamespaces maps all
      -- of their namespaces in under LedgerEvent, ImmDbEvent and so on. Listing
      -- them here as well checked every one of those namespaces twice, under two
      -- different names.
      testGroup
        "storage"
        [ metaTrace @(ChainDB.TraceEvent Blk) "ChainDB.TraceEvent"
        ]
    , testGroup
        "peras"
        [ metaTrace @(TracePerasCertInclusionEvent Blk) "TracePerasCertInclusionEvent"
        , metaTrace @(TracePerasVoteForgingEvent Blk) "TracePerasVoteForgingEvent"
        , metaTrace @(TracePerasCertDiffusionInbound Blk) "TracePerasCertDiffusionInbound"
        , metaTrace @(TracePerasCertDiffusionOutbound Blk) "TracePerasCertDiffusionOutbound"
        , metaTrace @(TracePerasVoteDiffusionInbound Blk) "TracePerasVoteDiffusionInbound"
        , metaTrace @(TracePerasVoteDiffusionOutbound Blk) "TracePerasVoteDiffusionOutbound"
        ]
    ]

-- | The checks that must hold for any 'MetaTrace' instance.
metaTrace :: forall a. MetaTrace a => String -> TestTree
metaTrace name =
  testGroup
    name
    [ testCase "allNamespaces is non-empty" $
        assertBool "no namespaces at all" (not (null nss))
    , testCase "no namespace is empty" $
        assertNoOffenders
          "namespace with no components"
          [ns | ns <- nss, null (nsGetComplete ns)]
    , testCase "namespaces are unique" $
        assertNoOffenders "namespace listed more than once" duplicates
    , testCase "every namespace has a severity" $
        assertNoOffenders "no severityFor" [ns | ns <- nss, Nothing <- [severityFor ns Nothing]]
    , testCase "every namespace has a privacy" $
        assertNoOffenders "no privacyFor" [ns | ns <- nss, Nothing <- [privacyFor ns Nothing]]
    , testCase "every namespace has a detail level" $
        assertNoOffenders "no detailsFor" [ns | ns <- nss, Nothing <- [detailsFor ns Nothing]]
    , -- A ratchet rather than a clean sheet: the namespaces in
      -- 'knownUndocumented' arrived undocumented and are recorded so that new
      -- ones cannot creep in. Write the documentation and delete the entry; the
      -- test below makes sure the list does not go stale.
      testCase "every namespace is documented" $
        assertNoOffenders
          "no documentFor, or blank"
          [ns | ns <- nss, not (documented ns), not (known ns)]
    , testCase "the undocumented-namespace list has no stale entries" $
        assertNoOffenders
          "documented now, so drop it from knownUndocumented"
          [ns | ns <- nss, documented ns, known ns]
    , -- The same check cardano-node runs over the assembled node configuration,
      -- here over one type's namespaces in isolation: it rejects a namespace that
      -- stops in the middle of another, which would make it unaddressable.
      testCase "trace-dispatcher accepts the namespace tree" $
        case checkTraceConfiguration' emptyTraceConfig (map nsGetTuple nss) of
          [] -> pure ()
          warnings -> assertFailure (Text.unpack (Text.intercalate "\n" warnings))
    ]
 where
  nss :: [Namespace a]
  nss = allNamespaces

  documented ns = case documentFor ns of
    Nothing -> False
    Just doc -> not (Text.null (Text.strip doc))

  known ns = (name, nsToText ns) `Set.member` knownUndocumented

  duplicates =
    [ ns
    | ns <- nss
    , let complete = nsGetComplete ns
    , length (filter (== complete) (map nsGetComplete nss)) > 1
    ]

  assertNoOffenders :: String -> [Namespace a] -> Assertion
  assertNoOffenders what offenders =
    assertBool
      (what <> ": " <> show (Set.toList (Set.fromList (map render offenders))))
      (null offenders)

  render :: Namespace a -> String
  render = Text.unpack . nsToText

-- | Namespaces that have no documentation yet, keyed by the traced type.
--
-- All of these predate the move of the tracing instances into Consensus, and
-- live in ChainDB, ImmutableDB, LedgerDB and the forge tracer. They are listed
-- so that the check above can still reject a newly added namespace with no
-- documentation. Shrink this list, never grow it.
knownUndocumented :: Set.Set (String, Text.Text)
knownUndocumented =
  Set.fromList
    [ ("BlockFetch.TraceFetchClientState", "CompletedBlockFetch")
    , ("ChainDB.TraceEvent", "AddBlockEvent.AddBlockValidation.UpdateLedgerDb")
    , ("ChainDB.TraceEvent", "AddBlockEvent.AddedReprocessLoEBlocksToQueue")
    , ("ChainDB.TraceEvent", "AddBlockEvent.ChainSelectionLoEDebug")
    , ("ChainDB.TraceEvent", "AddBlockEvent.PoppedBlockFromQueue")
    , ("ChainDB.TraceEvent", "AddBlockEvent.PoppedReprocessLoEBlocksFromQueue")
    , ("ChainDB.TraceEvent", "AddBlockEvent.PoppingFromQueue")
    , ("ChainDB.TraceEvent", "ImmDbEvent.CacheEvent.PastChunkExpired")
    , ("ChainDB.TraceEvent", "ImmDbEvent.ChunkValidation.InvalidChunkFile")
    , ("ChainDB.TraceEvent", "ImmDbEvent.ChunkValidation.InvalidPrimaryIndex")
    , ("ChainDB.TraceEvent", "ImmDbEvent.ChunkValidation.InvalidSecondaryIndex")
    , ("ChainDB.TraceEvent", "ImmDbEvent.ChunkValidation.MissingChunkFile")
    , ("ChainDB.TraceEvent", "ImmDbEvent.ChunkValidation.MissingPrimaryIndex")
    , ("ChainDB.TraceEvent", "ImmDbEvent.ChunkValidation.MissingSecondaryIndex")
    , ("ChainDB.TraceEvent", "ImmDbEvent.ChunkValidation.RewritePrimaryIndex")
    , ("ChainDB.TraceEvent", "ImmDbEvent.ChunkValidation.RewriteSecondaryIndex")
    , ("ChainDB.TraceEvent", "ImmDbEvent.ChunkValidation.StartedValidatingChunk")
    , ("ChainDB.TraceEvent", "ImmDbEvent.ChunkValidation.ValidatedChunk")
    , ("ChainDB.TraceEvent", "ImmDbEvent.DBAlreadyClosed")
    , ("ChainDB.TraceEvent", "InitChainSelEvent.Validation.UpdateLedgerDb")
    , ("ChainDB.TraceEvent", "IteratorEvent.UnknownRangeRequested.ForkTooOld")
    , ("ChainDB.TraceEvent", "IteratorEvent.UnknownRangeRequested.MissingBlock")
    , ("ChainDB.TraceEvent", "LedgerEvent.Flavor.V2.BackendTrace.LSM.LSMLookup")
    , ("ChainDB.TraceEvent", "LedgerEvent.Flavor.V2.BackendTrace.LSM.LSMOpenSession")
    , ("ChainDB.TraceEvent", "LedgerEvent.Flavor.V2.BackendTrace.LSM.LSMSnap")
    , ("ChainDB.TraceEvent", "LedgerEvent.Flavor.V2.BackendTrace.LSM.LSMTrace")
    , ("ChainDB.TraceEvent", "LedgerEvent.Flavor.V2.BackendTrace.LSM.LSMUpdate")
    , ("KESAgentClientTrace", "KESAgentClientException")
    , ("KESAgentClientTrace", "ServiceClientAbnormalTermination")
    , ("KESAgentClientTrace", "ServiceClientAttemptReconnect")
    , ("KESAgentClientTrace", "ServiceClientConnected")
    , ("KESAgentClientTrace", "ServiceClientDeclinedKey")
    , ("KESAgentClientTrace", "ServiceClientDriverTrace")
    , ("KESAgentClientTrace", "ServiceClientDroppedKey")
    , ("KESAgentClientTrace", "ServiceClientOpCertNumberCheck")
    , ("KESAgentClientTrace", "ServiceClientReceivedKey")
    , ("KESAgentClientTrace", "ServiceClientSocketClosed")
    , ("KESAgentClientTrace", "ServiceClientStopped")
    , ("KESAgentClientTrace", "ServiceClientVersionHandshakeFailed")
    , ("KESAgentClientTrace", "ServiceClientVersionHandshakeTrace")
    , ("TraceForgeEvent", "ForgeTickedLedgerState")
    , ("TraceForgeEvent", "ForgingMempoolSnapshot")
    ]
