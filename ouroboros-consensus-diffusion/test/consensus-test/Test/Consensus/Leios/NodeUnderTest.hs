{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- | One real node, the node under test (NUT), and a mocked environment around
-- it. The @nut@ prefix of names here abbreviates it.
--
-- The node is built by the node's own 'initNodeKernel' over a ChainDB on a
-- mocked filesystem, so the code under test is the node's, not a
-- reimplementation of it. Its block is 'LeiosTestBlock'. It does not forge,
-- since the test gives it no block forging.
--
-- The node's node-to-node applications and the versions it offers them at are
-- built by the node's own 'mkNodeToNodeApps' and
-- 'mkNodeToNodeInitiatorResponderVersions', as a running node builds them.
--
-- Everything outside the node is the test's: the environment runs the other
-- end of each of the node's connections itself.
module Test.Consensus.Leios.NodeUnderTest
  ( Blk
  , NodeUnderTest (..)
  , NodeUnderTestConfig (..)
  , NodeVersions
  , PeerAddr (..)
  , Seeds (..)
  , addBlocks
  , addBlocksAfter
  , decodePeerAddr
  , encodePeerAddr
  , nodeVersionData
  , defaultNodeUnderTestConfig
  , maxEbBodySize
  , withNode
  , withNodeForecastingOnly
  , withNodeOver
  , withNodeUnderTest
  ) where

import Cardano.Ledger.BaseTypes (Milliseconds32 (..), knownNonZeroBounded)
import Cardano.Network.Diffusion.Configuration
  ( defaultChainSyncIdleTimeout
  , defaultTxSubmissionLogicVersion
  )
import Cardano.Network.NodeToNode.Version
  ( DiffusionMode (..)
  , NodeToNodeVersion
  , NodeToNodeVersionData
  , getLocalPerasSupport
  )
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Cardano.Network.PeerSelection.PeerTrustable (PeerTrustable)
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (Serialise (..))
import Control.DeepSeq (NFData)
import Control.Monad.IOSim (IOSim)
import Control.Monad.State.Strict (evalState, state)
import Control.ResourceRegistry (ResourceRegistry, withRegistry)
import Control.Tracer (nullTracer)
import Data.ByteString.Lazy (ByteString)
import Data.Hashable (Hashable)
import Data.Maybe.Strict (StrictMaybe (SJust))
import Data.Proxy (Proxy (..))
import qualified Network.Mux as Mux
import Ouroboros.Consensus.Block (SlotNo, WithOrigin (..), blockSlot)
import Ouroboros.Consensus.BlockchainTime
  ( BackoffDelay (..)
  , BlockchainTime
  , HardForkBlockchainTimeArgs (..)
  , SystemTime
  , hardForkBlockchainTime
  , slotLengthFromSec
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Default (defaultSystemTime)
import Ouroboros.Consensus.Config
  ( DiffusionPipeliningSupport (..)
  , SecurityParam (..)
  , TopLevelConfig
  , configLedger
  )
import Ouroboros.Consensus.Config.SupportsNode (getNetworkMagic, getSystemStart)
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Leios.Protocol (LeiosLedgerView (..))
import Ouroboros.Consensus.Leios.Types (BytesSize)
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.HistoricityCheck as HistoricityCheck
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import Ouroboros.Consensus.Node
  ( mkNodeKernelArgs
  , mkNodeToNodeApps
  , mkNodeToNodeInitiatorResponderVersions
  , nodeKernelArgsEnforceInvariants
  , openChainDB
  , stdVersionDataNTN
  )
import Ouroboros.Consensus.Node.ExitPolicy (NodeToNodeInitiatorResult)
import qualified Ouroboros.Consensus.Node.GSM as GSM
import Ouroboros.Consensus.Node.Genesis
  ( disableGenesisConfig
  , mkGenesisNodeKernelArgs
  )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
  ( BlockNodeToNodeVersion
  , supportedNodeToNodeVersions
  )
import Ouroboros.Consensus.Node.Tracers (nullTracers)
import Ouroboros.Consensus.NodeKernel
  ( NodeKernel
  , NodeKernelArgs (..)
  , initNodeKernel
  )
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.ChainDB.API.Types.InvalidBlockPunishment (noPunishment)
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory (mkInMemoryArgs)
import Ouroboros.Consensus.Storage.LeiosDB (LeiosDbHandle, newLeiosDBInMemory)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.ConnectionId (ConnectionId)
import qualified Ouroboros.Network.Diffusion.Policies as Diffusion
import Ouroboros.Network.Hashable (mkUnsafeSalt)
import Ouroboros.Network.Mux (OuroborosBundleWithExpandedCtx)
import Ouroboros.Network.PeerSelection.Governor.Types
  ( makePublicPeerSelectionStateVar
  )
import Ouroboros.Network.PeerSelection.PeerMetric (newPeerMetric)
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.Protocol.Handshake.Version (Versions)
import Ouroboros.Network.TxSubmission.Inbound.V1 (TxSubmissionInitDelay (..))
import System.FS.API (SomeHasFS (..))
import System.FS.API.Types (mkFsPath)
import System.FS.Sim.MockFS (MockFS)
import System.FS.Sim.STM (simHasFS)
import System.Random (StdGen, mkStdGen, random, splitGen)
import Test.Consensus.Leios.Committee (testCommittee, testQuorumStakeThreshold)
import Test.Consensus.Leios.TestBlock
import Test.Util.ChainDB (NodeDBs (..), emptyNodeDBs)
import Test.Util.FS.Subdirectory (subdirectoryHasFS)
import Test.Util.Orphans.IOLike ()

type Blk = LeiosTestBlock

-- | What a test can vary about the node.
data NodeUnderTestConfig = NodeUnderTestConfig
  { nutcSecurityParam :: SecurityParam
  , nutcLedgerConfig :: LeiosTestLedgerConfig
  , nutcRandomness :: StdGen
  -- ^ The root of all the test's randomness; see 'Seeds'.
  }

defaultNodeUnderTestConfig :: SecurityParam -> LeiosTestLedgerConfig -> NodeUnderTestConfig
defaultNodeUnderTestConfig nutcSecurityParam nutcLedgerConfig =
  NodeUnderTestConfig
    { nutcSecurityParam
    , nutcLedgerConfig
    , nutcRandomness = mkStdGen 0
    }

-- | All the randomness of a Leios node-vs-environment test.
data Seeds a = Seeds
  { seedBlockFetchSalt :: a
  , seedEnvPeerSharing :: a
  , seedGsmAntiThunderingHerd :: a
  , seedKeepAliveAndPeerSharing :: a
  , seedNodeToNodeApps :: a
  , seedSnapshotDelay :: a
  -- ^ The ChainDB needs a seed for its snapshot delay whatever its snapshot
  -- policy, so this one is real, like the rest, rather than a dummy that a test
  -- might come to depend on.
  }
  deriving (Functor, Foldable, Traversable)

-- | Every seed is split off the one generator, as a running node splits its own,
-- so no two share a stream.
splitSeeds :: StdGen -> Seeds StdGen
splitSeeds = evalState (mapM (\() -> state splitGen) (Seeds () () () () () ()))

nodeSeeds :: NodeUnderTestConfig -> Seeds StdGen
nodeSeeds = splitSeeds . nutcRandomness

-- | The peer address these tests use: a peer is just a number.
newtype PeerAddr = PeerAddr Int
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, NFData, NoThunks)

-- | A running node, and the handles a test needs on it for its gray-box
-- predicates.
data NodeUnderTest m = NodeUnderTest
  { nutChainDB :: ChainDB m Blk
  , nutTopLevelConfig :: TopLevelConfig Blk
  , nutKernel :: NodeKernel m PeerAddr (ConnectionId ()) Blk
  , nutApps :: BlockNodeToNodeVersion Blk -> NodeApps m
  , nutVersions :: NodeVersions m
  , nutSeeds :: Seeds StdGen
  }

-- | Add a chain to the node's ChainDB, oldest first, starting at genesis
addBlocks :: IOLike m => NodeUnderTest m -> [Blk] -> m ()
addBlocks nut = addBlocksAfter nut Origin

-- | Add a chain to the node's ChainDB, oldest first, given the slot of its
-- first block's predecessor
--
-- A 'LeiosTestBlock''s ledger view is the same at every slot.
addBlocksAfter :: IOLike m => NodeUnderTest m -> WithOrigin SlotNo -> [Blk] -> m ()
addBlocksAfter nut predecessorSlot blks =
  sequence_ $
    zipWith
      ( \predSlot ->
          ChainDB.addBlock_ (nutChainDB nut) noPunishment predSlot leiosView
      )
      (predecessorSlot : map (NotOrigin . blockSlot) blks)
      blks
 where
  leiosView = SJust (ltlcLeios (configLedger (nutTopLevelConfig nut)))

-- | The node's node-to-node applications, at every mini-protocol's bytes.
type NodeApps m =
  NTN.Apps
    m
    PeerAddr
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    ByteString
    NodeToNodeInitiatorResult
    ()

-- | The node's node-to-node applications for each version it supports, as it
-- offers them on a connection where it is both initiator and responder.
type NodeVersions m =
  Versions
    NodeToNodeVersion
    NodeToNodeVersionData
    ( OuroborosBundleWithExpandedCtx
        'Mux.InitiatorResponderMode
        PeerAddr
        PeerTrustable
        ByteString
        m
        NodeToNodeInitiatorResult
        ()
    )

-- | Run the node over the given mocked filesystems, then shut it down.
--
-- As in a node's database directory, the immutable file system holds the
-- ImmutableDB, and the volatile one holds every other database, each in its own
-- subdirectory. The ledger and GSM file systems go unused.
withNodeUnderTest ::
  NodeUnderTestConfig ->
  NodeDBs (StrictTMVar (IOSim s) MockFS) ->
  LeiosDbHandle (IOSim s) ->
  (NodeUnderTest (IOSim s) -> IOSim s a) ->
  IOSim s a
withNodeUnderTest cfg nodeDBs leiosDB body = do
  -- What a node with Genesis disabled, the default, runs with.
  (genesisArgs, updateChainDbArgs) <- mkGenesisNodeKernelArgs disableGenesisConfig
  let seeds = nodeSeeds cfg
  -- The ChainDB and the node each get their own resource registry. The node's
  -- registry is nested inside the ChainDB's, and the body runs inside both. So
  -- on the way out, the body finishes first, then the node's threads stop, and
  -- only then does the ChainDB close.
  withRegistry $ \dbRegistry -> do
    bracket
      ( fst
          <$> openChainDB
            dbRegistry
            (topLevelConfigOf cfg)
            (leiosTestInitExtLedger (nutcLedgerConfig cfg))
            (mountedIn (nodeDBsImm nodeDBs))
            (mountedIn (nodeDBsVol nodeDBs))
            (seedSnapshotDelay seeds)
            (fst (mkInMemoryArgs ()))
            leiosDB
            ChainDB.defaultArgs
            updateChainDbArgs
      )
      ChainDB.closeDB
      $ \chainDB -> withRegistry $ \kernelRegistry -> do
        btime <- mkBlockchainTime cfg kernelRegistry chainDB
        publicPeerSelectionStateVar <- makePublicPeerSelectionStateVar
        kernelArgs <-
          nodeKernelArgsEnforceInvariants
            <$> mkNodeKernelArgs
              kernelRegistry
              (mkUnsafeSalt (fst (random (seedBlockFetchSalt seeds))))
              (seedGsmAntiThunderingHerd seeds)
              (seedKeepAliveAndPeerSharing seeds)
              (topLevelConfigOf cfg)
              mempty
              nullTracers
              btime
              nodeSystemTime
              InFutureCheck.defaultClockSkew
              -- With Genesis disabled, there is no historicity cutoff.
              (\_getGsmState -> HistoricityCheck.noCheck)
              chainDB
              leiosDB
              -- How long the node stays caught up at least, which does not
              -- matter, since it never leaves.
              0
              -- The node starts caught up, as one that was caught up when it
              -- last shut down does, and never judges its selection too old, so
              -- it stays caught up however far the simulated clock runs ahead of
              -- the test's chain.
              Nothing
              GSM.MarkerFileView
                { GSM.touchMarkerFile = pure ()
                , GSM.removeMarkerFile = pure ()
                , GSM.hasMarkerFile = pure True
                }
              (pure DontUseBootstrapPeers)
              publicPeerSelectionStateVar
              genesisArgs
              DiffusionPipeliningOn
              Nothing
              NoTxSubmissionInitDelay
        kernel <- initNodeKernel kernelArgs
        peerMetrics <- newPeerMetric Diffusion.peerMetricsConfiguration
        let apps =
              mkNodeToNodeApps
                (seedNodeToNodeApps seeds)
                NTN.nullTracers
                defaultChainSyncIdleTimeout
                disableGenesisConfig
                defaultTxSubmissionLogicVersion
                kernelArgs
                kernel
                peerMetrics
                (const encodePeerAddr)
                (const decodePeerAddr)
        body
          NodeUnderTest
            { nutChainDB = chainDB
            , nutTopLevelConfig = topLevelConfigOf cfg
            , nutKernel = kernel
            , nutApps = apps
            , nutVersions =
                mkNodeToNodeInitiatorResponderVersions
                  (featureFlags kernelArgs)
                  (miniProtocolParameters kernelArgs)
                  nodeVersionData
                  (supportedNodeToNodeVersions (Proxy @Blk))
                  apps
            , nutSeeds = seeds
            }

-- | A node whose ledger view forecasts further ahead than any test runs.
withNode :: (NodeUnderTest (IOSim s) -> IOSim s a) -> IOSim s a
withNode = withNodeForecastingOnly 100000

-- | A node whose ledger view forecasts this many slots past its immutable tip.
withNodeForecastingOnly :: SlotNo -> (NodeUnderTest (IOSim s) -> IOSim s a) -> IOSim s a
withNodeForecastingOnly range body = do
  nodeDBs <- emptyNodeDBs
  leiosDB <- newLeiosDBInMemory
  withNodeUnderTest (forecastingOnly range) nodeDBs leiosDB body

-- | 'withNode' over the given LeiosDB, which a test can fill before the node
-- starts.
withNodeOver :: LeiosDbHandle (IOSim s) -> (NodeUnderTest (IOSim s) -> IOSim s a) -> IOSim s a
withNodeOver leiosDB body = do
  nodeDBs <- emptyNodeDBs
  withNodeUnderTest (forecastingOnly 100000) nodeDBs leiosDB body

-- | The configuration of a node whose ledger view forecasts this many slots
-- past its immutable tip.
forecastingOnly :: SlotNo -> NodeUnderTestConfig
forecastingOnly range =
  defaultNodeUnderTestConfig
    k
    LeiosTestLedgerConfig
      { ltlcHardForkParams =
          HardFork.defaultEraParams k (slotLengthFromSec 1) HardFork.NoPerasEnabled
      , ltlcForecastRange = range
      , ltlcLeios =
          MkLeiosLedgerView
            { llvCommittee = testCommittee
            , llvQuorumStakeThreshold = testQuorumStakeThreshold
            , llvAnnouncementPeriod = Milliseconds32 1000
            , llvVotePeriod = Milliseconds32 4000
            , llvDiffusionPeriod = Milliseconds32 1000
            , llvMaxEbBodySize = maxEbBodySize
            , llvMaxEbTxsSize = maxEbBodySize
            }
      }
 where
  k = SecurityParam $ knownNonZeroBounded @10

-- | The largest endorser block, and total size of the transactions it
-- references, that the NUT's ledger admits.
maxEbBodySize :: BytesSize
maxEbBodySize = 5000

-- | The version data a relay offers: it accepts inbound connections and shares
-- peers.
nodeVersionData :: NodeToNodeVersion -> NodeToNodeVersionData
nodeVersionData version =
  stdVersionDataNTN
    (getNetworkMagic LeiosTestBlockConfig)
    InitiatorAndResponderDiffusionMode
    PeerSharingEnabled
    (getLocalPerasSupport mempty version)

encodePeerAddr :: PeerAddr -> CBOR.Encoding
encodePeerAddr (PeerAddr n) = encode n

decodePeerAddr :: CBOR.Decoder s PeerAddr
decodePeerAddr = PeerAddr <$> decode

-- | The node's slot clock, derived from the ledger as a real node's is.
mkBlockchainTime ::
  NodeUnderTestConfig ->
  ResourceRegistry (IOSim s) ->
  ChainDB (IOSim s) Blk ->
  IOSim s (BlockchainTime (IOSim s))
mkBlockchainTime cfg registry chainDB =
  hardForkBlockchainTime
    HardForkBlockchainTimeArgs
      { hfbtBackoffDelay = pure (BackoffDelay 1)
      , hfbtGetLedgerState = ledgerState <$> ChainDB.getCurrentLedger chainDB
      , hfbtLedgerConfig = configLedger (topLevelConfigOf cfg)
      , hfbtRegistry = registry
      , hfbtSystemTime = nodeSystemTime
      , hfbtTracer = nullTracer
      , hfbtMaxClockRewind = 0
      }

topLevelConfigOf :: NodeUnderTestConfig -> TopLevelConfig Blk
topLevelConfigOf NodeUnderTestConfig{nutcSecurityParam, nutcLedgerConfig} =
  singleNodeLeiosTestConfig nutcLedgerConfig nutcSecurityParam

-- | A node's database directory, mounting each database in its own
-- subdirectory as 'stdMkChainDbHasFS' does, but on a mock file system.
mountedIn :: StrictTMVar (IOSim s) MockFS -> ChainDB.RelativeMountPoint -> SomeHasFS (IOSim s)
mountedIn mockFS (ChainDB.RelativeMountPoint path) =
  SomeHasFS $ subdirectoryHasFS (mkFsPath [path]) (simHasFS mockFS)

-- | The node's wall clock, from its block's system start, as a running node's.
nodeSystemTime :: SystemTime (IOSim s)
nodeSystemTime = defaultSystemTime (getSystemStart LeiosTestBlockConfig) nullTracer
