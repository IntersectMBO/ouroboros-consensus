{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | One real node, and a mocked environment around it.
--
-- The node is built by the node's own 'initNodeKernel' over a ChainDB on a
-- mocked filesystem, so the code under test is the node's, not a
-- reimplementation of it. It is given no credentials, which is what keeps it
-- from forging and from voting.
--
-- Everything outside the node is the test's: the environment answers the
-- node's mini-protocol clients itself, so a test says which chains exist, who
-- offers what, and in which order any of it arrives.
module Test.Consensus.Leios.NodeUnderTest
  ( NodeUnderTest (..)
  , NodeUnderTestConfig (..)
  , PeerAddr (..)
  , defaultNodeUnderTestConfig
  , withNodeUnderTest
  ) where

import Cardano.Network.NodeToNode (defaultMiniProtocolParameters)
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Control.Monad.IOSim (IOSim)
import Control.ResourceRegistry (ResourceRegistry, withRegistry)
import Control.Tracer (Tracer, nullTracer)
import Data.Hashable (Hashable)
import qualified Data.IntMap.Strict as IntMap
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..))
import qualified LeiosDemoDb as LeiosDb
import LeiosTxCache (nullLeiosTxCache)
import Ouroboros.Consensus.BlockchainTime
  ( BlockchainTime (..)
  , CurrentSlot (..)
  , SystemStart (..)
  , SystemTime
  )
import Ouroboros.Consensus.BlockchainTime.WallClock.Default (defaultSystemTime)
import Ouroboros.Consensus.Config
  ( DiffusionPipeliningSupport (..)
  , SecurityParam (..)
  , TopLevelConfig
  )
import qualified Ouroboros.Consensus.HardFork.History as HardFork
import Ouroboros.Consensus.Mempool (MempoolCapacityBytesOverride (..))
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.HistoricityCheck as HistoricityCheck
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import qualified Ouroboros.Consensus.Node.GSM as GSM
import Ouroboros.Consensus.Node.Genesis
  ( GenesisConfig (..)
  , GenesisNodeKernelArgs (..)
  , LoEAndGDDConfig (..)
  , enableGenesisConfigDefault
  )
import Ouroboros.Consensus.Node.Tracers (nullTracers)
import Ouroboros.Consensus.NodeKernel
  ( NodeKernel
  , NodeKernelArgs (..)
  , initNodeKernel
  )
import Ouroboros.Consensus.Storage.ChainDB.API (ChainDB)
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDBImpl
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import Ouroboros.Consensus.Storage.ImmutableDB (simpleChunkInfo)
import Ouroboros.Consensus.Util.Args (Complete)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..))
import Ouroboros.Network.PeerSelection.Governor.Types
  ( makePublicPeerSelectionStateVar
  )
import Ouroboros.Network.TxSubmission.Inbound.V1 (TxSubmissionInitDelay (..))
import Ouroboros.Network.TxSubmission.Inbound.V2
  ( TxSubmissionLogicVersion (..)
  )
import System.FS.Sim.MockFS (MockFS)
import System.Random (mkStdGen)
import Test.Util.ChainDB
import Test.Util.LeiosTestBlock
import Test.Util.Orphans.IOLike ()

type Blk = LeiosTestBlock

-- | What a test can vary about the node.
data NodeUnderTestConfig = NodeUnderTestConfig
  { nutcLedgerConfig :: LeiosTestLedgerConfig
  , nutcSecurityParam :: SecurityParam
  }

defaultNodeUnderTestConfig ::
  LeiosTestLedgerConfig -> SecurityParam -> NodeUnderTestConfig
defaultNodeUnderTestConfig = NodeUnderTestConfig

-- | The peer address these tests use: a peer is just a number.
newtype PeerAddr = PeerAddr Int
  deriving stock (Eq, Ord, Show)
  deriving newtype Hashable

-- | A running node, and the handles a test needs on it.
data NodeUnderTest m = NodeUnderTest
  { nutChainDB :: ChainDB m Blk
  , nutTopLevelConfig :: TopLevelConfig Blk
  , nutKernel :: NodeKernel m PeerAddr () Blk
  , nutKernelArgs :: NodeKernelArgs m PeerAddr () Blk
  , nutHandlers :: NTN.Handlers m PeerAddr Blk
  -- ^ What the node's mini-protocol clients and servers are made of; the
  -- environment turns these into running protocols.
  }

-- | Run the node over the given mocked filesystems and LeiosDb, then shut it
-- down. Calling this twice over the same storage is a node restart.
withNodeUnderTest ::
  NodeUnderTestConfig ->
  NodeDBs (StrictTMVar (IOSim s) MockFS) ->
  LeiosDb.LeiosDbHandle (IOSim s) ->
  Tracer (IOSim s) (ChainDBImpl.TraceEvent Blk) ->
  (NodeUnderTest (IOSim s) -> IOSim s a) ->
  IOSim s a
withNodeUnderTest cfg nodeDBs leiosDb chainDBTracer body =
  -- The ChainDB's own resources outlive the node's threads, and the node's
  -- threads outlive whatever the body starts. Getting that order wrong shows
  -- up as a thread reading the ChainDB after it has been closed.
  withRegistry $ \dbRegistry -> do
    chainDbArgs <- mkChainDbArgs cfg dbRegistry nodeDBs leiosDb chainDBTracer
    bracket
      (ChainDBImpl.openDB chainDbArgs)
      ChainDB.closeDB
      $ \chainDB -> withRegistry $ \kernelRegistry -> do
        kernelArgs <- mkNodeKernelArgs cfg kernelRegistry chainDB leiosDb
        kernel <- initNodeKernel kernelArgs
        body
          NodeUnderTest
            { nutChainDB = chainDB
            , nutTopLevelConfig = topLevelConfigOf cfg
            , nutKernel = kernel
            , nutKernelArgs = kernelArgs
            , nutHandlers =
                NTN.mkHandlers kernelArgs kernel TxSubmissionLogicV2
            }

-- | A slot clock that never knows the slot, which nothing the node under test
-- does depends on: it neither forges nor votes, BlockFetch then simply stays in
-- bulk-sync mode, and the LeiosFetch logic defaults to putting /every/ offer in
-- its high-priority tier, ie oldest first.
stubBlockchainTime :: BlockchainTime (IOSim s)
stubBlockchainTime = BlockchainTime{getCurrentSlot = pure CurrentSlotUnknown}

-- | The node, with no credentials: it neither forges nor votes, so a test only
-- has to account for what it does with what the environment sends it.
mkNodeKernelArgs ::
  NodeUnderTestConfig ->
  ResourceRegistry (IOSim s) ->
  ChainDB (IOSim s) Blk ->
  LeiosDb.LeiosDbHandle (IOSim s) ->
  IOSim s (NodeKernelArgs (IOSim s) PeerAddr () Blk)
mkNodeKernelArgs cfg registry chainDB leiosDB = do
  publicPeerSelectionStateVar <- makePublicPeerSelectionStateVar
  pure
    NodeKernelArgs
      { tracers = nullTracers
      , registry
      , cfg = topLevelConfigOf cfg
      , featureFlags = mempty
      , btime = stubBlockchainTime
      , systemTime = nutSystemTime
      , chainDB
      , initChainDB = \_ _ -> pure ()
      , chainSyncFutureCheck =
          InFutureCheck.realHeaderInFutureCheck
            InFutureCheck.defaultClockSkew
            nutSystemTime
      , chainSyncHistoricityCheck = \_getGsmState -> HistoricityCheck.noCheck
      , blockFetchSize = const 1000
      , mempoolCapacityOverride = NoMempoolCapacityBytesOverride
      , mempoolTimeoutConfig = Nothing
      , miniProtocolParameters = defaultMiniProtocolParameters
      , blockFetchConfiguration =
          BlockFetchConfiguration
            { -- The slot clock is stubbed, so the node is always in bulk-sync
              -- mode; one peer at a time would let a peer that never answers
              -- starve every other peer, and indefinitely: the environment
              -- runs the mini-protocols without time limits, so nothing here
              -- ever disconnects a peer that a real node would eventually
              -- give up on.
              bfcMaxConcurrencyBulkSync = 4
            , bfcMaxConcurrencyDeadline = 4
            , bfcMaxRequestsInflight = 10
            , -- A zero interval makes the decision loop spin without the
              -- simulation's clock ever advancing, so give it a real one.
              bfcDecisionLoopIntervalPraos = 0.05
            , bfcDecisionLoopIntervalGenesis = 0.05
            , bfcSalt = 0
            , bfcGenesisBFConfig = gcBlockFetchConfig enableGenesisConfigDefault
            }
      , keepAliveRng = mkStdGen 1
      , gsmArgs =
          GSM.GsmNodeKernelArgs
            { gsmAntiThunderingHerd = mkStdGen 2
            , gsmDurationUntilTooOld = Nothing
            , gsmMarkerFileView =
                GSM.MarkerFileView
                  { touchMarkerFile = pure ()
                  , removeMarkerFile = pure ()
                  , hasMarkerFile = pure False
                  }
            , gsmMinCaughtUpDuration = 0
            }
      , getUseBootstrapPeers = pure DontUseBootstrapPeers
      , peerSharingRng = mkStdGen 3
      , txSubmissionInitDelay = NoTxSubmissionInitDelay
      , publicPeerSelectionStateVar
      , genesisArgs = GenesisNodeKernelArgs{gnkaLoEAndGDDArgs = LoEAndGDDDisabled}
      , getDiffusionPipeliningSupport = DiffusionPipeliningOn
      , leiosDB
      , leiosTxCache = nullLeiosTxCache
      , leiosFetchRng = mkStdGen 4
      }

topLevelConfigOf :: NodeUnderTestConfig -> TopLevelConfig Blk
topLevelConfigOf NodeUnderTestConfig{nutcLedgerConfig, nutcSecurityParam} =
  singleNodeLeiosTestConfig nutcLedgerConfig nutcSecurityParam

mkChainDbArgs ::
  NodeUnderTestConfig ->
  ResourceRegistry (IOSim s) ->
  NodeDBs (StrictTMVar (IOSim s) MockFS) ->
  LeiosDb.LeiosDbHandle (IOSim s) ->
  Tracer (IOSim s) (ChainDBImpl.TraceEvent Blk) ->
  IOSim s (Complete ChainDB.ChainDbArgs (IOSim s) Blk)
mkChainDbArgs cfg registry mcdbNodeDBs mcdbLeiosDb tracer = do
  let mcdbTopLevelConfig = topLevelConfigOf cfg
      mcdbChunkInfo = simpleChunkInfo (HardFork.eraEpochSize eraParams)
      mcdbInitLedger = leiosTestInitExtLedger (nutcLedgerConfig cfg) IntMap.empty
      mcdbRegistry = registry
  pure $
    ChainDB.updateTracer tracer $
      fromMinimalChainDbArgs MinimalChainDbArgs{..}
 where
  eraParams = ltlcHardForkParams (nutcLedgerConfig cfg)

-- | The node's wall clock is the simulation's own, so the system start has to
-- be where io-sim starts its clock. Starting it anywhere else would put every
-- header decades in the future, and the ChainSync client would wait for them.
nutSystemTime :: SystemTime (IOSim s)
nutSystemTime = defaultSystemTime (SystemStart simulationStart) nullTracer

simulationStart :: UTCTime
simulationStart = UTCTime (fromGregorian 1970 1 1) 0
