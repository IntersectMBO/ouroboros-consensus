{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Run the whole Node
--
-- Intended for qualified import.
module Ouroboros.Consensus.Node
  ( run
  , runWith

    -- * Standard arguments
  , StdRunNodeArgs (..)
  , stdBfcSaltIO
  , stdGsmAntiThunderingHerdIO
  , stdKeepAliveRngIO
  , stdLowLevelRunNodeArgsIO
  , stdMkChainDbHasFS
  , stdRunDataDiffusion
  , stdVersionDataNTC
  , stdVersionDataNTN
  , stdWithCheckedDB

    -- ** P2P Switch
  , NetworkP2PMode (..)

    -- * Exposed by 'run' et al
  , ChainDB.RelativeMountPoint (..)
  , ChainDB.TraceEvent (..)
  , ChainDbArgs (..)
  , HardForkBlockchainTimeArgs (..)
  , LastShutDownWasClean (..)
  , LowLevelRunNodeArgs (..)
  , MempoolCapacityBytesOverride (..)
  , NodeDatabasePaths (..)
  , NodeKernel (..)
  , NodeKernelArgs (..)
  , ProtocolInfo (..)
  , RunNode
  , RunNodeArgs (..)
  , SnapshotPolicyArgs (..)
  , Tracers
  , Tracers' (..)
  , pattern DoDiskSnapshotChecksum
  , pattern NoDoDiskSnapshotChecksum

    -- * Internal helpers
  , mkNodeKernelArgs
  , nodeKernelArgsEnforceInvariants
  , openChainDB
  ) where

import Cardano.Network.PeerSelection.Bootstrap
  ( UseBootstrapPeers (..)
  )
import Cardano.Network.Types (LedgerStateJudgement (..))
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (DeserialiseFailure)
import qualified Control.Concurrent.Class.MonadSTM.Strict as StrictSTM
import Control.DeepSeq (NFData)
import Control.Exception (IOException)
import Control.Monad (forM_, when)
import Control.Monad.Class.MonadTime.SI (MonadTime)
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.ResourceRegistry
import Control.Tracer (Tracer, condTracing, contramap, traceWith)
import Data.ByteString.Lazy (ByteString)
import Data.Functor.Contravariant (Predicate (..))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Time (NominalDiffTime)
import Data.Typeable (Typeable)
import qualified LeiosDemoOnlyTestFetch as LF
import LeiosDemoTypes (SomeLeiosDb, leiosMempoolSize)
import Network.DNS.Resolver (Resolver)
import qualified Network.Mux as Mux
import Network.Mux.Types
import qualified Ouroboros.Cardano.Network.ArgumentsExtra as Cardano
import qualified Ouroboros.Cardano.Network.LedgerPeerConsensusInterface as Cardano
import qualified Ouroboros.Cardano.Network.PeerSelection.Governor.PeerSelectionState as Cardano
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime hiding (getSystemStart)
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Config.SupportsNode
import Ouroboros.Consensus.Ledger.Basics (ValuesMK)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.Mempool.Capacity (mkCapacityBytesOverride)
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.HistoricityCheck
  ( HistoricityCheck
  )
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.HistoricityCheck as HistoricityCheck
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import qualified Ouroboros.Consensus.Network.NodeToClient as NTC
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import Ouroboros.Consensus.Node.DbLock
import Ouroboros.Consensus.Node.DbMarker
import Ouroboros.Consensus.Node.ErrorPolicy
import Ouroboros.Consensus.Node.ExitPolicy
import Ouroboros.Consensus.Node.GSM (GsmNodeKernelArgs (..))
import qualified Ouroboros.Consensus.Node.GSM as GSM
import Ouroboros.Consensus.Node.Genesis
  ( GenesisConfig (..)
  , GenesisNodeKernelArgs
  , mkGenesisNodeKernelArgs
  )
import Ouroboros.Consensus.Node.InitStorage
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.ProtocolInfo
import Ouroboros.Consensus.Node.Recovery
import Ouroboros.Consensus.Node.RethrowPolicy
import Ouroboros.Consensus.Node.Run
import Ouroboros.Consensus.Node.Tracers
import Ouroboros.Consensus.NodeKernel
import Ouroboros.Consensus.Storage.ChainDB
  ( ChainDB
  , ChainDbArgs
  , TraceEvent
  )
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import Ouroboros.Consensus.Storage.LedgerDB.Args
import Ouroboros.Consensus.Storage.LedgerDB.Snapshots
import Ouroboros.Consensus.Util.Args
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.Orphans ()
import Ouroboros.Consensus.Util.Time (secondsToNominalDiffTime)
import Ouroboros.Network.BlockFetch
  ( BlockFetchConfiguration (..)
  , FetchMode
  )
import qualified Ouroboros.Network.Diffusion as Diffusion
import qualified Ouroboros.Network.Diffusion.Common as Diffusion
import qualified Ouroboros.Network.Diffusion.Configuration as Diffusion
import qualified Ouroboros.Network.Diffusion.NonP2P as NonP2P
import qualified Ouroboros.Network.Diffusion.P2P as Diffusion.P2P
import Ouroboros.Network.Magic
import Ouroboros.Network.NodeToClient
  ( ConnectionId
  , LocalAddress
  , LocalSocket
  , NodeToClientVersionData (..)
  , combineVersions
  , simpleSingletonVersions
  )
import Ouroboros.Network.NodeToNode
  ( DiffusionMode (..)
  , ExceptionInHandler (..)
  , MiniProtocolParameters
  , NodeToNodeVersionData (..)
  , RemoteAddress
  , Socket
  , blockFetchPipeliningMax
  , defaultMiniProtocolParameters
  )
import qualified Ouroboros.Network.NodeToNode as N2N
import Ouroboros.Network.PeerSelection.Governor.Types
  ( PeerSelectionState
  , PublicPeerSelectionState
  )
import Ouroboros.Network.PeerSelection.LedgerPeers
  ( LedgerPeersConsensusInterface (..)
  , UseLedgerPeers (..)
  )
import Ouroboros.Network.PeerSelection.PeerMetric
  ( PeerMetrics
  , newPeerMetric
  , reportMetric
  )
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
import Ouroboros.Network.PeerSelection.PeerSharing.Codec
  ( decodeRemoteAddress
  , encodeRemoteAddress
  )
import Ouroboros.Network.PeerSelection.RootPeersDNS.PublicRootPeers
  ( TracePublicRootPeers
  )
import Ouroboros.Network.RethrowPolicy
import qualified SafeWildCards
import System.Exit (ExitCode (..))
import System.FS.API (SomeHasFS (..))
import System.FS.API.Types (MountPoint (..))
import System.FS.IO (ioHasFS)
import System.FilePath ((</>))
import System.Random (StdGen, newStdGen, randomIO, split)

{-------------------------------------------------------------------------------
  The arguments to the Consensus Layer node functionality
-------------------------------------------------------------------------------}

-- How to add a new argument
--
-- 1) As a Consensus Layer maintainer, use your judgement to determine whether
-- the new argument belongs in 'RunNodeArgs' or 'LowLevelArgs'. Give it the type
-- that seems most " natural ", future-proof, and useful for the whole range of
-- invocations: our tests, our own benchmarks, deployment on @mainnet@, etc. The
-- major litmus test is: it only belongs in 'RunNodeArgs' if /every/ invocation
-- of our node code must specify it.
--
-- 2) If you add it to 'LowLevelArgs', you'll have type errors in
-- 'stdLowLevelRunNodeArgsIO'. To fix them, you'll need to either hard-code a
-- default value or else extend 'StdRunNodeArgs' with a new sufficient field.
--
-- 3) When extending either 'RunNodeArgs' or 'StdRunNodeArgs', the
-- @cardano-node@ will have to be updated, so consider the Node Team's
-- preferences when choosing the new field's type. As an oversimplification,
-- Consensus /owns/ 'RunNodeArgs' while Node /owns/ 'StdRunNodeArgs', but it's
-- always worth spending some effort to try to find a type that satisfies both
-- teams.

-- | Arguments expected from any invocation of 'runWith', whether by deployed
-- code, tests, etc.
type RunNodeArgs ::
  (Type -> Type) ->
  Type ->
  Type ->
  Type ->
  Diffusion.P2P ->
  Type
data RunNodeArgs m addrNTN addrNTC blk p2p = RunNodeArgs
  { rnTraceConsensus :: Tracers m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
  -- ^ Consensus tracers
  , rnTraceNTN :: NTN.Tracers m addrNTN blk DeserialiseFailure
  -- ^ Protocol tracers for node-to-node communication
  , rnTraceNTC :: NTC.Tracers m (ConnectionId addrNTC) blk DeserialiseFailure
  -- ^ Protocol tracers for node-to-client communication
  , rnProtocolInfo :: ProtocolInfo blk
  -- ^ Protocol info
  , rnNodeKernelHook ::
      ResourceRegistry m ->
      NodeKernel m addrNTN (ConnectionId addrNTC) blk ->
      m ()
  -- ^ Hook called after the initialisation of the 'NodeKernel'
  --
  -- Called on the 'NodeKernel' after creating it, but before the network
  -- layer is initialised.
  , rnEnableP2P :: NetworkP2PMode p2p
  -- ^ Network P2P Mode switch
  , rnPeerSharing :: PeerSharing
  -- ^ Network PeerSharing miniprotocol willingness flag
  , rnGetUseBootstrapPeers :: STM m UseBootstrapPeers
  , rnGenesisConfig :: GenesisConfig
  , rnNewLeiosDbConnection :: m (SomeLeiosDb m)
  }

-- | Arguments that usually only tests /directly/ specify.
--
-- A non-testing invocation probably wouldn't explicitly provide these values to
-- 'runWith'. The @cardano-node@, for example, instead calls the 'run'
-- abbreviation, which uses 'stdLowLevelRunNodeArgsIO' to indirectly specify
-- these low-level values from the higher-level 'StdRunNodeArgs'.
type LowLevelRunNodeArgs ::
  (Type -> Type) ->
  Type ->
  Type ->
  Type ->
  Diffusion.P2P ->
  Type ->
  Type
data LowLevelRunNodeArgs m addrNTN addrNTC blk p2p extraAPI
  = LowLevelRunNodeArgs
  { llrnWithCheckedDB ::
      forall a.
      ( LastShutDownWasClean ->
        (ChainDB m blk -> m a -> m a) ->
        m a
      ) ->
      m a
  -- ^ An action that will receive a marker indicating whether the previous
  -- shutdown was considered clean and a wrapper for installing a handler to
  -- create a clean file on exit if needed. See
  -- 'Ouroboros.Consensus.Node.Recovery.runWithCheckedDB'.
  , llrnChainDbArgsDefaults :: Incomplete ChainDbArgs m blk
  -- ^ The " static " ChainDB arguments
  , llrnMkImmutableHasFS :: ChainDB.RelativeMountPoint -> SomeHasFS m
  -- ^ File-system on which the directory for the ImmutableDB will
  -- be created.
  , llrnMkVolatileHasFS :: ChainDB.RelativeMountPoint -> SomeHasFS m
  -- ^ File-system on which the directories for databases other than the ImmutableDB will
  -- be created.
  , llrnCustomiseChainDbArgs ::
      Complete ChainDbArgs m blk ->
      Complete ChainDbArgs m blk
  -- ^ Customise the 'ChainDbArgs'. 'StdRunNodeArgs' will use this field to
  -- set various options that are exposed in @cardano-node@ configuration
  -- files.
  , llrnCustomiseNodeKernelArgs ::
      NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk ->
      NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk
  -- ^ Customise the 'NodeArgs'
  , llrnBfcSalt :: Int
  -- ^ Ie 'bfcSalt'
  , llrnGsmAntiThunderingHerd :: StdGen
  -- ^ Ie 'gsmAntiThunderingHerd'
  , llrnKeepAliveRng :: StdGen
  -- ^ Ie 'keepAliveRng'
  , llrnCustomiseHardForkBlockchainTimeArgs ::
      HardForkBlockchainTimeArgs m blk ->
      HardForkBlockchainTimeArgs m blk
  -- ^ Customise the 'HardForkBlockchainTimeArgs'
  , llrnChainSyncTimeout :: m NTN.ChainSyncTimeout
  -- ^ See 'NTN.ChainSyncTimeout'
  , llrnGenesisConfig :: GenesisConfig
  , llrnRunDataDiffusion ::
      NodeKernel m addrNTN (ConnectionId addrNTC) blk ->
      Diffusion.Applications
        addrNTN
        NodeToNodeVersion
        NodeToNodeVersionData
        addrNTC
        NodeToClientVersion
        NodeToClientVersionData
        extraAPI
        m
        NodeToNodeInitiatorResult ->
      Diffusion.ApplicationsExtra p2p addrNTN m NodeToNodeInitiatorResult ->
      m ()
  -- ^ How to run the data diffusion applications
  --
  -- 'run' will not return before this does.
  , llrnVersionDataNTC :: NodeToClientVersionData
  , llrnVersionDataNTN :: NodeToNodeVersionData
  , llrnNodeToNodeVersions :: Map NodeToNodeVersion (BlockNodeToNodeVersion blk)
  -- ^ node-to-node protocol versions to run.
  , llrnNodeToClientVersions :: Map NodeToClientVersion (BlockNodeToClientVersion blk)
  -- ^ node-to-client protocol versions to run.
  , llrnMaxCaughtUpAge :: NominalDiffTime
  -- ^ If the volatile tip is older than this, then the node will exit the
  -- @CaughtUp@ state.
  , llrnMaxClockSkew :: InFutureCheck.ClockSkew
  -- ^ Maximum clock skew
  , llrnPublicPeerSelectionStateVar :: StrictSTM.StrictTVar m (PublicPeerSelectionState addrNTN)
  , llrnLdbFlavorArgs :: Complete LedgerDbFlavorArgs m
  -- ^ The flavor arguments
  }

data NodeDatabasePaths
  = -- | Databases will be stored under this path, such that given a
    -- path @/foo@, databases will be in @/foo/{immutable,volatile,...}@.
    OnePathForAllDbs
      FilePath
  | MultipleDbPaths
      -- | Immutable path, usually pointing to a non-necessarily
      -- performant volume. ImmutableDB will be stored under this path,
      -- so given @/foo@, the ImmutableDB will be in @/foo/immutable@.
      FilePath
      -- | Non-immutable (volatile data) path, usually pointing to a
      -- performant volume. Databases other than the ImmutableDB will
      -- be stored under this path, so given @/bar@, it will contain
      -- @/bar/{volatile,ledger,...}@.
      FilePath

immutableDbPath :: NodeDatabasePaths -> FilePath
immutableDbPath (OnePathForAllDbs f) = f
immutableDbPath (MultipleDbPaths imm _) = imm

nonImmutableDbPath :: NodeDatabasePaths -> FilePath
nonImmutableDbPath (OnePathForAllDbs f) = f
nonImmutableDbPath (MultipleDbPaths _ vol) = vol

-- | Higher-level arguments that can determine the 'LowLevelRunNodeArgs' under
-- some usual assumptions for realistic use cases such as in @cardano-node@.
--
-- See 'stdLowLevelRunNodeArgsIO'.
data
  StdRunNodeArgs
    m
    blk
    (p2p :: Diffusion.P2P)
    extraArgs
    extraState
    extraDebugState
    extraActions
    extraAPI
    extraPeers
    extraFlags
    extraChurnArgs
    extraCounters
    exception
  = StdRunNodeArgs
  { srnBfcMaxConcurrencyBulkSync :: Maybe Word
  , srnBfcMaxConcurrencyDeadline :: Maybe Word
  , srnChainDbValidateOverride :: Bool
  -- ^ If @True@, validate the ChainDB on init no matter what
  , srnDatabasePath :: NodeDatabasePaths
  -- ^ Location of the DBs
  , srnDiffusionArguments ::
      Diffusion.Arguments
        IO
        Socket
        RemoteAddress
        LocalSocket
        LocalAddress
  , srnDiffusionArgumentsExtra ::
      Diffusion.P2PDecision p2p (Tracer IO TracePublicRootPeers) () ->
      Diffusion.P2PDecision p2p (STM IO FetchMode) () ->
      Diffusion.P2PDecision p2p extraAPI () ->
      Diffusion.ArgumentsExtra
        p2p
        extraArgs
        extraState
        extraDebugState
        extraFlags
        extraPeers
        extraAPI
        extraChurnArgs
        extraCounters
        exception
        RemoteAddress
        LocalAddress
        Resolver
        IOException
        IO
  , srnDiffusionTracers ::
      Diffusion.Tracers
        RemoteAddress
        NodeToNodeVersion
        LocalAddress
        NodeToClientVersion
        IO
  , srnDiffusionTracersExtra ::
      Diffusion.ExtraTracers p2p extraState extraDebugState extraFlags extraPeers extraCounters IO
  , srnSigUSR1SignalHandler ::
      ( forall (mode :: Mode) x y.
        Diffusion.ExtraTracers
          p2p
          extraState
          Cardano.DebugPeerSelectionState
          extraFlags
          extraPeers
          extraCounters
          IO ->
        STM IO UseLedgerPeers ->
        PeerSharing ->
        STM IO UseBootstrapPeers ->
        STM IO LedgerStateJudgement ->
        Diffusion.P2P.NodeToNodeConnectionManager
          mode
          Socket
          RemoteAddress
          NodeToNodeVersionData
          NodeToNodeVersion
          IO
          x
          y ->
        StrictSTM.StrictTVar
          IO
          ( PeerSelectionState
              extraState
              extraFlags
              extraPeers
              RemoteAddress
              ( Diffusion.P2P.NodeToNodePeerConnectionHandle
                  mode
                  RemoteAddress
                  NodeToNodeVersionData
                  IO
                  x
                  y
              )
          ) ->
        PeerMetrics IO RemoteAddress ->
        IO ()
      )
  , srnEnableInDevelopmentVersions :: Bool
  -- ^ If @False@, then the node will limit the negotiated NTN and NTC
  -- versions to the latest " official " release (as chosen by Network and
  -- Consensus Team, with input from Node Team)
  , srnTraceChainDB :: Tracer m (ChainDB.TraceEvent blk)
  , srnMaybeMempoolCapacityOverride :: Maybe MempoolCapacityBytesOverride
  -- ^ Determine whether to use the system default mempool capacity or explicitly set
  -- capacity of the mempool.
  , srnChainSyncTimeout :: Maybe (m NTN.ChainSyncTimeout)
  -- ^ A custom timeout for ChainSync.
  , -- Ad hoc values to replace default ChainDB configurations
    srnSnapshotPolicyArgs :: SnapshotPolicyArgs
  , srnQueryBatchSize :: QueryBatchSize
  , srnLdbFlavorArgs :: Complete LedgerDbFlavorArgs m
  }

{-------------------------------------------------------------------------------
  Entrypoints to the Consensus Layer node functionality
-------------------------------------------------------------------------------}

-- | P2P Switch
data NetworkP2PMode (p2p :: Diffusion.P2P) where
  EnabledP2PMode :: NetworkP2PMode 'Diffusion.P2P
  DisabledP2PMode :: NetworkP2PMode 'Diffusion.NonP2P

deriving instance Eq (NetworkP2PMode p2p)
deriving instance Show (NetworkP2PMode p2p)

pure []

-- | Combination of 'runWith' and 'stdLowLevelRunArgsIO'
run ::
  forall blk p2p extraState extraActions extraPeers extraFlags extraChurnArgs extraCounters exception.
  ( RunNode blk
  , Monoid extraPeers
  , Eq extraCounters
  , Eq extraFlags
  , Exception exception
  ) =>
  RunNodeArgs IO RemoteAddress LocalAddress blk p2p ->
  StdRunNodeArgs
    IO
    blk
    p2p
    (Cardano.ExtraArguments IO)
    extraState
    Cardano.DebugPeerSelectionState
    extraActions
    (Cardano.LedgerPeersConsensusInterface IO)
    extraPeers
    extraFlags
    extraChurnArgs
    extraCounters
    exception ->
  IO ()
run args stdArgs =
  stdLowLevelRunNodeArgsIO args stdArgs
    >>= runWith args encodeRemoteAddress decodeRemoteAddress

-- | Extra constraints used by `ouroboros-network`.
type NetworkIO m =
  ( MonadTime m
  , MonadTimer m
  , MonadLabelledSTM m
  )

-- | Extra constraints used by `ouroboros-network`.
type NetworkAddr addr =
  ( Ord addr
  , Typeable addr
  , NoThunks addr
  , NFData addr
  )

-- | Start a node.
--
-- This opens the 'ChainDB', sets up the 'NodeKernel' and initialises the
-- network layer.
--
-- This function runs forever unless an exception is thrown.
runWith ::
  forall m addrNTN addrNTC blk p2p.
  ( RunNode blk
  , IOLike m
  , Hashable addrNTN -- the constraint comes from `initNodeKernel`
  , NetworkIO m
  , NetworkAddr addrNTN
  ) =>
  RunNodeArgs m addrNTN addrNTC blk p2p ->
  (NodeToNodeVersion -> addrNTN -> CBOR.Encoding) ->
  (NodeToNodeVersion -> forall s. CBOR.Decoder s addrNTN) ->
  LowLevelRunNodeArgs m addrNTN addrNTC blk p2p (Cardano.LedgerPeersConsensusInterface m) ->
  m ()
runWith RunNodeArgs{..} encAddrNtN decAddrNtN LowLevelRunNodeArgs{..} =
  llrnWithCheckedDB $ \(LastShutDownWasClean lastShutDownWasClean) continueWithCleanChainDB ->
    withRegistry $ \registry ->
      handleJust
        -- Ignore exception thrown in connection handlers and diffusion.
        -- Also ignore 'ExitSuccess'.
        ( runPredicate $
            Predicate
              ( \err ->
                  case fromException @ExceptionInLinkedThread err of
                    Just (ExceptionInLinkedThread _ err') ->
                      (/= Just ExitSuccess) $ fromException err'
                    Nothing -> False
              )
              <> Predicate (isNothing . fromException @ExceptionInHandler)
              <> Predicate (isNothing . fromException @Diffusion.Failure)
        )
        ( \err ->
            traceWith (consensusErrorTracer rnTraceConsensus) err
              >> throwIO err
        )
        $ do
          let systemStart :: SystemStart
              systemStart = getSystemStart (configBlock cfg)

              systemTime :: SystemTime m
              systemTime =
                defaultSystemTime
                  systemStart
                  (blockchainTimeTracer rnTraceConsensus)

          (genesisArgs, setLoEinChainDbArgs) <-
            mkGenesisNodeKernelArgs llrnGenesisConfig

          let maybeValidateAll
                | lastShutDownWasClean =
                    id
                | otherwise =
                    -- When the last shutdown was not clean, validate the complete
                    -- ChainDB to detect and recover from any disk corruption.
                    ChainDB.ensureValidateAll

          forM_ (sanityCheckConfig cfg) $ \issue ->
            traceWith (consensusSanityCheckTracer rnTraceConsensus) issue

          (chainDB, finalArgs) <-
            openChainDB
              registry
              cfg
              initLedger
              llrnMkImmutableHasFS
              llrnMkVolatileHasFS
              llrnLdbFlavorArgs
              llrnChainDbArgsDefaults
              ( setLoEinChainDbArgs
                  . maybeValidateAll
                  . llrnCustomiseChainDbArgs
              )

          continueWithCleanChainDB chainDB $ do
            btime <-
              hardForkBlockchainTime $
                llrnCustomiseHardForkBlockchainTimeArgs $
                  HardForkBlockchainTimeArgs
                    { hfbtBackoffDelay = pure $ BackoffDelay 60
                    , hfbtGetLedgerState =
                        ledgerState <$> ChainDB.getCurrentLedger chainDB
                    , hfbtLedgerConfig = configLedger cfg
                    , hfbtRegistry = registry
                    , hfbtSystemTime = systemTime
                    , hfbtTracer =
                        contramap
                          (fmap (fromRelativeTime systemStart))
                          (blockchainTimeTracer rnTraceConsensus)
                    , hfbtMaxClockRewind = secondsToNominalDiffTime 20
                    }

            nodeKernelArgs <- do
              durationUntilTooOld <-
                GSM.realDurationUntilTooOld
                  (configLedger cfg)
                  (ledgerState <$> ChainDB.getCurrentLedger chainDB)
                  llrnMaxCaughtUpAge
                  systemTime
              let gsmMarkerFileView =
                    case ChainDB.cdbsHasFSGsmDB $ ChainDB.cdbsArgs finalArgs of
                      SomeHasFS x -> GSM.realMarkerFileView chainDB x
                  historicityCheck getGsmState =
                    case gcHistoricityCutoff llrnGenesisConfig of
                      Nothing -> HistoricityCheck.noCheck
                      Just historicityCutoff ->
                        HistoricityCheck.mkCheck systemTime getGsmState historicityCutoff
              fmap (nodeKernelArgsEnforceInvariants . llrnCustomiseNodeKernelArgs) $
                mkNodeKernelArgs
                  registry
                  llrnBfcSalt
                  llrnGsmAntiThunderingHerd
                  llrnKeepAliveRng
                  cfg
                  rnTraceConsensus
                  btime
                  (InFutureCheck.realHeaderInFutureCheck llrnMaxClockSkew systemTime)
                  historicityCheck
                  chainDB
                  llrnMaxCaughtUpAge
                  (Just durationUntilTooOld)
                  gsmMarkerFileView
                  rnGetUseBootstrapPeers
                  llrnPublicPeerSelectionStateVar
                  genesisArgs
                  DiffusionPipeliningOn
                  rnNewLeiosDbConnection
            nodeKernel <- initNodeKernel nodeKernelArgs
            rnNodeKernelHook registry nodeKernel

            peerMetrics <- newPeerMetric Diffusion.peerMetricsConfiguration
            let ntnApps = mkNodeToNodeApps nodeKernelArgs nodeKernel peerMetrics encAddrNtN decAddrNtN
                ntcApps = mkNodeToClientApps nodeKernelArgs nodeKernel
                (apps, appsExtra) =
                  mkDiffusionApplications
                    rnEnableP2P
                    (miniProtocolParameters nodeKernelArgs)
                    ntnApps
                    ntcApps
                    nodeKernel
                    peerMetrics

            llrnRunDataDiffusion nodeKernel apps appsExtra
 where
  ProtocolInfo
    { pInfoConfig = cfg
    , pInfoInitLedger = initLedger
    } = rnProtocolInfo

  codecConfig :: CodecConfig blk
  codecConfig = configCodec cfg

  mkNodeToNodeApps ::
    NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk ->
    NodeKernel m addrNTN (ConnectionId addrNTC) blk ->
    PeerMetrics m addrNTN ->
    (NodeToNodeVersion -> addrNTN -> CBOR.Encoding) ->
    (NodeToNodeVersion -> forall s. CBOR.Decoder s addrNTN) ->
    BlockNodeToNodeVersion blk ->
    NTN.Apps
      m
      addrNTN
      ByteString
      ByteString
      ByteString
      ByteString
      ByteString
      ByteString
      ByteString
      NodeToNodeInitiatorResult
      ()
  mkNodeToNodeApps nodeKernelArgs nodeKernel peerMetrics encAddrNTN decAddrNTN version =
    NTN.mkApps
      nodeKernel
      rnTraceNTN
      (NTN.defaultCodecs codecConfig version encAddrNTN decAddrNTN)
      NTN.byteLimits
      llrnChainSyncTimeout
      (gcChainSyncLoPBucketConfig llrnGenesisConfig)
      (gcCSJConfig llrnGenesisConfig)
      (reportMetric Diffusion.peerMetricsConfiguration peerMetrics)
      (NTN.mkHandlers nodeKernelArgs nodeKernel)

  mkNodeToClientApps ::
    NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk ->
    NodeKernel m addrNTN (ConnectionId addrNTC) blk ->
    BlockNodeToClientVersion blk ->
    NodeToClientVersion ->
    NTC.Apps m (ConnectionId addrNTC) ByteString ByteString ByteString ByteString ()
  mkNodeToClientApps nodeKernelArgs nodeKernel blockVersion networkVersion =
    NTC.mkApps
      nodeKernel
      rnTraceNTC
      (NTC.defaultCodecs codecConfig blockVersion networkVersion)
      (NTC.mkHandlers nodeKernelArgs nodeKernel)

  mkDiffusionApplications ::
    NetworkP2PMode p2p ->
    MiniProtocolParameters ->
    ( BlockNodeToNodeVersion blk ->
      NTN.Apps
        m
        addrNTN
        ByteString
        ByteString
        ByteString
        ByteString
        ByteString
        ByteString
        ByteString
        NodeToNodeInitiatorResult
        ()
    ) ->
    ( BlockNodeToClientVersion blk ->
      NodeToClientVersion ->
      NTC.Apps
        m
        (ConnectionId addrNTC)
        ByteString
        ByteString
        ByteString
        ByteString
        ()
    ) ->
    NodeKernel m addrNTN (ConnectionId addrNTC) blk ->
    PeerMetrics m addrNTN ->
    ( Diffusion.Applications
        addrNTN
        NodeToNodeVersion
        NodeToNodeVersionData
        addrNTC
        NodeToClientVersion
        NodeToClientVersionData
        (Cardano.LedgerPeersConsensusInterface m)
        m
        NodeToNodeInitiatorResult
    , Diffusion.ApplicationsExtra p2p addrNTN m NodeToNodeInitiatorResult
    )
  mkDiffusionApplications
    enP2P
    miniProtocolParams
    ntnApps
    ntcApps
    kernel
    peerMetrics =
      case enP2P of
        EnabledP2PMode ->
          ( apps
          , Diffusion.P2PApplicationsExtra
              Diffusion.P2P.ApplicationsExtra
                { Diffusion.P2P.daRethrowPolicy = consensusRethrowPolicy (Proxy @blk)
                , Diffusion.P2P.daReturnPolicy = returnPolicy
                , Diffusion.P2P.daLocalRethrowPolicy = localRethrowPolicy
                , Diffusion.P2P.daPeerMetrics = peerMetrics
                , Diffusion.P2P.daPeerSharingRegistry = getPeerSharingRegistry kernel
                }
          )
        DisabledP2PMode ->
          ( apps
          , Diffusion.NonP2PApplicationsExtra
              NonP2P.ApplicationsExtra
                { NonP2P.daErrorPolicies = consensusErrorPolicy (Proxy @blk)
                }
          )
     where
      apps =
        Diffusion.Applications
          { Diffusion.daApplicationInitiatorMode =
              combineVersions
                [ simpleSingletonVersions
                    version
                    llrnVersionDataNTN
                    ( \versionData ->
                        NTN.initiator miniProtocolParams version versionData
                        -- Initiator side won't start responder side of Peer
                        -- Sharing protocol so we give a dummy implementation
                        -- here.
                        $
                          ntnApps blockVersion
                    )
                | (version, blockVersion) <- Map.toList llrnNodeToNodeVersions
                ]
          , Diffusion.daApplicationInitiatorResponderMode =
              combineVersions
                [ simpleSingletonVersions
                    version
                    llrnVersionDataNTN
                    ( \versionData ->
                        NTN.initiatorAndResponder miniProtocolParams version versionData $
                          ntnApps blockVersion
                    )
                | (version, blockVersion) <- Map.toList llrnNodeToNodeVersions
                ]
          , Diffusion.daLocalResponderApplication =
              combineVersions
                [ simpleSingletonVersions
                    version
                    llrnVersionDataNTC
                    (\versionData -> NTC.responder version versionData $ ntcApps blockVersion version)
                | (version, blockVersion) <- Map.toList llrnNodeToClientVersions
                ]
          , Diffusion.daLedgerPeersCtx =
              LedgerPeersConsensusInterface
                { lpGetLatestSlot = getImmTipSlot kernel
                , lpGetLedgerPeers = fromMaybe [] <$> getPeersFromCurrentLedger kernel (const True)
                , lpExtraAPI =
                    Cardano.LedgerPeersConsensusInterface
                      { Cardano.getLedgerStateJudgement = GSM.gsmStateToLedgerJudgement <$> getGsmState kernel
                      , Cardano.updateOutboundConnectionsState =
                          let varOcs = getOutboundConnectionsState kernel
                           in \newOcs -> do
                                oldOcs <- readTVar varOcs
                                when (newOcs /= oldOcs) $ writeTVar varOcs newOcs
                      }
                }
          }

      localRethrowPolicy :: RethrowPolicy
      localRethrowPolicy = mempty

  runPredicate :: Predicate a -> a -> Maybe a
  runPredicate (Predicate p) err = if p err then Just err else Nothing

-- | Check the DB marker, lock the DB and look for the clean shutdown marker.
--
-- Run the body action with the DB locked.
stdWithCheckedDB ::
  forall blk a.
  (StandardHash blk, Typeable blk) =>
  Proxy blk ->
  Tracer IO (TraceEvent blk) ->
  FilePath ->
  NetworkMagic ->
  -- | Body action with last shutdown was clean.
  (LastShutDownWasClean -> (ChainDB IO blk -> IO a -> IO a) -> IO a) ->
  IO a
stdWithCheckedDB pb tracer databasePath networkMagic body = do
  -- Check the DB marker first, before doing the lock file, since if the
  -- marker is not present, it expects an empty DB dir.
  either throwIO return
    =<< checkDbMarker
      hasFS
      mountPoint
      networkMagic

  -- Then create the lock file.
  withLockDB mountPoint $ runWithCheckedDB pb tracer hasFS body
 where
  mountPoint = MountPoint databasePath
  hasFS = ioHasFS mountPoint

openChainDB ::
  forall m blk.
  (RunNode blk, IOLike m) =>
  ResourceRegistry m ->
  TopLevelConfig blk ->
  -- | Initial ledger
  ExtLedgerState blk ValuesMK ->
  -- | Immutable FS, see 'NodeDatabasePaths'
  (ChainDB.RelativeMountPoint -> SomeHasFS m) ->
  -- | Volatile FS, see 'NodeDatabasePaths'
  (ChainDB.RelativeMountPoint -> SomeHasFS m) ->
  Complete LedgerDbFlavorArgs m ->
  -- | A set of default arguments (possibly modified from 'defaultArgs')
  Incomplete ChainDbArgs m blk ->
  -- | Customise the 'ChainDbArgs'
  (Complete ChainDbArgs m blk -> Complete ChainDbArgs m blk) ->
  m (ChainDB m blk, Complete ChainDbArgs m blk)
openChainDB registry cfg initLedger fsImm fsVol flavorArgs defArgs customiseArgs =
  let args =
        customiseArgs $
          ChainDB.completeChainDbArgs
            registry
            cfg
            initLedger
            (nodeImmutableDbChunkInfo (configStorage cfg))
            (nodeCheckIntegrity (configStorage cfg))
            fsImm
            fsVol
            flavorArgs
            defArgs
   in (,args) <$> ChainDB.openDB args

mkNodeKernelArgs ::
  forall m addrNTN addrNTC blk.
  (RunNode blk, IOLike m) =>
  ResourceRegistry m ->
  Int ->
  StdGen ->
  StdGen ->
  TopLevelConfig blk ->
  Tracers m (ConnectionId addrNTN) (ConnectionId addrNTC) blk ->
  BlockchainTime m ->
  InFutureCheck.SomeHeaderInFutureCheck m blk ->
  (m GSM.GsmState -> HistoricityCheck m blk) ->
  ChainDB m blk ->
  NominalDiffTime ->
  Maybe (GSM.WrapDurationUntilTooOld m blk) ->
  GSM.MarkerFileView m ->
  STM m UseBootstrapPeers ->
  StrictSTM.StrictTVar m (PublicPeerSelectionState addrNTN) ->
  GenesisNodeKernelArgs m blk ->
  DiffusionPipeliningSupport ->
  m (SomeLeiosDb m) ->
  m (NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk)
mkNodeKernelArgs
  registry
  bfcSalt
  gsmAntiThunderingHerd
  rng
  cfg
  tracers
  btime
  chainSyncFutureCheck
  chainSyncHistoricityCheck
  chainDB
  maxCaughtUpAge
  gsmDurationUntilTooOld
  gsmMarkerFileView
  getUseBootstrapPeers
  publicPeerSelectionStateVar
  genesisArgs
  getDiffusionPipeliningSupport
  nkaGetLeiosNewDbConnection = do
    let (kaRng, psRng) = split rng
    return
      NodeKernelArgs
        { tracers
        , registry
        , cfg
        , btime
        , chainDB
        , initChainDB = nodeInitChainDB
        , chainSyncFutureCheck
        , chainSyncHistoricityCheck
        , blockFetchSize = estimateBlockSize
        , -- XXX: This gets overridden by the node-config based override
          mempoolCapacityOverride = mkCapacityBytesOverride leiosMempoolSize
        , miniProtocolParameters = defaultMiniProtocolParameters
        , blockFetchConfiguration = Diffusion.defaultBlockFetchConfiguration bfcSalt
        , gsmArgs =
            GsmNodeKernelArgs
              { gsmAntiThunderingHerd
              , gsmDurationUntilTooOld
              , gsmMarkerFileView
              , gsmMinCaughtUpDuration = maxCaughtUpAge
              }
        , getUseBootstrapPeers
        , keepAliveRng = kaRng
        , peerSharingRng = psRng
        , publicPeerSelectionStateVar
        , genesisArgs
        , getDiffusionPipeliningSupport
        , nkaGetLeiosNewDbConnection
        }

-- | We allow the user running the node to customise the 'NodeKernelArgs'
-- through 'llrnCustomiseNodeKernelArgs', but there are some limits to some
-- values. This function makes sure we don't exceed those limits and that the
-- values are consistent.
nodeKernelArgsEnforceInvariants ::
  NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk ->
  NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk
nodeKernelArgsEnforceInvariants nodeKernelArgs =
  nodeKernelArgs
    { miniProtocolParameters =
        miniProtocolParameters
          { -- If 'blockFetchPipeliningMax' exceeds the configured default, it
            -- would be a protocol violation.
            blockFetchPipeliningMax =
              min
                (blockFetchPipeliningMax miniProtocolParameters)
                (blockFetchPipeliningMax defaultMiniProtocolParameters)
          }
    , blockFetchConfiguration =
        blockFetchConfiguration
          { -- 'bfcMaxRequestsInflight' must be <= 'blockFetchPipeliningMax'
            bfcMaxRequestsInflight =
              min
                (bfcMaxRequestsInflight blockFetchConfiguration)
                (fromIntegral $ blockFetchPipeliningMax miniProtocolParameters)
          }
    }
 where
  NodeKernelArgs{..} = nodeKernelArgs

{-------------------------------------------------------------------------------
  Arguments for use in the real node
-------------------------------------------------------------------------------}

-- | How to locate the ChainDB on disk
stdMkChainDbHasFS ::
  FilePath ->
  ChainDB.RelativeMountPoint ->
  SomeHasFS IO
stdMkChainDbHasFS rootPath (ChainDB.RelativeMountPoint relPath) =
  SomeHasFS $ ioHasFS $ MountPoint $ rootPath </> relPath

stdBfcSaltIO :: IO Int
stdBfcSaltIO = randomIO

stdGsmAntiThunderingHerdIO :: IO StdGen
stdGsmAntiThunderingHerdIO = newStdGen

stdKeepAliveRngIO :: IO StdGen
stdKeepAliveRngIO = newStdGen

stdVersionDataNTN ::
  NetworkMagic ->
  DiffusionMode ->
  PeerSharing ->
  NodeToNodeVersionData
stdVersionDataNTN networkMagic diffusionMode peerSharing =
  NodeToNodeVersionData
    { networkMagic
    , diffusionMode
    , peerSharing
    , query = False
    }

stdVersionDataNTC :: NetworkMagic -> NodeToClientVersionData
stdVersionDataNTC networkMagic =
  NodeToClientVersionData
    { networkMagic
    , query = False
    }

stdRunDataDiffusion ::
  ( Monoid extraPeers
  , Eq extraCounters
  , Eq extraFlags
  , Exception exception
  ) =>
  ( forall (mode :: Mode) x y.
    Diffusion.P2P.NodeToNodeConnectionManager
      mode
      Socket
      RemoteAddress
      NodeToNodeVersionData
      NodeToNodeVersion
      IO
      x
      y ->
    StrictSTM.StrictTVar
      IO
      ( PeerSelectionState
          extraState
          extraFlags
          extraPeers
          RemoteAddress
          ( Diffusion.P2P.NodeToNodePeerConnectionHandle
              mode
              RemoteAddress
              NodeToNodeVersionData
              IO
              x
              y
          )
      ) ->
    PeerMetrics IO RemoteAddress ->
    IO ()
  ) ->
  Diffusion.Tracers
    RemoteAddress
    NodeToNodeVersion
    LocalAddress
    NodeToClientVersion
    IO ->
  Diffusion.ExtraTracers
    p2p
    extraState
    extraDebugState
    extraFlags
    extraPeers
    extraCounters
    IO ->
  Diffusion.Arguments
    IO
    Socket
    RemoteAddress
    LocalSocket
    LocalAddress ->
  Diffusion.ArgumentsExtra
    p2p
    extraArgs
    extraState
    extraDebugState
    extraFlags
    extraPeers
    extraAPI
    extraChurnArgs
    extraCounters
    exception
    RemoteAddress
    LocalAddress
    Resolver
    IOException
    IO ->
  Diffusion.Applications
    RemoteAddress
    NodeToNodeVersion
    NodeToNodeVersionData
    LocalAddress
    NodeToClientVersion
    NodeToClientVersionData
    extraAPI
    IO
    a ->
  Diffusion.ApplicationsExtra p2p RemoteAddress IO a ->
  IO ()
stdRunDataDiffusion = Diffusion.run

-- | Conveniently packaged 'LowLevelRunNodeArgs' arguments from a standard
-- non-testing invocation.
stdLowLevelRunNodeArgsIO ::
  forall blk p2p extraState extraActions extraPeers extraFlags extraChurnArgs extraCounters exception.
  ( RunNode blk
  , Monoid extraPeers
  , Eq extraCounters
  , Eq extraFlags
  , Exception exception
  ) =>
  RunNodeArgs IO RemoteAddress LocalAddress blk p2p ->
  StdRunNodeArgs
    IO
    blk
    p2p
    (Cardano.ExtraArguments IO)
    extraState
    Cardano.DebugPeerSelectionState
    extraActions
    (Cardano.LedgerPeersConsensusInterface IO)
    extraPeers
    extraFlags
    extraChurnArgs
    extraCounters
    exception ->
  IO
    ( LowLevelRunNodeArgs
        IO
        RemoteAddress
        LocalAddress
        blk
        p2p
        (Cardano.LedgerPeersConsensusInterface IO)
    )
stdLowLevelRunNodeArgsIO
  RunNodeArgs
    { rnProtocolInfo
    , rnEnableP2P
    , rnPeerSharing
    , rnGenesisConfig
    , rnGetUseBootstrapPeers
    }
  $(SafeWildCards.fields 'StdRunNodeArgs) = do
    llrnBfcSalt <- stdBfcSaltIO
    llrnGsmAntiThunderingHerd <- stdGsmAntiThunderingHerdIO
    llrnKeepAliveRng <- stdKeepAliveRngIO
    pure
      LowLevelRunNodeArgs
        { llrnBfcSalt
        , llrnChainSyncTimeout = fromMaybe Diffusion.defaultChainSyncTimeout srnChainSyncTimeout
        , llrnGenesisConfig = rnGenesisConfig
        , llrnCustomiseHardForkBlockchainTimeArgs = id
        , llrnGsmAntiThunderingHerd
        , llrnKeepAliveRng
        , llrnMkImmutableHasFS = stdMkChainDbHasFS $ immutableDbPath srnDatabasePath
        , llrnMkVolatileHasFS = stdMkChainDbHasFS $ nonImmutableDbPath srnDatabasePath
        , llrnChainDbArgsDefaults = updateChainDbDefaults ChainDB.defaultArgs
        , llrnCustomiseChainDbArgs = id
        , llrnCustomiseNodeKernelArgs
        , llrnRunDataDiffusion =
            \kernel apps extraApps -> do
              case rnEnableP2P of
                EnabledP2PMode ->
                  case srnDiffusionTracersExtra of
                    Diffusion.P2PTracers extraTracers -> do
                      let srnDiffusionArgumentsExtra' =
                            srnDiffusionArgumentsExtra
                              (Diffusion.P2PDecision (Diffusion.P2P.dtTracePublicRootPeersTracer extraTracers))
                              (Diffusion.P2PDecision (getFetchMode kernel))
                              (Diffusion.P2PDecision (lpExtraAPI (Diffusion.daLedgerPeersCtx apps)))
                      case srnDiffusionArgumentsExtra' of
                        Diffusion.P2PArguments extraArgs ->
                          stdRunDataDiffusion
                            ( srnSigUSR1SignalHandler
                                srnDiffusionTracersExtra
                                (Diffusion.P2P.daReadUseLedgerPeers extraArgs)
                                rnPeerSharing
                                rnGetUseBootstrapPeers
                                (GSM.gsmStateToLedgerJudgement <$> getGsmState kernel)
                            )
                            (myf srnDiffusionTracers)
                            srnDiffusionTracersExtra
                            srnDiffusionArguments
                            srnDiffusionArgumentsExtra'
                            apps
                            extraApps
                DisabledP2PMode ->
                  stdRunDataDiffusion
                    ( srnSigUSR1SignalHandler
                        (Diffusion.NonP2PTracers NonP2P.nullTracers)
                        (pure DontUseLedgerPeers)
                        rnPeerSharing
                        (pure DontUseBootstrapPeers)
                        (pure TooOld)
                    )
                    (myf srnDiffusionTracers)
                    srnDiffusionTracersExtra
                    srnDiffusionArguments
                    ( srnDiffusionArgumentsExtra
                        (Diffusion.NonP2PDecision ())
                        (Diffusion.NonP2PDecision ())
                        (Diffusion.NonP2PDecision ())
                    )
                    apps
                    extraApps
        , llrnVersionDataNTC =
            stdVersionDataNTC networkMagic
        , llrnVersionDataNTN =
            stdVersionDataNTN
              networkMagic
              ( case rnEnableP2P of
                  EnabledP2PMode -> Diffusion.daMode srnDiffusionArguments
                  -- Every connection in non-p2p mode is unidirectional; We connect
                  -- from an ephemeral port.  We still pass `srnDiffusionArguments`
                  -- to the diffusion layer, so the server side will be run also in
                  -- `InitiatorAndResponderDiffusionMode`.
                  DisabledP2PMode -> InitiatorOnlyDiffusionMode
              )
              rnPeerSharing
        , llrnNodeToNodeVersions =
            limitToLatestReleasedVersion
              fst
              (supportedNodeToNodeVersions (Proxy @blk))
        , llrnNodeToClientVersions =
            limitToLatestReleasedVersion
              snd
              (supportedNodeToClientVersions (Proxy @blk))
        , llrnWithCheckedDB =
            -- 'stdWithCheckedDB' uses the FS just to check for the clean file.
            -- We put that one in the immutable path.
            stdWithCheckedDB (Proxy @blk) srnTraceChainDB (immutableDbPath srnDatabasePath) networkMagic
        , llrnMaxCaughtUpAge = secondsToNominalDiffTime $ 20 * 60 -- 20 min
        , llrnMaxClockSkew =
            InFutureCheck.defaultClockSkew
        , llrnPublicPeerSelectionStateVar =
            Diffusion.daPublicPeerSelectionVar srnDiffusionArguments
        , llrnLdbFlavorArgs =
            srnLdbFlavorArgs
        }
   where
    myf x = x{Diffusion.dtMuxTracer = condTracing muxCond $ Diffusion.dtMuxTracer x}

    muxCond = (. Mux.wbEvent) $ \case
      Mux.TraceRecvStart{} -> False
      Mux.TraceRecvEnd{} -> False
      Mux.TraceSendStart{} -> False
      Mux.TraceSendEnd{} -> False
      Mux.TraceChannelRecvStart mid -> midCond mid
      Mux.TraceChannelRecvEnd mid _ -> midCond mid
      Mux.TraceChannelSendStart mid _ -> midCond mid
      Mux.TraceChannelSendEnd mid -> midCond mid
      _ -> False

    midCond mid =
      mid == N2N.chainSyncMiniProtocolNum
        || mid == N2N.blockFetchMiniProtocolNum
        || mid == LF.leiosFetchMiniProtocolNum

    networkMagic :: NetworkMagic
    networkMagic = getNetworkMagic $ configBlock $ pInfoConfig rnProtocolInfo

    updateChainDbDefaults ::
      Incomplete ChainDbArgs IO blk ->
      Incomplete ChainDbArgs IO blk
    updateChainDbDefaults =
      ChainDB.updateSnapshotPolicyArgs srnSnapshotPolicyArgs
        . ChainDB.updateQueryBatchSize srnQueryBatchSize
        . ChainDB.updateTracer srnTraceChainDB
        . ( if not srnChainDbValidateOverride
              then id
              else ChainDB.ensureValidateAll
          )

    llrnCustomiseNodeKernelArgs ::
      NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk ->
      NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk
    llrnCustomiseNodeKernelArgs =
      overBlockFetchConfiguration modifyBlockFetchConfiguration
        . modifyMempoolCapacityOverride
     where
      modifyBlockFetchConfiguration =
        maybe
          id
          (\mc bfc -> bfc{bfcMaxConcurrencyDeadline = mc})
          srnBfcMaxConcurrencyDeadline
          . maybe
            id
            (\mc bfc -> bfc{bfcMaxConcurrencyBulkSync = mc})
            srnBfcMaxConcurrencyBulkSync
      modifyMempoolCapacityOverride =
        maybe
          id
          (\mc nka -> nka{mempoolCapacityOverride = mc})
          srnMaybeMempoolCapacityOverride

    -- Limit the node version unless srnEnableInDevelopmentVersions is set
    limitToLatestReleasedVersion ::
      forall k v.
      Ord k =>
      ((Maybe NodeToNodeVersion, Maybe NodeToClientVersion) -> Maybe k) ->
      Map k v ->
      Map k v
    limitToLatestReleasedVersion prj =
      if srnEnableInDevelopmentVersions
        then id
        else case prj $ latestReleasedNodeVersion (Proxy @blk) of
          Nothing -> id
          Just version -> Map.takeWhileAntitone (<= version)

{-------------------------------------------------------------------------------
  Miscellany
-------------------------------------------------------------------------------}

overBlockFetchConfiguration ::
  (BlockFetchConfiguration -> BlockFetchConfiguration) ->
  NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk ->
  NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk
overBlockFetchConfiguration f args =
  args
    { blockFetchConfiguration = f blockFetchConfiguration
    }
 where
  NodeKernelArgs{blockFetchConfiguration} = args
