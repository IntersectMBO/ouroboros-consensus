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
  , stdLowLevelRunNodeArgsIO
  , stdMkChainDbHasFS
  , stdRunDataDiffusion
  , stdVersionDataNTC
  , stdVersionDataNTN
  , stdWithCheckedDB

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
  , ChainSyncIdleTimeout (..)

    -- * Internal helpers
  , mkNodeKernelArgs
  , nodeKernelArgsEnforceInvariants
  , openChainDB
  ) where

import Cardano.Base.FeatureFlags (CardanoFeatureFlag)
import qualified Cardano.Network.Diffusion as Cardano.Diffusion
import Cardano.Network.Diffusion.Configuration (ChainSyncIdleTimeout (..))
import qualified Cardano.Network.Diffusion.Policies as Cardano.Diffusion
import qualified Cardano.Network.LedgerPeerConsensusInterface as Cardano
import Cardano.Network.PeerSelection.Bootstrap (UseBootstrapPeers (..))
import Cardano.Network.PeerSelection.Churn (ChurnMode (..))
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Codec.Serialise (DeserialiseFailure)
import qualified Control.Concurrent.Class.MonadSTM.Strict as StrictSTM
import Control.DeepSeq (NFData)
import Control.Monad (forM_, when)
import Control.Monad.Class.MonadTime.SI (MonadTime)
import Control.Monad.Class.MonadTimer.SI (MonadTimer)
import Control.ResourceRegistry
import Control.Tracer (Tracer, contramap, traceWith)
import Data.ByteString.Lazy (ByteString)
import Data.Functor (void)
import Data.Functor.Contravariant (Predicate (..))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set)
import Data.Time (NominalDiffTime)
import Data.Typeable (Typeable)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.BlockchainTime hiding (getSystemStart)
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Config.SupportsNode
import Ouroboros.Consensus.Ledger.Basics (ValuesMK)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client.HistoricityCheck
  ( HistoricityCheck
  )
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.HistoricityCheck as HistoricityCheck
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import qualified Ouroboros.Consensus.Network.NodeToClient as NTC
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import Ouroboros.Consensus.Node.DbLock
import Ouroboros.Consensus.Node.DbMarker
import Ouroboros.Consensus.Node.ExitPolicy
import Ouroboros.Consensus.Node.GSM (GsmNodeKernelArgs (..))
import qualified Ouroboros.Consensus.Node.GSM as GSM
import Ouroboros.Consensus.Node.Genesis
  ( GenesisConfig (..)
  , GenesisNodeKernelArgs (..)
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
  )
import qualified Ouroboros.Network.Diffusion as Diffusion
import qualified Ouroboros.Network.Diffusion.Configuration as Diffusion
import qualified Ouroboros.Network.Diffusion.Policies as Diffusion
import Ouroboros.Network.Magic
import Ouroboros.Network.NodeToClient
  ( ConnectionId
  , LocalAddress
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
  , blockFetchPipeliningMax
  , defaultMiniProtocolParameters
  )
import Ouroboros.Network.PeerSelection.Governor.Types
  ( PublicPeerSelectionState
  )
import Ouroboros.Network.PeerSelection.LedgerPeers
  ( LedgerPeersConsensusInterface (..)
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
import Ouroboros.Network.Protocol.ChainSync.Codec (timeLimitsChainSync)
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
  Type
data RunNodeArgs m addrNTN addrNTC blk = RunNodeArgs
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
  , rnPeerSharing :: PeerSharing
  -- ^ Network PeerSharing miniprotocol willingness flag
  , rnGetUseBootstrapPeers :: STM m UseBootstrapPeers
  , rnGenesisConfig :: GenesisConfig
  , rnFeatureFlags :: Set CardanoFeatureFlag
  -- ^ Enabled experimental features
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
  Type
data LowLevelRunNodeArgs m addrNTN addrNTC blk
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
  , llrnRng :: StdGen
  -- ^ StdGen for various applications, e.g. keep-alive, chain-sync, gsm anti
  -- thundering herd
  , llrnCustomiseHardForkBlockchainTimeArgs ::
      HardForkBlockchainTimeArgs m blk ->
      HardForkBlockchainTimeArgs m blk
  -- ^ Customise the 'HardForkBlockchainTimeArgs'
  , llrnChainSyncIdleTimeout :: ChainSyncIdleTimeout
  -- ^ custom Chain-Sync idle timeout
  , llrnGenesisConfig :: GenesisConfig
  , llrnRunDataDiffusion ::
      NodeKernel m addrNTN (ConnectionId addrNTC) blk ->
      Cardano.Diffusion.CardanoConsensusArguments addrNTN m ->
      Cardano.Diffusion.Applications
        addrNTN
        NodeToNodeVersion
        NodeToNodeVersionData
        addrNTC
        NodeToClientVersion
        NodeToClientVersionData
        m
        NodeToNodeInitiatorResult ->
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
  , llrnFeatureFlags :: Set CardanoFeatureFlag
  -- ^ Enabled experimental features
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
  = StdRunNodeArgs
  { srnBfcMaxConcurrencyBulkSync :: Maybe Word
  , srnBfcMaxConcurrencyDeadline :: Maybe Word
  , srnChainDbValidateOverride :: Bool
  -- ^ If @True@, validate the ChainDB on init no matter what
  , srnDatabasePath :: NodeDatabasePaths
  -- ^ Location of the DBs
  , srnDiffusionArguments :: Cardano.Diffusion.CardanoNodeArguments m
  , srnDiffusionConfiguration :: Cardano.Diffusion.CardanoConfiguration m
  , srnDiffusionTracers :: Cardano.Diffusion.CardanoTracers m
  , srnEnableInDevelopmentVersions :: Bool
  -- ^ If @False@, then the node will limit the negotiated NTN and NTC
  -- versions to the latest " official " release (as chosen by Network and
  -- Consensus Team, with input from Node Team)
  , srnTraceChainDB :: Tracer m (ChainDB.TraceEvent blk)
  , srnMaybeMempoolCapacityOverride :: Maybe MempoolCapacityBytesOverride
  -- ^ Determine whether to use the system default mempool capacity or explicitly set
  -- capacity of the mempool.
  , srnChainSyncIdleTimeout :: ChainSyncIdleTimeout
  , -- Ad hoc values to replace default ChainDB configurations
    srnSnapshotPolicyArgs :: SnapshotPolicyArgs
  , srnQueryBatchSize :: QueryBatchSize
  , srnLdbFlavorArgs :: Complete LedgerDbFlavorArgs m
  }

{-------------------------------------------------------------------------------
  Entrypoints to the Consensus Layer node functionality
-------------------------------------------------------------------------------}

pure []

-- | Combination of 'runWith' and 'stdLowLevelRunArgsIO'
run ::
  forall blk.
  RunNode blk =>
  RunNodeArgs IO RemoteAddress LocalAddress blk ->
  StdRunNodeArgs IO blk ->
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
--
-- This function spawns a resource registry (which we will refer to as
-- __the consensus registry__) that will include the ChainDB as one of
-- its resources. When the Consensus layer is shut down, the consensus
-- resource registry will exit the scope of the 'withRegistry'
-- function. This causes all resources allocated in the registry
-- —including the ChainDB— to be closed.
--
-- During it's operation, different consensus threads will create
-- resources associated with the ChainDB, eg Forkers in the LedgerDB,
-- or Followers in the ChainDB. These resources are not created by the
-- database themselves (LedgerDB, VolatileDB, and ImmutableDB). For
-- example, chain selection opens a forker using the LedgerDB.
-- Crucially, this means that clients creating these resources are
-- instantiated after the ChainDB.
--
-- We rely on a specific sequence of events for this design to be correct:
--
-- - The ChainDB is only closed by exiting the scope of the consensus
--   resource registry.
--
-- - If a client creates resources tied to any of the
--   aforementioned databases and is forked into a separate thread,
--   that thread is linked to the consensus registry. Because resources
--   in a registry are deallocated in reverse order of allocation, any
--   resources created by such threads will be deallocated before the
--   ChainDB is closed, ensuring proper cleanup.
--
-- Currently, we have two distinct approaches to resource management
-- and database closure:
--
-- - In the LedgerDB, closing the database does not close any resources
--   created by its clients. We rely on the resource registry to deallocate
--   these resources before the LedgerDB is closed. However, after closing
--   the LedgerDB, the only permitted action on these resources is to free them.
--   See 'ldbForkers'.
--
-- - In the ChainDB, closing the database also closes all followers and
--   iterators.
--
-- TODO: Ideally, the ChainDB and LedgerDB should follow a consistent
-- approach to resource deallocation.
runWith ::
  forall m addrNTN addrNTC blk.
  ( RunNode blk
  , IOLike m
  , Hashable addrNTN -- the constraint comes from `initNodeKernel`
  , NetworkIO m
  , NetworkAddr addrNTN
  ) =>
  RunNodeArgs m addrNTN addrNTC blk ->
  (NodeToNodeVersion -> addrNTN -> CBOR.Encoding) ->
  (NodeToNodeVersion -> forall s. CBOR.Decoder s addrNTN) ->
  LowLevelRunNodeArgs m addrNTN addrNTC blk ->
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
                  gsmAntiThunderingHerd
                  keepAliveRng
                  cfg
                  llrnFeatureFlags
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
            nodeKernel <- initNodeKernel nodeKernelArgs
            rnNodeKernelHook registry nodeKernel
            churnModeVar <- StrictSTM.newTVarIO ChurnModeNormal
            churnMetrics <- newPeerMetric Diffusion.peerMetricsConfiguration
            let consensusDiffusionArgs =
                  Cardano.Diffusion.CardanoConsensusArguments
                    { Cardano.Diffusion.churnModeVar
                    , Cardano.Diffusion.churnMetrics
                    , Cardano.Diffusion.ledgerPeersAPI =
                        LedgerPeersConsensusInterface
                          { lpGetLatestSlot = getImmTipSlot nodeKernel
                          , lpGetLedgerPeers = fromMaybe [] <$> getPeersFromCurrentLedger nodeKernel (const True)
                          , lpExtraAPI =
                              Cardano.LedgerPeersConsensusInterface
                                { Cardano.readFetchMode = getFetchMode nodeKernel
                                , Cardano.getLedgerStateJudgement = GSM.gsmStateToLedgerJudgement <$> getGsmState nodeKernel
                                , Cardano.updateOutboundConnectionsState =
                                    let varOcs = getOutboundConnectionsState nodeKernel
                                     in \newOcs -> do
                                          oldOcs <- readTVar varOcs
                                          when (newOcs /= oldOcs) $ writeTVar varOcs newOcs
                                }
                          }
                    , Cardano.Diffusion.readUseBootstrapPeers = rnGetUseBootstrapPeers
                    }

            stdGen <- StrictSTM.newTVarIO peerSelectionRng
            let ntnApps = mkNodeToNodeApps nodeKernelArgs nodeKernel churnMetrics encAddrNtN decAddrNtN
                ntcApps = mkNodeToClientApps nodeKernelArgs nodeKernel
                apps =
                  mkDiffusionApplications
                    stdGen
                    consensusDiffusionArgs
                    (miniProtocolParameters nodeKernelArgs)
                    ntnApps
                    ntcApps
                    nodeKernel

            llrnRunDataDiffusion nodeKernel consensusDiffusionArgs apps
 where
  (gsmAntiThunderingHerd, rng') = split llrnRng
  (peerSelectionRng, rng'') = split rng'
  (keepAliveRng, ntnAppsRng) = split rng''

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
      NodeToNodeInitiatorResult
      ()
  mkNodeToNodeApps nodeKernelArgs nodeKernel peerMetrics encAddrNTN decAddrNTN version =
    NTN.mkApps
      nodeKernel
      ntnAppsRng
      rnTraceNTN
      (NTN.defaultCodecs codecConfig version encAddrNTN decAddrNTN)
      NTN.byteLimits
      (timeLimitsChainSync llrnChainSyncIdleTimeout)
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
    StrictSTM.StrictTVar m StdGen ->
    Cardano.Diffusion.CardanoConsensusArguments addrNTN m ->
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
    Cardano.Diffusion.Applications
      addrNTN
      NodeToNodeVersion
      NodeToNodeVersionData
      addrNTC
      NodeToClientVersion
      NodeToClientVersionData
      m
      NodeToNodeInitiatorResult
  mkDiffusionApplications
    stdGenVar
    consensusDiffusionArgs
    miniProtocolParams
    ntnApps
    ntcApps
    kernel =
      apps
     where
      apps =
        Diffusion.Applications
          { Diffusion.daApplicationInitiatorMode =
              combineVersions
                [ simpleSingletonVersions
                    version
                    llrnVersionDataNTN
                    ( \versionData ->
                        NTN.initiator llrnFeatureFlags miniProtocolParams version versionData
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
                        NTN.initiatorAndResponder llrnFeatureFlags miniProtocolParams version versionData $
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
          , Diffusion.daRethrowPolicy = consensusRethrowPolicy (Proxy @blk)
          , Diffusion.daReturnPolicy = returnPolicy
          , Diffusion.daRepromoteErrorDelay = Diffusion.repromoteErrorDelay
          , Diffusion.daLocalRethrowPolicy = localRethrowPolicy
          , daPeerSelectionPolicy =
              Cardano.Diffusion.simpleChurnModePeerSelectionPolicy
                stdGenVar
                (StrictSTM.readTVar $ Cardano.Diffusion.churnModeVar consensusDiffusionArgs)
                (Cardano.Diffusion.churnMetrics consensusDiffusionArgs)
          , Diffusion.daPeerSharingRegistry = getPeerSharingRegistry kernel
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
  Set CardanoFeatureFlag ->
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
  m (NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk)
mkNodeKernelArgs
  registry
  bfcSalt
  gsmAntiThunderingHerd
  rng
  cfg
  featureFlags
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
  getDiffusionPipeliningSupport =
    do
      let (kaRng, psRng) = split rng
      return
        NodeKernelArgs
          { tracers
          , registry
          , cfg
          , featureFlags
          , btime
          , chainDB
          , initChainDB = nodeInitChainDB
          , chainSyncFutureCheck
          , chainSyncHistoricityCheck
          , blockFetchSize = estimateBlockSize
          , mempoolCapacityOverride = NoMempoolCapacityBytesOverride
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
  Cardano.Diffusion.CardanoNodeArguments IO ->
  Cardano.Diffusion.CardanoConsensusArguments RemoteAddress IO ->
  Cardano.Diffusion.CardanoTracers IO ->
  Cardano.Diffusion.CardanoConfiguration IO ->
  Cardano.Diffusion.CardanoApplications IO a ->
  IO ()
stdRunDataDiffusion = \nodeArgs consensusArgs tracers config apps ->
  void $ Cardano.Diffusion.run nodeArgs consensusArgs tracers config apps

-- | Conveniently packaged 'LowLevelRunNodeArgs' arguments from a standard
-- non-testing invocation.
stdLowLevelRunNodeArgsIO ::
  forall blk.
  RunNode blk =>
  RunNodeArgs IO RemoteAddress LocalAddress blk ->
  StdRunNodeArgs IO blk ->
  IO
    ( LowLevelRunNodeArgs
        IO
        RemoteAddress
        LocalAddress
        blk
    )
stdLowLevelRunNodeArgsIO
  RunNodeArgs
    { rnProtocolInfo
    , rnPeerSharing
    , rnGenesisConfig
    , rnFeatureFlags
    }
  $(SafeWildCards.fields 'StdRunNodeArgs) = do
    llrnBfcSalt <- stdBfcSaltIO
    llrnRng <- newStdGen
    pure
      LowLevelRunNodeArgs
        { llrnBfcSalt
        , llrnChainSyncIdleTimeout = srnChainSyncIdleTimeout
        , llrnGenesisConfig = rnGenesisConfig
        , llrnCustomiseHardForkBlockchainTimeArgs = id
        , llrnRng
        , llrnMkImmutableHasFS = stdMkChainDbHasFS $ immutableDbPath srnDatabasePath
        , llrnMkVolatileHasFS = stdMkChainDbHasFS $ nonImmutableDbPath srnDatabasePath
        , llrnChainDbArgsDefaults = updateChainDbDefaults ChainDB.defaultArgs
        , llrnCustomiseChainDbArgs = id
        , llrnCustomiseNodeKernelArgs
        , llrnRunDataDiffusion =
            \_kernel cardanoConsensusDiffusionArgs apps ->
              stdRunDataDiffusion
                srnDiffusionArguments
                cardanoConsensusDiffusionArgs
                srnDiffusionTracers
                srnDiffusionConfiguration
                apps
        , llrnVersionDataNTC =
            stdVersionDataNTC networkMagic
        , llrnVersionDataNTN =
            stdVersionDataNTN
              networkMagic
              (Diffusion.dcMode srnDiffusionConfiguration)
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
            Diffusion.dcPublicPeerSelectionVar srnDiffusionConfiguration
        , llrnLdbFlavorArgs =
            srnLdbFlavorArgs
        , llrnFeatureFlags =
            rnFeatureFlags
        }
   where
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
