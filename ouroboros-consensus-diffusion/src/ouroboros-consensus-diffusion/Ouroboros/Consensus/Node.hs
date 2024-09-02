{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

-- | Run the whole Node
--
-- Intended for qualified import.
--
module Ouroboros.Consensus.Node (
    run
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
  , DiskPolicyArgs (..)
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
  , Tracers
  , Tracers' (..)
    -- * Internal helpers
  , mkNodeKernelArgs
  , nodeKernelArgsEnforceInvariants
  , openChainDB
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (DeserialiseFailure)
import qualified Control.Concurrent.Class.MonadSTM.Strict as StrictSTM
import           Control.DeepSeq (NFData)
import           Control.Monad (forM_, when)
import           Control.Monad.Class.MonadTime.SI (MonadTime)
import           Control.Monad.Class.MonadTimer.SI (MonadTimer)
import           Control.Tracer (Tracer, contramap, traceWith)
import           Data.ByteString.Lazy (ByteString)
import           Data.Functor.Contravariant (Predicate (..))
import           Data.Hashable (Hashable)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Time (NominalDiffTime)
import           Data.Typeable (Typeable)
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime hiding (getSystemStart)
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Fragment.InFuture (CheckInFuture,
                     ClockSkew)
import qualified Ouroboros.Consensus.Fragment.InFuture as InFuture
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState (..))
import qualified Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck as InFutureCheck
import qualified Ouroboros.Consensus.Network.NodeToClient as NTC
import qualified Ouroboros.Consensus.Network.NodeToNode as NTN
import           Ouroboros.Consensus.Node.DbLock
import           Ouroboros.Consensus.Node.DbMarker
import           Ouroboros.Consensus.Node.ErrorPolicy
import           Ouroboros.Consensus.Node.ExitPolicy
import           Ouroboros.Consensus.Node.Genesis (GenesisConfig (..),
                     GenesisNodeKernelArgs, mkGenesisNodeKernelArgs)
import           Ouroboros.Consensus.Node.GSM (GsmNodeKernelArgs (..))
import qualified Ouroboros.Consensus.Node.GSM as GSM
import           Ouroboros.Consensus.Node.InitStorage
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Recovery
import           Ouroboros.Consensus.Node.RethrowPolicy
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.NodeKernel
import           Ouroboros.Consensus.Storage.ChainDB (ChainDB, ChainDbArgs,
                     TraceEvent)
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Args as ChainDB
import           Ouroboros.Consensus.Storage.LedgerDB.DiskPolicy
                     (DiskPolicyArgs (..))
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.Time (secondsToNominalDiffTime)
import           Ouroboros.Network.BlockFetch (BlockFetchConfiguration (..))
import qualified Ouroboros.Network.Diffusion as Diffusion
import qualified Ouroboros.Network.Diffusion.Configuration as Diffusion
import qualified Ouroboros.Network.Diffusion.NonP2P as NonP2P
import qualified Ouroboros.Network.Diffusion.P2P as P2P
import           Ouroboros.Network.Magic
import           Ouroboros.Network.NodeToClient (ConnectionId, LocalAddress,
                     LocalSocket, NodeToClientVersionData (..), combineVersions,
                     simpleSingletonVersions)
import           Ouroboros.Network.NodeToNode (DiffusionMode (..),
                     ExceptionInHandler (..), MiniProtocolParameters,
                     NodeToNodeVersionData (..), RemoteAddress, Socket,
                     blockFetchPipeliningMax, defaultMiniProtocolParameters)
import           Ouroboros.Network.PeerSelection.Bootstrap (UseBootstrapPeers)
import           Ouroboros.Network.PeerSelection.LedgerPeers
                     (LedgerPeersConsensusInterface (..))
import           Ouroboros.Network.PeerSelection.PeerMetric (PeerMetrics,
                     newPeerMetric, reportMetric)
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing)
import           Ouroboros.Network.PeerSelection.PeerSharing.Codec
                     (decodeRemoteAddress, encodeRemoteAddress)
import           Ouroboros.Network.RethrowPolicy
import qualified SafeWildCards
import           System.Exit (ExitCode (..))
import           System.FilePath ((</>))
import           System.FS.API (SomeHasFS (..))
import           System.FS.API.Types (MountPoint (..))
import           System.FS.IO (ioHasFS)
import           System.Random (StdGen, newStdGen, randomIO, split)

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
data RunNodeArgs m addrNTN addrNTC blk (p2p :: Diffusion.P2P) = RunNodeArgs {
      -- | Consensus tracers
      rnTraceConsensus :: Tracers m (ConnectionId addrNTN) (ConnectionId addrNTC) blk

      -- | Protocol tracers for node-to-node communication
    , rnTraceNTN :: NTN.Tracers m (ConnectionId addrNTN) blk DeserialiseFailure

      -- | Protocol tracers for node-to-client communication
    , rnTraceNTC :: NTC.Tracers m (ConnectionId addrNTC) blk DeserialiseFailure

      -- | Protocol info
    , rnProtocolInfo :: ProtocolInfo blk

      -- | Hook called after the initialisation of the 'NodeKernel'
      --
      -- Called on the 'NodeKernel' after creating it, but before the network
      -- layer is initialised.
    , rnNodeKernelHook :: ResourceRegistry m
                       -> NodeKernel m addrNTN (ConnectionId addrNTC) blk
                       -> m ()

      -- | Network P2P Mode switch
    , rnEnableP2P :: NetworkP2PMode p2p

      -- | Network PeerSharing miniprotocol willingness flag
    , rnPeerSharing :: PeerSharing

    , rnGetUseBootstrapPeers :: STM m UseBootstrapPeers

    , rnGenesisConfig :: GenesisConfig
    }


-- | Arguments that usually only tests /directly/ specify.
--
-- A non-testing invocation probably wouldn't explicitly provide these values to
-- 'runWith'. The @cardano-node@, for example, instead calls the 'run'
-- abbreviation, which uses 'stdLowLevelRunNodeArgsIO' to indirectly specify
-- these low-level values from the higher-level 'StdRunNodeArgs'.
data LowLevelRunNodeArgs m addrNTN addrNTC versionDataNTN versionDataNTC blk
                         (p2p :: Diffusion.P2P) =
   LowLevelRunNodeArgs {

      -- | An action that will receive a marker indicating whether the previous
      -- shutdown was considered clean and a wrapper for installing a handler to
      -- create a clean file on exit if needed. See
      -- 'Ouroboros.Consensus.Node.Recovery.runWithCheckedDB'.
      llrnWithCheckedDB :: forall a. (  LastShutDownWasClean
                                     -> (ChainDB m blk -> m a -> m a)
                                     -> m a)
                        -> m a

      -- | The " static " ChainDB arguments
    , llrnChainDbArgsDefaults :: Incomplete ChainDbArgs m blk

      -- | File-system on which the directory for the ImmutableDB will
      -- be created.
    , llrnMkImmutableHasFS :: ChainDB.RelativeMountPoint -> SomeHasFS m

      -- | File-system on which the directories for databases other than the ImmutableDB will
      -- be created.
    , llrnMkVolatileHasFS :: ChainDB.RelativeMountPoint -> SomeHasFS m

      -- | Customise the 'ChainDbArgs'
    , llrnCustomiseChainDbArgs ::
           Complete ChainDbArgs m blk
        -> Complete ChainDbArgs m blk

      -- | Customise the 'NodeArgs'
    , llrnCustomiseNodeKernelArgs ::
           NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk
        -> NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk

      -- | Ie 'bfcSalt'
    , llrnBfcSalt :: Int

      -- | Ie 'gsmAntiThunderingHerd'
    , llrnGsmAntiThunderingHerd :: StdGen

      -- | Ie 'keepAliveRng'
    , llrnKeepAliveRng :: StdGen

      -- | Customise the 'HardForkBlockchainTimeArgs'
    , llrnCustomiseHardForkBlockchainTimeArgs ::
           HardForkBlockchainTimeArgs m blk
        -> HardForkBlockchainTimeArgs m blk

      -- | See 'NTN.ChainSyncTimeout'
    , llrnChainSyncTimeout :: m NTN.ChainSyncTimeout

    , llrnGenesisConfig :: GenesisConfig

      -- | How to run the data diffusion applications
      --
      -- 'run' will not return before this does.
    , llrnRunDataDiffusion ::
           Diffusion.Applications
             addrNTN NodeToNodeVersion   versionDataNTN
             addrNTC NodeToClientVersion versionDataNTC
             m NodeToNodeInitiatorResult
        -> Diffusion.ExtraApplications p2p addrNTN m NodeToNodeInitiatorResult
        -> m ()

    , llrnVersionDataNTC :: versionDataNTC

    , llrnVersionDataNTN :: versionDataNTN

      -- | node-to-node protocol versions to run.
    , llrnNodeToNodeVersions :: Map NodeToNodeVersion (BlockNodeToNodeVersion blk)

      -- | node-to-client protocol versions to run.
    , llrnNodeToClientVersions :: Map NodeToClientVersion (BlockNodeToClientVersion blk)

      -- | If the volatile tip is older than this, then the node will exit the
      -- @CaughtUp@ state.
    , llrnMaxCaughtUpAge :: NominalDiffTime

      -- | Maximum clock skew
    , llrnMaxClockSkew :: ClockSkew

    , llrnPublicPeerSelectionStateVar :: StrictSTM.StrictTVar m (Diffusion.PublicPeerSelectionState addrNTN)
    }

data NodeDatabasePaths =
    OnePathForAllDbs
      FilePath -- ^ Databases will be stored under this path, such that given a
               -- path @/foo@, databases will be in @/foo/{immutable,volatile,...}@.
  | MultipleDbPaths
      FilePath -- ^ Immutable path, usually pointing to a non-necessarily
               -- performant volume. ImmutableDB will be stored under this path,
               -- so given @/foo@, the ImmutableDB will be in @/foo/immutable@.
      FilePath -- ^ Non-immutable (volatile data) path, usually pointing to a
               -- performant volume. Databases other than the ImmutableDB will
               -- be stored under this path, so given @/bar@, it will contain
               -- @/bar/{volatile,ledger,...}@.

immutableDbPath :: NodeDatabasePaths -> FilePath
immutableDbPath (OnePathForAllDbs f)    = f
immutableDbPath (MultipleDbPaths imm _) = imm

nonImmutableDbPath :: NodeDatabasePaths -> FilePath
nonImmutableDbPath (OnePathForAllDbs f)    = f
nonImmutableDbPath (MultipleDbPaths _ vol) = vol

-- | Higher-level arguments that can determine the 'LowLevelRunNodeArgs' under
-- some usual assumptions for realistic use cases such as in @cardano-node@.
--
-- See 'stdLowLevelRunNodeArgsIO'.
data StdRunNodeArgs m blk (p2p :: Diffusion.P2P) = StdRunNodeArgs
  { srnBfcMaxConcurrencyBulkSync    :: Maybe Word
  , srnBfcMaxConcurrencyDeadline    :: Maybe Word
  , srnChainDbValidateOverride      :: Bool
    -- ^ If @True@, validate the ChainDB on init no matter what
  , srnDiskPolicyArgs               :: DiskPolicyArgs
  , srnDatabasePath                 :: NodeDatabasePaths
    -- ^ Location of the DBs
  , srnDiffusionArguments           :: Diffusion.Arguments
                                         IO
                                         Socket      RemoteAddress
                                         LocalSocket LocalAddress
  , srnDiffusionArgumentsExtra      :: Diffusion.ExtraArguments p2p m
  , srnDiffusionTracers             :: Diffusion.Tracers
                                         RemoteAddress  NodeToNodeVersion
                                         LocalAddress   NodeToClientVersion
                                         IO
  , srnDiffusionTracersExtra        :: Diffusion.ExtraTracers p2p
  , srnEnableInDevelopmentVersions  :: Bool
    -- ^ If @False@, then the node will limit the negotiated NTN and NTC
    -- versions to the latest " official " release (as chosen by Network and
    -- Consensus Team, with input from Node Team)
  , srnTraceChainDB                 :: Tracer m (ChainDB.TraceEvent blk)
  , srnMaybeMempoolCapacityOverride :: Maybe MempoolCapacityBytesOverride
    -- ^ Determine whether to use the system default mempool capacity or explicitly set
    -- capacity of the mempool.
  , srnChainSyncTimeout             :: Maybe (m NTN.ChainSyncTimeout)
    -- ^ A custom timeout for ChainSync.
  }

{-------------------------------------------------------------------------------
  Entrypoints to the Consensus Layer node functionality
-------------------------------------------------------------------------------}

-- | P2P Switch
--
data NetworkP2PMode (p2p :: Diffusion.P2P) where
    EnabledP2PMode  :: NetworkP2PMode 'Diffusion.P2P
    DisabledP2PMode :: NetworkP2PMode 'Diffusion.NonP2P

deriving instance Eq   (NetworkP2PMode p2p)
deriving instance Show (NetworkP2PMode p2p)

pure []

-- | Combination of 'runWith' and 'stdLowLevelRunArgsIO'
run :: forall blk p2p.
     RunNode blk
  => RunNodeArgs IO RemoteAddress LocalAddress blk p2p
  -> StdRunNodeArgs IO blk p2p
  -> IO ()
run args stdArgs = stdLowLevelRunNodeArgsIO args stdArgs >>= runWith args encodeRemoteAddress decodeRemoteAddress


-- | Extra constraints used by `ouroboros-network`.
--
type NetworkIO m = (
        MonadTime m,
        MonadTimer m,
        MonadLabelledSTM m
      )

-- | Extra constraints used by `ouroboros-network`.
type NetworkAddr addr = (
      Ord addr,
      Typeable addr,
      NoThunks addr,
      NFData addr
    )

-- | Start a node.
--
-- This opens the 'ChainDB', sets up the 'NodeKernel' and initialises the
-- network layer.
--
-- This function runs forever unless an exception is thrown.
runWith :: forall m addrNTN addrNTC versionDataNTN versionDataNTC blk p2p.
     ( RunNode blk
     , IOLike m
     , Hashable addrNTN -- the constraint comes from `initNodeKernel`
     , NetworkIO m
     , NetworkAddr addrNTN
     )
  => RunNodeArgs m addrNTN addrNTC blk p2p
  -> (NodeToNodeVersion -> addrNTN -> CBOR.Encoding)
  -> (NodeToNodeVersion -> forall s . CBOR.Decoder s addrNTN)
  -> LowLevelRunNodeArgs m addrNTN addrNTC versionDataNTN versionDataNTC blk p2p
  -> m ()
runWith RunNodeArgs{..} encAddrNtN decAddrNtN LowLevelRunNodeArgs{..} =

    llrnWithCheckedDB $ \(LastShutDownWasClean lastShutDownWasClean) continueWithCleanChainDB ->
    withRegistry $ \registry ->
      handleJust
             -- Ignore exception thrown in connection handlers and diffusion.
             -- Also ignore 'ExitSuccess'.
             (runPredicate $
                   (Predicate $ \err ->
                     (case fromException @ExceptionInLinkedThread err of
                       Just (ExceptionInLinkedThread _ err')
                         -> maybe True (/= ExitSuccess) $ fromException err'
                       Nothing -> False))
                <> (Predicate $ \err ->
                     isNothing (fromException @ExceptionInHandler err))
                <> (Predicate $ \err ->
                     isNothing (fromException @Diffusion.Failure err))
              )
              (\err -> traceWith (consensusErrorTracer rnTraceConsensus) err
                    >> throwIO err
              ) $ do
        let systemStart :: SystemStart
            systemStart = getSystemStart (configBlock cfg)

            systemTime :: SystemTime m
            systemTime = defaultSystemTime
                           systemStart
                           (blockchainTimeTracer rnTraceConsensus)

            inFuture :: CheckInFuture m blk
            inFuture = InFuture.reference
                         (configLedger cfg)
                         llrnMaxClockSkew
                         systemTime

        (genesisArgs, setLoEinChainDbArgs) <-
          mkGenesisNodeKernelArgs llrnGenesisConfig

        let maybeValidateAll
              | lastShutDownWasClean
              = id
              | otherwise
                -- When the last shutdown was not clean, validate the complete
                -- ChainDB to detect and recover from any disk corruption.
              = ChainDB.ensureValidateAll

        forM_ (sanityCheckConfig cfg) $ \issue ->
          traceWith (consensusSanityCheckTracer rnTraceConsensus) issue

        (chainDB, finalArgs) <- openChainDB
                     registry
                     inFuture
                     cfg
                     initLedger
                     llrnMkImmutableHasFS
                     llrnMkVolatileHasFS
                     llrnChainDbArgsDefaults
                     (  setLoEinChainDbArgs
                      . maybeValidateAll
                      . llrnCustomiseChainDbArgs
                     )

        continueWithCleanChainDB chainDB $ do
          btime <-
            hardForkBlockchainTime $
            llrnCustomiseHardForkBlockchainTimeArgs $
            HardForkBlockchainTimeArgs
              { hfbtBackoffDelay   = pure $ BackoffDelay 60
              , hfbtGetLedgerState =
                  ledgerState <$> ChainDB.getCurrentLedger chainDB
              , hfbtLedgerConfig   = configLedger cfg
              , hfbtRegistry       = registry
              , hfbtSystemTime     = systemTime
              , hfbtTracer         =
                  contramap (fmap (fromRelativeTime systemStart))
                    (blockchainTimeTracer rnTraceConsensus)
              , hfbtMaxClockRewind = secondsToNominalDiffTime 20
              }

          nodeKernelArgs <- do
              durationUntilTooOld <- GSM.realDurationUntilTooOld
                (configLedger cfg)
                (ledgerState <$> ChainDB.getCurrentLedger chainDB)
                llrnMaxCaughtUpAge
                systemTime
              let gsmMarkerFileView =
                    case ChainDB.cdbsHasFSGsmDB $ ChainDB.cdbsArgs finalArgs of
                        SomeHasFS x -> GSM.realMarkerFileView chainDB x
              fmap (nodeKernelArgsEnforceInvariants . llrnCustomiseNodeKernelArgs)
                $ mkNodeKernelArgs
                    registry
                    llrnBfcSalt
                    llrnGsmAntiThunderingHerd
                    llrnKeepAliveRng
                    cfg
                    rnTraceConsensus
                    btime
                    (InFutureCheck.realHeaderInFutureCheck llrnMaxClockSkew systemTime)
                    chainDB
                    llrnMaxCaughtUpAge
                    (Just durationUntilTooOld)
                    gsmMarkerFileView
                    rnGetUseBootstrapPeers
                    llrnPublicPeerSelectionStateVar
                    genesisArgs
          nodeKernel <- initNodeKernel nodeKernelArgs
          rnNodeKernelHook registry nodeKernel

          peerMetrics <- newPeerMetric Diffusion.peerMetricsConfiguration
          let ntnApps = mkNodeToNodeApps   nodeKernelArgs nodeKernel peerMetrics encAddrNtN decAddrNtN
              ntcApps = mkNodeToClientApps nodeKernelArgs nodeKernel
              (apps, appsExtra) = mkDiffusionApplications
                                        rnEnableP2P
                                        (miniProtocolParameters nodeKernelArgs)
                                        ntnApps
                                        ntcApps
                                        nodeKernel
                                        peerMetrics

          llrnRunDataDiffusion apps appsExtra
  where
    ProtocolInfo
      { pInfoConfig       = cfg
      , pInfoInitLedger   = initLedger
      } = rnProtocolInfo

    codecConfig :: CodecConfig blk
    codecConfig = configCodec cfg

    mkNodeToNodeApps
      :: NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk
      -> NodeKernel     m addrNTN (ConnectionId addrNTC) blk
      -> PeerMetrics m addrNTN
      -> (NodeToNodeVersion -> addrNTN -> CBOR.Encoding)
      -> (NodeToNodeVersion -> forall s . CBOR.Decoder s addrNTN)
      -> BlockNodeToNodeVersion blk
      -> NTN.Apps m
          addrNTN
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

    mkNodeToClientApps
      :: NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk
      -> NodeKernel     m addrNTN (ConnectionId addrNTC) blk
      -> BlockNodeToClientVersion blk
      -> NodeToClientVersion
      -> NTC.Apps m (ConnectionId addrNTC) ByteString ByteString ByteString ByteString ()
    mkNodeToClientApps nodeKernelArgs nodeKernel blockVersion networkVersion =
        NTC.mkApps
          nodeKernel
          rnTraceNTC
          (NTC.defaultCodecs codecConfig blockVersion networkVersion)
          (NTC.mkHandlers nodeKernelArgs nodeKernel)

    mkDiffusionApplications
      :: NetworkP2PMode p2p
      -> MiniProtocolParameters
      -> (   BlockNodeToNodeVersion blk
          -> NTN.Apps
               m
               addrNTN
               ByteString
               ByteString
               ByteString
               ByteString
               ByteString
               NodeToNodeInitiatorResult
               ()
        )
      -> (   BlockNodeToClientVersion blk
          -> NodeToClientVersion
          -> NTC.Apps
               m (ConnectionId addrNTC) ByteString ByteString ByteString ByteString ()
        )
      -> NodeKernel m addrNTN (ConnectionId addrNTC) blk
      -> PeerMetrics m addrNTN
      -> ( Diffusion.Applications
             addrNTN NodeToNodeVersion   versionDataNTN
             addrNTC NodeToClientVersion versionDataNTC
             m NodeToNodeInitiatorResult
         , Diffusion.ExtraApplications p2p addrNTN m NodeToNodeInitiatorResult
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
          , Diffusion.P2PApplications
              P2P.ApplicationsExtra {
                P2P.daRethrowPolicy          = consensusRethrowPolicy (Proxy @blk),
                P2P.daReturnPolicy           = returnPolicy,
                P2P.daLocalRethrowPolicy     = localRethrowPolicy,
                P2P.daPeerMetrics            = peerMetrics,
                P2P.daBlockFetchMode         = getFetchMode kernel,
                P2P.daPeerSharingRegistry    = getPeerSharingRegistry kernel
              }
          )
        DisabledP2PMode ->
          ( apps
          , Diffusion.NonP2PApplications
              NonP2P.ApplicationsExtra {
                NonP2P.daErrorPolicies = consensusErrorPolicy (Proxy @blk)
              }
          )
      where
        apps = Diffusion.Applications {
            Diffusion.daApplicationInitiatorMode =
              combineVersions
                [ simpleSingletonVersions
                    version
                    llrnVersionDataNTN
                    (NTN.initiator miniProtocolParams version rnPeerSharing
                      -- Initiator side won't start responder side of Peer
                      -- Sharing protocol so we give a dummy implementation
                      -- here.
                      $ ntnApps blockVersion)
                | (version, blockVersion) <- Map.toList llrnNodeToNodeVersions
                ],
            Diffusion.daApplicationInitiatorResponderMode =
              combineVersions
                [ simpleSingletonVersions
                    version
                    llrnVersionDataNTN
                    (NTN.initiatorAndResponder miniProtocolParams version rnPeerSharing
                      $ ntnApps blockVersion)
                | (version, blockVersion) <- Map.toList llrnNodeToNodeVersions
                ],
            Diffusion.daLocalResponderApplication =
              combineVersions
                [ simpleSingletonVersions
                    version
                    llrnVersionDataNTC
                    (NTC.responder version $ ntcApps blockVersion version)
                | (version, blockVersion) <- Map.toList llrnNodeToClientVersions
                ],
            Diffusion.daLedgerPeersCtx =
              LedgerPeersConsensusInterface {
                  lpGetLatestSlot = getImmTipSlot kernel,
                  lpGetLedgerPeers = fromMaybe [] <$> getPeersFromCurrentLedger kernel (const True),
                  lpGetLedgerStateJudgement = GSM.gsmStateToLedgerJudgement <$> getGsmState kernel
                },
            Diffusion.daUpdateOutboundConnectionsState =
              let varOcs = getOutboundConnectionsState kernel in \newOcs -> do
                oldOcs <- readTVar varOcs
                when (newOcs /= oldOcs) $ writeTVar varOcs newOcs
          }

        localRethrowPolicy :: RethrowPolicy
        localRethrowPolicy = mempty

    runPredicate :: Predicate a -> a -> Maybe a
    runPredicate (Predicate p) err = if p err then Just err else Nothing


-- | Check the DB marker, lock the DB and look for the clean shutdown marker.
--
-- Run the body action with the DB locked.
--
stdWithCheckedDB ::
     forall blk a. (StandardHash blk, Typeable blk)
  => Proxy blk
  -> Tracer IO (TraceEvent blk)
  -> FilePath
  -> NetworkMagic
  -> (LastShutDownWasClean -> (ChainDB IO blk -> IO a -> IO a) -> IO a)  -- ^ Body action with last shutdown was clean.
  -> IO a
stdWithCheckedDB pb tracer databasePath networkMagic body = do

    -- Check the DB marker first, before doing the lock file, since if the
    -- marker is not present, it expects an empty DB dir.
    either throwIO return =<< checkDbMarker
      hasFS
      mountPoint
      networkMagic

    -- Then create the lock file.
    withLockDB mountPoint $ runWithCheckedDB pb tracer hasFS body
  where
    mountPoint = MountPoint databasePath
    hasFS      = ioHasFS mountPoint

openChainDB ::
     forall m blk. (RunNode blk, IOLike m)
  => ResourceRegistry m
  -> CheckInFuture m blk
  -> TopLevelConfig blk
  -> ExtLedgerState blk
     -- ^ Initial ledger
  -> (ChainDB.RelativeMountPoint -> SomeHasFS m)
     -- ^ Immutable FS, see 'NodeDatabasePaths'
  -> (ChainDB.RelativeMountPoint -> SomeHasFS m)
     -- ^ Volatile FS, see 'NodeDatabasePaths'
  -> Incomplete ChainDbArgs m blk
     -- ^ A set of default arguments (possibly modified from 'defaultArgs')
  -> (Complete ChainDbArgs m blk -> Complete ChainDbArgs m blk)
      -- ^ Customise the 'ChainDbArgs'
  -> m (ChainDB m blk, Complete ChainDbArgs m blk)
openChainDB registry inFuture cfg initLedger fsImm fsVol defArgs customiseArgs =
   let args = customiseArgs $ ChainDB.completeChainDbArgs
               registry
               inFuture
               cfg
               initLedger
               (nodeImmutableDbChunkInfo (configStorage cfg))
               (nodeCheckIntegrity (configStorage cfg))
               fsImm
               fsVol
               defArgs
      in (,args) <$> ChainDB.openDB args

mkNodeKernelArgs ::
     forall m addrNTN addrNTC blk. (RunNode blk, IOLike m)
  => ResourceRegistry m
  -> Int
  -> StdGen
  -> StdGen
  -> TopLevelConfig blk
  -> Tracers m (ConnectionId addrNTN) (ConnectionId addrNTC) blk
  -> BlockchainTime m
  -> InFutureCheck.SomeHeaderInFutureCheck m blk
  -> ChainDB m blk
  -> NominalDiffTime
  -> Maybe (GSM.WrapDurationUntilTooOld m blk)
  -> GSM.MarkerFileView m
  -> STM m UseBootstrapPeers
  -> StrictSTM.StrictTVar m (Diffusion.PublicPeerSelectionState addrNTN)
  -> GenesisNodeKernelArgs m blk
  -> m (NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk)
mkNodeKernelArgs
  registry
  bfcSalt
  gsmAntiThunderingHerd
  rng
  cfg
  tracers
  btime
  chainSyncFutureCheck
  chainDB
  maxCaughtUpAge
  gsmDurationUntilTooOld
  gsmMarkerFileView
  getUseBootstrapPeers
  publicPeerSelectionStateVar
  genesisArgs
  = do
    let (kaRng, psRng) = split rng
    return NodeKernelArgs
      { tracers
      , registry
      , cfg
      , btime
      , chainDB
      , initChainDB             = nodeInitChainDB
      , chainSyncFutureCheck
      , blockFetchSize          = estimateBlockSize
      , mempoolCapacityOverride = NoMempoolCapacityBytesOverride
      , miniProtocolParameters  = defaultMiniProtocolParameters
      , blockFetchConfiguration = Diffusion.defaultBlockFetchConfiguration bfcSalt
      , gsmArgs = GsmNodeKernelArgs {
          gsmAntiThunderingHerd
        , gsmDurationUntilTooOld
        , gsmMarkerFileView
        , gsmMinCaughtUpDuration = maxCaughtUpAge
        }
      , getUseBootstrapPeers
      , keepAliveRng = kaRng
      , peerSharingRng = psRng
      , publicPeerSelectionStateVar
      , genesisArgs
      }

-- | We allow the user running the node to customise the 'NodeKernelArgs'
-- through 'llrnCustomiseNodeKernelArgs', but there are some limits to some
-- values. This function makes sure we don't exceed those limits and that the
-- values are consistent.
nodeKernelArgsEnforceInvariants ::
     NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk
  -> NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk
nodeKernelArgsEnforceInvariants nodeKernelArgs = nodeKernelArgs
    { miniProtocolParameters = miniProtocolParameters
        -- If 'blockFetchPipeliningMax' exceeds the configured default, it
        -- would be a protocol violation.
        { blockFetchPipeliningMax =
            min (blockFetchPipeliningMax miniProtocolParameters)
                (blockFetchPipeliningMax defaultMiniProtocolParameters)
        }
    , blockFetchConfiguration = blockFetchConfiguration
        -- 'bfcMaxRequestsInflight' must be <= 'blockFetchPipeliningMax'
        { bfcMaxRequestsInflight =
            min (bfcMaxRequestsInflight blockFetchConfiguration)
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
     FilePath
  -> ChainDB.RelativeMountPoint
  -> SomeHasFS IO
stdMkChainDbHasFS rootPath (ChainDB.RelativeMountPoint relPath) =
    SomeHasFS $ ioHasFS $ MountPoint $ rootPath </> relPath

stdBfcSaltIO :: IO Int
stdBfcSaltIO = randomIO

stdGsmAntiThunderingHerdIO :: IO StdGen
stdGsmAntiThunderingHerdIO = newStdGen

stdKeepAliveRngIO :: IO StdGen
stdKeepAliveRngIO = newStdGen

stdVersionDataNTN :: NetworkMagic
                  -> DiffusionMode
                  -> PeerSharing
                  -> NodeToNodeVersionData
stdVersionDataNTN networkMagic diffusionMode peerSharing = NodeToNodeVersionData
    { networkMagic
    , diffusionMode
    , peerSharing
    , query         = False
    }

stdVersionDataNTC :: NetworkMagic -> NodeToClientVersionData
stdVersionDataNTC networkMagic = NodeToClientVersionData
    { networkMagic
    , query        = False
    }

stdRunDataDiffusion ::
     Diffusion.Tracers
       RemoteAddress  NodeToNodeVersion
       LocalAddress   NodeToClientVersion
       IO
  -> Diffusion.ExtraTracers p2p
  -> Diffusion.Arguments
       IO
       Socket      RemoteAddress
       LocalSocket LocalAddress
  -> Diffusion.ExtraArguments p2p IO
  -> Diffusion.Applications
       RemoteAddress  NodeToNodeVersion   NodeToNodeVersionData
       LocalAddress   NodeToClientVersion NodeToClientVersionData
       IO NodeToNodeInitiatorResult
  -> Diffusion.ExtraApplications p2p RemoteAddress IO NodeToNodeInitiatorResult
  -> IO ()
stdRunDataDiffusion = Diffusion.run

-- | Conveniently packaged 'LowLevelRunNodeArgs' arguments from a standard
-- non-testing invocation.
stdLowLevelRunNodeArgsIO ::
     forall blk p2p. RunNode blk
  => RunNodeArgs IO RemoteAddress LocalAddress blk p2p
  -> StdRunNodeArgs IO blk p2p
  -> IO (LowLevelRunNodeArgs
          IO
          RemoteAddress
          LocalAddress
          NodeToNodeVersionData
          NodeToClientVersionData
          blk
          p2p)
stdLowLevelRunNodeArgsIO RunNodeArgs{ rnProtocolInfo
                                    , rnEnableP2P
                                    , rnPeerSharing
                                    , rnGenesisConfig
                                    }
                         $(SafeWildCards.fields 'StdRunNodeArgs) = do
    llrnBfcSalt               <- stdBfcSaltIO
    llrnGsmAntiThunderingHerd <- stdGsmAntiThunderingHerdIO
    llrnKeepAliveRng          <- stdKeepAliveRngIO
    pure LowLevelRunNodeArgs
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
          \apps extraApps ->
            stdRunDataDiffusion srnDiffusionTracers
                                srnDiffusionTracersExtra
                                srnDiffusionArguments
                                srnDiffusionArgumentsExtra
                                apps extraApps
      , llrnVersionDataNTC =
          stdVersionDataNTC networkMagic
      , llrnVersionDataNTN =
          stdVersionDataNTN
            networkMagic
            (case rnEnableP2P of
               EnabledP2PMode  -> Diffusion.daMode srnDiffusionArguments
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
      , llrnMaxCaughtUpAge = secondsToNominalDiffTime $ 20 * 60   -- 20 min
      , llrnMaxClockSkew =
          InFuture.defaultClockSkew
      , llrnPublicPeerSelectionStateVar =
          Diffusion.daPublicPeerSelectionVar srnDiffusionArguments
      }
  where
    networkMagic :: NetworkMagic
    networkMagic = getNetworkMagic $ configBlock $ pInfoConfig rnProtocolInfo

    updateChainDbDefaults ::
         Incomplete ChainDbArgs IO blk
      -> Incomplete ChainDbArgs IO blk
    updateChainDbDefaults =
          ChainDB.updateDiskPolicyArgs srnDiskPolicyArgs
        . ChainDB.updateTracer srnTraceChainDB
        . (if   not srnChainDbValidateOverride
           then id
           else ChainDB.ensureValidateAll)


    llrnCustomiseNodeKernelArgs ::
         NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk
      -> NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk
    llrnCustomiseNodeKernelArgs =
        overBlockFetchConfiguration modifyBlockFetchConfiguration
      . modifyMempoolCapacityOverride
      where
        modifyBlockFetchConfiguration =
            maybe id
              (\mc bfc -> bfc { bfcMaxConcurrencyDeadline = mc })
              srnBfcMaxConcurrencyDeadline
          . maybe id
              (\mc bfc -> bfc { bfcMaxConcurrencyBulkSync = mc })
              srnBfcMaxConcurrencyBulkSync
        modifyMempoolCapacityOverride =
            maybe id
              (\mc nka -> nka { mempoolCapacityOverride = mc })
              srnMaybeMempoolCapacityOverride

    -- Limit the node version unless srnEnableInDevelopmentVersions is set
    limitToLatestReleasedVersion :: forall k v.
         Ord k
      => ((Maybe NodeToNodeVersion, Maybe NodeToClientVersion) -> Maybe k)
      -> Map k v
      -> Map k v
    limitToLatestReleasedVersion prj =
        if srnEnableInDevelopmentVersions then id
        else
        case prj $ latestReleasedNodeVersion (Proxy @blk) of
          Nothing      -> id
          Just version -> Map.takeWhileAntitone (<= version)

{-------------------------------------------------------------------------------
  Miscellany
-------------------------------------------------------------------------------}

overBlockFetchConfiguration ::
     (BlockFetchConfiguration -> BlockFetchConfiguration)
  -> NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk
  -> NodeKernelArgs m addrNTN (ConnectionId addrNTC) blk
overBlockFetchConfiguration f args = args {
      blockFetchConfiguration = f blockFetchConfiguration
    }
  where
    NodeKernelArgs { blockFetchConfiguration } = args
