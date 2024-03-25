{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.ImmDBServer.Diffusion (run) where

import           Cardano.Tools.ImmDBServer.MiniProtocols (immDBServer)
import           Control.Tracer
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Contravariant ((>$<))
import           Data.Void (Void)
import           Network.Socket (SockAddr (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
import           Ouroboros.Consensus.Node.InitStorage
                     (NodeInitStorage (nodeCheckIntegrity, nodeImmutableDbChunkInfo))
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints)
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDbArgs (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Mux
import qualified Ouroboros.Network.NodeToNode as N2N
import           Ouroboros.Network.PeerSelection.PeerSharing.Codec
                     (decodeRemoteAddress, encodeRemoteAddress)
import qualified Ouroboros.Network.Snocket as Snocket
import           Ouroboros.Network.Socket (configureSocket)
import           System.FS.API (SomeHasFS (..))
import           System.FS.API.Types (MountPoint (MountPoint))
import           System.FS.IO (ioHasFS)

-- | Glue code for using just the bits from the Diffusion Layer that we need in
-- this context.
serve ::
     SockAddr
  -> N2N.Versions N2N.NodeToNodeVersion N2N.NodeToNodeVersionData
       (OuroborosApplicationWithMinimalCtx 'ResponderMode SockAddr BL.ByteString IO Void ())
  -> IO Void
serve sockAddr application = withIOManager \iocp -> do
    let sn     = Snocket.socketSnocket iocp
        family = Snocket.addrFamily sn sockAddr
    bracket (Snocket.open sn family) (Snocket.close sn) \socket -> do
      networkMutableState <- N2N.newNetworkMutableState
      configureSocket socket (Just sockAddr)
      Snocket.bind sn socket sockAddr
      Snocket.listen sn socket
      N2N.withServer
        sn
        N2N.nullNetworkServerTracers {
          N2N.nstHandshakeTracer   = show >$< stdoutTracer
        , N2N.nstErrorPolicyTracer = show >$< stdoutTracer
        }
        networkMutableState
        acceptedConnectionsLimit
        socket
        application
        nullErrorPolicies
  where
    acceptedConnectionsLimit = N2N.AcceptedConnectionsLimit {
          N2N.acceptedConnectionsHardLimit = maxBound
        , N2N.acceptedConnectionsSoftLimit = maxBound
        , N2N.acceptedConnectionsDelay     = 0
        }

run ::
     forall blk.
     ( GetPrevHash blk
     , ShowProxy blk
     , SupportedNetworkProtocolVersion blk
     , SerialiseNodeToNodeConstraints blk
     , ImmutableDB.ImmutableDbSerialiseConstraints blk
     , NodeInitStorage blk
     , ConfigSupportsNode blk
     )
  => FilePath
  -> SockAddr
  -> TopLevelConfig blk
  -> IO Void
run immDBDir sockAddr cfg = withRegistry \registry ->
    ImmutableDB.withDB
      (ImmutableDB.openDB (immDBArgs registry) runWithTempRegistry)
      \immDB -> serve sockAddr $ immDBServer
        codecCfg
        encodeRemoteAddress
        decodeRemoteAddress
        immDB
        networkMagic
  where
    immDBArgs registry = ImmutableDB.defaultArgs {
          immCheckIntegrity = nodeCheckIntegrity storageCfg
        , immChunkInfo      = nodeImmutableDbChunkInfo storageCfg
        , immCodecConfig    = codecCfg
        , immRegistry       = registry
        , immHasFS          = SomeHasFS $ ioHasFS $ MountPoint immDBDir
        }

    codecCfg     = configCodec cfg
    storageCfg   = configStorage cfg
    networkMagic = getNetworkMagic . configBlock $ cfg
