{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tools.ImmDBServer.Diffusion (run) where

import qualified Cardano.Network.NodeToNode as N2N
import Cardano.Tools.ImmDBServer.MiniProtocols (immDBServer)
import Control.ResourceRegistry
import Control.Tracer
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Contravariant ((>$<))
import Data.Void (Void)
import qualified Network.Mux as Mux
import Network.Socket (SockAddr (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config
import Ouroboros.Consensus.Config.SupportsNode
import Ouroboros.Consensus.Node.InitStorage
  ( NodeInitStorage (nodeCheckIntegrity, nodeImmutableDbChunkInfo)
  )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints)
import Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDbArgs (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.IOManager (withIOManager)
import Ouroboros.Network.Mux
import Ouroboros.Network.PeerSelection.PeerSharing.Codec
  ( decodeRemoteAddress
  , encodeRemoteAddress
  )
import Ouroboros.Network.Protocol.Handshake (HandshakeArguments (..))
import qualified Ouroboros.Network.Protocol.Handshake as Handshake
import qualified Ouroboros.Network.Server.Simple as Server
import qualified Ouroboros.Network.Snocket as Snocket
import Ouroboros.Network.Socket (SomeResponderApplication (..), configureSocket)
import System.FS.API (SomeHasFS (..))
import System.FS.API.Types (MountPoint (MountPoint))
import System.FS.IO (ioHasFS)

-- | Glue code for using just the bits from the Diffusion Layer that we need in
-- this context.
serve ::
  SockAddr ->
  N2N.Versions
    N2N.NodeToNodeVersion
    N2N.NodeToNodeVersionData
    (OuroborosApplicationWithMinimalCtx 'Mux.ResponderMode SockAddr BL.ByteString IO Void ()) ->
  IO Void
serve sockAddr application = withIOManager \iocp ->
  Server.with
    (Snocket.socketSnocket iocp)
    Snocket.makeSocketBearer
    (\sock addr -> configureSocket sock (Just addr))
    sockAddr
    HandshakeArguments
      { haHandshakeTracer = show >$< stdoutTracer
      , haBearerTracer = show >$< stdoutTracer
      , haHandshakeCodec = Handshake.nodeToNodeHandshakeCodec
      , haVersionDataCodec = Handshake.cborTermVersionDataCodec N2N.nodeToNodeCodecCBORTerm
      , haAcceptVersion = Handshake.acceptableVersion
      , haQueryVersion = Handshake.queryVersion
      , haTimeLimits = Handshake.timeLimitsHandshake
      }
    (SomeResponderApplication <$> application)
    (\_ serverAsync -> wait serverAsync)

run ::
  forall blk.
  ( GetPrevHash blk
  , ShowProxy blk
  , SupportedNetworkProtocolVersion blk
  , SerialiseNodeToNodeConstraints blk
  , ImmutableDB.ImmutableDbSerialiseConstraints blk
  , NodeInitStorage blk
  , ConfigSupportsNode blk
  ) =>
  FilePath ->
  SockAddr ->
  TopLevelConfig blk ->
  IO Void
run immDBDir sockAddr cfg = withRegistry \registry ->
  ImmutableDB.withDB
    (ImmutableDB.openDB (immDBArgs registry) runWithTempRegistry)
    \immDB ->
      serve sockAddr $
        immDBServer
          codecCfg
          encodeRemoteAddress
          decodeRemoteAddress
          immDB
          networkMagic
 where
  immDBArgs registry =
    ImmutableDB.defaultArgs
      { immCheckIntegrity = nodeCheckIntegrity storageCfg
      , immChunkInfo = nodeImmutableDbChunkInfo storageCfg
      , immCodecConfig = codecCfg
      , immRegistry = registry
      , immHasFS = SomeHasFS $ ioHasFS $ MountPoint immDBDir
      }

  codecCfg = configCodec cfg
  storageCfg = configStorage cfg
  networkMagic = getNetworkMagic . configBlock $ cfg
