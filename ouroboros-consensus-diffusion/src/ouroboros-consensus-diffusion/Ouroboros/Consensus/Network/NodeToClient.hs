{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Intended for qualified import
module Ouroboros.Consensus.Network.NodeToClient
  ( -- * Handlers
    Handlers (..)
  , mkHandlers

    -- * Codecs
  , ClientCodecs
  , Codecs
  , Codecs' (..)
  , DefaultCodecs
  , clientCodecs
  , defaultCodecs
  , identityCodecs

    -- * ClientCodecs

    -- * Tracers
  , Tracers
  , Tracers' (..)
  , nullTracers
  , showTracers

    -- * Applications
  , App
  , Apps (..)
  , mkApps

    -- ** Projections
  , responder
  ) where

import Codec.CBOR.Decoding (Decoder)
import Codec.CBOR.Encoding (Encoding)
import Codec.CBOR.Read (DeserialiseFailure)
import Codec.Serialise (Serialise)
import Control.ResourceRegistry
import Control.Tracer
import Data.ByteString.Lazy (ByteString)
import Data.Typeable
import Data.Void (Void)
import qualified Network.Mux as Mux
import Network.TypedProtocol.Codec
import qualified Network.TypedProtocol.Stateful.Codec as Stateful
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Ledger.Extended
import Ouroboros.Consensus.Ledger.Query
import Ouroboros.Consensus.Ledger.SupportsMempool
import Ouroboros.Consensus.Ledger.SupportsProtocol
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import Ouroboros.Consensus.MiniProtocol.LocalStateQuery.Server
import Ouroboros.Consensus.MiniProtocol.LocalTxMonitor.Server
import Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.Run
import Ouroboros.Consensus.Node.Serialisation
import qualified Ouroboros.Consensus.Node.Tracers as Node
import Ouroboros.Consensus.NodeKernel
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Util (ShowProxy)
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Consensus.Util.Orphans ()
import Ouroboros.Network.Block
  ( Serialised
  , decodePoint
  , decodeTip
  , encodePoint
  , encodeTip
  )
import Ouroboros.Network.BlockFetch
import Ouroboros.Network.Channel
import Ouroboros.Network.Context
import Ouroboros.Network.Driver
import qualified Ouroboros.Network.Driver.Stateful as Stateful
import Ouroboros.Network.Mux
import Ouroboros.Network.NodeToClient hiding
  ( NodeToClientVersion (..)
  )
import qualified Ouroboros.Network.NodeToClient as N
  ( NodeToClientVersion (..)
  , NodeToClientVersionData
  )
import Ouroboros.Network.Protocol.ChainSync.Codec
import Ouroboros.Network.Protocol.ChainSync.Server
import Ouroboros.Network.Protocol.ChainSync.Type
import Ouroboros.Network.Protocol.LocalStateQuery.Codec
import Ouroboros.Network.Protocol.LocalStateQuery.Server
import Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import Ouroboros.Network.Protocol.LocalTxMonitor.Codec
import Ouroboros.Network.Protocol.LocalTxMonitor.Server
import Ouroboros.Network.Protocol.LocalTxMonitor.Type
import Ouroboros.Network.Protocol.LocalTxSubmission.Codec
import Ouroboros.Network.Protocol.LocalTxSubmission.Server
import Ouroboros.Network.Protocol.LocalTxSubmission.Type

{-------------------------------------------------------------------------------
  Handlers
-------------------------------------------------------------------------------}

-- | Protocol handlers for node-to-client (local) communication
data Handlers m peer blk = Handlers
  { hChainSyncServer ::
      ChainDB.Follower m blk (ChainDB.WithPoint blk (Serialised blk)) ->
      ChainSyncServer (Serialised blk) (Point blk) (Tip blk) m ()
  , hTxSubmissionServer ::
      LocalTxSubmissionServer (GenTx blk) (ApplyTxErr blk) m ()
  , hStateQueryServer ::
      ResourceRegistry m ->
      LocalStateQueryServer blk (Point blk) (Query blk) m ()
  , hTxMonitorServer ::
      LocalTxMonitorServer (GenTxId blk) (GenTx blk) SlotNo m ()
  }

mkHandlers ::
  forall m blk addrNTN addrNTC.
  ( IOLike m
  , LedgerSupportsMempool blk
  , LedgerSupportsProtocol blk
  , BlockSupportsLedgerQuery blk
  , ConfigSupportsNode blk
  ) =>
  NodeKernelArgs m addrNTN addrNTC blk ->
  NodeKernel m addrNTN addrNTC blk ->
  Handlers m addrNTC blk
mkHandlers NodeKernelArgs{cfg, tracers} NodeKernel{getChainDB, getMempool} =
  Handlers
    { hChainSyncServer =
        chainSyncBlocksServer
          (Node.chainSyncServerBlockTracer tracers)
          getChainDB
    , hTxSubmissionServer =
        localTxSubmissionServer
          (Node.localTxSubmissionServerTracer tracers)
          getMempool
    , hStateQueryServer =
        localStateQueryServer (ExtLedgerCfg cfg)
          . ChainDB.getReadOnlyForkerAtPoint getChainDB
    , hTxMonitorServer =
        localTxMonitorServer
          getMempool
    }

{-------------------------------------------------------------------------------
  Codecs
-------------------------------------------------------------------------------}

-- | Node-to-client protocol codecs needed to run 'Handlers'.
data Codecs' blk serialisedBlk e m bCS bTX bSQ bTM = Codecs
  { cChainSyncCodec :: Codec (ChainSync serialisedBlk (Point blk) (Tip blk)) e m bCS
  , cTxSubmissionCodec :: Codec (LocalTxSubmission (GenTx blk) (ApplyTxErr blk)) e m bTX
  , cStateQueryCodec ::
      Stateful.Codec (LocalStateQuery blk (Point blk) (Query blk)) e LocalStateQuery.State m bSQ
  , cTxMonitorCodec :: Codec (LocalTxMonitor (GenTxId blk) (GenTx blk) SlotNo) e m bTM
  }

type Codecs blk e m bCS bTX bSQ bTM =
  Codecs' blk (Serialised blk) e m bCS bTX bSQ bTM
type DefaultCodecs blk m =
  Codecs' blk (Serialised blk) DeserialiseFailure m ByteString ByteString ByteString ByteString
type ClientCodecs blk m =
  Codecs' blk blk DeserialiseFailure m ByteString ByteString ByteString ByteString

-- | Protocol codecs for the node-to-client protocols
--
-- We pass the 'BlockConfig' here, even though it is currently unused. If at any
-- point we want to introduce local protocols that for example send Byron blocks
-- or headers across, we will need to have the epoch size, which comes from the
-- Byron config. Unlike the full 'TopLevelConfig', it should not be difficult
-- for a wallet to construct the 'BlockConfig'.
--
-- NOTE: Somewhat confusingly, 'pcChainSyncCodec' currently /does/ send Byron
-- blocks across, but it does not deserialize them (the user of the codec is
-- itself responsible for doing that), which is why it currently does not need
-- the config.
--
-- Implementation mode: currently none of the consensus encoders/decoders do
-- anything different based on the version, so @_version@ is unused; it's just
-- that not all codecs are used, depending on the version number.
defaultCodecs ::
  forall m blk.
  ( MonadST m
  , SerialiseNodeToClientConstraints blk
  , BlockSupportsLedgerQuery blk
  , Show (BlockNodeToClientVersion blk)
  , StandardHash blk
  , Serialise (HeaderHash blk)
  ) =>
  CodecConfig blk ->
  BlockNodeToClientVersion blk ->
  N.NodeToClientVersion ->
  DefaultCodecs blk m
defaultCodecs ccfg version networkVersion =
  Codecs
    { cChainSyncCodec =
        codecChainSync
          enc
          dec
          (encodePoint (encodeRawHash p))
          (decodePoint (decodeRawHash p))
          (encodeTip (encodeRawHash p))
          (decodeTip (decodeRawHash p))
    , cTxSubmissionCodec =
        codecLocalTxSubmission
          enc
          dec
          enc
          dec
    , cStateQueryCodec =
        codecLocalStateQuery
          networkVersion
          (encodePoint (encodeRawHash p))
          (decodePoint (decodeRawHash p))
          (queryEncodeNodeToClient ccfg queryVersion version . SomeSecond)
          ((\(SomeSecond q) -> Some q) <$> queryDecodeNodeToClient ccfg queryVersion version)
          (encodeResult ccfg version)
          (decodeResult ccfg version)
    , cTxMonitorCodec =
        codecLocalTxMonitor
          networkVersion
          enc
          dec
          enc
          dec
          enc
          dec
    }
 where
  queryVersion :: QueryVersion
  queryVersion = nodeToClientVersionToQueryVersion networkVersion

  p :: Proxy blk
  p = Proxy

  enc :: SerialiseNodeToClient blk a => a -> Encoding
  enc = encodeNodeToClient ccfg version

  dec :: SerialiseNodeToClient blk a => forall s. Decoder s a
  dec = decodeNodeToClient ccfg version

-- | Protocol codecs for the node-to-client protocols which serialise
-- / deserialise blocks in /chain-sync/ protocol.
clientCodecs ::
  forall m blk.
  ( MonadST m
  , SerialiseNodeToClientConstraints blk
  , BlockSupportsLedgerQuery blk
  , Show (BlockNodeToClientVersion blk)
  , StandardHash blk
  , Serialise (HeaderHash blk)
  ) =>
  CodecConfig blk ->
  BlockNodeToClientVersion blk ->
  N.NodeToClientVersion ->
  ClientCodecs blk m
clientCodecs ccfg version networkVersion =
  Codecs
    { cChainSyncCodec =
        codecChainSync
          enc
          dec
          (encodePoint (encodeRawHash p))
          (decodePoint (decodeRawHash p))
          (encodeTip (encodeRawHash p))
          (decodeTip (decodeRawHash p))
    , cTxSubmissionCodec =
        codecLocalTxSubmission
          enc
          dec
          enc
          dec
    , cStateQueryCodec =
        codecLocalStateQuery
          networkVersion
          (encodePoint (encodeRawHash p))
          (decodePoint (decodeRawHash p))
          (queryEncodeNodeToClient ccfg queryVersion version . SomeSecond)
          ((\(SomeSecond q) -> Some q) <$> queryDecodeNodeToClient ccfg queryVersion version)
          (encodeResult ccfg version)
          (decodeResult ccfg version)
    , cTxMonitorCodec =
        codecLocalTxMonitor
          networkVersion
          enc
          dec
          enc
          dec
          enc
          dec
    }
 where
  queryVersion :: QueryVersion
  queryVersion = nodeToClientVersionToQueryVersion networkVersion

  p :: Proxy blk
  p = Proxy

  enc :: SerialiseNodeToClient blk a => a -> Encoding
  enc = encodeNodeToClient ccfg version

  dec :: SerialiseNodeToClient blk a => forall s. Decoder s a
  dec = decodeNodeToClient ccfg version

-- | Identity codecs used in tests.
identityCodecs ::
  (Monad m, BlockSupportsLedgerQuery blk) =>
  Codecs
    blk
    CodecFailure
    m
    (AnyMessage (ChainSync (Serialised blk) (Point blk) (Tip blk)))
    (AnyMessage (LocalTxSubmission (GenTx blk) (ApplyTxErr blk)))
    (Stateful.AnyMessage (LocalStateQuery blk (Point blk) (Query blk)) LocalStateQuery.State)
    (AnyMessage (LocalTxMonitor (GenTxId blk) (GenTx blk) SlotNo))
identityCodecs =
  Codecs
    { cChainSyncCodec = codecChainSyncId
    , cTxSubmissionCodec = codecLocalTxSubmissionId
    , cStateQueryCodec = codecLocalStateQueryId sameDepIndex
    , cTxMonitorCodec = codecLocalTxMonitorId
    }

{-------------------------------------------------------------------------------
  Tracers
-------------------------------------------------------------------------------}

-- | A record of 'Tracer's for the different protocols.
type Tracers m peer blk e =
  Tracers' peer blk e (Tracer m)

data Tracers' peer blk e f = Tracers
  { tChainSyncTracer ::
      f (TraceLabelPeer peer (TraceSendRecv (ChainSync (Serialised blk) (Point blk) (Tip blk))))
  , tTxSubmissionTracer ::
      f (TraceLabelPeer peer (TraceSendRecv (LocalTxSubmission (GenTx blk) (ApplyTxErr blk))))
  , tStateQueryTracer ::
      f
        ( TraceLabelPeer
            peer
            (Stateful.TraceSendRecv (LocalStateQuery blk (Point blk) (Query blk)) LocalStateQuery.State)
        )
  , tTxMonitorTracer ::
      f (TraceLabelPeer peer (TraceSendRecv (LocalTxMonitor (GenTxId blk) (GenTx blk) SlotNo)))
  }

instance (forall a. Semigroup (f a)) => Semigroup (Tracers' peer blk e f) where
  l <> r =
    Tracers
      { tChainSyncTracer = f tChainSyncTracer
      , tTxSubmissionTracer = f tTxSubmissionTracer
      , tStateQueryTracer = f tStateQueryTracer
      , tTxMonitorTracer = f tTxMonitorTracer
      }
   where
    f ::
      forall a.
      Semigroup a =>
      (Tracers' peer blk e f -> a) ->
      a
    f prj = prj l <> prj r

-- | Use a 'nullTracer' for each protocol.
nullTracers :: Monad m => Tracers m peer blk e
nullTracers =
  Tracers
    { tChainSyncTracer = nullTracer
    , tTxSubmissionTracer = nullTracer
    , tStateQueryTracer = nullTracer
    , tTxMonitorTracer = nullTracer
    }

showTracers ::
  ( Show peer
  , Show (GenTx blk)
  , Show (GenTxId blk)
  , Show (ApplyTxErr blk)
  , forall fp. ShowQuery (BlockQuery blk fp)
  , HasHeader blk
  ) =>
  Tracer m String -> Tracers m peer blk e
showTracers tr =
  Tracers
    { tChainSyncTracer = showTracing tr
    , tTxSubmissionTracer = showTracing tr
    , tStateQueryTracer = showTracing tr
    , tTxMonitorTracer = showTracing tr
    }

{-------------------------------------------------------------------------------
  Applications
-------------------------------------------------------------------------------}

-- | A node-to-client application
type App m peer bytes a = peer -> Channel m bytes -> m (a, Maybe bytes)

-- | Applications for the node-to-client (i.e., local) protocols
--
-- See 'Network.Mux.Types.MuxApplication'
data Apps m peer bCS bTX bSQ bTM a = Apps
  { aChainSyncServer :: App m peer bCS a
  -- ^ Start a local chain sync server.
  , aTxSubmissionServer :: App m peer bTX a
  -- ^ Start a local transaction submission server.
  , aStateQueryServer :: App m peer bSQ a
  -- ^ Start a local state query server.
  , aTxMonitorServer :: App m peer bTM a
  -- ^ Start a local transaction monitor server
  }

-- | Construct the 'NetworkApplication' for the node-to-client protocols
mkApps ::
  forall m addrNTN addrNTC blk e bCS bTX bSQ bTM.
  ( IOLike m
  , Exception e
  , ShowProxy blk
  , ShowProxy (ApplyTxErr blk)
  , ShowProxy (GenTx blk)
  , ShowProxy (GenTxId blk)
  , ShowProxy (Query blk)
  , forall fp. ShowQuery (BlockQuery blk fp)
  ) =>
  NodeKernel m addrNTN addrNTC blk ->
  Tracers m addrNTC blk e ->
  Codecs blk e m bCS bTX bSQ bTM ->
  Handlers m addrNTC blk ->
  Apps m addrNTC bCS bTX bSQ bTM ()
mkApps kernel Tracers{..} Codecs{..} Handlers{..} =
  Apps{..}
 where
  aChainSyncServer ::
    addrNTC ->
    Channel m bCS ->
    m ((), Maybe bCS)
  aChainSyncServer them channel = do
    labelThisThread "LocalChainSyncServer"
    bracketWithPrivateRegistry
      (chainSyncBlockServerFollower (getChainDB kernel))
      ChainDB.followerClose
      $ \flr ->
        runPeer
          (contramap (TraceLabelPeer them) tChainSyncTracer)
          cChainSyncCodec
          channel
          $ chainSyncServerPeer
          $ hChainSyncServer flr

  aTxSubmissionServer ::
    addrNTC ->
    Channel m bTX ->
    m ((), Maybe bTX)
  aTxSubmissionServer them channel = do
    labelThisThread "LocalTxSubmissionServer"
    runPeer
      (contramap (TraceLabelPeer them) tTxSubmissionTracer)
      cTxSubmissionCodec
      channel
      (localTxSubmissionServerPeer (pure hTxSubmissionServer))

  aStateQueryServer ::
    addrNTC ->
    Channel m bSQ ->
    m ((), Maybe bSQ)
  aStateQueryServer them channel = do
    labelThisThread "LocalStateQueryServer"
    withRegistry $ \rr ->
      Stateful.runPeer
        (contramap (TraceLabelPeer them) tStateQueryTracer)
        cStateQueryCodec
        channel
        LocalStateQuery.StateIdle
        (localStateQueryServerPeer (hStateQueryServer rr))

  aTxMonitorServer ::
    addrNTC ->
    Channel m bTM ->
    m ((), Maybe bTM)
  aTxMonitorServer them channel = do
    labelThisThread "LocalTxMonitorServer"
    runPeer
      (contramap (TraceLabelPeer them) tTxMonitorTracer)
      cTxMonitorCodec
      channel
      (localTxMonitorServerPeer hTxMonitorServer)

{-------------------------------------------------------------------------------
  Projections from 'Apps'
-------------------------------------------------------------------------------}

-- | A projection from 'NetworkApplication' to a server-side
-- 'OuroborosApplication' for the node-to-client protocols.
responder ::
  N.NodeToClientVersion ->
  N.NodeToClientVersionData ->
  Apps m (ConnectionId peer) b b b b a ->
  OuroborosApplicationWithMinimalCtx 'Mux.ResponderMode peer b m Void a
responder version versionData Apps{..} =
  nodeToClientProtocols
    ( NodeToClientProtocols
        { localChainSyncProtocol =
            ResponderProtocolOnly $ MiniProtocolCb $ \ctx ->
              aChainSyncServer (rcConnectionId ctx)
        , localTxSubmissionProtocol =
            ResponderProtocolOnly $ MiniProtocolCb $ \ctx ->
              aTxSubmissionServer (rcConnectionId ctx)
        , localStateQueryProtocol =
            ResponderProtocolOnly $ MiniProtocolCb $ \ctx ->
              aStateQueryServer (rcConnectionId ctx)
        , localTxMonitorProtocol =
            ResponderProtocolOnly $ MiniProtocolCb $ \ctx ->
              aTxMonitorServer (rcConnectionId ctx)
        }
    )
    version
    versionData
