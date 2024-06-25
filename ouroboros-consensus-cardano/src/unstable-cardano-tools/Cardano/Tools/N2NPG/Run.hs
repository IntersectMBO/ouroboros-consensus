{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Tools.N2NPG.Run (
    Opts (..)
  , run
  ) where

import qualified Cardano.Tools.DBAnalyser.Block.Cardano as Cardano
import           Cardano.Tools.DBAnalyser.HasAnalysis (mkProtocolInfo)
import           Control.Monad.Class.MonadSay (MonadSay (..))
import           Control.Monad.Cont
import           Control.Monad.Trans (MonadTrans (..))
import           Control.Tracer (nullTracer, stdoutTracer)
import qualified Data.ByteString.Lazy as BL
import           Data.Functor ((<&>))
import           Data.Functor.Contravariant ((>$<))
import qualified Data.Map.Strict as Map
import           Data.Void (Void)
import           Data.Word (Word64)
import qualified Network.Socket as Socket
import           Network.TypedProtocol (PeerHasAgency (..), PeerPipelined (..),
                     PeerRole (..), PeerSender (..))
import           Network.TypedProtocol.Pipelined (N (..))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Config.SupportsNode
                     (ConfigSupportsNode (..))
import           Ouroboros.Consensus.Ledger.SupportsProtocol
import           Ouroboros.Consensus.Network.NodeToNode (Codecs (..),
                     defaultCodecs)
import           Ouroboros.Consensus.Node hiding (run)
import           Ouroboros.Consensus.Node.InitStorage
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Storage.ImmutableDB (ImmutableDbArgs (..))
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmutableDB
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.Args
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Network.Block (Tip)
import           Ouroboros.Network.Diffusion.Configuration (PeerSharing (..))
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Mux (MiniProtocol (..),
                     MiniProtocolLimits (..), MuxMode (..),
                     OuroborosApplication (..),
                     OuroborosApplicationWithMinimalCtx, RunMiniProtocol (..),
                     mkMiniProtocolCbFromPeerPipelined)
import           Ouroboros.Network.NodeToNode (DiffusionMode (..),
                     NetworkConnectTracers (..), NodeToNodeVersionData,
                     Versions (..), blockFetchMiniProtocolNum,
                     chainSyncMiniProtocolNum, connectTo)
import           Ouroboros.Network.PeerSelection.PeerSharing.Codec
                     (decodeRemoteAddress, encodeRemoteAddress)
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch,
                     ChainRange (..))
import qualified Ouroboros.Network.Protocol.BlockFetch.Type as BlockFetch
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import           Ouroboros.Network.Protocol.Handshake.Version (Version (..))
import           Ouroboros.Network.Snocket (SocketSnocket)
import qualified Ouroboros.Network.Snocket as Snocket
import           System.FS.API (SomeHasFS (..))
import           System.FS.API.Types (MountPoint (..))
import           System.FS.IO (ioHasFS)

data Opts = Opts {
    configFile     :: FilePath
  , immutableDBDir :: FilePath
  , serverAddr     :: (Socket.HostName, Socket.ServiceName)
  , startSlot      :: WithOrigin SlotNo
  , numBlocks      :: Word64
  }

run :: Opts -> IO ()
run opts = evalContT $ do
    let immDBFS = SomeHasFS $ ioHasFS $ MountPoint immutableDBDir
        args = Cardano.CardanoBlockArgs configFile Nothing
    ProtocolInfo{pInfoConfig = cfg} <- lift $ mkProtocolInfo args
    registry <- ContT withRegistry
    internalImmDB <- ContT $ withImmutableDBInternal cfg registry immDBFS
    snocket <- Snocket.socketSnocket <$> ContT withIOManager
    lift $ do
      ptQueue        <- newTQueueIO
      varNumDequeued <- newTVarIO (0 :: Word64)
      blockFetchDone <- newEmptyTMVarIO

      startPoint <- case startSlot of
        Origin      -> pure GenesisPoint
        NotOrigin s -> ImmutableDB.getHashForSlot internalImmDB s >>= \case
          Just h  -> pure $ BlockPoint s h
          Nothing -> fail $ "Slot not in ImmutableDB: " <> show s

      let enqueue hdr = do
            atomically $ writeTQueue ptQueue (headerPoint hdr)

          dequeue = atomically $ do
            numDequeued <- readTVar varNumDequeued
            if numDequeued >= numBlocks
              then pure Nothing
              else do
                modifyTVar varNumDequeued (+ 1)
                Just <$> readTQueue ptQueue

          waitBlockFetchDone   = atomically $ takeTMVar blockFetchDone
          signalBlockFetchDone = atomically $ putTMVar blockFetchDone ()

      let hints = Socket.defaultHints { Socket.addrSocketType = Socket.Stream }
      -- The result is always non-empty
      serverInfo <- head <$> Socket.getAddrInfo (Just hints) (Just serverHostName) (Just serverPort)
      runApplication snocket (Socket.addrAddress serverInfo) $
        mkApplication
          (configCodec cfg)
          (getNetworkMagic (configBlock cfg))
          (simpleChainSync startPoint numBlocks enqueue waitBlockFetchDone)
          (simpleBlockFetch dequeue signalBlockFetchDone)
  where
    Opts {
        configFile
      , immutableDBDir
      , serverAddr = (serverHostName, serverPort)
      , startSlot
      , numBlocks
      } = opts

{-------------------------------------------------------------------------------
  ChainSync and BlockFetch logic
-------------------------------------------------------------------------------}

{-

TODOs if we want this as a proper tool:

 - Think more about tracing/logging (no MonadSay)
 - Proper exceptions instead of MonadFail
 - protocol pipelining?
 - handle rollbacks in a way other than ignoring?
 - BlockFetch batching?

-}

-- | Find an intersection for the given point, and download the given number of
-- headers after that.
simpleChainSync ::
     forall m blk.
     ( MonadFail m, MonadSay m
     , HasHeader blk
     )
  => Point blk
     -- ^ Point to start from.
  -> Word64
     -- ^ Number of headers to process.
  -> (Header blk -> m ())
     -- ^ Callback on a new header.
  -> m ()
     -- ^ Invoked when we are done, as ChainSync needs to be kept open.
  -> PeerPipelined (ChainSync' blk) AsClient ChainSync.StIdle m ()
simpleChainSync startPt numHeaders onHdr waitEnd =
      PeerPipelined
    $ SenderYield (ClientAgency ChainSync.TokIdle) (ChainSync.MsgFindIntersect [startPt])
    $ SenderAwait (ServerAgency ChainSync.TokIntersect) $ \case
        ChainSync.MsgIntersectNotFound {} ->
          SenderEffect $ fail $ "Server doesn't know " <> show startPt
        ChainSync.MsgIntersectFound {}    ->
          go numHeaders
  where
    go ::
         forall c.
         Word64
      -> PeerSender (ChainSync' blk) AsClient ChainSync.StIdle Z c m ()
    go = \case
      0 -> SenderEffect $ do
        waitEnd
        pure
          $ SenderYield (ClientAgency ChainSync.TokIdle) ChainSync.MsgDone
          $ SenderDone ChainSync.TokDone ()
      n ->
          SenderYield (ClientAgency ChainSync.TokIdle) ChainSync.MsgRequestNext
        $ SenderAwait (ServerAgency (ChainSync.TokNext ChainSync.TokCanAwait)) $ \case
            ChainSync.MsgRollForward hdr _tip -> onRollForward hdr
            ChainSync.MsgRollBackward to _tip -> onRollBackward to
            ChainSync.MsgAwaitReply ->
              SenderAwait (ServerAgency (ChainSync.TokNext ChainSync.TokMustReply)) $ \case
                ChainSync.MsgRollForward hdr _tip -> onRollForward hdr
                ChainSync.MsgRollBackward to _tip -> onRollBackward to
        where
          onRollForward ::
               Header blk
            -> PeerSender (ChainSync' blk) AsClient ChainSync.StIdle Z c m ()
          onRollForward hdr = SenderEffect $ do
              onHdr hdr
              pure $ go (n - 1)

          onRollBackward ::
               Point blk
            -> PeerSender (ChainSync' blk) AsClient ChainSync.StIdle Z c m ()
          onRollBackward to = SenderEffect $ do
              say $ "Ignoring rollback to " <> show to
              pure $ go n

simpleBlockFetch ::
     forall m blk.
     ( MonadFail m, MonadSay m
     , HasHeader blk
     )
  => m (Maybe (Point blk))
     -- ^ Get the next point to download, or 'Nothing' to terminate.
  -> m ()
     -- ^ Invoked when we are done.
  -> PeerPipelined (BlockFetch' blk) AsClient BlockFetch.BFIdle m ()
simpleBlockFetch getNextPt signalDone =
    PeerPipelined go
  where
    go :: forall c. PeerSender (BlockFetch' blk) AsClient BlockFetch.BFIdle Z c m ()
    go = SenderEffect $ getNextPt <&> \case
      Nothing ->
          SenderYield (ClientAgency BlockFetch.TokIdle) BlockFetch.MsgClientDone
        $ SenderEffect $ do
            signalDone
            pure $ SenderDone BlockFetch.TokDone ()
      Just pt ->
          SenderYield (ClientAgency BlockFetch.TokIdle)
            (BlockFetch.MsgRequestRange (ChainRange pt pt))
        $ SenderAwait (ServerAgency BlockFetch.TokBusy) $ \case
            BlockFetch.MsgNoBlocks -> SenderEffect $
              fail $ "Server doesn't have block corresponding to header: " <> show pt
            BlockFetch.MsgStartBatch ->
              SenderAwait (ServerAgency BlockFetch.TokStreaming) $ \case
                BlockFetch.MsgBatchDone -> SenderEffect $
                  fail "Server sent an empty batch"
                BlockFetch.MsgBlock blk -> SenderEffect $ do
                  say $ "Received block " <> show (blockPoint blk)
                  pure $ SenderAwait (ServerAgency BlockFetch.TokStreaming) $ \case
                    BlockFetch.MsgBlock {} -> SenderEffect $
                      fail "Server sent too many blocks in a batch"
                    BlockFetch.MsgBatchDone -> go

{-------------------------------------------------------------------------------
  Invoke Networking layer
-------------------------------------------------------------------------------}

runApplication ::
     SocketSnocket
  -> Socket.SockAddr
  -> Versions
       NodeToNodeVersion
       NodeToNodeVersionData
       (OuroborosApplicationWithMinimalCtx InitiatorMode Socket.SockAddr BL.ByteString IO a b)
  -> IO ()
runApplication sn sockAddr application =
    connectTo
      sn
      adhocTracers
      application
      Nothing
      sockAddr
  where
    adhocTracers = NetworkConnectTracers {
        nctMuxTracer       = nullTracer
      , nctHandshakeTracer = show >$< stdoutTracer
      }

mkApplication ::
     forall m blk addr a.
     ( IOLike m
     , SupportedNetworkProtocolVersion blk
     , SerialiseNodeToNodeConstraints blk
     , ShowProxy blk
     , ShowProxy (Header blk)
     )
  => CodecConfig blk
  -> NetworkMagic
  -> PeerPipelined (ChainSync' blk) AsClient ChainSync.StIdle m a
  -> PeerPipelined (BlockFetch' blk) AsClient BlockFetch.BFIdle m a
  -> Versions
       NodeToNodeVersion
       NodeToNodeVersionData
       (OuroborosApplicationWithMinimalCtx InitiatorMode addr BL.ByteString m a Void)
mkApplication codecCfg networkMagic chainSync blockFetch =
    forAllVersions application
  where
    forAllVersions ::
         (NodeToNodeVersion -> BlockNodeToNodeVersion blk -> r)
      -> Versions NodeToNodeVersion NodeToNodeVersionData r
    forAllVersions mkR =
          Versions
        $ Map.mapWithKey mkVersion
        $ supportedNodeToNodeVersions (Proxy @blk)
      where
        mkVersion version blockVersion = Version {
            versionApplication = \_ -> mkR version blockVersion
          , versionData =
              stdVersionDataNTN
                networkMagic
                InitiatorOnlyDiffusionMode
                PeerSharingDisabled
          }

    application ::
         NodeToNodeVersion
      -> BlockNodeToNodeVersion blk
      -> OuroborosApplicationWithMinimalCtx InitiatorMode addr BL.ByteString m a Void
    application version blockVersion = OuroborosApplication
        [ mkMiniProtocol chainSyncMiniProtocolNum $
            mkMiniProtocolCbFromPeerPipelined $ \_ ->
              (nullTracer, cChainSyncCodec, chainSync)
        , mkMiniProtocol blockFetchMiniProtocolNum $
            mkMiniProtocolCbFromPeerPipelined $ \_ ->
              (nullTracer, cBlockFetchCodec, blockFetch)
        ]
      where
        Codecs {cChainSyncCodec, cBlockFetchCodec} =
          defaultCodecs
            codecCfg
            blockVersion
            encodeRemoteAddress
            decodeRemoteAddress
            version

        mkMiniProtocol miniProtocolNum muxPeer = MiniProtocol {
            miniProtocolNum
          , miniProtocolRun    = InitiatorProtocolOnly muxPeer
          , miniProtocolLimits = MiniProtocolLimits {
              maximumIngressQueue = 1_000_000 -- in bytes
            }
          }

{-------------------------------------------------------------------------------
  Util
-------------------------------------------------------------------------------}

type ChainSync' blk = ChainSync (Header blk) (Point blk) (Tip blk)

type BlockFetch' blk = BlockFetch blk (Point blk)

withImmutableDBInternal ::
     forall m blk a.
     ( IOLike m
     , ConvertRawHash blk
     , LedgerSupportsProtocol blk
     , ImmutableDB.ImmutableDbSerialiseConstraints blk
     , NodeInitStorage blk
     )
  => TopLevelConfig blk
  -> ResourceRegistry m
  -> SomeHasFS m
  -> (ImmutableDB.Internal m blk -> m a)
  -> m a
withImmutableDBInternal cfg registry dbFS f =
    bracket
      (ImmutableDB.openDBInternal immDBArgs runWithTempRegistry)
      (ImmutableDB.closeDB . fst)
      (\(_immDB, internal) -> f internal)
  where
    codecCfg   = configCodec   cfg
    storageCfg = configStorage cfg

    immDBArgs :: Complete ImmutableDbArgs m blk
    immDBArgs = ImmutableDB.defaultArgs {
          immCheckIntegrity = nodeCheckIntegrity storageCfg
        , immChunkInfo      = nodeImmutableDbChunkInfo storageCfg
        , immCodecConfig    = codecCfg
        , immRegistry       = registry
        , immHasFS          = dbFS
        }
