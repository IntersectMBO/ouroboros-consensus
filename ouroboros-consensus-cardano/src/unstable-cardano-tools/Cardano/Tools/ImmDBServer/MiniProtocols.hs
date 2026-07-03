{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Implement ChainSync and BlockFetch servers on top of just the immutable DB,
-- plus schedule-driven LeiosNotify and LeiosFetch servers.
module Cardano.Tools.ImmDBServer.MiniProtocols
  ( LeiosNotifyContext (..)
  , immDBServer
  ) where

import Cardano.Network.NodeToNode
  ( NodeToNodeVersionData (..)
  , Versions (..)
  )
import qualified Cardano.Network.NodeToNode as N2N
import Cardano.Slotting.Slot (WithOrigin (At))
import qualified Cardano.Tools.ImmDBServer.Json as Json
import qualified Cardano.Tools.ImmDBServer.Json.SendRecv as Json
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Control.Concurrent.Class.MonadMVar as MVar
import Control.Monad (forever)
import Control.ResourceRegistry
import Control.Tracer
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Data.Void (Void)
import Data.Word (Word32)
import GHC.Generics (Generic)
import qualified LeiosDemoLogic as LeiosLogic
import LeiosDemoOnlyTestFetch as LF
import LeiosDemoOnlyTestNotify
import LeiosDemoTypes (messageLeiosFetchToObject)
import qualified LeiosDemoTypes as Leios
import qualified Network.Mux as Mux
import Network.TypedProtocol.Codec (AnyMessage (AnyMessage))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
  ( TraceBlockFetchServerEvent (..)
  , blockFetchServer'
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( chainSyncServerForFollower
  )
import Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import qualified Ouroboros.Consensus.Network.NodeToNode as Consensus.N2N
import Ouroboros.Consensus.Node (stdVersionDataNTN)
import Ouroboros.Consensus.Node.NetworkProtocolVersion
import Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints)
import Ouroboros.Consensus.Storage.ChainDB.API (Follower (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.Common
import Ouroboros.Consensus.Storage.ImmutableDB.API (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.API as ImmutableDB
import Ouroboros.Consensus.Util
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Block (ChainUpdate (..), Tip (..))
import Ouroboros.Network.Driver (runPeer)
import Ouroboros.Network.KeepAlive (keepAliveServer)
import Ouroboros.Network.Magic (NetworkMagic)
import Ouroboros.Network.Mux
  ( MiniProtocol (..)
  , MiniProtocolCb (..)
  , OuroborosApplication (..)
  , OuroborosApplicationWithMinimalCtx
  , RunMiniProtocol (..)
  )
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.Protocol.BlockFetch.Server
import qualified Ouroboros.Network.Protocol.BlockFetch.Type as BF
import Ouroboros.Network.Protocol.ChainSync.Server
import qualified Ouroboros.Network.Protocol.ChainSync.Type as CS
import Ouroboros.Network.Protocol.Handshake.Version (Version (..))
import Ouroboros.Network.Protocol.KeepAlive.Server
  ( keepAliveServerPeer
  )

leiosResponderProtocolLimits :: Mux.MiniProtocolLimits
leiosResponderProtocolLimits =
  Mux.MiniProtocolLimits
    { Mux.maximumIngressQueue = 5 * 1024 * 1024
    }

immDBServer ::
  forall m blk addr.
  ( IOLike m
  , HasHeader blk
  , ShowProxy blk
  , SerialiseNodeToNodeConstraints blk
  , Show addr
  , SupportedNetworkProtocolVersion blk
  ) =>
  CodecConfig blk ->
  (NodeToNodeVersion -> addr -> CBOR.Encoding) ->
  (NodeToNodeVersion -> forall s. CBOR.Decoder s addr) ->
  ImmutableDB m blk ->
  NetworkMagic ->
  (SlotNo -> m DiffTime) ->
  (ResourceRegistry m -> m (LeiosNotifyContext m)) ->
  m (LeiosLogic.SomeLeiosFetchContext m) ->
  Tracer m Json.LogEvent ->
  Versions
    NodeToNodeVersion
    NodeToNodeVersionData
    (OuroborosApplicationWithMinimalCtx 'Mux.ResponderMode addr BL.ByteString m Void ())
immDBServer codecCfg encAddr decAddr immDB networkMagic getSlotDelay mkLeiosNotifyContext mkLeiosFetchContext tracer = do
  forAllVersions application
 where
  forAllVersions ::
    (NodeToNodeVersion -> BlockNodeToNodeVersion blk -> r) ->
    Versions NodeToNodeVersion NodeToNodeVersionData r
  forAllVersions mkR =
    Versions $
      Map.mapWithKey mkVersion $
        supportedNodeToNodeVersions (Proxy @blk)
   where
    mkVersion version blockVersion =
      Version
        { versionApplication = const $ mkR version blockVersion
        , versionData =
            stdVersionDataNTN
              networkMagic
              N2N.InitiatorOnlyDiffusionMode
              PeerSharingDisabled
        }

  application ::
    NodeToNodeVersion ->
    BlockNodeToNodeVersion blk ->
    OuroborosApplicationWithMinimalCtx 'Mux.ResponderMode addr BL.ByteString m Void ()
  application version blockVersion =
    OuroborosApplication miniprotocols
   where
    miniprotocols =
      [ mkMiniProtocol
          Mux.StartOnDemandAny
          N2N.keepAliveMiniProtocolNum
          N2N.keepAliveProtocolLimits
          keepAliveProt
      , mkMiniProtocol
          Mux.StartOnDemand
          N2N.chainSyncMiniProtocolNum
          N2N.chainSyncProtocolLimits
          chainSyncProt
      , mkMiniProtocol
          Mux.StartOnDemand
          N2N.blockFetchMiniProtocolNum
          N2N.blockFetchProtocolLimits
          blockFetchProt
      , mkMiniProtocol
          Mux.StartOnDemand
          N2N.txSubmissionMiniProtocolNum
          N2N.txSubmissionProtocolLimits
          txSubmissionProt
      , mkMiniProtocol
          Mux.StartOnDemand
          leiosNotifyMiniProtocolNum
          (const leiosResponderProtocolLimits)
          leiosNotifyProt
      , mkMiniProtocol
          Mux.StartOnDemand
          leiosFetchMiniProtocolNum
          (const leiosResponderProtocolLimits)
          leiosFetchProt
      ]
     where
      Consensus.N2N.Codecs
        { cKeepAliveCodec
        , cChainSyncCodecSerialised
        , cBlockFetchCodecSerialised
        , cLeiosNotifyCodec
        , cLeiosFetchCodec
        } =
          Consensus.N2N.defaultCodecs codecCfg blockVersion encAddr decAddr version

      keepAliveProt =
        MiniProtocolCb $ \_ctx channel ->
          runPeer nullTracer cKeepAliveCodec channel $
            keepAliveServerPeer keepAliveServer
      chainSyncProt =
        MiniProtocolCb $ \ctx channel ->
          withRegistry $
            runPeer (traceMaybe (maybeShowSendRecvCS ctx) tracer) cChainSyncCodecSerialised channel
              . chainSyncServerPeer
              . chainSyncServer immDB ChainDB.getSerialisedHeaderWithPoint getSlotDelay
      blockFetchProt =
        MiniProtocolCb $ \ctx channel ->
          withRegistry $
            runPeer (traceMaybe (maybeShowSendRecvBF ctx) tracer) cBlockFetchCodecSerialised channel
              . blockFetchServerPeer
              . blockFetchServer (traceMaybe (mapBFEvent ctx) tracer) immDB ChainDB.getSerialisedBlockWithPoint
      txSubmissionProt =
        -- never reply, there is no timeout
        MiniProtocolCb $ \_ctx _channel -> forever $ threadDelay 10
      leiosNotifyProt =
        MiniProtocolCb $ \_ctx channel ->
          withRegistry $ \reg ->
            mkLeiosNotifyContext reg >>= \leiosContext ->
              runPeer nullTracer cLeiosNotifyCodec channel $
                leiosNotifyServerPeer
                  ( MVar.takeMVar (leiosMailbox leiosContext) <&> \case
                      (p, Just sz) -> MsgLeiosBlockOffer p sz
                      (p, Nothing) -> MsgLeiosBlockTxsOffer p
                  )
      leiosFetchProt =
        MiniProtocolCb $ \ctx channel ->
          mkLeiosFetchContext >>= \(LeiosLogic.MkSomeLeiosFetchContext leiosContext) ->
            runPeer (traceMaybe (maybeShowSendRecvLF ctx) tracer) cLeiosFetchCodec channel $
              leiosFetchServerPeer $
                pure (LeiosLogic.leiosFetchHandler nullTracer leiosContext)

    mkMiniProtocol miniProtocolStart miniProtocolNum limits proto =
      MiniProtocol
        { miniProtocolNum
        , miniProtocolLimits = limits N2N.defaultMiniProtocolParameters
        , miniProtocolRun = ResponderProtocolOnly proto
        , miniProtocolStart
        }

responderContextToConnectionIdString :: Show addr => N2N.ResponderContext addr -> String
responderContextToConnectionIdString ctx =
  show (N2N.localAddress connId) ++ " " ++ show (N2N.remoteAddress connId)
 where
  connId = N2N.rcConnectionId ctx

maybeShowSendRecvLF ::
  Show addr =>
  N2N.ResponderContext addr ->
  N2N.TraceSendRecv (LeiosFetch Leios.LeiosPoint Leios.LeiosEb Leios.LeiosTx) ->
  Maybe Json.LogEvent
maybeShowSendRecvLF ctx = \case
  N2N.TraceRecvMsg (AnyMessage msg) ->
    Just $ mkEvent Json.Recv (Aeson.Object $ messageLeiosFetchToObject msg)
  N2N.TraceSendMsg (AnyMessage msg) ->
    Just $ mkEvent Json.Send (Aeson.Object $ messageLeiosFetchToObject msg)
 where
  mkEvent dir msg =
    Json.SendRecvEvent $
      Json.MkSendRecvEvent
        { Json.at = Json.TBD
        , Json.prevCount = Json.TBD
        , Json.connectionId = responderContextToConnectionIdString ctx
        , Json.direction = dir
        , Json.mux_at = Nothing
        , Json.msg = msg
        }

maybeShowSendRecvCS ::
  Show addr =>
  N2N.ResponderContext addr ->
  N2N.TraceSendRecv (CS.ChainSync h p tip) ->
  Maybe Json.LogEvent
maybeShowSendRecvCS ctx = \case
  N2N.TraceSendMsg (AnyMessage CS.MsgRollForward{}) -> Just $ send "MsgRollForward"
  _ -> Nothing
 where
  send y =
    Json.SendRecvEvent $
      Json.MkSendRecvEvent
        { Json.at = Json.TBD
        , Json.prevCount = Json.TBD
        , Json.connectionId = responderContextToConnectionIdString ctx
        , Json.direction = Json.Send
        , Json.mux_at = Nothing
        , Json.msg = y
        }

maybeShowSendRecvBF ::
  Show addr =>
  N2N.ResponderContext addr ->
  N2N.TraceSendRecv (BF.BlockFetch blk p) ->
  Maybe Json.LogEvent
maybeShowSendRecvBF ctx = \case
  N2N.TraceRecvMsg (AnyMessage BF.MsgRequestRange{}) -> Just $ recv "MsgRequestRange"
  N2N.TraceSendMsg (AnyMessage BF.MsgBlock{}) -> Nothing -- replaced by 'mapBFEvent' below
  _ -> Nothing
 where
  recv y =
    Json.SendRecvEvent $
      Json.MkSendRecvEvent
        { Json.at = Json.TBD
        , Json.prevCount = Json.TBD
        , Json.connectionId = responderContextToConnectionIdString ctx
        , Json.direction = Json.Recv
        , Json.mux_at = Nothing
        , Json.msg = y
        }

-- The BlockFetch send tracer only sees @Serialised blk@; hook
-- 'TraceBlockFetchServerSendBlock' so the JSON event includes the block hash.
mapBFEvent ::
  forall blk addr.
  SerialiseNodeToNodeConstraints blk =>
  Show addr =>
  N2N.ResponderContext addr ->
  TraceBlockFetchServerEvent blk ->
  Maybe Json.LogEvent
mapBFEvent ctx = \case
  TraceBlockFetchServerSendBlock p ->
    Just . Json.SendRecvEvent $
      Json.MkSendRecvEvent
        { Json.at = Json.TBD
        , Json.prevCount = Json.TBD
        , Json.connectionId = responderContextToConnectionIdString ctx
        , Json.direction = Json.Send
        , Json.mux_at = Nothing
        , Json.msg =
            Aeson.object
              [ "kind" .= Aeson.String "MsgBlock"
              , "blockHash" .= case pointHash p of
                  GenesisHash -> "origin"
                  BlockHash h -> BS8.unpack . BS16.encode $ toRawHash (Proxy @blk) h
              ]
        }

data ChainSyncIntersection blk
  = JustNegotiatedIntersection !(Point blk)
  | AlreadySentRollbackToIntersection
  deriving stock Generic
  deriving anyclass NoThunks

data DelayingBuffer a = Empty | Exhausted | Delayed a
  deriving stock Generic
  deriving anyclass NoThunks

chainSyncServer ::
  forall m blk a.
  (IOLike m, HasHeader blk) =>
  ImmutableDB m blk ->
  BlockComponent blk (ChainDB.WithPoint blk a) ->
  (SlotNo -> m DiffTime) ->
  ResourceRegistry m ->
  ChainSyncServer a (Point blk) (Tip blk) m ()
chainSyncServer immDB blockComponent getSlotDelay registry = ChainSyncServer $ do
  follower <- newImmutableDBFollower
  runChainSyncServer $
    chainSyncServerForFollower nullTracer getImmutableTip follower
 where
  newImmutableDBFollower :: m (Follower m blk (ChainDB.WithPoint blk a))
  newImmutableDBFollower = do
    varForBlocking <- uncheckedNewTVarM Empty
    varIterator <-
      newTVarIO =<< ImmutableDB.streamAll immDB registry blockComponent
    varIntersection <-
      newTVarIO $ JustNegotiatedIntersection GenesisPoint

    let followerInstruction =
          readTVarIO varIntersection >>= \case
            JustNegotiatedIntersection intersectionPt -> do
              atomically $ do
                writeTVar varIntersection AlreadySentRollbackToIntersection
              pure $ Just $ RollBack intersectionPt
            AlreadySentRollbackToIntersection -> do
              iterator <- readTVarIO varIterator
              ImmutableDB.iteratorNext iterator >>= \case
                ImmutableDB.IteratorExhausted -> do
                  ImmutableDB.iteratorClose iterator
                  atomically $ writeTVar varForBlocking Exhausted
                  pure Nothing
                ImmutableDB.IteratorResult a -> do
                  delay <- case pointSlot $ ChainDB.point a of
                    Origin -> pure 0
                    At slot -> getSlotDelay slot
                  if delay <= 0
                    then pure $ Just $ AddBlock a
                    else do
                      atomically $ writeTVar varForBlocking $ Delayed a
                      pure Nothing

    let followerInstructionBlocking =
          readTVarIO varForBlocking >>= \case
            Empty -> error "impossible!"
            Exhausted -> do
              threadDelay (100 :: DiffTime)
              throwIO ReachedImmutableTip
            Delayed a -> do
              case pointSlot $ ChainDB.point a of
                Origin -> pure ()
                At slot -> getSlotDelay slot >>= threadDelay
              pure $ AddBlock a

        followerClose = ImmutableDB.iteratorClose =<< readTVarIO varIterator

        followerForward [] = pure Nothing
        followerForward (pt : pts) =
          ImmutableDB.streamAfterPoint immDB registry blockComponent pt >>= \case
            Left _ -> followerForward pts
            Right iterator -> do
              followerClose
              atomically $ do
                writeTVar varIterator iterator
                writeTVar varIntersection $ JustNegotiatedIntersection pt
              pure $ Just pt

    pure
      Follower
        { followerInstruction
        , followerInstructionBlocking
        , followerForward
        , followerClose
        }

  getImmutableTip :: STM m (Tip blk)
  getImmutableTip =
    ImmutableDB.getTip immDB <&> \case
      Origin -> TipGenesis
      NotOrigin tip -> Tip tipSlotNo tipHash tipBlockNo
       where
        ImmutableDB.Tip tipSlotNo _ tipBlockNo tipHash = tip

blockFetchServer ::
  forall m blk a.
  (IOLike m, StandardHash blk, Typeable blk) =>
  Tracer m (TraceBlockFetchServerEvent blk) ->
  ImmutableDB m blk ->
  BlockComponent blk (ChainDB.WithPoint blk a) ->
  ResourceRegistry m ->
  BlockFetchServer a (Point blk) m ()
blockFetchServer tracer immDB blockComponent registry =
  blockFetchServer' tracer stream
 where
  stream from to =
    bimap convertError convertIterator
      <$> ImmutableDB.stream immDB registry blockComponent from to

  convertError = ChainDB.MissingBlock . ImmutableDB.missingBlockPoint
  convertIterator iterator =
    ChainDB.Iterator
      { ChainDB.iteratorNext =
          ImmutableDB.iteratorNext iterator <&> \case
            ImmutableDB.IteratorResult b -> ChainDB.IteratorResult b
            ImmutableDB.IteratorExhausted -> ChainDB.IteratorExhausted
      , ChainDB.iteratorClose = ImmutableDB.iteratorClose iterator
      }

data ImmDBServerException
  = ReachedImmutableTip
  | TriedToFetchGenesis
  deriving stock Show
  deriving anyclass Exception

-----

data LeiosNotifyContext m = MkLeiosNotifyContext
  { leiosMailbox :: !(MVar.MVar m (Leios.LeiosPoint, Maybe Word32))
  }
