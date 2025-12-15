{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Implement ChainSync and BlockFetch servers on top of just the immutable DB.
module Cardano.Tools.ImmDBServer.MiniProtocols (
    LeiosNotifyContext (..)
  , immDBServer
  ) where

import           Cardano.Slotting.Slot (WithOrigin (At))
import qualified Cardano.Tools.ImmDBServer.Json as Json
import qualified Cardano.Tools.ImmDBServer.Json.SendRecv as Json
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Control.Concurrent.Class.MonadMVar as MVar
import           Control.Monad (forever)
import           Control.ResourceRegistry
import           Control.Tracer
import           Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy as BL
import           Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import           Data.Typeable (Typeable)
import           Data.Void (Void)
import           Data.Word (Word32)
import           GHC.Generics (Generic)
import qualified LeiosDemoLogic as LeiosLogic
import           LeiosDemoOnlyTestFetch as LF
import           LeiosDemoOnlyTestNotify
import qualified LeiosDemoTypes as Leios
import qualified Network.Mux as Mux
import           Network.TypedProtocol.Codec (AnyMessage (AnyMessage))
import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (blockFetchServer')
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
                     (chainSyncServerForFollower)
import           Ouroboros.Consensus.Network.NodeToNode (Codecs (..))
import qualified Ouroboros.Consensus.Network.NodeToNode as Consensus.N2N
import           Ouroboros.Consensus.Node (stdVersionDataNTN)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints)
import           Ouroboros.Consensus.Storage.ChainDB.API (Follower (..))
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import           Ouroboros.Consensus.Storage.Common
import           Ouroboros.Consensus.Storage.ImmutableDB.API (ImmutableDB)
import qualified Ouroboros.Consensus.Storage.ImmutableDB.API as ImmutableDB
import           Ouroboros.Consensus.Util
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Block (ChainUpdate (..), Tip (..))
import           Ouroboros.Network.Driver (runPeer)
import           Ouroboros.Network.KeepAlive (keepAliveServer)
import           Ouroboros.Network.Magic (NetworkMagic)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolCb (..),
                     OuroborosApplication (..),
                     OuroborosApplicationWithMinimalCtx, RunMiniProtocol (..))
import           Ouroboros.Network.NodeToNode (NodeToNodeVersionData (..),
                     Versions (..))
import qualified Ouroboros.Network.NodeToNode as N2N
import           Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import           Ouroboros.Network.Protocol.BlockFetch.Server
import qualified Ouroboros.Network.Protocol.BlockFetch.Type as BF
import           Ouroboros.Network.Protocol.ChainSync.Server
import qualified Ouroboros.Network.Protocol.ChainSync.Type as CS
import           Ouroboros.Network.Protocol.Handshake.Version (Version (..))
import           Ouroboros.Network.Protocol.KeepAlive.Server
                     (keepAliveServerPeer)

immDBServer ::
     forall m blk addr.
     ( IOLike m
     , HasHeader blk
     , ShowProxy blk
     , SerialiseNodeToNodeConstraints blk
     , Show addr
     , SupportedNetworkProtocolVersion blk
     )
  => CodecConfig blk
  -> (NodeToNodeVersion -> addr -> CBOR.Encoding)
  -> (NodeToNodeVersion -> forall s . CBOR.Decoder s addr)
  -> ImmutableDB m blk
  -> NetworkMagic
  -> (SlotNo -> m DiffTime)
  -> (ResourceRegistry m -> m (LeiosNotifyContext m))
  -> m (LeiosLogic.SomeLeiosFetchContext m)
  -> Tracer m Json.LogEvent
  -> Versions NodeToNodeVersion NodeToNodeVersionData
       (OuroborosApplicationWithMinimalCtx 'Mux.ResponderMode addr BL.ByteString m Void ())
immDBServer codecCfg encAddr decAddr immDB networkMagic getSlotDelay mkLeiosNotifyContext mkLeiosFetchContext tracer = do
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
            versionApplication = const $ mkR version blockVersion
          , versionData        =
              stdVersionDataNTN
                networkMagic
                N2N.InitiatorOnlyDiffusionMode
                PeerSharingDisabled
          }

    application ::
         NodeToNodeVersion
      -> BlockNodeToNodeVersion blk
      -> OuroborosApplicationWithMinimalCtx 'Mux.ResponderMode addr BL.ByteString m Void ()
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
                (const Consensus.N2N.leiosNotifyProtocolLimits)
                leiosNotifyProt
            , mkMiniProtocol
                Mux.StartOnDemand
                leiosFetchMiniProtocolNum
                (const Consensus.N2N.leiosFetchProtocolLimits)
                leiosFetchProt
            ]
          where
            Consensus.N2N.Codecs {
                cKeepAliveCodec
              , cChainSyncCodecSerialised
              , cBlockFetchCodecSerialised
              , cLeiosNotifyCodec
              , cLeiosFetchCodec
              } =
              Consensus.N2N.defaultCodecs codecCfg blockVersion encAddr decAddr version

            keepAliveProt  =
              MiniProtocolCb $ \_ctx channel ->
                runPeer nullTracer cKeepAliveCodec channel
                      $ keepAliveServerPeer keepAliveServer
            chainSyncProt  =
                MiniProtocolCb $ \ctx channel ->
                withRegistry
              $ runPeer (traceMaybe (maybeShowSendRecvCS ctx) tracer) cChainSyncCodecSerialised channel
              . chainSyncServerPeer
              . chainSyncServer immDB ChainDB.getSerialisedHeaderWithPoint getSlotDelay
            blockFetchProt =
                MiniProtocolCb $ \ctx channel ->
                withRegistry
              $ runPeer (traceMaybe (maybeShowSendRecvBF ctx) tracer) cBlockFetchCodecSerialised channel
              . blockFetchServerPeer
              . blockFetchServer immDB ChainDB.getSerialisedBlockWithPoint
            txSubmissionProt =
                -- never reply, there is no timeout
                MiniProtocolCb $ \_ctx _channel -> forever $ threadDelay 10
            leiosNotifyProt =
                MiniProtocolCb $ \_ctx channel -> id
              $ withRegistry $ \reg -> id
              $ mkLeiosNotifyContext reg >>= \leiosContext -> id
              $ runPeer nullTracer cLeiosNotifyCodec channel
              $ leiosNotifyServerPeer
                    (MVar.takeMVar (leiosMailbox leiosContext) <&> \case
                        (p, Just sz) -> MsgLeiosBlockOffer p sz
                        (p, Nothing) -> MsgLeiosBlockTxsOffer p
                    )
            leiosFetchProt =
                MiniProtocolCb $ \ctx channel -> id
              $ mkLeiosFetchContext >>= \(LeiosLogic.MkSomeLeiosFetchContext leiosContext) -> id
              $ runPeer (traceMaybe (maybeShowSendRecvLF ctx) tracer) cLeiosFetchCodec channel
              $ leiosFetchServerPeer
              $ pure (LeiosLogic.leiosFetchHandler nullTracer leiosContext)

        mkMiniProtocol miniProtocolStart miniProtocolNum limits proto = MiniProtocol {
            miniProtocolNum
          , miniProtocolLimits = limits N2N.defaultMiniProtocolParameters
          , miniProtocolRun    = ResponderProtocolOnly proto
          , miniProtocolStart
          }

responderContextToConnectionIdString :: Show addr => N2N.ResponderContext addr -> String
responderContextToConnectionIdString ctx =
    show (N2N.localAddress connId) ++ " " ++ show (N2N.remoteAddress connId)
  where
    connId = N2N.rcConnectionId ctx

traceMaybe :: Monad m => (a -> Maybe b) -> Tracer m b -> Tracer m a
traceMaybe f tr = Tracer $ \x -> case f x of
    Nothing -> pure ()
    Just y  -> traceWith tr y

maybeShowSendRecvLF :: Show addr => N2N.ResponderContext addr -> N2N.TraceSendRecv (LeiosFetch Leios.LeiosPoint Leios.LeiosEb Leios.LeiosTx) -> Maybe Json.LogEvent
maybeShowSendRecvLF ctx = \case
    N2N.TraceRecvMsg mbTm (AnyMessage MsgLeiosBlockRequest{}) -> Just $ recv mbTm "MsgLeiosBlockRequest"
    N2N.TraceSendMsg tm (AnyMessage MsgLeiosBlock{}) -> Just $ send tm "MsgLeiosBlock"
    N2N.TraceRecvMsg mbTm (AnyMessage MsgLeiosBlockTxsRequest{}) -> Just $ recv mbTm "MsgLeiosBlockTxsRequest"
    N2N.TraceSendMsg tm (AnyMessage MsgLeiosBlockTxs{}) -> Just $ send tm "MsgLeiosBlockTxs"
    N2N.TraceRecvMsg mbTm (AnyMessage LF.MsgDone{}) -> Just $ recv mbTm "MsgDone"
    _ -> Nothing
  where
    f tm x y = Json.SendRecvEvent $ Json.MkSendRecvEvent { Json.at = Json.TBD, Json.prevCount = Json.TBD, Json.connectionId = responderContextToConnectionIdString ctx, Json.direction = x, Json.mux_at = tm, Json.msg = y }
    send tm y = f tm Json.Send y
    recv mbTm y = case mbTm of
        Nothing -> error $ "impossible! " ++ y
        Just tm -> f tm Json.Recv y

maybeShowSendRecvCS :: Show addr => N2N.ResponderContext addr -> N2N.TraceSendRecv (CS.ChainSync h p tip) -> Maybe Json.LogEvent
maybeShowSendRecvCS ctx = \case
    N2N.TraceSendMsg tm (AnyMessage CS.MsgRollForward{}) -> Just $ send tm "MsgRollForward"
    _ -> Nothing
  where
    send tm y = Json.SendRecvEvent $ Json.MkSendRecvEvent { Json.at = Json.TBD, Json.prevCount = Json.TBD, Json.connectionId = responderContextToConnectionIdString ctx, Json.direction = Json.Send, Json.mux_at = tm, Json.msg = y }

maybeShowSendRecvBF :: Show addr => N2N.ResponderContext addr -> N2N.TraceSendRecv (BF.BlockFetch blk p) -> Maybe Json.LogEvent
maybeShowSendRecvBF ctx = \case
    N2N.TraceRecvMsg mbTm (AnyMessage BF.MsgRequestRange{}) -> Just $ recv mbTm "MsgRequestRange"
    N2N.TraceSendMsg tm (AnyMessage BF.MsgBlock{}) -> Just $ send tm "MsgBlock"
    _ -> Nothing
  where
    f tm x y = Json.SendRecvEvent $ Json.MkSendRecvEvent { Json.at = Json.TBD, Json.prevCount = Json.TBD, Json.connectionId = responderContextToConnectionIdString ctx, Json.direction = x, Json.mux_at = tm, Json.msg = y }
    send tm y = f tm Json.Send y
    recv mbTm y = case mbTm of
        Nothing -> error $ "impossible! " ++ y
        Just tm -> f tm Json.Recv y

-- | The ChainSync specification requires sending a rollback instruction to the
-- intersection point right after an intersection has been negotiated. (Opening
-- a connection implicitly negotiates the Genesis point as the intersection.)
data ChainSyncIntersection blk =
    JustNegotiatedIntersection !(Point blk)
  | AlreadySentRollbackToIntersection
  deriving stock (Generic)
  deriving anyclass (NoThunks)

-- | An auxiliary data type used so that immdb-server sends MsgAwaitReply only
-- when appropriate.
data DelayingBuffer a = Empty | Exhausted | Delayed a
  deriving stock (Generic)
  deriving anyclass (NoThunks)

chainSyncServer ::
     forall m blk a. (IOLike m, HasHeader blk)
  => ImmutableDB m blk
  -> BlockComponent blk (ChainDB.WithPoint blk a)
  -> (SlotNo -> m DiffTime)
  -> ResourceRegistry m
  -> ChainSyncServer a (Point blk) (Tip blk) m ()
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
                -- Otherwise, get the next block from the iterator (or fail).
                AlreadySentRollbackToIntersection -> do
                  iterator <- readTVarIO varIterator
                  ImmutableDB.iteratorNext iterator >>= \case
                    ImmutableDB.IteratorExhausted -> do
                      ImmutableDB.iteratorClose iterator
                      atomically $ writeTVar varForBlocking Exhausted
                      pure Nothing
                    ImmutableDB.IteratorResult a  -> do
                      delay <- case pointSlot $ ChainDB.point a of
                          Origin  -> pure 0
                          At slot -> getSlotDelay slot
                      if delay <= 0 then pure $ Just $ AddBlock a else do
                          atomically $ writeTVar varForBlocking $ Delayed a
                          pure Nothing

        let followerInstructionBlocking = do
                readTVarIO varForBlocking >>= \case
                    Empty -> error "impossible!"
                      -- The contract is for followerInstructionBlocking to
                      -- only be called if followerInstruction returned
                      -- Nothing. And it only does so immediately after writing
                      -- a non-Empty value to varForBlocking.
                    Exhausted -> do
                      -- This delay gives the downstream peer a chance to
                      -- finish some req/rsp exchanges, eg BlockFetch.
                      threadDelay (100 :: DiffTime)
                      throwIO ReachedImmutableTip
                    Delayed a -> do
                      -- Wait until the slot of the current block has been reache
                      case pointSlot $ ChainDB.point a of
                          Origin  -> pure ()
                          At slot -> getSlotDelay slot >>= threadDelay
                      pure $ AddBlock a

            followerClose = ImmutableDB.iteratorClose =<< readTVarIO varIterator

            followerForward []         = pure Nothing
            followerForward (pt : pts) =
              ImmutableDB.streamAfterPoint immDB registry blockComponent pt >>= \case
                Left _         -> followerForward pts
                Right iterator -> do
                  followerClose
                  atomically $ do
                    writeTVar varIterator iterator
                    writeTVar varIntersection $ JustNegotiatedIntersection pt
                  pure $ Just pt

        pure Follower {
            followerInstruction
          , followerInstructionBlocking
          , followerForward
          , followerClose
          }

    getImmutableTip :: STM m (Tip blk)
    getImmutableTip = ImmutableDB.getTip immDB <&> \case
        Origin        -> TipGenesis
        NotOrigin tip -> Tip tipSlotNo tipHash tipBlockNo
          where
            ImmutableDB.Tip tipSlotNo _ tipBlockNo tipHash = tip

blockFetchServer ::
     forall m blk a. (IOLike m, StandardHash blk, Typeable blk)
  => ImmutableDB m blk
  -> BlockComponent blk (ChainDB.WithPoint blk a)
  -> ResourceRegistry m
  -> BlockFetchServer a (Point blk) m ()
blockFetchServer immDB blockComponent registry =
    blockFetchServer' nullTracer stream
  where
    stream from to =
            bimap convertError convertIterator
        <$> ImmutableDB.stream immDB registry blockComponent from to

    convertError = ChainDB.MissingBlock . ImmutableDB.missingBlockPoint
    convertIterator iterator = ChainDB.Iterator {
          ChainDB.iteratorNext  = ImmutableDB.iteratorNext iterator <&> \case
            ImmutableDB.IteratorResult b  -> ChainDB.IteratorResult b
            ImmutableDB.IteratorExhausted -> ChainDB.IteratorExhausted
        , ChainDB.iteratorClose = ImmutableDB.iteratorClose iterator
        }

data ImmDBServerException =
    ReachedImmutableTip
  | TriedToFetchGenesis
  deriving stock (Show)
  deriving anyclass (Exception)

-----

data LeiosNotifyContext m = MkLeiosNotifyContext {
    leiosMailbox :: !(MVar.MVar m (Leios.LeiosPoint, Maybe Word32))
  }
