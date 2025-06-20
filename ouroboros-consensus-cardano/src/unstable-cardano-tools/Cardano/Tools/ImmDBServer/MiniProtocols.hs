{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Implement ChainSync and BlockFetch servers on top of just the immutable DB.
module Cardano.Tools.ImmDBServer.MiniProtocols (immDBServer) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import Control.Monad (forever)
import Control.ResourceRegistry
import Control.Tracer
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generic)
import qualified Network.Mux as Mux
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
  ( blockFetchServer'
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
import Ouroboros.Network.NodeToNode
  ( NodeToNodeVersionData (..)
  , Versions (..)
  )
import qualified Ouroboros.Network.NodeToNode as N2N
import Ouroboros.Network.PeerSelection.PeerSharing (PeerSharing (..))
import Ouroboros.Network.Protocol.BlockFetch.Server
import Ouroboros.Network.Protocol.ChainSync.Server
import Ouroboros.Network.Protocol.Handshake.Version (Version (..))
import Ouroboros.Network.Protocol.KeepAlive.Server
  ( keepAliveServerPeer
  )

immDBServer ::
  forall m blk addr.
  ( IOLike m
  , HasHeader blk
  , ShowProxy blk
  , SerialiseNodeToNodeConstraints blk
  , SupportedNetworkProtocolVersion blk
  ) =>
  CodecConfig blk ->
  (NodeToNodeVersion -> addr -> CBOR.Encoding) ->
  (NodeToNodeVersion -> forall s. CBOR.Decoder s addr) ->
  ImmutableDB m blk ->
  NetworkMagic ->
  Versions
    NodeToNodeVersion
    NodeToNodeVersionData
    (OuroborosApplicationWithMinimalCtx 'Mux.ResponderMode addr BL.ByteString m Void ())
immDBServer codecCfg encAddr decAddr immDB networkMagic = do
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
      ]
     where
      Consensus.N2N.Codecs
        { cKeepAliveCodec
        , cChainSyncCodecSerialised
        , cBlockFetchCodecSerialised
        } =
          Consensus.N2N.defaultCodecs codecCfg blockVersion encAddr decAddr version

      keepAliveProt =
        MiniProtocolCb $ \_ctx channel ->
          runPeer nullTracer cKeepAliveCodec channel $
            keepAliveServerPeer keepAliveServer
      chainSyncProt =
        MiniProtocolCb $ \_ctx channel ->
          withRegistry $
            runPeer nullTracer cChainSyncCodecSerialised channel
              . chainSyncServerPeer
              . chainSyncServer immDB ChainDB.getSerialisedHeaderWithPoint
      blockFetchProt =
        MiniProtocolCb $ \_ctx channel ->
          withRegistry $
            runPeer nullTracer cBlockFetchCodecSerialised channel
              . blockFetchServerPeer
              . blockFetchServer immDB ChainDB.getSerialisedBlockWithPoint
      txSubmissionProt =
        -- never reply, there is no timeout
        MiniProtocolCb $ \_ctx _channel -> forever $ threadDelay 10

    mkMiniProtocol miniProtocolStart miniProtocolNum limits proto =
      MiniProtocol
        { miniProtocolNum
        , miniProtocolLimits = limits N2N.defaultMiniProtocolParameters
        , miniProtocolRun = ResponderProtocolOnly proto
        , miniProtocolStart
        }

-- | The ChainSync specification requires sending a rollback instruction to the
-- intersection point right after an intersection has been negotiated. (Opening
-- a connection implicitly negotiates the Genesis point as the intersection.)
data ChainSyncIntersection blk
  = JustNegotiatedIntersection !(Point blk)
  | AlreadySentRollbackToIntersection
  deriving stock Generic
  deriving anyclass NoThunks

chainSyncServer ::
  forall m blk a.
  (IOLike m, HasHeader blk) =>
  ImmutableDB m blk ->
  BlockComponent blk (ChainDB.WithPoint blk a) ->
  ResourceRegistry m ->
  ChainSyncServer a (Point blk) (Tip blk) m ()
chainSyncServer immDB blockComponent registry = ChainSyncServer $ do
  follower <- newImmutableDBFollower
  runChainSyncServer $
    chainSyncServerForFollower nullTracer getImmutableTip follower
 where
  newImmutableDBFollower :: m (Follower m blk (ChainDB.WithPoint blk a))
  newImmutableDBFollower = do
    varIterator <-
      newTVarIO =<< ImmutableDB.streamAll immDB registry blockComponent
    varIntersection <-
      newTVarIO $ JustNegotiatedIntersection GenesisPoint

    let followerInstructionBlocking =
          readTVarIO varIntersection >>= \case
            JustNegotiatedIntersection intersectionPt -> do
              atomically $
                writeTVar varIntersection AlreadySentRollbackToIntersection
              pure $ RollBack intersectionPt
            -- Otherwise, get the next block from the iterator (or fail).
            AlreadySentRollbackToIntersection -> do
              iterator <- readTVarIO varIterator
              ImmutableDB.iteratorNext iterator >>= \case
                ImmutableDB.IteratorExhausted -> do
                  ImmutableDB.iteratorClose iterator
                  throwIO ReachedImmutableTip
                ImmutableDB.IteratorResult a ->
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
        { followerInstruction = Just <$> followerInstructionBlocking
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
  ImmutableDB m blk ->
  BlockComponent blk (ChainDB.WithPoint blk a) ->
  ResourceRegistry m ->
  BlockFetchServer a (Point blk) m ()
blockFetchServer immDB blockComponent registry =
  blockFetchServer' nullTracer stream
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
