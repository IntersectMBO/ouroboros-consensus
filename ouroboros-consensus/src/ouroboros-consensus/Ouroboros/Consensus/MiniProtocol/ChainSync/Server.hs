{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( Tip
  , chainSyncBlockServerFollower
  , chainSyncBlocksServer
  , chainSyncHeaderServerFollower
  , chainSyncHeadersServer

    -- * Trace events
  , BlockingType (..)
  , TraceChainSyncServerEvent (..)

    -- * Low-level API
  , chainSyncServerForFollower
  ) where

import Cardano.Binary (DecoderError)
import qualified Codec.CBOR.Decoding as CBOR.Decoding
import qualified Codec.CBOR.Read as CBOR.Read
import qualified Codec.CBOR.Write as CBOR.Write
import Control.ResourceRegistry (ResourceRegistry)
import Control.Tracer
import qualified Data.ByteString.Lazy as Lazy
import Data.Functor ((<&>))
import LeiosDemoDb (LeiosDbConnection)
import LeiosDemoTypes (LeiosPoint)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Storage.ChainDB.API
  ( BlockComponent (GetHeader, GetRawBlock)
  , ChainDB
  , Follower (..)
  , WithPoint (..)
  , getPoint
  , getSerialisedHeaderWithPoint
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.LedgerDB (ResolveLeiosBlock (..))
import Ouroboros.Consensus.Storage.Serialisation
import Ouroboros.Consensus.Util.Enclose
  ( Enclosing
  , Enclosing' (..)
  , pattern FallingEdge
  )
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.Block
  ( ChainUpdate (..)
  , Serialised (..)
  , Tip (..)
  )
import Ouroboros.Network.Protocol.ChainSync.Server

chainSyncHeaderServerFollower ::
  ChainDB m blk ->
  ChainDB.ChainType ->
  ResourceRegistry m ->
  m (Follower m blk (WithPoint blk (SerialisedHeader blk)))
chainSyncHeaderServerFollower chainDB chainType registry =
  ChainDB.newFollower chainDB registry chainType getSerialisedHeaderWithPoint

chainSyncBlockServerFollower ::
  ChainDB m blk ->
  ResourceRegistry m ->
  m (Follower m blk (WithPoint blk (Header blk, Serialised blk)))
chainSyncBlockServerFollower chainDB registry =
  ChainDB.newFollower chainDB registry ChainDB.SelectedChain comp
 where
  comp :: BlockComponent blk (WithPoint blk (Header blk, Serialised blk))
  comp =
    (\h sb pt -> WithPoint (h, Serialised sb) pt)
      <$> GetHeader
      <*> GetRawBlock
      <*> getPoint

-- | Chain Sync Server for block headers for a given a 'ChainDB'.
--
-- The node-to-node protocol uses the chain sync mini-protocol with chain
-- headers (and fetches blocks separately with the block fetch mini-protocol).
chainSyncHeadersServer ::
  forall m blk.
  ( IOLike m
  , HasHeader (Header blk)
  ) =>
  Tracer m (TraceChainSyncServerEvent blk) ->
  ChainDB m blk ->
  Follower m blk (WithPoint blk (SerialisedHeader blk)) ->
  ChainSyncServer (SerialisedHeader blk) (Point blk) (Tip blk) m ()
chainSyncHeadersServer tracer chainDB flr =
  chainSyncServerForFollower tracer (ChainDB.getCurrentTip chainDB) flr

-- | Chain Sync Server for blocks for a given a 'ChainDB'.
--
-- The local node-to-client protocol uses the chain sync mini-protocol with
-- chains of full blocks (rather than a header \/ body split).
--
-- Dijkstra-era certifier blocks (whose body carries a 'LeiosCert' and an
-- empty tx list on the wire) are resolved before being sent: the
-- transactions of the certified EB are spliced in using the previously
-- announced EB on the announcer's header. Pre-Dijkstra blocks are passed
-- through unchanged.
--
-- The server keeps only the announcer's announcement point ('LeiosPoint')
-- between blocks rather than the full announcer header. A splice is needed
-- only for a CertRB ('headerContainsLeiosCert'); a block carrying no cert is
-- served unchanged (its predecessor's announced EB may never have been
-- certified), which also skips the deserialise/re-encode roundtrip.
chainSyncBlocksServer ::
  forall m blk.
  ( IOLike m
  , HasHeader (Header blk)
  , ResolveLeiosBlock blk
  , DecodeDisk blk (Lazy.ByteString -> Either DecoderError blk)
  , EncodeDisk blk blk
  ) =>
  Tracer m (TraceChainSyncServerEvent blk) ->
  ChainDB m blk ->
  CodecConfig blk ->
  LeiosDbConnection m ->
  Follower m blk (WithPoint blk (Header blk, Serialised blk)) ->
  ChainSyncServer (Serialised blk) (Point blk) (Tip blk) m ()
chainSyncBlocksServer tracer chainDB ccfg leiosDb flr = ChainSyncServer $ do
  prevAnnVar <- newTVarIO Nothing
  runChainSyncServer $
    chainSyncServerForFollower tracer (ChainDB.getCurrentTip chainDB) $
      wrapFollower prevAnnVar flr
 where
  wrapFollower ::
    StrictTVar m (Maybe LeiosPoint) ->
    Follower m blk (WithPoint blk (Header blk, Serialised blk)) ->
    Follower m blk (WithPoint blk (Serialised blk))
  wrapFollower prevAnnVar f =
    Follower
      { followerInstruction = followerInstruction f >>= traverse onUpdate
      , followerInstructionBlocking = followerInstructionBlocking f >>= onUpdate
      , followerForward = \pts -> do
          changed <- followerForward f pts
          case changed of
            Just pt -> setPrev pt >> pure (Just pt)
            Nothing -> pure Nothing
      , followerClose = followerClose f
      }
   where
    onUpdate ::
      ChainUpdate blk (WithPoint blk (Header blk, Serialised blk)) ->
      m (ChainUpdate blk (WithPoint blk (Serialised blk)))
    onUpdate (AddBlock wp) = AddBlock <$> resolve wp
    onUpdate (RollBack pt) = setPrev pt >> pure (RollBack pt)

    resolve ::
      WithPoint blk (Header blk, Serialised blk) ->
      m (WithPoint blk (Serialised blk))
    resolve (WithPoint (hdr, sblk) pt) = do
      mPrevAnn <- readTVarIO prevAnnVar
      atomically $ writeTVar prevAnnVar (fst <$> headerLeiosAnnouncement hdr)
      sblk' <- case mPrevAnn of
        -- Only a CertRB — a block whose header records that it carries a Leios
        -- certificate ('headerContainsLeiosCert') — splices in an EB closure,
        -- and it splices the EB announced by its predecessor (the one the cert
        -- attests to). A block that merely follows an announcement but carries
        -- no certificate of its own — e.g. one whose predecessor announced an
        -- EB that was never certified — is served unchanged. Splicing there
        -- would inline an EB that is not chain content, and whose closure may
        -- be absent from the LeiosDb (throwing in 'resolveLeiosClosure').
        Just prevAnn | headerContainsLeiosCert hdr -> case decodeRaw sblk of
          Left _ -> pure sblk
          Right blk -> do
            resolveLeiosClosure leiosDb prevAnn
              <&> inlineLeiosClosure blk
              <&> encode
        _ -> pure sblk
      pure (WithPoint sblk' pt)

    decodeRaw :: Serialised blk -> Either String blk
    decodeRaw (Serialised bs) =
      case CBOR.Read.deserialiseFromBytes annotator bs of
        Left e -> Left (show e)
        Right (_, applyBytes) -> case applyBytes bs of
          Left e -> Left (show e)
          Right blk -> Right blk
     where
      annotator :: forall s. CBOR.Decoding.Decoder s (Lazy.ByteString -> Either DecoderError blk)
      annotator = decodeDisk ccfg

    -- possible race? 'pt' could be GC'd from the VolatileDB between the
    -- follower emitting it and this lookup; we then fall back to Nothing.
    setPrev :: Point blk -> m ()
    setPrev pt = case pointToWithOriginRealPoint pt of
      Origin -> atomically $ writeTVar prevAnnVar Nothing
      NotOrigin rp -> do
        mHdr <- ChainDB.getBlockComponent chainDB GetHeader rp
        atomically $ writeTVar prevAnnVar (fst <$> (mHdr >>= headerLeiosAnnouncement))

    encode :: blk -> Serialised blk
    encode = Serialised . CBOR.Write.toLazyByteString . encodeDisk ccfg

-- | A chain sync server.
--
-- This is a version of
-- 'Ouroboros.Network.Protocol.ChainSync.Examples.chainSyncServerExample' that
-- uses an action to get the current 'Tip' and a 'Follower' instead of
-- 'Ourboros.Network.ChainProducerState.ChainProducerState'.
--
-- All the hard work is done by the 'Follower's provided by the 'ChainDB'.
chainSyncServerForFollower ::
  forall m blk b.
  IOLike m =>
  Tracer m (TraceChainSyncServerEvent blk) ->
  STM m (Tip blk) ->
  Follower m blk (WithPoint blk b) ->
  ChainSyncServer b (Point blk) (Tip blk) m ()
chainSyncServerForFollower tracer getCurrentTip flr =
  idle'
 where
  idle :: ServerStIdle b (Point blk) (Tip blk) m ()
  idle =
    ServerStIdle
      { recvMsgRequestNext = handleRequestNext
      , recvMsgFindIntersect = handleFindIntersect
      , recvMsgDoneClient = pure ()
      }

  idle' :: ChainSyncServer b (Point blk) (Tip blk) m ()
  idle' = ChainSyncServer $ return idle

  handleRequestNext ::
    m
      ( Either
          (ServerStNext b (Point blk) (Tip blk) m ())
          (m (ServerStNext b (Point blk) (Tip blk) m ()))
      )
  handleRequestNext =
    ChainDB.followerInstruction flr >>= \case
      Just update -> do
        tip <- atomically getCurrentTip
        let mkTraceEvent =
              TraceChainSyncServerUpdate tip (point <$> update) NonBlocking
        traceWith tracer $ mkTraceEvent RisingEdge
        return $ Left $ sendNext mkTraceEvent tip update
      Nothing -> return $ Right $ do
        -- Follower is at the head, we have to block and wait for the chain to
        -- change.
        update <- ChainDB.followerInstructionBlocking flr
        tip <- atomically getCurrentTip
        let mkTraceEvent =
              TraceChainSyncServerUpdate tip (point <$> update) Blocking
        traceWith tracer $ mkTraceEvent RisingEdge
        return $ sendNext mkTraceEvent tip update

  sendNext ::
    (Enclosing -> TraceChainSyncServerEvent blk) ->
    Tip blk ->
    ChainUpdate blk (WithPoint blk b) ->
    ServerStNext b (Point blk) (Tip blk) m ()
  sendNext mkTraceEvent tip = \case
    AddBlock hdr -> SendMsgRollForward (withoutPoint hdr) tip traceThenIdle
    RollBack pt -> SendMsgRollBackward pt tip traceThenIdle
   where
    traceThenIdle = ChainSyncServer $ do
      traceWith tracer $ mkTraceEvent FallingEdge
      return idle

  handleFindIntersect ::
    [Point blk] ->
    m (ServerStIntersect b (Point blk) (Tip blk) m ())
  handleFindIntersect points = do
    -- TODO guard number of points
    changed <- ChainDB.followerForward flr points
    tip <- atomically getCurrentTip
    case changed of
      Just pt -> return $ SendMsgIntersectFound pt tip idle'
      Nothing -> return $ SendMsgIntersectNotFound tip idle'

{-------------------------------------------------------------------------------
  Trace events
-------------------------------------------------------------------------------}

-- | Events traced by the Chain Sync Server.
data TraceChainSyncServerEvent blk
  = -- | Send a 'ChainUpdate' message.
    TraceChainSyncServerUpdate
      -- | Tip of the currently selected chain.
      (Tip blk)
      -- | The whole headers/blocks in the traced 'ChainUpdate' are substituted
      -- with their corresponding 'Point'.
      (ChainUpdate blk (Point blk))
      BlockingType
      Enclosing
  deriving (Eq, Show)

-- | Whether reading a ChainSync server update instruction was blocking or
-- non-blocking.
data BlockingType = Blocking | NonBlocking
  deriving (Eq, Ord, Show)
