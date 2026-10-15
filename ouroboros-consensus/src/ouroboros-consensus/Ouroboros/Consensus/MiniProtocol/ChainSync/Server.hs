{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( Tip
  , chainSyncBlockServerFollower
  , chainSyncBlocksServer
  , chainSyncHeaderServerFollower
  , chainSyncHeadersServer
  , inlineCertifiedEbsFollower

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
import LeiosDemoTypes (LeiosPoint (..))
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
import Ouroboros.Consensus.Storage.LeiosDB (LeiosDbReader)
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
-- The blocks go through 'inlineCertifiedEbsFollower', so a client sees the
-- transactions of a certified EB in the block that announced it.
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
  LeiosDbReader m ->
  Follower m blk (WithPoint blk (Header blk, Serialised blk)) ->
  ChainSyncServer (Serialised blk) (Point blk) (Tip blk) m ()
chainSyncBlocksServer tracer chainDB ccfg leiosDbReader flr = ChainSyncServer $ do
  flr' <- inlineCertifiedEbsFollower ccfg leiosDbReader flr
  runChainSyncServer $
    chainSyncServerForFollower tracer (ChainDB.getCurrentTip chainDB) flr'

-- | Wrap a follower so that, once an EB is certified, the client gets RB⁺
-- (RB plus the EB's transactions) in place of the RB that announced it.
-- CertRBs are sent unchanged.
--
-- * __Rule 1: look ahead.__ Before sending an RB that announced an EB, peek at
--   the next block without waiting. If it is a CertRB, send RB⁺, else send RB.
--   A client catching up gets RB⁺ with no rollback.
--
-- * __Rule 2: late certificate.__ If a CertRB comes after the client got plain
--   RB, roll back to RB's parent, then send RB⁺ and the CertRB.
--
-- A CertRB can announce its own EB, so it follows the same rules.
--
-- TODO rule 3: after a chain switch or a reconnection, the client can have RB
-- or RB⁺ at a point the server rolls back to, and the server cannot tell
-- which. Rolling back one block deeper fixes this.
inlineCertifiedEbsFollower ::
  forall m blk.
  ( IOLike m
  , HasHeader (Header blk)
  , ResolveLeiosBlock blk
  , DecodeDisk blk (Lazy.ByteString -> Either DecoderError blk)
  , EncodeDisk blk blk
  ) =>
  CodecConfig blk ->
  LeiosDbReader m ->
  Follower m blk (WithPoint blk (Header blk, Serialised blk)) ->
  m (Follower m blk (WithPoint blk (Serialised blk)))
inlineCertifiedEbsFollower ccfg leiosDbReader f = do
  -- Unchecked: the state holds headers and raw blocks, and only this
  -- client's thread uses it.
  stVar <- uncheckedNewTVarM initState
  pure
    Follower
      { followerInstruction =
          popPending stVar >>= \case
            Just p -> Just <$> process stVar p
            Nothing -> followerInstruction f >>= traverse (process stVar . PendingRaw)
      , followerInstructionBlocking =
          popPending stVar >>= \case
            Just p -> process stVar p
            Nothing -> followerInstructionBlocking f >>= process stVar . PendingRaw
      , followerForward = \pts -> do
          changed <- followerForward f pts
          case changed of
            -- The inner follower starts again from the intersection, so what
            -- was pending is stale.
            Just pt -> atomically $ writeTVar stVar initState{wsClientTip = pt}
            Nothing -> pure ()
          pure changed
      , followerClose = followerClose f
      }
 where
  initState :: WrapperState blk
  initState =
    WrapperState
      { wsPending = []
      , wsClientTip = GenesisPoint
      , wsPlainAnnouncer = Nothing
      }

  popPending :: StrictTVar m (WrapperState blk) -> m (Maybe (Pending blk))
  popPending stVar = atomically $ do
    st <- readTVar stVar
    case wsPending st of
      [] -> pure Nothing
      p : ps -> writeTVar stVar st{wsPending = ps} >> pure (Just p)

  -- The next instruction, for rule 1. It stays pending, to be sent next.
  lookAhead ::
    StrictTVar m (WrapperState blk) ->
    m (Maybe (ChainUpdate blk (WithPoint blk (Header blk, Serialised blk))))
  lookAhead stVar =
    wsPending <$> readTVarIO stVar >>= \case
      PendingRaw u : _ -> pure (Just u)
      -- Not reached: rule 2 queues RB⁺ ahead of the CertRB, and RB⁺ is sent
      -- without looking ahead.
      PendingReady _ : _ -> pure Nothing
      [] ->
        followerInstruction f >>= \case
          Nothing -> pure Nothing
          Just u -> do
            atomically $ modifyTVar stVar $ \st -> st{wsPending = wsPending st <> [PendingRaw u]}
            pure (Just u)

  process ::
    StrictTVar m (WrapperState blk) ->
    Pending blk ->
    m (ChainUpdate blk (WithPoint blk (Serialised blk)))
  process stVar = \case
    PendingReady u -> do
      atomically $ modifyTVar stVar $ \st ->
        st{wsClientTip = updatePoint u, wsPlainAnnouncer = Nothing}
      pure u
    PendingRaw (RollBack pt) -> do
      atomically $ modifyTVar stVar $ \st ->
        st
          { wsClientTip = pt
          , -- The client still has plain RB only if the rollback keeps it as
            -- the tip.
            wsPlainAnnouncer = case wsPlainAnnouncer st of
              Just pa | blockPoint (fst (withoutPoint (paBlock pa))) == castPoint pt -> Just pa
              _ -> Nothing
          }
      pure (RollBack pt)
    PendingRaw (AddBlock wp@(WithPoint (hdr, sblk) pt)) -> do
      st <- readTVarIO stVar
      case wsPlainAnnouncer st of
        -- Rule 2. The CertRB extends the client's tip, so it certifies the EB
        -- of the plain RB the client has.
        Just pa | headerContainsLeiosCert hdr -> do
          rbPlus <- buildRbPlus (paBlock pa) (paEb pa)
          atomically $
            writeTVar
              stVar
              st
                { wsPending =
                    PendingReady (AddBlock rbPlus) : PendingRaw (AddBlock wp) : wsPending st
                , wsClientTip = paParent pa
                , wsPlainAnnouncer = Nothing
                }
          pure (RollBack (paParent pa))
        _ -> case headerLeiosAnnouncement hdr of
          Nothing -> send (WithPoint sblk pt) Nothing
          -- Rule 1.
          Just (eb, _size) ->
            lookAhead stVar >>= \case
              Just (AddBlock (WithPoint (nextHdr, _) _))
                | headerContainsLeiosCert nextHdr -> do
                    rbPlus <- buildRbPlus wp eb
                    send rbPlus Nothing
              _ ->
                send (WithPoint sblk pt) $
                  Just
                    PlainAnnouncer
                      { paParent = wsClientTip st
                      , paBlock = wp
                      , paEb = eb
                      }
   where
    send out pa = do
      atomically $ modifyTVar stVar $ \st ->
        st{wsClientTip = point out, wsPlainAnnouncer = pa}
      pure (AddBlock out)

  updatePoint :: ChainUpdate blk (WithPoint blk b) -> Point blk
  updatePoint = \case
    AddBlock wp -> point wp
    RollBack pt -> pt

  -- RB⁺ has RB's header, so it has RB's point.
  --
  -- TODO: this sends RB unchanged when it can't decode RB or read the EB's
  -- closure. It should fail loudly instead.
  buildRbPlus ::
    WithPoint blk (Header blk, Serialised blk) ->
    LeiosPoint ->
    m (WithPoint blk (Serialised blk))
  buildRbPlus (WithPoint (_, sblk) pt) eb = case decodeRaw sblk of
    Left _ -> pure (WithPoint sblk pt)
    Right blk ->
      resolveLeiosClosure leiosDbReader (pointEbHash eb) >>= \case
        Left _ -> pure (WithPoint sblk pt)
        Right closure -> pure (WithPoint (encode (inlineLeiosClosure blk (map snd closure))) pt)

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

  encode :: blk -> Serialised blk
  encode = Serialised . CBOR.Write.toLazyByteString . encodeDisk ccfg

-- | The state of 'inlineCertifiedEbsFollower' for one client.
data WrapperState blk = WrapperState
  { wsPending :: ![Pending blk]
  -- ^ What to send before asking the inner follower again. One instruction
  -- from the inner follower can become up to three for the client.
  , wsClientTip :: !(Point blk)
  -- ^ The client's tip, once it has what was sent.
  , wsPlainAnnouncer :: !(Maybe (PlainAnnouncer blk))
  -- ^ Set only when the client's tip is plain RB, and RB announced an EB.
  }

data Pending blk
  = -- | From the inner follower, still to go through the rules.
    PendingRaw !(ChainUpdate blk (WithPoint blk (Header blk, Serialised blk)))
  | -- | To send as it is.
    PendingReady !(ChainUpdate blk (WithPoint blk (Serialised blk)))

-- | A plain RB at the client's tip, which announced an EB. What rule 2 needs
-- if a CertRB comes next.
data PlainAnnouncer blk = PlainAnnouncer
  { paParent :: !(Point blk)
  -- ^ RB's parent, recorded when RB was sent, so no lookup is needed.
  , paBlock :: !(WithPoint blk (Header blk, Serialised blk))
  , paEb :: !LeiosPoint
  -- ^ The EB that RB announced.
  }

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
