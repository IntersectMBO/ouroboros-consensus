{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( Tip
  , chainSyncBlockServerFollower
  , chainSyncBlocksServer
  , chainSyncHeaderServerFollower
  , chainSyncHeadersServer
  , RbParentNotFound (..)
  , findParentPoint
  , headerOnChain
  , inlineCertifiedEbsFollower
  , inlineCertifiedEbsFollowerOnChainDB
  , parentOnChain

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
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.ResourceRegistry (ResourceRegistry)
import Control.Tracer
import qualified Data.ByteString.Lazy as Lazy
import Data.Typeable (Typeable)
import LeiosDemoTypes (LeiosPoint (..))
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Storage.ChainDB.API
  ( BlockComponent (GetHeader, GetRawBlock, GetSlot)
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
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
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
-- The blocks go through 'inlineCertifiedEbsFollowerOnChainDB', so a client sees
-- the transactions of a certified EB in the block that announced it.
chainSyncBlocksServer ::
  forall m blk.
  ( IOLike m
  , GetPrevHash blk
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
  flr' <- inlineCertifiedEbsFollowerOnChainDB chainDB ccfg leiosDbReader flr
  runChainSyncServer $
    chainSyncServerForFollower tracer (ChainDB.getCurrentTip chainDB) flr'

-- | 'inlineCertifiedEbsFollower' for a follower of this ChainDB. When a
-- rollback needs a block or its parent, it reads them from this ChainDB.
inlineCertifiedEbsFollowerOnChainDB ::
  forall m blk.
  ( IOLike m
  , GetPrevHash blk
  , ResolveLeiosBlock blk
  , DecodeDisk blk (Lazy.ByteString -> Either DecoderError blk)
  , EncodeDisk blk blk
  ) =>
  ChainDB m blk ->
  CodecConfig blk ->
  LeiosDbReader m ->
  Follower m blk (WithPoint blk (Header blk, Serialised blk)) ->
  m (Follower m blk (WithPoint blk (Serialised blk)))
inlineCertifiedEbsFollowerOnChainDB chainDB ccfg leiosDbReader =
  inlineCertifiedEbsFollower
    ccfg
    leiosDbReader
    (headerOnChain getChain (ChainDB.getBlockComponent chainDB GetHeader))
    -- Read only after 'headerOnChain' found RB on the selected chain.
    (ChainDB.getBlockComponent chainDB (Serialised <$> GetRawBlock))
    (parentOnChain getChain (ChainDB.getBlockComponent chainDB GetSlot))
 where
  getChain = ChainDB.getCurrentChain chainDB

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
-- * __Rule 3: one block deeper.__ A rollback to an RB that announced an EB
--   goes to RB's parent instead, then RB is sent again through rule 1. After a
--   chain switch or a reconnection, the client may have the wrong version of
--   RB. Its parent is still right: its version depends only on RB, which the
--   rollback keeps.
--
-- A CertRB can announce its own EB, so it follows the same rules.
--
-- Throws 'RbParentNotFound' if rule 3 cannot find RB's parent, rather than
-- leave the client with the wrong version of RB.
inlineCertifiedEbsFollower ::
  forall m blk.
  ( IOLike m
  , HasHeader blk
  , ResolveLeiosBlock blk
  , DecodeDisk blk (Lazy.ByteString -> Either DecoderError blk)
  , EncodeDisk blk blk
  ) =>
  CodecConfig blk ->
  LeiosDbReader m ->
  -- | Read the header at a rollback point, to see if rule 3 applies.
  -- 'Nothing' if the point is no longer on the chain.
  (RealPoint blk -> m (Maybe (Header blk))) ->
  -- | Read RB, to send it again. 'Nothing' if the ChainDB no longer has it.
  (RealPoint blk -> m (Maybe (Serialised blk))) ->
  -- | Find RB's parent. 'Nothing' throws 'RbParentNotFound'.
  (Header blk -> m (Maybe (Point blk))) ->
  Follower m blk (WithPoint blk (Header blk, Serialised blk)) ->
  m (Follower m blk (WithPoint blk (Serialised blk)))
inlineCertifiedEbsFollower ccfg leiosDbReader lookupHeader lookupRawBlock lookupParent f = do
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
    PendingRaw (RollBack pt) -> case pointToWithOriginRealPoint pt of
      Origin -> rollBackTo pt
      NotOrigin rp ->
        runMaybeT announcingRb >>= \case
          -- No EB announced, or the block left the chain. If it left, the
          -- inner follower rolls back past it next.
          Nothing -> rollBackTo pt
          -- Rule 3.
          Just (hdr, sblk) -> do
            parent <- lookupParent hdr >>= maybe (throwIO (RbParentNotFound rp)) pure
            -- Send RB next, before anything already queued.
            atomically $ modifyTVar stVar $ \st ->
              st{wsPending = PendingRaw (AddBlock (WithPoint (hdr, sblk) pt)) : wsPending st}
            rollBackTo parent
       where
        -- The block at the rollback point, if it announced an EB and is still
        -- on the chain.
        announcingRb = do
          hdr <- MaybeT (lookupHeader rp)
          _ <- MaybeT (pure (headerLeiosAnnouncement hdr))
          sblk <- MaybeT (lookupRawBlock rp)
          pure (hdr, sblk)
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
    -- After a rollback, rule 2 has nothing to fix: rule 3 never leaves the
    -- client at an RB that announced an EB. If the parent it rolls back to
    -- announced one too, the client already has the right version of it.
    rollBackTo p = do
      atomically $ modifyTVar stVar $ \st ->
        st{wsClientTip = p, wsPlainAnnouncer = Nothing}
      pure (RollBack p)

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

-- | Thrown by 'inlineCertifiedEbsFollower' when rule 3 cannot find the parent
-- of this RB, which announced an EB.
data RbParentNotFound blk = RbParentNotFound !(RealPoint blk)

deriving instance StandardHash blk => Show (RbParentNotFound blk)

instance (StandardHash blk, Typeable blk) => Exception (RbParentNotFound blk)

-- | The header at a point, if the point is on the selected chain.
--
-- The node keeps its last k blocks in memory, in order. There are three cases:
--
-- * The block is one of them: its header is taken from memory.
--
-- * The block is older than all of them: its header is read with the given
--   function.
--
-- * The block is recent but not one of them: it is no longer on the node's
--   chain, so the result is 'Nothing'.
headerOnChain ::
  (IOLike m, HasHeader (Header blk)) =>
  STM m (AnchoredFragment (Header blk)) ->
  (RealPoint blk -> m (Maybe (Header blk))) ->
  RealPoint blk ->
  m (Maybe (Header blk))
headerOnChain getChain readHeader rp = do
  chain <- atomically getChain
  case AF.splitAfterPoint chain (realPointToPoint rp) of
    Just (before, _) | Right hdr <- AF.head before -> pure (Just hdr)
    _
      | atOrBelowImmutableTip chain (realPointSlot rp) -> readHeader rp
      | otherwise -> pure Nothing

-- | The point of a header's parent, given a way to read the slot of a block.
--
-- The node keeps its last k blocks in memory, in order. There are three cases:
--
-- * The block is one of them: its parent is the block just before it.
--
-- * The block is older than all of them: 'findParentPoint' finds its parent.
--
-- * The block is recent but not one of them: it is no longer on the node's
--   chain, so the result is 'Nothing'.
parentOnChain ::
  (IOLike m, GetPrevHash blk) =>
  STM m (AnchoredFragment (Header blk)) ->
  (RealPoint blk -> m (Maybe SlotNo)) ->
  Header blk ->
  m (Maybe (Point blk))
parentOnChain getChain slotOf hdr = do
  chain <- atomically getChain
  case AF.splitBeforePoint chain (blockPoint hdr) of
    Just (before, _) -> pure (Just (castPoint (AF.headPoint before)))
    Nothing
      | atOrBelowImmutableTip chain (blockSlot hdr) -> findParentPoint slotOf hdr
      | otherwise -> pure Nothing

-- | Whether a slot is older than all of the node's last k blocks, which it
-- keeps in memory.
atOrBelowImmutableTip :: AnchoredFragment (Header blk) -> SlotNo -> Bool
atOrBelowImmutableTip chain s = NotOrigin s <= pointSlot (AF.anchorPoint chain)

-- | The point of a block's parent, given a way to read the slot of a block.
--
-- The header has the parent's hash but not its slot, so this searches for it:
--
-- * It tries each slot, going down from the block's own slot. It starts there,
--   not one below, because a Byron EBB has the same slot as the next block.
--
-- * If the parent is missing, it goes all the way down to slot 0.
--   'parentOnChain' only calls this for older blocks, whose parent is always on
--   disk, so that only happens if the ImmutableDB is corrupt.
findParentPoint ::
  forall m blk.
  (Monad m, GetPrevHash blk) =>
  (RealPoint blk -> m (Maybe SlotNo)) ->
  Header blk ->
  m (Maybe (Point blk))
findParentPoint slotOf hdr = case headerPrevHash hdr of
  GenesisHash -> pure (Just GenesisPoint)
  BlockHash h -> go h (blockSlot hdr)
 where
  go :: HeaderHash blk -> SlotNo -> m (Maybe (Point blk))
  go h s =
    slotOf (RealPoint s h) >>= \case
      Just s' -> pure (Just (BlockPoint s' h))
      Nothing
        | s == 0 -> pure Nothing
        | otherwise -> go h (s - 1)

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
