{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.MiniProtocol.ChainSync.Server
  ( Tip
  , chainSyncBlockServerFollower
  , chainSyncBlocksServer
  , chainSyncHeaderServerFollower
  , chainSyncHeadersServer

    -- * Certified endorser blocks
  , inlineCertifiedEbTxs
  , CertRbWithoutEbTxs (..)
  , CertRbWithoutEbTxsReason (..)

    -- * Trace events
  , BlockingType (..)
  , TraceChainSyncServerEvent (..)

    -- * Low-level API
  , chainSyncServerForFollower
  ) where

import Cardano.Ledger.Binary (DecoderError (..))
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import Control.ResourceRegistry (ResourceRegistry)
import Control.Tracer
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Leios.Protocol (LeiosProtocol (..))
import Ouroboros.Consensus.Leios.Types (LeiosPoint)
import Ouroboros.Consensus.Storage.ChainDB.API
  ( ChainDB
  , Follower
  , WithPoint (..)
  , getSerialisedBlockWithPoint
  , getSerialisedHeaderWithPoint
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.API as ChainDB
import Ouroboros.Consensus.Storage.LedgerDB.Forker (ResolveEndorserBlock)
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
  m (Follower m blk (WithPoint blk (Serialised blk)))
chainSyncBlockServerFollower chainDB registry =
  ChainDB.newFollower chainDB registry ChainDB.SelectedChain getSerialisedBlockWithPoint

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
chainSyncBlocksServer ::
  forall m blk.
  (IOLike m, HasHeader (Header blk)) =>
  Tracer m (TraceChainSyncServerEvent blk) ->
  ChainDB m blk ->
  Follower m blk (WithPoint blk (Serialised blk)) ->
  ChainSyncServer (Serialised blk) (Point blk) (Tip blk) m ()
chainSyncBlocksServer tracer chainDB flr =
  chainSyncServerForFollower tracer (ChainDB.getCurrentTip chainDB) flr

{-------------------------------------------------------------------------------
  Certified endorser blocks
-------------------------------------------------------------------------------}

-- | Prepare a stored block for the node-to-client chain sync server.
--
-- A block that certifies an endorser block is sent with the endorser block's
-- transactions in its body. All other blocks are sent as stored.
--
-- Throws 'CertRbWithoutEbTxs' if the transactions cannot be added.
-- A missing endorser block is thrown by the resolver instead,
-- as 'ChainDB.ChainDbMissingEndorserBlock'.
inlineCertifiedEbTxs ::
  forall m blk.
  ( MonadThrow m
  , BlockSupportsProtocol blk
  , BlockSupportsLeios blk
  , DecodeDisk blk (Lazy.ByteString -> Either DecoderError blk)
  , EncodeDisk blk blk
  ) =>
  BlockConfig blk ->
  CodecConfig blk ->
  ResolveEndorserBlock m ->
  -- | The endorser block the block before this one announced, if any.
  Maybe LeiosPoint ->
  Point blk ->
  Header blk ->
  Serialised blk ->
  m (Serialised blk)
inlineCertifiedEbTxs bcfg ccfg resolveEb mAnnounced pt hdr stored
  | not containsCert = pure stored
  | otherwise = do
      announced <- maybe (failWith NoAnnouncement) pure mAnnounced
      blk <- either (failWith . Undecodable) pure (decodeStored stored)
      eb <- resolveEb announced
      certRb <- either (failWith . EbTxsNotInlined) pure (inlineEndorserBlockTxs eb blk)
      pure . Serialised . CBOR.toLazyByteString $ encodeDisk ccfg certRb
 where
  containsCert = getContainsLeiosCert (Proxy @(BlockProtocol blk)) (validateView bcfg hdr)

  failWith :: CertRbWithoutEbTxsReason -> m a
  failWith = throwIO . CertRbWithoutEbTxs pt

  decodeStored :: Serialised blk -> Either DecoderError blk
  decodeStored (Serialised bytes) =
    case CBOR.deserialiseFromBytes (decodeDisk ccfg) bytes of
      Left err -> Left $ DecoderErrorDeserialiseFailure label err
      Right (trailing, mkBlk)
        | Lazy.null trailing -> mkBlk bytes
        | otherwise -> Left $ DecoderErrorLeftover label (Lazy.toStrict trailing)
   where
    label = Text.pack "Block"

-- | Thrown by 'inlineCertifiedEbTxs' when it cannot add the endorser block's
-- transactions to a CertRB. Holds the CertRB's point and the reason.
data CertRbWithoutEbTxs blk = CertRbWithoutEbTxs
  { certRbPoint :: !(Point blk)
  , certRbReason :: !CertRbWithoutEbTxsReason
  }

deriving instance StandardHash blk => Show (CertRbWithoutEbTxs blk)

instance (StandardHash blk, Typeable blk) => Exception (CertRbWithoutEbTxs blk)

-- | Reason for a 'CertRbWithoutEbTxs'.
data CertRbWithoutEbTxsReason
  = -- | No endorser block announcement was given for the CertRB.
    NoAnnouncement
  | -- | The stored CertRB does not decode.
    Undecodable !DecoderError
  | -- | The endorser block's transactions could not be added to the CertRB
    -- ('inlineEndorserBlockTxs' failed).
    EbTxsNotInlined !DecoderError
  deriving (Show, Eq)

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
