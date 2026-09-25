{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Outbound
  ( objectDiffusionOutbound
  , TraceObjectDiffusionOutbound (..)
  , ObjectDiffusionOutboundError (..)
  ) where

import Cardano.Network.NodeToNode.Version (NodeToNodeVersion)
import Control.Monad (join, unless, when)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer, traceWith)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as Seq
import Data.Set qualified as Set
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

-- Note: This module is inspired from TxSubmission outbound side.

data TraceObjectDiffusionOutbound objectId object
  = TraceObjectDiffusionOutboundRecvMsgRequestObjectIds NumObjectIdsReq
  | -- | The IDs to be sent in the response
    TraceObjectDiffusionOutboundSendMsgReplyObjectIds [objectId]
  | -- | The IDs of the objects requested.
    TraceObjectDiffusionOutboundRecvMsgRequestObjects
      [objectId]
  | -- | The objects to be sent in the response.
    TraceObjectDiffusionOutboundSendMsgReplyObjects
      [object]
  | -- | Received 'MsgDone'
    TraceObjectDiffusionOutboundTerminated
  deriving Show

data ObjectDiffusionOutboundError
  = ProtocolErrorAckedTooManyObjectIds
  | ProtocolErrorRequestedNothing
  | ProtocolErrorRequestedTooManyObjectIds NumObjectIdsReq NumObjectsUnacknowledged
  | ProtocolErrorRequestBlocking
  | ProtocolErrorRequestNonBlocking
  | ProtocolErrorRequestedUnavailableObject
  | ProtocolErrorRequestedDuplicateObject
  deriving Show

instance Exception ObjectDiffusionOutboundError where
  displayException ProtocolErrorAckedTooManyObjectIds =
    "The peer tried to acknowledged more objectIds than are available to do so."
  displayException (ProtocolErrorRequestedTooManyObjectIds reqNo maxUnacked) =
    "The peer requested "
      ++ show reqNo
      ++ " objectIds which would put the "
      ++ "total in flight over the limit of "
      ++ show maxUnacked
  displayException ProtocolErrorRequestedNothing =
    "The peer requested zero objectIds."
  displayException ProtocolErrorRequestBlocking =
    "The peer made a blocking request for more objectIds when there are still "
      ++ "unacknowledged objectIds. It should have used a non-blocking request."
  displayException ProtocolErrorRequestNonBlocking =
    "The peer made a non-blocking request for more objectIds when there are "
      ++ "no unacknowledged objectIds. It should have used a blocking request."
  displayException ProtocolErrorRequestedUnavailableObject =
    "The peer requested an object which is not available, either "
      ++ "because it was never available or because it was previously requested."
  displayException ProtocolErrorRequestedDuplicateObject =
    "The peer requested the same object twice."

data OutboundSt objectId object ticketNo = OutboundSt
  { outstandingFifo :: !(StrictSeq object)
  , lastTicketNo :: !ticketNo
  }

objectDiffusionOutbound ::
  forall objectId object ticketNo m.
  (Ord objectId, MonadSTM m, MonadThrow m) =>
  Tracer m (TraceObjectDiffusionOutbound objectId object) ->
  -- | Maximum number of unacknowledged objectIds allowed
  NumObjectsUnacknowledged ->
  ObjectPoolReader objectId object ticketNo m ->
  NodeToNodeVersion ->
  ObjectDiffusionOutbound objectId object m ()
objectDiffusionOutbound tracer maxFifoLength ObjectPoolReader{..} _version =
  ObjectDiffusionOutbound (pure (makeBundle $ OutboundSt Seq.empty oprZeroTicketNo))
 where
  makeBundle :: OutboundSt objectId object ticketNo -> OutboundStIdle objectId object m ()
  makeBundle !st =
    OutboundStIdle
      { recvMsgRequestObjectIds = recvMsgRequestObjectIds st
      , recvMsgRequestObjects = recvMsgRequestObjects st
      , recvMsgDone = traceWith tracer TraceObjectDiffusionOutboundTerminated
      }

  updateStNewObjects ::
    OutboundSt objectId object ticketNo ->
    [(ticketNo, object)] ->
    OutboundSt objectId object ticketNo
  updateStNewObjects !OutboundSt{..} sortedNewContent =
    let !outstandingFifo' =
          outstandingFifo
            <> (Seq.fromList $ snd <$> sortedNewContent)
        !lastTicketNo'
          | null sortedNewContent = lastTicketNo
          | otherwise = fst $ last sortedNewContent
     in OutboundSt
          { outstandingFifo = outstandingFifo'
          , lastTicketNo = lastTicketNo'
          }

  recvMsgRequestObjectIds ::
    forall blocking.
    OutboundSt objectId object ticketNo ->
    SingBlockingStyle blocking ->
    NumObjectIdsAck ->
    NumObjectIdsReq ->
    m (OutboundStObjectIds blocking objectId object m ())
  recvMsgRequestObjectIds !st@OutboundSt{..} blocking numIdsToAck numIdsToReq = do
    traceWith tracer (TraceObjectDiffusionOutboundRecvMsgRequestObjectIds numIdsToReq)

    when (numIdsToAck > fromIntegral (Seq.length outstandingFifo)) $
      throwIO ProtocolErrorAckedTooManyObjectIds

    when
      ( Seq.length outstandingFifo
          - fromIntegral numIdsToAck
          + fromIntegral numIdsToReq
          > fromIntegral maxFifoLength
      )
      $ throwIO (ProtocolErrorRequestedTooManyObjectIds numIdsToReq maxFifoLength)

    -- First we update our FIFO to remove the number of objectIds that the
    -- inbound peer has acknowledged.
    let !outstandingFifo' = Seq.drop (fromIntegral numIdsToAck) outstandingFifo
        -- must specify the type here otherwise GHC complains about mismatch objectId types
        st' :: OutboundSt objectId object ticketNo
        !st' = st{outstandingFifo = outstandingFifo'}

    -- Grab info about any new objects after the last object ticketNo we've
    -- seen, up to the number that the peer has requested.
    case blocking of
      -----------------------------------------------------------------------
      SingBlocking -> do
        when (numIdsToReq == 0) $
          throwIO ProtocolErrorRequestedNothing
        unless (Seq.null outstandingFifo') $
          throwIO ProtocolErrorRequestBlocking

        -- oprObjectsAfter returns STM (Maybe (m (Map ...))).
        -- The STM layer retries efficiently until new content is signalled (Just).
        -- However, in rare cases the IO action may still yield an empty
        -- map (e.g. objects GC'd between the STM check and IO read),
        -- so we loop in IO as well.
        let getNewContent = do
              content <- join . atomically $ do
                maybeNewObjectsAction <-
                  oprObjectsAfter
                    lastTicketNo
                    (fromIntegral numIdsToReq)
                case maybeNewObjectsAction of
                  Nothing -> retry
                  Just newObjectsAction -> pure newObjectsAction
              if null content then getNewContent else pure content
        sortedNewContent <- Map.toAscList <$> getNewContent

        let !newIds = oprObjectId . snd <$> sortedNewContent
            st'' = updateStNewObjects st' sortedNewContent

        traceWith tracer (TraceObjectDiffusionOutboundSendMsgReplyObjectIds newIds)

        pure $
          SendMsgReplyObjectIds
            (BlockingReply (NonEmpty.fromList $ newIds))
            (makeBundle st'')

      -----------------------------------------------------------------------
      SingNonBlocking -> do
        when (numIdsToReq == 0 && numIdsToAck == 0) $
          throwIO ProtocolErrorRequestedNothing
        when (Seq.null outstandingFifo') $
          throwIO ProtocolErrorRequestNonBlocking

        let getNewContent = join . atomically $ do
              maybeNewObjectsAction <-
                oprObjectsAfter
                  lastTicketNo
                  (fromIntegral numIdsToReq)
              pure $ fromMaybe (pure Map.empty) maybeNewObjectsAction

        sortedNewContent <- Map.toAscList <$> getNewContent

        let !newIds = oprObjectId . snd <$> sortedNewContent
            st'' = updateStNewObjects st' sortedNewContent

        traceWith tracer (TraceObjectDiffusionOutboundSendMsgReplyObjectIds newIds)

        pure (SendMsgReplyObjectIds (NonBlockingReply newIds) (makeBundle st''))

  recvMsgRequestObjects ::
    OutboundSt objectId object ticketNo ->
    [objectId] ->
    m (OutboundStObjects objectId object m ())
  recvMsgRequestObjects !st@OutboundSt{..} requestedIds = do
    traceWith tracer (TraceObjectDiffusionOutboundRecvMsgRequestObjects requestedIds)

    -- All the objects correspond to advertised objectIds are already in the
    -- outstandingFifo. So we don't need to read from the object pool here.

    -- I've optimized the search to do only one traversal of 'outstandingFifo'.
    -- When the 'requestedIds' is exactly the whole 'outstandingFifo', then this
    -- should take O(n * log n) time.
    --
    -- TODO: We might need to revisit the underlying 'outstandingFifo' data
    -- structure and the search if performance isn't sufficient when we'll use
    -- ObjectDiffusion for votes diffusion (and not just cert diffusion).

    let requestedIdsSet = Set.fromList requestedIds

    when (Set.size requestedIdsSet /= length requestedIds) $
      throwIO ProtocolErrorRequestedDuplicateObject

    let requestedObjects =
          foldr
            ( \obj acc ->
                if Set.member (oprObjectId obj) requestedIdsSet
                  then obj : acc
                  else acc
            )
            []
            outstandingFifo

    when (Set.size requestedIdsSet /= length requestedObjects) $
      throwIO ProtocolErrorRequestedUnavailableObject

    traceWith tracer (TraceObjectDiffusionOutboundSendMsgReplyObjects requestedObjects)

    pure (SendMsgReplyObjects requestedObjects (makeBundle st))
