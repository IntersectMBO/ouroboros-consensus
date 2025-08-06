{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Outbound
  ( objectDiffusionOutbound
  , TraceObjectDiffusionOutbound (..)
  , ObjectDiffusionOutboundError (..)
  ) where

import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer, traceWith)
import Data.Foldable (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, isNothing)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as Seq
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Network.ControlMessage
  ( ControlMessage
  , ControlMessageSTM
  , timeoutWithControlMessage
  )
import Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion)
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

data TraceObjectDiffusionOutbound objectId object
  = -- | The IDs of the transactions requested.
    TraceObjectDiffusionOutboundRecvMsgRequestObjects
      [objectId]
  | -- | The transactions to be sent in the response.
    TraceObjectDiffusionOutboundSendMsgReplyObjects
      [object]
  | TraceControlMessage ControlMessage
  deriving Show

data ObjectDiffusionOutboundError
  = ProtocolErrorAckedTooManyObjectIds
  | ProtocolErrorRequestedNothing
  | ProtocolErrorRequestedTooManyObjectIds NumObjectIdsToReq NumObjectIdsToAck
  | ProtocolErrorRequestBlocking
  | ProtocolErrorRequestNonBlocking
  | ProtocolErrorRequestedUnavailableObject
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
    "The peer requested a transaction which is not available, either "
      ++ "because it was never available or because it was previously requested."

objectDiffusionOutbound ::
  forall objectId object ticketNo m.
  (Ord objectId, Ord ticketNo, MonadSTM m, MonadThrow m) =>
  Tracer m (TraceObjectDiffusionOutbound objectId object) ->
  -- | Maximum number of unacknowledged objectIds allowed
  NumObjectIdsToAck ->
  ObjectPoolReader objectId object ticketNo m ->
  NodeToNodeVersion ->
  ControlMessageSTM m ->
  ObjectDiffusionOutbound objectId object m ()
objectDiffusionOutbound tracer maxUnacked ObjectPoolReader{..} _version controlMessageSTM =
  ObjectDiffusionOutbound (pure (mkOutboundState Seq.empty objectPoolZeroTicketNo))
 where
  mkOutboundState :: StrictSeq (object, ticketNo) -> ticketNo -> OutboundStIdle objectId object m ()
  mkOutboundState !outstandingFifo !lastTicketNo =
    OutboundStIdle{recvMsgRequestObjectIds, recvMsgRequestObjects}
   where
    -- Compared to TxSubmission, `outstandingFifo` here contains the full objects, not just their IDs.
    -- Sometimes we need to recover just the IDS.
    onlyIds :: forall f. Functor f => f (object, SizeInBytes) -> f (objectId, SizeInBytes)
    onlyIds = fmap (\(obj, size) -> (rdrGetObjectId obj, size))

    recvMsgRequestObjectIds ::
      forall blocking.
      SingBlockingStyle blocking ->
      NumObjectIdsToAck ->
      NumObjectIdsToReq ->
      m (OutboundStObjectIds blocking objectId object m ())
    recvMsgRequestObjectIds blocking ackNo reqNo = do
      when (getNumObjectIdsToAck ackNo > fromIntegral (Seq.length outstandingFifo)) $
        throwIO ProtocolErrorAckedTooManyObjectIds

      when
        ( fromIntegral (Seq.length outstandingFifo)
            - getNumObjectIdsToAck ackNo
            + getNumObjectIdsToReq reqNo
            > getNumObjectIdsToAck maxUnacked
        )
        $ throwIO (ProtocolErrorRequestedTooManyObjectIds reqNo maxUnacked)

      -- First we update our tracking state to remove the number of objectIds
      -- that the peer has acknowledged.
      let !outstandingFifo' = Seq.drop (fromIntegral ackNo) outstandingFifo

      -- We define how to update our tracking state when extra objects are available.
      let updateState newObjects =
            -- These objects should all be fresh
            assert (all (\(_, ticketNo, _) -> ticketNo > lastTicketNo) newObjects) $
              let !outstandingFifo'' =
                    outstandingFifo'
                      <> Seq.fromList
                        [(object, ticketNo) | (object, ticketNo, _) <- newObjects]
                  !lastTicketNo'
                    | null newObjects = lastTicketNo
                    | otherwise = ticketNo
                   where
                    (_, ticketNo, _) = last newObjects
                  objects' :: [(object, SizeInBytes)]
                  objects' = [(object, size) | (object, _, size) <- newObjects]
                  outboundState = mkOutboundState outstandingFifo'' lastTicketNo'
               in (objects', outboundState)

      -- Grab info about any new objects after the last object ticketNo we've seen,
      -- up to the number that the peer has requested.
      case blocking of
        -----------------------------------------------------------------------
        SingBlocking -> do
          when (reqNo == 0) $
            throwIO ProtocolErrorRequestedNothing
          unless (Seq.null outstandingFifo') $
            throwIO ProtocolErrorRequestBlocking

          mbNewObjects <- timeoutWithControlMessage controlMessageSTM $
            do
              ObjectPoolSnapshot{objectPoolObjectsAfter} <- objectPoolGetSnapshot
              let newObjects = objectPoolObjectsAfter lastTicketNo
              check (not $ null newObjects)
              pure (take (fromIntegral reqNo) newObjects)

          case mbNewObjects of
            Nothing -> pure (SendMsgDone ())
            Just newObjects ->
              let !(newObjects', outboundState') = updateState newObjects
                  newObjects'' = case NonEmpty.nonEmpty newObjects' of
                    Just x -> x
                    -- Assert objects is non-empty: we blocked until objects was non-null,
                    -- and we know reqNo > 0, hence `take reqNo objects` is non-null.
                    Nothing -> error "objectDiffusionOutbound: empty transaction's list"
               in pure (SendMsgReplyObjectIds (BlockingReply (onlyIds newObjects'')) outboundState')
        -----------------------------------------------------------------------
        SingNonBlocking -> do
          when (reqNo == 0 && ackNo == 0) $
            throwIO ProtocolErrorRequestedNothing
          when (Seq.null outstandingFifo') $
            throwIO ProtocolErrorRequestNonBlocking

          newObjects <- atomically $ do
            ObjectPoolSnapshot{objectPoolObjectsAfter} <- objectPoolGetSnapshot
            let newObjects = objectPoolObjectsAfter lastTicketNo
            return (take (fromIntegral reqNo) newObjects)

          let !(newObjects', outboundState') = updateState newObjects
          pure (SendMsgReplyObjectIds (NonBlockingReply (onlyIds newObjects')) outboundState')

    recvMsgRequestObjects ::
      [objectId] ->
      m (OutboundStObjects objectId object m ())
    recvMsgRequestObjects reqObjectIds = do
      -- Trace the IDs of the transactions requested.
      traceWith tracer (TraceObjectDiffusionOutboundRecvMsgRequestObjects reqObjectIds)

      -- All the objects correspond to advertised objectIds are already in the outstandingFifo.
      -- So we don't need to read from the object pool here.

      -- The window size is expected to be small for cert diffusion, so the find is probably acceptable.
      -- TODO: revisit the underlying outstandingFifo data structure and search when
      -- we will use ObjectDiffusion for votes (and not just cert diffusion)
      let requestedObjects =
            [ find (\(obj, _) -> rdrGetObjectId obj == reqObjectId) outstandingFifo | reqObjectId <- reqObjectIds
            ]
      when (any isNothing requestedObjects) $
        throwIO ProtocolErrorRequestedUnavailableObject
      let requestedObjects' = fst <$> catMaybes requestedObjects
          outboundState' = mkOutboundState outstandingFifo lastTicketNo

      -- Trace the transactions to be sent in the response.
      traceWith tracer (TraceObjectDiffusionOutboundSendMsgReplyObjects requestedObjects')

      return $ SendMsgReplyObjects requestedObjects' outboundState'
