{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Outbound
  ( objectDiffusionOutbound
  , TraceObjectDiffusionOutbound (..)
  , ObjectDiffusionProtocolError (..)
  ) where

import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer, traceWith)
import Data.Foldable (find)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as Seq
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPoolReader
import Ouroboros.Network.ControlMessage
  ( ControlMessage
  , ControlMessageSTM
  , timeoutWithControlMessage
  )
import Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion)
import Ouroboros.Network.Protocol.ObjectDiffusion.Client
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

data ObjectDiffusionProtocolError
  = ProtocolErrorAckedTooManyObjectIds
  | ProtocolErrorRequestedNothing
  | ProtocolErrorRequestedTooManyObjectIds NumObjectIdsToReq NumObjectIdsToAck
  | ProtocolErrorRequestBlocking
  | ProtocolErrorRequestNonBlocking
  | ProtocolErrorRequestedUnavailableObject
  deriving Show

instance Exception ObjectDiffusionProtocolError where
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
  forall objectId object index m.
  (Ord objectId, Ord index, MonadSTM m, MonadThrow m) =>
  Tracer m (TraceObjectDiffusionOutbound objectId object) ->
  -- | Maximum number of unacknowledged objectIds allowed
  NumObjectIdsToAck ->
  ObjectPoolReader objectId object index m ->
  NodeToNodeVersion ->
  ControlMessageSTM m ->
  ObjectDiffusionClient objectId object m ()
objectDiffusionOutbound tracer maxUnacked ObjectPoolReader{..} _version controlMessageSTM =
  ObjectDiffusionClient (pure (client Seq.empty objectPoolZeroIndex))
 where
  client :: StrictSeq (objectId, index) -> index -> ClientStIdle objectId object m ()
  client !unackedSeq !lastIndex =
    ClientStIdle{recvMsgRequestObjectIds, recvMsgRequestObjects}
   where
    recvMsgRequestObjectIds ::
      forall blocking.
      SingBlockingStyle blocking ->
      NumObjectIdsToAck ->
      NumObjectIdsToReq ->
      m (ClientStObjectIds blocking objectId object m ())
    recvMsgRequestObjectIds blocking ackNo reqNo = do
      when (getNumObjectIdsToAck ackNo > fromIntegral (Seq.length unackedSeq)) $
        throwIO ProtocolErrorAckedTooManyObjectIds

      when
        ( fromIntegral (Seq.length unackedSeq)
            - getNumObjectIdsToAck ackNo
            + getNumObjectIdsToReq reqNo
            > getNumObjectIdsToAck maxUnacked
        )
        $ throwIO (ProtocolErrorRequestedTooManyObjectIds reqNo maxUnacked)

      -- Update our tracking state to remove the number of objectIds that the
      -- peer has acknowledged.
      let !unackedSeq' = Seq.drop (fromIntegral ackNo) unackedSeq

      -- Update our tracking state with any extra objects available.
      let update objects =
            -- These objects should all be fresh
            assert (all (\(_, index, _) -> index > lastIndex) objects) $
              let !unackedSeq'' =
                    unackedSeq'
                      <> Seq.fromList
                        [(objectId, index) | (objectId, index, _) <- objects]
                  !lastIndex'
                    | null objects = lastIndex
                    | otherwise = index
                   where
                    (_, index, _) = last objects
                  objects' :: [(objectId, SizeInBytes)]
                  objects' = [(objectId, size) | (objectId, _, size) <- objects]
                  client' = client unackedSeq'' lastIndex'
               in (objects', client')

      -- Grab info about any new objects after the last object index we've seen,
      -- up to the number that the peer has requested.
      case blocking of
        SingBlocking -> do
          when (reqNo == 0) $
            throwIO ProtocolErrorRequestedNothing
          unless (Seq.null unackedSeq') $
            throwIO ProtocolErrorRequestBlocking

          mbobjects <- timeoutWithControlMessage controlMessageSTM $
            do
              ObjectPoolSnapshot{objectPoolObjectIdsAfter} <- objectPoolGetSnapshot
              let objects = objectPoolObjectIdsAfter lastIndex
              check (not $ null objects)
              pure (take (fromIntegral reqNo) objects)

          case mbobjects of
            Nothing -> pure (SendMsgDone ())
            Just objects ->
              let !(objects', client') = update objects
                  objects'' = case NonEmpty.nonEmpty objects' of
                    Just x -> x
                    -- Assert objects is non-empty: we blocked until objects was non-null,
                    -- and we know reqNo > 0, hence `take reqNo objects` is non-null.
                    Nothing -> error "objectDiffusionOutbound: empty transaction's list"
               in pure (SendMsgReplyObjectIds (BlockingReply objects'') client')
        SingNonBlocking -> do
          when (reqNo == 0 && ackNo == 0) $
            throwIO ProtocolErrorRequestedNothing
          when (Seq.null unackedSeq') $
            throwIO ProtocolErrorRequestNonBlocking

          objects <- atomically $ do
            ObjectPoolSnapshot{objectPoolObjectIdsAfter} <- objectPoolGetSnapshot
            let objects = objectPoolObjectIdsAfter lastIndex
            return (take (fromIntegral reqNo) objects)

          let !(objects', client') = update objects
          pure (SendMsgReplyObjectIds (NonBlockingReply objects') client')

    recvMsgRequestObjects ::
      [objectId] ->
      m (ClientStObjects objectId object m ())
    recvMsgRequestObjects objectIds = do
      -- Trace the IDs of the transactions requested.
      traceWith tracer (TraceObjectDiffusionOutboundRecvMsgRequestObjects objectIds)

      ObjectPoolSnapshot{objectPoolLookupObject} <- atomically objectPoolGetSnapshot

      -- The window size is expected to be small (currently 10) so the find is acceptable.
      let objectIndices = [find (\(t, _) -> t == objectId) unackedSeq | objectId <- objectIds]
          objectIndices' = map snd $ catMaybes objectIndices

      when (any isNothing objectIndices) $
        throwIO ProtocolErrorRequestedUnavailableObject

      -- The 'objectPoolLookupObject' will return nothing if the transaction is no
      -- longer in the objectPool. This is good. Neither the sending nor
      -- receiving side wants to forward objects that are no longer of interest.
      let objects = mapMaybe objectPoolLookupObject objectIndices'
          client' = client unackedSeq lastIndex

      -- Trace the transactions to be sent in the response.
      traceWith tracer (TraceObjectDiffusionOutboundSendMsgReplyObjects objects)

      return $ SendMsgReplyObjects objects client'
