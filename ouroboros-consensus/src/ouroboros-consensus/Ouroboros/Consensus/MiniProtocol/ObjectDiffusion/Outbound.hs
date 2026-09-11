{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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
import Control.Monad.Class.MonadSTM.Internal qualified as TVar
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI (DiffTime)
import Control.Monad.Class.MonadTimer.SI (MonadTimer, registerDelay)
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
  | -- | No IDs are immediately available, so the server will wait.
    TraceObjectDiffusionOutboundSendMsgAwaitReply
  | -- | No IDs became available before the bounded blocking wait expired.
    TraceObjectDiffusionOutboundSendMsgServerIdle
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
  (Ord objectId, MonadThrow m, MonadTimer m) =>
  Tracer m (TraceObjectDiffusionOutbound objectId object) ->
  -- | Maximum number of unacknowledged objectIds allowed
  NumObjectsUnacknowledged ->
  -- | Maximum time a blocking request waits before returning client agency.
  DiffTime ->
  ObjectPoolReader objectId object ticketNo m ->
  NodeToNodeVersion ->
  ObjectDiffusionOutbound objectId object m ()
objectDiffusionOutbound tracer maxFifoLength idleTimeout ObjectPoolReader{..} _version =
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
    forall kind.
    OutboundSt objectId object ticketNo ->
    ObjectIdsRequestKind kind ->
    NumObjectIdsAck ->
    NumObjectIdsReq ->
    m (OutboundStObjectIds kind objectId object m ())
  recvMsgRequestObjectIds !st@OutboundSt{..} requestKind numIdsToAck numIdsToReq = do
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
    case requestKind of
      -----------------------------------------------------------------------
      RequestObjectIdsBlocking -> do
        when (numIdsToReq == 0) $
          throwIO ProtocolErrorRequestedNothing
        unless (Seq.null outstandingFifo') $
          throwIO ProtocolErrorRequestBlocking

        let sendNewContent ::
              forall phase.
              Map.Map ticketNo object ->
              m (OutboundStObjectIds ('StObjectIdsBlocking phase) objectId object m ())
            sendNewContent newContent = do
              let sortedNewContent = Map.toAscList newContent
                  !newIds = oprObjectId . snd <$> sortedNewContent
                  st'' = updateStNewObjects st' sortedNewContent

              traceWith tracer (TraceObjectDiffusionOutboundSendMsgReplyObjectIds newIds)

              pure $
                SendMsgReplyObjectIds
                  (BlockingReply (NonEmpty.fromList $ newIds))
                  (makeBundle st'')

            -- After 'MsgAwaitReply' has been sent, wait for either new objects
            -- or the idle timeout. Check the timer first so a pool reader that
            -- repeatedly yields stale, garbage-collected actions cannot starve
            -- the timeout. Reuse the same timer when retrying such actions.
            waitForNewContentOrIdle ::
              m (OutboundStObjectIds ('StObjectIdsBlocking 'StMustReply) objectId object m ())
            waitForNewContentOrIdle = do
              idleVar <- registerDelay idleTimeout
              let getNewContentOrIdle = do
                    result <-
                      atomically $
                        (TVar.readTVar idleVar >>= check >> pure Nothing)
                          `orElse` do
                            maybeNewObjectsAction <-
                              oprObjectsAfter
                                lastTicketNo
                                (fromIntegral numIdsToReq)
                            case maybeNewObjectsAction of
                              Nothing -> retry
                              Just newObjectsAction -> pure (Just newObjectsAction)
                    case result of
                      Nothing -> pure Nothing
                      Just getNewObjects -> do
                        content <- getNewObjects
                        if null content
                          then getNewContentOrIdle
                          else pure (Just content)

              maybeNewContent <- getNewContentOrIdle
              case maybeNewContent of
                Nothing -> do
                  traceWith tracer TraceObjectDiffusionOutboundSendMsgServerIdle
                  pure $ SendMsgServerIdle (makeBundle st')
                Just newContent -> sendNewContent newContent

            sendAwaitReply ::
              m (OutboundStObjectIds ('StObjectIdsBlocking 'StCanAwait) objectId object m ())
            sendAwaitReply = do
              traceWith tracer TraceObjectDiffusionOutboundSendMsgAwaitReply
              pure $ SendMsgAwaitReply waitForNewContentOrIdle

        -- Check once without blocking so that the caught-up observation is
        -- prompt. If the advertised objects disappear before the IO action is
        -- run, report 'MsgAwaitReply' rather than blocking before that message.
        maybeNewObjectsAction <-
          atomically $
            oprObjectsAfter
              lastTicketNo
              (fromIntegral numIdsToReq)
        case maybeNewObjectsAction of
          Nothing -> sendAwaitReply
          Just getNewObjects -> do
            newContent <- getNewObjects
            if null newContent
              then sendAwaitReply
              else sendNewContent newContent

      -----------------------------------------------------------------------
      RequestObjectIdsNonBlocking -> do
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
