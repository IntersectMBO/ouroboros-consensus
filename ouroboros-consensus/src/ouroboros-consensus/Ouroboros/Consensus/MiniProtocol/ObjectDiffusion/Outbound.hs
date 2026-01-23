{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines the outbound side of the ObjectDiffusion mini-protocol.
-- This protocol is inspired from TxSubmission and aims to provide a generic
-- way to diffuse objects for Peras, i.e. Peras votes and certificates.
--
-- See the design document here: https://tweag.github.io/cardano-peras/peras-design.pdf
module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Outbound
  ( objectDiffusionOutbound
  , TraceObjectDiffusionOutbound (..)
  , ObjectDiffusionOutboundError (..)
  ) where

import Control.Exception (assert)
import Control.Monad (join, unless, when)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer, traceWith)
import Data.Foldable qualified as Foldable
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as Seq
import Data.Set qualified as Set
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Network.NodeToNode.Version (NodeToNodeVersion)
import Ouroboros.Network.Protocol.ObjectDiffusion.Outbound
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

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
  | -- | There is a mismatch between the result from the in-memory object pool
    -- index and the content we can load so we have to retry the whole monad m
    -- action to load new objects.
    TraceObjectDiffusionOutboundUnableToLoadObjectsThusRetrying
  | -- | Received 'MsgDone'
    TraceObjectDiffusionOutboundTerminated
  deriving Show

data ObjectDiffusionOutboundError
  = ProtocolErrorAckedTooManyObjectIds
  | ProtocolErrorRequestedNothing
  | ProtocolErrorRequestedTooManyObjectIds NumObjectIdsReq NumObjectsOutstanding
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
  (Ord objectId, Ord ticketNo, MonadSTM m, MonadThrow m) =>
  Tracer m (TraceObjectDiffusionOutbound objectId object) ->
  -- | Maximum number of unacknowledged objectIds allowed
  NumObjectsOutstanding ->
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
    Map ticketNo object ->
    OutboundSt objectId object ticketNo
  updateStNewObjects !OutboundSt{..} newObjectsWithTicketNos =
    -- These objects should all be fresh
    assert (all (> lastTicketNo) (Map.keys newObjectsWithTicketNos)) $
      let !outstandingFifo' =
            Foldable.foldl'
              (Seq.|>)
              outstandingFifo
              newObjectsWithTicketNos
          !lastTicketNo' = case Map.lookupMax newObjectsWithTicketNos of
            Nothing -> lastTicketNo
            Just (ticketNo, _) -> ticketNo
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

        -- oprObjectIdsAfter returns early with Nothing if there are no new
        -- objects after lastTicketNo. So we retry efficiently using STM until
        -- we get 'Just'.
        -- But even if we get 'Just', when loading the actual objects (as a
        -- non-STM action, taking place in monad m), we might find that the map
        -- is empty (because objects were deleted from the pool in the meantime).
        -- So we have a outer-level retry loop, this time in monad m.
        --
        -- The only case were the outer-level retry loop would spin forever is
        -- if the in-memory index of the object pool keeps indicating that
        -- there are new objects after 'lastTicketNo', but when we try to load
        -- them, they are not there anymore. This would indicate a bug in the
        -- object pool implementation.
        newObjects <- retryMaybeWith (traceWith tracer TraceObjectDiffusionOutboundUnableToLoadObjectsThusRetrying)
          . join
          . atomically
          $ do
            mLoadNewObjects <- oprObjectsAfter lastTicketNo (fromIntegral numIdsToReq)
            maybe retry (pure . fmap ensureNonEmpty) mLoadNewObjects

        let !newIds = oprObjectId <$> Map.elems newObjects
            st'' = updateStNewObjects st' newObjects

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

        -- oprObjectIdsAfter returns early with Nothing if there are no new
        -- objects after lastTicketNo. So we use fromMaybe to provide an empty map
        -- in that case.
        newObjects <-
          join . atomically . fmap (fromMaybe $ pure Map.empty) $
            oprObjectsAfter lastTicketNo (fromIntegral numIdsToReq)

        let !newIds = oprObjectId <$> Map.elems newObjects
            st'' = updateStNewObjects st' newObjects

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

-- | Retry an action that returns 'Maybe a' until it returns 'Just a'.
-- The 'onRetry' action is executed before each retry.
retryMaybeWith :: Monad m => m () -> m (Maybe a) -> m a
retryMaybeWith onRetry action = loop
 where
  loop = do
    mx <- action
    case mx of
      Just x -> pure x
      Nothing -> onRetry >> loop

-- | Convert an empty map to 'Nothing', and a non-empty map to 'Just' that map.
--
-- TODO: we could alternatively replace this with 'Data.NonEmpty.Map.nonEmptyMap'
ensureNonEmpty :: Map k v -> Maybe (Map k v)
ensureNonEmpty m
  | Map.null m = Nothing
  | otherwise = Just m
