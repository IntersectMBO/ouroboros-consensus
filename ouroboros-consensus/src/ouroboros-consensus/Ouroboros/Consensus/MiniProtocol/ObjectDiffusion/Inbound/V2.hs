{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2
  ( -- * ObjectDiffusion Inbound client
    objectSubmissionInboundV2

    -- * PeerObjectAPI
  , withPeer
  , PeerObjectAPI

    -- * Supporting types
  , module V2
  , ObjectChannelsVar
  , newObjectChannelsVar
  , ObjectObjectPoolSem
  , newObjectObjectPoolSem
  , SharedObjectStateVar
  , newSharedObjectStateVar
  , ObjectDecisionPolicy (..)
  , defaultObjectDecisionPolicy
  ) where

import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Network.TypedProtocol
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Policy
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Registry
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.State
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types as V2
import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound

-- | A object-submission inbound side (server, sic!).
--
-- The server blocks on receiving `ObjectDecision` from the decision logic. If
-- there are object's to download it pipelines two requests: first for object's second
-- for objectId's. If there are no object's to download, it either sends a blocking or
-- non-blocking request for objectId's.
objectSubmissionInboundV2 ::
  forall objectId object idx m.
  ( MonadDelay m
  , MonadThrow m
  , Ord objectId
  ) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  ObjectDiffusionInitDelay ->
  ObjectDiffusionObjectPoolWriter objectId object idx m ->
  PeerObjectAPI m objectId object ->
  ObjectDiffusionServerPipelined objectId object m ()
objectSubmissionInboundV2
  tracer
  initDelay
  ObjectDiffusionObjectPoolWriter{objectId}
  PeerObjectAPI
    { readObjectDecision
    , handleReceivedObjectIds
    , handleReceivedObjects
    , submitObjectToObjectPool
    } =
    ObjectDiffusionServerPipelined $ do
      case initDelay of
        ObjectDiffusionInitDelay delay -> threadDelay delay
        NoObjectDiffusionInitDelay -> return ()
      serverIdle
   where
    serverIdle ::
      m (ServerStIdle Z objectId object m ())
    serverIdle = do
      -- Block on next decision.
      object@ObjectDecision
        { objectsToRequest = objectsToRequest
        , objectsToObjectPool = ObjectsToObjectPool{listOfObjectsToObjectPool}
        } <-
        readObjectDecision
      traceWith tracer (TraceObjectInboundDecision object)

      let !collected = length listOfObjectsToObjectPool

      -- Only attempt to add OBJECTs if we have some work to do
      when (collected > 0) $ do
        -- submitObjectToObjectPool traces:
        -- \* `TraceObjectDiffusionProcessed`,
        -- \* `TraceObjectInboundAddedToObjectPool`, and
        -- \* `TraceObjectInboundRejectedFromObjectPool`
        -- events.
        mapM_ (uncurry $ submitObjectToObjectPool tracer) listOfObjectsToObjectPool

      -- TODO:
      -- We can update the state so that other `object-submission` servers will
      -- not try to add these objects to the objectpool.
      if Map.null objectsToRequest
        then serverReqObjectIds Zero object
        else serverReqObjects object

    -- Pipelined request of objects
    serverReqObjects ::
      ObjectDecision objectId object ->
      m (ServerStIdle Z objectId object m ())
    serverReqObjects object@ObjectDecision{objectsToRequest = objectsToRequest} =
      pure $
        SendMsgRequestObjectsPipelined
          objectsToRequest
          (serverReqObjectIds (Succ Zero) object)

    serverReqObjectIds ::
      forall (n :: N).
      Nat n ->
      ObjectDecision objectId object ->
      m (ServerStIdle n objectId object m ())
    serverReqObjectIds
      n
      ObjectDecision{objectIdsToRequest = 0} =
        case n of
          Zero -> serverIdle
          Succ _ -> handleReplies n
    serverReqObjectIds
      -- if there are no unacknowledged objectIds, the protocol requires sending
      -- a blocking `MsgRequestObjectIds` request.  This is important, as otherwise
      -- the client side wouldn't have a chance to terminate the
      -- mini-protocol.
      Zero
      ObjectDecision
        { objectIdsToAcknowledge = objectIdsToAck
        , objectPipelineObjectIds = False
        , objectIdsToRequest = objectIdsToReq
        } =
        pure $
          SendMsgRequestObjectIdsBlocking
            objectIdsToAck
            objectIdsToReq
            -- Our result if the client terminates the protocol
            (traceWith tracer TraceObjectInboundTerminated)
            ( \objectIds -> do
                let objectIds' = NonEmpty.toList objectIds
                    objectIdsSeq = StrictSeq.fromList $ fst <$> objectIds'
                    objectIdsMap = Map.fromList objectIds'
                unless (StrictSeq.length objectIdsSeq <= fromIntegral objectIdsToReq) $
                  throwIO ProtocolErrorObjectIdsNotRequested
                handleReceivedObjectIds objectIdsToReq objectIdsSeq objectIdsMap
                serverIdle
            )
    serverReqObjectIds
      n@Zero
      ObjectDecision
        { objectIdsToAcknowledge = objectIdsToAck
        , objectPipelineObjectIds = True
        , objectIdsToRequest = objectIdsToReq
        } =
        pure $
          SendMsgRequestObjectIdsPipelined
            objectIdsToAck
            objectIdsToReq
            (handleReplies (Succ n))
    serverReqObjectIds
      n@Succ{}
      ObjectDecision
        { objectIdsToAcknowledge = objectIdsToAck
        , objectPipelineObjectIds
        , objectIdsToRequest = objectIdsToReq
        } =
        -- it is impossible that we have had `object`'s to request (Succ{} - is an
        -- evidence for that), but no unacknowledged `objectId`s.
        assert objectPipelineObjectIds $
          pure $
            SendMsgRequestObjectIdsPipelined
              objectIdsToAck
              objectIdsToReq
              (handleReplies (Succ n))

    handleReplies ::
      forall (n :: N).
      Nat (S n) ->
      m (ServerStIdle (S n) objectId object m ())
    handleReplies (Succ n'@Succ{}) =
      pure $
        CollectPipelined
          Nothing
          (handleReply (handleReplies n'))
    handleReplies (Succ Zero) =
      pure $
        CollectPipelined
          Nothing
          (handleReply serverIdle)

    handleReply ::
      forall (n :: N).
      m (ServerStIdle n objectId object m ()) ->
      -- continuation
      Collect objectId object ->
      m (ServerStIdle n objectId object m ())
    handleReply k = \case
      CollectObjectIds objectIdsToReq objectIds -> do
        let objectIdsSeq = StrictSeq.fromList $ fst <$> objectIds
            objectIdsMap = Map.fromList objectIds
        unless (StrictSeq.length objectIdsSeq <= fromIntegral objectIdsToReq) $
          throwIO ProtocolErrorObjectIdsNotRequested
        handleReceivedObjectIds objectIdsToReq objectIdsSeq objectIdsMap
        k
      CollectObjects objectIds objects -> do
        let requested = Map.keysSet objectIds
            received = Map.fromList [(objectId object, object) | object <- objects]

        unless (Map.keysSet received `Set.isSubsetOf` requested) $
          throwIO ProtocolErrorObjectNotRequested

        mbe <- handleReceivedObjects objectIds received
        traceWith tracer $ TraceObjectDiffusionCollected (objectId `map` objects)
        case mbe of
          -- one of `object`s had a wrong size
          Just e ->
            traceWith tracer (TraceObjectInboundError e)
              >> throwIO e
          Nothing -> k
