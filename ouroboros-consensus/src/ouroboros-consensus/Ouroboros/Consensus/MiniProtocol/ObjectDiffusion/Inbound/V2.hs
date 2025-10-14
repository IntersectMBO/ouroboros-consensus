{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2
  ( -- * ObjectDiffusion Inbound client
    objectDiffusionInbound

    -- * PeerStateAPI
  , withPeer
  , PeerStateAPI

    -- * Supporting types
  , module V2
  , PeerDecisionChannelsVar
  , newPeerDecisionChannelsVar
  , ObjectPoolSem
  , newObjectPoolSem
  , DecisionGlobalStateVar
  , newDecisionGlobalStateVar
  , DecisionPolicy (..)
  , defaultDecisionPolicy
  ) where

import Control.Concurrent.Class.MonadSTM (atomically)
import Control.Exception (assert)
import Control.Monad (unless, when)
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Network.TypedProtocol
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Policy
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Registry
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.State qualified as State
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types as V2
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound

-- | A object-submission inbound side (client).
--
-- The goIdle' function blocks on receiving `PeerDecision` from the decision logic.
objectDiffusionInbound ::
  forall objectId object ticketNo m.
  ( MonadDelay m
  , MonadThrow m
  , Ord objectId
  ) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  ObjectDiffusionInitDelay ->
  ControlMessageSTM m ->
  PeerStateAPI m objectId object ->
  ObjectDiffusionInboundPipelined objectId object m ()
objectDiffusionInbound
  tracer
  initDelay
  controlMessageSTM
  PeerStateAPI
    { psaReadDecision
    , psaOnRequestIds
    , psaOnRequestObjects
    , psaOnReceivedIds
    , psaOnReceivedObjects
    } =
    ObjectDiffusionInboundPipelined $ do
      case initDelay of
        ObjectDiffusionInitDelay delay -> threadDelay delay
        NoObjectDiffusionInitDelay -> return ()
      (goIdle Zero)
   where
    goIdle ::
      forall (n :: N).
      Nat n ->
      m (InboundStIdle Z objectId object m ())
    goIdle n = do
      ctrlMsg <- atomically controlMessageSTM
      traceWith tracer $ TraceObjectDiffusionInboundReceivedControlMessage ctrlMsg
      case ctrlMsg of
        -- The peer selection governor is asking us to terminate the connection.
        Terminate ->
          pure $ terminateAfterDrain n
        -- Otherwise, we can continue the protocol normally.
        _continue -> goIdle'
    goIdle' ::
      forall (n :: N).
      Nat n ->
      m (InboundStIdle Z objectId object m ())
    goIdle' n = do
      -- Block on next decision.
      decision@PeerDecision
        { pdNumIdsToAck
        , pdNumIdsToReq
        , pdObjectsToReqIds
        , pdCanPipelineIdsRequests
        } <-
        psaReadDecision
      traceWith tracer (TraceObjectDiffusionInboundReceivedDecision decision)

      when (not StrictSeq.null pdObjectsToSubmitToPoolIds) $ do
        psaSubmitObjectsToPool pdObjectsToSubmitToPoolIds

      let shouldRequestMoreObjects = not $ Set.null pdObjectsToReqIds

      case n of
        -- We didn't pipeline any requests, so there are no replies in flight
        -- (nothing to collect)
        Zero ->
          if shouldRequestMoreObjects
            then do
              -- There are no replies in flight, but we do know some more objects
              -- we can ask for, so lets ask for them and more objectIds in a
              -- pipelined way.
              traceWith tracer (TraceObjectDiffusionInboundCanRequestMoreObjects (natToInt n))
              goReqObjectsAndIdsPipelined Zero decision
            else do
              -- There's no replies in flight, and we have no more objects we can
              -- ask for so the only remaining thing to do is to ask for more
              -- objectIds. Since this is the only thing to do now, we make this a
              -- blocking call.
              traceWith tracer (TraceObjectDiffusionInboundCannotRequestMoreObjects (natToInt n))
              goReqIdsBlocking decision

        -- We have pipelined some requests, so there are some replies in flight.
        n@(Succ _) ->
          if shouldRequestMoreObjects
            then do
              -- We have replies in flight and we should eagerly collect them if
              -- available, but there are objects to request too so we
              -- should *not* block waiting for replies.
              -- So we ask for new objects and objectIds in a pipelined way.
              pure $
                CollectPipelined
                  -- if no replies are available immediately, we continue with
                  (Just (goReqObjectsIdsPipelined n decision))
                  -- if one reply is available, we go collect it.
                  -- We will continue to goIdle after; so in practice we will loop
                  -- until all immediately available replies have been collected
                  -- before requesting objects and ids in a pipelined fashion
                  (goCollect n decision)
            else do undefined

      if Set.null pdObjectsToReqIds
        then goReqObjectIds Zero undefined
        else goReqObjects undefined

    -- Pipelined request of objects
    goReqObjects ::
      PeerDecision objectId object ->
      m (InboundStIdle Z objectId object m ())
    goReqObjects object@PeerDecision{pdObjectsToReqIds = pdObjectsToReqIds} =
      pure $
        SendMsgRequestObjectsPipelined
          (Set.toList pdObjectsToReqIds)
          (goReqObjectIds (Succ Zero) object)

    goReqObjectIds ::
      forall (n :: N).
      Nat n ->
      PeerDecision objectId object ->
      m (InboundStIdle n objectId object m ())
    goReqObjectIds
      n
      PeerDecision{pdNumIdsToReq = 0} =
        case n of
          Zero -> goIdle
          Succ _ -> handleReplies n
    goReqObjectIds
      -- if there are no unacknowledged objectIds, the protocol requires sending
      -- a blocking `MsgRequestObjectIds` request.  This is important, as otherwise
      -- the client side wouldn't have a chance to terminate the
      -- mini-protocol.
      Zero
      PeerDecision
        { pdNumIdsToAck = objectIdsToAck
        , pdCanPipelineIdsRequests = False
        , pdNumIdsToReq = objectIdsToReq
        } =
        pure $
          SendMsgRequestObjectIdsBlocking
            objectIdsToAck
            objectIdsToReq
            -- Our result if the client terminates the protocol
            -- (traceWith tracer TraceObjectDiffusionInboundTerminated)
            ( \objectIds -> do
                let objectIds' = NonEmpty.toList objectIds
                    receivedIdsSeq = StrictSeq.fromList $ fst <$> objectIds'
                    objectIdsMap = Map.fromList objectIds'
                when (StrictSeq.length receivedIdsSeq > fromIntegral objectIdsToReq) $
                  throwIO ProtocolErrorObjectIdsNotRequested
                onReceiveIds objectIdsToReq receivedIdsSeq objectIdsMap
                goIdle
            )
    goReqObjectIds
      n@Zero
      PeerDecision
        { pdNumIdsToAck = objectIdsToAck
        , pdCanPipelineIdsRequests = True
        , pdNumIdsToReq = objectIdsToReq
        } =
        pure $
          SendMsgRequestObjectIdsPipelined
            objectIdsToAck
            objectIdsToReq
            (handleReplies (Succ n))
    goReqObjectIds
      n@Succ{}
      PeerDecision
        { pdNumIdsToAck = objectIdsToAck
        , pdCanPipelineIdsRequests
        , pdNumIdsToReq = objectIdsToReq
        } =
        -- it is impossible that we have had `object`'s to request (Succ{} - is an
        -- evidence for that), but no unacknowledged `objectId`s.
        assert pdCanPipelineIdsRequests $
          pure $
            SendMsgRequestObjectIdsPipelined
              objectIdsToAck
              objectIdsToReq
              (handleReplies (Succ n))

    handleReplies ::
      forall (n :: N).
      Nat (S n) ->
      m (InboundStIdle (S n) objectId object m ())
    handleReplies (Succ n'@Succ{}) =
      pure $
        CollectPipelined
          Nothing
          (handleReply (handleReplies n'))
    handleReplies (Succ Zero) =
      pure $
        CollectPipelined
          Nothing
          (handleReply goIdle)

    handleReply ::
      forall (n :: N).
      m (InboundStIdle n objectId object m ()) ->
      -- continuation
      Collect objectId object ->
      m (InboundStIdle n objectId object m ())
    handleReply k = \case
      CollectObjectIds objectIdsToReq objectIds -> do
        let receivedIdsSeq = StrictSeq.fromList $ fst <$> objectIds
            objectIdsMap = Map.fromList objectIds
        unless (StrictSeq.length receivedIdsSeq <= fromIntegral objectIdsToReq) $
          throwIO ProtocolErrorObjectIdsNotRequested
        onReceiveIds objectIdsToReq receivedIdsSeq objectIdsMap
        k
      CollectObjects objectIds objects -> do
        let requested = Map.keysSet objectIds
            received = Map.fromList undefined

        unless (Map.keysSet received `Set.isSubsetOf` requested) $
          throwIO ProtocolErrorObjectNotRequested

        mbe <- onReceiveObjects objectIds received
        traceWith tracer $ TraceObjectDiffusionCollected (getId `map` objects)
        case mbe of
          -- one of `object`s had a wrong size
          Just e ->
            traceWith tracer (TraceObjectInboundError e)
              >> throwIO e
          Nothing -> k
