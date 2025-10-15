{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments #-}

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
import Network.TypedProtocol.Peer
import Ouroboros.Network.Protocol.ObjectDiffusion.Type

-- | A object-diffusion inbound side (client).
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
    , psaOnDecisionExecuted
    , psaOnRequestIds
    , psaOnRequestObjects
    , psaOnReceivedIds
    , psaOnReceivedObjects
    } =
    ObjectDiffusionInboundPipelined $ do
      -- TODO: delete initDelay
      case initDelay of
        ObjectDiffusionInitDelay delay -> threadDelay delay
        NoObjectDiffusionInitDelay -> return ()
      (goIdle Zero)
   where

    terminateAfterDrain ::
      Nat n -> InboundStIdle n objectId object m ()
    terminateAfterDrain = \case
      Zero -> SendMsgDone (pure ())
      Succ n -> CollectPipelined Nothing $ \_ignoredMsg -> pure $ terminateAfterDrain n

    -- Wrapper around goIdle' that handles termination on reception of 
    -- Terminate control message.
    goIdle :: forall (n :: N). Nat n -> m (InboundStIdle n objectId object m ())
    goIdle n = do
      ctrlMsg <- atomically controlMessageSTM
      traceWith tracer $ TraceObjectDiffusionInboundReceivedControlMessage ctrlMsg
      case ctrlMsg of
        -- The peer selection governor is asking us to terminate the connection.
        Terminate ->
          pure $ terminateAfterDrain n
        -- Otherwise, we can continue the protocol normally.
        _continue -> goIdle' n

    goIdle' :: forall (n :: N). Nat n -> m (InboundStIdle n objectId object m ())
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

      -- We need to make sure we don't go again into `goIdle` until `psaOnDecisionExecuted` has been called
      case n of
        Zero -> goReqObjectsAndIds Zero decision
        n@Succ{} -> pure $
          CollectPipelined
            (Just (goReqObjectsAndIds n decision))
            (\collectResult -> undefined) -- loopUntilAllCollected; goReqObjectsAndIds n)

    goReqObjectsAndIds ::
      Nat n ->
      PeerDecision objectId object ->
      m (InboundStIdle n objectId object m ())
    goReqObjectsAndIds n object@PeerDecision{pdObjectsToReqIds} =
      if Set.null pdObjectsToReqIds
      then
        goReqIds n object
      else do
        psaOnRequestObjects pdObjectsToReqIds
        pure $ SendMsgRequestObjectsPipelined
          (Set.toList pdObjectsToReqIds)
          (goReqIds (Succ n) object)

    goReqIds ::
      forall (n :: N).
      Nat n ->
      PeerDecision objectId object ->
      m (InboundStIdle n objectId object m ())
    goReqIds n pd@PeerDecision{pdCanPipelineIdsRequests} =
      if pdCanPipelineIdsRequests
        then goReqIdsPipelined n pd
        else case n of
          Zero -> goReqIdsBlocking pd
          Succ{} -> error "Impossible to have pipelined requests when we have no known unacknowledged objectIds"

    goReqIdsBlocking ::
      PeerDecision objectId object ->
      m (InboundStIdle Z objectId object m ())
    goReqIdsBlocking PeerDecision{pdNumIdsToAck, pdNumIdsToReq} =
      if pdNumIdsToReq == 0
        then do
          psaOnDecisionExecuted
          goIdle Zero
        else do
          psaOnRequestIds pdNumIdsToAck pdNumIdsToReq
          psaOnDecisionExecuted
          pure $ SendMsgRequestObjectIdsBlocking
                pdNumIdsToAck
                pdNumIdsToReq
                ( \objectIds -> do
                  psaOnReceivedIds pdNumIdsToReq (NonEmpty.toList objectIds)
                  goIdle Zero
                )

    goReqIdsPipelined ::
      forall (n :: N).
      Nat n ->
      PeerDecision objectId object ->
      m (InboundStIdle n objectId object m ())
    goReqIdsPipelined n PeerDecision{pdNumIdsToAck, pdNumIdsToReq} =
      if pdNumIdsToReq == 0
        then do
          psaOnDecisionExecuted
          goIdle n
        else do
          psaOnRequestIds pdNumIdsToAck pdNumIdsToReq
          psaOnDecisionExecuted
          pure $ SendMsgRequestObjectIdsPipelined
                pdNumIdsToAck
                pdNumIdsToReq
                (goIdle (Succ n))

    goCollectIds :: Nat n -> NumObjectIdsReq -> [objectId] -> m (InboundStIdle n objectId object m ())
    goCollectIds n numIdsRequested ids = do
      psaOnReceivedIds numIdsRequested ids
      undefined

    ---------------------------------------------------------------------------
    -- OLD STUFF FOR REFERENCE BELOW
    ---------------------------------------------------------------------------

    goReqIds'
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
    goReqIds'
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
    goReqIds'
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
