{-# LANGUAGE BlockArguments #-}
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
  , withObjectDiffusionInboundPeer
  , PeerStateAPI

    -- * Supporting types
  , module V2
  , PeerDecisionChannelsVar
  , newPeerDecisionChannelsVar
  , DecisionPolicy (..)
  ) where

import Control.Concurrent.Class.MonadSTM (MonadSTM, atomically)
import Control.Monad.Class.MonadThrow
import Control.Tracer (Tracer, traceWith)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Network.TypedProtocol
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Registry
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types as V2
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)
import Ouroboros.Network.Protocol.ObjectDiffusion.Inbound

-- | A object-diffusion inbound side (client).
--
-- The steps are as follow
-- 1. Block on next decision from the decision logic
-- 2. Handle any available reply (`goCollect`)
-- 3. Request new objects if possible (`goReqObjects`)
-- 4. Request new ids (also responsible for ack) (`goReqIds`)
-- 5. Signal psaOnDecisionCompleted (as part of `goReqIds{Blocking,NonBlocking}`)
-- And loop again
--
-- The architecture/code org of this module should make sure we don't go again
-- into `goIdle` until `psaOnDecisionCompleted` has been called
--
-- NOTE: each `go____` function is responsible for calling the next one in order
-- to continue the protocol.
-- E.g. `goReqObjects` will call `goReqIds` whatever the outcome of its logic is.
objectDiffusionInbound ::
  forall objectId object m.
  ( MonadThrow m
  , MonadSTM m
  ) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  ControlMessageSTM m ->
  PeerStateAPI m objectId object ->
  ObjectDiffusionInboundPipelined objectId object m ()
objectDiffusionInbound
  tracer
  controlMessageSTM
  PeerStateAPI
    { psaReadDecision
    , psaOnDecisionCompleted
    , psaOnRequestIds
    , psaOnRequestObjects
    , psaOnReceiveIds
    , psaOnReceiveObjects
    } =
    ObjectDiffusionInboundPipelined $ goIdle Zero
   where
    goIdle :: forall (n :: N). Nat n -> InboundStIdle n objectId object m ()
    goIdle n = WithEffect $ do
      ctrlMsg <- atomically controlMessageSTM
      traceWith tracer $ TraceObjectDiffusionInboundReceivedControlMessage ctrlMsg
      case ctrlMsg of
        -- The peer selection governor is asking us to terminate the connection.
        Terminate ->
          pure $ terminateAfterDrain n
        -- Otherwise, we can continue the protocol normally.
        _continue -> do
          -- Block on next decision.
          decision <- psaReadDecision
          traceWith tracer (TraceObjectDiffusionInboundReceivedDecision decision)
          pure $ goCollect n decision

    -- \| Block until all replies of pipelined requests have been received, then
    -- sends `MsgDone` to terminate the protocol.
    terminateAfterDrain ::
      Nat n -> InboundStIdle n objectId object m ()
    terminateAfterDrain = \case
      Zero -> WithEffect $ do
        traceWith tracer TraceObjectDiffusionInboundTerminated
        pure $ SendMsgDone ()
      Succ n -> CollectPipelined Nothing $ \_ignoredMsg -> terminateAfterDrain n

    -- \| Handle potential available replies before continuing with `goReqObjects`.
    --
    -- If there are no pipelined requests, this will directly call `goReqObjects`.
    -- If there are pipelined requests, it will collect as many replies as
    -- possible before continuing with `goReqObjects` once no more replies are
    -- immediately available.
    goCollect :: Nat n -> PeerDecision objectId object -> InboundStIdle n objectId object m ()
    goCollect Zero decision =
      goReqObjects Zero decision
    goCollect (Succ n) decision =
      CollectPipelined
        (Just $ goReqObjects (Succ n) decision)
        ( \case
            CollectObjectIds numIdsRequested ids -> WithEffect $ do
              psaOnReceiveIds numIdsRequested ids
              pure $ goCollect n decision
            CollectObjects _objectIds objects -> WithEffect $ do
              -- TODO: We could try to validate objects here, i.e.
              -- as early as possible, instead of validating them when adding
              -- them to the ObjectPool, in order to pivot away from
              -- adversarial peers as soon as possible.
              psaOnReceiveObjects objects
              pure $ goCollect n decision
        )

    -- \| Request objects, if the set of ids of objects to request in the
    -- decision is non-empty.
    -- Regardless, it will ultimately call `goReqIds`.
    goReqObjects ::
      Nat n ->
      PeerDecision objectId object ->
      InboundStIdle n objectId object m ()
    goReqObjects n decision = do
      let objectIds = rodObjectsToReqIds (pdReqObjects decision)
      if Set.null objectIds
        then
          goReqIds n decision
        else WithEffect $ do
          psaOnRequestObjects objectIds
          pure $
            SendMsgRequestObjectsPipelined
              (Set.toList objectIds)
              (goReqIds (Succ n) decision)

    -- \| Request objectIds, either in a blocking or pipelined fashion depending
    -- on the decision's `ridCanPipelineIdsRequests` flag.
    -- In both cases, once done, we will ultimately call `psaOnDecisionCompleted`
    -- and return to `goIdle`.
    goReqIds ::
      forall (n :: N).
      Nat n ->
      PeerDecision objectId object ->
      InboundStIdle n objectId object m ()
    goReqIds n decision = do
      let canPipelineIdRequests = ridCanPipelineIdsRequests (pdReqIds decision)
      if canPipelineIdRequests
        then goReqIdsPipelined n decision
        else case n of
          Zero -> goReqIdsBlocking decision
          Succ{} -> error "Impossible to have pipelined requests when we have no known unacknowledged objectIds"

    -- \| Request objectIds in a blocking fashion if the number to request in the
    -- decision is non-zero.
    -- Regardless, it will ultimately call `psaOnDecisionCompleted` and return to
    -- `goIdle`.
    goReqIdsBlocking ::
      PeerDecision objectId object ->
      InboundStIdle Z objectId object m ()
    goReqIdsBlocking decision = WithEffect $ do
      let numIdsToAck = ridNumIdsToAck (pdReqIds decision)
      let numIdsToReq = ridNumIdsToReq (pdReqIds decision)
      if numIdsToReq == 0
        then do
          psaOnDecisionCompleted
          pure $ goIdle Zero
        else do
          psaOnRequestIds numIdsToAck numIdsToReq
          psaOnDecisionCompleted
          pure $
            SendMsgRequestObjectIdsBlocking
              numIdsToAck
              numIdsToReq
              ( \objectIds -> WithEffect $ do
                  psaOnReceiveIds numIdsToReq (NonEmpty.toList objectIds)
                  pure $ goIdle Zero
              )

    -- \| Request objectIds in a pipelined fashion if the number to request in the
    -- decision is non-zero.
    -- Regardless, it will ultimately call `psaOnDecisionCompleted` and return to
    -- `goIdle`.
    goReqIdsPipelined ::
      forall (n :: N).
      Nat n ->
      PeerDecision objectId object ->
      InboundStIdle n objectId object m ()
    goReqIdsPipelined n decision = WithEffect $ do
      let numIdsToAck = ridNumIdsToAck (pdReqIds decision)
      let numIdsToReq = ridNumIdsToReq (pdReqIds decision)
      if numIdsToReq == 0
        then do
          psaOnDecisionCompleted
          pure $ goIdle n
        else do
          psaOnRequestIds numIdsToAck numIdsToReq
          psaOnDecisionCompleted
          pure $
            SendMsgRequestObjectIdsPipelined
              numIdsToAck
              numIdsToReq
              (goIdle (Succ n))
