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
  , withPeer
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

-- TODO: Add checks and validation

-- | A object-diffusion inbound side (client).
--
-- The steps are as follow
-- 1. Block on next decision from the decision logic
-- 2. Handle any available reply (`goCollect`)
-- 3. Request new objects if possible (`goReqObjects`)
-- 4. Request new ids (also responsible for ack) (`goReqIds`)
-- 5. signal psaOnDecisionCompleted (as part of `goReqIds{Blocking,NonBlocking}`)
-- And loop again
-- We need to make sure we don't go again into `goIdle` until `psaOnDecisionCompleted` has been called
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

    terminateAfterDrain ::
      Nat n -> InboundStIdle n objectId object m ()
    terminateAfterDrain = \case
      Zero -> SendMsgDone ()
      Succ n -> CollectPipelined Nothing $ \_ignoredMsg -> terminateAfterDrain n

    goCollect :: Nat n -> PeerDecision objectId object -> InboundStIdle n objectId object m ()
    goCollect Zero decision =
      goReqObjects Zero decision
    goCollect (Succ n) decision =
      CollectPipelined
        (Just $ goReqObjects (Succ n) decision)
        ( \case
            CollectObjectIds numIdsRequested ids -> WithEffect $ do
              -- TODO: Add checks and validation
              psaOnReceiveIds numIdsRequested ids
              pure $ goCollect n decision
            CollectObjects _objectIds objects -> WithEffect $ do
              -- TODO: Add checks and validation
              psaOnReceiveObjects objects
              pure $ goCollect n decision
        )

    goReqObjects ::
      Nat n ->
      PeerDecision objectId object ->
      InboundStIdle n objectId object m ()
    goReqObjects n object@PeerDecision{pdObjectsToReqIds} =
      if Set.null pdObjectsToReqIds
        then
          goReqIds n object
        else WithEffect $ do
          psaOnRequestObjects pdObjectsToReqIds
          pure $
            SendMsgRequestObjectsPipelined
              (Set.toList pdObjectsToReqIds)
              (goReqIds (Succ n) object)

    goReqIds ::
      forall (n :: N).
      Nat n ->
      PeerDecision objectId object ->
      InboundStIdle n objectId object m ()
    goReqIds n pd@PeerDecision{pdCanPipelineIdsRequests} =
      if pdCanPipelineIdsRequests
        then goReqIdsPipelined n pd
        else case n of
          Zero -> goReqIdsBlocking pd
          Succ{} -> error "Impossible to have pipelined requests when we have no known unacknowledged objectIds"

    goReqIdsBlocking ::
      PeerDecision objectId object ->
      InboundStIdle Z objectId object m ()
    goReqIdsBlocking PeerDecision{pdNumIdsToAck, pdNumIdsToReq} = WithEffect $ do
      if pdNumIdsToReq == 0
        then do
          psaOnDecisionCompleted
          pure $ goIdle Zero
        else do
          psaOnRequestIds pdNumIdsToAck pdNumIdsToReq
          psaOnDecisionCompleted
          pure $
            SendMsgRequestObjectIdsBlocking
              pdNumIdsToAck
              pdNumIdsToReq
              ( \objectIds -> WithEffect $ do
                  -- TODO: Add checks and validation
                  psaOnReceiveIds pdNumIdsToReq (NonEmpty.toList objectIds)
                  pure $ goIdle Zero
              )

    goReqIdsPipelined ::
      forall (n :: N).
      Nat n ->
      PeerDecision objectId object ->
      InboundStIdle n objectId object m ()
    goReqIdsPipelined n PeerDecision{pdNumIdsToAck, pdNumIdsToReq} = WithEffect $ do
      if pdNumIdsToReq == 0
        then do
          psaOnDecisionCompleted
          pure $ goIdle n
        else do
          psaOnRequestIds pdNumIdsToAck pdNumIdsToReq
          psaOnDecisionCompleted
          pure $
            SendMsgRequestObjectIdsPipelined
              pdNumIdsToAck
              pdNumIdsToReq
              (goIdle (Succ n))
