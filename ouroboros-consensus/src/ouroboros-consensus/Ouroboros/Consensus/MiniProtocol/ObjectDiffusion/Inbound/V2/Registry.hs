{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Registry
  ( PeerDecisionChannels
  , PeerDecisionChannelsVar
  , ObjectPoolSem
  , DecisionGlobalStateVar
  , newPeerDecisionChannelsVar
  , newObjectPoolSem
  , PeerStateAPI (..)
  , withPeer
  , decisionLogicThread
  ) where

import Control.Concurrent.Class.MonadMVar.Strict
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (forever)
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Monad.IO.Class (MonadIO)
import Control.Tracer (Tracer, traceWith)
import Data.Foldable as Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Decision
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Policy
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.State qualified as State
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsAck, NumObjectIdsReq)
import System.Random (initStdGen)

-- | Communication channels between `ObjectDiffusion` mini-protocol inbound side
-- and decision logic.
type PeerDecisionChannels m peerAddr objectId object =
  Map peerAddr (StrictMVar m (PeerDecision objectId object))

type PeerDecisionChannelsVar m peerAddr objectId object =
  StrictMVar m (PeerDecisionChannels m peerAddr objectId object)

newPeerDecisionChannelsVar ::
  MonadMVar m => m (PeerDecisionChannelsVar m peerAddr objectId object)
newPeerDecisionChannelsVar = newMVar (Map.empty)

data PeerStateAPI m objectId object = PeerStateAPI
  { psaReadDecision :: m (PeerDecision objectId object)
  -- ^ a blocking action which reads `PeerDecision`
  , psaOnDecisionExecuted :: m ()
  -- ^ to be called by the peer when it has fully executed the decision.
  -- Marks the peer as available for the `makeDecision` logic
  , psaOnRequestIds :: NumObjectIdsAck -> NumObjectIdsReq -> m ()
  , psaOnRequestObjects :: Set objectId -> m ()
  , psaOnReceiveIds :: NumObjectIdsReq -> [objectId] -> m ()
  -- ^ Error handling should have been done before calling this
  , psaOnReceiveObjects :: [object] -> m ()
  -- ^ Error handling should have been done before calling this
  -- Also every object should have been validated!
  }

-- | A bracket function which registers / de-registers a new peer in
-- `DecisionGlobalStateVar` and `PeerDecisionChannelsVar`s, which exposes `PeerStateAPI`.
-- `PeerStateAPI` is only safe inside the `withPeer` scope.
withPeer ::
  forall object peerAddr objectId m a.
  ( MonadMask m
  , MonadMVar m
  , MonadSTM m
  , Ord objectId
  , Ord peerAddr
  ) =>
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  PeerDecisionChannelsVar m peerAddr objectId object ->
  DecisionPolicy ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  ObjectPoolWriter objectId object m ->
  ObjectPoolSem m ->
  -- | new peer
  peerAddr ->
  -- | callback which gives access to `PeerStateAPI`
  (PeerStateAPI m objectId object -> m a) ->
  m a
withPeer
  decisionTracer
  objectDiffusionTracer
  decisionChannelsVar
  _decisionPolicy
  globalStateVar
  objectPoolWriter
  objectPoolSem
  peerAddr
  withAPI =
    bracket registerPeerAndCreateAPI unregisterPeer withAPI
   where
    registerPeerAndCreateAPI :: m (PeerStateAPI m objectId object)
    registerPeerAndCreateAPI = do
      -- create the API for this peer, obtaining a channel for it in the process
      !inboundPeerAPI <-
        modifyMVar
          decisionChannelsVar
          \peerToChannel -> do
            -- We get a channel for this peer, and register it in peerToChannel.
            (chan', peerToChannel') <-
              case peerToChannel Map.!? peerAddr of
                -- Checks if a channel already exists for this peer, in case we reuse it
                Just chan -> return (chan, peerToChannel)
                -- Otherwise create a new channel and register it
                Nothing -> do
                  chan <- newEmptyMVar
                  return (chan, Map.insert peerAddr chan peerToChannel)
            return
              ( peerToChannel'
              , PeerStateAPI
                  { psaReadDecision = do
                      -- TODO: make atomic
                      decision <- takeMVar chan'
                      let decision' = decision{pdExecutingDecision = True}
                      putMVar chan' decision'
                      return decision'
                  , psaOnDecisionExecuted = do
                      -- TODO: make atomic
                      decision <- takeMVar chan'
                      let decision' = decision{pdExecutingDecision = False}
                      putMVar chan' decision'
                  , psaOnRequestIds =
                      State.onRequestIds
                        objectDiffusionTracer
                        decisionTracer
                        globalStateVar
                        peerAddr
                  , psaOnRequestObjects =
                      State.onRequestObjects
                        objectDiffusionTracer
                        decisionTracer
                        globalStateVar
                        peerAddr
                  , psaOnReceiveIds =
                      State.onReceiveIds
                        objectDiffusionTracer
                        decisionTracer
                        globalStateVar
                        peerAddr
                  , psaOnReceiveObjects =
                      State.onReceiveObjects
                        objectDiffusionTracer
                        decisionTracer
                        globalStateVar
                        objectPoolWriter
                        objectPoolSem
                        peerAddr
                  }
              )
      -- register the peer in the global state now
      atomically $ modifyTVar globalStateVar registerPeerGlobalState
      -- initialization is complete for this peer, it can proceed and
      -- interact through its given API
      return inboundPeerAPI
     where

    unregisterPeer :: PeerStateAPI m objectId object -> m ()
    unregisterPeer _ =
      -- the handler is a short blocking operation, thus we need to use
      -- `uninterruptibleMask_`
      uninterruptibleMask_ do
        -- unregister the peer from the global state
        atomically $ modifyTVar globalStateVar unregisterPeerGlobalState
        -- remove the channel of this peer from the global channel map
        modifyMVar_
          decisionChannelsVar
          \peerToChannel ->
            return $ Map.delete peerAddr peerToChannel

    registerPeerGlobalState ::
      DecisionGlobalState peerAddr objectId object ->
      DecisionGlobalState peerAddr objectId object
    registerPeerGlobalState st@DecisionGlobalState{dgsPeerStates} =
      st
        { dgsPeerStates =
            Map.insert
              peerAddr
              DecisionPeerState
                { dpsObjectsAvailableIds = Set.empty
                , dpsNumIdsInflight = 0
                , dpsObjectsInflightIds = Set.empty
                , dpsOutstandingFifo = StrictSeq.empty
                , dpsObjectsOwtPool = Map.empty
                }
              dgsPeerStates
        }

    -- TODO: this function needs to be tested!
    -- Issue: https://github.com/IntersectMBO/ouroboros-network/issues/5151
    unregisterPeerGlobalState ::
      DecisionGlobalState peerAddr objectId object ->
      DecisionGlobalState peerAddr objectId object
    unregisterPeerGlobalState
      st@DecisionGlobalState
        { dgsPeerStates
        } =
        st
          { dgsPeerStates = Map.delete peerAddr dgsPeerStates
          }

decisionLogicThread ::
  forall m peerAddr objectId object.
  ( MonadDelay m
  , MonadMVar m
  , MonadSTM m
  , MonadFork m
  , MonadIO m
  , Ord peerAddr
  , Ord objectId
  ) =>
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  Tracer m ObjectDiffusionCounters ->
  ObjectPoolWriter objectId object m ->
  DecisionPolicy ->
  PeerDecisionChannelsVar m peerAddr objectId object ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  m Void
decisionLogicThread decisionTracer countersTracer ObjectPoolWriter{opwHasObject} decisionPolicy decisionChannelsVar globalStateVar = do
  labelThisThread "ObjectDiffusionInbound.decisionLogicThread"
  forever $ do
    -- We rate limit the decision making process, it could overwhelm the CPU
    -- if there are too many inbound connections.
    threadDelay _DECISION_LOOP_DELAY

    rng <- initStdGen

    -- TODO: can we make this whole block atomic?
    -- because makeDecisions should be atomic with respect to reading the global state and
    -- reading the previous decisions
    decisionsChannels <- readMVar decisionChannelsVar
    prevDecisions <- traverse takeMVar decisionsChannels
    (newDecisions, globalState) <- atomically $ do
      globalState <- readTVar globalStateVar
      hasObject <- opwHasObject
      pure $ (makeDecisions rng hasObject decisionPolicy globalState prevDecisions, globalState)

    traceWith decisionTracer (TraceDecisionLogicDecisionsMade newDecisions)
    peerToChannel <- readMVar decisionChannelsVar
    -- Pair decision channel with the corresponding decision
    let peerToChannelAndDecision =
          Map.intersectionWith
            (,)
            peerToChannel
            newDecisions
    -- Send the newDecisions to the corresponding peers
    -- Note that newDecisions are incremental, so we merge the old one to the new one (using the semigroup instance) if there is an old one
    traverse_ (uncurry putMVar) peerToChannelAndDecision

    traceWith countersTracer (makeObjectDiffusionCounters globalState)

-- `5ms` delay
_DECISION_LOOP_DELAY :: DiffTime
_DECISION_LOOP_DELAY = 0.005
