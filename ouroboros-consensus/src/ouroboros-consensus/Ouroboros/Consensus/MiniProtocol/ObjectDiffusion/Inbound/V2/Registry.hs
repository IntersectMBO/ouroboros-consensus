{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
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

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (forever, when)
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
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.State qualified as State
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsAck, NumObjectIdsReq)
import System.Random (initStdGen)

-- | Communication channels between `ObjectDiffusion` mini-protocol inbound side
-- and decision logic.
type PeerDecisionChannels m peerAddr objectId object =
  Map peerAddr (StrictTVar m (PeerDecision objectId object))

type PeerDecisionChannelsVar m peerAddr objectId object =
  StrictTVar m (PeerDecisionChannels m peerAddr objectId object)

newPeerDecisionChannelsVar ::
  MonadSTM m => m (PeerDecisionChannelsVar m peerAddr objectId object)
newPeerDecisionChannelsVar = newTVarIO (Map.empty)

data PeerStateAPI m objectId object = PeerStateAPI
  { psaReadDecision :: m (PeerDecision objectId object)
  -- ^ A blocking action which reads the `PeerDecision` for this peer from the decision channel.
  -- It blocks until a new decision (i.e. with status `DecisionUnread`) is emitted for the peer by the deecision thread,
  -- and immediately turn its status to `DecisionBeingActedUpon`.
  --
  -- PRECONDITIONS:
  -- * The decision in the channel has status `DecisionUnread` or `DecisionCompleted`
  -- POSTCONDITIONS:
  -- * The decision in the channel has status `DecisionBeingActedUpon`
  , psaOnDecisionCompleted :: m ()
  -- ^ To be called by the peer when it has fully executed the decision.
  -- Marks the peer as available for the decision logic.
  --
  -- PRECONDITIONS:
  -- * The decision in the channel has status `DecisionBeingActedUpon`
  -- POSTCONDITIONS:
  -- * The decision in the channel has status `DecisionCompleted`
  , psaOnRequestIds :: NumObjectIdsAck -> NumObjectIdsReq -> m ()
  -- ^ To be called when emitting a request for new IDs (that also acks previously received IDs that we no longer care about).
  -- Under the hood, it will increase the `dpsNumIdsInFlight` count by the requested number of IDs, and remove the acked IDs
  -- from `dpsOutstandingFifo` and `dpsObjectsAvailableIds`. Note that those IDs may not be present in the latter, if they have
  -- already been requested to the outbound peer.
  --
  -- PRECONDITIONS:
  -- * `dpsOutstandingFifo` has at least `nAck :: NumObjectIdsAck` IDs that will be removed from it
  -- POSTCONDITIONS:
  -- * The `nAck` first IDs from `dpsOutstandingFifo` are removed from `dpsOutstandingFifo` and  removed from `dpsObjectsAvailableIds`
  , psaOnRequestObjects :: Set objectId -> m ()
  -- ^ To be called when emitting a request for new objects. Under the hood, it will remove the requested IDs from `dpsObjectsAvailableIds`
  -- and add them to `dpsObjectsInflightIds`.
  --
  -- PRECONDITIONS:
  -- * The requested IDs are a subset of `dpsObjectsAvailableIds`
  -- * The requested IDs are not in `dpsObjectsInflightIds`
  -- POSTCONDITIONS:
  -- * The requested IDs are removed from `dpsObjectsAvailableIds`
  -- * The requested IDs are now in `dpsObjectsInflightIds`
  , psaOnReceiveIds :: NumObjectIdsReq -> [objectId] -> m ()
  -- ^ To be called after receiving new IDs from the outbound peer, after validating that we received the correct number (not more than requested).
  -- Under the hood, it will decrease the `dpsNumIdsInFlight` count by **the number of IDs that were requested in the request corresponding to this reply**.
  -- This number might be more than the number of received IDs. It also add the received IDs to `dpsOutstandingFifo` and `dpsObjectsAvailableIds`.
  --
  -- PRECONDITIONS:
  -- * The number of received IDs is less than or equal to `nReq :: NumObjectIdsReq` (the number of IDs that were requested in the request corresponding to this reply)
  -- * The received IDs are not already in `dpsObjectsAvailableIds` nor in `dpsObjectsInflightIds` nor in `dpsObjectsOwtPool`
  -- * The received IDs do not contain duplicates
  -- * `dpsNumIdsInFlight` is greater than or equal to `nReq :: NumObjectIdsReq`
  -- POSTCONDITIONS:
  -- * `dpsNumIdsInflight` is `nReq` less than before
  -- * `dpsOutstandingFifo` contains the received IDs appended at the end in the same order as they were received
  -- * `dpsObjectsAvailableIds` contains the received IDs
  , psaOnReceiveObjects :: [object] -> m ()
  -- ^ To be called when receiving objects from the outbound peer, after validating that the received objects match exactly the requested IDs.
  -- It also checks that all received objects have valid cryptographic proofs.
  -- Under the hood, it will remove the received IDs from `dpsObjectsInflightIds`, add the received objects to `dpsOwtPool`,
  -- and call the `submitObjectsToPool` subroutine that will actually insert the objects into the object pool.
  --
  -- PRECONDITIONS:
  -- * All received objects are valid wrt. their cryptographic proofs/invariants specific to the object type
  -- * The received objects correspond exactly to the set of requested objects (order not mattering)
  -- * The IDs of the received objects are a subset of `dpsObjectsInflightIds`
  -- POSTCONDITIONS:
  -- * The IDs of the received objects are removed from `dpsObjectsInflightIds`
  -- * `dpsObjectsOwtPool` contains the received objects
  }

-- | A bracket function which registers / de-registers a new peer in
-- `DecisionGlobalStateVar` and `PeerDecisionChannelsVar`s, which exposes `PeerStateAPI`.
-- `PeerStateAPI` is only safe inside the `withPeer` scope.
withPeer ::
  forall object peerAddr objectId m a.
  ( MonadMask m
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
    registerPeerAndCreateAPI = atomically $ do
      peerToChannel <- readTVar decisionChannelsVar
      decisionChan <- case peerToChannel Map.!? peerAddr of
        -- Checks if a channel already exists for this peer, in case we reuse it
        -- Should not happen normally, because we unregister the peer from the channels map on disconnection through the bracket function
        Just chan -> return chan
        -- Otherwise create a new channel and register it
        Nothing -> do
          newChan <- newTVar unavailableDecision
          modifyTVar decisionChannelsVar (Map.insert peerAddr newChan)
          return newChan

      let !inboundPeerAPI =
            PeerStateAPI
              { psaReadDecision = atomically $ do
                  -- This should block until the decision has status `DecisionUnread`
                  -- which means it is a new decision that the peer has not acted upon yet
                  -- If `DecisionCompleted` is read here, it means the decision logic hasn't had time to make a new decision for this peer
                  decision@PeerDecision{pdStatus} <- readTVar decisionChan
                  when (pdStatus == DecisionBeingActedUpon) $
                    error "Forgot to call `psaOnDecisionCompleted` for this peer"
                  check $ pdStatus == DecisionUnread
                  let decision' = decision{pdStatus = DecisionBeingActedUpon}
                  writeTVar decisionChan decision'
                  return decision'
              , psaOnDecisionCompleted = atomically $ do
                  decision@PeerDecision{pdStatus} <- readTVar decisionChan
                  when (pdStatus == DecisionUnread) $
                    error
                      "Forgot to call `psaReadDecision` for this peer, or the decision thread has mistakenly updated the decision for this peer while it was executing it"
                  when (pdStatus == DecisionCompleted) $
                    error "`psaOnDecisionCompleted` has already been called for this peer"
                  let decision' = decision{pdStatus = DecisionCompleted}
                  writeTVar decisionChan decision'
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
                    objectPoolWriter
                    globalStateVar
                    peerAddr
              , psaOnReceiveObjects = \objects -> do
                  PeerDecision{pdObjectsToReqIds} <- atomically $ readTVar decisionChan
                  State.onReceiveObjects
                    objectDiffusionTracer
                    decisionTracer
                    globalStateVar
                    objectPoolWriter
                    objectPoolSem
                    peerAddr
                    pdObjectsToReqIds
                    objects
              }

      -- register the peer in the global state now
      modifyTVar globalStateVar registerPeerGlobalState
      -- initialization is complete for this peer, it can proceed and
      -- interact through its given API
      return inboundPeerAPI
     where

    unregisterPeer :: PeerStateAPI m objectId object -> m ()
    unregisterPeer _api =
      -- the handler is a short blocking operation, thus we need to use
      -- `uninterruptibleMask_`
      uninterruptibleMask_ $ atomically $ do
        -- unregister the peer from the global state
        modifyTVar globalStateVar unregisterPeerGlobalState
        -- remove the channel of this peer from the global channel map
        modifyTVar decisionChannelsVar (Map.delete peerAddr)

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
    threadDelay const_DECISION_LOOP_DELAY

    rng <- initStdGen

    -- TODO: can we make this whole block atomic?
    -- because makeDecisions should be atomic with respect to reading the global state and
    -- reading the previous decisions
    (newDecisions, counters) <- atomically $ do
      decisionsChannels <- readTVar decisionChannelsVar
      prevDecisions <- traverse readTVar decisionsChannels
      globalState <- readTVar globalStateVar
      hasObject <- opwHasObject
      let newDecisions =
            makeDecisions
              DecisionContext
                { dcRng = rng
                , dcHasObject = hasObject
                , dcDecisionPolicy = decisionPolicy
                , dcGlobalState = globalState
                , dcPrevDecisions = prevDecisions
                }

      peerToChannel <- readTVar decisionChannelsVar
      -- Pair decision channel with the corresponding decision
      let peerToChannelAndDecision =
            Map.intersectionWith
              (,)
              peerToChannel
              newDecisions
      -- Send the newDecisions to the corresponding peers
      traverse_
        (\(chan, decision) -> writeTVar chan decision)
        peerToChannelAndDecision

      -- Return values for tracing purposes
      let counters = makeObjectDiffusionCounters globalState
      return (newDecisions, counters)

    traceWith decisionTracer (TraceDecisionLogicDecisionsMade newDecisions)
    traceWith countersTracer counters

-- `5ms` delay
const_DECISION_LOOP_DELAY :: DiffTime
const_DECISION_LOOP_DELAY = 0.005
