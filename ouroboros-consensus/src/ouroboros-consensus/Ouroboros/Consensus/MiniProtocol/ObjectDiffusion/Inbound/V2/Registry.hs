{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Registry
  ( PeerDecisionChannels
  , PeerDecisionChannelsVar
  , ObjectPoolSem
  , PeerStatesVar
  , newPeerDecisionChannelsVar
  , newObjectPoolSem
  , PeerStateAPI (..)
  , withObjectDiffusionInboundPeer
  , decisionLogicThread
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Monad (forever)
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
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
  Map peerAddr (StrictTVar m (PeerDecisionStatus objectId object))

type PeerDecisionChannelsVar m peerAddr objectId object =
  StrictTVar m (PeerDecisionChannels m peerAddr objectId object)

newPeerDecisionChannelsVar ::
  MonadSTM m => m (PeerDecisionChannelsVar m peerAddr objectId object)
newPeerDecisionChannelsVar = newTVarIO (Map.empty)

data PeerStateAPI m objectId object = PeerStateAPI
  { psaReadDecision :: m (PeerDecision objectId object)
  -- ^ A blocking action which reads the `PeerDecision` for this peer from the
  -- decision channel.
  -- It blocks until a new decision (i.e. with status `DecisionUnread`) is
  -- emitted for the peer by the decision thread, and immediately turn its
  -- status to `DecisionBeingActedUpon`.
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
  -- ^ To be called when emitting a request for new IDs (that also acks
  -- previously received IDs that we no longer care about). Under the hood, it
  -- will increase the `psNumIdsInFlight` count by the requested number of IDs,
  -- and remove the acked IDs from `psOutstandingFifo` and `psObjectsAvailableIds`.
  -- Note that those IDs may not be present in the latter, if they have already
  -- been requested to the outbound peer.
  --
  -- PRECONDITIONS:
  -- * `psOutstandingFifo` has at least `nAck :: NumObjectIdsAck` IDs that will
  -- be removed from it
  -- POSTCONDITIONS:
  -- * The `nAck` first IDs from `psOutstandingFifo` are removed from
  --  `psOutstandingFifo` and removed from `psObjectsAvailableIds`
  , psaOnRequestObjects :: Set objectId -> m ()
  -- ^ To be called when emitting a request for new objects. Under the hood, it
  -- will remove the requested IDs from `psObjectsAvailableIds` and add them to
  -- `psObjectsInflightIds`.
  --
  -- PRECONDITIONS:
  -- * The requested IDs are a subset of `psObjectsAvailableIds`
  -- * The requested IDs are not in `psObjectsInflightIds`
  -- POSTCONDITIONS:
  -- * The requested IDs are removed from `psObjectsAvailableIds`
  -- * The requested IDs are now in `psObjectsInflightIds`
  , psaOnReceiveIds :: NumObjectIdsReq -> [objectId] -> m ()
  -- ^ To be called after receiving new IDs from the outbound peer, after
  -- validating that we received the correct number (not more than requested).
  -- Under the hood, it will decrease the `psNumIdsInFlight` count by **the
  -- number of IDs that were requested in the request corresponding to this reply**.
  -- This number might be more than the number of received IDs. It also add the
  -- received IDs to `psOutstandingFifo` and `psObjectsAvailableIds`.
  --
  -- PRECONDITIONS:
  -- * The number of received IDs is less than or equal to `nReq :: NumObjectIdsReq`
  -- (the number of IDs that were requested in the request corresponding to this reply)
  -- * The received IDs are not already in `psObjectsAvailableIds` nor in
  --   `psObjectsInflightIds` nor in `psObjectsOwtPool`
  -- * The received IDs do not contain duplicates
  -- * `psNumIdsInFlight` is greater than or equal to `nReq :: NumObjectIdsReq`
  -- POSTCONDITIONS:
  -- * `psNumIdsInflight` is `nReq` less than before
  -- * `psOutstandingFifo` contains the received IDs appended at the end in the
  --   same order as they were received
  -- * `psObjectsAvailableIds` contains the received IDs
  , psaOnReceiveObjects :: [object] -> m ()
  -- ^ To be called when receiving objects from the outbound peer, after
  -- validating that the received objects match exactly the requested IDs.
  -- It also checks that all received objects have valid cryptographic proofs.
  -- Under the hood, it will remove the received IDs from `psObjectsInflightIds`,
  -- add the received objects to `dpsOwtPool`, and call the `submitObjectsToPool`
  -- subroutine that will actually insert the objects into the object pool.
  --
  -- PRECONDITIONS:
  -- * All received objects are valid wrt. their cryptographic proofs/invariants
  --   specific to the object type
  -- * The received objects correspond exactly to the set of requested objects
  --   (order not mattering)
  -- * The IDs of the received objects are a subset of `psObjectsInflightIds`
  -- POSTCONDITIONS:
  -- * The IDs of the received objects are removed from `psObjectsInflightIds`
  -- * `psObjectsOwtPool` contains the received objects
  }

-- | A bracket function which registers / de-registers a new peer in
-- `PeerStatesVar` and `PeerDecisionChannelsVar`s, which exposes `PeerStateAPI`.
-- `PeerStateAPI` is only safe inside the `withObjectDiffusionInboundPeer` scope.
withObjectDiffusionInboundPeer ::
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
  PeerStatesVar m peerAddr objectId object ->
  ObjectPoolWriter objectId object m ->
  ObjectPoolSem m ->
  -- | new peer
  peerAddr ->
  -- | callback which gives access to `PeerStateAPI`
  (PeerStateAPI m objectId object -> m a) ->
  m a
withObjectDiffusionInboundPeer
  decisionTracer
  objectDiffusionTracer
  decisionChannelsVar
  _decisionPolicy
  peerStatesVar
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
        -- Should not happen normally, because we unregister the peer from the
        -- channels map on disconnection through the bracket function
        Just chan -> return chan
        -- Otherwise create a new channel and register it
        Nothing -> do
          newChan <- newTVar PeerDecisionCompleted
          modifyTVar decisionChannelsVar (Map.insert peerAddr newChan)
          return newChan

      let !inboundPeerAPI =
            PeerStateAPI
              { psaReadDecision = atomically $ do
                  -- This should block until the decision has status `DecisionUnread`
                  -- which means it is a new decision that the peer has not acted upon yet
                  -- If `DecisionCompleted` is read here, it means the decision
                  -- logic hasn't had time to make a new decision for this peer
                  decision <- readTVar decisionChan
                  case decision of
                    PeerDecisionBeingActedUpon{} ->
                      error "Forgot to call `psaOnDecisionCompleted` for this peer"
                    PeerDecisionCompleted -> retry
                    PeerDecisionUnread dec ->
                      let decision' = PeerDecisionBeingActedUpon dec
                       in do
                            writeTVar decisionChan decision'
                            return dec
              , psaOnDecisionCompleted = atomically $ do
                  decision <- readTVar decisionChan
                  case decision of
                    PeerDecisionUnread{} ->
                      error
                        ( "Forgot to call `psaReadDecision` for this peer, or the "
                            ++ "decision thread has mistakenly updated the decision "
                            ++ "for this peer while it was executing it"
                        )
                    PeerDecisionCompleted ->
                      error "`psaOnDecisionCompleted` has already been called for this peer"
                    PeerDecisionBeingActedUpon{} ->
                      writeTVar decisionChan PeerDecisionCompleted
              , psaOnRequestIds =
                  State.onRequestIds
                    objectDiffusionTracer
                    decisionTracer
                    peerStatesVar
                    peerAddr
              , psaOnRequestObjects =
                  State.onRequestObjects
                    objectDiffusionTracer
                    decisionTracer
                    peerStatesVar
                    peerAddr
              , psaOnReceiveIds =
                  State.onReceiveIds
                    objectDiffusionTracer
                    decisionTracer
                    objectPoolWriter
                    peerStatesVar
                    peerAddr
              , psaOnReceiveObjects = \objects -> do
                  status <- atomically $ readTVar decisionChan
                  case status of
                    PeerDecisionUnread{} ->
                      error
                        ( "The peer shouldn't be processing received objects "
                            ++ "if it has no decision being acted upon"
                        )
                    PeerDecisionCompleted ->
                      error
                        ( "The peer shouldn't be processing received objects if it "
                            ++ "has finished acting upon its decision"
                        )
                    PeerDecisionBeingActedUpon decision -> do
                      let objectsToRequest = rodObjectsToReqIds (pdReqObjects decision)
                      State.onReceiveObjects
                        objectDiffusionTracer
                        decisionTracer
                        peerStatesVar
                        objectPoolWriter
                        objectPoolSem
                        peerAddr
                        objectsToRequest
                        objects
              }

      -- register the peer in the global state now
      modifyTVar peerStatesVar registerPeerPeerStates
      -- initialization is complete for this peer, it can proceed and
      -- interact through its given API
      return inboundPeerAPI

    unregisterPeer :: PeerStateAPI m objectId object -> m ()
    unregisterPeer _api =
      -- the handler is a short blocking operation, thus we need to use
      -- `uninterruptibleMask_`
      uninterruptibleMask_ $ atomically $ do
        -- unregister the peer from the global state
        modifyTVar peerStatesVar unregisterPeerPeerStates
        -- remove the channel of this peer from the global channel map
        modifyTVar decisionChannelsVar (Map.delete peerAddr)

    registerPeerPeerStates ::
      Map peerAddr (PeerState objectId object) ->
      Map peerAddr (PeerState objectId object)
    registerPeerPeerStates peerStates =
      Map.insert
        peerAddr
        PeerState
          { psObjectsAvailableIds = Set.empty
          , psNumIdsInflight = 0
          , psObjectsInflightIds = Set.empty
          , psOutstandingFifo = StrictSeq.empty
          , psObjectsOwtPool = Map.empty
          }
        peerStates

    -- TODO: this function needs to be tested!
    -- Issue: https://github.com/IntersectMBO/ouroboros-network/issues/5151
    unregisterPeerPeerStates ::
      Map peerAddr (PeerState objectId object) ->
      Map peerAddr (PeerState objectId object)
    unregisterPeerPeerStates = Map.delete peerAddr

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
  PeerStatesVar m peerAddr objectId object ->
  m Void
decisionLogicThread
  decisionTracer
  countersTracer
  ObjectPoolWriter{opwHasObject}
  decisionPolicy@DecisionPolicy{dpDecisionThreadSleepDelay}
  decisionChannelsVar
  peerStatesVar = do
    labelThisThread "ObjectDiffusionInbound.decisionLogicThread"
    forever $ do
      -- We rate limit the decision making process, it could overwhelm the CPU
      -- if there are too many inbound connections.
      --
      -- TODO: change that for a watcher pattern based on decisions/global state
      -- being updated?
      threadDelay dpDecisionThreadSleepDelay

      rng <- initStdGen

      (newDecisions, counters) <- atomically $ do
        decisionsChannels <- readTVar decisionChannelsVar
        prevDecisions <- traverse readTVar decisionsChannels
        peerStates <- readTVar peerStatesVar
        hasObject <- opwHasObject
        let newDecisions =
              makeDecisions
                DecisionContext
                  { dcRng = rng
                  , dcHasObject = hasObject
                  , dcDecisionPolicy = decisionPolicy
                  , dcPeerStates = peerStates
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
          (\(chan, decision) -> writeTVar chan (PeerDecisionUnread decision))
          peerToChannelAndDecision

        -- Return values for tracing purposes
        let counters = makeObjectDiffusionCounters peerStates
        return (newDecisions, counters)

      traceWith decisionTracer (TraceDecisionLogicDecisionsMade newDecisions)
      traceWith countersTracer counters
