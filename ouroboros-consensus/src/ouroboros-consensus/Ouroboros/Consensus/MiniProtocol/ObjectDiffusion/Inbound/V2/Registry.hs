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
  , InboundPeerAPI (..)
  , withPeer
  , decisionLogicThread
  ) where

import Control.Concurrent.Class.MonadMVar.Strict
import Control.Concurrent.Class.MonadSTM.Strict
import Control.Concurrent.Class.MonadSTM.TSem
import Control.Monad.Class.MonadFork
import Control.Monad.Class.MonadThrow
import Control.Monad.Class.MonadTime.SI
import Control.Monad.Class.MonadTimer.SI
import Control.Tracer (Tracer, traceWith)
import Data.Foldable as Foldable (foldl', traverse_)
import Data.Hashable
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import Data.Void (Void)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Decision
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Policy
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.State (DecisionGlobalStateVar)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.State qualified as State
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API
import Control.Monad (forever)

-- | Communication channels between `ObjectDiffusion` mini-protocol inbound side
-- and decision logic.
type PeerDecisionChannels m peerAddr objectId object =
  Map peerAddr (StrictMVar m (PeerDecision objectId object))

type PeerDecisionChannelsVar m peerAddr objectId object =
  StrictMVar m (PeerDecisionChannels m peerAddr objectId object)

newPeerDecisionChannelsVar ::
  MonadMVar m => m (PeerDecisionChannelsVar m peerAddr objectId object)
newPeerDecisionChannelsVar = newMVar (Map.empty)

-- | Semaphore to guard access to the ObjectPool
newtype ObjectPoolSem m = ObjectPoolSem (TSem m)

newObjectPoolSem :: MonadSTM m => m (ObjectPoolSem m)
newObjectPoolSem = ObjectPoolSem <$> atomically (newTSem 1)

data InboundPeerAPI m objectId object = InboundPeerAPI
  { readPeerDecision :: m (PeerDecision objectId object)
  -- ^ a blocking action which reads `PeerDecision`
  , handleReceivedIds :: [objectId] -> m ()
  , handleReceivedObjects :: [object] -> m ()
  , submitObjectsToPool :: [object] -> m ()
  }

-- | A bracket function which registers / de-registers a new peer in
-- `DecisionGlobalStateVar` and `PeerDecisionChannelsVar`s, which exposes `InboundPeerAPI`.
-- `InboundPeerAPI` is only safe inside the `withPeer` scope.
withPeer ::
  forall object peerAddr objectId ticketNo m a.
  ( MonadMask m
  , MonadMVar m
  , MonadSTM m
  , MonadMonotonicTime m
  , Ord objectId
  , Show objectId
  , Typeable objectId
  , Ord peerAddr
  , Show peerAddr
  ) =>
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  PeerDecisionChannelsVar m peerAddr objectId object ->
  ObjectPoolSem m ->
  DecisionPolicy ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  ObjectPoolReader objectId object ticketNo m ->
  ObjectPoolWriter objectId object m ->
  -- | new peer
  peerAddr ->
  -- | callback which gives access to `InboundPeerAPI`
  (InboundPeerAPI m objectId object -> m a) ->
  m a
withPeer
  decisionTracer
  decisionChannelsVar
  (ObjectPoolSem poolSem)
  decisionPolicy
  globalStateVar
  objectPoolReader
  objectPoolWriter
  peerAddr
  withAPI =
    bracket registerPeerAndCreateAPI unregisterPeer withAPI
  where
    registerPeerAndCreateAPI :: m (InboundPeerAPI m objectId object)
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
                  , InboundPeerAPI
                      { readPeerDecision = takeMVar chan'
                      , handleReceivedIds = State.handleReceivedIds
                      , handleReceivedObjects = State.handleReceivedObjects
                      , submitObjectsToPool = State.submitObjectsToPool
                      }
                  )
          -- register the peer in the global state now
          atomically $ modifyTVar globalStateVar registerPeerGlobalState
          -- initialization is complete for this peer, it can proceed and
          -- interact through its given API
          return inboundPeerAPI
      where

    unregisterPeer :: InboundPeerAPI m objectId object -> m ()
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
                , dpsObjectsPending = Map.empty
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
        , dgsObjectsLiveMultiplicities
        , dgsObjectsInflightMultiplicities
        , dgsObjectsOwtPoolMultiplicities
        } =
        st
          { dgsPeerStates = dgsPeerStates'
          , dgsObjectsLiveMultiplicities = dgsObjectsLiveMultiplicities'
          , dgsObjectsInflightMultiplicities = dgsObjectsInflightMultiplicities'
          , dgsObjectsOwtPoolMultiplicities = dgsObjectsOwtPoolMultiplicities'
          }
       where
        -- First extract the DPS of the specified peer from the DGS
        ( DecisionPeerState
            { dpsOutstandingFifo
            , dpsObjectsInflightIds
            , dpsObjectsOwtPool
            }
          , dgsPeerStates'
          ) =
            Map.alterF
              ( \case
                  Nothing -> error ("ObjectDiffusion.withPeer: can't unregister peer " ++ show peerAddr ++ " because it isn't registered")
                  Just a -> (a, Nothing)
              )
              peerAddr
              dgsPeerStates
        
        -- Update the dgsObjectsLiveMultiplicities map by decreasing the count of each
        -- objectId which is part of the dpsOutstandingFifo of this peer.
        dgsObjectsLiveMultiplicities' =
          Foldable.foldl'
            decreaseCount
            dgsObjectsLiveMultiplicities
            dpsOutstandingFifo

        -- Update dgsInflightMultiplicities map by decreasing the count
        -- of objects that were in-flight for this peer.
        dgsObjectsInflightMultiplicities' =
          Foldable.foldl'
            decreaseCount
            dgsObjectsInflightMultiplicities
            dpsObjectsInflightIds

        -- Finally, we need to update dgsObjectsOwtPoolMultiplicities by decreasing the count of
        -- each objectId which is part of the dpsObjectsOwtPool of this peer.
        dgsObjectsOwtPoolMultiplicities' =
          Foldable.foldl'
            decreaseCount
            dgsObjectsOwtPoolMultiplicities
            (Map.keysSet dpsObjectsOwtPool)

decisionLogicThread ::
  forall m peerAddr objectId object.
  ( MonadDelay m
  , MonadMVar m
  , MonadSTM m
  , MonadFork m
  , Ord peerAddr
  , Ord objectId
  , Hashable peerAddr
  ) =>
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  Tracer m ObjectDiffusionCounters ->
  DecisionPolicy ->
  PeerDecisionChannelsVar m peerAddr objectId object ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  m Void
decisionLogicThread decisionTracer countersTracer decisionPolicy decisionChannelsVar globalStateVar = do
  labelThisThread "ObjectDiffusionInbound.decisionLogicThread"
  forever $ do
    -- We rate limit the decision making process, it could overwhelm the CPU
    -- if there are too many inbound connections.
    threadDelay _DECISION_LOOP_DELAY

    -- Make decisions and update the global state var accordingly
    (decisions, globalState') <- atomically $ do
      globalState <- readTVar globalStateVar
      let activePeers = filterActivePeers decisionPolicy globalState

      -- block until at least one peer is active
      check (not (Map.null activePeers))

      let (globalState', decisions) = makeDecisions decisionPolicy globalState activePeers
      writeTVar globalStateVar globalState'
      return (decisions, globalState')

    traceWith decisionTracer (TraceDecisionLogicGlobalStateUpdated "decisionLogicThread" globalState')
    traceWith decisionTracer (TraceDecisionLogicDecisionsMade decisions)
    peerToChannel <- readMVar decisionChannelsVar
    -- Pair decision channel with the corresponding decision
    let peerToChannelAndDecision =
          Map.intersectionWith
            (,)
            peerToChannel
            decisions
    -- Send the decisions to the corresponding peers
    -- Note that decisions are incremental, so we merge the old one to the new one (using the semigroup instance) if there is an old one
    traverse_
      (\(chan, newDecision) ->
        modifyMVarWithDefault_
          chan newDecision (\oldDecision -> pure (oldDecision <> newDecision)))
      peerToChannelAndDecision

    traceWith countersTracer (makeObjectDiffusionCounters globalState')

-- Variant of modifyMVar_ that puts a default value if the MVar is empty.
modifyMVarWithDefault_ :: StrictMVar m a -> a -> (a -> m a) -> m ()
modifyMVarWithDefault_ m d io =
  mask $ \restore -> do
    mbA <- tryTakeMVar m
    case mbA of
      Just a -> do
        a' <- restore (io a) `onException` putMVar m a
        putMVar m a'
      Nothing -> putMVar m d

-- `5ms` delay
_DECISION_LOOP_DELAY :: DiffTime
_DECISION_LOOP_DELAY = 0.005
