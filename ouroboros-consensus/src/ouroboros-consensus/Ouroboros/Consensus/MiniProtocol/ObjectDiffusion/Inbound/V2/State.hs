{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.State
  ( -- * Core API
    DecisionGlobalState (..)
  , DecisionPeerState (..)
  , onRequestIds
  , onRequestObjects
  , onReceivedIds
  , onReceivedObjects
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Concurrent.Class.MonadSTM.TSem
import Control.Exception (assert)
import Control.Tracer (Tracer, traceWith)
import Data.Foldable qualified as Foldable
import Data.Map.Strict (Map, findWithDefault)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set ((\\), Set)
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API (ObjectPoolWriter (..))
import Ouroboros.Consensus.Util.IOLike (MonadMask, MonadMVar, bracket_)
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsReq)

onRequestIds ::
  forall m peerAddr object objectId.
  (MonadSTM m, Ord objectId, Ord peerAddr) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  ObjectPoolWriter objectId object m ->
  peerAddr ->
  -- | number of requests to req
  NumObjectIdsReq ->
  m ()
onRequestIds odTracer decisionTracer globalStateVar _objectPoolWriter peerAddr numIdsToReq = do
  globalState' <- atomically $ do
    stateTVar
      globalStateVar
      ( \globalState ->
          let globalState' = onRequestIdsImpl peerAddr numIdsToReq globalState
           in (globalState', globalState') )
  traceWith odTracer (TraceObjectDiffusionInboundRequestedIds (fromIntegral numIdsToReq))
  traceWith decisionTracer (TraceDecisionLogicGlobalStateUpdated "onRequestIds" globalState')

onRequestIdsImpl ::
  forall peerAddr object objectId.
  (Ord objectId, Ord peerAddr, HasCallStack) =>
  peerAddr ->
  -- | number of requests to req
  NumObjectIdsReq ->
  DecisionGlobalState peerAddr objectId object ->
  DecisionGlobalState peerAddr objectId object
onRequestIdsImpl
  peerAddr
  numIdsToReq
  globalState@DecisionGlobalState
    { dgsPeerStates
    } =
    globalState
      { dgsPeerStates = dgsPeerStates'
      }
    where
      dgsPeerStates' =
        Map.adjust
          (\ps@DecisionPeerState{dpsNumIdsInflight} -> ps{dpsNumIdsInflight = dpsNumIdsInflight + numIdsToReq})
          peerAddr
          dgsPeerStates

onRequestObjects ::
  forall m peerAddr object objectId.
  (MonadSTM m, Ord objectId, Ord peerAddr) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  ObjectPoolWriter objectId object m ->
  peerAddr ->
  -- | objets to request, by id
  Set objectId ->
  m ()
onRequestObjects odTracer decisionTracer globalStateVar _objectPoolWriter peerAddr objectIds = do
  globalState' <- atomically $ do
    stateTVar
      globalStateVar
      ( \globalState ->
          let globalState' = onRequestObjectsImpl peerAddr objectIds globalState
            in (globalState', globalState') )
  traceWith odTracer (TraceObjectDiffusionInboundRequestedObjects (Set.size objectIds))
  traceWith decisionTracer (TraceDecisionLogicGlobalStateUpdated "onRequestObjects" globalState')

onRequestObjectsImpl ::
  forall peerAddr object objectId.
  (Ord objectId, Ord peerAddr, HasCallStack) =>
  peerAddr ->
  -- | objets to request, by id
  Set objectId ->
  DecisionGlobalState peerAddr objectId object ->
  DecisionGlobalState peerAddr objectId object
onRequestObjectsImpl
  peerAddr
  objectIds
  globalState@DecisionGlobalState
    { dgsPeerStates
    , dgsObjectsInflightMultiplicities
    } =
    globalState
      { dgsObjectsInflightMultiplicities = dgsObjectsInflightMultiplicities'
      , dgsPeerStates = dgsPeerStates'
      }
    where
      dgsObjectsInflightMultiplicities' =
        Foldable.foldl'
          increaseCount
          dgsObjectsInflightMultiplicities
          objectIds
      dgsPeerStates' =
        Map.adjust
          (\ps@DecisionPeerState{dpsObjectsAvailableIds, dpsObjectsInflightIds} ->
            ps{dpsObjectsAvailableIds = dpsObjectsAvailableIds \\ objectIds,
               dpsObjectsInflightIds = dpsObjectsInflightIds `Set.union` objectIds})
          peerAddr
          dgsPeerStates

-- | Wrapper around `onReceivedIdsImpl`.
-- Obtain the `hasObject` function atomically from the STM context and
-- updates and traces the global state TVar.
onReceivedIds ::
  forall m peerAddr object objectId.
  (MonadSTM m, Ord objectId, Ord peerAddr) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  ObjectPoolWriter objectId object m ->
  peerAddr ->
  -- | number of requests to subtract from
  -- `dpsNumIdsInflight`
  NumObjectIdsReq ->
  -- | sequence of received `objectIds`
  [objectId] ->
  -- | received `objectId`s
  m ()
onReceivedIds odTracer decisionTracer globalStateVar objectPoolWriter peerAddr numIdsInitiallyRequested receivedIds = do
  globalState' <- atomically $ do
    hasObject <- opwHasObject objectPoolWriter
    stateTVar
      globalStateVar
      ( \globalState -> let globalState' = onReceivedIdsImpl hasObject peerAddr numIdsInitiallyRequested receivedIds globalState
         in (globalState', globalState') )
  traceWith odTracer (TraceObjectDiffusionInboundReceivedIds (length receivedIds))
  traceWith decisionTracer (TraceDecisionLogicGlobalStateUpdated "onReceivedIds" globalState')

onReceivedIdsImpl ::
  forall peerAddr object objectId.
  (Ord objectId, Ord peerAddr, HasCallStack) =>
  -- | check if objectId is in the objectpool, ref
  -- 'opwHasObject'
  (objectId -> Bool) ->
  peerAddr ->
  -- | number of requests to subtract from
  -- `dpsNumIdsInflight`
  NumObjectIdsReq ->
  -- | sequence of received `objectId`s
  [objectId] ->
  DecisionGlobalState peerAddr objectId object ->
  DecisionGlobalState peerAddr objectId object
onReceivedIdsImpl
  hasObject
  peerAddr
  numIdsInitiallyRequested
  receivedIds
  globalState@DecisionGlobalState
    { dgsPeerStates
    } =
    globalState
      { dgsPeerStates = dgsPeerStates'
      }
    where
      peerState@DecisionPeerState
        { dpsOutstandingFifo
        , dpsObjectsInflightIds
        , dpsObjectsAvailableIds
        , dpsNumIdsInflight
        } =
        findWithDefault
          (error "ObjectDiffusion.onReceivedIdsImpl: the peer should appear in dgsPeerStates")
          peerAddr
          dgsPeerStates
  
      -- Actually we don't need to filter out availableIds, because
      -- makeDecisions is the only reader of dpsObjectsAvailableIds
      -- and will filter it when needed with the actualized state of the object
      -- pool.
      dpsObjectsAvailableIds' =
        dpsObjectsAvailableIds `Set.union` Set.fromList receivedIds

      -- Add received objectIds to `dpsOutstandingFifo`.
      dpsOutstandingFifo' = dpsOutstandingFifo <> StrictSeq.fromList receivedIds

      peerState' =
        assert
          (dpsNumIdsInflight >= numIdsInitiallyRequested)
          peerState
            { dpsObjectsAvailableIds = dpsObjectsAvailableIds'
            , dpsOutstandingFifo = dpsOutstandingFifo'
            , dpsNumIdsInflight = dpsNumIdsInflight - numIdsInitiallyRequested
            }
      
      dgsPeerStates' = Map.insert peerAddr peerState' dgsPeerStates

-- | Wrapper around `onReceivedObjectsImpl` that updates and traces the
-- global state TVar.
--
-- Error handling should be done by the client before using the API.
-- In particular we assume:
-- assert (objectsRequestedIds `Set.isSubsetOf` dpsObjectsInflightIds)
--
-- IMPORTANT: We also assume that every object has been *validated* before being passed to this function.
onReceivedObjects ::
  forall m peerAddr object objectId.
  ( MonadSTM m
  , MonadMask m
  , MonadMVar m
  , Ord objectId
  , Ord peerAddr
  ) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  ObjectPoolWriter objectId object m ->
  ObjectPoolSem m ->
  peerAddr ->
  -- | received objects
  [object] ->
  m ()
onReceivedObjects odTracer tracer globalStateVar objectPoolWriter poolSem peerAddr objectsReceived = do
  let getId = opwObjectId objectPoolWriter
  let objectsReceivedMap = Map.fromList $ (\obj -> (getId obj, obj)) <$> objectsReceived

  globalState' <- atomically $ do
    stateTVar
      globalStateVar
      ( \globalState ->
        let globalState' =
              onReceivedObjectsImpl
                peerAddr
                objectsReceivedMap
                globalState
         in (globalState', globalState'))
  traceWith odTracer (TraceObjectDiffusionInboundReceivedObjects (length objectsReceived))
  traceWith tracer (TraceDecisionLogicGlobalStateUpdated "onReceivedObjects" globalState')
  submitObjectsToPool
    odTracer
    tracer
    globalStateVar
    objectPoolWriter
    poolSem
    peerAddr
    objectsReceivedMap

onReceivedObjectsImpl ::
  forall peerAddr object objectId.
  ( Ord peerAddr
  , Ord objectId
  ) =>
  peerAddr ->
  -- | received objects
  Map objectId object ->
  DecisionGlobalState peerAddr objectId object ->
  DecisionGlobalState peerAddr objectId object
onReceivedObjectsImpl
  peerAddr
  objectsReceived
  st@DecisionGlobalState
    { dgsPeerStates
    , dgsObjectsInflightMultiplicities
    , dgsObjectsOwtPoolMultiplicities
    } =
    st
      { dgsObjectsInflightMultiplicities = dgsObjectsInflightMultiplicities'
      , dgsObjectsOwtPoolMultiplicities = dgsObjectsOwtPoolMultiplicities'
      , dgsPeerStates = dgsPeerStates'
      }
    where
      objectsReceivedIds = Map.keysSet objectsReceived

      peerState@DecisionPeerState
        { dpsObjectsInflightIds
        , dpsObjectsOwtPool
        } =
        findWithDefault
          (error "ObjectDiffusion.onReceivedObjectsImpl: the peer should appear in dgsPeerStates")
          peerAddr
          dgsPeerStates

      -- subtract requested from in-flight
      dpsObjectsInflightIds' =
        dpsObjectsInflightIds \\ objectsReceivedIds

      dgsObjectsInflightMultiplicities' =
        Foldable.foldl'
          decreaseCount
          dgsObjectsInflightMultiplicities
          objectsReceivedIds
      
      dpsObjectsOwtPool' = dpsObjectsOwtPool <> objectsReceived

      dgsObjectsOwtPoolMultiplicities' =
        Foldable.foldl'
          increaseCount
          dgsObjectsOwtPoolMultiplicities
          objectsReceivedIds

      peerState' =
        peerState
          { dpsObjectsInflightIds = dpsObjectsInflightIds'
          , dpsObjectsOwtPool = dpsObjectsOwtPool'
          }

      dgsPeerStates' = Map.insert peerAddr peerState' dgsPeerStates

-- | Should be called by `acknowledgeIds`
submitObjectsToPool ::
  forall m peerAddr object objectId.
  ( Ord objectId
  , Ord peerAddr
  , MonadMask m
  , MonadMVar m
  , MonadSTM m
  ) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  ObjectPoolWriter objectId object m ->
  ObjectPoolSem m ->
  peerAddr ->
  Map objectId object ->
  m ()
submitObjectsToPool
  odTracer
  decisionTracer
  globalStateVar
  objectPoolWriter
  (ObjectPoolSem poolSem)
  peerAddr
  objects = do
  let getId = opwObjectId objectPoolWriter

  bracket_
    (atomically $ waitTSem poolSem)
    (atomically $ signalTSem poolSem)
    $ do

      -- When the lock over the object pool is obtained
      opwAddObjects objectPoolWriter (Map.elems objects)
      traceWith odTracer $
        TraceObjectDiffusionInboundAddedObjects $ length objects

      -- Move objects from `owtPool` to `inPool` state
      globalState' <- atomically $ stateTVar globalStateVar $ \globalState ->
        let globalState' =
              Foldable.foldl'
                (\st object -> updateStateWhenObjectAddedToPool (getId object) st)
                globalState
                objects
         in (globalState', globalState')
      traceWith decisionTracer (TraceDecisionLogicGlobalStateUpdated "submitObjectsToPool.updateStateWhenObjectAddedToPool" globalState')

  where
    updateStateWhenObjectAddedToPool ::
      objectId ->
      DecisionGlobalState peerAddr objectId object ->
      DecisionGlobalState peerAddr objectId object
    updateStateWhenObjectAddedToPool
      objectId
      st@DecisionGlobalState
        { dgsObjectsOwtPoolMultiplicities
        , dgsPeerStates
        } =
        st
          { dgsObjectsOwtPoolMultiplicities = dgsObjectsOwtPoolMultiplicities'
          , dgsPeerStates = dgsPeerStates'
          }
        where
        dgsObjectsOwtPoolMultiplicities' = decreaseCount dgsObjectsOwtPoolMultiplicities objectId

        dgsPeerStates' =
          Map.adjust
          (\ps@DecisionPeerState{dpsObjectsOwtPool} -> ps{dpsObjectsOwtPool = Map.delete objectId dpsObjectsOwtPool})
          peerAddr
          dgsPeerStates
