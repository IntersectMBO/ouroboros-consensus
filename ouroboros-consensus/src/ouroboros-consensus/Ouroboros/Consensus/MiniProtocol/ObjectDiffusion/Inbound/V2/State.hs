{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.State
  ( -- * Core API
    DecisionGlobalState (..)
  , DecisionPeerState (..)
  , handleReceivedIds
  , handleReceivedObjects
  , submitObjectsToPool
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Concurrent.Class.MonadSTM.TSem
import Control.Exception (assert)
import Control.Tracer (Tracer, traceWith)
import Data.Foldable qualified as Foldable
import Data.Map.Strict (Map, findWithDefault)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set ((\\), Set)
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API (ObjectPoolWriter (..))
import Ouroboros.Consensus.Util.IOLike (MonadMask, MonadMVar, bracket_)
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsReq)

strictSeqToSet :: Ord a => StrictSeq a -> Set a
strictSeqToSet = Foldable.foldl' (flip Set.insert) Set.empty

-- | Wrapper around `handleReceivedIdsImpl`.
-- Obtain the `hasObject` function atomically from the STM context and
-- updates and traces the global state TVar.
handleReceivedIds ::
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
  StrictSeq objectId ->
  -- | received `objectId`s
  m ()
handleReceivedIds odTracer decisionTracer globalStateVar objectPoolWriter peerAddr numIdsInitiallyRequested receivedIdsSeq = do
  globalState' <- atomically $ do
    hasObject <- opwHasObject objectPoolWriter
    stateTVar
      globalStateVar
      ( \globalState -> let globalState' = handleReceivedIdsImpl hasObject peerAddr numIdsInitiallyRequested receivedIdsSeq globalState
         in (globalState', globalState') )
  traceWith odTracer (TraceObjectDiffusionInboundReceivedIds (StrictSeq.length receivedIdsSeq))
  traceWith decisionTracer (TraceDecisionLogicGlobalStateUpdated "handleReceivedIds" globalState')

handleReceivedIdsImpl ::
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
  StrictSeq objectId ->
  DecisionGlobalState peerAddr objectId object ->
  DecisionGlobalState peerAddr objectId object
handleReceivedIdsImpl
  hasObject
  peerAddr
  numIdsInitiallyRequested
  receivedIdsSeq
  globalState@DecisionGlobalState
    { dgsPeerStates
    , dgsObjectsPendingMultiplicities
    , dgsObjectsOwtPoolMultiplicities
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
          (error "ObjectDiffusion.handleReceivedIdsImpl: the peer should appear in dgsPeerStates")
          peerAddr
          dgsPeerStates
  
      -- Filter out objects we are not interesting in downloading
      newObjectsAvailableIds =
        Set.filter (\objectId ->
          (not . hasObject $ objectId) -- object isn't already in the object pool
            && objectId `Set.notMember` dpsObjectsInflightIds -- object isn't in flight from current peer
            && objectId `Map.notMember` dgsObjectsPendingMultiplicities -- the object has not been successfully downloaded from another peer
            && objectId `Map.notMember` dgsObjectsOwtPoolMultiplicities -- (either pending ack or owt pool)
          ) $ strictSeqToSet $ receivedIdsSeq

      dpsObjectsAvailableIds' =
        dpsObjectsAvailableIds `Set.union` newObjectsAvailableIds

      -- Add received objectIds to `dpsOutstandingFifo`.
      dpsOutstandingFifo' = dpsOutstandingFifo <> receivedIdsSeq

      peerState' =
        assert
          (dpsNumIdsInflight >= numIdsInitiallyRequested)
          peerState
            { dpsObjectsAvailableIds = dpsObjectsAvailableIds'
            , dpsOutstandingFifo = dpsOutstandingFifo'
            , dpsNumIdsInflight = dpsNumIdsInflight - numIdsInitiallyRequested
            }
      
      dgsPeerStates' = Map.insert peerAddr peerState' dgsPeerStates

-- | Wrapper around `handleReceivedObjectsImpl` that updates and traces the
-- global state TVar.
--
-- Error handling should be done by the client before using the API.
-- In particular we assume:
-- assert (objectsRequestedIds `Set.isSubsetOf` dpsObjectsInflightIds)
--
-- IMPORTANT: We also assume that every object has been *validated* before being passed to this function.
handleReceivedObjects ::
  forall m peerAddr object objectId.
  ( MonadSTM m
  , Ord objectId
  , Ord peerAddr
  ) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  ObjectPoolWriter objectId object m ->
  peerAddr ->
  -- | received objects
  Map objectId object ->
  m ()
handleReceivedObjects odTracer tracer globalStateVar _objectPoolWriter peerAddr objectsReceived = do
  globalState' <- atomically $ do
    stateTVar
      globalStateVar
      ( \globalState -> let globalState' = handleReceivedObjectsImpl peerAddr objectsReceived globalState
         in (globalState', globalState') )
  traceWith odTracer (TraceObjectDiffusionInboundReceivedObjects (Map.size objectsReceived))
  traceWith tracer (TraceDecisionLogicGlobalStateUpdated "handleReceivedObjects" globalState')

handleReceivedObjectsImpl ::
  forall peerAddr object objectId.
  ( Ord peerAddr
  , Ord objectId
  ) =>
  peerAddr ->
  -- | received objects
  Map objectId object ->
  DecisionGlobalState peerAddr objectId object ->
  DecisionGlobalState peerAddr objectId object
handleReceivedObjectsImpl
  peerAddr
  objectsReceived
  st@DecisionGlobalState
    { dgsPeerStates
    , dgsObjectsInflightMultiplicities
    , dgsObjectsPendingMultiplicities
    } =
    st
      { dgsObjectsInflightMultiplicities = dgsObjectsInflightMultiplicities'
      , dgsObjectsPendingMultiplicities = dgsObjectsPendingMultiplicities'
      , dgsPeerStates = dgsPeerStates'
      }
    where
      objectsReceivedIds = Map.keysSet objectsReceived

      peerState@DecisionPeerState
        { dpsObjectsInflightIds
        , dpsObjectsPending
        } =
        findWithDefault
          (error "ObjectDiffusion.handleReceivedObjectsImpl: the peer should appear in dgsPeerStates")
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
      
      dpsObjectsPending' = dpsObjectsPending <> objectsReceived

      dgsObjectsPendingMultiplicities' =
        Foldable.foldl'
          increaseCount
          dgsObjectsPendingMultiplicities
          objectsReceivedIds

      peerState' =
        peerState
          { dpsObjectsInflightIds = dpsObjectsInflightIds'
          , dpsObjectsPending = dpsObjectsPending'
          }

      dgsPeerStates' = Map.insert peerAddr peerState' dgsPeerStates

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
  [object] ->
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

  -- Move objects from `pending` to `owtPool` state
  globalState' <- atomically $ stateTVar globalStateVar $ \globalState ->
      let globalState' =
            Foldable.foldl'
              (\st object -> updateStateWhenObjectOwtPool (getId object) st)
              globalState
              objects
      in (globalState', globalState')
  traceWith decisionTracer (TraceDecisionLogicGlobalStateUpdated "submitObjectsToPool.updateStateWhenObjectOwtPool" globalState')

  bracket_
    (atomically $ waitTSem poolSem)
    (atomically $ signalTSem poolSem)
    $ do
      -- When the lock over the object pool is obtained
      opwAddObjects objectPoolWriter objects
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
    updateStateWhenObjectOwtPool ::
      objectId ->
      DecisionGlobalState peerAddr objectId object ->
      DecisionGlobalState peerAddr objectId object
    updateStateWhenObjectOwtPool
      objectId
      st@DecisionGlobalState
        { dgsObjectsPendingMultiplicities
        , dgsObjectsOwtPoolMultiplicities
        , dgsPeerStates
        } =
        st
          { dgsObjectsPendingMultiplicities = dgsObjectsPendingMultiplicities'
          , dgsObjectsOwtPoolMultiplicities = dgsObjectsOwtPoolMultiplicities'
          , dgsPeerStates = dgsPeerStates'
          }
        where
        dgsObjectsPendingMultiplicities' = decreaseCount dgsObjectsPendingMultiplicities objectId
        dgsObjectsOwtPoolMultiplicities' = increaseCount dgsObjectsOwtPoolMultiplicities objectId

        dgsPeerStates' =
          Map.adjust
          (\ps@DecisionPeerState{dpsObjectsOwtPool, dpsObjectsPending} ->
            let object = case Map.lookup objectId dpsObjectsPending of
                            Just obj -> obj
                            Nothing -> error "ObjectDiffusion.updateStateWhenObjectOwtPool: the object should be in dpsObjectsPending"
            in ps{dpsObjectsPending = Map.delete objectId dpsObjectsPending, dpsObjectsOwtPool = Map.insert objectId object dpsObjectsOwtPool})
          peerAddr
          dgsPeerStates

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
