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
  , onReceiveIds
  , onReceiveObjects
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Concurrent.Class.MonadSTM.TSem
import Control.Exception (assert)
import Control.Tracer (Tracer, traceWith)
import Data.Foldable qualified as Foldable
import Data.Map.Strict (Map, findWithDefault)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API (ObjectPoolWriter (..))
import Ouroboros.Consensus.Util.IOLike (MonadMVar, MonadMask, bracket_)
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsAck, NumObjectIdsReq)

onRequestIds ::
  forall m peerAddr object objectId.
  (MonadSTM m, Ord objectId, Ord peerAddr) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  peerAddr ->
  NumObjectIdsAck ->
  -- | number of requests to req
  NumObjectIdsReq ->
  m ()
onRequestIds odTracer decisionTracer globalStateVar peerAddr numIdsToAck numIdsToReq = do
  globalState' <- atomically $ do
    stateTVar
      globalStateVar
      ( \globalState ->
          let globalState' = onRequestIdsImpl peerAddr numIdsToAck numIdsToReq globalState
           in (globalState', globalState')
      )
  traceWith odTracer (TraceObjectDiffusionInboundRequestedIds (fromIntegral numIdsToReq))
  traceWith decisionTracer (TraceDecisionLogicGlobalStateUpdated "onRequestIds" globalState')

-- Acknowledgment is done when a requestIds is made.
-- That's why we update the dpsOutstandingFifo and dpsObjectsAvailableIds here.
onRequestIdsImpl ::
  forall peerAddr object objectId.
  (Ord objectId, Ord peerAddr) =>
  peerAddr ->
  NumObjectIdsAck ->
  -- | number of requests to req
  NumObjectIdsReq ->
  DecisionGlobalState peerAddr objectId object ->
  DecisionGlobalState peerAddr objectId object
onRequestIdsImpl
  peerAddr
  numIdsToAck
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
        ( \ps@DecisionPeerState{dpsNumIdsInflight, dpsOutstandingFifo, dpsObjectsAvailableIds} ->
            -- we isolate the longest prefix of outstandingFifo that matches our ack criteria (see above in computeAck doc)
            let
              -- We compute the ids to ack and new state of the FIFO based on the number of ids to ack given by the decision logic
              (idsToAck, dpsOutstandingFifo') =
                StrictSeq.splitAt
                  (fromIntegral numIdsToAck)
                  dpsOutstandingFifo

              -- We remove the acknowledged ids from dpsObjectsAvailableIds if they were present.
              -- We need to do that because objects that were advertised by this corresponding outbound peer
              -- but never downloaded because we already have them in pool were consequently never removed
              -- from dpsObjectsAvailableIds by onRequestObjects
              dpsObjectsAvailableIds' =
                Foldable.foldl' (\set objectId -> Set.delete objectId set) dpsObjectsAvailableIds idsToAck
             in
              ps
                { dpsNumIdsInflight = dpsNumIdsInflight + numIdsToReq
                , dpsOutstandingFifo = dpsOutstandingFifo'
                , dpsObjectsAvailableIds = dpsObjectsAvailableIds'
                }
        )
        peerAddr
        dgsPeerStates

onRequestObjects ::
  forall m peerAddr object objectId.
  (MonadSTM m, Ord objectId, Ord peerAddr) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  peerAddr ->
  -- | objets to request, by id
  Set objectId ->
  m ()
onRequestObjects odTracer decisionTracer globalStateVar peerAddr objectIds = do
  globalState' <- atomically $ do
    stateTVar
      globalStateVar
      ( \globalState ->
          let globalState' = onRequestObjectsImpl peerAddr objectIds globalState
           in (globalState', globalState')
      )
  traceWith odTracer (TraceObjectDiffusionInboundRequestedObjects (Set.size objectIds))
  traceWith decisionTracer (TraceDecisionLogicGlobalStateUpdated "onRequestObjects" globalState')

onRequestObjectsImpl ::
  forall peerAddr object objectId.
  (Ord objectId, Ord peerAddr) =>
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
    } =
    globalState
      { dgsPeerStates = dgsPeerStates'
      }
   where
    dgsPeerStates' =
      Map.adjust
        ( \ps@DecisionPeerState{dpsObjectsAvailableIds, dpsObjectsInflightIds} ->
            ps
              { dpsObjectsAvailableIds = dpsObjectsAvailableIds \\ objectIds
              , dpsObjectsInflightIds = dpsObjectsInflightIds `Set.union` objectIds
              }
        )
        peerAddr
        dgsPeerStates

-- | Wrapper around `onReceiveIdsImpl`.
-- Obtain the `hasObject` function atomically from the STM context and
-- updates and traces the global state TVar.
onReceiveIds ::
  forall m peerAddr object objectId.
  (MonadSTM m, Ord objectId, Ord peerAddr) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  peerAddr ->
  -- | number of requests to subtract from
  -- `dpsNumIdsInflight`
  NumObjectIdsReq ->
  -- | sequence of received `objectIds`
  [objectId] ->
  -- | received `objectId`s
  m ()
onReceiveIds odTracer decisionTracer globalStateVar peerAddr numIdsInitiallyRequested receivedIds = do
  globalState' <- atomically $ do
    stateTVar
      globalStateVar
      ( \globalState ->
          let globalState' = onReceiveIdsImpl peerAddr numIdsInitiallyRequested receivedIds globalState
           in (globalState', globalState')
      )
  traceWith odTracer (TraceObjectDiffusionInboundReceivedIds (length receivedIds))
  traceWith decisionTracer (TraceDecisionLogicGlobalStateUpdated "onReceiveIds" globalState')

onReceiveIdsImpl ::
  forall peerAddr object objectId.
  (Ord objectId, Ord peerAddr, HasCallStack) =>
  peerAddr ->
  -- | number of requests to subtract from
  -- `dpsNumIdsInflight`
  NumObjectIdsReq ->
  -- | sequence of received `objectId`s
  [objectId] ->
  DecisionGlobalState peerAddr objectId object ->
  DecisionGlobalState peerAddr objectId object
onReceiveIdsImpl
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
      , dpsObjectsAvailableIds
      , dpsNumIdsInflight
      } =
        findWithDefault
          (error "ObjectDiffusion.onReceiveIdsImpl: the peer should appear in dgsPeerStates")
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

-- | Wrapper around `onReceiveObjectsImpl` that updates and traces the
-- global state TVar.
--
-- Error handling should be done by the client before using the API.
-- In particular we assume:
-- assert (objectsRequestedIds `Set.isSubsetOf` dpsObjectsInflightIds)
--
-- IMPORTANT: We also assume that every object has been *validated* before being passed to this function.
onReceiveObjects ::
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
onReceiveObjects odTracer tracer globalStateVar objectPoolWriter poolSem peerAddr objectsReceived = do
  let getId = opwObjectId objectPoolWriter
  let objectsReceivedMap = Map.fromList $ (\obj -> (getId obj, obj)) <$> objectsReceived

  globalState' <- atomically $ do
    stateTVar
      globalStateVar
      ( \globalState ->
          let globalState' =
                onReceiveObjectsImpl
                  peerAddr
                  objectsReceivedMap
                  globalState
           in (globalState', globalState')
      )
  traceWith odTracer (TraceObjectDiffusionInboundReceivedObjects (length objectsReceived))
  traceWith tracer (TraceDecisionLogicGlobalStateUpdated "onReceiveObjects" globalState')
  submitObjectsToPool
    odTracer
    tracer
    globalStateVar
    objectPoolWriter
    poolSem
    peerAddr
    objectsReceivedMap

onReceiveObjectsImpl ::
  forall peerAddr object objectId.
  ( Ord peerAddr
  , Ord objectId
  ) =>
  peerAddr ->
  -- | received objects
  Map objectId object ->
  DecisionGlobalState peerAddr objectId object ->
  DecisionGlobalState peerAddr objectId object
onReceiveObjectsImpl
  peerAddr
  objectsReceived
  st@DecisionGlobalState
    { dgsPeerStates
    } =
    st
      { dgsPeerStates = dgsPeerStates'
      }
   where
    objectsReceivedIds = Map.keysSet objectsReceived

    peerState@DecisionPeerState
      { dpsObjectsInflightIds
      , dpsObjectsOwtPool
      } =
        findWithDefault
          (error "ObjectDiffusion.onReceiveObjectsImpl: the peer should appear in dgsPeerStates")
          peerAddr
          dgsPeerStates

    -- subtract requested from in-flight
    dpsObjectsInflightIds' =
      dpsObjectsInflightIds \\ objectsReceivedIds

    dpsObjectsOwtPool' = dpsObjectsOwtPool <> objectsReceived

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
          TraceObjectDiffusionInboundAddedObjects $
            length objects

        -- Move objects from `owtPool` to `inPool` state
        globalState' <- atomically $ stateTVar globalStateVar $ \globalState ->
          let globalState' =
                Foldable.foldl'
                  (\st object -> updateStateWhenObjectAddedToPool (getId object) st)
                  globalState
                  objects
           in (globalState', globalState')
        traceWith
          decisionTracer
          ( TraceDecisionLogicGlobalStateUpdated
              "submitObjectsToPool.updateStateWhenObjectAddedToPool"
              globalState'
          )
   where
    updateStateWhenObjectAddedToPool ::
      objectId ->
      DecisionGlobalState peerAddr objectId object ->
      DecisionGlobalState peerAddr objectId object
    updateStateWhenObjectAddedToPool
      objectId
      st@DecisionGlobalState
        { dgsPeerStates
        } =
        st
          { dgsPeerStates = dgsPeerStates'
          }
       where
        dgsPeerStates' =
          Map.adjust
            ( \ps@DecisionPeerState{dpsObjectsOwtPool} -> ps{dpsObjectsOwtPool = Map.delete objectId dpsObjectsOwtPool}
            )
            peerAddr
            dgsPeerStates
