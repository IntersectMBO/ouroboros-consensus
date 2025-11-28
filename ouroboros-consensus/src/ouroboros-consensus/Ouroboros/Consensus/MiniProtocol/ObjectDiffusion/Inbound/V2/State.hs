{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.State
  ( -- * Core API
    PeerState (..)
  , onRequestIds
  , onRequestObjects
  , onReceiveIds
  , onReceiveObjects
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Concurrent.Class.MonadSTM.TSem
import Control.Exception (assert, throw)
import Control.Monad (when)
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
import Ouroboros.Consensus.Util.IOLike (MonadMask, bracket_)
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsAck, NumObjectIdsReq)

onRequestIds ::
  forall m peerAddr object objectId.
  (MonadSTM m, Ord objectId, Ord peerAddr) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  PeerStatesVar m peerAddr objectId object ->
  peerAddr ->
  NumObjectIdsAck ->
  -- | number of requests to req
  NumObjectIdsReq ->
  m ()
onRequestIds
  odTracer
  decisionTracer
  peerStatesVar
  peerAddr
  numIdsToAck
  numIdsToReq = do
    peerStates' <- atomically $ do
      stateTVar
        peerStatesVar
        ( \peerStates ->
            let peerStates' = onRequestIdsImpl peerAddr numIdsToAck numIdsToReq peerStates
             in (peerStates', peerStates')
        )
    traceWith odTracer (TraceObjectDiffusionInboundRequestedIds (fromIntegral numIdsToReq))
    traceWith decisionTracer (TraceDecisionLogicPeerStatesUpdated "onRequestIds" peerStates')

-- Acknowledgment is done when a requestIds is made.
-- That's why we update the psOutstandingFifo and psObjectsAvailableIds here.
onRequestIdsImpl ::
  forall peerAddr object objectId.
  (Ord objectId, Ord peerAddr) =>
  peerAddr ->
  NumObjectIdsAck ->
  -- | number of requests to req
  NumObjectIdsReq ->
  Map peerAddr (PeerState objectId object) ->
  Map peerAddr (PeerState objectId object)
onRequestIdsImpl
  peerAddr
  numIdsToAck
  numIdsToReq
  peerStates =
    Map.adjust
      ( \ps@PeerState{psNumIdsInflight, psOutstandingFifo, psObjectsAvailableIds} ->
          -- we isolate the longest prefix of outstandingFifo that matches our
          -- ack criteria (see above in computeAck doc)
          let
            -- We compute the ids to ack and new state of the FIFO based on the
            -- number of ids to ack given by the decision logic
            (idsToAck, psOutstandingFifo') =
              assert (StrictSeq.length psOutstandingFifo >= fromIntegral numIdsToAck) $
                StrictSeq.splitAt
                  (fromIntegral numIdsToAck)
                  psOutstandingFifo

            -- We remove the acknowledged ids from psObjectsAvailableIds if they
            -- were present.
            -- We need to do that because objects that were advertised by this
            -- corresponding outbound peer but never downloaded because we
            -- already have them in pool were consequently never removed
            -- from psObjectsAvailableIds by onRequestObjects
            psObjectsAvailableIds' =
              Foldable.foldl'
                (\set objectId -> Set.delete objectId set)
                psObjectsAvailableIds
                idsToAck
           in
            ps
              { psNumIdsInflight = psNumIdsInflight + numIdsToReq
              , psOutstandingFifo = psOutstandingFifo'
              , psObjectsAvailableIds = psObjectsAvailableIds'
              }
      )
      peerAddr
      peerStates

onRequestObjects ::
  forall m peerAddr object objectId.
  (MonadSTM m, Ord objectId, Ord peerAddr) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  PeerStatesVar m peerAddr objectId object ->
  peerAddr ->
  -- | objets to request, by id
  Set objectId ->
  m ()
onRequestObjects odTracer decisionTracer peerStatesVar peerAddr objectIds = do
  peerStates' <- atomically $ do
    stateTVar
      peerStatesVar
      ( \peerStates ->
          let peerStates' = onRequestObjectsImpl peerAddr objectIds peerStates
           in (peerStates', peerStates')
      )
  traceWith odTracer (TraceObjectDiffusionInboundRequestedObjects (Set.size objectIds))
  traceWith decisionTracer (TraceDecisionLogicPeerStatesUpdated "onRequestObjects" peerStates')

onRequestObjectsImpl ::
  forall peerAddr object objectId.
  (Ord objectId, Ord peerAddr) =>
  peerAddr ->
  -- | objets to request, by id
  Set objectId ->
  Map peerAddr (PeerState objectId object) ->
  Map peerAddr (PeerState objectId object)
onRequestObjectsImpl
  peerAddr
  objectIds
  peerStates =
    Map.adjust
      ( \ps@PeerState{psObjectsAvailableIds, psObjectsInflightIds} ->
          assert
            ( objectIds `Set.isSubsetOf` psObjectsAvailableIds
                && Set.null (objectIds `Set.intersection` psObjectsInflightIds)
            )
            $ ps
              { psObjectsAvailableIds = psObjectsAvailableIds \\ objectIds
              , psObjectsInflightIds = psObjectsInflightIds `Set.union` objectIds
              }
      )
      peerAddr
      peerStates

-- | Wrapper around `onReceiveIdsImpl`.
-- Obtain the `hasObject` function atomically from the STM context and
-- updates and traces the global state TVar.
onReceiveIds ::
  forall m peerAddr object objectId.
  (MonadSTM m, Ord objectId, Ord peerAddr) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  ObjectPoolWriter objectId object m ->
  PeerStatesVar m peerAddr objectId object ->
  peerAddr ->
  -- | number of requests to subtract from
  -- `psNumIdsInflight`
  NumObjectIdsReq ->
  -- | sequence of received `objectIds`
  [objectId] ->
  -- | received `objectId`s
  m ()
onReceiveIds
  odTracer
  decisionTracer
  ObjectPoolWriter{opwHasObject}
  peerStatesVar
  peerAddr
  numIdsInitiallyRequested
  receivedIds = do
    peerStates' <- atomically $ do
      peerState <- (Map.! peerAddr) <$> readTVar peerStatesVar
      hasObject <- opwHasObject
      checkProtocolErrors hasObject peerState numIdsInitiallyRequested receivedIds
      stateTVar
        peerStatesVar
        ( \peerStates ->
            let peerStates' =
                  onReceiveIdsImpl
                    peerAddr
                    numIdsInitiallyRequested
                    receivedIds
                    peerStates
             in (peerStates', peerStates')
        )
    traceWith odTracer (TraceObjectDiffusionInboundReceivedIds (length receivedIds))
    traceWith decisionTracer (TraceDecisionLogicPeerStatesUpdated "onReceiveIds" peerStates')
   where
    checkProtocolErrors ::
      (objectId -> Bool) ->
      PeerState objectId object ->
      NumObjectIdsReq ->
      [objectId] ->
      STM m ()
    checkProtocolErrors
      hasObject
      PeerState{psObjectsAvailableIds, psObjectsInflightIds}
      nReq
      ids = do
        when (length ids > fromIntegral nReq) $ throw ProtocolErrorObjectIdsNotRequested
        let idSet = Set.fromList ids
        when (length ids /= Set.size idSet) $ throw ProtocolErrorObjectIdsDuplicate
        when
          ( (not $ Set.null $ idSet `Set.intersection` psObjectsAvailableIds)
              || (not $ Set.null $ idSet `Set.intersection` psObjectsInflightIds)
              || (any hasObject ids)
          )
          $ throw ProtocolErrorObjectIdAlreadyKnown

onReceiveIdsImpl ::
  forall peerAddr object objectId.
  (Ord objectId, Ord peerAddr, HasCallStack) =>
  peerAddr ->
  -- | number of requests to subtract from
  -- `psNumIdsInflight`
  NumObjectIdsReq ->
  -- | sequence of received `objectId`s
  [objectId] ->
  Map peerAddr (PeerState objectId object) ->
  Map peerAddr (PeerState objectId object)
onReceiveIdsImpl
  peerAddr
  numIdsInitiallyRequested
  receivedIds
  peerStates = peerStates'
   where
    peerState@PeerState
      { psOutstandingFifo
      , psObjectsAvailableIds
      , psNumIdsInflight
      } =
        findWithDefault
          (error "ObjectDiffusion.onReceiveIdsImpl: the peer should appear in peerStates")
          peerAddr
          peerStates

    -- Actually we don't need to filter out availableIds, because
    -- makeDecisions is the only reader of psObjectsAvailableIds
    -- and will filter it when needed with the actualized state of the object
    -- pool.
    psObjectsAvailableIds' =
      psObjectsAvailableIds `Set.union` Set.fromList receivedIds

    -- Add received objectIds to `psOutstandingFifo`.
    psOutstandingFifo' = psOutstandingFifo <> StrictSeq.fromList receivedIds

    peerState' =
      assert
        (psNumIdsInflight >= numIdsInitiallyRequested)
        peerState
          { psObjectsAvailableIds = psObjectsAvailableIds'
          , psOutstandingFifo = psOutstandingFifo'
          , psNumIdsInflight = psNumIdsInflight - numIdsInitiallyRequested
          }

    peerStates' = Map.insert peerAddr peerState' peerStates

-- | Wrapper around `onReceiveObjectsImpl` that updates and traces the
-- global state TVar.
onReceiveObjects ::
  forall m peerAddr object objectId.
  ( MonadSTM m
  , MonadMask m
  , Ord objectId
  , Ord peerAddr
  ) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  PeerStatesVar m peerAddr objectId object ->
  ObjectPoolWriter objectId object m ->
  ObjectPoolSem m ->
  peerAddr ->
  -- | requested objects
  Set objectId ->
  -- | received objects
  [object] ->
  m ()
onReceiveObjects
  odTracer
  tracer
  peerStatesVar
  objectPoolWriter
  poolSem
  peerAddr
  objectsRequestedIds
  objectsReceived = do
    let getId = opwObjectId objectPoolWriter
    let objectsReceivedMap =
          Map.fromList $ (\obj -> (getId obj, obj)) <$> objectsReceived
    checkProtocolErrors objectsRequestedIds objectsReceivedMap
    peerStates' <- atomically $ do
      stateTVar
        peerStatesVar
        ( \peerStates ->
            let peerStates' =
                  onReceiveObjectsImpl
                    peerAddr
                    objectsReceivedMap
                    peerStates
             in (peerStates', peerStates')
        )
    traceWith odTracer (TraceObjectDiffusionInboundReceivedObjects (length objectsReceived))
    traceWith tracer (TraceDecisionLogicPeerStatesUpdated "onReceiveObjects" peerStates')
    submitObjectsToPool
      odTracer
      tracer
      peerStatesVar
      objectPoolWriter
      poolSem
      peerAddr
      objectsReceivedMap
   where
    checkProtocolErrors ::
      Set objectId ->
      Map objectId object ->
      m ()
    checkProtocolErrors requested received' = do
      let received = Map.keysSet received'
      when (not $ Set.null $ requested \\ received) $ throw ProtocolErrorObjectMissing
      when (not $ Set.null $ received \\ requested) $ throw ProtocolErrorObjectNotRequested

onReceiveObjectsImpl ::
  forall peerAddr object objectId.
  ( Ord peerAddr
  , Ord objectId
  ) =>
  peerAddr ->
  -- | received objects
  Map objectId object ->
  Map peerAddr (PeerState objectId object) ->
  Map peerAddr (PeerState objectId object)
onReceiveObjectsImpl
  peerAddr
  objectsReceived
  peerStates = peerStates'
   where
    objectsReceivedIds = Map.keysSet objectsReceived

    peerState@PeerState
      { psObjectsInflightIds
      , psObjectsOwtPool
      } =
        findWithDefault
          (error "ObjectDiffusion.onReceiveObjectsImpl: the peer should appear in peerStates")
          peerAddr
          peerStates

    -- subtract requested from in-flight
    psObjectsInflightIds' =
      assert (objectsReceivedIds `Set.isSubsetOf` psObjectsInflightIds) $
        psObjectsInflightIds \\ objectsReceivedIds

    psObjectsOwtPool' = psObjectsOwtPool <> objectsReceived

    peerState' =
      peerState
        { psObjectsInflightIds = psObjectsInflightIds'
        , psObjectsOwtPool = psObjectsOwtPool'
        }

    peerStates' = Map.insert peerAddr peerState' peerStates

submitObjectsToPool ::
  forall m peerAddr object objectId.
  ( Ord objectId
  , Ord peerAddr
  , MonadMask m
  , MonadSTM m
  ) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  PeerStatesVar m peerAddr objectId object ->
  ObjectPoolWriter objectId object m ->
  ObjectPoolSem m ->
  peerAddr ->
  Map objectId object ->
  m ()
submitObjectsToPool
  odTracer
  decisionTracer
  peerStatesVar
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
        peerStates' <- atomically $ stateTVar peerStatesVar $ \peerStates ->
          let peerStates' =
                Foldable.foldl'
                  (\st object -> removeObjectsFromStateWhenAddedToPool (getId object) st)
                  peerStates
                  objects
           in (peerStates', peerStates')
        traceWith
          decisionTracer
          ( TraceDecisionLogicPeerStatesUpdated
              "submitObjectsToPool.removeObjectsFromStateWhenAddedToPool"
              peerStates'
          )
   where
    removeObjectsFromStateWhenAddedToPool ::
      objectId ->
      Map peerAddr (PeerState objectId object) ->
      Map peerAddr (PeerState objectId object)
    removeObjectsFromStateWhenAddedToPool
      objectId
      peerStates =
        Map.adjust
          ( \ps@PeerState{psObjectsOwtPool} ->
              ps{psObjectsOwtPool = Map.delete objectId psObjectsOwtPool}
          )
          peerAddr
          peerStates
