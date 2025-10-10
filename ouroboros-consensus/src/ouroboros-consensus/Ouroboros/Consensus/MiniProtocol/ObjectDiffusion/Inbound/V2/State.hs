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
import Data.Sequence qualified as Seq
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set ((\\), Set)
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API (ObjectPoolWriter (..))
import Ouroboros.Consensus.Util.IOLike (MonadMask, MonadMVar, bracket_)
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsReq)
import System.Random (StdGen)

strictSeqToSet :: Ord a => StrictSeq a -> Set a
strictSeqToSet = Foldable.foldl' (flip Set.insert) Set.empty

-- | Insert received `objectId`s and return the number of objectIds to be acknowledged with next request
-- and the updated `DecisionGlobalState`.
-- TODO: check for possible errors in the peer response, raise exception if it happened
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
  -- | sequence of received `objectIds`
  StrictSeq objectId ->
  DecisionGlobalState peerAddr objectId object ->
  DecisionGlobalState peerAddr objectId object
handleReceivedIdsImpl
  hasObject
  peerAddr
  numIdsInitiallyRequested
  receivedIdsSeq
  st@DecisionGlobalState
    { dgsPeerStates
    , dgsObjectsLiveMultiplicities
    } =
    st
      { dgsPeerStates = dgsPeerStates'
      }
    where
      peerState@DecisionPeerState
        { dpsOutstandingFifo
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
            && objectId `notElem` dpsOutstandingFifo -- object hasn't been advertised before by the outbound peer (this covers the cases where the object would be in flight, or already downloaded but no acknowledged yet, by the current peer)
               -- TODO; the condition below is problematic because it would prevent requesting an object from two different peers if the first one has already requested it (i.e. it is in flight)!
            && objectId `Map.notMember` dgsObjectsLiveMultiplicities -- the object is not currently in flight or further in the processing from another peer
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
    } =
    -- TODO: error handling should be done by the client before using the API
    -- past that point we assume:
    -- assert (objectsRequestedIds `Set.isSubsetOf` dpsObjectsInflightIds) $
    -- assert (objectsReceivedIds == objectsRequestedIds) $
    st
      { dgsObjectsInflightMultiplicities = dgsObjectsInflightMultiplicities'
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

      dpsObjectsPending' = dpsObjectsPending <> objectsReceived

      -- subtract requested from in-flight
      dpsObjectsInflightIds' =
        dpsObjectsInflightIds \\ objectsReceivedIds

      dgsObjectsInflightMultiplicities' =
        Foldable.foldl'
          decreaseCount
          dgsObjectsInflightMultiplicities
          objectsReceivedIds

      -- Update DecisionPeerState
      --
      -- Remove the downloaded `objectId`s from the dpsObjectsAvailableIds map, this
      -- guarantees that we won't attempt to download the `objectIds` from
      -- this peer twice.
      -- TODO: this is wrong, it should be done earlier when the request for objects is emitted
      -- aka in when the decision for this peer is emitted/read?
      -- dpsObjectsAvailableIds'' = dpsObjectsAvailableIds `Set.difference` objectsReceivedIds

      peerState' =
        peerState
          { dpsObjectsInflightIds = dpsObjectsInflightIds'
          , dpsObjectsPending = dpsObjectsPending'
          }

      dgsPeerStates' = Map.insert peerAddr peerState' dgsPeerStates

--
-- Monadic public API
--

-- | Wrapper around `handleReceivedIdsImpl`.
-- Obtain the `hasObject` function atomically from the STM context and
-- updates and traces the global state TVar.
handleReceivedIds ::
  forall m peerAddr object objectId.
  (MonadSTM m, Ord objectId, Ord peerAddr) =>
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
handleReceivedIds tracer globalStateVar objectPoolWriter peerAddr numIdsInitiallyRequested receivedIdsSeq = do
  globalState' <- atomically $ do
    hasObject <- opwHasObject objectPoolWriter
    stateTVar
      globalStateVar
      ( \globalState -> let globalState' = handleReceivedIdsImpl hasObject peerAddr numIdsInitiallyRequested receivedIdsSeq globalState
         in (globalState', globalState') )
  traceWith tracer (TraceDecisionLogicGlobalStateUpdated "handleReceivedIds" globalState')

-- | Wrapper around `handleReceivedObjectsImpl` that updates and traces the
-- global state TVar.
handleReceivedObjects ::
  forall m peerAddr object objectId.
  ( MonadSTM m
  , Ord objectId
  , Ord peerAddr
  ) =>
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  peerAddr ->
  -- | received objects
  Map objectId object ->
  m ()
handleReceivedObjects tracer globalStateVar peerAddr objectsReceived = do
  globalState' <- atomically $ do
    stateTVar
      globalStateVar
      ( \globalState -> let globalState' = handleReceivedObjectsImpl peerAddr objectsReceived globalState
         in (globalState', globalState') )
  traceWith tracer (TraceDecisionLogicGlobalStateUpdated "handleReceivedObjects" globalState')

submitObjectsToPool ::
  forall m peerAddr object objectId.
  ( Ord objectId
  , Ord peerAddr
  , MonadMask m
  , MonadMVar m
  , MonadSTM m
  ) =>
  Tracer m (TraceObjectDiffusionInbound objectId object) ->
  ObjectPoolSem m ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  ObjectPoolWriter objectId object m ->
  peerAddr ->
  [object] ->
  m ()
submitObjectsToPool
  tracer
  (ObjectPoolSem poolSem)
  globalStateVar
  objectPoolWriter
  peerAddr
  objects =
  bracket_
    (atomically $ waitTSem poolSem)
    (atomically $ signalTSem poolSem)
    $ do
      opwAddObjects objectPoolWriter objects
      traceWith tracer $
        TraceObjectDiffusionInboundCollectedObjects $
        NumObjectsProcessed $ fromIntegral $ length objects
      atomically $
        let getId = opwObjectId objectPoolWriter in
        modifyTVar globalStateVar $ \globalState ->
          Foldable.foldl'
            (\st object -> updateStateWhenObjectAddedToPool (getId object) st)
            globalState
            objects
  where
    updateStateWhenObjectAddedToPool ::
      objectId ->
      DecisionGlobalState peerAddr objectId object ->
      DecisionGlobalState peerAddr objectId object
    updateStateWhenObjectAddedToPool
      objectId
      st@DecisionGlobalState
        { dgsObjectsLiveMultiplicities
        , dgsObjectsOwtPoolMultiplicities
        , dgsPeerStates
        } =
        st
          { dgsObjectsLiveMultiplicities = dgsObjectsLiveMultiplicities'
          , dgsObjectsOwtPoolMultiplicities = dgsObjectsOwtPoolMultiplicities'
          , dgsPeerStates = dgsPeerStates'
          }
        where
        dgsObjectsOwtPoolMultiplicities' = decreaseCount dgsObjectsOwtPoolMultiplicities objectId
        dgsObjectsLiveMultiplicities' = decreaseCount dgsObjectsLiveMultiplicities objectId

        dgsPeerStates' =
          Map.adjust
          (\ps@DecisionPeerState{dpsObjectsOwtPool} -> ps{dpsObjectsOwtPool = Map.delete objectId dpsObjectsOwtPool})
          peerAddr
          dgsPeerStates
