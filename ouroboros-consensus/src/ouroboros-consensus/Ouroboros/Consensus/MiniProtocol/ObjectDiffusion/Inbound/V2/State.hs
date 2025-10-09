{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.State
  ( -- * Core API
    DecisionGlobalState (..)
  , DecisionPeerState (..)
  , DecisionGlobalStateVar
  , newDecisionGlobalStateVar
  , handleReceivedIds
  , handleReceivedObjects
  , submitObjectsToPool
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.Class.MonadTime.SI
import Control.Tracer (Tracer, traceWith)
import Data.Foldable qualified as Foldable
import Data.Functor (($>))
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map, findWithDefault)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Sequence qualified as Seq
import Data.Sequence.Strict (StrictSeq, fromStrict)
import Data.Set ((\\))
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Policy
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API (ObjectPoolWriter (opwHasObject, opwObjectId))
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsReq)
import System.Random (StdGen)

data SizeInBytes

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
      { dgsObjectsLiveMultiplicities = dgsObjectsLiveMultiplicities'
      , dgsPeerStates = dgsPeerStates'
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
  
      -- Divide the new objectIds in two: those that are already in the objectpool
      -- and those that are not. We'll request some objects from the latter.
      newIdsAvailableSeq =
        Seq.filter (not . hasObject) $ fromStrict receivedIdsSeq

      -- Add all `objectIds` from `dpsObjectsAvailableIdsMap` which are not
      -- unacknowledged or already buffered. Unacknowledged objectIds must have
      -- already been added to `dpsObjectsAvailableIds` map before.
      dpsObjectsAvailableIds' =
        Foldable.foldl'
          (\m objectId -> Set.insert objectId m)
          dpsObjectsAvailableIds
          ( Seq.filter
              ( \objectId ->
                  objectId `notElem` dpsOutstandingFifo
                    && objectId `Map.notMember` dgsObjectsLiveMultiplicities
              )
              newIdsAvailableSeq
          )

      -- Add received objectIds to `dpsOutstandingFifo`.
      dpsOutstandingFifo' = dpsOutstandingFifo <> receivedIdsSeq

      dgsObjectsLiveMultiplicities' =
        Foldable.foldl'
          decreaseCount
          dgsObjectsLiveMultiplicities
          receivedIdsSeq

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
  -- | requested objectIds
  Map objectId SizeInBytes ->
  -- | received objects
  Map objectId object ->
  DecisionGlobalState peerAddr objectId object ->
  -- | Return list of `objectId` which sizes didn't match or a new state.
  -- If one of the `object` has wrong size, we return an error.  The
  -- mini-protocol will throw, which will clean the state map from this peer.
  Either
    ObjectDiffusionInboundError
    (DecisionGlobalState peerAddr objectId object)
handleReceivedObjectsImpl
  peerAddr
  requestedObjectIdsMap
  receivedObjects
  st@DecisionGlobalState
    { dgsPeerStates
    , dgsObjectsInflightMultiplicities
    } = do
    
    let
      peerState@DecisionPeerState
        { dpsObjectsAvailableIds
        , dpsObjectsInflightIds
        , dpsObjectsPending
        } =
        findWithDefault
          (error "ObjectDiffusion.handleReceivedIdsImpl: the peer should appear in dgsPeerStates")
          peerAddr
          dgsPeerStates
      requestedObjectIds = Map.keysSet requestedObjectIdsMap
      receivedObjectIds = Map.keysSet receivedObjects
    when (not $ Set.null $ requestedObjectIds \\ receivedObjectIds) $
      Left ProtocolErrorObjectMissing
    when (not $ Set.null $ receivedObjectIds \\ requestedObjectIds) $
      Left ProtocolErrorObjectNotRequested
    -- past that point we know that `requestedObjectIds` == `receivedObjectIds`
    let
      dpsObjectsPending' = dpsObjectsPending <> receivedObjects

      -- subtract requested from in-flight
      dpsObjectsInflightIds' =
        assert (requestedObjectIds `Set.isSubsetOf` dpsObjectsInflightIds) $
          dpsObjectsInflightIds \\ requestedObjectIds

      dgsObjectsInflightMultiplicities' =
        Map.foldrWithKey
          (\objectId count m ->
            if objectId `Set.member` requestedObjectIds && count > 1
              then Map.insert objectId (count-1) m
              else m
          )
          Map.empty
          dgsObjectsInflightMultiplicities

      -- Update DecisionPeerState
      --
      -- Remove the downloaded `objectId`s from the dpsObjectsAvailableIds map, this
      -- guarantees that we won't attempt to download the `objectIds` from
      -- this peer twice.
      dpsObjectsAvailableIds'' = dpsObjectsAvailableIds `Set.difference` requestedObjectIds

      peerState' =
        peerState
          { dpsObjectsAvailableIds = dpsObjectsAvailableIds''
          , dpsObjectsInflightIds = dpsObjectsInflightIds'
          , dpsObjectsPending = dpsObjectsPending'
          }

      dgsPeerStates' = Map.insert peerAddr peerState' dgsPeerStates

    pure $ st
      { dgsObjectsInflightMultiplicities = dgsObjectsInflightMultiplicities'
      , dgsPeerStates = dgsPeerStates'
      }

--
-- Monadic public API
--

type DecisionGlobalStateVar m peerAddr objectId object =
  StrictTVar m (DecisionGlobalState peerAddr objectId object)

newDecisionGlobalStateVar ::
  MonadSTM m =>
  StdGen ->
  m (DecisionGlobalStateVar m peerAddr objectId object)
newDecisionGlobalStateVar rng =
  newTVarIO
    DecisionGlobalState
      { dgsPeerStates = Map.empty
      , dgsObjectsInflightMultiplicities = Map.empty
      , dgsObjectsLiveMultiplicities = Map.empty
      , dgsObjectsOwtPoolMultiplicities = Map.empty
      , dgsRng = rng
      }

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

-- | Include received `object`s in `DecisionGlobalState`.  Return number of `objectIds`
-- to be acknowledged and list of `object` to be added to the objectpool.
handleReceivedObjects ::
  forall m peerAddr object objectId.
  ( MonadSTM m
  , Ord objectId
  , Ord peerAddr
  , Show objectId
  , Typeable objectId
  ) =>
  Tracer m (TraceDecisionLogic peerAddr objectId object) ->
  (object -> SizeInBytes) ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  peerAddr ->
  -- | set of requested objectIds with their announced size
  Map objectId SizeInBytes ->
  -- | received objects
  Map objectId object ->
  -- | number of objectIds to be acknowledged and objects to be added to the
  -- objectpool
  m (Maybe ObjectDiffusionInboundError)
handleReceivedObjects tracer objectSize globalStateVar peerAddr objectIdsRequested objectsMap = do
  r <- atomically $ do
    st <- readTVar globalStateVar
    case handleReceivedObjectsImpl objectSize peerAddr objectIdsRequested objectsMap st of
      r@(Right st') ->
        writeTVar globalStateVar st'
          $> r
      r@Left{} -> pure r
  case r of
    Right st ->
      traceWith tracer (TraceDecisionLogicGlobalStateUpdated "handleReceivedObjects" st)
        $> Nothing
    Left e -> return (Just e)

submitObjectsToPool ::
  Tracer m (TraceObjectDiffusionInbound objectId object) -> ObjectPoolWriter objectId object m -> [object] -> m ()
submitObjectsToPool tracer objectPoolWriter objects =
  bracket_
    (atomically $ waitTSem poolSem)
    (atomically $ signalTSem poolSem)
    $ do
      opwAddObjects objectPoolWriter objects
      now <- getMonotonicTime
      traceWith tracer (TraceObjectDiffusionInboundSubmittedObjects (NumObjectsProcessed $ length objects))
      atomically $
        let getId = opwObjectId objectPoolWriter in
        modifyTVar globalStateVar $ \globalState ->
          foldl' (\st object -> updateObjectsOwtPool now (getId object) object st) globalState
  where
    updateObjectsOwtPool ::
      Time ->
      objectId ->
      object ->
      DecisionGlobalState peerAddr objectId object ->
      DecisionGlobalState peerAddr objectId object
    updateObjectsOwtPool
      now
      objectId
      st@DecisionGlobalState
        { dgsPeerStates
        , dgsObjectsLiveMultiplicities
        , dgsObjectsOwtPoolMultiplicities
        } =
        st
          { dgsPeerStates = dgsPeerStates'
          , dgsObjectsLiveMultiplicities = dgsObjectsLiveMultiplicities'
          , dgsObjectsOwtPoolMultiplicities = dgsObjectsOwtPoolMultiplicities'
          }
        where
        dgsObjectsOwtPoolMultiplicities' =
          Map.update
            (\case 1 -> Nothing; n -> Just $! pred n)
            objectId
            dgsObjectsOwtPoolMultiplicities

        dgsObjectsLiveMultiplicities' =
          Map.alter
            (\case Nothing -> Just 1; Just n -> Just $! succ n)
            objectId
            dgsObjectsLiveMultiplicities

        dgsPeerStates' =
          Map.update
          (\ps -> Just $! ps{dpsObjectsPending = Map.insert objectId object (dpsObjectsPending ps)})
          peerAddr
          dgsPeerStates