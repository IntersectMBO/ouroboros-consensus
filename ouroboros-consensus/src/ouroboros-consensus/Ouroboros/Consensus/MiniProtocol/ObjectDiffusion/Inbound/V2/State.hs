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
import Control.Monad.Class.MonadTime.SI
import Control.Tracer (Tracer, traceWith)
import Data.Foldable (toList)
import Data.Foldable qualified as Foldable
import Data.Functor (($>))
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map, findWithDefault)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Policy
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API (ObjectPoolWriter (opwHasObject, opwObjectId))
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsReq)
import System.Random (StdGen)

data SizeInBytes

strictSeqPartition :: (a -> Bool) -> StrictSeq a -> (StrictSeq a, StrictSeq a)
strictSeqPartition p xs = go xs StrictSeq.empty StrictSeq.empty
  where
    go StrictSeq.Empty trues falses = (trues, falses)
    go (y StrictSeq.:<| ys) trues falses
      | p y =
          go ys (trues StrictSeq.:|> y) falses
      | otherwise =
          go ys trues (falses StrictSeq.:|> y)

-- | Insert received `objectId`s and return the number of objectIds to be acknowledged with next request
-- and the updated `DecisionGlobalState`.
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
    let peerState =
          findWithDefault
            (error "ObjectDiffusion.handleReceivedIdsImpl: the peer should appear in dgsPeerStates")
            peerAddr
            dgsPeerStates
    
        -- Divide the new objectIds in two: those that are already in the objectpool
        -- and those that are not. We'll request some objects from the latter.
        (idsAlreadyInPoolSeq, newIdsAvailableSeq) =
          strictSeqPartition hasObject receivedIdsSeq

        -- TODO: Stopped there

        -- Add all `objectIds` from `dpsIdsAvailableMap` which are not
        -- unacknowledged or already buffered. Unacknowledged objectIds must have
        -- already been added to `dpsIdsAvailable` map before.
        dpsIdsAvailable' =
          StrictSeq.foldl'
            (\m objectId -> Set.insert objectId m)
            dpsIdsAvailable
            ( Set.filter
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
            ( flip $
                Map.alter
                  ( \case
                      Nothing -> Just $! 1
                      Just cnt -> Just $! succ cnt
                  )
            )
            dgsObjectsLiveMultiplicities
            receivedIdsSeq

        st' =
          st
            { dgsObjectsLiveMultiplicities = dgsObjectsLiveMultiplicities'
            }
        ps' =
          assert
            (dpsNumIdsInflight >= numIdsInitiallyRequested)
            ps
              { dpsIdsAvailable = dpsIdsAvailable'
              , dpsOutstandingFifo = dpsOutstandingFifo'
              , dpsNumIdsInflight = dpsNumIdsInflight - numIdsInitiallyRequested
              }
       in undefined

handleReceivedObjectsImpl ::
  forall peerAddr object objectId.
  ( Ord peerAddr
  , Ord objectId
  , Show objectId
  , Typeable objectId
  ) =>
  -- | compute object size
  (object -> SizeInBytes) ->
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
  objectSize
  peerAddr
  requestedObjectIdsMap
  receivedObjects
  st@DecisionGlobalState{dgsPeerStates} =
    -- using `alterF` so the update of `DecisionPeerState` is done in one lookup
    case Map.alterF
      (fmap Just . fn . fromJust)
      peerAddr
      dgsPeerStates of
      (Right st', dgsPeerStates') ->
        Right st'{dgsPeerStates = dgsPeerStates'}
      (Left e, _) ->
        Left $ ProtocolErrorObjectSizeError e
   where
    -- Update `DecisionPeerState` and partially update `DecisionGlobalState` (except of
    -- `dgsPeerStates`).
    fn ::
      DecisionPeerState objectId object ->
      ( Either
          [(objectId, SizeInBytes, SizeInBytes)]
          (DecisionGlobalState peerAddr objectId object)
      , DecisionPeerState objectId object
      )
    fn ps =
      case wrongSizedObjects of
        [] ->
          ( Right st'
          , ps''
          )
        _ ->
          ( Left wrongSizedObjects
          , ps
          )
     where
      wrongSizedObjects :: [(objectId, SizeInBytes, SizeInBytes)]
      wrongSizedObjects =
        map (\(a, (b, c)) -> (a, b, c))
          . Map.toList
          $ Map.merge
            Map.dropMissing
            Map.dropMissing
            ( Map.zipWithMaybeMatched \_ receivedSize advertisedSize ->
                if receivedSize `checkObjectSize` advertisedSize
                  then Nothing
                  else Just (receivedSize, advertisedSize)
            )
            (objectSize `Map.map` receivedObjects)
            requestedObjectIdsMap

      checkObjectSize ::
        SizeInBytes ->
        SizeInBytes ->
        Bool
      checkObjectSize received advertised
        | received > advertised =
            received - advertised <= const_MAX_OBJECT_SIZE_DISCREPENCY
        | otherwise =
            advertised - received <= const_MAX_OBJECT_SIZE_DISCREPENCY

      requestedObjectIds = Map.keysSet requestedObjectIdsMap
      notReceived = requestedObjectIds Set.\\ Map.keysSet receivedObjects
      dpsObjectsPending' = dpsObjectsPending ps <> receivedObjects
      -- Add not received objects to `dpsObjectsRequestedButNotReceivedIds` before acknowledging objectIds.
      dpsObjectsRequestedButNotReceivedIds' = dpsObjectsRequestedButNotReceivedIds ps <> notReceived

      dpsObjectsInflightIds' =
        assert (requestedObjectIds `Set.isSubsetOf` dpsObjectsInflightIds ps) $
          dpsObjectsInflightIds ps Set.\\ requestedObjectIds

      -- subtract requested from in-flight
      dgsObjectsInflightMultiplicities'' =
        Map.merge
          (Map.mapMaybeMissing \_ x -> Just x)
          (Map.mapMaybeMissing \_ _ -> assert False Nothing)
          ( Map.zipWithMaybeMatched \_ x y ->
              assert
                (x >= y)
                let z = x - y
                 in if z > 0
                      then Just z
                      else Nothing
          )
          (dgsObjectsInflightMultiplicities st)
          (Map.fromSet (const 1) requestedObjectIds)

      st' =
        st
          { dgsObjectsInflightMultiplicities = dgsObjectsInflightMultiplicities''
          }

      --
      -- Update DecisionPeerState
      --

      -- Remove the downloaded `objectId`s from the dpsIdsAvailable map, this
      -- guarantees that we won't attempt to download the `objectIds` from this peer
      -- once we collect the `objectId`s. Also restrict keys to `liveSet`.
      --
      -- NOTE: we could remove `notReceived` from `dpsIdsAvailable`; and
      -- possibly avoid using `dpsObjectsRequestedButNotReceivedIds` field at all.
      --
      dpsIdsAvailable'' = dpsIdsAvailable ps `Set.difference` requestedObjectIds

      -- Remove all acknowledged `objectId`s from unknown set, but only those
      -- which are not present in `dpsOutstandingFifo'`
      dpsObjectsRequestedButNotReceivedIds'' =
        dpsObjectsRequestedButNotReceivedIds'
          `Set.intersection` live
       where
        -- We cannot use `liveSet` as `unknown <> notReceived` might
        -- contain `objectIds` which are in `liveSet` but are not `live`.
        live = Set.fromList (toList (dpsOutstandingFifo ps))

      ps'' =
        ps
          { dpsIdsAvailable = dpsIdsAvailable''
          , dpsObjectsInflightIds = dpsObjectsInflightIds'
          , dpsObjectsPending = dpsObjectsPending'
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
      , dgsRententionTimeouts = Map.empty
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
        , dgsRententionTimeouts
        , dgsObjectsOwtPoolMultiplicities
        } =
        st
          { dgsPeerStates = dgsPeerStates'
          , dgsRententionTimeouts = dgsRententionTimeouts'
          , dgsObjectsLiveMultiplicities = dgsObjectsLiveMultiplicities'
          , dgsObjectsOwtPoolMultiplicities = dgsObjectsOwtPoolMultiplicities'
          }
        where
        dgsObjectsOwtPoolMultiplicities' =
          Map.update
            (\case 1 -> Nothing; n -> Just $! pred n)
            objectId
            dgsObjectsOwtPoolMultiplicities

        dgsRententionTimeouts' =
          Map.alter
            (\case Nothing -> Just [objectId]; Just objectIds -> Just (objectId : objectIds))
            (addTime dpMinObtainedButNotAckedObjectsLifetime now)
            dgsRententionTimeouts

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