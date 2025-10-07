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
  , acknowledgeObjectIds
  , splitAcknowledgedObjectIds
  , tickTimedObjects
  , const_MAX_OBJECT_SIZE_DISCREPENCY

    -- * Internals, only exported for testing purposes:
  , RefCountDiff (..)
  , updateRefCounts
  , handleReceivedIdsImpl
  , handleReceivedObjectsImpl
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
import Control.Exception (assert)
import Control.Monad.Class.MonadTime.SI
import Control.Tracer (Tracer, traceWith)
import Data.Foldable (fold, toList)
import Data.Foldable qualified as Foldable
import Data.Functor (($>))
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, maybeToList)
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Policy
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API (SizeInBytes (..), ObjectPoolWriter (opwHasObject, opwObjectId))
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsAck (..))
import System.Random (StdGen)

--
-- Pure public API
--

acknowledgeObjectIds ::
  forall peerAddr object objectId.
  Ord objectId =>
  HasCallStack =>
  DecisionPolicy ->
  DecisionGlobalState peerAddr objectId object ->
  DecisionPeerState objectId object ->
  -- | number of objectId to acknowledge, requests, objects which we can submit to the
  -- objectpool, objectIds to acknowledge with multiplicities, updated DecisionPeerState.
  ( NumObjectIdsAck
  , NumObjectIdsReq
  , Map objectId object
  -- ^ objectsOwtPool
  , RefCountDiff objectId
  , DecisionPeerState objectId object
  )
{-# INLINE acknowledgeObjectIds #-}
acknowledgeObjectIds
  decisionPolicy
  globalState
  ps@DecisionPeerState
    { dpsIdsAvailable
    , dpsNumIdsInflight
    , dpsObjectsPending
    , dpsObjectsOwtPool
    } =
    -- We can only acknowledge objectIds when we can request new ones, since
    -- a `MsgRequestObjectIds` for 0 objectIds is a protocol error.
    if pdIdsToReq > 0
      then
        ( pdIdsToAck
        , pdIdsToReq
        , objectsOwtPool
        , refCountDiff
        , ps
            { dpsOutstandingFifo = dpsOutstandingFifo'
            , dpsIdsAvailable = dpsIdsAvailable'
            , dpsNumIdsInflight =
                dpsNumIdsInflight
                  + pdIdsToReq
            , dpsObjectsPending = dpsObjectsPending'
            , dpsObjectsOwtPool = dpsObjectsOwtPool'
            }
        )
      else
        ( 0
        , 0
        , objectsOwtPool
        , RefCountDiff Map.empty
        , ps{dpsObjectsOwtPool = dpsObjectsOwtPool'}
        )
   where
    -- Split `dpsOutstandingFifo'` into the longest prefix of `objectId`s which
    -- can be acknowledged and the unacknowledged `objectId`s.
    (pdIdsToReq, acknowledgedObjectIds, dpsOutstandingFifo') =
      splitAcknowledgedObjectIds decisionPolicy globalState ps

    objectsOwtPoolList =
      [ (objectId, object)
      | objectId <- toList toObjectPoolObjectIds
      , objectId `Map.notMember` dgsObjectsPending globalState
      , object <- maybeToList $ objectId `Map.lookup` dpsObjectsPending
      ]
    (toObjectPoolObjectIds, _) =
      StrictSeq.spanl (`Map.member` dpsObjectsPending) acknowledgedObjectIds

    objectsOwtPool = Map.fromList objectsOwtPoolList

    dpsObjectsOwtPool' = dpsObjectsOwtPool <> objectsOwtPool

    (dpsObjectsPending', ackedDownloadedObjects) = Map.partitionWithKey (\objectId _ -> objectId `Set.member` liveSet) dpsObjectsPending
    -- latexObjects: objects which were downloaded by another peer before we
    -- downloaded them; it relies on that `objectToObjectPool` filters out
    -- `dgsObjectsPending`.
    lateObjects =
      Map.filterWithKey
        (\objectId _ -> objectId `Map.notMember` objectsOwtPool)
        ackedDownloadedObjects

    -- the set of live `objectIds`
    liveSet = Set.fromList (toList dpsOutstandingFifo')
    dpsIdsAvailable' = dpsIdsAvailable `Set.intersection` liveSet

    -- We remove all acknowledged `objectId`s which are not in
    -- `dpsOutstandingFifo''`, but also return the unknown set before any
    -- modifications (which is used to compute `dpsOutstandingFifo''`
    -- above).

    refCountDiff =
      RefCountDiff $
        foldr
          (Map.alter fn)
          Map.empty
          acknowledgedObjectIds
     where
      fn :: Maybe Int -> Maybe Int
      fn Nothing = Just 1
      fn (Just n) = Just $! n + 1

    pdIdsToAck :: NumObjectIdsAck
    pdIdsToAck = fromIntegral $ StrictSeq.length acknowledgedObjectIds

-- | Split unacknowledged objectIds into acknowledged and unacknowledged parts, also
-- return number of objectIds which can be requested.
splitAcknowledgedObjectIds ::
  Ord objectId =>
  HasCallStack =>
  DecisionPolicy ->
  DecisionGlobalState peer objectId object ->
  DecisionPeerState objectId object ->
  -- | number of objectIds to request, acknowledged objectIds, unacknowledged objectIds
  (NumObjectIdsReq, StrictSeq.StrictSeq objectId, StrictSeq.StrictSeq objectId)
splitAcknowledgedObjectIds
  DecisionPolicy
    { dpMaxNumObjectsOutstanding
    , dpMaxNumObjectIdsReq
    }
  DecisionGlobalState
    { dgsObjectsPending
    }
  DecisionPeerState
    { dpsOutstandingFifo
    , dpsObjectsPending
    , dpsObjectsInflightIds
    , dpsNumIdsInflight
    } =
    (pdIdsToReq, acknowledgedObjectIds', dpsOutstandingFifo')
   where
    (acknowledgedObjectIds', dpsOutstandingFifo') =
      StrictSeq.spanl
        ( \objectId ->
            ( objectId `Map.member` dgsObjectsPending
                || objectId `Set.member` dpsObjectsRequestedButNotReceivedIds
                || objectId `Map.member` dpsObjectsPending
            )
              && objectId `Set.notMember` dpsObjectsInflightIds
        )
        dpsOutstandingFifo
    numOfUnacked = StrictSeq.length dpsOutstandingFifo
    numOfAcked = StrictSeq.length acknowledgedObjectIds'
    unackedAndRequested = fromIntegral numOfUnacked + dpsNumIdsInflight

    pdIdsToReq =
      assert (unackedAndRequested <= dpMaxNumObjectsOutstanding) $
        assert (dpsNumIdsInflight <= dpMaxNumObjectIdsReq) $
          (dpMaxNumObjectsOutstanding - unackedAndRequested + fromIntegral numOfAcked)
            `min` (dpMaxNumObjectIdsReq - dpsNumIdsInflight)

-- | `RefCountDiff` represents a map of `objectId` which can be acknowledged
-- together with their multiplicities.
newtype RefCountDiff objectId = RefCountDiff
  { rcdIdsToAckMultiplicities :: Map objectId Int
  }

updateRefCounts ::
  Ord objectId =>
  Map objectId Int ->
  RefCountDiff objectId ->
  Map objectId Int
updateRefCounts dgsObjectsLiveMultiplicities (RefCountDiff diff) =
  Map.merge
    (Map.mapMaybeMissing \_ x -> Just x)
    (Map.mapMaybeMissing \_ _ -> Nothing)
    ( Map.zipWithMaybeMatched \_ x y ->
        assert
          (x >= y)
          if x > y
            then Just $! x - y
            else Nothing
    )
    dgsObjectsLiveMultiplicities
    diff

tickTimedObjects ::
  forall peerAddr object objectId.
  Ord objectId =>
  Time ->
  DecisionGlobalState peerAddr objectId object ->
  DecisionGlobalState peerAddr objectId object
tickTimedObjects
  now
  st@DecisionGlobalState
    { dgsRententionTimeouts
    , dgsObjectsLiveMultiplicities
    , dgsObjectsPending
    } =
    let (expiredObjects', dgsRententionTimeouts') =
          case Map.splitLookup now dgsRententionTimeouts of
            (expired, Just objectIds, timed) ->
              ( expired -- Map.split doesn't include the `now` entry in the map
              , Map.insert now objectIds timed
              )
            (expired, Nothing, timed) ->
              (expired, timed)
        refDiff = Map.foldl' fn Map.empty expiredObjects'
        dgsObjectsLiveMultiplicities' = updateRefCounts dgsObjectsLiveMultiplicities (RefCountDiff refDiff)
        liveSet = Map.keysSet dgsObjectsLiveMultiplicities'
        dgsObjectsPending' = dgsObjectsPending `Map.restrictKeys` liveSet
     in st
          { dgsRententionTimeouts = dgsRententionTimeouts'
          , dgsObjectsLiveMultiplicities = dgsObjectsLiveMultiplicities'
          , dgsObjectsPending = dgsObjectsPending'
          }
   where
    fn ::
      Map objectId Int ->
      [objectId] ->
      Map objectId Int
    fn m objectIds = Foldable.foldl' gn m objectIds

    gn ::
      Map objectId Int ->
      objectId ->
      Map objectId Int
    gn m objectId = Map.alter af objectId m

    af ::
      Maybe Int ->
      Maybe Int
    af Nothing = Just 1
    af (Just n) = Just $! succ n

--
-- Pure internal API
--

-- | Insert received `objectId`s and return the number of objectIds to be acknowledged
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
  -- | received `objectId`s with sizes
  Set objectId ->
  DecisionGlobalState peerAddr objectId object ->
  DecisionGlobalState peerAddr objectId object
handleReceivedIdsImpl
  opwHasObject
  peerAddr
  reqNo
  objectIdsSeq
  objectIdsSet
  st@DecisionGlobalState
    { dgsPeerStates
    , dgsObjectsPending
    , dgsObjectsLiveMultiplicities
    } =
    -- using `alterF` so the update of `DecisionPeerState` is done in one lookup
    case Map.alterF
      (fmap Just . fn . fromJust)
      peerAddr
      dgsPeerStates of
      (st', dgsPeerStates') ->
        st'{dgsPeerStates = dgsPeerStates'}
   where
    -- update `DecisionPeerState` and return number of `objectId`s to acknowledged and
    -- updated `DecisionGlobalState`.
    fn ::
      DecisionPeerState objectId object ->
      ( DecisionGlobalState peerAddr objectId object
      , DecisionPeerState objectId object
      )
    fn
      ps@DecisionPeerState
        { dpsIdsAvailable
        , dpsNumIdsInflight
        , dpsOutstandingFifo
        } =
        (st', ps')
       where
        --
        -- Handle new `objectId`s
        --

        -- Divide the new objectIds in two: those that are already in the objectpool
        -- and those that are not. We'll request some objects from the latter.
        (ignoredObjectIds, dpsIdsAvailableSet) =
          Set.partition opwHasObject objectIdsSet

        -- Add all `objectIds` from `dpsIdsAvailableMap` which are not
        -- unacknowledged or already buffered. Unacknowledged objectIds must have
        -- already been added to `dpsIdsAvailable` map before.
        dpsIdsAvailable' =
          Set.foldl
            (\m objectId -> Set.insert objectId m)
            dpsIdsAvailable
            ( Set.filter
                ( \objectId ->
                    objectId `notElem` dpsOutstandingFifo
                      && objectId `Map.notMember` dgsObjectsPending
                )
                dpsIdsAvailableSet
            )

        -- Add received objectIds to `dpsOutstandingFifo`.
        dpsOutstandingFifo' = dpsOutstandingFifo <> objectIdsSeq

        -- Add ignored `objects` to buffered ones.
        -- Note: we prefer to keep the `object` if it's already in `dgsObjectsPending`.
        dgsObjectsPending' =
          dgsObjectsPending
            <> Map.fromList ((, Nothing) <$> Set.toList ignoredObjectIds)

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
            objectIdsSeq

        st' =
          st
            { dgsObjectsPending = dgsObjectsPending'
            , dgsObjectsLiveMultiplicities = dgsObjectsLiveMultiplicities'
            }
        ps' =
          assert
            (dpsNumIdsInflight >= reqNo)
            ps
              { dpsIdsAvailable = dpsIdsAvailable'
              , dpsOutstandingFifo = dpsOutstandingFifo'
              , dpsNumIdsInflight = dpsNumIdsInflight - reqNo
              }

-- | We check advertised sizes up in a fuzzy way.  The advertised and received
-- sizes need to agree up to `const_MAX_OBJECT_SIZE_DISCREPENCY`.
const_MAX_OBJECT_SIZE_DISCREPENCY :: SizeInBytes
const_MAX_OBJECT_SIZE_DISCREPENCY = 32

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
      , dgsObjectsPending = Map.empty
      , dgsObjectsLiveMultiplicities = Map.empty
      , dgsRententionTimeouts = Map.empty
      , dgsObjectsOwtPoolMultiplicities = Map.empty
      , dgsRng = rng
      }

-- | Acknowledge `objectId`s, return the number of `objectIds` to be acknowledged to the
-- remote side.
handleReceivedIds ::
  forall m peerAddr ticketNo object objectId.
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
  Set objectId ->
  m ()
handleReceivedIds tracer sharedVar objectPoolWriter peerAddr reqNo objectIdsSeq objectIds = do
  st <- atomically $ do
    hasObject <- opwHasObject objectPoolWriter
    stateTVar
      sharedVar
      ((\a -> (a, a)) . handleReceivedIdsImpl hasObject peerAddr reqNo objectIdsSeq objectIds)
  traceWith tracer (TraceDecisionLogicGlobalStateUpdated "handleReceivedIds" st)

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
handleReceivedObjects tracer objectSize sharedVar peerAddr objectIdsRequested objectsMap = do
  r <- atomically $ do
    st <- readTVar sharedVar
    case handleReceivedObjectsImpl objectSize peerAddr objectIdsRequested objectsMap st of
      r@(Right st') ->
        writeTVar sharedVar st'
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
        , dgsObjectsPending
        , dgsObjectsLiveMultiplicities
        , dgsRententionTimeouts
        , dgsObjectsOwtPoolMultiplicities
        } =
        st
          { dgsPeerStates = dgsPeerStates'
          , dgsObjectsPending = dgsObjectsPending'
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

        dgsObjectsPending' =
          Map.insert
          objectId
          (Just object)
          dgsObjectsPending

        dgsPeerStates' =
          Map.update
          (\ps -> Just $! ps{dpsObjectsPending = Map.insert objectId object (dpsObjectsPending ps)})
          peerAddr
          dgsPeerStates