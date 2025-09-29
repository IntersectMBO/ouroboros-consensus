{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.State
  ( -- * Core API
    DecisionGlobalState (..)
  , DecisionPeerState (..)
  , DecisionGlobalStateVar
  , newDecisionGlobalStateVar
  , receivedObjectIds
  , collectObjects
  , acknowledgeObjectIds
  , splitAcknowledgedObjectIds
  , tickTimedObjects
  , const_MAX_OBJECT_SIZE_DISCREPENCY

    -- * Internals, only exported for testing purposes:
  , RefCountDiff (..)
  , updateRefCounts
  , receivedObjectIdsImpl
  , collectObjectsImpl
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
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API (SizeInBytes (..), ObjectPoolWriter (opwHasObject))
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsAck (..))
import System.Random (StdGen)

--
-- Pure public API
--

acknowledgeObjectIds ::
  forall peerAddr object objectId.
  Ord objectId =>
  HasCallStack =>
  PeerDecisionPolicy ->
  DecisionGlobalState peerAddr objectId object ->
  DecisionPeerState objectId object ->
  -- | number of objectId to acknowledge, requests, objects which we can submit to the
  -- objectpool, objectIds to acknowledge with multiplicities, updated DecisionPeerState.
  ( NumObjectIdsAck
  , NumObjectIdsReq
  , ObjectsToObjectPool objectId object
  , RefCountDiff objectId
  , DecisionPeerState objectId object
  )
{-# INLINE acknowledgeObjectIds #-}
acknowledgeObjectIds
  policy
  sharedObjectState
  ps@DecisionPeerState
    { availableObjectIds
    , requestedButNotReceived
    , numIdsInFlight
    , pendingObjects
    , score
    , toPoolObjects
    } =
    -- We can only acknowledge objectIds when we can request new ones, since
    -- a `MsgRequestObjectIds` for 0 objectIds is a protocol error.
    if objectIdsToRequest > 0
      then
        ( objectIdsToAcknowledge
        , objectIdsToRequest
        , ObjectsToObjectPool objectsToObjectPool
        , refCountDiff
        , ps
            { outstandingFifo = outstandingFifo'
            , availableObjectIds = availableObjectIds'
            , requestedButNotReceived = requestedButNotReceived'
            , numIdsInFlight =
                numIdsInFlight
                  + objectIdsToRequest
            , pendingObjects = pendingObjects'
            , score = score'
            , toPoolObjects = toPoolObjects'
            }
        )
      else
        ( 0
        , 0
        , ObjectsToObjectPool objectsToObjectPool
        , RefCountDiff Map.empty
        , ps{toPoolObjects = toPoolObjects'}
        )
   where
    -- Split `outstandingFifo'` into the longest prefix of `objectId`s which
    -- can be acknowledged and the unacknowledged `objectId`s.
    (objectIdsToRequest, acknowledgedObjectIds, outstandingFifo') =
      splitAcknowledgedObjectIds policy sharedObjectState ps

    objectsToObjectPool =
      [ (objectId, object)
      | objectId <- toList toObjectPoolObjectIds
      , objectId `Map.notMember` globalObtainedButNotAckedObjects sharedObjectState
      , object <- maybeToList $ objectId `Map.lookup` pendingObjects
      ]
    (toObjectPoolObjectIds, _) =
      StrictSeq.spanl (`Map.member` pendingObjects) acknowledgedObjectIds

    objectsToObjectPoolMap = Map.fromList objectsToObjectPool

    toPoolObjects' = toPoolObjects <> objectsToObjectPoolMap

    (pendingObjects', ackedDownloadedObjects) = Map.partitionWithKey (\objectId _ -> objectId `Set.member` liveSet) pendingObjects
    -- latexObjects: transactions which were downloaded by another peer before we
    -- downloaded them; it relies on that `objectToObjectPool` filters out
    -- `globalObtainedButNotAckedObjects`.
    lateObjects =
      Map.filterWithKey
        (\objectId _ -> objectId `Map.notMember` objectsToObjectPoolMap)
        ackedDownloadedObjects
    score' = score + fromIntegral (Map.size lateObjects)

    -- the set of live `objectIds`
    liveSet = Set.fromList (toList outstandingFifo')

    availableObjectIds' =
      availableObjectIds
        `Map.restrictKeys` liveSet

    -- We remove all acknowledged `objectId`s which are not in
    -- `outstandingFifo''`, but also return the unknown set before any
    -- modifications (which is used to compute `outstandingFifo''`
    -- above).
    requestedButNotReceived' = requestedButNotReceived `Set.intersection` liveSet

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

    objectIdsToAcknowledge :: NumObjectIdsAck
    objectIdsToAcknowledge = fromIntegral $ StrictSeq.length acknowledgedObjectIds

-- | Split unacknowledged objectIds into acknowledged and unacknowledged parts, also
-- return number of objectIds which can be requested.
splitAcknowledgedObjectIds ::
  Ord objectId =>
  HasCallStack =>
  PeerDecisionPolicy ->
  DecisionGlobalState peer objectId object ->
  DecisionPeerState objectId object ->
  -- | number of objectIds to request, acknowledged objectIds, unacknowledged objectIds
  (NumObjectIdsReq, StrictSeq.StrictSeq objectId, StrictSeq.StrictSeq objectId)
splitAcknowledgedObjectIds
  PeerDecisionPolicy
    { maxUnacknowledgedObjectIds
    , maxNumObjectIdsRequest
    }
  DecisionGlobalState
    { globalObtainedButNotAckedObjects
    }
  DecisionPeerState
    { outstandingFifo
    , requestedButNotReceived
    , pendingObjects
    , inFlight
    , numIdsInFlight
    } =
    (objectIdsToRequest, acknowledgedObjectIds', outstandingFifo')
   where
    (acknowledgedObjectIds', outstandingFifo') =
      StrictSeq.spanl
        ( \objectId ->
            ( objectId `Map.member` globalObtainedButNotAckedObjects
                || objectId `Set.member` requestedButNotReceived
                || objectId `Map.member` pendingObjects
            )
              && objectId `Set.notMember` inFlight
        )
        outstandingFifo
    numOfUnacked = StrictSeq.length outstandingFifo
    numOfAcked = StrictSeq.length acknowledgedObjectIds'
    unackedAndRequested = fromIntegral numOfUnacked + numIdsInFlight

    objectIdsToRequest =
      assert (unackedAndRequested <= maxUnacknowledgedObjectIds) $
        assert (numIdsInFlight <= maxNumObjectIdsRequest) $
          (maxUnacknowledgedObjectIds - unackedAndRequested + fromIntegral numOfAcked)
            `min` (maxNumObjectIdsRequest - numIdsInFlight)

-- | `RefCountDiff` represents a map of `objectId` which can be acknowledged
-- together with their multiplicities.
newtype RefCountDiff objectId = RefCountDiff
  { objectIdsToAck :: Map objectId Int
  }

updateRefCounts ::
  Ord objectId =>
  Map objectId Int ->
  RefCountDiff objectId ->
  Map objectId Int
updateRefCounts referenceCounts (RefCountDiff diff) =
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
    referenceCounts
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
    { globalRententionTimeouts
    , referenceCounts
    , globalObtainedButNotAckedObjects
    } =
    let (expiredObjects', globalRententionTimeouts') =
          case Map.splitLookup now globalRententionTimeouts of
            (expired, Just objectIds, timed) ->
              ( expired -- Map.split doesn't include the `now` entry in the map
              , Map.insert now objectIds timed
              )
            (expired, Nothing, timed) ->
              (expired, timed)
        refDiff = Map.foldl' fn Map.empty expiredObjects'
        referenceCounts' = updateRefCounts referenceCounts (RefCountDiff refDiff)
        liveSet = Map.keysSet referenceCounts'
        globalObtainedButNotAckedObjects' = globalObtainedButNotAckedObjects `Map.restrictKeys` liveSet
     in st
          { globalRententionTimeouts = globalRententionTimeouts'
          , referenceCounts = referenceCounts'
          , globalObtainedButNotAckedObjects = globalObtainedButNotAckedObjects'
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
receivedObjectIdsImpl ::
  forall peerAddr object objectId.
  (Ord objectId, Ord peerAddr, HasCallStack) =>
  -- | check if objectId is in the objectpool, ref
  -- 'objectpoolHasObject'
  (objectId -> Bool) ->
  peerAddr ->
  -- | number of requests to subtract from
  -- `numIdsInFlight`
  NumObjectIdsReq ->
  -- | sequence of received `objectIds`
  StrictSeq objectId ->
  -- | received `objectId`s with sizes
  Set objectId ->
  DecisionGlobalState peerAddr objectId object ->
  DecisionGlobalState peerAddr objectId object
receivedObjectIdsImpl
  objectpoolHasObject
  peerAddr
  reqNo
  objectIdsSeq
  objectIdsSet
  st@DecisionGlobalState
    { peerStates
    , globalObtainedButNotAckedObjects
    , referenceCounts
    } =
    -- using `alterF` so the update of `DecisionPeerState` is done in one lookup
    case Map.alterF
      (fmap Just . fn . fromJust)
      peerAddr
      peerStates of
      (st', peerStates') ->
        st'{peerStates = peerStates'}
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
        { availableObjectIds
        , numIdsInFlight
        , outstandingFifo
        } =
        (st', ps')
       where
        --
        -- Handle new `objectId`s
        --

        -- Divide the new objectIds in two: those that are already in the objectpool
        -- and those that are not. We'll request some objects from the latter.
        (ignoredObjectIds, availableObjectIdsSet) =
          Set.partition objectpoolHasObject objectIdsSet

        -- Add all `objectIds` from `availableObjectIdsMap` which are not
        -- unacknowledged or already buffered. Unacknowledged objectIds must have
        -- already been added to `availableObjectIds` map before.
        availableObjectIds' =
          Set.foldl
            (\m objectId -> Set.insert objectId m)
            availableObjectIds
            ( Set.filter
                ( \objectId ->
                    objectId `notElem` outstandingFifo
                      && objectId `Map.notMember` globalObtainedButNotAckedObjects
                )
                availableObjectIdsSet
            )

        -- Add received objectIds to `outstandingFifo`.
        outstandingFifo' = outstandingFifo <> objectIdsSeq

        -- Add ignored `objects` to buffered ones.
        -- Note: we prefer to keep the `object` if it's already in `globalObtainedButNotAckedObjects`.
        globalObtainedButNotAckedObjects' =
          globalObtainedButNotAckedObjects
            <> Map.map (const Nothing) ignoredObjectIds

        referenceCounts' =
          Foldable.foldl'
            ( flip $
                Map.alter
                  ( \case
                      Nothing -> Just $! 1
                      Just cnt -> Just $! succ cnt
                  )
            )
            referenceCounts
            objectIdsSeq

        st' =
          st
            { globalObtainedButNotAckedObjects = globalObtainedButNotAckedObjects'
            , referenceCounts = referenceCounts'
            }
        ps' =
          assert
            (numIdsInFlight >= reqNo)
            ps
              { availableObjectIds = availableObjectIds'
              , outstandingFifo = outstandingFifo'
              , numIdsInFlight = numIdsInFlight - reqNo
              }

-- | We check advertised sizes up in a fuzzy way.  The advertised and received
-- sizes need to agree up to `const_MAX_OBJECT_SIZE_DISCREPENCY`.
const_MAX_OBJECT_SIZE_DISCREPENCY :: SizeInBytes
const_MAX_OBJECT_SIZE_DISCREPENCY = 32

collectObjectsImpl ::
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
    ObjectDiffusionProtocolError
    (DecisionGlobalState peerAddr objectId object)
collectObjectsImpl
  objectSize
  peerAddr
  requestedObjectIdsMap
  receivedObjects
  st@DecisionGlobalState{peerStates} =
    -- using `alterF` so the update of `DecisionPeerState` is done in one lookup
    case Map.alterF
      (fmap Just . fn . fromJust)
      peerAddr
      peerStates of
      (Right st', peerStates') ->
        Right st'{peerStates = peerStates'}
      (Left e, _) ->
        Left $ ProtocolErrorObjectSizeError e
   where
    -- Update `DecisionPeerState` and partially update `DecisionGlobalState` (except of
    -- `peerStates`).
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
      pendingObjects' = pendingObjects ps <> receivedObjects
      -- Add not received objects to `requestedButNotReceived` before acknowledging objectIds.
      requestedButNotReceived' = requestedButNotReceived ps <> notReceived

      inFlight' =
        assert (requestedObjectIds `Set.isSubsetOf` inFlight ps) $
          inFlight ps Set.\\ requestedObjectIds

      requestedSize = fold $ availableObjectIds ps `Map.restrictKeys` requestedObjectIds

      -- subtract requested from in-flight
      globalInFlightObjects'' =
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
          (globalInFlightObjects st)
          (Map.fromSet (const 1) requestedObjectIds)

      st' =
        st
          { globalInFlightObjects = globalInFlightObjects''
          }

      --
      -- Update DecisionPeerState
      --

      -- Remove the downloaded `objectId`s from the availableObjectIds map, this
      -- guarantees that we won't attempt to download the `objectIds` from this peer
      -- once we collect the `objectId`s. Also restrict keys to `liveSet`.
      --
      -- NOTE: we could remove `notReceived` from `availableObjectIds`; and
      -- possibly avoid using `requestedButNotReceived` field at all.
      --
      availableObjectIds'' =
        availableObjectIds ps
          `Map.withoutKeys` requestedObjectIds

      -- Remove all acknowledged `objectId`s from unknown set, but only those
      -- which are not present in `outstandingFifo'`
      requestedButNotReceived'' =
        requestedButNotReceived'
          `Set.intersection` live
       where
        -- We cannot use `liveSet` as `unknown <> notReceived` might
        -- contain `objectIds` which are in `liveSet` but are not `live`.
        live = Set.fromList (toList (outstandingFifo ps))

      ps'' =
        ps
          { availableObjectIds = availableObjectIds''
          , requestedButNotReceived = requestedButNotReceived''
          , inFlight = inFlight'
          , pendingObjects = pendingObjects'
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
      { peerStates = Map.empty
      , globalInFlightObjects = Map.empty
      , globalObtainedButNotAckedObjects = Map.empty
      , referenceCounts = Map.empty
      , globalRententionTimeouts = Map.empty
      , globalToPoolObjects = Map.empty
      , orderRng = rng
      }

-- | Acknowledge `objectId`s, return the number of `objectIds` to be acknowledged to the
-- remote side.
receivedObjectIds ::
  forall m peerAddr ticketNo object objectId.
  (MonadSTM m, Ord objectId, Ord peerAddr) =>
  Tracer m (TraceObjectLogic peerAddr objectId object) ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  ObjectPoolWriter objectId object m ->
  peerAddr ->
  -- | number of requests to subtract from
  -- `numIdsInFlight`
  NumObjectIdsReq ->
  -- | sequence of received `objectIds`
  StrictSeq objectId ->
  -- | received `objectId`s with sizes
  Map objectId SizeInBytes ->
  m ()
receivedObjectIds tracer sharedVar objectPoolWriter peerAddr reqNo objectIdsSeq objectIdsMap = do
  st <- atomically $ do
    hasObject <- opwHasObject objectPoolWriter
    stateTVar
      sharedVar
      ((\a -> (a, a)) . receivedObjectIdsImpl hasObject peerAddr reqNo objectIdsSeq objectIdsMap)
  traceWith tracer (TraceDecisionGlobalState "receivedObjectIds" st)

-- | Include received `object`s in `DecisionGlobalState`.  Return number of `objectIds`
-- to be acknowledged and list of `object` to be added to the objectpool.
collectObjects ::
  forall m peerAddr object objectId.
  ( MonadSTM m
  , Ord objectId
  , Ord peerAddr
  , Show objectId
  , Typeable objectId
  ) =>
  Tracer m (TraceObjectLogic peerAddr objectId object) ->
  (object -> SizeInBytes) ->
  DecisionGlobalStateVar m peerAddr objectId object ->
  peerAddr ->
  -- | set of requested objectIds with their announced size
  Map objectId SizeInBytes ->
  -- | received objects
  Map objectId object ->
  -- | number of objectIds to be acknowledged and objects to be added to the
  -- objectpool
  m (Maybe ObjectDiffusionProtocolError)
collectObjects tracer objectSize sharedVar peerAddr objectIdsRequested objectsMap = do
  r <- atomically $ do
    st <- readTVar sharedVar
    case collectObjectsImpl objectSize peerAddr objectIdsRequested objectsMap st of
      r@(Right st') ->
        writeTVar sharedVar st'
          $> r
      r@Left{} -> pure r
  case r of
    Right st ->
      traceWith tracer (TraceDecisionGlobalState "collectObjects" st)
        $> Nothing
    Left e -> return (Just e)
