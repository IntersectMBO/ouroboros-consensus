{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.State
  ( -- * Core API
    SharedObjectState (..)
  , PeerObjectState (..)
  , SharedObjectStateVar
  , newSharedObjectStateVar
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
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Policy
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API (ObjectPoolSnapshot (..), SizeInBytes (..))
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectIdsToAck (..))
import System.Random (StdGen)

--
-- Pure public API
--

acknowledgeObjectIds ::
  forall peerAddr object objectId.
  Ord objectId =>
  HasCallStack =>
  ObjectDecisionPolicy ->
  SharedObjectState peerAddr objectId object ->
  PeerObjectState objectId object ->
  -- | number of objectId to acknowledge, requests, objects which we can submit to the
  -- objectpool, objectIds to acknowledge with multiplicities, updated PeerObjectState.
  ( NumObjectIdsToAck
  , NumObjectIdsToReq
  , ObjectsToObjectPool objectId object
  , RefCountDiff objectId
  , PeerObjectState objectId object
  )
{-# INLINE acknowledgeObjectIds #-}
acknowledgeObjectIds
  policy
  sharedObjectState
  ps@PeerObjectState
    { availableObjectIds
    , unknownObjects
    , requestedObjectIdsInflight
    , downloadedObjects
    , score
    , toObjectPoolObjects
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
            { unacknowledgedObjectIds = unacknowledgedObjectIds'
            , availableObjectIds = availableObjectIds'
            , unknownObjects = unknownObjects'
            , requestedObjectIdsInflight =
                requestedObjectIdsInflight
                  + objectIdsToRequest
            , downloadedObjects = downloadedObjects'
            , score = score'
            , toObjectPoolObjects = toObjectPoolObjects'
            }
        )
      else
        ( 0
        , 0
        , ObjectsToObjectPool objectsToObjectPool
        , RefCountDiff Map.empty
        , ps{toObjectPoolObjects = toObjectPoolObjects'}
        )
   where
    -- Split `unacknowledgedObjectIds'` into the longest prefix of `objectId`s which
    -- can be acknowledged and the unacknowledged `objectId`s.
    (objectIdsToRequest, acknowledgedObjectIds, unacknowledgedObjectIds') =
      splitAcknowledgedObjectIds policy sharedObjectState ps

    objectsToObjectPool =
      [ (objectId, object)
      | objectId <- toList toObjectPoolObjectIds
      , objectId `Map.notMember` bufferedObjects sharedObjectState
      , object <- maybeToList $ objectId `Map.lookup` downloadedObjects
      ]
    (toObjectPoolObjectIds, _) =
      StrictSeq.spanl (`Map.member` downloadedObjects) acknowledgedObjectIds

    objectsToObjectPoolMap = Map.fromList objectsToObjectPool

    toObjectPoolObjects' = toObjectPoolObjects <> objectsToObjectPoolMap

    (downloadedObjects', ackedDownloadedObjects) = Map.partitionWithKey (\objectId _ -> objectId `Set.member` liveSet) downloadedObjects
    -- latexObjects: transactions which were downloaded by another peer before we
    -- downloaded them; it relies on that `objectToObjectPool` filters out
    -- `bufferedObjects`.
    lateObjects =
      Map.filterWithKey
        (\objectId _ -> objectId `Map.notMember` objectsToObjectPoolMap)
        ackedDownloadedObjects
    score' = score + fromIntegral (Map.size lateObjects)

    -- the set of live `objectIds`
    liveSet = Set.fromList (toList unacknowledgedObjectIds')

    availableObjectIds' =
      availableObjectIds
        `Map.restrictKeys` liveSet

    -- We remove all acknowledged `objectId`s which are not in
    -- `unacknowledgedObjectIds''`, but also return the unknown set before any
    -- modifications (which is used to compute `unacknowledgedObjectIds''`
    -- above).
    unknownObjects' = unknownObjects `Set.intersection` liveSet

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

    objectIdsToAcknowledge :: NumObjectIdsToAck
    objectIdsToAcknowledge = fromIntegral $ StrictSeq.length acknowledgedObjectIds

-- | Split unacknowledged objectIds into acknowledged and unacknowledged parts, also
-- return number of objectIds which can be requested.
splitAcknowledgedObjectIds ::
  Ord objectId =>
  HasCallStack =>
  ObjectDecisionPolicy ->
  SharedObjectState peer objectId object ->
  PeerObjectState objectId object ->
  -- | number of objectIds to request, acknowledged objectIds, unacknowledged objectIds
  (NumObjectIdsToReq, StrictSeq.StrictSeq objectId, StrictSeq.StrictSeq objectId)
splitAcknowledgedObjectIds
  ObjectDecisionPolicy
    { maxUnacknowledgedObjectIds
    , maxNumObjectIdsToRequest
    }
  SharedObjectState
    { bufferedObjects
    }
  PeerObjectState
    { unacknowledgedObjectIds
    , unknownObjects
    , downloadedObjects
    , requestedObjectsInflight
    , requestedObjectIdsInflight
    } =
    (objectIdsToRequest, acknowledgedObjectIds', unacknowledgedObjectIds')
   where
    (acknowledgedObjectIds', unacknowledgedObjectIds') =
      StrictSeq.spanl
        ( \objectId ->
            ( objectId `Map.member` bufferedObjects
                || objectId `Set.member` unknownObjects
                || objectId `Map.member` downloadedObjects
            )
              && objectId `Set.notMember` requestedObjectsInflight
        )
        unacknowledgedObjectIds
    numOfUnacked = StrictSeq.length unacknowledgedObjectIds
    numOfAcked = StrictSeq.length acknowledgedObjectIds'
    unackedAndRequested = fromIntegral numOfUnacked + requestedObjectIdsInflight

    objectIdsToRequest =
      assert (unackedAndRequested <= maxUnacknowledgedObjectIds) $
        assert (requestedObjectIdsInflight <= maxNumObjectIdsToRequest) $
          (maxUnacknowledgedObjectIds - unackedAndRequested + fromIntegral numOfAcked)
            `min` (maxNumObjectIdsToRequest - requestedObjectIdsInflight)

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
  SharedObjectState peerAddr objectId object ->
  SharedObjectState peerAddr objectId object
tickTimedObjects
  now
  st@SharedObjectState
    { timedObjects
    , referenceCounts
    , bufferedObjects
    } =
    let (expiredObjects', timedObjects') =
          case Map.splitLookup now timedObjects of
            (expired, Just objectIds, timed) ->
              ( expired -- Map.split doesn't include the `now` entry in the map
              , Map.insert now objectIds timed
              )
            (expired, Nothing, timed) ->
              (expired, timed)
        refDiff = Map.foldl' fn Map.empty expiredObjects'
        referenceCounts' = updateRefCounts referenceCounts (RefCountDiff refDiff)
        liveSet = Map.keysSet referenceCounts'
        bufferedObjects' = bufferedObjects `Map.restrictKeys` liveSet
     in st
          { timedObjects = timedObjects'
          , referenceCounts = referenceCounts'
          , bufferedObjects = bufferedObjects'
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
-- and the updated `SharedObjectState`.
receivedObjectIdsImpl ::
  forall peerAddr object objectId.
  (Ord objectId, Ord peerAddr, HasCallStack) =>
  -- | check if objectId is in the objectpool, ref
  -- 'objectpoolHasObject'
  (objectId -> Bool) ->
  peerAddr ->
  -- | number of requests to subtract from
  -- `requestedObjectIdsInflight`
  NumObjectIdsToReq ->
  -- | sequence of received `objectIds`
  StrictSeq objectId ->
  -- | received `objectId`s with sizes
  Map objectId SizeInBytes ->
  SharedObjectState peerAddr objectId object ->
  SharedObjectState peerAddr objectId object
receivedObjectIdsImpl
  objectpoolHasObject
  peerAddr
  reqNo
  objectIdsSeq
  objectIdsMap
  st@SharedObjectState
    { peerObjectStates
    , bufferedObjects
    , referenceCounts
    } =
    -- using `alterF` so the update of `PeerObjectState` is done in one lookup
    case Map.alterF
      (fmap Just . fn . fromJust)
      peerAddr
      peerObjectStates of
      (st', peerObjectStates') ->
        st'{peerObjectStates = peerObjectStates'}
   where
    -- update `PeerObjectState` and return number of `objectId`s to acknowledged and
    -- updated `SharedObjectState`.
    fn ::
      PeerObjectState objectId object ->
      ( SharedObjectState peerAddr objectId object
      , PeerObjectState objectId object
      )
    fn
      ps@PeerObjectState
        { availableObjectIds
        , requestedObjectIdsInflight
        , unacknowledgedObjectIds
        } =
        (st', ps')
       where
        --
        -- Handle new `objectId`s
        --

        -- Divide the new objectIds in two: those that are already in the objectpool
        -- and those that are not. We'll request some objects from the latter.
        (ignoredObjectIds, availableObjectIdsMap) =
          Map.partitionWithKey
            (\objectId _ -> objectpoolHasObject objectId)
            objectIdsMap

        -- Add all `objectIds` from `availableObjectIdsMap` which are not
        -- unacknowledged or already buffered. Unacknowledged objectIds must have
        -- already been added to `availableObjectIds` map before.
        availableObjectIds' =
          Map.foldlWithKey
            (\m objectId sizeInBytes -> Map.insert objectId sizeInBytes m)
            availableObjectIds
            ( Map.filterWithKey
                ( \objectId _ ->
                    objectId `notElem` unacknowledgedObjectIds
                      && objectId `Map.notMember` bufferedObjects
                )
                availableObjectIdsMap
            )

        -- Add received objectIds to `unacknowledgedObjectIds`.
        unacknowledgedObjectIds' = unacknowledgedObjectIds <> objectIdsSeq

        -- Add ignored `objects` to buffered ones.
        -- Note: we prefer to keep the `object` if it's already in `bufferedObjects`.
        bufferedObjects' =
          bufferedObjects
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
            { bufferedObjects = bufferedObjects'
            , referenceCounts = referenceCounts'
            }
        ps' =
          assert
            (requestedObjectIdsInflight >= reqNo)
            ps
              { availableObjectIds = availableObjectIds'
              , unacknowledgedObjectIds = unacknowledgedObjectIds'
              , requestedObjectIdsInflight = requestedObjectIdsInflight - reqNo
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
  SharedObjectState peerAddr objectId object ->
  -- | Return list of `objectId` which sizes didn't match or a new state.
  -- If one of the `object` has wrong size, we return an error.  The
  -- mini-protocol will throw, which will clean the state map from this peer.
  Either
    ObjectDiffusionProtocolError
    (SharedObjectState peerAddr objectId object)
collectObjectsImpl
  objectSize
  peerAddr
  requestedObjectIdsMap
  receivedObjects
  st@SharedObjectState{peerObjectStates} =
    -- using `alterF` so the update of `PeerObjectState` is done in one lookup
    case Map.alterF
      (fmap Just . fn . fromJust)
      peerAddr
      peerObjectStates of
      (Right st', peerObjectStates') ->
        Right st'{peerObjectStates = peerObjectStates'}
      (Left e, _) ->
        Left $ ProtocolErrorObjectSizeError e
   where
    -- Update `PeerObjectState` and partially update `SharedObjectState` (except of
    -- `peerObjectStates`).
    fn ::
      PeerObjectState objectId object ->
      ( Either
          [(objectId, SizeInBytes, SizeInBytes)]
          (SharedObjectState peerAddr objectId object)
      , PeerObjectState objectId object
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
      downloadedObjects' = downloadedObjects ps <> receivedObjects
      -- Add not received objects to `unknownObjects` before acknowledging objectIds.
      unknownObjects' = unknownObjects ps <> notReceived

      requestedObjectsInflight' =
        assert (requestedObjectIds `Set.isSubsetOf` requestedObjectsInflight ps) $
          requestedObjectsInflight ps Set.\\ requestedObjectIds

      requestedSize = fold $ availableObjectIds ps `Map.restrictKeys` requestedObjectIds
      requestedObjectsInflightSize' =
        assert (requestedObjectsInflightSize ps >= requestedSize) $
          requestedObjectsInflightSize ps - requestedSize

      -- subtract requested from in-flight
      inflightObjects'' =
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
          (inflightObjects st)
          (Map.fromSet (const 1) requestedObjectIds)

      inflightObjectsSize'' =
        assert (inflightObjectsSize st >= requestedSize) $
          inflightObjectsSize st - requestedSize

      st' =
        st
          { inflightObjects = inflightObjects''
          , inflightObjectsSize = inflightObjectsSize''
          }

      --
      -- Update PeerObjectState
      --

      -- Remove the downloaded `objectId`s from the availableObjectIds map, this
      -- guarantees that we won't attempt to download the `objectIds` from this peer
      -- once we collect the `objectId`s. Also restrict keys to `liveSet`.
      --
      -- NOTE: we could remove `notReceived` from `availableObjectIds`; and
      -- possibly avoid using `unknownObjects` field at all.
      --
      availableObjectIds'' =
        availableObjectIds ps
          `Map.withoutKeys` requestedObjectIds

      -- Remove all acknowledged `objectId`s from unknown set, but only those
      -- which are not present in `unacknowledgedObjectIds'`
      unknownObjects'' =
        unknownObjects'
          `Set.intersection` live
       where
        -- We cannot use `liveSet` as `unknown <> notReceived` might
        -- contain `objectIds` which are in `liveSet` but are not `live`.
        live = Set.fromList (toList (unacknowledgedObjectIds ps))

      ps'' =
        ps
          { availableObjectIds = availableObjectIds''
          , unknownObjects = unknownObjects''
          , requestedObjectsInflightSize = requestedObjectsInflightSize'
          , requestedObjectsInflight = requestedObjectsInflight'
          , downloadedObjects = downloadedObjects'
          }

--
-- Monadic public API
--

type SharedObjectStateVar m peerAddr objectId object =
  StrictTVar m (SharedObjectState peerAddr objectId object)

newSharedObjectStateVar ::
  MonadSTM m =>
  StdGen ->
  m (SharedObjectStateVar m peerAddr objectId object)
newSharedObjectStateVar rng =
  newTVarIO
    SharedObjectState
      { peerObjectStates = Map.empty
      , inflightObjects = Map.empty
      , inflightObjectsSize = 0
      , bufferedObjects = Map.empty
      , referenceCounts = Map.empty
      , timedObjects = Map.empty
      , inSubmissionToObjectPoolObjects = Map.empty
      , peerRng = rng
      }

-- | Acknowledge `objectId`s, return the number of `objectIds` to be acknowledged to the
-- remote side.
receivedObjectIds ::
  forall m peerAddr idx object objectId.
  (MonadSTM m, Ord objectId, Ord peerAddr) =>
  Tracer m (TraceObjectLogic peerAddr objectId object) ->
  SharedObjectStateVar m peerAddr objectId object ->
  STM m (ObjectPoolSnapshot objectId object idx) ->
  peerAddr ->
  -- | number of requests to subtract from
  -- `requestedObjectIdsInflight`
  NumObjectIdsToReq ->
  -- | sequence of received `objectIds`
  StrictSeq objectId ->
  -- | received `objectId`s with sizes
  Map objectId SizeInBytes ->
  m ()
receivedObjectIds tracer sharedVar getObjectPoolSnapshot peerAddr reqNo objectIdsSeq objectIdsMap = do
  st <- atomically $ do
    ObjectPoolSnapshot{objectpoolHasObject} <- getObjectPoolSnapshot
    stateTVar
      sharedVar
      ((\a -> (a, a)) . receivedObjectIdsImpl objectpoolHasObject peerAddr reqNo objectIdsSeq objectIdsMap)
  traceWith tracer (TraceSharedObjectState "receivedObjectIds" st)

-- | Include received `object`s in `SharedObjectState`.  Return number of `objectIds`
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
  SharedObjectStateVar m peerAddr objectId object ->
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
      traceWith tracer (TraceSharedObjectState "collectObjects" st)
        $> Nothing
    Left e -> return (Just e)
