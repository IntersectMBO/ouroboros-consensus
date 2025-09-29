{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Decision
  ( ObjectDecision (..)
  , emptyObjectDecision

    -- * Internal API exposed for testing
  , makeDecisions
  , filterActivePeers
  , pickObjectsToDownload
  ) where

import Control.Arrow ((>>>))
import Control.Exception (assert)
import Data.Bifunctor (second)
import Data.Hashable
import Data.List qualified as List
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Policy
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.State
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
import Ouroboros.Network.Protocol.ObjectDiffusion.Type
import System.Random (random)

-- | Make download decisions.
makeDecisions ::
  forall peeraddr objectId object.
  ( Ord peeraddr
  , Ord objectId
  , Hashable peeraddr
  ) =>
  -- | decision policy
  ObjectDecisionPolicy ->
  -- | decision context
  SharedObjectState peeraddr objectId object ->
  -- | list of available peers.
  --
  -- This is a subset of `peerObjectStates` of peers which either:
  -- * can be used to download a `object`,
  -- * can acknowledge some `objectId`s.
  Map peeraddr (PeerObjectState objectId object) ->
  ( SharedObjectState peeraddr objectId object
  , Map peeraddr (ObjectDecision objectId object)
  )
makeDecisions policy st =
  let (salt, rng') = random (peerRng st)
      st' = st{peerRng = rng'}
   in fn
        . pickObjectsToDownload policy st'
        . orderByRejections salt
 where
  fn ::
    forall a.
    (a, [(peeraddr, ObjectDecision objectId object)]) ->
    (a, Map peeraddr (ObjectDecision objectId object))
  fn (a, as) = (a, Map.fromList as)

-- | Order peers by how useful the OBJECTs they have provided are.
--
-- OBJECTs delivered late will fail to apply because they were included in
-- a recently adopted block. Peers can race against each other by setting
-- `objectInflightMultiplicity` to > 1. In case of a tie a hash of the peeraddr
-- is used as a tie breaker. Since every invocation use a new salt a given
-- peeraddr does not have an advantage over time.
orderByRejections ::
  Hashable peeraddr =>
  Int ->
  Map peeraddr (PeerObjectState objectId object) ->
  [(peeraddr, PeerObjectState objectId object)]
orderByRejections salt =
  List.sortOn (\(peeraddr, ps) -> (score ps, hashWithSalt salt peeraddr))
    . Map.toList

-- | Internal state of `pickObjectsToDownload` computation.
data St peeraddr objectId object
  = St
  { stInflightSize :: !SizeInBytes
  -- ^ size of all `object`s in-flight.
  , stInflight :: !(Map objectId Int)
  -- ^ `objectId`s in-flight.
  , stAcknowledged :: !(Map objectId Int)
  -- ^ acknowledged `objectId` with multiplicities.  It is used to update
  -- `referenceCounts`.
  , stInSubmissionToObjectPoolObjects :: Set objectId
  -- ^ OBJECTs on their way to the objectpool. Used to prevent issueing new
  -- fetch requests for them.
  }

-- | Distribute `object`'s to download among available peers.  Peers are considered
-- in the given order.
--
-- * pick objects from the set of available object's (in `objectId` order, note these sets
--   might be different for different peers).
-- * pick objects until the peers in-flight limit (we can go over the limit by one object)
--   (`objectsSizeInflightPerPeer` limit)
-- * pick objects until the overall in-flight limit (we can go over the limit by one object)
--   (`maxObjectsSizeInflight` limit)
-- * each object can be downloaded simultaneously from at most
--   `objectInflightMultiplicity` peers.
pickObjectsToDownload ::
  forall peeraddr objectId object.
  ( Ord peeraddr
  , Ord objectId
  ) =>
  -- | decision policy
  ObjectDecisionPolicy ->
  -- | shared state
  SharedObjectState peeraddr objectId object ->
  [(peeraddr, PeerObjectState objectId object)] ->
  ( SharedObjectState peeraddr objectId object
  , [(peeraddr, ObjectDecision objectId object)]
  )
pickObjectsToDownload
  policy@ObjectDecisionPolicy
    { objectsSizeInflightPerPeer
    , maxObjectsSizeInflight
    , objectInflightMultiplicity
    }
  sharedState@SharedObjectState
    { peerObjectStates
    , inflightObjects
    , inflightObjectsSize
    , bufferedObjects
    , inSubmissionToObjectPoolObjects
    , referenceCounts
    } =
    -- outer fold: fold `[(peeraddr, PeerObjectState objectId object)]`
    List.mapAccumR
      accumFn
      -- initial state
      St
        { stInflight = inflightObjects
        , stInflightSize = inflightObjectsSize
        , stAcknowledged = Map.empty
        , stInSubmissionToObjectPoolObjects = Map.keysSet inSubmissionToObjectPoolObjects
        }
      >>> gn
   where
    accumFn ::
      St peeraddr objectId object ->
      (peeraddr, PeerObjectState objectId object) ->
      ( St peeraddr objectId object
      , ( (peeraddr, PeerObjectState objectId object)
        , ObjectDecision objectId object
        )
      )
    accumFn
      st@St
        { stInflight
        , stInflightSize
        , stAcknowledged
        , stInSubmissionToObjectPoolObjects
        }
      ( peeraddr
        , peerObjectState@PeerObjectState
            { availableObjectIds
            , unknownObjects
            , requestedObjectsInflight
            , requestedObjectsInflightSize
            }
        ) =
        let sizeInflightAll :: SizeInBytes
            sizeInflightOther :: SizeInBytes

            sizeInflightAll = stInflightSize
            sizeInflightOther = sizeInflightAll - requestedObjectsInflightSize
         in if sizeInflightAll >= maxObjectsSizeInflight
              then
                let ( numObjectIdsToAck
                      , numObjectIdsToReq
                      , objectsToObjectPool@ObjectsToObjectPool{listOfObjectsToObjectPool}
                      , RefCountDiff{objectIdsToAck}
                      , peerObjectState'
                      ) = acknowledgeObjectIds policy sharedState peerObjectState

                    stAcknowledged' = Map.unionWith (+) stAcknowledged objectIdsToAck
                    stInSubmissionToObjectPoolObjects' =
                      stInSubmissionToObjectPoolObjects
                        <> Set.fromList (map fst listOfObjectsToObjectPool)
                 in if requestedObjectIdsInflight peerObjectState' > 0
                      then
                        -- we have objectIds to request
                        ( st
                            { stAcknowledged = stAcknowledged'
                            , stInSubmissionToObjectPoolObjects = stInSubmissionToObjectPoolObjects'
                            }
                        ,
                          ( (peeraddr, peerObjectState')
                          , ObjectDecision
                              { objectIdsToAcknowledge = numObjectIdsToAck
                              , objectIdsToRequest = numObjectIdsToReq
                              , objectPipelineObjectIds =
                                  not
                                    . StrictSeq.null
                                    . unacknowledgedObjectIds
                                    $ peerObjectState'
                              , objectsToRequest = Map.empty
                              , objectsToObjectPool = objectsToObjectPool
                              }
                          )
                        )
                      else
                        -- there are no `objectId`s to request, nor we can request `object`s due
                        -- to in-flight size limits
                        ( st
                        ,
                          ( (peeraddr, peerObjectState')
                          , emptyObjectDecision
                          )
                        )
              else
                let requestedObjectsInflightSize' :: SizeInBytes
                    objectsToRequestMap :: Map objectId SizeInBytes

                    (requestedObjectsInflightSize', objectsToRequestMap) =
                      -- inner fold: fold available `objectId`s
                      --
                      -- Note: although `Map.foldrWithKey` could be used here, it
                      -- does not allow to short circuit the fold, unlike
                      -- `foldWithState`.
                      foldWithState
                        ( \(objectId, (objectSize, inflightMultiplicity)) sizeInflight ->
                            if -- note that we pick `objectId`'s as long the `s` is
                            -- smaller or equal to `objectsSizeInflightPerPeer`.
                            sizeInflight <= objectsSizeInflightPerPeer
                              -- overall `object`'s in-flight must be smaller than
                              -- `maxObjectsSizeInflight`
                              && sizeInflight + sizeInflightOther <= maxObjectsSizeInflight
                              -- the transaction must not be downloaded from more
                              -- than `objectInflightMultiplicity` peers simultaneously
                              && inflightMultiplicity < objectInflightMultiplicity
                              -- TODO: we must validate that `objectSize` is smaller than
                              -- maximum objects size
                              then Just (sizeInflight + objectSize, (objectId, objectSize))
                              else Nothing
                        )
                        ( Map.assocs $
                            -- merge `availableObjectIds` with `stInflight`, so we don't
                            -- need to lookup into `stInflight` on every `objectId` which
                            -- is in `availableObjectIds`.
                            Map.merge
                              (Map.mapMaybeMissing \_objectId -> Just . (,0))
                              Map.dropMissing
                              (Map.zipWithMatched \_objectId -> (,))
                              availableObjectIds
                              stInflight
                              -- remove `object`s which were already downloaded by some
                              -- other peer or are in-flight or unknown by this peer.
                              `Map.withoutKeys` ( Map.keysSet bufferedObjects
                                                    <> requestedObjectsInflight
                                                    <> unknownObjects
                                                    <> stInSubmissionToObjectPoolObjects
                                                )
                        )
                        requestedObjectsInflightSize
                    -- pick from `objectId`'s which are available from that given
                    -- peer.  Since we are folding a dictionary each `objectId`
                    -- will be selected only once from a given peer (at least
                    -- in each round).

                    objectsToRequest = Map.keysSet objectsToRequestMap
                    peerObjectState' =
                      peerObjectState
                        { requestedObjectsInflightSize = requestedObjectsInflightSize'
                        , requestedObjectsInflight =
                            requestedObjectsInflight
                              <> objectsToRequest
                        }

                    ( numObjectIdsToAck
                      , numObjectIdsToReq
                      , objectsToObjectPool@ObjectsToObjectPool{listOfObjectsToObjectPool}
                      , RefCountDiff{objectIdsToAck}
                      , peerObjectState''
                      ) = acknowledgeObjectIds policy sharedState peerObjectState'

                    stAcknowledged' = Map.unionWith (+) stAcknowledged objectIdsToAck

                    stInflightDelta :: Map objectId Int
                    stInflightDelta = Map.fromSet (\_ -> 1) objectsToRequest
                    -- note: this is right since every `objectId`
                    -- could be picked at most once

                    stInflight' :: Map objectId Int
                    stInflight' = Map.unionWith (+) stInflightDelta stInflight

                    stInSubmissionToObjectPoolObjects' =
                      stInSubmissionToObjectPoolObjects
                        <> Set.fromList (map fst listOfObjectsToObjectPool)
                 in if requestedObjectIdsInflight peerObjectState'' > 0
                      then
                        -- we can request `objectId`s & `object`s
                        ( St
                            { stInflight = stInflight'
                            , stInflightSize = sizeInflightOther + requestedObjectsInflightSize'
                            , stAcknowledged = stAcknowledged'
                            , stInSubmissionToObjectPoolObjects = stInSubmissionToObjectPoolObjects'
                            }
                        ,
                          ( (peeraddr, peerObjectState'')
                          , ObjectDecision
                              { objectIdsToAcknowledge = numObjectIdsToAck
                              , objectPipelineObjectIds =
                                  not
                                    . StrictSeq.null
                                    . unacknowledgedObjectIds
                                    $ peerObjectState''
                              , objectIdsToRequest = numObjectIdsToReq
                              , objectsToRequest = objectsToRequestMap
                              , objectsToObjectPool = objectsToObjectPool
                              }
                          )
                        )
                      else
                        -- there are no `objectId`s to request, only `object`s.
                        ( st
                            { stInflight = stInflight'
                            , stInflightSize = sizeInflightOther + requestedObjectsInflightSize'
                            , stInSubmissionToObjectPoolObjects = stInSubmissionToObjectPoolObjects'
                            }
                        ,
                          ( (peeraddr, peerObjectState'')
                          , emptyObjectDecision{objectsToRequest = objectsToRequestMap}
                          )
                        )

    gn ::
      ( St peeraddr objectId object
      , [((peeraddr, PeerObjectState objectId object), ObjectDecision objectId object)]
      ) ->
      ( SharedObjectState peeraddr objectId object
      , [(peeraddr, ObjectDecision objectId object)]
      )
    gn
      ( St
          { stInflight
          , stInflightSize
          , stAcknowledged
          }
        , as
        ) =
        let peerObjectStates' =
              Map.fromList ((\(a, _) -> a) <$> as)
                <> peerObjectStates

            referenceCounts' =
              Map.merge
                (Map.mapMaybeMissing \_ x -> Just x)
                (Map.mapMaybeMissing \_ _ -> assert False Nothing)
                ( Map.zipWithMaybeMatched \_ x y ->
                    if x > y
                      then Just $! x - y
                      else Nothing
                )
                referenceCounts
                stAcknowledged

            liveSet = Map.keysSet referenceCounts'

            bufferedObjects' =
              bufferedObjects
                `Map.restrictKeys` liveSet

            inSubmissionToObjectPoolObjects' =
              List.foldl' updateInSubmissionToObjectPoolObjects inSubmissionToObjectPoolObjects as
         in ( sharedState
                { peerObjectStates = peerObjectStates'
                , inflightObjects = stInflight
                , inflightObjectsSize = stInflightSize
                , bufferedObjects = bufferedObjects'
                , referenceCounts = referenceCounts'
                , inSubmissionToObjectPoolObjects = inSubmissionToObjectPoolObjects'
                }
            , -- exclude empty results
              mapMaybe
                ( \((a, _), b) -> case b of
                    ObjectDecision
                      { objectIdsToAcknowledge = 0
                      , objectIdsToRequest = 0
                      , objectsToRequest
                      , objectsToObjectPool = ObjectsToObjectPool{listOfObjectsToObjectPool}
                      }
                        | null objectsToRequest
                        , null listOfObjectsToObjectPool ->
                            Nothing
                    _ -> Just (a, b)
                )
                as
            )
       where
        updateInSubmissionToObjectPoolObjects ::
          forall a.
          Map objectId Int ->
          (a, ObjectDecision objectId object) ->
          Map objectId Int
        updateInSubmissionToObjectPoolObjects m (_, ObjectDecision{objectsToObjectPool}) =
          List.foldl' fn m (listOfObjectsToObjectPool objectsToObjectPool)
         where
          fn ::
            Map objectId Int ->
            (objectId, object) ->
            Map objectId Int
          fn x (objectId, _) =
            Map.alter
              ( \case
                  Nothing -> Just 1
                  Just n -> Just $! succ n
              )
              objectId
              x

-- | Filter peers which can either download a `object` or acknowledge `objectId`s.
filterActivePeers ::
  forall peeraddr objectId object.
  Ord objectId =>
  HasCallStack =>
  ObjectDecisionPolicy ->
  SharedObjectState peeraddr objectId object ->
  Map peeraddr (PeerObjectState objectId object)
filterActivePeers
  policy@ObjectDecisionPolicy
    { maxUnacknowledgedObjectIds
    , objectsSizeInflightPerPeer
    , maxObjectsSizeInflight
    , objectInflightMultiplicity
    }
  sharedObjectState@SharedObjectState
    { peerObjectStates
    , bufferedObjects
    , inflightObjects
    , inflightObjectsSize
    , inSubmissionToObjectPoolObjects
    }
    | inflightObjectsSize > maxObjectsSizeInflight =
        -- we might be able to request objectIds, we cannot download objects
        Map.filter fn peerObjectStates
    | otherwise =
        -- we might be able to request objectIds or objects.
        Map.filter gn peerObjectStates
   where
    unrequestable =
      Map.keysSet (Map.filter (>= objectInflightMultiplicity) inflightObjects)
        <> Map.keysSet bufferedObjects

    fn :: PeerObjectState objectId object -> Bool
    fn
      peerObjectState@PeerObjectState
        { requestedObjectIdsInflight
        } =
        requestedObjectIdsInflight == 0
          -- if a peer has objectIds in-flight, we cannot request more objectIds or objects.
          && requestedObjectIdsInflight + numOfUnacked <= maxUnacknowledgedObjectIds
          && objectIdsToRequest > 0
       where
        -- Split `unacknowledgedObjectIds'` into the longest prefix of `objectId`s which
        -- can be acknowledged and the unacknowledged `objectId`s.
        (objectIdsToRequest, _, unackedObjectIds) = splitAcknowledgedObjectIds policy sharedObjectState peerObjectState
        numOfUnacked = fromIntegral (StrictSeq.length unackedObjectIds)

    gn :: PeerObjectState objectId object -> Bool
    gn
      peerObjectState@PeerObjectState
        { unacknowledgedObjectIds
        , requestedObjectIdsInflight
        , requestedObjectsInflight
        , requestedObjectsInflightSize
        , availableObjectIds
        , unknownObjects
        } =
        ( requestedObjectIdsInflight == 0
            && requestedObjectIdsInflight + numOfUnacked <= maxUnacknowledgedObjectIds
            && objectIdsToRequest > 0
        )
          || (underSizeLimit && not (Map.null downloadable))
       where
        numOfUnacked = fromIntegral (StrictSeq.length unacknowledgedObjectIds)
        underSizeLimit = requestedObjectsInflightSize <= objectsSizeInflightPerPeer
        downloadable =
          availableObjectIds
            `Map.withoutKeys` requestedObjectsInflight
            `Map.withoutKeys` unknownObjects
            `Map.withoutKeys` unrequestable
            `Map.withoutKeys` Map.keysSet inSubmissionToObjectPoolObjects

        -- Split `unacknowledgedObjectIds'` into the longest prefix of `objectId`s which
        -- can be acknowledged and the unacknowledged `objectId`s.
        (objectIdsToRequest, _, _) = splitAcknowledgedObjectIds policy sharedObjectState peerObjectState

--
-- Auxiliary functions
--

-- | A fold with state implemented as a `foldr` to take advantage of fold-build
-- fusion optimisation.
foldWithState ::
  forall s a b c.
  Ord b =>
  (a -> s -> Maybe (s, (b, c))) ->
  [a] ->
  s ->
  (s, Map b c)
{-# INLINE foldWithState #-}
foldWithState f = foldr cons nil
 where
  cons ::
    a ->
    (s -> (s, Map b c)) ->
    (s -> (s, Map b c))
  cons a k = \ !s ->
    case f a s of
      Nothing -> nil s
      Just (!s', (!b, !c)) ->
        case Map.insert b c `second` k s' of
          r@(!_s, !_bs) -> r

  nil :: s -> (s, Map b c)
  nil = \ !s -> (s, Map.empty)
