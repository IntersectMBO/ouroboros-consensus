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
  ( PeerDecision (..)
  , emptyPeerDecision

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
  forall peerAddr objectId object.
  ( Ord peerAddr
  , Ord objectId
  , Hashable peerAddr
  ) =>
  -- | decision policy
  PeerDecisionPolicy ->
  -- | decision context
  DecisionGlobalState peerAddr objectId object ->
  -- | list of available peers.
  --
  -- This is a subset of `peerStates` of peers which either:
  -- * can be used to download a `object`,
  -- * can acknowledge some `objectId`s.
  Map peerAddr (DecisionPeerState objectId object) ->
  ( DecisionGlobalState peerAddr objectId object
  , Map peerAddr (PeerDecision objectId object)
  )
makeDecisions policy st =
  let (salt, rng') = random (orderRng st)
      st' = st{orderRng = rng'}
   in fn
        . pickObjectsToDownload policy st'
        . orderByRejections salt
 where
  fn ::
    forall a.
    (a, [(peerAddr, PeerDecision objectId object)]) ->
    (a, Map peerAddr (PeerDecision objectId object))
  fn (a, as) = (a, Map.fromList as)

-- | Order peers by how useful the OBJECTs they have provided are.
--
-- OBJECTs delivered late will fail to apply because they were included in
-- a recently adopted block. Peers can race against each other by setting
-- `objectInflightMultiplicity` to > 1. In case of a tie a hash of the peerAddr
-- is used as a tie breaker. Since every invocation use a new salt a given
-- peerAddr does not have an advantage over time.
orderByRejections ::
  Hashable peerAddr =>
  Int ->
  Map peerAddr (DecisionPeerState objectId object) ->
  [(peerAddr, DecisionPeerState objectId object)]
orderByRejections salt =
  List.sortOn (\(peerAddr, ps) -> (score ps, hashWithSalt salt peerAddr))
    . Map.toList

-- | Internal state of `pickObjectsToDownload` computation.
data St peerAddr objectId object
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
  forall peerAddr objectId object.
  ( Ord peerAddr
  , Ord objectId
  ) =>
  -- | decision policy
  PeerDecisionPolicy ->
  -- | shared state
  DecisionGlobalState peerAddr objectId object ->
  [(peerAddr, DecisionPeerState objectId object)] ->
  ( DecisionGlobalState peerAddr objectId object
  , [(peerAddr, PeerDecision objectId object)]
  )
pickObjectsToDownload
  policy@PeerDecisionPolicy
    { objectsSizeInflightPerPeer
    , maxObjectsSizeInflight
    , objectInflightMultiplicity
    }
  sharedState@DecisionGlobalState
    { peerStates
    , globalInFlightObjects
    , globalInFlightObjectsSize
    , globalObtainedButNotAckedObjects
    , globalToPoolObjects
    , referenceCounts
    } =
    -- outer fold: fold `[(peerAddr, DecisionPeerState objectId object)]`
    List.mapAccumR
      accumFn
      -- initial state
      St
        { stInflight = globalInFlightObjects
        , stInflightSize = globalInFlightObjectsSize
        , stAcknowledged = Map.empty
        , stInSubmissionToObjectPoolObjects = Map.keysSet globalToPoolObjects
        }
      >>> gn
   where
    accumFn ::
      St peerAddr objectId object ->
      (peerAddr, DecisionPeerState objectId object) ->
      ( St peerAddr objectId object
      , ( (peerAddr, DecisionPeerState objectId object)
        , PeerDecision objectId object
        )
      )
    accumFn
      st@St
        { stInflight
        , stInflightSize
        , stAcknowledged
        , stInSubmissionToObjectPoolObjects
        }
      ( peerAddr
        , peerObjectState@DecisionPeerState
            { availableObjectIds
            , requestedButNotReceived
            , inFlight
            , inFlightSize
            }
        ) =
        let sizeInflightAll :: SizeInBytes
            sizeInflightOther :: SizeInBytes

            sizeInflightAll = stInflightSize
            sizeInflightOther = sizeInflightAll - inFlightSize
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
                 in if numIdsInFlight peerObjectState' > 0
                      then
                        -- we have objectIds to request
                        ( st
                            { stAcknowledged = stAcknowledged'
                            , stInSubmissionToObjectPoolObjects = stInSubmissionToObjectPoolObjects'
                            }
                        ,
                          ( (peerAddr, peerObjectState')
                          , PeerDecision
                              { objectIdsToAcknowledge = numObjectIdsToAck
                              , objectIdsToRequest = numObjectIdsToReq
                              , objectPipelineObjectIds =
                                  not
                                    . StrictSeq.null
                                    . outstandingFifo
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
                          ( (peerAddr, peerObjectState')
                          , emptyPeerDecision
                          )
                        )
              else
                let inFlightSize' :: SizeInBytes
                    objectsToRequestMap :: Map objectId SizeInBytes

                    (inFlightSize', objectsToRequestMap) =
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
                              `Map.withoutKeys` ( Map.keysSet globalObtainedButNotAckedObjects
                                                    <> inFlight
                                                    <> requestedButNotReceived
                                                    <> stInSubmissionToObjectPoolObjects
                                                )
                        )
                        inFlightSize
                    -- pick from `objectId`'s which are available from that given
                    -- peer.  Since we are folding a dictionary each `objectId`
                    -- will be selected only once from a given peer (at least
                    -- in each round).

                    objectsToRequest = Map.keysSet objectsToRequestMap
                    peerObjectState' =
                      peerObjectState
                        { inFlightSize = inFlightSize'
                        , inFlight =
                            inFlight
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
                 in if numIdsInFlight peerObjectState'' > 0
                      then
                        -- we can request `objectId`s & `object`s
                        ( St
                            { stInflight = stInflight'
                            , stInflightSize = sizeInflightOther + inFlightSize'
                            , stAcknowledged = stAcknowledged'
                            , stInSubmissionToObjectPoolObjects = stInSubmissionToObjectPoolObjects'
                            }
                        ,
                          ( (peerAddr, peerObjectState'')
                          , PeerDecision
                              { objectIdsToAcknowledge = numObjectIdsToAck
                              , objectPipelineObjectIds =
                                  not
                                    . StrictSeq.null
                                    . outstandingFifo
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
                            , stInflightSize = sizeInflightOther + inFlightSize'
                            , stInSubmissionToObjectPoolObjects = stInSubmissionToObjectPoolObjects'
                            }
                        ,
                          ( (peerAddr, peerObjectState'')
                          , emptyPeerDecision{objectsToRequest = objectsToRequestMap}
                          )
                        )

    gn ::
      ( St peerAddr objectId object
      , [((peerAddr, DecisionPeerState objectId object), PeerDecision objectId object)]
      ) ->
      ( DecisionGlobalState peerAddr objectId object
      , [(peerAddr, PeerDecision objectId object)]
      )
    gn
      ( St
          { stInflight
          , stInflightSize
          , stAcknowledged
          }
        , as
        ) =
        let peerStates' =
              Map.fromList ((\(a, _) -> a) <$> as)
                <> peerStates

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

            globalObtainedButNotAckedObjects' =
              globalObtainedButNotAckedObjects
                `Map.restrictKeys` liveSet

            globalToPoolObjects' =
              List.foldl' updateInSubmissionToObjectPoolObjects globalToPoolObjects as
         in ( sharedState
                { peerStates = peerStates'
                , globalInFlightObjects = stInflight
                , globalInFlightObjectsSize = stInflightSize
                , globalObtainedButNotAckedObjects = globalObtainedButNotAckedObjects'
                , referenceCounts = referenceCounts'
                , globalToPoolObjects = globalToPoolObjects'
                }
            , -- exclude empty results
              mapMaybe
                ( \((a, _), b) -> case b of
                    PeerDecision
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
          (a, PeerDecision objectId object) ->
          Map objectId Int
        updateInSubmissionToObjectPoolObjects m (_, PeerDecision{objectsToObjectPool}) =
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
  forall peerAddr objectId object.
  Ord objectId =>
  HasCallStack =>
  PeerDecisionPolicy ->
  DecisionGlobalState peerAddr objectId object ->
  Map peerAddr (DecisionPeerState objectId object)
filterActivePeers
  policy@PeerDecisionPolicy
    { maxUnacknowledgedObjectIds
    , objectsSizeInflightPerPeer
    , maxObjectsSizeInflight
    , objectInflightMultiplicity
    }
  sharedObjectState@DecisionGlobalState
    { peerStates
    , globalObtainedButNotAckedObjects
    , globalInFlightObjects
    , globalInFlightObjectsSize
    , globalToPoolObjects
    }
    | globalInFlightObjectsSize > maxObjectsSizeInflight =
        -- we might be able to request objectIds, we cannot download objects
        Map.filter fn peerStates
    | otherwise =
        -- we might be able to request objectIds or objects.
        Map.filter gn peerStates
   where
    unrequestable =
      Map.keysSet (Map.filter (>= objectInflightMultiplicity) globalInFlightObjects)
        <> Map.keysSet globalObtainedButNotAckedObjects

    fn :: DecisionPeerState objectId object -> Bool
    fn
      peerObjectState@DecisionPeerState
        { numIdsInFlight
        } =
        numIdsInFlight == 0
          -- if a peer has objectIds in-flight, we cannot request more objectIds or objects.
          && numIdsInFlight + numOfUnacked <= maxUnacknowledgedObjectIds
          && objectIdsToRequest > 0
       where
        -- Split `outstandingFifo'` into the longest prefix of `objectId`s which
        -- can be acknowledged and the unacknowledged `objectId`s.
        (objectIdsToRequest, _, unackedObjectIds) = splitAcknowledgedObjectIds policy sharedObjectState peerObjectState
        numOfUnacked = fromIntegral (StrictSeq.length unackedObjectIds)

    gn :: DecisionPeerState objectId object -> Bool
    gn
      peerObjectState@DecisionPeerState
        { outstandingFifo
        , numIdsInFlight
        , inFlight
        , inFlightSize
        , availableObjectIds
        , requestedButNotReceived
        } =
        ( numIdsInFlight == 0
            && numIdsInFlight + numOfUnacked <= maxUnacknowledgedObjectIds
            && objectIdsToRequest > 0
        )
          || (underSizeLimit && not (Map.null downloadable))
       where
        numOfUnacked = fromIntegral (StrictSeq.length outstandingFifo)
        underSizeLimit = inFlightSize <= objectsSizeInflightPerPeer
        downloadable =
          availableObjectIds
            `Map.withoutKeys` inFlight
            `Map.withoutKeys` requestedButNotReceived
            `Map.withoutKeys` unrequestable
            `Map.withoutKeys` Map.keysSet globalToPoolObjects

        -- Split `outstandingFifo'` into the longest prefix of `objectId`s which
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
