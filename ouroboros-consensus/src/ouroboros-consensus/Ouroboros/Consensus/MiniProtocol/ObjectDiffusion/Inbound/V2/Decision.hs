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
  DecisionPolicy ->
  -- | decision context
  DecisionGlobalState peerAddr objectId object ->
  -- | list of available peers.
  --
  -- This is a subset of `dgsPeerStates` of peers which either:
  -- * can be used to download a `object`,
  -- * can acknowledge some `objectId`s.
  Map peerAddr (DecisionPeerState objectId object) ->
  ( DecisionGlobalState peerAddr objectId object
  , Map peerAddr (PeerDecision objectId object)
  )
makeDecisions policy st =
  let (salt, rng') = random (dgsRng st)
      st' = st{dgsRng = rng'}
   in fn
        . pickObjectsToDownload policy st'
        . orderByRejections salt
 where
  fn ::
    forall a.
    (a, [(peerAddr, PeerDecision objectId object)]) ->
    (a, Map peerAddr (PeerDecision objectId object))
  fn (a, as) = (a, Map.fromList as)

-- | Order peers by how useful the objects they have provided are.
--
-- objects delivered late will fail to apply because they were included in
-- a recently adopted block. Peers can race against each other by setting
-- `dpMaxObjectInflightMultiplicity` to > 1. In case of a tie a hash of the peerAddr
-- is used as a tie breaker. Since every invocation use a new salt a given
-- peerAddr does not have an advantage over time.
orderByRejections ::
  Hashable peerAddr =>
  Int ->
  Map peerAddr (DecisionPeerState objectId object) ->
  [(peerAddr, DecisionPeerState objectId object)]
orderByRejections salt =
  List.sortOn (\(peerAddr, ps) -> (dpsScore ps, hashWithSalt salt peerAddr))
    . Map.toList

-- | Internal state of `pickObjectsToDownload` computation.
data DecisionInternalState peerAddr objectId object
  = DecisionInternalState
  { disNumObjectsInflight :: !NumObjectsReq
  -- ^ number of all `object`s in-flight.
  , disObjectsInflightMultiplicities :: !(Map objectId Int)
  -- ^ `objectId`s in-flight.
  , disIdsToAckMultiplicities :: !(Map objectId Int)
  -- ^ acknowledged `objectId` with multiplicities.  It is used to update
  -- `dgsObjectReferenceCounts`.
  , disObjectsOwtPoolIds :: Set objectId
  -- ^ objects on their way to the objectpool. Used to prevent issueing new
  -- fetch requests for them.
  }

-- | Distribute `object`'s to download among available peers.  Peers are considered
-- in the given order.
--
-- * pick objects from the set of available object's (in `objectId` order, note these sets
--   might be different for different peers).
-- * pick objects until the peers in-flight limit (we can go over the limit by one object)
--   (`dpMaxNumObjectsInflightPerPeer` limit)
-- * pick objects until the overall in-flight limit (we can go over the limit by one object)
--   (`dpMaxNumObjectsInflightTotal` limit)
-- * each object can be downloaded simultaneously from at most
--   `dpMaxObjectInflightMultiplicity` peers.
pickObjectsToDownload ::
  forall peerAddr objectId object.
  ( Ord peerAddr
  , Ord objectId
  ) =>
  -- | decision policy
  DecisionPolicy ->
  -- | shared state
  DecisionGlobalState peerAddr objectId object ->
  [(peerAddr, DecisionPeerState objectId object)] ->
  ( DecisionGlobalState peerAddr objectId object
  , [(peerAddr, PeerDecision objectId object)]
  )
pickObjectsToDownload
  policy@DecisionPolicy
    { dpMaxNumObjectsInflightPerPeer
    , dpMaxNumObjectsInflightTotal
    , dpMaxObjectInflightMultiplicity
    }
  sharedState@DecisionGlobalState
    { dgsPeerStates
    , dgsObjectsInflightMultiplicities
    , dgsObjectsPending
    , dgsObjectsOwtPool
    , dgsObjectReferenceCounts
    } =
    -- outer fold: fold `[(peerAddr, DecisionPeerState objectId object)]`
    List.mapAccumR
      accumFn
      -- initial state
      DecisionInternalState
        { disObjectsInflightMultiplicities = dgsObjectsInflightMultiplicities
        , disNumObjectsInflight = fromIntegral $ Map.foldl' (+) 0 dgsObjectsInflightMultiplicities
              -- Thomas: not sure here if we must count disctinct objects in flight, or total number of objects in flight (considering multiplicities)
        , disIdsToAckMultiplicities = Map.empty
        , disObjectsOwtPoolIds = Map.keysSet dgsObjectsOwtPool
        }
      >>> gn
   where
    accumFn ::
      DecisionInternalState peerAddr objectId object ->
      (peerAddr, DecisionPeerState objectId object) ->
      ( DecisionInternalState peerAddr objectId object
      , ( (peerAddr, DecisionPeerState objectId object)
        , PeerDecision objectId object
        )
      )
    accumFn
      st@DecisionInternalState
        { disObjectsInflightMultiplicities
        , disNumObjectsInflight
        , disIdsToAckMultiplicities
        , disObjectsOwtPoolIds
        }
      ( peerAddr
        , peerObjectState@DecisionPeerState
            { dpsIdsAvailable
            , dpsObjectsInflightIds
            }
        ) =
        let sizeInflightAll :: NumObjectsReq
            sizeInflightOther :: NumObjectsReq

            sizeInflightAll = disNumObjectsInflight
            sizeInflightOther = sizeInflightAll - fromIntegral (Set.size dpsObjectsInflightIds)
         in if sizeInflightAll >= dpMaxNumObjectsInflightTotal
              then
                let ( numObjectIdsToAck
                      , numObjectIdsToReq
                      , pdObjectsOwtPool
                      , RefCountDiff{rcdIdsToAckMultiplicities}
                      , peerObjectState'
                      ) = acknowledgeObjectIds policy sharedState peerObjectState

                    disIdsToAckMultiplicities' = Map.unionWith (+) disIdsToAckMultiplicities rcdIdsToAckMultiplicities
                    disObjectsOwtPoolIds' =
                      disObjectsOwtPoolIds
                        <> Map.keysSet pdObjectsOwtPool
                 in if dpsNumIdsInflight peerObjectState' > 0
                      then
                        -- we have objectIds to request
                        ( st
                            { disIdsToAckMultiplicities = disIdsToAckMultiplicities'
                            , disObjectsOwtPoolIds = disObjectsOwtPoolIds'
                            }
                        ,
                          ( (peerAddr, peerObjectState')
                          , PeerDecision
                              { pdIdsToAck = numObjectIdsToAck
                              , pdIdsToReq = numObjectIdsToReq
                              , pdCanPipelineIdsReq =
                                  not
                                    . StrictSeq.null
                                    . dpsOutstandingFifo
                                    $ peerObjectState'
                              , pdObjectsToReqIds = Set.empty
                              , pdObjectsOwtPool = pdObjectsOwtPool
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
                let dpsObjectsInflightIdsNum' :: NumObjectsReq
                    pdObjectsToReqIdsMap :: Set objectId

                    (dpsObjectsInflightIdsNum', pdObjectsToReqIdsMap) =
                      -- inner fold: fold available `objectId`s
                      --
                      -- Note: although `Map.foldrWithKey` could be used here, it
                      -- does not allow to short circuit the fold, unlike
                      -- `foldWithState`.
                      foldWithState
                        ( \(objectId, (_objectSize, inflightMultiplicity)) sizeInflight ->
                            if -- note that we pick `objectId`'s as long the `s` is
                            -- smaller or equal to `dpMaxNumObjectsInflightPerPeer`.
                            sizeInflight <= dpMaxNumObjectsInflightPerPeer
                              -- overall `object`'s in-flight must be smaller than
                              -- `dpMaxNumObjectsInflightTotal`
                              && sizeInflight + sizeInflightOther <= dpMaxNumObjectsInflightTotal
                              -- the object must not be downloaded from more
                              -- than `dpMaxObjectInflightMultiplicity` peers simultaneously
                              && inflightMultiplicity < dpMaxObjectInflightMultiplicity
                              -- TODO: we must validate that `objectSize` is smaller than
                              -- maximum objects size
                              then Just (sizeInflight + objectSize, (objectId, objectSize))
                              else Nothing
                        )
                        ( Map.assocs $
                            -- merge `dpsIdsAvailable` with `disObjectsInflightMultiplicities`, so we don't
                            -- need to lookup into `disObjectsInflightMultiplicities` on every `objectId` which
                            -- is in `dpsIdsAvailable`.
                            Map.merge
                              (Map.mapMaybeMissing \_objectId -> Just . (,0))
                              Map.dropMissing
                              (Map.zipWithMatched \_objectId -> (,))
                              dpsIdsAvailable
                              disObjectsInflightMultiplicities
                              -- remove `object`s which were already downloaded by some
                              -- other peer or are in-flight or unknown by this peer.
                              `Set.unions` ( Map.keysSet dgsObjectsPending
                                                    <> dpsObjectsInflightIds
                                                    <> dpsObjectsRequestedButNotReceivedIds
                                                    <> disObjectsOwtPoolIds
                                                )
                        )
                        dpsObjectsInflightIdsNum
                    -- pick from `objectId`'s which are available from that given
                    -- peer.  Since we are folding a dictionary each `objectId`
                    -- will be selected only once from a given peer (at least
                    -- in each round).

                    pdObjectsToReqIds = Map.keysSet pdObjectsToReqIdsMap
                    peerObjectState' = peerObjectState {dpsObjectsInflightIds = dpsObjectsInflightIds <> pdObjectsToReqIds}

                    ( numObjectIdsToAck
                      , numObjectIdsToReq
                      , pdObjectsOwtPool
                      , RefCountDiff{rcdIdsToAckMultiplicities}
                      , peerObjectState''
                      ) = acknowledgeObjectIds policy sharedState peerObjectState'

                    disIdsToAckMultiplicities' = Map.unionWith (+) disIdsToAckMultiplicities rcdIdsToAckMultiplicities

                    stInflightDelta :: Map objectId Int
                    stInflightDelta = Map.fromSet (\_ -> 1) pdObjectsToReqIds
                    -- note: this is right since every `objectId`
                    -- could be picked at most once

                    disObjectsInflightMultiplicities' :: Map objectId Int
                    disObjectsInflightMultiplicities' = Map.unionWith (+) stInflightDelta disObjectsInflightMultiplicities

                    disObjectsOwtPoolIds' =
                      disObjectsOwtPoolIds
                        <> Set.fromList (map fst pdObjectsOwtPool)
                 in if dpsNumIdsInflight peerObjectState'' > 0
                      then
                        -- we can request `objectId`s & `object`s
                        ( DecisionInternalState
                            { disObjectsInflightMultiplicities = disObjectsInflightMultiplicities'
                            , disNumObjectsInflight = undefined
                            , disIdsToAckMultiplicities = disIdsToAckMultiplicities'
                            , disObjectsOwtPoolIds = disObjectsOwtPoolIds'
                            }
                        ,
                          ( (peerAddr, peerObjectState'')
                          , PeerDecision
                              { pdIdsToAck = numObjectIdsToAck
                              , pdCanPipelineIdsReq =
                                  not
                                    . StrictSeq.null
                                    . dpsOutstandingFifo
                                    $ peerObjectState''
                              , pdIdsToReq = numObjectIdsToReq
                              , pdObjectsToReqIds = pdObjectsToReqIdsMap
                              , pdObjectsOwtPool = pdObjectsOwtPool
                              }
                          )
                        )
                      else
                        -- there are no `objectId`s to request, only `object`s.
                        ( st
                            { disObjectsInflightMultiplicities = disObjectsInflightMultiplicities'
                            , disObjectsOwtPoolIds = disObjectsOwtPoolIds'
                            }
                        ,
                          ( (peerAddr, peerObjectState'')
                          , emptyPeerDecision{pdObjectsToReqIds = pdObjectsToReqIdsMap}
                          )
                        )

    gn ::
      ( DecisionInternalState peerAddr objectId object
      , [((peerAddr, DecisionPeerState objectId object), PeerDecision objectId object)]
      ) ->
      ( DecisionGlobalState peerAddr objectId object
      , [(peerAddr, PeerDecision objectId object)]
      )
    gn
      ( DecisionInternalState
          { disObjectsInflightMultiplicities
          , disIdsToAckMultiplicities
          }
        , as
        ) =
        let dgsPeerStates' =
              Map.fromList ((\(a, _) -> a) <$> as)
                <> dgsPeerStates

            dgsObjectReferenceCounts' =
              Map.merge
                (Map.mapMaybeMissing \_ x -> Just x)
                (Map.mapMaybeMissing \_ _ -> assert False Nothing)
                ( Map.zipWithMaybeMatched \_ x y ->
                    if x > y
                      then Just $! x - y
                      else Nothing
                )
                dgsObjectReferenceCounts
                disIdsToAckMultiplicities

            liveSet = Map.keysSet dgsObjectReferenceCounts'

            dgsObjectsPending' =
              dgsObjectsPending
                `Map.restrictKeys` liveSet

            dgsObjectsOwtPool' =
              List.foldl' updateInSubmissionToObjectPoolObjects dgsObjectsOwtPool as
         in ( sharedState
                { dgsPeerStates = dgsPeerStates'
                , dgsObjectsInflightMultiplicities = disObjectsInflightMultiplicities
                , dgsObjectsPending = dgsObjectsPending'
                , dgsObjectReferenceCounts = dgsObjectReferenceCounts'
                , dgsObjectsOwtPool = dgsObjectsOwtPool'
                }
            , -- exclude empty results
              mapMaybe
                ( \((a, _), b) -> case b of
                    PeerDecision
                      { pdIdsToAck = 0
                      , pdIdsToReq = 0
                      , pdObjectsToReqIds
                      , pdObjectsOwtPool }
                        | null pdObjectsToReqIds
                        , Map.null pdObjectsOwtPool ->
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
        updateInSubmissionToObjectPoolObjects m (_, PeerDecision{pdObjectsOwtPool}) =
          List.foldl' fn m (Map.toList pdObjectsOwtPool)
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

-- | Filter peers which can either download an `object` or acknowledge `objectId`s.
filterActivePeers ::
  forall peerAddr objectId object.
  Ord objectId =>
  HasCallStack =>
  DecisionPolicy ->
  DecisionGlobalState peerAddr objectId object ->
  Map peerAddr (DecisionPeerState objectId object)
filterActivePeers
  policy@DecisionPolicy
    { dpMaxNumObjectsOutstanding
    , dpMaxObjectInflightMultiplicity
    }
  sharedObjectState@DecisionGlobalState
    { dgsPeerStates
    , dgsObjectsPending
    , dgsObjectsInflightMultiplicities
    , dgsObjectsOwtPool
    }
  = Map.filter gn dgsPeerStates
   where
    unrequestable =
      Map.keysSet (Map.filter (>= dpMaxObjectInflightMultiplicity) dgsObjectsInflightMultiplicities)
        <> Map.keysSet dgsObjectsPending

    fn :: DecisionPeerState objectId object -> Bool
    fn
      peerObjectState@DecisionPeerState
        { dpsNumIdsInflight
        } =
        dpsNumIdsInflight == 0
          -- if a peer has objectIds in-flight, we cannot request more objectIds or objects.
          && dpsNumIdsInflight + numOfUnacked <= dpMaxNumObjectsOutstanding
          && pdIdsToReq > 0
       where
        -- Split `dpsOutstandingFifo'` into the longest prefix of `objectId`s which
        -- can be acknowledged and the unacknowledged `objectId`s.
        (pdIdsToReq, _, unackedObjectIds) = splitAcknowledgedObjectIds policy sharedObjectState peerObjectState
        numOfUnacked = fromIntegral (StrictSeq.length unackedObjectIds)

    gn :: DecisionPeerState objectId object -> Bool
    gn
      peerObjectState@DecisionPeerState
        { dpsOutstandingFifo
        , dpsNumIdsInflight
        , dpsObjectsInflightIds
        , dpsIdsAvailable
        } =
        ( dpsNumIdsInflight == 0
            && dpsNumIdsInflight + numOfUnacked <= dpMaxNumObjectsOutstanding
            && pdIdsToReq > 0
        )
          || (not (Set.null downloadable))
       where
        numOfUnacked = fromIntegral (StrictSeq.length dpsOutstandingFifo)
        downloadable =
          dpsIdsAvailable
            `Set.difference` dpsObjectsInflightIds
            `Set.difference` dpsObjectsRequestedButNotReceivedIds
            `Set.difference` unrequestable
            `Set.difference` Map.keysSet dgsObjectsOwtPool

        -- Split `dpsOutstandingFifo'` into the longest prefix of `objectId`s which
        -- can be acknowledged and the unacknowledged `objectId`s.
        (pdIdsToReq, _, _) = splitAcknowledgedObjectIds policy sharedObjectState peerObjectState

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
