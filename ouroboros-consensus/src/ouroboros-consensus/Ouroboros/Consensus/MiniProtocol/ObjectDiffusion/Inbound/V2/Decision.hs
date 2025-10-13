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
  , mempty

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
import Data.Foldable qualified as Foldable
import GHC.Stack (HasCallStack)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Policy
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.State
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
import Ouroboros.Network.Protocol.ObjectDiffusion.Type
import System.Random (random, StdGen)

-- | Make download decisions.
makeDecisions ::
  forall peerAddr objectId object.
  ( Ord peerAddr
  , Ord objectId
  , Hashable peerAddr
  ) =>
  (objectId -> Bool) ->
  -- | decision decisionPolicy
  DecisionPolicy ->
  -- | decision context
  DecisionGlobalState peerAddr objectId object ->
  ( DecisionGlobalState peerAddr objectId object
  , Map peerAddr (PeerDecision objectId object)
  )
makeDecisions hasObject decisionPolicy globalState =
  -- We do it in two steps, because pre-acknowledging will remove objects from dpsObjectsAvailableIds sets of each peer,
  -- that the pickObjectsToReq function will use to decide which objects can be requested.
  let (globalState', ackAndRequestIdsDecisions) = preAcknowledge hasObject decisionPolicy globalState
      (globalState'', objectsToReqSets) = pickObjectsToReq hasObject decisionPolicy globalState'
    in (globalState'', Map.intersectionWith (\decision objectsToReqIds -> decision{ pdObjectsToReqIds = objectsToReqIds }) ackAndRequestIdsDecisions objectsToReqSets)

-- | We pre-acknowledge the longest prefix of outstandingFifo of each peer that match the following criteria:
-- * either the object is owt pool for the peer who has downloaded it
-- * or the object is already in pool
preAcknowledge ::
  forall peerAddr objectId object.
  ( Ord peerAddr
  , Ord objectId
  ) =>
  (objectId -> Bool) ->
  DecisionPolicy ->
  DecisionGlobalState peerAddr objectId object ->
  ( DecisionGlobalState peerAddr objectId object
  , Map peerAddr (PeerDecision objectId object)
  )
preAcknowledge poolHasObject decisionPolicy globalState@DecisionGlobalState{dgsPeerStates} =
  -- We use `Map.mapAccumWithKey` to traverse the peer states and both update
  -- the peer states and accumulate ack decisions made along the way.
  let (decisions, dgsPeerStates') =
        Map.mapAccumWithKey preAcknowledgeForPeer Map.empty dgsPeerStates
   in ( globalState{dgsPeerStates = dgsPeerStates'}
      , decisions
      )

  where
  preAcknowledgeForPeer ::
    -- | Accumulator containing decisions already made for other peers
    -- It's a map in which we need to insert the new decision into
    Map peerAddr (PeerDecision objectId object) ->
    peerAddr ->
    DecisionPeerState objectId object ->
    (Map peerAddr (PeerDecision objectId object), DecisionPeerState objectId object)
  preAcknowledgeForPeer decisionsAcc peerAddr peerState@DecisionPeerState{dpsOutstandingFifo, dpsObjectsAvailableIds, dpsObjectsOwtPool} =
    let 
        -- we isolate the longest prefix of outstandingFifo that matches our ack criteria (see above in preAcknowledge doc)
        (idsToAck, dpsOutstandingFifo') =
          StrictSeq.spanl
            (\objectId -> poolHasObject objectId || objectId `Map.member` dpsObjectsOwtPool)
            dpsOutstandingFifo

        -- we remove the acknowledged ids from dpsObjectsAvailableIds if they were present
        -- we need to do that because objects that were advertised by this corresponding outbound peer
        -- but never downloaded because we already have them in pool were consequently never removed
        -- from dpsObjectsAvailableIds by onRequestObjects
        dpsObjectsAvailableIds' =
          Foldable.foldl' (\set objectId -> Set.delete objectId set) dpsObjectsAvailableIds idsToAck


        pdNumIdsToAck = fromIntegral $ StrictSeq.length idsToAck

        -- should this be incremental or overwrite the previous value in the semigroup instance?
        pdNumIdsToReq =
    --         numOfUnacked = StrictSeq.length dpsOutstandingFifo
    -- numOfAcked = StrictSeq.length acknowledgedObjectIds'
    -- unackedAndRequested = fromIntegral numOfUnacked + dpsNumIdsInflight

    -- pdNumIdsToReq =
    --   assert (unackedAndRequested <= dpMaxNumObjectsOutstanding) $
    --     assert (dpsNumIdsInflight <= dpMaxNumObjectIdsReq) $
    --       (dpMaxNumObjectsOutstanding - unackedAndRequested + fromIntegral numOfAcked)
    --         `min` (dpMaxNumObjectIdsReq - dpsNumIdsInflight)
            undefined -- TODO
        
        pdCanPipelineIdsRequests = not . StrictSeq.null $ dpsOutstandingFifo'

        peerDecision = PeerDecision
          { pdNumIdsToAck
          , pdNumIdsToReq
          , pdCanPipelineIdsRequests
          , pdObjectsToReqIds = Set.empty -- we don't decide this here
          }

        peerState' = peerState
          { dpsOutstandingFifo = dpsOutstandingFifo'
          , dpsObjectsAvailableIds = dpsObjectsAvailableIds'
          }

     in (Map.insert peerAddr peerDecision decisionsAcc, peerState')

orderPeers :: Map peerAddr (DecisionPeerState objectId object) -> StdGen -> ([(peerAddr, DecisionPeerState objectId object)], StdGen)
orderPeers = undefined

-- TODO: be careful about additive semigroup instance of PeerDecision
-- e.g. what if an object is first available and picked to download, but the download request isn't emitted yet
-- then the object is received from another peer, so we can ack it from our peer on the next makeDecision call
-- So later when the download request actually takes place, we don't need the object anymore, and it will no
-- longer be part of dpsObjectsAvailableIds of the peer! But also no longer in the FIFO
-- So if the requestIds doing the ack has been made before the requestObject, then the server
-- won't be able to serve the object.

-- | This function could just be pure if it hadn't be for the rng used to order peers
pickObjectsToReq ::
  forall peerAddr objectId object.
  ( Ord peerAddr
  , Ord objectId
  , Hashable peerAddr
  ) =>
  (objectId -> Bool) ->
  DecisionPolicy ->
  DecisionGlobalState peerAddr objectId object ->
  (DecisionGlobalState peerAddr objectId object
  , Map peerAddr (Set objectId))
pickObjectsToReq poolHasObject DecisionPolicy{dpMaxNumObjectsInflightPerPeer, dpMaxNumObjectsInflightTotal,dpMaxObjectInflightMultiplicity} globalState@DecisionGlobalState{dgsRng, dgsPeerStates, dgsObjectsInflightMultiplicities, dgsObjectsOwtPoolMultiplicities} =
  -- objects that are inflight or owtPool for a given peer are no longer in dpsObjectsAvailableIds of this peer
  -- so we only need to filter out dpsObjectsAvailableIds by removing the objects that are already in pool

  let objectsExpectedSoonMultiplicities = Map.unionWith (+) dgsObjectsInflightMultiplicities dgsObjectsOwtPoolMultiplicities

      (orderedPeers, dgsRng') = orderPeers dgsPeerStates dgsRng

      -- We want to map each objectId to the sorted list of peers that can provide it (and from which we should download them preferably)
      -- For each peer we also indicate how many objects it has in flight at the moment
      -- We filter out here the objects that are already in pool
      objectsToSortedProviders :: Map objectId [(peerAddr, NumObjectsReq)]
      objectsToSortedProviders =
        -- We iterate over each peer and the corresponding dpsObjectsAvailableIds
        Foldable.foldl'
          ( \accMap (peerAddr, DecisionPeerState{dpsObjectsAvailableIds, dpsObjectsInflightIds}) ->
              -- For each peer, we iterate over dpsObjectsAvailableIds filtered from the objects that are already in pool
              Foldable.foldl'
                (\accMap' objectId -> Map.insertWith (++) objectId [(peerAddr, fromIntegral $ Set.size dpsObjectsInflightIds)] accMap')
                accMap
                (Set.filter (not . poolHasObject) dpsObjectsAvailableIds)
          )
          Map.empty
          orderedPeers
      
      totalNumObjectsInflight :: NumObjectsReq
      totalNumObjectsInflight = fromIntegral $ Map.foldl' (+) 0 dgsObjectsInflightMultiplicities
      
      -- Now we combine these maps for easy fold
      objectsToProvidersAndExpectedMultiplicities :: Map objectId ([(peerAddr, NumObjectsReq)], ObjectMultiplicity)
      objectsToProvidersAndExpectedMultiplicities =
        Map.merge
          -- if an objectId is missing from objectsExpectedSoonMultiplicities, then its expected multiplicity is 0
          (Map.mapMissing \_ providers -> (providers, 0))
          -- if an objectId is missing from objectsToSortedProviders, then we don't care about it
          Map.dropMissing
          -- Combine in a tuple the list of providers and the expected multiplicity
          (Map.zipWithMatched \_ providers expectedMultiplicity -> (providers, expectedMultiplicity))
          objectsToSortedProviders
          objectsExpectedSoonMultiplicities
      
      St{peersToObjectsToReq} =
        -- We iterate over each objectId and the corresponding (providers, expectedMultiplicity)
        Map.foldlWithKey'
          ( \st objectId (providers, expectedMultiplicity) ->
              -- reset the objectMultiplicity counter for each new objectId
              let st' = st{objectMultiplicity = 0}

              in Foldable.foldl'
                    (howToFoldProviderList objectId expectedMultiplicity)
                    st'
                    providers
          )
          St{
              totalNumObjectsToReq = 0
            , objectMultiplicity = 0
            , peersToObjectsToReq = Map.empty
          }
          objectsToProvidersAndExpectedMultiplicities

      -- This function decides whether or not we should select a given peer as provider for the current objectId
      -- it takes into account if we are expecting to obtain the object from other sources (either inflight/owt pool already, or if the object will be requested from already selected peers in this given round)
      howToFoldProviderList :: objectId -> ObjectMultiplicity -> St peerAddr objectId -> (peerAddr, NumObjectsReq) -> St peerAddr objectId
      howToFoldProviderList objectId expectedMultiplicity st@St{totalNumObjectsToReq, objectMultiplicity, peersToObjectsToReq} (peerAddr, numObjectsInFlight) =
        let -- see what has already been attributed to this peer
            objectsToReq = Map.findWithDefault Set.empty peerAddr peersToObjectsToReq

            shouldSelect =
              -- We should not go over the multiplicity limit per object
              objectMultiplicity + expectedMultiplicity < dpMaxObjectInflightMultiplicity
              -- We should not go over the total number of objects inflight limit
              && totalNumObjectsInflight + totalNumObjectsToReq < dpMaxNumObjectsInflightTotal
              -- We should not go over the per-peer number of objects inflight limit
              && numObjectsInFlight + (fromIntegral $ Set.size objectsToReq) < dpMaxNumObjectsInflightPerPeer
          
          in if shouldSelect
              then
                -- We increase both global count and per-object count, and we add the object to the peer's set
                St
                  { totalNumObjectsToReq = totalNumObjectsToReq + 1
                  , objectMultiplicity = objectMultiplicity + 1
                  , peersToObjectsToReq = Map.insert peerAddr (Set.insert objectId objectsToReq) peersToObjectsToReq
                  }
                -- Or we keep the state as is if we don't select this peer
              else st

   in (globalState{dgsRng = dgsRng'}, peersToObjectsToReq)

data St peerAddr objectId = St { totalNumObjectsToReq :: !NumObjectsReq, objectMultiplicity :: ObjectMultiplicity, peersToObjectsToReq :: Map peerAddr (Set objectId) }

-------------------------------------------------------------------------------
-- OLD STUFF ONLY HERE FOR REFERENCE
-------------------------------------------------------------------------------

-- | Internal state of `pickObjectsToDownload` computation.
data DecisionInternalState peerAddr objectId object
  = DecisionInternalState
  { disNumObjectsInflight :: !NumObjectsReq
  -- ^ number of all `object`s in-flight.
  , disObjectsInflightMultiplicities :: !(Map objectId ObjectMultiplicity)
  -- ^ `objectId`s in-flight.
  , disIdsToAckMultiplicities :: !(Map objectId ObjectMultiplicity)
  -- ^ acknowledged `objectId` with multiplicities. It is used to update
  -- `dgsObjectsPendingMultiplicities`.
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
  -- | decision decisionPolicy
  DecisionPolicy ->
  -- | shared state
  DecisionGlobalState peerAddr objectId object ->
  [(peerAddr, DecisionPeerState objectId object)] ->
  ( DecisionGlobalState peerAddr objectId object
  , [(peerAddr, PeerDecision objectId object)]
  )
pickObjectsToDownload
  decisionPolicy@DecisionPolicy
    { dpMaxNumObjectsInflightPerPeer
    , dpMaxNumObjectsInflightTotal
    , dpMaxObjectInflightMultiplicity
    }
  sharedState@DecisionGlobalState
    { dgsPeerStates
    , dgsObjectsInflightMultiplicities
    , dgsObjectsOwtPoolMultiplicities
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
        , disObjectsOwtPoolIds = Map.keysSet dgsObjectsOwtPoolMultiplicities
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
            { dpsObjectsAvailableIds
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
                      , pdObjectsToSubmitToPoolIds
                      , RefCountDiff{rcdIdsToAckMultiplicities}
                      , peerObjectState'
                      ) = acknowledgeObjectIds decisionPolicy sharedState peerObjectState

                    disIdsToAckMultiplicities' = Map.unionWith (+) disIdsToAckMultiplicities rcdIdsToAckMultiplicities
                    disObjectsOwtPoolIds' =
                      disObjectsOwtPoolIds
                        <> Map.keysSet pdObjectsToSubmitToPoolIds
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
                              { pdNumIdsToAck = numObjectIdsToAck
                              , pdNumIdsToReq = numObjectIdsToReq
                              , pdCanPipelineIdsRequests =
                                  not
                                    . StrictSeq.null
                                    . dpsOutstandingFifo
                                    $ peerObjectState'
                              , pdObjectsToReqIds = Set.empty
                              }
                          )
                        )
                      else
                        -- there are no `objectId`s to request, nor we can request `object`s due
                        -- to in-flight size limits
                        ( st
                        ,
                          ( (peerAddr, peerObjectState')
                          , mempty
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
                            -- merge `dpsObjectsAvailableIds` with `disObjectsInflightMultiplicities`, so we don't
                            -- need to lookup into `disObjectsInflightMultiplicities` on every `objectId` which
                            -- is in `dpsObjectsAvailableIds`.
                            Map.merge
                              (Map.mapMaybeMissing \_objectId -> Just . (,0))
                              Map.dropMissing
                              (Map.zipWithMatched \_objectId -> (,))
                              dpsObjectsAvailableIds
                              disObjectsInflightMultiplicities
                              -- remove `object`s which were already downloaded by some
                              -- other peer or are in-flight or unknown by this peer.
                              `Set.unions` ( Map.keysSet dgsObjectsPendingMultiplicities
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
                      , pdObjectsToSubmitToPoolIds
                      , RefCountDiff{rcdIdsToAckMultiplicities}
                      , peerObjectState''
                      ) = acknowledgeObjectIds decisionPolicy sharedState peerObjectState'

                    disIdsToAckMultiplicities' = Map.unionWith (+) disIdsToAckMultiplicities rcdIdsToAckMultiplicities

                    stInflightDelta :: Map objectId Int
                    stInflightDelta = Map.fromSet (\_ -> 1) pdObjectsToReqIds
                    -- note: this is right since every `objectId`
                    -- could be picked at most once

                    disObjectsInflightMultiplicities' :: Map objectId Int
                    disObjectsInflightMultiplicities' = Map.unionWith (+) stInflightDelta disObjectsInflightMultiplicities

                    disObjectsOwtPoolIds' =
                      disObjectsOwtPoolIds
                        <> Set.fromList (map fst pdObjectsToSubmitToPoolIds)
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
                              { pdNumIdsToAck = numObjectIdsToAck
                              , pdCanPipelineIdsRequests =
                                  not
                                    . StrictSeq.null
                                    . dpsOutstandingFifo
                                    $ peerObjectState''
                              , pdNumIdsToReq = numObjectIdsToReq
                              , pdObjectsToReqIds = pdObjectsToReqIdsMap
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
                          , mempty{pdObjectsToReqIds = pdObjectsToReqIdsMap}
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

            dgsObjectsPendingMultiplicities' =
              Map.merge
                (Map.mapMaybeMissing \_ x -> Just x)
                (Map.mapMaybeMissing \_ _ -> assert False Nothing)
                ( Map.zipWithMaybeMatched \_ x y ->
                    if x > y
                      then Just $! x - y
                      else Nothing
                )
                dgsObjectsPendingMultiplicities
                disIdsToAckMultiplicities

            dgsObjectsOwtPoolMultiplicities' =
              List.foldl' updateInSubmissionToObjectPoolObjects dgsObjectsOwtPoolMultiplicities as
         in ( sharedState
                { dgsPeerStates = dgsPeerStates'
                , dgsObjectsInflightMultiplicities = disObjectsInflightMultiplicities
                , dgsObjectsOwtPoolMultiplicities = dgsObjectsOwtPoolMultiplicities'
                }
            , -- exclude empty results
              mapMaybe
                ( \((a, _), b) -> case b of
                    PeerDecision
                      { pdNumIdsToAck = 0
                      , pdNumIdsToReq = 0
                      , pdObjectsToReqIds
                       }
                        | null pdObjectsToReqIds
                        , Map.null pdObjectsToSubmitToPoolIds ->
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
        updateInSubmissionToObjectPoolObjects m (_, PeerDecision{}) =
          List.foldl' fn m (Map.toList pdObjectsToSubmitToPoolIds)
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
  decisionPolicy@DecisionPolicy
    { dpMaxNumObjectsOutstanding
    , dpMaxObjectInflightMultiplicity
    }
  globalState@DecisionGlobalState
    { dgsPeerStates
    , dgsObjectsInflightMultiplicities
    , dgsObjectsOwtPoolMultiplicities
    }
  = Map.filter gn dgsPeerStates
   where
    unrequestable =
      Map.keysSet (Map.filter (>= dpMaxObjectInflightMultiplicity) dgsObjectsInflightMultiplicities)
        <> Map.keysSet dgsObjectsPendingMultiplicities

    fn :: DecisionPeerState objectId object -> Bool
    fn
      peerObjectState@DecisionPeerState
        { dpsNumIdsInflight
        } =
        dpsNumIdsInflight == 0
          -- if a peer has objectIds in-flight, we cannot request more objectIds or objects.
          && dpsNumIdsInflight + numOfUnacked <= dpMaxNumObjectsOutstanding
          && pdNumIdsToReq > 0
       where
        -- Split `dpsOutstandingFifo'` into the longest prefix of `objectId`s which
        -- can be acknowledged and the unacknowledged `objectId`s.
        (pdNumIdsToReq, _, unackedObjectIds) = splitAcknowledgedObjectIds decisionPolicy globalState peerObjectState
        numOfUnacked = fromIntegral (StrictSeq.length unackedObjectIds)

    gn :: DecisionPeerState objectId object -> Bool
    gn
      peerObjectState@DecisionPeerState
        { dpsOutstandingFifo
        , dpsNumIdsInflight
        , dpsObjectsInflightIds
        , dpsObjectsAvailableIds
        } =
        ( dpsNumIdsInflight == 0
            && dpsNumIdsInflight + numOfUnacked <= dpMaxNumObjectsOutstanding
            && pdNumIdsToReq > 0
        )
          || (not (Set.null downloadable))
       where
        numOfUnacked = fromIntegral (StrictSeq.length dpsOutstandingFifo)
        downloadable =
          dpsObjectsAvailableIds
            `Set.difference` dpsObjectsInflightIds
            `Set.difference` dpsObjectsRequestedButNotReceivedIds
            `Set.difference` unrequestable
            `Set.difference` Map.keysSet dgsObjectsOwtPoolMultiplicities

        -- Split `dpsOutstandingFifo'` into the longest prefix of `objectId`s which
        -- can be acknowledged and the unacknowledged `objectId`s.
        (pdNumIdsToReq, _, _) = splitAcknowledgedObjectIds decisionPolicy globalState peerObjectState

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
    { dpsObjectsAvailableIds
    , dpsNumIdsInflight
    , dpsObjectsOwtPool
    } =
    -- We can only acknowledge objectIds when we can request new ones, since
    -- a `MsgRequestObjectIds` for 0 objectIds is a protocol error.
    if pdNumIdsToReq > 0
      then
        ( pdNumIdsToAck
        , pdNumIdsToReq
        , objectsOwtPool
        , refCountDiff
        , ps
            { dpsOutstandingFifo = dpsOutstandingFifo'
            , dpsObjectsAvailableIds = dpsObjectsAvailableIds'
            , dpsNumIdsInflight =
                dpsNumIdsInflight
                  + pdNumIdsToReq
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
    (pdNumIdsToReq, acknowledgedObjectIds, dpsOutstandingFifo') =
      splitAcknowledgedObjectIds decisionPolicy globalState ps

    objectsOwtPoolList =
      [ (objectId, object)
      | objectId <- toList toObjectPoolObjectIds
      , objectId `Map.notMember` dgsObjectsPendingMultiplicities globalState
      , object <- maybeToList $ objectId `Map.lookup` dpsObjectsPending
      ]
    (toObjectPoolObjectIds, _) =
      StrictSeq.spanl (`Map.member` dpsObjectsPending) acknowledgedObjectIds

    objectsOwtPool = Map.fromList objectsOwtPoolList

    dpsObjectsOwtPool' = dpsObjectsOwtPool <> objectsOwtPool

    (dpsObjectsPending', ackedDownloadedObjects) = Map.partitionWithKey (\objectId _ -> objectId `Set.member` liveSet) dpsObjectsPending
    -- lateObjects: objects which were downloaded by another peer before we
    -- downloaded them; it relies on that `objectToObjectPool` filters out
    -- `dgsObjectsPending`.
    lateObjects =
      Map.filterWithKey
        (\objectId _ -> objectId `Map.notMember` objectsOwtPool)
        ackedDownloadedObjects

    -- the set of live `objectIds`
    liveSet = Set.fromList (toList dpsOutstandingFifo')
    dpsObjectsAvailableIds' = dpsObjectsAvailableIds `Set.intersection` liveSet

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

    pdNumIdsToAck :: NumObjectIdsAck
    pdNumIdsToAck = fromIntegral $ StrictSeq.length acknowledgedObjectIds

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
    {
    }
  DecisionPeerState
    { dpsOutstandingFifo
    , dpsObjectsInflightIds
    , dpsNumIdsInflight
    } =
    (pdNumIdsToReq, acknowledgedObjectIds', dpsOutstandingFifo')
   where
    (acknowledgedObjectIds', dpsOutstandingFifo') =
      StrictSeq.spanl
        ( \objectId ->
            ( objectId `Map.member` dgsObjectsPendingMultiplicities
                || objectId `Set.member` dpsObjectsRequestedButNotReceivedIds
                || objectId `Map.member` dpsObjectsPending
            )
              && objectId `Set.notMember` dpsObjectsInflightIds
        )
        dpsOutstandingFifo
    numOfUnacked = StrictSeq.length dpsOutstandingFifo
    numOfAcked = StrictSeq.length acknowledgedObjectIds'
    unackedAndRequested = fromIntegral numOfUnacked + dpsNumIdsInflight

    pdNumIdsToReq =
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
updateRefCounts dgsObjectsPendingMultiplicities (RefCountDiff diff) =
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
    dgsObjectsPendingMultiplicities
    diff