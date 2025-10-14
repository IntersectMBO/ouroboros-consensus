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
import Data.Sequence.Strict (StrictSeq)

strictSeqToSet :: Ord a => StrictSeq a -> Set a
strictSeqToSet = Set.fromList . Foldable.toList

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
  -- We do it in two steps, because computing the acknowledgment tell which objects from dpsObjectsAvailableIds sets of each peer won't actually be available anymore (as soon as we ack them),
  -- so that the pickObjectsToReq function can take this into account.
  let (ackAndRequestIdsDecisions, peerToIdsToAck) = computeAck hasObject decisionPolicy globalState
      (globalState', peersToObjectsToReq) = pickObjectsToReq hasObject decisionPolicy globalState peerToIdsToAck
      completeDecisions = Map.intersectionWith (\decision objectsToReqIds -> decision{ pdObjectsToReqIds = objectsToReqIds }) ackAndRequestIdsDecisions peersToObjectsToReq
    in (globalState', completeDecisions)

-- | The ids to ack are the longest prefix of outstandingFifo of each peer that match the following criteria:
-- * either the object is owt pool for the peer who has downloaded it
-- * or the object is already in pool
computeAck ::
  forall peerAddr objectId object.
  ( Ord peerAddr
  , Ord objectId
  ) =>
  (objectId -> Bool) ->
  DecisionPolicy ->
  DecisionGlobalState peerAddr objectId object ->
  ( Map peerAddr (PeerDecision objectId object)
  , Map peerAddr (Set objectId)
  )
computeAck poolHasObject DecisionPolicy{dpMaxNumObjectIdsReq, dpMaxNumObjectsOutstanding} DecisionGlobalState{dgsPeerStates} =
  let (decisions, peerToIdsToAck) =
        Map.foldlWithKey' computeAckForPeer (Map.empty, Map.empty)  dgsPeerStates
   in ( decisions
      , peerToIdsToAck
      )

  where
  computeAckForPeer ::
    -- | Accumulator containing decisions already made for other peers
    -- It's a map in which we need to insert the new decision into
    (Map peerAddr (PeerDecision objectId object), Map peerAddr (Set objectId)) ->
    peerAddr ->
    DecisionPeerState objectId object ->
    (Map peerAddr (PeerDecision objectId object), Map peerAddr (Set objectId))
  computeAckForPeer (decisionsAcc, peerToIdsToAck) peerAddr DecisionPeerState{dpsOutstandingFifo, dpsObjectsOwtPool, dpsNumIdsInflight} =
    let 
        -- we isolate the longest prefix of outstandingFifo that matches our ack criteria (see above in computeAck doc)
        (idsToAck, dpsOutstandingFifo') =
          StrictSeq.spanl
            (\objectId -> poolHasObject objectId || objectId `Map.member` dpsObjectsOwtPool)
            dpsOutstandingFifo

        pdNumIdsToAck = fromIntegral $ StrictSeq.length idsToAck

        futureFifoSizeOnOutboundPeer :: NumObjectIdsReq =
          -- the new known fifo state after we ack the idsToAck
          (fromIntegral $ StrictSeq.length dpsOutstandingFifo')
          -- plus the number of ids that we have already requested but we didn't receive yet
          -- that the outbound peer might consequently already have added to its fifo
            + dpsNumIdsInflight

        pdNumIdsToReq =
          (fromIntegral dpMaxNumObjectsOutstanding - futureFifoSizeOnOutboundPeer)
          `min` dpMaxNumObjectIdsReq
        
        pdCanPipelineIdsRequests = not . StrictSeq.null $ dpsOutstandingFifo'

        peerDecision = PeerDecision
          { pdNumIdsToAck
          , pdNumIdsToReq
          , pdCanPipelineIdsRequests
          , pdObjectsToReqIds = Set.empty -- we don't decide this here
          }

     in (Map.insert peerAddr peerDecision decisionsAcc, Map.insert peerAddr (strictSeqToSet idsToAck) peerToIdsToAck)

orderPeers :: Map peerAddr (DecisionPeerState objectId object) -> StdGen -> ([(peerAddr, DecisionPeerState objectId object)], StdGen)
orderPeers = undefined

-- TODO: be careful about additive semigroup instance of PeerDecision
-- e.g. what if an object is first available and picked to download, but the download request isn't emitted yet
-- then the object is received from another peer, so we can ack it from our peer on the next makeDecision call
-- So later when the download request actually takes place, we don't need the object anymore, and it will no
-- longer be part of dpsObjectsAvailableIds of the peer! But also no longer in the FIFO
-- So if the requestIds doing the ack has been made before the requestObject, then the server
-- won't be able to serve the object.

-- pdNumIdsToAck should probably be additive, because we can't recompute/recover how many ids were pre-acked before (as they have been removed from the FIFO and from dpsObjectsAvailableIds)

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
  -- | map from peer to the set of ids that will be acked for that peer on next requestIds
  -- we should treat these ids as not available anymore for the purpose of picking objects to request
  Map peerAddr (Set objectId) ->
  -- | new global state (with just RNG updated), and objects to request from each peer
  (DecisionGlobalState peerAddr objectId object, Map peerAddr (Set objectId))
pickObjectsToReq poolHasObject DecisionPolicy{dpMaxNumObjectsInflightPerPeer, dpMaxNumObjectsInflightTotal,dpMaxObjectInflightMultiplicity} globalState@DecisionGlobalState{dgsRng, dgsPeerStates, dgsObjectsInflightMultiplicities, dgsObjectsOwtPoolMultiplicities} peerToIdsToAck =
  (globalState{dgsRng = dgsRng'}, peersToObjectsToReq)
  where
      (orderedPeers, dgsRng') = orderPeers dgsPeerStates dgsRng

      -- We want to map each objectId to the sorted list of peers that can provide it
      -- For each peer we also indicate how many objects it has in flight at the moment
      -- We filter out here the objects that are already in pool
      objectsToSortedProviders :: Map objectId [(peerAddr, NumObjectsReq)]
      objectsToSortedProviders =
        -- We iterate over each peer and the corresponding available ids
        -- and turn the map "inside-out"
        Foldable.foldl'
          ( \accMap (peerAddr, DecisionPeerState{dpsObjectsAvailableIds, dpsObjectsInflightIds}) ->
              let -- ids that will be acked for this peer won't be available anymore, so we should not consider them in the decision logic
                  idsToAckForThisPeer = Map.findWithDefault (error "invariant violated: peer must be in peerToIdsToAck map") peerAddr peerToIdsToAck
                  -- we should also remove objects that are already in the pool
                  interestingAndAvailableObjectIds = Set.filter (not . poolHasObject) $
                                                      dpsObjectsAvailableIds `Set.difference` idsToAckForThisPeer
               in -- we iterate over interestingAndAvailableObjectIds and add the peer to the list of providers for each object it can provide
                  Foldable.foldl'
                    (\accMap' objectId -> Map.insertWith (++) objectId [(peerAddr, fromIntegral $ Set.size dpsObjectsInflightIds)] accMap')
                    accMap
                    interestingAndAvailableObjectIds
          )
          Map.empty
          orderedPeers

      -- We also want to know for each objects how many peers have it in the inflight or owtPool,
      -- meaning that we should receive them soon.
      objectsExpectedSoonMultiplicities :: Map objectId ObjectMultiplicity
      objectsExpectedSoonMultiplicities = Map.unionWith (+) dgsObjectsInflightMultiplicities dgsObjectsOwtPoolMultiplicities

      -- Now we join objectsToSortedProviders and objectsExpectedSoonMultiplicities maps on objectId for easy fold
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
      
      -- Now we compute the actual attribution of downloads for peers
      DownloadPickState{peersToObjectsToReq} =
        -- We iterate over each objectId and the corresponding (providers, expectedMultiplicity)
        Map.foldlWithKey'
          ( \st objectId (providers, expectedMultiplicity) ->
              -- reset the objectMultiplicity counter for each new objectId
              let st' = st{objectMultiplicity = 0}

              -- We iterate over the list of providers, and pick them or not according to the current state
              -- When a peer is selected as a provider for this objectId, we insert the objectId in the peer's set in peersToObjectsToReq (inside St)
              -- So the result of the filtering of providers is part of the final St state
              in Foldable.foldl'
                    (howToFoldProviders objectId expectedMultiplicity)
                    st'
                    providers
          )
          DownloadPickState{
              totalNumObjectsToReq = 0
            , objectMultiplicity = 0
            , peersToObjectsToReq = Map.empty
          }
          objectsToProvidersAndExpectedMultiplicities

      totalNumObjectsInflight :: NumObjectsReq
      totalNumObjectsInflight = fromIntegral $ Map.foldl' (+) 0 dgsObjectsInflightMultiplicities

      -- This function decides whether or not we should select a given peer as provider for the current objectId
      -- it takes into account if we are expecting to obtain the object from other sources (either inflight/owt pool already, or if the object will be requested from already selected peers in this given round)
      howToFoldProviders :: objectId -> ObjectMultiplicity -> DownloadPickState peerAddr objectId -> (peerAddr, NumObjectsReq) -> DownloadPickState peerAddr objectId
      howToFoldProviders objectId expectedMultiplicity st@DownloadPickState{totalNumObjectsToReq, objectMultiplicity, peersToObjectsToReq} (peerAddr, numObjectsInFlight) =
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
                DownloadPickState
                  { totalNumObjectsToReq = totalNumObjectsToReq + 1
                  , objectMultiplicity = objectMultiplicity + 1
                  , peersToObjectsToReq = Map.insert peerAddr (Set.insert objectId objectsToReq) peersToObjectsToReq
                  }
                -- Or we keep the state as is if we don't select this peer
              else st

data DownloadPickState peerAddr objectId =
  DownloadPickState
    { totalNumObjectsToReq :: !NumObjectsReq,
      objectMultiplicity :: ObjectMultiplicity,
      peersToObjectsToReq :: Map peerAddr (Set objectId)
    }
