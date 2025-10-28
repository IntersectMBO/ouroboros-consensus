{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Decision
  ( PeerDecision (..)
  , makeDecisions

    -- * Internal API exposed for testing
  , DecisionContext (..)
  ) where

import Data.Foldable qualified as Foldable
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence.Strict (StrictSeq)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set)
import Data.Set qualified as Set
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.State
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
import Ouroboros.Network.Protocol.ObjectDiffusion.Type
import System.Random (StdGen)

strictSeqToSet :: Ord a => StrictSeq a -> Set a
strictSeqToSet = Set.fromList . Foldable.toList

-- | Make download decisions.
makeDecisions ::
  forall peerAddr objectId object.
  ( Ord peerAddr
  , Ord objectId
  ) =>
  DecisionContext peerAddr objectId object ->
  -- | New decisions
  Map peerAddr (PeerDecision objectId object)
makeDecisions DecisionContext{dcRng, dcHasObject, dcDecisionPolicy, dcGlobalState, dcPrevDecisions} =
  let
    -- A subset of peers are currently executing a decision. We shouldn't update the decision for them
    frozenPeersToDecisions = Map.filter (\PeerDecision{pdStatus} -> pdStatus == DecisionBeingActedUpon) dcPrevDecisions

    -- We do it in two steps, because computing the acknowledgment tell which objects from dpsObjectsAvailableIds sets of each peer won't actually be available anymore (as soon as we ack them),
    -- so that the pickObjectsToReq function can take this into account.
    (ackAndRequestIdsDecisions, peerToIdsToAck) = computeAck dcHasObject dcDecisionPolicy dcGlobalState frozenPeersToDecisions
    peersToObjectsToReq =
      pickObjectsToReq
        dcRng
        dcHasObject
        dcDecisionPolicy
        dcGlobalState
        frozenPeersToDecisions
        peerToIdsToAck
   in
    Map.intersectionWith
      (\decision objectsToReqIds -> decision{pdObjectsToReqIds = objectsToReqIds})
      ackAndRequestIdsDecisions
      peersToObjectsToReq

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
  -- | Frozen peers and their previous decisions
  Map peerAddr (PeerDecision objectId object) ->
  ( Map peerAddr (PeerDecision objectId object)
  , Map peerAddr (Set objectId)
  )
computeAck poolHasObject DecisionPolicy{dpMaxNumObjectIdsReq, dpMaxNumObjectsOutstanding} DecisionGlobalState{dgsPeerStates} frozenPeersToDecisions =
  let
    -- We shouldn't create a new decision for peers that are currently executing a decision
    filteredPeerStates = Map.withoutKeys dgsPeerStates (Map.keysSet frozenPeersToDecisions)
    (decisions, peerToIdsToAck) =
      Map.foldlWithKey' computeAckForPeer (Map.empty, Map.empty) filteredPeerStates
   in
    ( decisions
    , peerToIdsToAck
    )
 where
  computeAckForPeer ::
    -- \| Accumulator containing decisions already made for other peers
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

      -- TODO: in the case where pdNumIdsToReq == 0, we know we actually won't be able to ack anything
      -- during this round. So it might make sense to set pdNumIdsToAck = 0 and idsToAck = mempty as well?
      -- If we do this change, we could add an assert in `V2.hs` that whenever pdNumIdsToReq == 0, then pdNumIdsToAck == 0 as well
      -- /!\ We should also revise documentation in V2.md accordingly

      pdCanPipelineIdsRequests = not . StrictSeq.null $ dpsOutstandingFifo'

      peerDecision =
        PeerDecision
          { pdNumIdsToAck
          , pdNumIdsToReq
          , pdCanPipelineIdsRequests
          , pdObjectsToReqIds = Set.empty -- we don't decide this here
          , pdStatus = DecisionUnread
          }
     in
      ( Map.insert peerAddr peerDecision decisionsAcc
      , Map.insert peerAddr (strictSeqToSet idsToAck) peerToIdsToAck
      )

orderPeers ::
  StdGen ->
  Map peerAddr (DecisionPeerState objectId object) ->
  [(peerAddr, DecisionPeerState objectId object)]
orderPeers _rng = undefined -- TODO

data DownloadPickState peerAddr objectId
  = DownloadPickState
  { totalNumObjectsToReq :: !NumObjectsReq
  , objectMultiplicity :: ObjectMultiplicity
  , peersToObjectsToReq :: Map peerAddr (Set objectId)
  }

-- | This function could just be pure if it hadn't be for the rng used to order peers
pickObjectsToReq ::
  forall peerAddr objectId object.
  ( Ord peerAddr
  , Ord objectId
  ) =>
  StdGen ->
  (objectId -> Bool) ->
  DecisionPolicy ->
  DecisionGlobalState peerAddr objectId object ->
  -- | Frozen peers and their previous decisions
  Map peerAddr (PeerDecision objectId object) ->
  -- | map from peer to the set of ids that will be acked for that peer on next requestIds
  -- we should treat these ids as not available anymore for the purpose of picking objects to request
  Map peerAddr (Set objectId) ->
  -- | new global state (with just RNG updated), and objects to request from each peer
  Map peerAddr (Set objectId)
pickObjectsToReq
  rng
  poolHasObject
  DecisionPolicy
    { dpMaxNumObjectsInflightPerPeer
    , dpMaxNumObjectsInflightTotal
    , dpTargetObjectRedundancy
    }
  DecisionGlobalState
    { dgsPeerStates
    }
  frozenPeersToDecisions
  peerToIdsToAck =
    peersToObjectsToReq
   where
    -- We order the peers that are not currently executing a decision
    orderedPeers = orderPeers rng (dgsPeerStates `Map.withoutKeys` Map.keysSet frozenPeersToDecisions)

    -- We want to map each objectId to the sorted list of peers that can provide it
    -- For each peer we also indicate how many objects it has in flight at the moment
    -- We filter out here the objects that are already in pool
    objectsToSortedProviders :: Map objectId [(peerAddr, NumObjectsReq)]
    objectsToSortedProviders =
      -- We iterate over each peer and the corresponding available ids
      -- and turn the map "inside-out"
      Foldable.foldl'
        ( \accMap (peerAddr, DecisionPeerState{dpsObjectsAvailableIds, dpsObjectsInflightIds}) ->
            let
              -- ids that will be acked for this peer won't be available anymore, so we should not consider them in the decision logic
              --
              -- TODO: this is quite redundant, because ack can only be made when the object is already in the pool (in which case it would have been filtered out anyway in next step) or when the object is in dpsObjectsOwtPool of this peer (in which case it shouldn't be anymore in dpsObjectsAvailableIds)
              idsToAckForThisPeer =
                Map.findWithDefault
                  (error "invariant violated: peer must be in peerToIdsToAck map")
                  peerAddr
                  peerToIdsToAck
              -- we should also remove objects that are already in the pool
              interestingAndAvailableObjectIds =
                Set.filter (not . poolHasObject) $
                  dpsObjectsAvailableIds `Set.difference` idsToAckForThisPeer
             in
              -- we iterate over interestingAndAvailableObjectIds and add the peer to the list of providers for each object it can provide
              Foldable.foldl'
                ( \accMap' objectId -> Map.insertWith (++) objectId [(peerAddr, fromIntegral $ Set.size dpsObjectsInflightIds)] accMap'
                )
                accMap
                interestingAndAvailableObjectIds
        )
        Map.empty
        orderedPeers

    frozenPeerStatesWithDecisions = Map.intersectionWith (,) dgsPeerStates frozenPeersToDecisions

    availablePeerStates = Map.withoutKeys dgsPeerStates (Map.keysSet frozenPeersToDecisions)

    -- For frozen peers, we should consider that the objects in pdObjectsToReqIds will be requested soon, so we should consider them as inflight for the purpose of picking objects to request for other peers
    objectsInFlightMultiplicitiesOfFrozenPeer =
      Map.foldl'
        ( \accMap (DecisionPeerState{dpsObjectsInflightIds}, PeerDecision{pdObjectsToReqIds}) ->
            Foldable.foldl'
              (\accMap' objectId -> Map.insertWith (+) objectId 1 accMap')
              accMap
              (Set.union dpsObjectsInflightIds pdObjectsToReqIds)
        )
        Map.empty
        frozenPeerStatesWithDecisions
    -- Finally, we add to the previous map the objects that are currently inflight from peers for which we will make a decision in this round
    objectsInFlightMultiplicities =
      Map.foldl'
        ( \accMap (DecisionPeerState{dpsObjectsInflightIds}) ->
            Foldable.foldl'
              (\accMap' objectId -> Map.insertWith (+) objectId 1 accMap')
              accMap
              dpsObjectsInflightIds
        )
        objectsInFlightMultiplicitiesOfFrozenPeer
        availablePeerStates

    totalNumObjectsInflight :: NumObjectsReq
    totalNumObjectsInflight = fromIntegral $ Map.foldl' (+) 0 objectsInFlightMultiplicities

    objectsOwtPoolMultiplicities =
      Map.foldl'
        ( \accMap (DecisionPeerState{dpsObjectsOwtPool}) ->
            Foldable.foldl'
              (\accMap' objectId -> Map.insertWith (+) objectId 1 accMap')
              accMap
              (Map.keys dpsObjectsOwtPool)
        )
        Map.empty
        dgsPeerStates

    -- We also want to know for each objects how many peers have it in the inflight or owtPool,
    -- meaning that we should receive them soon.
    -- We should also add here the objects that are in the pdObjectsToReqIds of each peer decision for frozen peers,
    -- if these ids are not already in dpsObjectsInflight or dpsObjectsOwtPool of this peer
    objectsExpectedSoonMultiplicities :: Map objectId ObjectMultiplicity
    objectsExpectedSoonMultiplicities = Map.unionWith (+) objectsInFlightMultiplicities objectsOwtPoolMultiplicities

    -- Now we join objectsToSortedProviders and objectsExpectedSoonMultiplicities maps on objectId for easy fold
    objectsToProvidersAndExpectedMultiplicities ::
      Map objectId ([(peerAddr, NumObjectsReq)], ObjectMultiplicity)
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

    -- NOW HERE TAKE PLACE THE ACTUAL DECISION LOGIC AND ATTRIBUTION OF OBJECTS TO PEERS

    -- The current decision logic is greedy on objects, so it will try to request as many copies of the same object as possible,
    -- meaning we will have optimal coverage of the first objects, but might not request some other objects at all if they are (only) provided by peers that are already saturated.

    -- Now we compute the actual attribution of downloads for peers
    DownloadPickState{peersToObjectsToReq} =
      -- We iterate over each objectId and the corresponding (providers, expectedMultiplicity)
      Map.foldlWithKey'
        ( \st objectId (providers, expectedMultiplicity) ->
            -- reset the objectMultiplicity counter for each new objectId
            let st' = st{objectMultiplicity = 0}
             in -- We iterate over the list of providers, and pick them or not according to the current state
                -- When a peer is selected as a provider for this objectId, we insert the objectId in the peer's set in peersToObjectsToReq (inside St)
                -- So the result of the filtering of providers is part of the final St state
                Foldable.foldl'
                  (howToFoldProviders objectId expectedMultiplicity)
                  st'
                  providers
        )
        DownloadPickState
          { totalNumObjectsToReq = 0
          , objectMultiplicity = 0
          , peersToObjectsToReq = Map.empty
          }
        objectsToProvidersAndExpectedMultiplicities

    -- This function decides whether or not we should select a given peer as provider for the current objectId
    -- it takes into account if we are expecting to obtain the object from other sources (either inflight/owt pool already, or if the object will be requested from already selected peers in this given round)
    howToFoldProviders ::
      objectId ->
      ObjectMultiplicity ->
      DownloadPickState peerAddr objectId ->
      (peerAddr, NumObjectsReq) ->
      DownloadPickState peerAddr objectId
    howToFoldProviders objectId expectedMultiplicity st@DownloadPickState{totalNumObjectsToReq, objectMultiplicity, peersToObjectsToReq} (peerAddr, numObjectsInFlight) =
      let
        -- see what has already been attributed to this peer
        objectsToReq = Map.findWithDefault Set.empty peerAddr peersToObjectsToReq

        shouldSelect =
          -- We should not go over the multiplicity limit per object
          objectMultiplicity + expectedMultiplicity < dpTargetObjectRedundancy
            -- We should not go over the total number of objects inflight limit
            && totalNumObjectsInflight + totalNumObjectsToReq < dpMaxNumObjectsInflightTotal
            -- We should not go over the per-peer number of objects inflight limit
            && numObjectsInFlight + (fromIntegral $ Set.size objectsToReq) < dpMaxNumObjectsInflightPerPeer
       in
        if shouldSelect
          then
            -- We increase both global count and per-object count, and we add the object to the peer's set
            DownloadPickState
              { totalNumObjectsToReq = totalNumObjectsToReq + 1
              , objectMultiplicity = objectMultiplicity + 1
              , peersToObjectsToReq = Map.insert peerAddr (Set.insert objectId objectsToReq) peersToObjectsToReq
              }
          -- Or we keep the state as is if we don't select this peer
          else st
