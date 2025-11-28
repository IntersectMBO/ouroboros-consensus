{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Decision (makeDecisions) where

import Control.Exception (assert)
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
import System.Random.Shuffle (shuffle')

strictSeqToSet :: Ord a => StrictSeq a -> Set a
strictSeqToSet = Set.fromList . Foldable.toList

newtype Pending a = Pending a
newtype Busy a = Busy a

splitPeerStates ::
  Ord peerAddr =>
  Map peerAddr (PeerState objectId object) ->
  Map peerAddr (PeerDecisionStatus objectId object) ->
  ( Pending (Map peerAddr (PeerState objectId object))
  , Busy (Map peerAddr (PeerState objectId object, Set objectId))
  )
splitPeerStates peerStates peerDecisions =
  let peerStatesAndDecisions = Map.intersectionWith (,) peerStates peerDecisions
      (pending, busy) =
        Map.mapEither
          ( \(state, status) ->
              case status of
                PeerDecisionBeingActedUpon decision ->
                  Right (state, rodObjectsToReqIds (pdReqObjects decision))
                _ -> Left state
          )
          peerStatesAndDecisions
   in (Pending pending, Busy busy)

-- | Make download decisions.
makeDecisions ::
  forall peerAddr objectId object.
  ( Ord peerAddr
  , Ord objectId
  ) =>
  DecisionContext peerAddr objectId object ->
  -- | New decisions
  Map peerAddr (PeerDecision objectId object)
makeDecisions
  DecisionContext
    { dcRng
    , dcHasObject
    , dcDecisionPolicy
    , dcPeerStates
    , dcPrevDecisions
    } =
    let
      (pendingPeersStates, busyPeerStatesAndObjectToReqIds) =
        splitPeerStates dcPeerStates dcPrevDecisions
      -- We do it in two steps, because computing the acknowledgment tell which
      -- objects from psObjectsAvailableIds sets of each peer won't actually be
      -- available anymore (as soon as we ack them), so that the makeReqObjectsDecisions
      -- function can take this into account.
      (reqIdsDecisions, peerToIdsToAck) =
        makeReqIdsAndAckDecisions dcHasObject dcDecisionPolicy pendingPeersStates
      reqObjectsDecisions =
        makeReqObjectsDecisions
          dcRng
          dcHasObject
          dcDecisionPolicy
          pendingPeersStates
          busyPeerStatesAndObjectToReqIds
          peerToIdsToAck
     in
      Map.intersectionWith
        PeerDecision
        reqObjectsDecisions
        reqIdsDecisions

-- | The ids to ack are the longest prefix of outstandingFifo of each peer that
-- match the following criteria:
-- * either the object is owt pool for the peer who has downloaded it
-- * or the object is already in pool
makeReqIdsAndAckDecisions ::
  forall peerAddr objectId object.
  Ord objectId =>
  (objectId -> Bool) ->
  DecisionPolicy ->
  Pending (Map peerAddr (PeerState objectId object)) ->
  ( Map peerAddr (ReqIdsDecision objectId object)
  , Map peerAddr (Set objectId)
  )
makeReqIdsAndAckDecisions
  poolHasObject
  DecisionPolicy{dpMaxNumObjectIdsReq, dpMaxNumObjectsOutstanding}
  (Pending pendingPeerStates) =
    let
      reqIdsAndAckDecisions = makeReqIdsAndAckDecision <$> pendingPeerStates
     in
      ( fst <$> reqIdsAndAckDecisions
      , snd <$> reqIdsAndAckDecisions
      )
   where
    makeReqIdsAndAckDecision ::
      PeerState objectId object ->
      (ReqIdsDecision objectId object, Set objectId)
    makeReqIdsAndAckDecision
      PeerState
        { psOutstandingFifo
        , psObjectsOwtPool
        , psNumIdsInflight
        } =
        let
          -- we isolate the longest prefix of outstandingFifo that matches our ack
          -- criteria (see above in computeAck doc)
          (idsToAck, psOutstandingFifo') =
            StrictSeq.spanl
              (\objectId -> poolHasObject objectId || objectId `Map.member` psObjectsOwtPool)
              psOutstandingFifo

          futureFifoSizeOnOutboundPeer :: NumObjectIdsReq =
            -- the new known fifo state after we ack the idsToAck
            (fromIntegral $ StrictSeq.length psOutstandingFifo')
              -- plus the number of ids that we have already requested but we didn't
              -- receive yet that the outbound peer might consequently already have
              -- added to its fifo
              + psNumIdsInflight

          ridNumIdsToReq =
            (fromIntegral dpMaxNumObjectsOutstanding - futureFifoSizeOnOutboundPeer)
              `min` dpMaxNumObjectIdsReq

          ridNumIdsToAck =
            -- in the case where ridNumIdsToReq == 0, we know we actually won't be
            -- able to ack anything because we won't emit a request, and acknowledgment
            -- is done at the same time as the request for ids
            let numIdsToAck = fromIntegral $ StrictSeq.length idsToAck
             in assert (not (ridNumIdsToReq == 0 && numIdsToAck > 0)) $
                  numIdsToAck

          ridCanPipelineIdsRequests = not . StrictSeq.null $ psOutstandingFifo'

          reqIdsDecision =
            ReqIdsDecision
              { ridNumIdsToAck
              , ridNumIdsToReq
              , ridCanPipelineIdsRequests
              }
         in
          (reqIdsDecision, strictSeqToSet idsToAck)

-- | Order peers randomly based on the provided RNG.
-- We do that to avoid biasing the decision logic towards the peers that
-- would happen to be first in the map default ordering, as that could
-- be exploited by an adversary.
orderPeers ::
  StdGen ->
  Map peerAddr (PeerState objectId object) ->
  [(peerAddr, PeerState objectId object)]
orderPeers rng peerStates =
  let peerPairs = Map.toList peerStates
   in shuffle' peerPairs (length peerPairs) rng

data DownloadPickState peerAddr objectId
  = DownloadPickState
  { totalNumObjectsToReq :: !NumObjectsReq
  , objectMultiplicity :: ObjectMultiplicity
  , reqObjectsDecisions :: Map peerAddr (Set objectId)
  }

makeReqObjectsDecisions ::
  forall peerAddr objectId object.
  ( Ord peerAddr
  , Ord objectId
  ) =>
  StdGen ->
  (objectId -> Bool) ->
  DecisionPolicy ->
  Pending (Map peerAddr (PeerState objectId object)) ->
  -- | Busy peers and the set of ids of objects they are currently requesting
  Busy (Map peerAddr (PeerState objectId object, Set objectId)) ->
  -- | map from peer to the set of ids that will be acked for that peer on next
  -- requestIds. We should treat these ids as not available anymore for the
  -- purpose of picking objects to request
  Map peerAddr (Set objectId) ->
  Map peerAddr (ReqObjectsDecision objectId object)
makeReqObjectsDecisions
  rng
  poolHasObject
  DecisionPolicy
    { dpMaxNumObjectsInflightPerPeer
    , dpMaxNumObjectsInflightTotal
    , dpTargetObjectRedundancy
    }
  (Pending pendingPeerStates)
  (Busy busyPeerStatesAndObjectToReqIds)
  peerToIdsToAck =
    ReqObjectsDecision <$> decisions
   where
    -- We order the peers that are not currently executing a decision
    orderedPendingPeers = orderPeers rng pendingPeerStates

    -- We want to map each objectId to the sorted list of peers that can provide it
    -- For each peer we also indicate how many objects it has in flight at the moment
    -- We filter out here the objects that are already in pool.
    -- TODO: we may want to consider using a Seq instead of a list when we'll care
    -- about performance.
    objectsToSortedProviders :: Map objectId [(peerAddr, NumObjectsReq)]
    objectsToSortedProviders =
      -- We iterate over each peer and the corresponding available ids
      -- and turn the map "inside-out"
      Foldable.foldl'
        ( \accMap (peerAddr, PeerState{psObjectsAvailableIds, psObjectsInflightIds}) ->
            let
              -- ids that will be acked for this peer won't be available anymore,
              -- so we should not consider them in the decision logic
              --
              -- TODO: this is quite redundant, because ack can only be made when
              -- the object is already in the pool (in which case it would have
              -- been filtered out anyway in next step) or when the object is in
              -- psObjectsOwtPool of this peer (in which case it shouldn't be
              -- anymore in psObjectsAvailableIds)
              idsToAckForThisPeer =
                Map.findWithDefault
                  (error "invariant violated: peer must be in peerToIdsToAck map")
                  peerAddr
                  peerToIdsToAck
              -- we should also remove objects that are already in the pool
              interestingAndAvailableObjectIds =
                Set.filter (not . poolHasObject) $
                  psObjectsAvailableIds `Set.difference` idsToAckForThisPeer
             in
              -- we iterate over interestingAndAvailableObjectIds and add the
              -- peer to the list of providers for each object it can provide
              Foldable.foldl'
                ( \accMap' objectId ->
                    Map.insertWith
                      (++)
                      objectId
                      [(peerAddr, fromIntegral $ Set.size psObjectsInflightIds)]
                      accMap'
                )
                accMap
                interestingAndAvailableObjectIds
        )
        Map.empty
        orderedPendingPeers

    -- For busy peers, we should consider that the objects in pdObjectsToReqIds
    -- will be requested soon, so we should consider them as inflight for the
    -- purpose of picking objects to request for other peers
    objectsInFlightMultiplicitiesOfBusyPeers =
      Map.foldl'
        ( \accMap (PeerState{psObjectsInflightIds}, pdObjectsToReqIds) ->
            Foldable.foldl'
              (\accMap' objectId -> Map.insertWith (+) objectId 1 accMap')
              accMap
              (Set.union psObjectsInflightIds pdObjectsToReqIds)
        )
        Map.empty
        busyPeerStatesAndObjectToReqIds
    -- Finally, we add to the previous map the objects that are currently
    -- inflight from peers for which we will make a decision in this round
    objectsInFlightMultiplicities =
      Map.foldl'
        ( \accMap (PeerState{psObjectsInflightIds}) ->
            Foldable.foldl'
              (\accMap' objectId -> Map.insertWith (+) objectId 1 accMap')
              accMap
              psObjectsInflightIds
        )
        objectsInFlightMultiplicitiesOfBusyPeers
        pendingPeerStates

    totalNumObjectsInflight :: NumObjectsReq
    totalNumObjectsInflight = fromIntegral $ Map.foldl' (+) 0 objectsInFlightMultiplicities

    objectsOwtPoolMultiplicities =
      Map.foldl'
        ( \accMap (PeerState{psObjectsOwtPool}) ->
            Foldable.foldl'
              (\accMap' objectId -> Map.insertWith (+) objectId 1 accMap')
              accMap
              (Map.keys psObjectsOwtPool)
        )
        Map.empty
        (Map.union (fst <$> busyPeerStatesAndObjectToReqIds) pendingPeerStates)

    -- We also want to know for each objects how many peers have it in the
    -- inflight or owtPool, meaning that we should receive them soon.
    -- We should also add here the objects that are in the pdObjectsToReqIds of
    -- each peer decision for busy peers, if these ids are not already in
    -- dpsObjectsInflight or psObjectsOwtPool of this peer
    objectsExpectedSoonMultiplicities :: Map objectId ObjectMultiplicity
    objectsExpectedSoonMultiplicities =
      Map.unionWith
        (+)
        objectsInFlightMultiplicities
        objectsOwtPoolMultiplicities

    -- Now we join objectsToSortedProviders and objectsExpectedSoonMultiplicities
    -- maps on objectId for easy fold
    objectsToProvidersAndExpectedMultiplicities ::
      Map objectId ([(peerAddr, NumObjectsReq)], ObjectMultiplicity)
    objectsToProvidersAndExpectedMultiplicities =
      Map.merge
        -- if an objectId is missing from objectsExpectedSoonMultiplicities,
        -- then its expected multiplicity is 0
        (Map.mapMissing $ \_ providers -> (providers, 0))
        -- if an objectId is missing from objectsToSortedProviders, then we
        -- don't care about it
        Map.dropMissing
        -- Combine in a tuple the list of providers and the expected multiplicity
        ( Map.zipWithMatched $ \_ providers expectedMultiplicity ->
            (providers, expectedMultiplicity)
        )
        objectsToSortedProviders
        objectsExpectedSoonMultiplicities

    -- NOW HERE TAKE PLACE THE ACTUAL DECISION LOGIC AND ATTRIBUTION OF OBJECTS TO PEERS

    -- The current decision logic is greedy on objects, so it will try to request as
    -- many copies of the same object as possible, meaning we will have optimal
    -- coverage of the first objects, but might not request some other objects at
    -- all if they are (only) provided by peers that are already saturated.

    -- Now we compute the actual attribution of downloads for peers
    DownloadPickState{reqObjectsDecisions = decisions} =
      -- We iterate over each objectId and the corresponding (providers, expectedMult)
      Map.foldlWithKey'
        ( \st objectId (providers, expectedMultiplicity) ->
            -- reset the objectMultiplicity counter for each new objectId
            let st' = st{objectMultiplicity = 0}
             in -- We iterate over the list of providers, and pick them or not
                -- according to the current state.
                -- When a peer is selected as a provider for this objectId, we
                -- insert the objectId in the peer's set in reqObjectsDecisions
                -- (inside St), so the result of the filtering of providers is
                -- part of the final St state
                Foldable.foldl'
                  (conditionallyPickProviders objectId expectedMultiplicity)
                  st'
                  providers
        )
        DownloadPickState
          { totalNumObjectsToReq = 0
          , objectMultiplicity = 0
          , reqObjectsDecisions = Map.empty
          }
        objectsToProvidersAndExpectedMultiplicities

    -- This function decides whether or not we should select a given peer as
    -- provider for the current objectId. It takes into account if we are
    -- expecting to obtain the object from other sources (either inflight/owt
    -- pool already, or if the object will be requested from already selected
    -- peers in this given round)
    conditionallyPickProviders ::
      objectId ->
      ObjectMultiplicity ->
      DownloadPickState peerAddr objectId ->
      (peerAddr, NumObjectsReq) ->
      DownloadPickState peerAddr objectId
    conditionallyPickProviders
      objectId
      expectedMultiplicity
      st@DownloadPickState
        { totalNumObjectsToReq
        , objectMultiplicity
        , reqObjectsDecisions
        }
      (peerAddr, numObjectsInFlight) =
        let
          -- see what has already been attributed to this peer
          objectsToReq = Map.findWithDefault Set.empty peerAddr reqObjectsDecisions
          numObjectsToReq = fromIntegral (Set.size objectsToReq)

          shouldSelect =
            -- We should not go over the multiplicity limit per object
            objectMultiplicity + expectedMultiplicity < dpTargetObjectRedundancy
              -- We should not go over the total number of objects inflight limit
              && totalNumObjectsInflight + totalNumObjectsToReq < dpMaxNumObjectsInflightTotal
              -- We should not go over the per-peer number of objects inflight limit
              && numObjectsInFlight + numObjectsToReq < dpMaxNumObjectsInflightPerPeer
         in
          if shouldSelect
            then
              -- We increase both global count and per-object count, and we add
              -- the object to the peer's set
              DownloadPickState
                { totalNumObjectsToReq = totalNumObjectsToReq + 1
                , objectMultiplicity = objectMultiplicity + 1
                , reqObjectsDecisions =
                    Map.insert
                      peerAddr
                      (Set.insert objectId objectsToReq)
                      reqObjectsDecisions
                }
            -- Or we keep the state as is if we don't select this peer
            else st
