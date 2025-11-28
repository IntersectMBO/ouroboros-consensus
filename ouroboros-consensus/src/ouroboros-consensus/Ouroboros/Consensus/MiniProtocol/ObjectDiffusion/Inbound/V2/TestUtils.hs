{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.TestUtils
  ( chooseGeometricWithMedian
  , partitionWithProb
  , genDecisionPolicy
  , genDecisionContext
  , genPeerStates
  , genPeerState
  , defaultDecisionPolicy
  )
where

import Data.Either (partitionEithers)
import qualified Data.Foldable as Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import Data.Traversable (for)
import Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.Inbound.V2.Types
  ( DecisionContext (..)
  , DecisionPolicy (..)
  , PeerDecisionStatus (PeerDecisionCompleted)
  , PeerState (..)
  )
import Ouroboros.Network.Protocol.ObjectDiffusion.Type (NumObjectsReq (..))
import System.Random (mkStdGen)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, choose, shuffle, vector)

-- | Partition a list into two lists according to a given probability, in the
-- QuickCheck 'Gen' monad.
-- Each element has probability 'p' of going into the first list,
-- and probability '1 - p' of going into the second list.
partitionWithProb ::
  -- | Probability 'p' of going into the first list
  Double ->
  -- | Input list
  [a] ->
  Gen ([a], [a])
partitionWithProb p xs = do
  partitionEithers
    <$> traverse
      ( \x -> do
          r <- choose (0.0, 1.0)
          if r < p then return (Left x) else return (Right x)
      )
      xs

-- TODO: this needs to be tested and inspected

-- | Geometric-decay generator over [1 .. maxBound - 1] for the type 'a'.
-- Smaller values are more likely; the (lower) median is ~ medianTarget.
-- Works for any Integral + Bounded numeric type (e.g. Int, Word32, Int64
-- and newtypes around those).
chooseGeometricWithMedian :: forall a. (Integral a, Bounded a) => a -> Gen a
chooseGeometricWithMedian medianTarget
  | (maxBound @a) <= 1 =
      error "Type's maxBound <= 1: no room for [1..maxBound-1]"
  | medianTarget < 1 || medianTarget >= maxBound =
      error "medianTarget must be in [1 .. maxBound-1]"
  | otherwise = do
      let lo = 1
          hi = maxBound - 1
          -- use Integer for counts, Double for CDF inversion
          nI = toInteger (hi - lo + 1)
          mI = toInteger (medianTarget - lo + 1)
          n = fromIntegral nI :: Double
          m = fromIntegral mI :: Double
          p = 1 - 2 ** (-1 / m) -- set so P(X ≤ median) ≈ 0.5
          q = 1 - p -- decay factor
          qn = q ** n -- truncation term
      u <- choose (0, 1 :: Double)
      let y = 1 - u * (1 - qn)
          k = floor (log y / log q) -- inverse truncated geometric CDF
          k' = max 0 (min (floor (n - 1)) k)
      pure (lo + fromInteger k')

-- | Default decision policy suitable for tests. It is in-between the expected
-- values for cert diffusion and these for vote diffusion.
defaultDecisionPolicy :: DecisionPolicy
defaultDecisionPolicy =
  DecisionPolicy
    { dpMaxNumObjectIdsReq = 10
    , dpMaxNumObjectsOutstanding = 100
    , dpMaxNumObjectsInflightPerPeer = 50
    , dpMaxNumObjectsInflightTotal = 500
    , dpTargetObjectRedundancy = 3
    , dpDecisionThreadSleepDelay = 0.005
    }

-- | Generate a random 'DecisionPolicy' based on a geometric variation using the
-- values of 'defaultDecisionPolicy' as median targets.
genDecisionPolicy :: Gen DecisionPolicy
genDecisionPolicy =
  let DecisionPolicy
        { dpMaxNumObjectIdsReq
        , dpMaxNumObjectsOutstanding
        , dpMaxNumObjectsInflightPerPeer
        , dpMaxNumObjectsInflightTotal
        , dpTargetObjectRedundancy
        } = defaultDecisionPolicy
   in DecisionPolicy
        <$> (chooseGeometricWithMedian dpMaxNumObjectIdsReq)
        <*> (chooseGeometricWithMedian dpMaxNumObjectsOutstanding)
        <*> (chooseGeometricWithMedian dpMaxNumObjectsInflightPerPeer)
        <*> (chooseGeometricWithMedian dpMaxNumObjectsInflightTotal)
        <*> (chooseGeometricWithMedian dpTargetObjectRedundancy)
        <*> pure 0.005

-- TODO: in all functions below, we may want to use random ratios centered on a
-- given median instead of fixed ratios, to have more variability in the
-- generated states.

-- | Generate a random 'DecisionContext' suitable for tests.
--
-- The number of desired peers and existing objects must be specified.
-- It will take the whole pool of existing objects as a starting point, and
-- randomly attribute a subset of them to each peer.
-- Then for each peer the `genPeerState` function will randomly
-- decide which objects are available, inflight, in the OWT pool, etc.
--
-- NOTE: given that the generated decision context is meant for benchs
-- primarily, all peers receive an 'PeerDecisionCompleted' as their previous
-- so that they are all selected for the next round of decisions.
--
-- We also try to uphold as much realism as possible, e.g. we try to enforce
-- the invariants that must be held in the peer states and global state,
-- as well as respecting the decision policy limits.
-- The only policy limit we may not respect is `dpTargetObjectRedundancy`, as
-- we may have more copies inflight of a given object than the target redundancy
-- specified in the policy, but it isn't an invariant like the others, it's
-- just a target to aim for.
genDecisionContext ::
  forall peerAddr objectId object.
  ( Arbitrary peerAddr
  , Arbitrary object
  , Ord peerAddr
  , Ord objectId
  , Ord object
  ) =>
  -- | Number of concurrent connections to outbound peers
  Int ->
  -- | Number of total distinct objects that exist at this given time
  Int ->
  -- | How to get the id out of an object
  (object -> objectId) ->
  -- | If we want to provide a specific decision policy instead of relying on
  --   an arbitrary variation of the default one
  Maybe DecisionPolicy ->
  Gen (DecisionContext peerAddr objectId object)
genDecisionContext peersNb objectsNb getId mPolicy = do
  -- Ratio of objects (compared to all existing objects) that will be
  -- considered already in the ObjectPool
  let alreadyInPoolRatio :: Double = 0.2

  dcRng <- mkStdGen <$> arbitrary

  -- Either use the provided policy, or generate a new one using
  -- `genDecisionPolicy`
  dcDecisionPolicy <- fromMaybe genDecisionPolicy (pure <$> mPolicy)

  -- Generate the pool of all existing objects
  objects <- vector objectsNb

  -- Mark a subset of them as already in the ObjectPool
  -- We try to make the `dcHasObject` function as efficient as possible
  (alreadyInPool, _) <- partitionWithProb alreadyInPoolRatio objects
  let !alreadyInPoolIds = Set.fromList $ getId <$> alreadyInPool
      dcHasObject = (`Set.member` alreadyInPoolIds)

  -- Delegate generation of the state to `genPeerStates` (and beneath
  -- that `genPeerState`)
  dcPeerStates <- genPeerStates getId dcDecisionPolicy peersNb objects

  -- Use `PeerDecisionCompleted` as the previous decision for all peers so that
  -- they are all selected in the next round of decisions.
  let dcPrevDecisions =
        Map.map (\_ -> PeerDecisionCompleted) (dcPeerStates)

  pure $
    DecisionContext
      { dcRng
      , dcHasObject
      , dcDecisionPolicy
      , dcPeerStates
      , dcPrevDecisions
      }

-- | Generate a random 'PeerStates' suitable for benchs,
-- based on a given decision policy and a pool of existing objects.
genPeerStates ::
  (Arbitrary peerAddr, Ord peerAddr, Ord objectId, Ord object) =>
  (object -> objectId) ->
  -- | Decision policy to respect when generating the global state.
  DecisionPolicy ->
  -- | Number of concurrent connections to outbound peers
  Int ->
  -- | Pool of all existing objects to choose from
  [object] ->
  Gen (Map peerAddr (PeerState objectId object))
genPeerStates
  getId
  policy@DecisionPolicy{dpMaxNumObjectsInflightTotal}
  peersNb
  objects =
    do
      let objectRepresentationRatio :: Double = 0.6

      peerPairs <- for [1 .. peersNb] \_ -> do
        (peerObjects, _) <-
          partitionWithProb
            objectRepresentationRatio
            objects

        peerAddr <- arbitrary
        peerState <- genPeerState getId policy peerObjects
        pure (peerAddr, peerState)

      -- Now we need to make sure that we are not over the limit of
      -- `dpMaxNumObjectsInflightTotal` across all peers.
      -- It's a bit ugly, because we can't really control that in the individual
      -- generation of each peer state, as it depends on the values of
      -- `dpMaxNumObjectsInflightPerPeer`, `dpMaxNumObjectsInflightTotal`,
      -- `peersNb`, and the size of the pool of existing objects we use
      -- in the generation of the state as well as various ratios which are
      -- implementation details.
      let numObjectsInflightTotal =
            Foldable.foldl'
              ( \count (_, PeerState{psObjectsInflightIds}) ->
                  count + (fromIntegral $ Set.size psObjectsInflightIds)
              )
              (NumObjectsReq 0)
              peerPairs
          trimRatio :: Double
          trimRatio =
            ( fromIntegral numObjectsInflightTotal
                / fromIntegral dpMaxNumObjectsInflightTotal
            )
              - 1
      peerPairs' <-
        if trimRatio > 0
          then traverse (trimObjectsInflightIds trimRatio) peerPairs
          else pure peerPairs

      pure $ Map.fromList peerPairs'

-- | Remove a given ratio of inflight ids from a peer state in a most equitable
-- fashion. It also removes them from the FIFO.
trimObjectsInflightIds ::
  Ord objectId =>
  -- | Ratio of inflight ids to remove (between 0 and 1)
  Double ->
  -- | Peer address and its peer state
  (peerAddr, PeerState objectId object) ->
  Gen (peerAddr, PeerState objectId object)
trimObjectsInflightIds
  trimRatio
  ( peerAddr
    , peerState@PeerState{psObjectsInflightIds, psOutstandingFifo}
    ) =
    do
      -- We take the ceiling of the number to remove to make sure we go under the limit
      let nbToRemove = ceiling $ fromIntegral (Set.size psObjectsInflightIds) * trimRatio
      -- Then pick randomly this number of ids to remove
      idsToRemove <- take nbToRemove <$> shuffle (Set.toList psObjectsInflightIds)
      -- Then we remove them from both the inflight set and the outstanding FIFO
      let psObjectsInflightIds' =
            Foldable.foldl'
              (flip Set.delete)
              psObjectsInflightIds
              idsToRemove

          -- Should we actually do that?
          psOutstandingFifo' =
            StrictSeq.filter
              (\objId -> not (objId `elem` idsToRemove))
              psOutstandingFifo
      pure
        ( peerAddr
        , peerState
            { psObjectsInflightIds = psObjectsInflightIds'
            , psOutstandingFifo = psOutstandingFifo'
            }
        )

-- | Generate a random 'PeerState' suitable for benchs,
-- based on a given decision policy and a pool of existing objects that this
-- peer can use in its state.
genPeerState ::
  (Ord objectId, Ord object) =>
  (object -> objectId) ->
  -- | Decision policy to respect when generating the peer state.
  DecisionPolicy ->
  -- | Pool of existing objects this peer should use for its state
  [object] ->
  Gen (PeerState objectId object)
genPeerState
  getId
  DecisionPolicy{dpMaxNumObjectsOutstanding, dpMaxNumObjectsInflightPerPeer}
  peerObjects =
    do
      let inflightRatio :: Double = 0.1
      let owtPoolRatio :: Double = 0.1
      -- let availableRatio :: Double = 1 - (inflightRatio + owtPoolRatio)
      let owtPoolStillInFifoRatio :: Double = 0.3

      -- First we divide the existing objects into three groups that must be disjoint:
      -- objects available, inflight, and owt pool, according to the specified ratios.
      (objectsAvailable, rest) <-
        partitionWithProb (1 - (inflightRatio + owtPoolRatio)) peerObjects
      (objectsInflight, objectsOwtPool) <-
        partitionWithProb (owtPoolRatio / (owtPoolRatio + inflightRatio)) rest
      -- For objects inflight we limit their number according to the decision policy
      -- (we dont' need to do that for the other two categories)
      let objectsInflight' =
            take (fromIntegral dpMaxNumObjectsInflightPerPeer) objectsInflight

      -- Now we decide of a subset of objects owt pool that haven't been acked yet,
      -- so must still be in the FIFO.
      (owtPoolStillInFifo, _) <-
        partitionWithProb owtPoolStillInFifoRatio objectsOwtPool

      -- We can now reconstruct the FIFO, by taking all available and inflight objects
      -- (as they must be in the FIFO according to the state invariants), and
      -- and topping up with the subset of objects owt pool that we defined above.
      -- We also shuffle the FIFO and limit its size according to the decision policy.
      objectsInFifo <-
        take (fromIntegral dpMaxNumObjectsOutstanding)
          <$> (shuffle $ objectsAvailable ++ objectsInflight ++ owtPoolStillInFifo)

      -- Retropendingly, to uphold the invariants, we must remove elements from
      -- the available and inflight lists that are not in the FIFO.
      let objectsAvailable' =
            filter (\obj -> obj `elem` objectsInFifo) objectsAvailable
          objectsInflight'' =
            filter (\obj -> obj `elem` objectsInFifo) objectsInflight'

      -- Finally, we decide how many ids are inflight
      -- We can't have more than the difference between the policy limit for the FIFO
      -- and the current size of the FIFO.
      let maxNumIdsInFlight =
            fromIntegral dpMaxNumObjectsOutstanding - length objectsInFifo
      numIdsInFlight <- choose (0, maxNumIdsInFlight)

      pure $
        PeerState
          { psObjectsAvailableIds = Set.fromList $ getId <$> objectsAvailable'
          , psObjectsInflightIds = Set.fromList $ getId <$> objectsInflight''
          , psObjectsOwtPool = Map.fromList $ (\obj -> (getId obj, obj)) <$> objectsOwtPool
          , psOutstandingFifo = StrictSeq.fromList $ getId <$> objectsInFifo
          , psNumIdsInflight = fromIntegral $ numIdsInFlight
          }
