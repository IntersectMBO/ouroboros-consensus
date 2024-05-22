{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.PointSchedule.Shrinking (
    -- | Exported only for testing (that is, checking the properties of the function)
    shrinkByRemovingAdversaries
  , shrinkHonestPeer
  , shrinkHonestPeers
  , shrinkPeerSchedules
  ) where

import           Control.Monad.Class.MonadTime.SI (DiffTime, Time, addTime,
                     diffTime)
import           Data.Containers.ListUtils (nubOrd)
import           Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     AnchoredSeq (Empty), takeWhileOldest)
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..),
                     addBranch', mkTrunk)
import           Test.Consensus.PeerSimulator.StateView (StateView)
import           Test.Consensus.PointSchedule (GenesisTest (..),
                     GenesisTestFull, PeerSchedule, PeersSchedule,
                     peerSchedulesBlocks)
import           Test.Consensus.PointSchedule.Peers (Peers (..))
import           Test.Consensus.PointSchedule.SinglePeer (SchedulePoint (..))
import           Test.QuickCheck (shrinkList)
import           Test.Util.TestBlock (TestBlock, isAncestorOf,
                     isStrictAncestorOf)

-- | Shrink a 'Peers PeerSchedule'. This does not affect the honest peer; it
-- does, however, attempt to remove other peers or ticks of other peers. The
-- block tree is trimmed to keep only parts that are necessary for the shrunk
-- schedule.
shrinkPeerSchedules ::
  GenesisTestFull TestBlock ->
  StateView TestBlock ->
  [GenesisTestFull TestBlock]
shrinkPeerSchedules genesisTest _stateView =
  let trimmedBlockTree sch = trimBlockTree' sch (gtBlockTree genesisTest)
      shrunkAdversarialPeers =
        shrinkAdversarialPeers shrinkAdversarialPeer (gtSchedule genesisTest)
          <&> \shrunkSchedule ->
            genesisTest
              { gtSchedule = shrunkSchedule,
                gtBlockTree = trimmedBlockTree shrunkSchedule
              }
      shrunkHonestPeers =
        shrinkHonestPeers
          (gtSchedule genesisTest)
          -- No need to update the tree here, shrinking the honest peers never discards blocks
          <&> \shrunkSchedule -> genesisTest {gtSchedule = shrunkSchedule}
   in shrunkAdversarialPeers ++ shrunkHonestPeers

-- | Shrink a 'Peers PeerSchedule' by removing adversaries. This does not affect
-- the honest peer; and it does not remove ticks from the schedules of the
-- remaining adversaries.
shrinkByRemovingAdversaries ::
  GenesisTestFull TestBlock ->
  StateView TestBlock ->
  [GenesisTestFull TestBlock]
shrinkByRemovingAdversaries genesisTest _stateView =
  shrinkAdversarialPeers (const []) (gtSchedule genesisTest) <&> \shrunkSchedule ->
    let trimmedBlockTree = trimBlockTree' shrunkSchedule (gtBlockTree genesisTest)
     in (genesisTest{gtSchedule = shrunkSchedule, gtBlockTree = trimmedBlockTree})

-- | Shrink a 'PeerSchedule' by removing ticks from it. The other ticks are kept
-- unchanged.
shrinkAdversarialPeer :: (PeerSchedule blk) -> [PeerSchedule blk]
shrinkAdversarialPeer = shrinkList (const [])

-- | Shrink the 'others' field of a 'Peers' structure by attempting to remove
-- peers or by shrinking their values using the given shrinking function.
shrinkAdversarialPeers :: (a -> [a]) -> Peers a -> [Peers a]
shrinkAdversarialPeers shrink Peers {honestPeers, adversarialPeers} =
  map (Peers honestPeers . Map.fromList) $
    shrinkList (traverse shrink) $
      Map.toList adversarialPeers

-- | Shrinks honest peers by removing ticks. Because we are manipulating
-- 'PeerSchedule' at this point, there is no proper notion of a tick. Instead,
-- we remove points from the honest 'PeerSchedule', and move all other points sooner.
--
-- We check that this operation does not changes the final state of the honest peer,
-- that is, it keeps the same final tip point, header point, and block point.
--
-- NOTE: This operation makes the honest peer to end its schedule sooner, which *may*
-- trigger disconnections when the timeout for MsgAwaitReply is reached. In those cases,
-- it is probably more pertinent to disable this timeout in tests than to disable shrinking.
shrinkHonestPeers :: Peers (PeerSchedule blk) -> [Peers (PeerSchedule blk)]
shrinkHonestPeers Peers {honestPeers, adversarialPeers} = do
  (k, honestSch) <- Map.toList honestPeers
  let (lastHonest, _) = last honestSch
  shrunk <- shrinkHonestPeer honestSch
  pure $ Peers
    { honestPeers = Map.insert k shrunk honestPeers
    , adversarialPeers = fmap (extendAdversary lastHonest) adversarialPeers
    }
  where
    -- Add an extra point at the end of the adversarial schedule if the honest one
    -- was longer than it. Preserves the total duration of the simulation, so that
    -- timeouts/LoP disconnections can still happen.
    extendAdversary tLast = \case
      [] -> []
      ps -> case last ps of
        (t, p) | t < tLast -> ps ++ [(tLast, p)]
        _                  -> ps

shrinkHonestPeer :: PeerSchedule blk -> [PeerSchedule blk]
shrinkHonestPeer sch =
  mapMaybe (speedUpTheSchedule sch) splits
  where
    -- | A list of non-zero time intervals between successive points of the honest schedule
    splits :: [(Time, DiffTime)]
    splits = mapMaybe
      (\((t1, _), (t2, _)) ->
        if t1 == t2
          then Nothing
          else Just (t1, diffTime t2 t1)
      )
      (zip sch (drop 1 sch))

-- | Speeds up _the_ schedule (that is, the one that we are actually trying to
-- speed up) after `at` time, by `speedUpBy`. This "speeding up" is done by
-- removing `speedUpBy` to all points after `at`, and removing those points if
-- they fall before `at`. We check that the operation doesn't change the final
-- state of the peer, i.e. it doesn't remove all TP, HP, and BP in the sped up
-- part.
speedUpTheSchedule :: PeerSchedule blk -> (Time, DiffTime) -> Maybe (PeerSchedule blk)
speedUpTheSchedule sch (at, speedUpBy) =
  if stillValid then Just $ beforeSplit ++ spedUpSchedule else Nothing
  where
    (beforeSplit, afterSplit) = span ((< at) . fst) sch
    threshold = addTime speedUpBy at
    spedUpSchedule = mapMaybe
      (\(t, p) -> if t < threshold then Nothing else Just (addTime (-speedUpBy) t, p))
      afterSplit
    stillValid =
         (hasTP spedUpSchedule == hasTP afterSplit)
      && (hasHP spedUpSchedule == hasHP afterSplit)
      && (hasBP spedUpSchedule == hasBP afterSplit)
    hasTP = any (\case (_, ScheduleTipPoint    _) -> True; _ -> False)
    hasHP = any (\case (_, ScheduleHeaderPoint _) -> True; _ -> False)
    hasBP = any (\case (_, ScheduleBlockPoint  _) -> True; _ -> False)

-- | Remove blocks from the given block tree that are not necessary for the
-- given peer schedules. If entire branches are unused, they are removed. If the
-- trunk is unused, then it remains as an empty anchored fragment.
trimBlockTree' :: PeersSchedule TestBlock -> BlockTree TestBlock -> BlockTree TestBlock
trimBlockTree' = keepOnlyAncestorsOf . peerSchedulesBlocks

-- | Given some blocks and a block tree, keep only the prefix of the block tree
-- that contains ancestors of the given blocks.
keepOnlyAncestorsOf :: [TestBlock] -> BlockTree TestBlock -> BlockTree TestBlock
keepOnlyAncestorsOf blocks bt =
    let leaves = blocksWithoutDescendents blocks
        trunk = keepOnlyAncestorsOf' leaves (btTrunk bt)
        branches = mapMaybe (fragmentToMaybe . keepOnlyAncestorsOf' leaves . btbSuffix) (btBranches bt)
     in foldr addBranch' (mkTrunk trunk) branches
  where
    fragmentToMaybe (Empty _) = Nothing
    fragmentToMaybe fragment  = Just fragment

    -- | Given some blocks and a fragment, keep only the prefix of the fragment
    -- that contains ancestors of the given blocks.
    keepOnlyAncestorsOf' :: [TestBlock] -> AnchoredFragment TestBlock -> AnchoredFragment TestBlock
    keepOnlyAncestorsOf' leaves = takeWhileOldest (\block -> (block `isAncestorOf`) `any` leaves)

    -- | Return a subset of the given blocks containing only the ones that do
    -- not have any other descendents in the set.
    blocksWithoutDescendents :: [TestBlock] -> [TestBlock]
    blocksWithoutDescendents bs =
      let bs' = nubOrd bs
       in [ b | b <- bs', not ((b `isStrictAncestorOf`) `any` bs') ]
