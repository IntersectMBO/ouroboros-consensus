{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.PointSchedule.Shrinking (
    -- | Exported only for testing (that is, checking the properties of the function)
    shrinkByRemovingAdversaries
  , shrinkHonestPeer
  , shrinkPeerSchedules
  ) where

import           Control.Monad.Class.MonadTime.SI (DiffTime, Time, addTime,
                     diffTime)
import           Data.Containers.ListUtils (nubOrd)
import           Data.Function ((&))
import           Data.Functor ((<&>))
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe, maybeToList)
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
import           Test.Util.PartialAccessors
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
        shrinkAdversarialPeers shrinkPeerSchedule (gtSchedule genesisTest)
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
shrinkPeerSchedule :: (PeerSchedule blk) -> [PeerSchedule blk]
shrinkPeerSchedule = shrinkList (const [])

-- | Shrink the 'others' field of a 'Peers' structure by attempting to remove
-- peers or by shrinking their values using the given shrinking function.
shrinkAdversarialPeers :: (a -> [a]) -> Peers a -> [Peers a]
shrinkAdversarialPeers shrink Peers {honestPeers, adversarialPeers} =
  map (Peers honestPeers . Map.fromList) $
    shrinkList (traverse shrink) $
      Map.toList adversarialPeers

-- | Shrinks honest peers by removing ticks. Because we are manipulating
-- 'PeerSchedule' at this point, there is no proper notion of a tick. Instead,
-- we remove points from the honest 'PeerSchedule', and move all other points
-- sooner, including those on the other schedules. We check that this operation
-- neither changes the final state of the honest peer, nor removes points from
-- the other schedules.
shrinkHonestPeers :: Peers (PeerSchedule blk) -> [Peers (PeerSchedule blk)]
shrinkHonestPeers Peers {honestPeers, adversarialPeers} =
  Map.toList honestPeers
    & concatMap
      ( \(n, schedule) ->
          shrinkTheHonestPeer schedule (Map.delete n honestPeers) adversarialPeers
            & map
              ( \(schedule', otherHonestPeers', otherAdversarialPeers') ->
                  Peers
                    { honestPeers = Map.insert n schedule' otherHonestPeers',
                      adversarialPeers = otherAdversarialPeers'
                    }
              )
      )

shrinkTheHonestPeer ::
  PeerSchedule blk ->
  Map Int (PeerSchedule blk) ->
  Map Int (PeerSchedule blk) ->
  [(PeerSchedule blk, Map Int (PeerSchedule blk), Map Int (PeerSchedule blk))]
shrinkTheHonestPeer theSchedule otherHonestPeers otherAdversarialPeers = do
  (at, speedUpBy) <- splits
  maybeToList $ do
    theSchedule' <- speedUpTheSchedule at speedUpBy theSchedule
    otherHonestPeers' <- mapM (speedUpOtherSchedule at speedUpBy) otherHonestPeers
    otherAdversarialPeers' <- mapM (speedUpOtherSchedule at speedUpBy) otherAdversarialPeers
    pure (theSchedule', otherHonestPeers', otherAdversarialPeers')
  where
    -- | A list of non-zero time intervals between successive points of the honest schedule
    splits :: [(Time, DiffTime)]
    splits = mapMaybe
      (\((t1, _), (t2, _)) ->
        if t1 == t2
          then Nothing
          else Just (t1, diffTime t2 t1)
      )
      (zip theSchedule (drop 1 theSchedule))

-- | For testing purposes only. Assumes there is exactly one honest peer and
-- shrinks it.
shrinkHonestPeer :: PeersSchedule blk -> [PeersSchedule blk]
shrinkHonestPeer Peers {honestPeers, adversarialPeers} =
  shrinkTheHonestPeer (getHonestPeer honestPeers) Map.empty adversarialPeers
    & map
      ( \(schedule', _, otherAdversarialPeers') ->
          Peers
            { honestPeers = Map.singleton 1 schedule',
              adversarialPeers = otherAdversarialPeers'
            }
      )

-- | Speeds up _the_ schedule (that is, the one that we are actually trying to
-- speed up) after `at` time, by `speedUpBy`. This "speeding up" is done by
-- removing `speedUpBy` to all points after `at`, and removing those points if
-- they fall before `at`. We check that the operation doesn't change the final
-- state of the peer, i.e. it doesn't remove all TP, HP, and BP in the sped up
-- part.
speedUpTheSchedule :: Time -> DiffTime -> PeerSchedule blk -> Maybe (PeerSchedule blk)
speedUpTheSchedule at speedUpBy sch =
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

-- | Speeds up the other schedules after `at` time, by `speedUpBy`. This
-- "speeding up" is done by removing `speedUpBy` to all points after `at`. We
-- check that the schedule had no points between `at` and `at + speedUpBy`. We
-- also keep the last point where it is, so that the end time stays the same.
speedUpOtherSchedule :: Time -> DiffTime -> PeerSchedule blk -> Maybe (PeerSchedule blk)
speedUpOtherSchedule at speedUpBy sch =
  if losesPoint then Nothing else Just $ beforeSplit ++ spedUpSchedule ++ lastPoint
  where
    (beforeSplit, afterSplit) = span ((< at) . fst) sch
    spedUpSchedule = map (\(t, p) -> (addTime (-speedUpBy) t, p)) $ take (length afterSplit - 1) afterSplit
    losesPoint = any ((< (addTime speedUpBy at)) . fst) afterSplit
    lastPoint = case afterSplit of
      [] -> []
      as -> [last as]

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
