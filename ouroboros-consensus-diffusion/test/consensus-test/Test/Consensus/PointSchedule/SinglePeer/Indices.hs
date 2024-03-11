{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Schedule generators for a single peer
--
-- These functions generate schedules for the tip points, the header points and
-- the block points of a single peer. All of them are expressed in terms of
-- block indices rather than actual block points.
--
-- The tip points are to be generated first with either of 'singleJumpTipPoints'
-- or 'rollbacksTipPoints'. Then the tip points can be assigned times at which
-- to announce them with 'tipPointSchedule'. Then, the header points can be
-- generated with 'headerPointSchedule'. Finally, the block points can be
-- generated with 'headerPointSchedule' as well. See the implementation of
-- 'Test.Consensus.PointSchedule.Random.singleJumpPeerSchedule' for an example.
--
module Test.Consensus.PointSchedule.SinglePeer.Indices (
    HeaderPointSchedule (..)
  , headerPointSchedule
  , rollbacksTipPoints
  , singleJumpTipPoints
  , tipPointSchedule
  ) where

import           Control.Monad (forM, replicateM)
import           Data.List (sort)
import           Data.Time.Clock (DiffTime, diffTimeToPicoseconds,
                     picosecondsToDiffTime)
import           GHC.Stack (HasCallStack)
import           Ouroboros.Network.Block (SlotNo (SlotNo))
import qualified System.Random.Stateful as R


-- | @singleJumpTipPoints g m n@ generates a list of tip points for a single peer
-- serving a single branch between block indices @m@ and @n@. The schedule is a
-- list of block indices for the tip point of the peer state.
--
-- The first tip jumps to a block in the middle of the index range, and
-- then updates the tip one block at a time.
--
-- > singleJumpTipPoints
-- >   :: g
-- >   -> {m:Int | m >= 0}
-- >   -> {n:Int | n >= 0}
-- >   -> m {v:[Int]
-- >        | isSorted v &&
-- >          all (m<=) v &&
-- >          all (<=n) v &&
-- >          not (hasDuplicates v)
-- >        }
--
singleJumpTipPoints :: R.StatefulGen g m => g -> Int -> Int -> m [Int]
singleJumpTipPoints _g m n
    | n < m = pure []
singleJumpTipPoints g m n = do
    jump <- R.uniformRM (m, n) g
    pure [jump..n]

-- | @rollbacksTipPoints k bs g@ generates a schedule for a single peer
-- serving from multiple alternative branches. The schedule is a list of block
-- indices for the tip point of the peer state. Here the block indices are
-- separated per branch in the result. Each index is relative to the branch it
-- belongs to.
--
-- @k@ is the security parameter.
--
-- @bs@ are the length in blocks of the alternative branches of the block tree.
-- The lengths need to be provided in the order in which the branches intersect
-- with the trunk.
--
-- > rollbacksTipPoints
-- >   :: g
-- >   -> {k:Int | k > 0}
-- >   -> {bs:[Int] | all (0<=) bs}
-- >   -> m {v:[Int]
-- >        | isSorted v &&
-- >          all (all (0<=)) v &&
-- >          all (all (<k)) v &&
-- >          all isSorted v &&
-- >          all (not . hasDuplicates) v &&
-- >          and [all (<bn) bbs | (bn, bbs) <- zip bs v] &&
-- >          length v == length bs bracketChainSyncClient
-- >        }
--
rollbacksTipPoints
  :: R.StatefulGen g m => g -> Int -> [Int] -> m [[Int]]
rollbacksTipPoints g k = mapM walkBranch
  where
    walkBranch bn = singleJumpTipPoints g 0 (min (k-1) (bn - 1))


-- | @tipPointSchedule g slotLengh msgDelayInterval slots@ attaches times to a
-- sequence of tip points. These times are the times at which the tip points
-- should be offered to the node under test. Times are expressed as an offset
-- from the time of slot 0.
--
-- @slotLength@ is the length of a slot in seconds.
--
-- @msgDelayInterval@ is the interval from which to sample the delay of
-- each tip point after the slot in which it was minted.
--
-- @slots@ are the slot numbers of the blocks in the tip point schedule.
-- Because the slots might belong to different branches, they might be
-- duplicated or not monotonically increasing. e.g.
--
-- If 0s and 1s signal the tip points that we want to announce
--
-- > slot number: 0123456
-- > trunk  :     011001
-- > alternative:   01101
--
-- The slots of the tip points to serve could be @[1, 2, 5, 3, 4, 6]@
-- Then the generated times could be close to
--
-- > [1*20, 2*20, 5*20, t3, t4, 6*20]
--
-- where @t3@ and @t4@ are chosen randomly in the interval between the
-- branch tips, that is between 5*20 and 6*20.
--
-- > tipPointSchedule
-- >   :: g
-- >   -> {slotLength:DiffTime | slotLength >= 0}
-- >   -> {msgDelayInterval:(DiffTime, DiffTime)
-- >      | fst msgDelayInterval <= snd msgDelayInterval
-- >      }
-- >   -> {slots:[SlotNo] | all (0<=) slots}
-- >   -> m {v:[DiffTime] | isSorted v && length v == length slots}
--
tipPointSchedule
  :: forall g m. R.StatefulGen g m => g -> DiffTime -> (DiffTime, DiffTime) -> [SlotNo] -> m [DiffTime]
tipPointSchedule _g slotLength (a, b) _slots
    | slotLength <= b = error "tipPointSchedule: slotLength <= maximum delay"
    | b < a = error "tipPointSchedule: empty delay interval"
tipPointSchedule g slotLength msgDelayInterval slots = do
    let -- pairs of times corresponding to the start and end of each interval
        -- between tip points
        slotTimes = map toDiffTime slots
        timePairs = zip slotTimes $ (drop 1 slotTimes) ++ [last slotTimes + 1]
    go timePairs
  where
    go :: [(DiffTime, DiffTime)] -> m [DiffTime]
    go [] = pure []
    go xs = do
      -- While the slots are increasing, assign a time to each point
      -- by choosing a random time in the delay interval after the
      -- slot start
      let (pointSeq, newBranch) = span (\(a, b) -> a < b) xs
      times <- forM pointSeq $ \(s, _) -> do
                 delay <- uniformRMDiffTime msgDelayInterval g
                 pure $ s + delay
      (times', xss) <- case newBranch of
        [] -> pure ([], [])
        ((seqLast, _) : branches) -> do
          delay <- uniformRMDiffTime msgDelayInterval g
          let lastTime = seqLast + delay
          (times', xss) <- handleDelayedTipPoints lastTime branches
          pure (lastTime : times', xss)
      -- When the slots are not increasing, we must be doing a rollback.
      -- We might have tip points in past slots.
      times'' <- go xss
      pure $ times ++ times' ++ times''

    -- | The start of each slot in the schedule
    toDiffTime :: SlotNo -> DiffTime
    toDiffTime (SlotNo s) = fromIntegral s * slotLength

    -- | Assign times to tip points in past slots. A past slots is
    -- any earlier slot than the first parameter.
    --
    -- Yields the assigned times and the remaining tip points which
    -- aren't in the past.
    handleDelayedTipPoints :: DiffTime -> [(DiffTime, DiffTime)] -> m ([DiffTime], [(DiffTime, DiffTime)])
    handleDelayedTipPoints lastTime xss = do
      let (pointSeq, newBranch) = span (\(a, _) -> a + fst msgDelayInterval <= lastTime) xss
          nseq = length pointSeq
          -- The first point in xss that is not in the past
          firstLater = case newBranch of
            -- If there is no later point, pick an arbitrary later time interval
            -- to sample from
            []           -> lastTime + toDiffTime (toEnum nseq)
            ((a, _) : _) -> a + fst msgDelayInterval
      times <- replicateM nseq (uniformRMDiffTime (lastTime, firstLater) g)
      pure (sort times, newBranch)

uniformRMDiffTime :: R.StatefulGen g m => (DiffTime, DiffTime) -> g -> m DiffTime
uniformRMDiffTime (a, b) g =
    picosecondsToDiffTime <$>
      R.uniformRM (diffTimeToPicoseconds a, diffTimeToPicoseconds b) g

data HeaderPointSchedule = HeaderPointSchedule {
    hpsTrunk  :: [(DiffTime, Int)] -- ^ header points up to the intersection
  , hpsBranch :: [(DiffTime, Int)] -- ^ header points after the intersection
                                   -- indices are relative to the branch
  }
  deriving (Show)

-- | @headerPointSchedule g msgDelayInterval tpSchedule@ generates a
-- schedule of header points for a single peer.
--
-- @msgDelayInterval@ is the interval from which to sample the delay of
-- each header after offering the previous header.
--
-- @tpSchedule@ is the tip point schedule for the peer.
-- Tip points are grouped by the branch they point to. Each group has
-- the index of the intersection block with the trunk. Then each group
-- has a list of tip point block indices relative to the branch. Groups
-- corresponding to tip points in trunk use 'Nothing' as the intersection.
--
-- For each group of tip points, the schedule generates a HeaderPointSchedule,
-- which provides the time at which each header should be offered.
--
-- If scheduled, header points are guaranteed to be scheduled after the tip
-- point that enables them. They might not be generated if they cannot be
-- delivered before a tip point of a different branch is announced.  The
-- header points on a same chain are never announced out of order.
--
-- > headerPointSchedule
-- >   :: g
-- >   -> {msgDelayInterval:(DiffTime, DiffTime)
-- >      | fst msgDelayInterval <= snd msgDelayInterval
-- >      }
-- >   -> {tpSchedule:[(Maybe Int, [(DiffTime, Int)]]
-- >      | isSorted (catMaybes (map fst tpSchedule)) &&
-- >        all (\(_, xs) -> isSorted xs) tpSchedule &&
-- >        all (\(_, xs) -> all (0<=) (map snd xs)) tpSchedule &&
-- >        all (\(_, xs) -> not (hasDuplicates (map snd xs))) tpSchedule &&
-- >        all (\(_, xs) -> not (null xs)) tpSchedule &&
-- >        all (\(_, xs) -> all (0<=) (map snd xs)) tpSchedule
-- >      }
-- >   -> m {v:[HeaderPointSchedule]
-- >        | length v == length tpSchedule &&
-- >          isSorted [ map fst (hpsTrunk hps) ++ map fst (hpsBranch hps) | hps <- v ] &&
-- >          isSorted [ map snd (hpsTrunk hps) | hps <- v ] &&
-- >          all (\hps -> isSorted (map snd $ hpsBranch hps)) v &&
-- >          all (\hps -> all (0<=) (map snd (hpsTrunk hps))) v &&
-- >          all (\hps -> all (0<=) (map snd (hpsBranch hps))) v &&
-- >          all (\hps -> not (hasDuplicates (map snd (hpsTrunk hps)))) v &&
-- >          all (\hps -> not (hasDuplicates (map snd (hpsBranch hps)))) v
-- >        }
headerPointSchedule
  :: forall g m. (HasCallStack, R.StatefulGen g m)
  => g
  -> (DiffTime, DiffTime)
  -> [(Maybe Int, [(DiffTime, Int)])]
  -> m [HeaderPointSchedule]
headerPointSchedule g msgDelayInterval xs =
   let -- Pair each  branch with the maximum time at which its header points
       -- should be offered
       xs' = zip xs $ map (Just . fst . headCallStack . snd) (drop 1 xs) ++ [Nothing]
    in snd <$> mapAccumM genHPBranchSchedule (0, 0) xs'

  where
    -- | @genHPBranchSchedule (tNext, trunkNextHp) ((mi, tps), mtMax)@ generates
    -- a schedule for a single branch.
    --
    -- @tNext@ is the time at which the next header point should be offered.
    --
    -- @trunkNextHp@ is the index of the next header point that was offered
    -- from the trunk.
    --
    -- @mi@ is the index of the intersection block with the trunk. Nothing
    -- means this group has tip points from the trunk.
    --
    -- @tps@ is the list of tip point indices relative to the branch.
    --
    -- @mtMax@ is the maximum time at which the last header point can be
    -- offered. 'Nothing' stands for infinity.
    --
    -- Returns the time at which the last header point was offered, the next
    -- header point to offer and the schedule for the branch.
    genHPBranchSchedule
      :: (DiffTime, Int)
      -> ((Maybe Int, [(DiffTime, Int)]), Maybe DiffTime)
      -> m ((DiffTime, Int), HeaderPointSchedule)
    genHPBranchSchedule (tNext, trunkNextHp) ((_mi, []), _mtMax) =
      pure ((tNext, trunkNextHp), HeaderPointSchedule [] [])
    genHPBranchSchedule (tNext, trunkNextHp) ((Nothing, tps), mtMax) = do
      (p, tsTrunk) <- mapAccumM (generatePerTipPointTimes mtMax) (tNext, trunkNextHp) tps
      pure (p, HeaderPointSchedule (concat tsTrunk) [])
    genHPBranchSchedule (tNext, trunkNextHp) ((Just iLast, tps@((firstTipTime, _):_)), mtMax) = do
      ((tNext', trunkNextHp'), tsTrunk) <- generatePerTipPointTimes mtMax (tNext, trunkNextHp) (firstTipTime, iLast)
      ((tNext'', _), tsBranch) <- mapAccumM (generatePerTipPointTimes mtMax) (tNext', 0) tps
      pure ((tNext'', trunkNextHp'), HeaderPointSchedule tsTrunk (concat tsBranch))

    -- | @generatePerTipPointTimes mtMax (tNext, nextHp) (tTip, tp)@ schedules the header
    -- points from @nextHp@ to @tp@ in ascending order starting from the maximum
    -- of @tNext@ and @tTip + t@ where t is sampled from @msgDelayInterval@.
    --
    -- Less header points are scheduled if they would be scheduled after @mtMax@.
    --
    -- The delay of each tipPoint is sampled from @msgDelayInterval@.
    --
    generatePerTipPointTimes
      :: Maybe DiffTime
      -> (DiffTime, Int)
      -> (DiffTime, Int)
      -> m ((DiffTime, Int), [(DiffTime, Int)])
    generatePerTipPointTimes mtMax (tNext0, nextHp0) (tTip, tp) = do
       t <- uniformRMDiffTime msgDelayInterval g
       go (max tNext0 (tTip + t)) nextHp0 []
      where
        go :: DiffTime -> Int -> [(DiffTime, Int)] -> m ((DiffTime, Int), [(DiffTime, Int)])
        go tNext nextHp acc = do
          if maybe False (tNext >) mtMax || nextHp > tp then
            pure ((tNext, nextHp), reverse acc)
          else do
            t <- (+tNext) <$> uniformRMDiffTime msgDelayInterval g
            go t (nextHp+1) ((tNext, nextHp) : acc)

mapAccumM :: Monad m => (s -> x -> m (s, y)) -> s -> [x] -> m (s, [y])
mapAccumM _ acc [] = pure (acc, [])
mapAccumM f acc (x:xs) = do
    (acc', y) <- f acc x
    (acc'', ys) <- mapAccumM f acc' xs
    pure (acc'', y:ys)

headCallStack :: HasCallStack => [a] -> a
headCallStack = \case
  x:_ -> x
  _   -> error "headCallStack: empty list"
