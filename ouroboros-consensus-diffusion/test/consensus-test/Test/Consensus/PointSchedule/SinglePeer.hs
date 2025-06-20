-- | This module contains functions for generating random point schedules.
--
-- A point schedule is a set of tables, having one table per simulated peer.
-- Each row of a table correspond to a point in time, also called a tick.
--
-- Each tick has a timestamp and a state to set for the corresponding peer at
-- that time. The state of each peer is described by three blocks points
-- related to the peer's hypothetical chain selection:
--
-- * Tip Point: the tip of the hypothetical chain selection. This tip is
--       advertised to the node under test in ChainSync client exchanges.
-- * Header Point: the most recent header that the peer should send to the
--       node under test. Any newer headers should wait until the Header
--       Point is updated to newer headers.
-- * Block Point: the most recent block that the peer should send to the
--       node under test. Any newer blocks should wait until the Block Point
--       is updated to newer headers.
--
-- Given a chain selection like this:
--
-- > Genesis -> A -> B -> C -> D
--
-- The point schedule of a peer might look like this:
--
-- > +--------+----------------+----------------+----------------+
-- > | Tick   | Tip Point      | Header Point   | Block Point    |
-- > +--------+----------------+----------------+----------------+
-- > |   0.0s | Genesis        | Genesis        | Genesis        |
-- > +--------+----------------+----------------+----------------+
-- > |   1.2s | D              | Genesis        | Genesis        |
-- > +--------+----------------+----------------+----------------+
-- > |   1.3s | D              | C              | Genesis        |
-- > +--------+----------------+----------------+----------------+
-- > |   1.6s | D              | C              | B              |
-- > +--------+----------------+----------------+----------------+
-- > |   2.0s | D              | C              | C              |
-- > +--------+----------------+----------------+----------------+
-- > |   2.2s | D              | D              | D              |
-- > +--------+----------------+----------------+----------------+
--
-- Some rules apply to how the point schedule progresses, although
-- exceptions might be tested occasionally. In general,
--
-- * The Tip Point is set first
-- * Header points are not allowed to point to newer blocks than the tip point
-- * Block points are not allowed to point to newer blocks than the header point
--
-- The following is an example with rollbacks:
--
-- > Genesis -> A -> B -> C -> D
-- >                   \-> C' -> D' -> E
--
-- > +--------+----------------+----------------+----------------+
-- > | Tick   | Tip Point      | Header Point   | Block Point    |
-- > +--------+----------------+----------------+----------------+
-- > |   0.0s | Genesis        | Genesis        | Genesis        |
-- > +--------+----------------+----------------+----------------+
-- > |   1.2s | D              | Genesis        | Genesis        |
-- > +--------+----------------+----------------+----------------+
-- > |   1.3s | D              | C              | Genesis        |
-- > +--------+----------------+----------------+----------------+
-- > |   1.6s | D              | C              | B              |
-- > +--------+----------------+----------------+----------------+
-- > |   2.0s | D              | C              | C              |
-- > +--------+----------------+----------------+----------------+
-- > |   2.1s | D              | D              | C              |
-- > +--------+----------------+----------------+----------------+
-- > |   2.3s | D              | D              | D              |
-- > +--------+----------------+----------------+----------------+
-- > |   2.4s | E              | D              | D              |
-- > +--------+----------------+----------------+----------------+
-- > |   2.6s | E              | D'             | D              |
-- > +--------+----------------+----------------+----------------+
-- > |   2.7s | E              | D'             | C'             |
-- > +--------+----------------+----------------+----------------+
-- > |   2.9s | E              | D'             | D'             |
-- > +--------+----------------+----------------+----------------+
-- > |   3.0s | E              | E              | D'             |
-- > +--------+----------------+----------------+----------------+
-- > |   3.1s | E              | E              | E              |
-- > +--------+----------------+----------------+----------------+
module Test.Consensus.PointSchedule.SinglePeer
  ( IsTrunk (..)
  , PeerScheduleParams (..)
  , SchedulePoint (..)
  , defaultPeerScheduleParams
  , peerScheduleFromTipPoints
  , schedulePointToBlock
  , singleJumpPeerSchedule

    -- * Exposed for testing
  , mergeOn
  , scheduleBlockPoint
  , scheduleHeaderPoint
  , scheduleTipPoint
  , zipMany
  ) where

import Cardano.Slotting.Slot (WithOrigin (At, Origin), withOrigin)
import Control.Arrow (second)
import Control.Monad.Class.MonadTime.SI (Time)
import Data.List (mapAccumL)
import Data.Time.Clock (DiffTime)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (BlockNo (unBlockNo), blockSlot)
import qualified System.Random.Stateful as R (StatefulGen)
import Test.Consensus.PointSchedule.SinglePeer.Indices
  ( HeaderPointSchedule (hpsBranch, hpsTrunk)
  , headerPointSchedule
  , singleJumpTipPoints
  , tipPointSchedule
  )

-- | A point in the schedule of a single peer.
data SchedulePoint blk
  = ScheduleTipPoint (WithOrigin blk)
  | ScheduleHeaderPoint (WithOrigin blk)
  | ScheduleBlockPoint (WithOrigin blk)
  deriving (Eq, Show)

scheduleTipPoint :: blk -> SchedulePoint blk
scheduleTipPoint = ScheduleTipPoint . At

scheduleHeaderPoint :: blk -> SchedulePoint blk
scheduleHeaderPoint = ScheduleHeaderPoint . At

scheduleBlockPoint :: blk -> SchedulePoint blk
scheduleBlockPoint = ScheduleBlockPoint . At

schedulePointToBlock :: SchedulePoint blk -> WithOrigin blk
schedulePointToBlock (ScheduleTipPoint b) = b
schedulePointToBlock (ScheduleHeaderPoint b) = b
schedulePointToBlock (ScheduleBlockPoint b) = b

-- | Parameters for generating a schedule for a single peer.
--
-- In the most general form, the caller provides a list of tip points and the
-- schedule is generated by following the given tip points. All headers points
-- and block points are sent eventually, but the points are delayed according
-- to these parameters.
data PeerScheduleParams = PeerScheduleParams
  { pspSlotLength :: DiffTime
  , pspTipDelayInterval :: (DiffTime, DiffTime)
  -- ^ Each of these pairs specifies a range of delays for a point. The
  -- actual delay is chosen uniformly at random from the range.
  --
  -- For tip points, the delay is relative to the slot of the tip point.
  , pspHeaderDelayInterval :: (DiffTime, DiffTime)
  -- ^ For header points, the delay is relative to the previous header point
  -- or the tip point that advertises the existence of the header (whichever
  -- happened most recently).
  , pspBlockDelayInterval :: (DiffTime, DiffTime)
  -- ^ For block points, the delay is relative to the previous block point or
  -- the header point that advertises the existence of the block (whichever
  -- happened most recently).
  }
  deriving Show

defaultPeerScheduleParams :: PeerScheduleParams
defaultPeerScheduleParams =
  PeerScheduleParams
    { pspSlotLength = 20
    , pspTipDelayInterval = (0, 1)
    , pspHeaderDelayInterval = (0.018, 0.021)
    , pspBlockDelayInterval = (0.050, 0.055)
    }

-- | Generate a schedule for a single peer that jumps once to the middle of a
-- sequence of blocks.
--
--  See 'peerScheduleFromTipPoints' for generation of schedules with rollbacks
singleJumpPeerSchedule ::
  (R.StatefulGen g m, AF.HasHeader blk) =>
  g ->
  PeerScheduleParams ->
  AF.AnchoredFragment blk ->
  m [(Time, SchedulePoint blk)]
singleJumpPeerSchedule g psp chain = do
  let chainv = Vector.fromList $ AF.toOldestFirst chain
  (tps, hps, bps) <- singleJumpRawPeerSchedule g psp chainv
  let tipPoints = map (second scheduleTipPoint) tps
      headerPoints = map (second scheduleHeaderPoint) hps
      blockPoints = map (second scheduleBlockPoint) bps
  -- merge the schedules
  pure $
    mergeOn fst tipPoints $
      mergeOn fst headerPoints blockPoints

singleJumpRawPeerSchedule ::
  (R.StatefulGen g m, AF.HasHeader b) =>
  g ->
  PeerScheduleParams ->
  Vector b ->
  m ([(Time, b)], [(Time, b)], [(Time, b)])
singleJumpRawPeerSchedule g psp chainv = do
  -- generate the tip points
  ixs <- singleJumpTipPoints g 0 (Vector.length chainv - 1)
  let tipPointBlks = map (chainv Vector.!) ixs
      tipPointSlots = map blockSlot tipPointBlks
  -- generate the tip point schedule
  ts <- tipPointSchedule g (pspSlotLength psp) (pspTipDelayInterval psp) tipPointSlots
  -- generate the header point schedule
  hpss <- headerPointSchedule g (pspHeaderDelayInterval psp) [(Nothing, zip ts ixs)]
  let hps = concatMap hpsTrunk hpss
  -- generate the block point schedule
  bpss <- headerPointSchedule g (pspBlockDelayInterval psp) [(Nothing, hps)]
  -- collect the blocks for each schedule
  let bps = concatMap hpsTrunk bpss
      tipPointTips = zip ts tipPointBlks
      hpsHeaders = map (second (chainv Vector.!)) hps
      bpsBlks = map (second (chainv Vector.!)) bps
  pure (tipPointTips, hpsHeaders, bpsBlks)

data IsTrunk = IsTrunk | IsBranch
  deriving (Eq, Show)

-- | @peerScheduleFromTipPoints g params tps trunk branches@ generates a
-- schedule for a single peer that follows the given tip points.
--
-- @tps@ contains the tip points for each fragment. The indices correspond to
-- the fragments in branches and go from @0@ to @length branch - 1@.
--
-- @trunk@ is the fragment for the honest chain
--
-- @branches@ contains the fragments for the alternative chains in ascending
-- order of their intersections with the honest chain. Each fragment is anchored
-- at the intersection, and therefore their first block must be the first block
-- after the intersection.
peerScheduleFromTipPoints ::
  (R.StatefulGen g m, AF.HasHeader blk) =>
  g ->
  PeerScheduleParams ->
  [(IsTrunk, [Int])] ->
  AF.AnchoredFragment blk ->
  [AF.AnchoredFragment blk] ->
  m [(Time, SchedulePoint blk)]
peerScheduleFromTipPoints g psp tipPoints trunk0 branches0 = do
  let trunk0v = Vector.fromList $ AF.toOldestFirst trunk0
      -- NOTE: Is this still correct? Shouldn't it be `withOrigin 0 (+1)`?
      firstTrunkBlockNo = withOrigin 1 (+ 1) $ AF.anchorBlockNo trunk0
      branches0v = map (Vector.fromList . AF.toOldestFirst) branches0
      anchorBlockIndices =
        [ fromIntegral $ unBlockNo $ fragmentAnchorBlockNo b - firstTrunkBlockNo
        | b <- branches0
        ]
      isTrunks = map fst tipPoints
      intersections = intersperseTrunkFragments anchorBlockIndices isTrunks
  (tps, hps, bps) <- rawPeerScheduleFromTipPoints g psp tipPoints trunk0v branches0v intersections
  let tipPoints' = map (second scheduleTipPoint) tps
      headerPoints = map (second scheduleHeaderPoint) hps
      blockPoints = map (second scheduleBlockPoint) bps
  -- merge the schedules
  pure $
    mergeOn fst tipPoints' $
      mergeOn fst headerPoints blockPoints
 where
  fragmentAnchorBlockNo :: AF.AnchoredFragment blk -> BlockNo
  fragmentAnchorBlockNo f = case AF.anchorBlockNo f of
    At s -> s
    Origin -> 0

  intersperseTrunkFragments :: [Int] -> [IsTrunk] -> [Maybe Int]
  intersperseTrunkFragments [] [] = []
  intersperseTrunkFragments iis (IsTrunk : isTrunks) = Nothing : intersperseTrunkFragments iis isTrunks
  intersperseTrunkFragments (i : is) (IsBranch : isTrunks) = Just i : intersperseTrunkFragments is isTrunks
  intersperseTrunkFragments _ [] = error "intersperseTrunkFragments: not enough isTrunk flags"
  intersperseTrunkFragments [] _ = error "intersperseTrunkFragments: not enough intersections"

rawPeerScheduleFromTipPoints ::
  (R.StatefulGen g m, AF.HasHeader b) =>
  g ->
  PeerScheduleParams ->
  [(IsTrunk, [Int])] ->
  Vector b ->
  [Vector b] ->
  [Maybe Int] ->
  m ([(Time, b)], [(Time, b)], [(Time, b)])
rawPeerScheduleFromTipPoints g psp tipPoints trunk0v branches0v intersections = do
  let (isTrunks, tpIxs) = unzip tipPoints
      pairedVectors = pairVectorsWithChunks trunk0v branches0v isTrunks
      tipPointBlks = concat $ zipWith indicesToBlocks pairedVectors tpIxs
      tipPointSlots = map blockSlot tipPointBlks
  -- generate the tip point schedule
  ts <- tipPointSchedule g (pspSlotLength psp) (pspTipDelayInterval psp) tipPointSlots
  -- generate the header point schedule
  let tpSchedules = zipMany ts tpIxs
  hpss <- headerPointSchedule g (pspHeaderDelayInterval psp) $ zip intersections tpSchedules
  -- generate the block point schedule
  let (hpsPerBranch, vs) =
        unzip $
          filter (not . null . snd . fst) $
            concat
              [ [((Nothing, hpsTrunk hps), trunk0v), ((mi, hpsBranch hps), v)]
              | (mi, hps, v) <- zip3 intersections hpss pairedVectors
              ]
  bpss <- headerPointSchedule g (pspBlockDelayInterval psp) hpsPerBranch
  let tipPointTips = zip ts tipPointBlks
      hpsHeaders = concat $ zipWith (scheduleIndicesToBlocks trunk0v) pairedVectors hpss
      bpsBlks = concat $ zipWith (scheduleIndicesToBlocks trunk0v) vs bpss
  pure (tipPointTips, hpsHeaders, bpsBlks)
 where
  pairVectorsWithChunks ::
    Vector b ->
    [Vector b] ->
    [IsTrunk] ->
    [Vector b]
  pairVectorsWithChunks trunk branches =
    snd . mapAccumL pairVectors branches
   where
    pairVectors brs IsTrunk = (brs, trunk)
    pairVectors (br : brs) IsBranch = (brs, br)
    pairVectors [] IsBranch = error "not enough branches"

  -- \| Replaces block indices with the actual blocks
  scheduleIndicesToBlocks :: Vector b -> Vector b -> HeaderPointSchedule -> [(Time, b)]
  scheduleIndicesToBlocks trunk v hps =
    map (second (trunk Vector.!)) (hpsTrunk hps)
      ++ map (second (v Vector.!)) (hpsBranch hps)

  indicesToBlocks :: Vector b -> [Int] -> [b]
  indicesToBlocks v ixs = map (v Vector.!) ixs

-- | Merge two sorted lists.
--
-- PRECONDITION: The lists are sorted.
mergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeOn _f [] ys = ys
mergeOn _f xs [] = xs
mergeOn f xxs@(x : xs) yys@(y : ys) =
  if f x <= f y
    then x : mergeOn f xs yys
    else y : mergeOn f xxs ys

zipMany :: [a] -> [[b]] -> [[(a, b)]]
zipMany xs0 = snd . mapAccumL (go []) xs0
 where
  go acc xs [] = (xs, reverse acc)
  go _acc [] _ys = error "zipMany: lengths don't match"
  go acc (x : xs) (y : ys) = go ((x, y) : acc) xs ys
