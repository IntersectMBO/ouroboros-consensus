{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.PointSchedule.Shrink (shrinkPointSchedule) where

import           Data.Function ((&))
import           Data.List.NonEmpty (toList)
import           Data.Maybe (mapMaybe)
import           Ouroboros.Network.AnchoredFragment (headPoint)
import           Ouroboros.Network.Block (Point)
import           Test.Consensus.BlockTree (BlockTree (btTrunk))
import           Test.Consensus.PointSchedule
import           Test.QuickCheck (shrinkList)
import           Test.Util.TestBlock (TestBlock)

-- | Given a block tree and an honest node's tick, check whether the honest peer
-- will be alert enough. NOTE: For now, this just means that it serves all of
-- its blocks; when LoP gets implemented, this will have to be revised.
honestNodeIsAlert :: BlockTree TestBlock -> [Tick] -> Bool
honestNodeIsAlert blockTree ticks =
    let point = headPoint (btTrunk blockTree)
     in any (\Tick{active=Peer{value}} -> nodeStateServes point value) ticks
  where
    -- | Whether the given 'NodeState' serves the given 'Point', that is whether
    -- both its 'HeaderPoint' and 'BlockPoint' are the given 'Point'.
    nodeStateServes :: Point TestBlock -> NodeState -> Bool
    nodeStateServes _ NodeOffline = False
    nodeStateServes point (NodeOnline AdvertisedPoints{header, block}) =
      headerPointPoint header == point && blockPointPoint block == point

-- | Whether a point schedule exhibits reasonable behaviour of the peers. For
-- instance, the honest peer cannot be too slow.
isReasonablePointSchedule :: BlockTree TestBlock -> PointSchedule -> Bool
isReasonablePointSchedule blockTree PointSchedule{ticks} =
  toList ticks
    & filter (\Tick{active=Peer{name}} -> name == HonestPeer)
    & honestNodeIsAlert blockTree

-- | Shrink a 'PointSchedule' in a generic way; only yield “reasonable” point
-- schedules. See 'isReasonablePointSchedule'.
shrinkPointSchedule :: BlockTree TestBlock -> PointSchedule -> [PointSchedule]
shrinkPointSchedule blockTree PointSchedule{ticks, peerIds} =
  shrinkList (const []) (toList ticks)
    & mapMaybe (flip pointSchedule peerIds)
    & filter (isReasonablePointSchedule blockTree)
