-- | Functions to move to Ouroboros.Network.AnchoredFragment
module Test.Consensus.Network.AnchoredFragment.Extras
  ( intersectWith
  , slotLength
  ) where

import Cardano.Slotting.Slot (SlotNo (unSlotNo), withOrigin)
import Data.List (find)
import Data.Maybe (isJust)
import Ouroboros.Network.AnchoredFragment
  ( AnchoredFragment
  , HasHeader
  , Point
  , anchor
  , anchorToSlotNo
  , headAnchor
  , splitAfterPoint
  )

-- | Find the first point in the fragment
intersectWith :: HasHeader b => AnchoredFragment b -> [Point b] -> Maybe (Point b)
intersectWith fullFrag = find (isJust . splitAfterPoint fullFrag)

-- | The number of slots the fragment spans. This is different from the
-- 'length' which is the number of blocks in the fragment.
slotLength :: HasHeader blk => AnchoredFragment blk -> Int
slotLength fragment =
  fromIntegral $
    unSlotNo $
      withOrigin 0 id (anchorToSlotNo $ headAnchor fragment)
        - withOrigin 0 id (anchorToSlotNo $ anchor fragment)
