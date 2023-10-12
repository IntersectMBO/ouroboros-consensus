module Test.Consensus.Network.AnchoredFragment.Extras (slotLength) where

import           Cardano.Slotting.Slot (SlotNo (unSlotNo), withOrigin)
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment,
                     HasHeader, anchor, anchorToSlotNo, headAnchor)

-- | Number of slots on which the fragment spans. This is different from the
-- 'length' which is the number of blocks in the fragment.
slotLength :: HasHeader blk => AnchoredFragment blk -> Int
slotLength fragment =
  fromIntegral $ unSlotNo $
    withOrigin 0 id (anchorToSlotNo $ headAnchor fragment)
    - withOrigin 0 id (anchorToSlotNo $ anchor fragment)
