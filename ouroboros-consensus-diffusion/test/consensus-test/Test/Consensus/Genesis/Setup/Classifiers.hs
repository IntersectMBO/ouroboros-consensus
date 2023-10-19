{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Consensus.Genesis.Setup.Classifiers (
    Classifiers (..)
  , classifiers
  ) where

import           Ouroboros.Consensus.Block.Abstract (SlotNo (SlotNo),
                     withOrigin)
import           Ouroboros.Consensus.Config
import           Ouroboros.Network.AnchoredFragment (anchor, anchorToSlotNo,
                     headSlot)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Test.Consensus.BlockTree (BlockTree (..), BlockTreeBranch (..))
import           Test.Consensus.Network.AnchoredFragment.Extras (slotLength)
import           Test.Consensus.PointSchedule
import           Test.Util.Orphans.IOLike ()

-- | Interesting categories to classify test inputs
data Classifiers =
  Classifiers {
    -- | There are more than k blocks in at least one alternative chain after the intersection
    existsSelectableAdversary      :: Bool,
    -- | There are more than k blocks in all alternative chains after the intersection
    allAdversariesSelectable       :: Bool,
    -- | There are at least scg slots after the intesection on both the honest
    -- and the alternative chain
    --
    -- Knowing if there is a Genesis window after the intersection is important because
    -- otherwise the Genesis node has no chance to advance the immutable tip past
    -- the Limit on Eagerness.
    --
    genesisWindowAfterIntersection :: Bool
  }

classifiers :: GenesisTest -> Classifiers
classifiers GenesisTest {gtBlockTree, gtSecurityParam = SecurityParam k, gtGenesisWindow = GenesisWindow scg} =
  Classifiers {existsSelectableAdversary, allAdversariesSelectable, genesisWindowAfterIntersection}
  where
    genesisWindowAfterIntersection =
      any fragmentHasGenesis branches

    fragmentHasGenesis btb =
      let
        frag = btbSuffix btb
        SlotNo intersection = withOrigin 0 id (anchorToSlotNo (anchor frag))
      in isSelectable btb && slotLength frag > fromIntegral scg && goodTipSlot - intersection > scg

    existsSelectableAdversary =
      any isSelectable branches

    allAdversariesSelectable =
      all isSelectable branches

    isSelectable bt = AF.length (btbSuffix bt) > fromIntegral k

    SlotNo goodTipSlot = withOrigin 0 id (headSlot goodChain)

    branches = btBranches gtBlockTree

    goodChain = btTrunk gtBlockTree
