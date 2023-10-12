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

{- | Interesting categories to classify test inputs

Knowing if there is a Genesis window after the intersection is important because
otherwise the Genesis node has no chance to advance the immutable tip past
the Limit on Eagerness.

It is possible that, at the end of a test, the selection might be dishonest.
Consider the following example, where k = 5 and scg = 12:

    slots:  0  1  2  3  4  5  6  7  8  9  10  11  12
    trunk:  0──1──2──3──4──5──6──7──8──────9──10──11
                     ╰──4──5──6──7─────────8───9──10

Because of the Limit on Eagerness, the selection is stuck at slot 8 (k-th
block after the intersection).
Since the test chains only have 7 and 8 blocks after the intersection, the Genesis node
will not be able to make a final decision, since it needs to have enough information
to determine that one of the fragments is definitely denser within the 12 slots after
the intersection (at slot 16), and here both chains still have the chance to be
the densest.
This means that a test will terminate while the selection contains an arbitrary fragment
whose tip is the block in slot 8.
-}
data Classifiers =
  Classifiers {
    -- | There are more than k blocks in the alternative chain after the intersection
    existsSelectableAdversary      :: Bool,
    -- | There are at least scg slots after the intesection on both the honest
    -- and the alternative chain
    genesisWindowAfterIntersection :: Bool
  }

classifiers :: GenesisTest -> Classifiers
classifiers GenesisTest {gtBlockTree, gtSecurityParam = SecurityParam k, gtGenesisWindow = GenesisWindow scg} =
  Classifiers {existsSelectableAdversary, genesisWindowAfterIntersection}
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

    isSelectable BlockTreeBranch{..} = AF.length btbSuffix > fromIntegral k

    SlotNo goodTipSlot = withOrigin 0 id (headSlot goodChain)

    branches = btBranches gtBlockTree

    goodChain = btTrunk gtBlockTree
