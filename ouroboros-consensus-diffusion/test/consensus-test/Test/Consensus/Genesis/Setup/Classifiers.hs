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

{- | Interesting classification predicates used by the test:

1. the immutable tip should ALWAYS be honest
2. the selection should be honest IF there is at least a Genesis window after the intersection.

It is possible that, at the end of a test, the selection might be dishonest.
Consider the following example, where k = 5 and scg = 12:

    slots:  0  1  2  3  4  5  6  7  8  9  10  11  12
    trunk:  0──1──2──3──4──5──6──7──8──────9──10──11
                     ╰──4──5──6──7──8──9──────────10

Because of the Limit on Eagerness, the selection is stuck at slot 8 (k-th
block after the intersection).
Since the test chains only have 7 and 8 blocks after the intersection, Genesis
will not be able to make a final decision, since it needs to have enough information
to determine that one of the fragments is definitely denser within 12 slots, and here
both fragments have the potential to win the race.
This means that a test will terminate while the selection contains an arbitrary fragment
whose tip is the block in slot 8.

Without Genesis, if the blocks are sent so that the N+1st slot's blocks arrive after all
blocks in slot N have arrived, the adversarial fragment will be selected every time because
it will have k+1 blocks first.

So in our test we classify the input data according to the presence of an entire Genesis
window after the intersection of at least one adversarial chain with the honest chain,
on both chains.
-}
data Classifiers =
  Classifiers {
    existsSelectableAdversary :: Bool,
    genesisAfterIntersection  :: Bool
  }

classifiers :: GenesisTest -> Classifiers
classifiers GenesisTest {gtBlockTree, gtSecurityParam = SecurityParam k, gtGenesisWindow = GenesisWindow scg} =
  Classifiers {existsSelectableAdversary, genesisAfterIntersection}
  where
    genesisAfterIntersection =
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
