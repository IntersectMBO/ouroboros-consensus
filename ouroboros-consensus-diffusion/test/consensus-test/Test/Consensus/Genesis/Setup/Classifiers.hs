{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Test.Consensus.Genesis.Setup.Classifiers (
    Classifiers (..)
  , classifiers
  , simpleHash
  ) where

import           Cardano.Slotting.Slot (WithOrigin (At))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Word (Word64)
import           Ouroboros.Consensus.Block (ChainHash (BlockHash), HeaderHash,
                     blockSlot, succWithOrigin)
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
import           Test.Util.TestBlock (TestHash (TestHash))

-- | Interesting categories to classify test inputs
data Classifiers =
  Classifiers {
    -- | There are more than k blocks in at least one alternative chain after the intersection
    existsSelectableAdversary      :: Bool,
    -- | There are more than k blocks in all alternative chains after the
    -- intersection. Note that this is always guaranteed for the honest chain.
    allAdversariesSelectable       :: Bool,
    -- | There is always at least one block per sliding forecast window in all
    -- alternative chains. Note that this is always guaranteed for the honest
    -- chain.
    allAdversariesForecastable     :: Bool,
    -- | All adversaries have at least k+1 block in the forecast window the
    -- follows their intersection with the trunk. Note that the generator always
    -- enforces that the trunk wins in all _Genesis_ windows after the
    -- intersection. In particular, if sgen = sfor, then the trunk will have at
    -- least k+2.
    allAdversariesKPlus1InForecast :: Bool,
    -- | There are at least scg slots after the intesection on both the honest
    -- and the alternative chain
    --
    -- Knowing if there is a Genesis window after the intersection is important because
    -- otherwise the Genesis node has no chance to advance the immutable tip past
    -- the Limit on Eagerness.
    --
    genesisWindowAfterIntersection :: Bool,
    -- | The honest chain's slot count is greater than or equal to the Genesis window size.
    longerThanGenesisWindow        :: Bool
  }

classifiers :: AF.HasHeader blk => GenesisTest blk schedule -> Classifiers
classifiers GenesisTest {gtBlockTree, gtSecurityParam = SecurityParam k, gtGenesisWindow = GenesisWindow scg} =
  Classifiers {
    existsSelectableAdversary,
    allAdversariesSelectable,
    allAdversariesForecastable,
    allAdversariesKPlus1InForecast,
    genesisWindowAfterIntersection,
    longerThanGenesisWindow
  }
  where
    longerThanGenesisWindow = AF.headSlot goodChain >= At (fromIntegral scg)

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

    allAdversariesForecastable =
      all isForecastable branches

    isForecastable bt =
      -- FIXME: We are using `scg` here but what we really mean is `sfor`.
      -- Distinguish `scg` vs. `sgen` vs. `sfor` and use the latter here.
      let slotNos = map blockSlot $ AF.toOldestFirst $ btbFull bt in
      all (\(SlotNo prev, SlotNo next) -> next - prev <= scg) (zip slotNos (drop 1 slotNos))

    allAdversariesKPlus1InForecast =
      all hasKPlus1InForecast branches

    hasKPlus1InForecast BlockTreeBranch{btbSuffix} =
      -- FIXME: We are using `scg` here but what we really mean is `sfor`.
      -- Distinguish `scg` vs. `sgen` vs. `sfor` and use the latter here.
      let forecastSlot = succWithOrigin (anchorToSlotNo $ anchor btbSuffix) + SlotNo scg
          forecastBlocks = AF.takeWhileOldest (\b -> blockSlot b < forecastSlot) btbSuffix
       in AF.length forecastBlocks >= fromIntegral k + 1

    SlotNo goodTipSlot = withOrigin 0 id (headSlot goodChain)

    branches = btBranches gtBlockTree

    goodChain = btTrunk gtBlockTree

simpleHash ::
  HeaderHash block ~ TestHash =>
  ChainHash block ->
  [Word64]
simpleHash = \case
  BlockHash (TestHash h) -> reverse (NonEmpty.toList h)
  -- not matching on @GenesisHash@ because 8.10 can't prove exhaustiveness of
  -- TestHash with the equality constraint
  _ -> []
