{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.GenChain (module Test.Ouroboros.Consensus.ChainGenerator.Tests.GenChain) where

import           Data.List (foldl')
import qualified Data.Vector.Unboxed as Vector
import           Debug.Trace (trace)
import           Ouroboros.Consensus.Block.Abstract (SlotNo (SlotNo))
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Test.Ouroboros.Consensus.ChainGenerator.Adversarial as A
import           Test.Ouroboros.Consensus.ChainGenerator.Counting
                     (Count (Count), getVector)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Honest as H
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Slot as S
import           Test.Ouroboros.Consensus.ChainGenerator.Slot (S)
import           Test.QuickCheck.Random (QCGen)
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock, TestBlockWith (tbSlot),
                     firstBlock, forkBlock, successorBlock)
import Ouroboros.Network.Block (blockNo)
import Ouroboros.Consensus.Block (BlockNo)

-- | Returns the good chain, bad chain, block number of the last common block,
-- slot number of the last common block, and the total number of slots in the
-- adversarial chain.
genChains ::
  Asc ->
  A.AdversarialRecipe base hon ->
  A.SomeCheckedAdversarialRecipe base hon ->
  QCGen ->
  (AF.AnchoredFragment TestBlock, AF.AnchoredFragment TestBlock, BlockNo, SlotNo, SlotNo)
genChains ascA A.AdversarialRecipe {A.arHonest, A.arPrefix = Count prefixCount} (A.SomeCheckedAdversarialRecipe _ recipeA') seed =
  -- trace ("honest block count: " ++ show (length goodBlocks)) $
  -- trace ("honest slot count: " ++ show (length slotsH)) $
  -- trace ("prefix: " ++ show prefixCount) $
  -- trace ("good: " ++ show (S.test S.notInverted <$> slotsH)) $
  -- trace ("bad: " ++ show (S.test S.notInverted <$> (Vector.toList (getVector vA)))) $
  (goodChain, badChain, prefixBlockNo, prefixSlotNo, SlotNo (fromIntegral (length slotsA)))
  where
    goodChain = mkTestFragment goodBlocks

    badChain = mkTestFragment (mkTestBlocks False prefix slotsA)

    (prefixBlockNo, prefixSlotNo) =
      case prefix of
        h : _ -> (blockNo h, tbSlot h)
        []    -> (0, 0)

    -- blocks in the common prefix in reversed order
    prefix = drop (length goodBlocks - prefixCount) goodBlocks

    slotsA = Vector.toList (getVector vA)

    -- blocks for the good chain in reversed order
    goodBlocks = mkTestBlocks True [] slotsH

    slotsH = Vector.toList (getVector vH)

    incSlot :: SlotNo -> TestBlock -> TestBlock
    incSlot n b = b { tbSlot = tbSlot b + n }

    mkTestFragment :: [TestBlock] -> AnchoredFragment TestBlock
    mkTestFragment =
      AF.fromNewestFirst AF.AnchorGenesis

    mkTestBlocks :: Bool -> [TestBlock] -> [S] -> [TestBlock]
    mkTestBlocks honest pre active =
      fst (foldl' folder ([], 0) active)
      where
        folder (chain, inc) s | S.test S.notInverted s = (issue inc chain, 0)
                              | otherwise = (chain, inc + 1)
        issue inc (h : t) = incSlot inc (successorBlock h) : h : t
        issue inc [] | [] <- pre = [ incSlot inc (firstBlock (if honest then 0 else 1)) ]
                     | h : t <- pre = incSlot inc (forkBlock (successorBlock h)) : h : t

    H.ChainSchema _ vH = arHonest
    H.ChainSchema _ vA = A.uniformAdversarialChain (Just ascA) recipeA' seed
