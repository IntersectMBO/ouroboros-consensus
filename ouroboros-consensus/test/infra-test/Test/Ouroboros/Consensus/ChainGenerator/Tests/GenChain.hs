{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Consensus.ChainGenerator.Tests.GenChain (module Test.Ouroboros.Consensus.ChainGenerator.Tests.GenChain) where

import           Cardano.Slotting.Slot (SlotNo)
import           Data.List (foldl')
import qualified Data.Vector.Unboxed as Vector
import           Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import qualified Test.Ouroboros.Consensus.ChainGenerator.Adversarial as A
import           Test.Ouroboros.Consensus.ChainGenerator.Counting
                     (Count (Count), getVector)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Honest as H
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Asc)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Slot as S
import           Test.Ouroboros.Consensus.ChainGenerator.Slot (S)
import qualified Test.Ouroboros.Consensus.ChainGenerator.Tests.BlockTree as BT
import           Test.QuickCheck.Random (QCGen)
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock, TestBlockWith (tbSlot),
                     firstBlock, forkBlock, successorBlock)

genChains ::
  Asc ->
  A.AdversarialRecipe base hon ->
  A.SomeCheckedAdversarialRecipe base hon ->
  QCGen ->
  BT.BlockTree TestBlock
genChains ascA A.AdversarialRecipe {A.arHonest, A.arPrefix = Count prefixCount} (A.SomeCheckedAdversarialRecipe _ recipeA') seed =
  BT.addBranch' badChain $ BT.mkTrunk goodChain
  where
    goodChain = mkTestFragment goodBlocks

    badChain = mkTestFragment (mkTestBlocks False prefix slotsA)

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
