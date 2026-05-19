{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Consensus.BlockTree.Tests (tests) where

import Data.Function (on)
import qualified Data.List as L
import qualified Data.Map as M
import Ouroboros.Consensus.Block.Abstract (HasHeader, HeaderHash)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Block (blockHash)
import Test.Consensus.BlockTree
import Test.Consensus.Genesis.Setup.GenChains
  ( GenesisTest (..)
  , genChains
  )
import Test.QuickCheck
import qualified Test.QuickCheck as QC
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.TestBlock (TestBlock)

genTestBlockTree :: QC.Gen Word -> QC.Gen (BlockTree TestBlock)
genTestBlockTree = fmap gtBlockTree . genChains

genTestAnchoredFragment :: QC.Gen (AF.AnchoredFragment TestBlock)
genTestAnchoredFragment = fmap btTrunk $ genTestBlockTree (pure 0)

tests :: TestTree
tests =
  let branchFactor = pure 4
   in testGroup
        "BlockTree"
        [ testGroup
            "nonemptyPrefixesOf"
            [ testProperty "nonemptyPrefixesArePrefixes" $
                forAll genTestAnchoredFragment $
                  prop_nonemptyPrefixesOf_nonemptyPrefixesArePrefixes
            , testProperty "nonemptyPrefixesAreNonempty" $
                forAll genTestAnchoredFragment $
                  prop_nonemptyPrefixesOf_nonemptyPrefixesAreNonempty
            , testProperty "nonemptyPrefixesAreUnique" $
                forAll genTestAnchoredFragment $
                  prop_nonemptyPrefixesOf_nonemptyPrefixesAreUnique
            , testProperty "allShareInputAnchor" $
                forAll genTestAnchoredFragment $
                  prop_nonemptyPrefixesOf_allShareInputAnchor
            ]
        , testGroup
            "deforestBlockTree"
            [ testProperty "headPointsAreDistinct" $
                forAll (genTestBlockTree branchFactor) $
                  prop_deforestBlockTree_headPointsAreDistinct
            , testProperty "imagesAreNonempty" $
                forAll (genTestBlockTree branchFactor) $
                  prop_deforestBlockTree_imagesAreNonempty
            , testProperty "allShareTrunkAnchor" $
                forAll (genTestBlockTree branchFactor) $
                  prop_deforestBlockTree_allShareTrunkAnchor
            , testProperty "fullBranchesAreBranches" $
                forAll (genTestBlockTree branchFactor) $
                  prop_deforestBlockTree_fullBranchesAreBranches
            , testProperty "everyHeaderHashIsInTheMap" $
                forAll (genTestBlockTree branchFactor) $
                  prop_deforestBlockTree_everyHeaderHashIsInTheMap
            , testProperty "prefixMaximalPrefixesAreBranches" $
                forAll (genTestBlockTree branchFactor) $
                  prop_deforestBlockTree_prefixMaximalPrefixesAreBranches
            ]
        ]

-- | The nonempty prefixes of an `AF.AnchoredFragment` are in fact prefixes.
prop_nonemptyPrefixesOf_nonemptyPrefixesArePrefixes ::
  (Eq blk, HasHeader blk) => AF.AnchoredFragment blk -> QC.Property
prop_nonemptyPrefixesOf_nonemptyPrefixesArePrefixes fragment =
  QC.property . all (flip AF.isPrefixOf fragment) . nonemptyPrefixesOf $ fragment

-- | The nonempty prefixes of an `AF.AnchoredFragment` are in fact nonempty.
prop_nonemptyPrefixesOf_nonemptyPrefixesAreNonempty ::
  HasHeader blk => AF.AnchoredFragment blk -> QC.Property
prop_nonemptyPrefixesOf_nonemptyPrefixesAreNonempty fragment =
  QC.property . all (not . AF.null) . nonemptyPrefixesOf $ fragment

-- | The nonempty prefixes of an `AF.AnchoredFragment` are unique.
prop_nonemptyPrefixesOf_nonemptyPrefixesAreUnique ::
  forall blk. HasHeader blk => AF.AnchoredFragment blk -> QC.Property
prop_nonemptyPrefixesOf_nonemptyPrefixesAreUnique =
  QC.property . noDuplicates . fmap (fmap blockHash . AF.toOldestFirst) . nonemptyPrefixesOf

noDuplicates :: Ord a => [a] -> Bool
noDuplicates =
  let tally k = M.insertWith (+) k (1 :: Int)
   in all (== 1) . M.elems . foldr tally mempty

-- | All the nonempty prefixes should share the original fragment's anchor.
prop_nonemptyPrefixesOf_allShareInputAnchor ::
  HasHeader blk => AF.AnchoredFragment blk -> QC.Property
prop_nonemptyPrefixesOf_allShareInputAnchor fragment =
  let sharesTrunkAnchor = ((==) `on` AF.anchor) fragment
   in QC.property . all sharesTrunkAnchor . nonemptyPrefixesOf $ fragment

-- | The head points of all the branches are distinct.
-- (Points uniquely determine positions in the tree.)
prop_deforestBlockTree_headPointsAreDistinct ::
  HasHeader blk => BlockTree blk -> QC.Property
prop_deforestBlockTree_headPointsAreDistinct =
  QC.property . noDuplicates . fmap AF.headPoint . M.elems . deforestBlockTree

-- | The deforested branches are all populated.
prop_deforestBlockTree_imagesAreNonempty ::
  BlockTree blk -> QC.Property
prop_deforestBlockTree_imagesAreNonempty =
  QC.property . all (not . AF.null) . deforestBlockTree

-- | All the deforested branches share the trunk's anchor.
prop_deforestBlockTree_allShareTrunkAnchor ::
  HasHeader blk => BlockTree blk -> QC.Property
prop_deforestBlockTree_allShareTrunkAnchor tree =
  let sharesTrunkAnchor = ((==) `on` AF.anchor) (btTrunk tree)
   in QC.property . all sharesTrunkAnchor . deforestBlockTree $ tree

-- | Full branches are in the deforestation.
prop_deforestBlockTree_fullBranchesAreBranches ::
  (Eq blk, HasHeader blk) => BlockTree blk -> QC.Property
prop_deforestBlockTree_fullBranchesAreBranches tree =
  let inDeforestation = flip elem (deforestBlockTree tree)
   in QC.property . all inDeforestation . fmap btbFull . btBranches $ tree

-- | Every block header from the `BlockTree` is in the deforestation map.
prop_deforestBlockTree_everyHeaderHashIsInTheMap ::
  forall blk. HasHeader blk => BlockTree blk -> QC.Property
prop_deforestBlockTree_everyHeaderHashIsInTheMap tree@(BlockTree trunk branches) =
  let
    allBranchHeaderHashes :: BlockTreeBranch blk -> [HeaderHash blk]
    allBranchHeaderHashes (BlockTreeBranch prefix suffix restOfTrunk full) =
      fmap blockHash $ concatMap AF.toOldestFirst [prefix, suffix, restOfTrunk, full]

    allHeaderHashes :: [HeaderHash blk]
    allHeaderHashes =
      fmap blockHash (AF.toOldestFirst trunk)
        <> concatMap allBranchHeaderHashes branches
   in
    QC.property $ all (flip M.member $ deforestBlockTree tree) allHeaderHashes

-- | An `AF.AnchoredFragment` is /prefix maximal/ in a list if it is not a nontrivial
-- prefix of another fragment in the list. After deforesting a tree, the maximal
-- prefixes in the result are precisely the trunk and branches of the tree in
-- some order.
prop_deforestBlockTree_prefixMaximalPrefixesAreBranches ::
  forall blk. (Ord blk, HasHeader blk) => BlockTree blk -> QC.Property
prop_deforestBlockTree_prefixMaximalPrefixesAreBranches tree@(BlockTree trunk branches) =
  QC.property $
    ((==) `on` (L.sort . fmap AF.toOldestFirst))
      (foldr (insertIfMaximalBy AF.isPrefixOf) [] $ deforestBlockTree tree)
      (trunk : fmap btbFull branches)

-- | If @u@ is smaller than any of the elements of @xs@, return @xs@.
-- Otherwise, remove any elements of @xs@ smaller than @u@ and append @u@ to
-- the remainder on the right.
insertIfMaximalBy :: forall u. (u -> u -> Bool) -> u -> [u] -> [u]
insertIfMaximalBy lessThan u =
  let
    go xs = case xs of
      [] -> [u]
      x : rest -> case x `lessThan` u of
        True -> go rest
        False ->
          x
            : case u `lessThan` x of
              True -> rest
              False -> go rest
   in
    go
