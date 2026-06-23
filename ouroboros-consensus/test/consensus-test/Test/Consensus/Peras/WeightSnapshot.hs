{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

#if __GLASGOW_HASKELL__ >= 910
{-# OPTIONS_GHC -Wno-x-partial #-}
#endif

-- | Test that 'PerasWeightSnapshot' can correctly compute the weight of points
-- and fragments.
module Test.Consensus.Peras.WeightSnapshot (tests) where

import Cardano.Ledger.BaseTypes (unNonZero)
import Data.Containers.ListUtils (nubOrd)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Traversable (for)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Config.SecurityParam
import Ouroboros.Consensus.Peras.Weight
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Mock.Chain (Chain)
import qualified Ouroboros.Network.Mock.Chain as Chain
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.Orphans.Arbitrary ()
import Test.Util.QuickCheck
import Test.Util.TestBlock

tests :: TestTree
tests =
  testGroup
    "PerasWeightSnapshot"
    [ testProperty "correctness" prop_perasWeightSnapshot
    ]

prop_perasWeightSnapshot :: TestSetup -> Property
prop_perasWeightSnapshot testSetup =
  tabulate "log₂ # of points" [show $ round @Double @Int $ logBase 2 (fromIntegral (length tsPoints))]
    . counterexample ("PerasWeightSnapshot: " <> show snap)
    $ conjoin
      [ conjoin
          [ counterexample ("Incorrect weight for " <> condense pt) $
              weightBoostOfPointReference pt =:= weightBoostOfPoint snap pt
          | pt <- tsPoints
          ]
      , conjoin
          [ conjoin
              [ counterexample ("Incorrect weight for " <> condense frag) $
                  weightBoostOfFragmentReference frag =:= weightBoostOfFragment snap frag
              , counterexample ("Weight not inductively consistent for " <> condense frag) $
                  prop_fragmentInduction snap frag
              ]
          | frag <- tsFragments
          ]
      , conjoin
          [ conjoin
              [ counterexample ("Incorrect volatile suffix for " <> condense frag) $
                  takeVolatileSuffixReference frag =:= volSuffix
              , counterexample ("Volatile suffix must be a suffix of" <> condense frag) $
                  AF.headPoint frag =:= AF.headPoint volSuffix
                    .&&. AF.withinFragmentBounds (AF.anchorPoint volSuffix) frag
              , counterexample ("A longer volatile suffix still has total weight at most k") $
                  let isImproperSuffix = AF.length volSuffix == AF.length frag
                      fragSuffixOneLonger =
                        AF.anchorNewest (fromIntegral (AF.length volSuffix) + 1) frag
                      weightOneLonger = totalWeightOfFragment snap fragSuffixOneLonger
                   in isImproperSuffix .||. weightOneLonger `gt` maxRollbackWeight tsSecParam
              , counterexample ("Volatile suffix of " <> condense frag <> " must contain at most k blocks") $
                  AF.length volSuffix `le` fromIntegral (unNonZero (maxRollbacks tsSecParam))
              ]
          | frag <- tsFragments
          , let volSuffix = takeVolatileSuffix snap tsSecParam frag
          ]
      ]
 where
  TestSetup
    { tsWeights
    , tsPoints
    , tsFragments
    , tsSecParam
    } = testSetup

  snap = mkPerasWeightSnapshot $ Map.toList tsWeights

  weightBoostOfPointReference :: Point TestBlock -> PerasWeight
  weightBoostOfPointReference pt = Map.findWithDefault mempty pt tsWeights

  weightBoostOfFragmentReference :: AnchoredFragment TestBlock -> PerasWeight
  weightBoostOfFragmentReference frag =
    foldMap
      (weightBoostOfPointReference . blockPoint)
      (AF.toOldestFirst frag)

  takeVolatileSuffixReference ::
    AnchoredFragment TestBlock -> AnchoredFragment TestBlock
  takeVolatileSuffixReference =
    fromJust . find hasWeightAtMostK . suffixes
   where
    -- Consider suffixes of @frag@, longest first
    suffixes frag =
      [ AF.anchorNewest (fromIntegral len) frag
      | len <- reverse [0 .. AF.length frag]
      ]

    hasWeightAtMostK frag =
      totalWeight <= maxRollbackWeight tsSecParam
     where
      weightBoost = weightBoostOfFragmentReference frag
      lengthWeight = PerasWeight (fromIntegral (AF.length frag))
      totalWeight = lengthWeight <> weightBoost

-- | Test that the weight of a fragment is equal to the weight of its
-- first\/last point plus the weight of the remaining suffix\/infix.
prop_fragmentInduction ::
  PerasWeightSnapshot TestBlock ->
  AnchoredFragment TestBlock ->
  Property
prop_fragmentInduction snap =
  \frag -> fromLeft frag .&&. fromRight frag
 where
  fromLeft :: AnchoredFragment TestBlock -> Property
  fromLeft frag = case frag of
    AF.Empty _ ->
      weightBoostOfFragment snap frag === mempty
    b AF.:< frag' ->
      weightBoostOfFragment snap frag
        === weightBoostOfPoint snap (blockPoint b) <> weightBoostOfFragment snap frag'

  fromRight :: AnchoredFragment TestBlock -> Property
  fromRight frag = case frag of
    AF.Empty _ ->
      weightBoostOfFragment snap frag === mempty
    frag' AF.:> b ->
      weightBoostOfFragment snap frag
        === weightBoostOfPoint snap (blockPoint b) <> weightBoostOfFragment snap frag'

data TestSetup = TestSetup
  { tsWeights :: Map (Point TestBlock) PerasWeight
  , tsPoints :: [Point TestBlock]
  -- ^ Check the weight of these points.
  , tsFragments :: [AnchoredFragment TestBlock]
  -- ^ Check the weight of these fragments.
  , tsSecParam :: SecurityParam
  }
  deriving stock Show

instance Arbitrary TestSetup where
  arbitrary = do
    -- Generate a block tree rooted at Genesis.
    tree :: BlockTree <- arbitrary

    let
      -- Points for all blocks in the block tree.
      tsPoints :: [Point TestBlock]
      tsPoints = nubOrd $ GenesisPoint : (blockPoint <$> treeToBlocks tree)

      -- Chains from Genesis to all leaves of the block tree.
      treeChains :: [Chain TestBlock]
      treeChains = treeToChains tree

    -- Randomly boost some points. This might need to be refined in the future
    -- (as per https://github.com/tweag/cardano-peras/issues/124).
    tsWeights :: Map (Point TestBlock) PerasWeight <-
      Map.fromList . catMaybes <$> for tsPoints \pt ->
        fmap (pt,) <$> genWeightBoost

    -- Generate a list of fragments as random infixes of the @treeChains@.
    tsFragments <-
      for treeChains genInfixFragment

    tsSecParam <- arbitrary
    pure
      TestSetup
        { tsWeights
        , tsPoints
        , tsFragments
        , tsSecParam
        }
   where
    -- Generate a weight boost (for some point).
    genWeightBoost :: Gen (Maybe PerasWeight)
    genWeightBoost =
      frequency
        [ (3, pure Nothing)
        , (1, Just . PerasWeight <$> choose (1, 10))
        ]

    -- Given a chain, generate an infix fragment of that chain.
    genInfixFragment :: Chain TestBlock -> Gen (AnchoredFragment TestBlock)
    genInfixFragment chain = do
      let lenChain = Chain.length chain
          fullFrag = Chain.toAnchoredFragment chain
      nTakeNewest <- choose (0, lenChain)
      nDropNewest <- choose (0, nTakeNewest)
      pure $
        AF.dropNewest nDropNewest $
          AF.anchorNewest (fromIntegral nTakeNewest) fullFrag

  shrink ts =
    concat
      [ [ ts{tsWeights = Map.fromList tsWeights'}
        | tsWeights' <-
            shrinkList
              (\(pt, w) -> (pt,) <$> shrinkWeight w)
              $ Map.toList tsWeights
        ]
      , [ ts{tsPoints = tsPoints'}
        | tsPoints' <- shrinkList (\_pt -> []) tsPoints
        ]
      , [ ts{tsFragments = tsFragments'}
        | tsFragments' <- shrinkList (\_frag -> []) tsFragments
        ]
      , [ ts{tsSecParam = tsSecParam'}
        | tsSecParam' <- shrink tsSecParam
        ]
      ]
   where
    -- Decrease by @1@, unless this would mean that it is non-positive.
    shrinkWeight :: PerasWeight -> [PerasWeight]
    shrinkWeight (PerasWeight w)
      | w >= 1 = [PerasWeight (w - 1)]
      | otherwise = []

    TestSetup
      { tsWeights
      , tsPoints
      , tsFragments
      , tsSecParam
      } = ts
