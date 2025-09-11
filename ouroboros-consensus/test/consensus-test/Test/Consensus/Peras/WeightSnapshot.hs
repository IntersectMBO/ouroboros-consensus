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

import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Traversable (for)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Peras.Weight
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Network.AnchoredFragment (AnchoredFragment)
import qualified Ouroboros.Network.AnchoredFragment as AF
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
          [ counterexample ("Incorrect weight for " <> condense frag) $
              weightBoostOfFragmentReference frag =:= weightBoostOfFragment snap frag
          | frag <- tsFragments
          ]
      ]
 where
  TestSetup
    { tsWeights
    , tsPoints
    , tsFragments
    } = testSetup

  snap = mkPerasWeightSnapshot $ Map.toList tsWeights

  weightBoostOfPointReference :: Point TestBlock -> PerasWeight
  weightBoostOfPointReference pt = Map.findWithDefault mempty pt tsWeights

  weightBoostOfFragmentReference :: AnchoredFragment TestBlock -> PerasWeight
  weightBoostOfFragmentReference frag =
    foldMap
      (weightBoostOfPointReference . blockPoint)
      (AF.toOldestFirst frag)

data TestSetup = TestSetup
  { tsWeights :: Map (Point TestBlock) PerasWeight
  , tsPoints :: [Point TestBlock]
  -- ^ Check the weight of these points.
  , tsFragments :: [AnchoredFragment TestBlock]
  -- ^ Check the weight of these fragments.
  }
  deriving stock Show

instance Arbitrary TestSetup where
  arbitrary = do
    tree :: BlockTree <- arbitrary
    let tsPoints = nubOrd $ GenesisPoint : (blockPoint <$> treeToBlocks tree)
        treeChains = treeToChains tree
    tsWeights <- do
      boostedChain <- elements treeChains
      let boostablePts =
            GenesisPoint : (blockPoint <$> Chain.toOldestFirst boostedChain)
      Map.fromList . catMaybes <$> for boostablePts \pt -> do
        weight <-
          frequency
            [ (3, pure Nothing)
            , (1, Just . PerasWeight <$> choose (1, 10))
            ]
        pure $ (pt,) <$> weight
    tsFragments <- for treeChains \chain -> do
      let lenChain = Chain.length chain
          fullFrag = Chain.toAnchoredFragment chain
      nTakeNewest <- choose (0, lenChain)
      nDropNewest <- choose (0, nTakeNewest)
      pure $
        AF.dropNewest nDropNewest $
          AF.anchorNewest (fromIntegral nTakeNewest) fullFrag
    pure
      TestSetup
        { tsWeights
        , tsPoints
        , tsFragments
        }

  shrink ts =
    concat
      [ [ ts{tsWeights = Map.fromList tsWeights'}
        | tsWeights' <-
            shrinkList
              -- Shrink boosted points to have weight 1.
              (\(pt, w) -> [(pt, w1) | w1 /= w])
              $ Map.toList tsWeights
        ]
      , [ ts{tsPoints = tsPoints'}
        | tsPoints' <- shrinkList (\_pt -> []) tsPoints
        ]
      , [ ts{tsFragments = tsFragments'}
        | tsFragments' <- shrinkList (\_frag -> []) tsFragments
        ]
      ]
   where
    w1 = PerasWeight 1

    TestSetup
      { tsWeights
      , tsPoints
      , tsFragments
      } = ts
