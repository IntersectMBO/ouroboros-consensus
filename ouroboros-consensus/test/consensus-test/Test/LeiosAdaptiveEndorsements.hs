-- | Properties of the adaptive-endorsement controller, 'stepAdaptiveEb'.
module Test.LeiosAdaptiveEndorsements (tests) where

import LeiosAdaptiveEndorsements
  ( LeiosAdaptiveEbConfig (..)
  , defaultLeiosAdaptiveEbConfig
  , stepAdaptiveEb
  )
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
  testGroup
    "LeiosAdaptiveEndorsements"
    [ testProperty "scale stays within its bounds" prop_scaleBounded
    , testProperty "quorums made keep the full capacity" prop_noMiss
    , testProperty "persistent misses sink to the minimum" prop_persistentMiss
    , testProperty "a miss cuts and quorums made recover" prop_missRecovers
    ]

prop_scaleBounded :: [Bool] -> Property
prop_scaleBounded misses =
  conjoin
    [ counterexample (show s) $ s >= aebMinScale cfg && s <= 1
    | s <- scanl (flip (stepAdaptiveEb cfg)) 1 misses
    ]
 where
  cfg = defaultLeiosAdaptiveEbConfig

prop_noMiss :: Positive Int -> Property
prop_noMiss (Positive n) =
  iterate (stepAdaptiveEb cfg False) 1 !! n === 1
 where
  cfg = defaultLeiosAdaptiveEbConfig

prop_persistentMiss :: Property
prop_persistentMiss =
  iterate (stepAdaptiveEb cfg True) 1 !! 100 === aebMinScale cfg
 where
  cfg = defaultLeiosAdaptiveEbConfig

-- | One miss takes @(1 - decrease) / increase@ quorums made to undo, and one
-- more for rounding.
prop_missRecovers :: Property
prop_missRecovers =
  counterexample (show (cut, recovered)) $
    cut < 1 .&&. recovered === 1
 where
  cfg = defaultLeiosAdaptiveEbConfig
  cut = stepAdaptiveEb cfg True 1
  steps = ceiling ((1 - aebDecrease cfg) / aebIncrease cfg) :: Int
  recovered = iterate (stepAdaptiveEb cfg False) cut !! (steps + 1)
