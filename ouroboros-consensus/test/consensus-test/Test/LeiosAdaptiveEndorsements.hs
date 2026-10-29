-- | Properties of the adaptive-endorsement controller, 'stepAdaptiveEb'.
module Test.LeiosAdaptiveEndorsements (tests) where

import LeiosAdaptiveEndorsements
  ( AdaptiveEbState (..)
  , LeiosAdaptiveEbConfig (..)
  , defaultLeiosAdaptiveEbConfig
  , initialAdaptiveEbState
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
    , testProperty "full participation keeps the full capacity" prop_fullParticipation
    , testProperty "a persistent shortfall holds a fixed cut" prop_persistentShortfall
    ]

newtype Participation = Participation Double
  deriving Show

instance Arbitrary Participation where
  arbitrary = Participation <$> choose (0, 1)

prop_scaleBounded :: [Participation] -> Property
prop_scaleBounded ps =
  conjoin
    [ counterexample (show st) $
        aebScale st >= aebMinScale cfg && aebScale st <= 1
    | st <- scanl (flip (stepAdaptiveEb cfg)) initialAdaptiveEbState [p | Participation p <- ps]
    ]
 where
  cfg = defaultLeiosAdaptiveEbConfig

prop_fullParticipation :: Positive Int -> Property
prop_fullParticipation (Positive n) =
  aebScale (iterate (stepAdaptiveEb cfg 1) initialAdaptiveEbState !! n) === 1
 where
  cfg = defaultLeiosAdaptiveEbConfig

-- | Without an I term, the capacity settles at @exp (-gainP * shortfall)@
-- rather than sinking to its minimum.
prop_persistentShortfall :: Property
prop_persistentShortfall =
  forAll (choose (0, 0.4)) $ \shortfall ->
    let participation = aebTargetParticipation cfg - shortfall
        settled = aebScale (iterate (stepAdaptiveEb cfg participation) initialAdaptiveEbState !! 500)
        expected = max (aebMinScale cfg) (exp (negate (aebGainP cfg) * shortfall))
     in counterexample (show (settled, expected)) $ abs (settled - expected) < 1e-6
 where
  cfg = defaultLeiosAdaptiveEbConfig
