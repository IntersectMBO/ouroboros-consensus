-- | Properties of the adaptive-endorsement controller, 'stepAdaptiveEb'.
module Test.LeiosAdaptiveEndorsements (tests) where

import LeiosAdaptiveEndorsements
  ( AdaptiveEbState (..)
  , LeiosAdaptiveEbConfig (..)
  , defaultLeiosAdaptiveEbConfig
  , initialAdaptiveEbState
  , lostMarginShare
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
    , testProperty "no lost margin keeps the full capacity" prop_noLoss
    , testProperty "a persistent loss holds a fixed cut" prop_persistentLoss
    , testProperty "the lost margin share is within [-1, 0]" prop_lostMarginShareBounded
    , testProperty "no margin left reads as all of it lost" prop_noMargin
    ]

newtype Stake = Stake Double
  deriving Show

instance Arbitrary Stake where
  arbitrary = Stake <$> choose (0, 1)

prop_scaleBounded :: [Stake] -> Property
prop_scaleBounded es =
  conjoin
    [ counterexample (show st) $
        aebScale st >= aebMinScale cfg && aebScale st <= 1
    | st <- scanl (flip (stepAdaptiveEb cfg)) initialAdaptiveEbState [negate e | Stake e <- es]
    ]
 where
  cfg = defaultLeiosAdaptiveEbConfig

prop_noLoss :: Positive Int -> Property
prop_noLoss (Positive n) =
  aebScale (iterate (stepAdaptiveEb cfg 0) initialAdaptiveEbState !! n) === 1
 where
  cfg = defaultLeiosAdaptiveEbConfig

-- | Without an I term, the capacity settles at @exp (-gainP * loss)@ rather
-- than sinking to its minimum.
prop_persistentLoss :: Stake -> Property
prop_persistentLoss (Stake loss) =
  counterexample (show (settled, expected)) $ abs (settled - expected) < 1e-6
 where
  cfg = defaultLeiosAdaptiveEbConfig
  settled = aebScale (iterate (stepAdaptiveEb cfg (negate loss)) initialAdaptiveEbState !! 1000)
  expected = max (aebMinScale cfg) (exp (negate (aebGainP cfg) * loss))

prop_lostMarginShareBounded :: Stake -> Stake -> Stake -> Property
prop_lostMarginShareBounded (Stake threshold) (Stake alive) (Stake participation) =
  participation <= alive ==>
    let e = lostMarginShare threshold alive participation
     in counterexample (show e) $ e >= -1 && e <= 0

prop_noMargin :: Stake -> Stake -> Property
prop_noMargin (Stake alive) (Stake participation) =
  forAll (choose (alive, 1)) $ \threshold ->
    lostMarginShare threshold alive participation === -1
