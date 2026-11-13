module Test.Consensus.Leios.LeiosFetch.PeerPenalty (tests) where

import Control.Monad.Class.MonadTime.SI (Time (..))
import Data.List (foldl')
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.PeerPenalty
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

one :: Penalty
one = chargePenalty (Time 0) emptyPenalty

tests :: TestTree
tests =
  testGroup
    "peer penalty aging"
    [ testCase "empty evidence stays unpenalized" $ do
        effectiveScore (Time 1000000) emptyPenalty @?= zeroScore
        penaltySnapshot emptyPenalty @?= Snapshot zeroScore Nothing
    , testCase "isolated stall is forgiven at its inclusive ten-minute boundary" $ do
        assertBool "not early" (scoreValue (effectiveScore (Time 599.999) one) > 0.5)
        effectiveScore (Time 600) one @?= zeroScore
        effectiveScore (Time 600.001) one @?= zeroScore
        -- Even repeated queries do not renew the baseline.
        map (`effectiveScore` one) [Time 0, Time 300, Time 600]
          @?= [effectiveScore (Time 0) one, effectiveScore (Time 300) one, zeroScore]
        savedAt (penaltySnapshot one) @?= Just (Time 0)
    , testCase "accumulated evidence takes longer to forgive" $ do
        let eight = iterate (chargePenalty (Time 0)) emptyPenalty !! 8
        scoreValue (effectiveScore (Time 600) eight) @?= 4
        scoreValue (effectiveScore (Time 1200) eight) @?= 2
        scoreValue (effectiveScore (Time 1800) eight) @?= 1
        effectiveScore (Time 2400) eight @?= zeroScore
    , testCase "fresh charge ages old evidence before adding one" $ do
        let two = chargePenalty (Time 0) one
            next = chargePenalty (Time 600) two
        scoreValue (savedScore (penaltySnapshot next)) @?= 2
        savedAt (penaltySnapshot next) @?= Just (Time 600)
        let afterForgiveness = chargePenalty (Time 600) one
        scoreValue (savedScore (penaltySnapshot afterForgiveness)) @?= 1
        assertBool
          "new stall does not inherit the old expiry"
          (effectiveScore (Time 1199) afterForgiveness > zeroScore)
        effectiveScore (Time 1200) afterForgiveness @?= zeroScore
    , testCase "stale reads cannot amplify and stale charges cannot rewind" $ do
        let first = chargePenalty (Time 100) emptyPenalty
            stale = chargePenalty (Time 50) first
        scoreValue (effectiveScore (Time 0) first) @?= 1
        scoreValue (savedScore (penaltySnapshot stale)) @?= 2
        savedAt (penaltySnapshot stale) @?= Just (Time 100)
        scoreValue (effectiveScore (Time 700) stale) @?= 1
    , testCase "continued repeated stalls retain evidence" $ do
        let persistent = foldl' (flip chargePenalty) emptyPenalty [Time (fromIntegral (n * 60)) | n <- [0 .. 20 :: Int]]
        assertBool
          "frequent defection is not erased"
          (scoreValue (effectiveScore (Time 1200) persistent) > 10)
    , testProperty "arbitrary charge times keep evidence finite and its baseline monotone" $
        forAll (resize 100 (listOf (chooseInt (0, 100000)))) $ \times ->
          let states = scanl (\p t -> chargePenalty (Time (fromIntegral t)) p) emptyPenalty times
              snapshots = map penaltySnapshot (drop 1 states)
              values = map (scoreValue . savedScore) snapshots
              baselines = map savedAt snapshots
           in conjoin
                [ counterexample
                    "finite bounded charge evidence"
                    (all (\v -> not (isNaN v || isInfinite v) && v >= 1 && v <= fromIntegral (length times)) values)
                , counterexample "monotone baseline" (and (zipWith (<=) baselines (drop 1 baselines)))
                , counterexample
                    "a pure read never increases stored evidence"
                    ( all
                        (\p -> scoreValue (effectiveScore (Time 50000) p) <= scoreValue (savedScore (penaltySnapshot p)))
                        states
                    )
                ]
    ]
