module Test.Consensus.Leios.LeiosFetch.Retry (tests) where

import Control.Monad.Class.MonadTime.SI (Time (..), addTime)
import Data.List (foldl', mapAccumL)
import Data.Ratio ((%))
import Data.Word (Word32)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Retry
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

config :: RetryConfig
config = defaultRetryConfig

initial :: RetryState
initial = newRetryState config

failAt :: Int -> RetryState -> RetryState
failAt n = recordRetryFailure config (Time (fromIntegral n))

failed :: Int -> RetryState
failed n = foldl' (flip failAt) initial [1 .. n]

near :: Rational -> Double -> Assertion
near actual expected =
  assertBool
    (show (fromRational actual :: Double, expected))
    (abs (fromRational actual - expected) < 1e-11 * max 1 (abs expected))

tests :: TestTree
tests =
  testGroup
    "closure retry"
    [ testCase "defaults start without pressure, failures or cooldown" $
        retrySnapshot config initial @?= RetrySnapshot 1 0 1 Nothing
    , testCase "each actual rescue failure grows by 1.5 and saturates at sixteen" $ do
        mapM_ (\n -> near (retryFactor config (failed n)) (min 16 (1.5 ^ n))) [1 .. 10 :: Int]
        retryFactor config (failed 10000) @?= 16
    , testCase "partial useful service recovers in proportion to nominal checkpoint bytes" $ do
        near (retryFactor config (recoverRetryService config 20000 10000 (failed 1))) (1.5 * sqrt 0.7)
        near (retryFactor config (recoverRetryService config 20000 20000 (failed 1))) 1.05
    , testCase "underfilled and overhanging batches credit actual payload" $ do
        near
          (retryFactor config (recoverRetryService config 20000 5000 (failed 4)))
          (1.5 ^ (4 :: Int) * 0.7 ** 0.25)
        near
          (retryFactor config (recoverRetryService config 20000 24000 (failed 4)))
          (1.5 ^ (4 :: Int) * 0.7 ** 1.2)
    , testCase "splitting a useful delivery into TX callbacks gives the same recovery" $ do
        let one = recoverRetryService config 20000 10000 (failed 4)
            split = recoverRetryService config 20000 7000 (recoverRetryService config 20000 3000 (failed 4))
        near (retryFactor config split) (fromRational (retryFactor config one))
    , testCase "recovery stops at one and zero-byte observations are inert" $ do
        let before = failed 3
        recoverRetryService config 20000 0 before @?= before
        recoverRetryService config 0 1000 before @?= before
        retryFactor config (recoverRetryService config 1 maxBound before) @?= 1
    , testCase "five-failure groups cool down for one, two, four, eight and sixteen seconds" $ do
        let group s n =
              let now = Time (fromIntegral (n * 100))
                  next = iterate (recordRetryFailure config now) (advanceRetry now s) !! 5
                  snap = retrySnapshot config next
               in (next, (snapshotFailuresInGroup snap, snapshotCooldownUntil snap, snapshotNextCooldown snap))
            (_, groups) = mapAccumL group initial [1 .. 7 :: Int]
        groups
          @?= [ (0, Just (Time (fromIntegral (100 * n + duration))), fromIntegral next)
              | (n, duration, next) <-
                  zip3 [1 .. 7 :: Int] [1, 2, 4, 8, 16, 16, 16 :: Int] [2, 4, 8, 16, 16, 16, 16 :: Int]
              ]
    , testCase "useful service changes neither grouped history nor an absolute cooldown" $ do
        let before = failed 5
            after = recoverRetryService config 20000 1000000 before
        retryFactor config after @?= 1
        snapshotCooldownUntil (retrySnapshot config after) @?= Just (Time 6)
        snapshotNextCooldown (retrySnapshot config after) @?= 2
        let twoMore = failAt 8 (failAt 7 after)
            recovered = recoverRetryService config 20000 20000 twoMore
        snapshotFailuresInGroup (retrySnapshot config recovered) @?= 2
    , testCase "cooldown is eligible at its exact expiry and is reconciled once" $ do
        let state = failed 5
        retryEligible (Time 5.9) state @?= False
        nextRetryDeadline (Time 5.9) state @?= Just (Time 6)
        retryEligible (Time 6) state @?= True
        nextRetryDeadline (Time 6) state @?= Nothing
        let finished = advanceRetry (Time 6) state
        snapshotCooldownUntil (retrySnapshot config finished) @?= Nothing
        advanceRetry (Time 7) finished @?= finished
        advanceRetry (Time 5.9) state @?= state
    , testCase "cooldown starts at handling time, not a request's earlier deadline" $ do
        let state = iterate (recordRetryFailure config (Time 1)) initial !! 4
            handledLate = recordRetryFailure config (Time 10) state
        nextRetryDeadline (Time 10) handledLate @?= Just (addTime 1 (Time 10))
    , testCase "local policy settings can change group size, cap and recovery" $ do
        let custom = right (mkRetryConfig 4 2 (1 % 2) 2 0.25 1)
            twice = recordRetryFailure custom (Time 1) (recordRetryFailure custom (Time 0) (newRetryState custom))
        retrySnapshot custom twice @?= RetrySnapshot 4 0 0.5 (Just (Time 1.25))
        near (retryFactor custom (recoverRetryService custom 1000 500 twice)) (4 * sqrt 0.5)
    , testCase "invalid, zero and unrepresentable settings are rejected" $ do
        mapM_
          (\(cap, grow, recover) -> mkRetryConfig cap grow recover 5 1 16 @?= Left InvalidRetryFactors)
          [ (0, 1.5, 0.7)
          , (16, 1, 0.7)
          , (16, 1.5, 0)
          , (16, 1.5, 1)
          , (10 ^ (1000 :: Int), 1.5, 0.7)
          , (16, 1.5, 1 % (10 ^ (1000 :: Int)))
          ]
        mapM_
          (\(group, start, cap) -> mkRetryConfig 16 1.5 0.7 group start cap @?= Left InvalidCooldown)
          [(0, 1, 16), (5, 0, 16), (5, 2, 1)]
    , testProperty "long mixed schedules keep finite bounded factors and grouped counters" $ \events ->
        let states = scanl step initial (take 1000 (events :: [Word32]))
            valid s =
              let snap = retrySnapshot config s
               in snapshotRetryFactor snap >= 1
                    && snapshotRetryFactor snap <= 16
                    && snapshotFailuresInGroup snap < 5
                    && snapshotNextCooldown snap >= 1
                    && snapshotNextCooldown snap <= 16
         in counterexample (show states) (all valid states)
    ]
 where
  step s n
    | even n = recordRetryFailure config (Time (fromIntegral n)) s
    | otherwise = recoverRetryService config 20000 n s
  right = either (error . show) id
