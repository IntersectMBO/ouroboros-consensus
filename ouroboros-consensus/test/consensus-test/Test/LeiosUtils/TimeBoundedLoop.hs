{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.LeiosUtils.TimeBoundedLoop (tests) where

import Control.Monad.IOSim (runSimOrThrow)
import LeiosUtils.TimeBoundedLoop
  ( DoneType (..)
  , iterateUntilOrTimeout
  , propImmediateEndOnInit
  , propTimeoutOverrunBoundedByOneStep
  )
import Ouroboros.Consensus.Util.IOLike (IOLike)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.QuickCheck (Arbitrary, Positive (..), Small (..), ioProperty, testProperty)
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "TimeBoundedLoop"
    [ testIOLike
        "predicate holds after exactly 3 steps → EndReached, 3 steps, correct final state"
        (iterateUntilOrTimeout 10.0 (>= 3) (pure . (+ 1)) (0 :: Int))
        ( \(finalSt, steps, done) -> do
            done @?= EndReached
            steps @?= 3
            finalSt @?= 3
        )
    , testGroup
        "propImmediateEndOnInit"
        [ testIOLike
            "fixed ttl=1.0"
            (propImmediateEndOnInit 1.0 pure ())
            (@?= True)
        , testPropertyIOLike "any positive ttl" $
            \(Positive (Small (n :: Int))) ->
              propImmediateEndOnInit (fromIntegral n) pure ()
        ]
    , testGroup
        "propTimeoutOverrunBoundedByOneStep [IOSim]"
        [ testCase "ttl = 0" $
            runSimOrThrow (propTimeoutOverrunBoundedByOneStep 0 0.1) @?= True
        , testCase "step < ttl" $
            runSimOrThrow (propTimeoutOverrunBoundedByOneStep 0.1 0.05) @?= True
        , testCase "step > ttl" $
            runSimOrThrow (propTimeoutOverrunBoundedByOneStep 0.05 0.1) @?= True
        , testProperty "any positive ttl and stepDelay" $
            \(Positive (Small (ttlN :: Int))) (Positive (Small (delayN :: Int))) ->
              runSimOrThrow (propTimeoutOverrunBoundedByOneStep (fromIntegral ttlN) (fromIntegral delayN))
        ]
    ]

-- | Run a test action in both IOSim and IO.
testIOLike :: String -> (forall m. IOLike m => m a) -> (a -> Assertion) -> TestTree
testIOLike lbl act ass =
  testGroup
    lbl
    [ testCase "IOSim" $ ass (runSimOrThrow act)
    , testCase "IO" $ act >>= ass
    ]

-- | Run a QuickCheck property in both IOSim and IO.
testPropertyIOLike ::
  (Arbitrary a, Show a) =>
  String ->
  (forall m. IOLike m => a -> m Bool) ->
  TestTree
testPropertyIOLike lbl prop =
  testGroup
    lbl
    [ testProperty "IOSim" $ \a -> runSimOrThrow (prop a)
    , testProperty "IO" $ \a -> ioProperty (prop a :: IO Bool)
    ]
