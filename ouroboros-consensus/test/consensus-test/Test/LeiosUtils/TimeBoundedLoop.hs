{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.LeiosUtils.TimeBoundedLoop (tests) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IOSim (runSimOrThrow)
import LeiosUtils.TimeBoundedLoop
  ( propFoldEmptyListTerminates
  , propFoldEndReachedIffConsumedAll
  , propFoldJustFoldl
  , propFoldStepsMatchConsumed
  , propIterImmediateEndOnInit
  , propIterTimeoutOverrunBoundedByOneStep
  )
import Ouroboros.Consensus.Util.IOLike (IOLike)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual)
import Test.Tasty.QuickCheck
  ( Arbitrary
  , NonNegative (NonNegative)
  , Positive (..)
  , Small (..)
  , Testable (property)
  , counterexample
  , ioProperty
  , label
  , testProperty
  )
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "TimeBoundedLoop"
    [ testGroup
        "foldOrTimeout"
        [ testPropertyIOLike
            "(val, remaining, steps, done) <- propFoldEmptyListTerminates ttl sumAction initSt"
            (\(ttl, _initSt) ret -> labelTtl ttl <> ", " <> labelFoldRet ret)
            $ \(NonNegative (Small (ttl :: Int)), Small initSt) -> propFoldEmptyListTerminates (fromIntegral ttl) sumAction (getSmall initSt)
        , testPropertyIOLike
            "(val, remaining, steps, done) <- propFoldStepsMatchConsumed ttl sumAction initSt xs"
            labelFoldProp
            $ \(NonNegative (Small (ttl :: Int)), xs :: [Small Int], Small initSt) ->
              propFoldStepsMatchConsumed
                (fromIntegral ttl)
                sumAction
                (getSmall initSt)
                (getSmall <$> xs)
        , testPropertyIOLike
            "(val, remaining, steps, done) <- propFoldEndReachedIffConsumedAll ttl sumAction initSt xs"
            labelFoldProp
            $ \(NonNegative (Small (ttl :: Int)), xs :: [Small Int], Small initSt) ->
              propFoldEndReachedIffConsumedAll
                (fromIntegral ttl)
                sumAction
                (getSmall initSt)
                (getSmall <$> xs)
        , testPropertyIOLike
            "(val, remaining, steps, done) <- propFoldJustFoldl ttl (+) initSt xs"
            labelFoldProp
            $ \(NonNegative (Small (ttl :: Int)), xs :: [Small Int], Small initSt) ->
              propFoldJustFoldl
                (fromIntegral ttl)
                (+)
                (getSmall initSt)
                (getSmall <$> xs)
        ]
    , testGroup
        "iterateUntilOrTimeout"
        [ testPropertyIOLike
            "(val, steps, done) <- propIterImmediateEndOnInit ttl countAction initSt"
            (\(ttl, _initSt) ret -> labelTtl ttl <> ", " <> labelIterRet ret)
            $ \(NonNegative (Small (ttl :: Int)), Small initSt) ->
              propIterImmediateEndOnInit (fromIntegral ttl) countAction initSt
        , testPropertyIOSim
            "(val, steps, done) <- propIterTimeoutOverrunBoundedByOneStep ttl delay"
            ( \(ttl, stepDelay) ret -> labelTtl ttl <> ", " <> labelDelayWithTtl stepDelay ttl <> ", " <> labelIterRet2 ret
            )
            ( \(NonNegative (Small (ttl :: Int)), Positive (Small (stepDelay :: Int))) ->
                -- NOTE(bladyjoker): stepDelay must be bigger then zero because IOSim doesn't advance the clock in pure computations and then this hangs.
                -- ghci> import Control.Monad.Class.MonadTime
                -- ghci> import Control.Monad.IOSim (runSimOrThrow)
                -- ghci> :set +s
                -- ghci> runSimOrThrow $ do { start <- getMonotonicTimeNSec; !x <- pure $ sum [1..100000000]; end <- getMonotonicTimeNSec; return (x, end - start) }
                -- (5000000050000000,0)
                -- (1.31 secs, 8,800,104,744 bytes)
                propIterTimeoutOverrunBoundedByOneStep (fromIntegral ttl) (fromIntegral stepDelay)
            )
        ]
    ]
 where
  sumAction :: Monad m => Int -> Int -> m Int
  sumAction total x = return (total + x)

  countAction :: Monad m => Int -> m Int
  countAction total = return (total + 1)

  labelTtl (NonNegative (Small ttl)) | ttl == 0 = "ttl = 0"
  labelTtl _ = "ttl > 0"

  labelDelayWithTtl (Positive (Small delay)) (NonNegative (Small ttl)) | delay > ttl = "delay > ttl"
  labelDelayWithTtl (Positive (Small delay)) (NonNegative (Small ttl)) | delay < ttl = "delay < ttl"
  labelDelayWithTtl (Positive (Small delay)) (NonNegative (Small ttl)) | delay == ttl = "delay = ttl"
  labelDelayWithTtl _ _ = "label = impossible"

  labelDone done = "done = " <> show done

  labelSteps steps | steps == 0 = "steps = 0"
  labelSteps _ = "steps > 0"

  labelIterRes (_val, steps, done) = labelSteps steps <> ", " <> labelDone done

  labelIterRet (Left (failReason, res)) = failReason <> ", " <> labelIterRes res
  labelIterRet (Right res) = labelIterRes res

  labelIterRet2 (Left (failReason, (_, res))) = failReason <> ", " <> labelIterRes res
  labelIterRet2 (Right res) = labelIterRes res

  labelFoldProp (ttl, xs, _initSt) ret = labelTtl ttl <> ", " <> labelXs xs <> ", " <> labelFoldRet ret

  labelXs [] = "xs = empty"
  labelXs _ = "xs = non-empty"

  labelRemaining [] = "remaining = empty"
  labelRemaining _ = "remaining = non-empty"

  labelFoldRes (_, remaining, steps, done) = labelRemaining remaining <> ", " <> labelSteps steps <> ", " <> labelDone done

  labelFoldRet (Left (failReason, res)) = failReason <> ", " <> labelFoldRes res
  labelFoldRet (Right res) = labelFoldRes res

testPropertyIOLike ::
  (Arbitrary a, Show a, Show e, Eq e, Eq b, Show b) =>
  String ->
  (a -> (Either e b) -> String) ->
  (forall m. IOLike m => a -> ExceptT e m b) ->
  TestTree
testPropertyIOLike lbl lblInAndOut prop =
  testGroup
    lbl
    [ testProperty "IOSim + IO" $ \x -> ioProperty $ do
        let succOrFailSim = runSimOrThrow $ runExceptT (prop x)
        succOrFailIO <- runExceptT (prop x)
        assertEqual "IO and IOSim results must be the same" succOrFailSim succOrFailIO
        pure $
          counterexample
            (show succOrFailSim)
            (label (lblInAndOut x succOrFailSim) $ property $ either (const False) (const True) succOrFailSim)
    ]

testPropertyIOSim ::
  (Arbitrary a, Show a, Show e, Show b) =>
  String ->
  (a -> (Either e b) -> String) ->
  (forall m. IOLike m => a -> ExceptT e m b) ->
  TestTree
testPropertyIOSim lbl lblInAndOut prop =
  testGroup
    lbl
    [ testProperty "IOSim" $ \x ->
        let succOrFail = runSimOrThrow $ runExceptT (prop x)
         in counterexample
              (show succOrFail)
              (label (lblInAndOut x succOrFail) $ property $ either (const False) (const True) succOrFail)
    ]
