{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.LeiosUtils.TimeBoundedLoop (tests) where

import Control.Monad.Class.MonadTimer.SI (DiffTime, MonadDelay (threadDelay))
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
import Test.Tasty.QuickCheck
  ( Arbitrary (..)
  , NonNegative (NonNegative)
  , Small (..)
  , Testable (property)
  , chooseInt
  , counterexample
  , ioProperty
  , label
  , testProperty
  , (.&&.)
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
            ( \(ttl, delay, _initSt) ret -> labelTtl ttl <> ", " <> labelDelayWithTtl delay ttl <> ", " <> labelFoldRet ret
            )
            $ \(NonNegative (Small (ttl :: Int)), delay :: DelayUs, Small initSt) ->
              propFoldEmptyListTerminates (ms ttl) (sumAction delay) (getSmall initSt)
        , testPropertyIOLike
            "(val, remaining, steps, done) <- propFoldStepsMatchConsumed ttl (sumAction delay) initSt xs"
            labelFoldProp
            $ \(NonNegative (Small (ttl :: Int)), delay :: DelayUs, xs :: [Small Int], Small initSt) ->
              propFoldStepsMatchConsumed
                (ms ttl)
                (sumAction delay)
                (getSmall initSt)
                (getSmall <$> xs)
        , testPropertyIOLike
            "(val, remaining, steps, done) <- propFoldEndReachedIffConsumedAll ttl (sumAction delay) initSt xs"
            labelFoldProp
            $ \(NonNegative (Small (ttl :: Int)), delay :: DelayUs, xs :: [Small Int], Small initSt) ->
              propFoldEndReachedIffConsumedAll
                (ms ttl)
                (sumAction delay)
                (getSmall initSt)
                (getSmall <$> xs)
        , testPropertyIOLike
            "(val, remaining, steps, done) <- propFoldJustFoldl ttl (+) initSt xs"
            (\(ttl, xs, _initSt) ret -> labelTtl ttl <> ", " <> labelXs xs <> ", " <> labelFoldRet ret)
            $ \(NonNegative (Small (ttl :: Int)), xs :: [Small Int], Small initSt) ->
              propFoldJustFoldl
                (ms ttl)
                (+)
                (getSmall initSt)
                (getSmall <$> xs)
        ]
    , testGroup
        "iterateUntilOrTimeout"
        [ testPropertyIOLike
            "(val, steps, done) <- propIterImmediateEndOnInit ttl (countAction delay) initSt"
            ( \(ttl, delay, _initSt) ret -> labelTtl ttl <> ", " <> labelDelayWithTtl delay ttl <> ", " <> labelIterRet ret
            )
            $ \(NonNegative (Small (ttl :: Int)), delay :: DelayUs, Small initSt) ->
              propIterImmediateEndOnInit (ms ttl) (countAction delay) initSt
        , testPropertyIOSim
            "(val, steps, done) <- propIterTimeoutOverrunBoundedByOneStep ttl delay"
            ( \(ttl, stepDelay) ret -> labelTtl ttl <> ", " <> labelDelayWithTtl stepDelay ttl <> ", " <> labelIterRet2 ret
            )
            ( \(NonNegative (Small (ttl :: Int)), stepDelay :: DelayUs) ->
                -- NOTE(bladyjoker): stepDelay must be bigger then zero because IOSim doesn't advance the clock in pure computations and then this hangs.
                -- ghci> import Control.Monad.Class.MonadTime
                -- ghci> import Control.Monad.IOSim (runSimOrThrow)
                -- ghci> :set +s
                -- ghci> runSimOrThrow $ do { start <- getMonotonicTimeNSec; !x <- pure $ sum [1..100000000]; end <- getMonotonicTimeNSec; return (x, end - start) }
                -- (5000000050000000,0)
                -- (1.31 secs, 8,800,104,744 bytes)
                propIterTimeoutOverrunBoundedByOneStep (ms ttl) (us . unDelayUs $ stepDelay)
            )
        ]
    ]
 where
  ms :: Int -> DiffTime
  ms n = fromIntegral n / 1000

  us :: Int -> DiffTime
  us n = fromIntegral n / 1_000_000

  sumAction :: MonadDelay m => DelayUs -> Int -> Int -> m Int
  sumAction (DelayUs delay) total x = do
    threadDelay (us delay)
    return (total + x)

  countAction :: MonadDelay m => DelayUs -> Int -> m Int
  countAction (DelayUs delay) total = do
    threadDelay (us delay)
    return (total + 1)

  labelTtl (NonNegative (Small ttl)) | ttl == 0 = "ttl = 0"
  labelTtl _ = "ttl > 0"

  labelDelayWithTtl (DelayUs delay) (NonNegative (Small ttl)) | delay > ttl = "delay > ttl"
  labelDelayWithTtl (DelayUs delay) (NonNegative (Small ttl)) | delay < ttl = "delay < ttl"
  labelDelayWithTtl (DelayUs delay) (NonNegative (Small ttl)) | delay == ttl = "delay = ttl"
  labelDelayWithTtl _ _ = "label = impossible"

  labelDone done = "done = " <> show done

  labelSteps steps | steps == 0 = "steps = 0"
  labelSteps _ = "steps > 0"

  labelIterRes (_val, steps, done) = labelSteps steps <> ", " <> labelDone done

  labelIterRet (Left (failReason, res)) = failReason <> ", " <> labelIterRes res
  labelIterRet (Right res) = labelIterRes res

  labelIterRet2 (Left (failReason, (_, res))) = failReason <> ", " <> labelIterRes res
  labelIterRet2 (Right res) = labelIterRes res

  labelFoldProp (ttl, delay, xs, _initSt) ret =
    labelTtl ttl
      <> ", "
      <> labelDelayWithTtl delay ttl
      <> ", "
      <> labelXs xs
      <> ", "
      <> labelFoldRet ret

  labelXs [] = "xs = empty"
  labelXs _ = "xs = non-empty"

  labelRemaining [] = "remaining = empty"
  labelRemaining _ = "remaining = non-empty"

  labelFoldRes (_, remaining, steps, done) = labelRemaining remaining <> ", " <> labelSteps steps <> ", " <> labelDone done

  labelFoldRet (Left (failReason, res)) = failReason <> ", " <> labelFoldRes res
  labelFoldRet (Right res) = labelFoldRes res

testPropertyIOLike ::
  (Arbitrary a, Show a, Show e, Show b) =>
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
        pure $
          counterexample
            ("IO: " <> show succOrFailIO)
            (label (lblInAndOut x succOrFailIO) $ property $ either (const False) (const True) succOrFailIO)
            .&&. counterexample
              ("IOSIM: " <> show succOrFailSim)
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

newtype DelayUs = DelayUs {unDelayUs :: Int} deriving Show

instance Arbitrary DelayUs where
  arbitrary = DelayUs <$> chooseInt (1, 50)
  shrink (DelayUs n) = DelayUs <$> filter (>= 1) (shrink n)
