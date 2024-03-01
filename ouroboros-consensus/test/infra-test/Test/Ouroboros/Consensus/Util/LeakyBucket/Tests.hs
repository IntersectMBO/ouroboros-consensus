{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Ouroboros.Consensus.Util.LeakyBucket.Tests (tests) where

import           Control.Monad (foldM, void)
import           Control.Monad.IOSim (IOSim, runSimOrThrow)
import           Data.Either (isLeft, isRight)
import           Data.Functor ((<&>))
import           Data.Ratio ((%))
import           Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import           Ouroboros.Consensus.Util.IOLike (Exception (displayException),
                     MonadAsync, MonadCatch (try), MonadDelay, MonadFork,
                     MonadMask, MonadThrow (throwIO), SomeException,
                     Time (Time), addTime, fromException, threadDelay)
import           Ouroboros.Consensus.Util.LeakyBucket
import           Test.QuickCheck (Arbitrary (arbitrary), Gen, Property,
                     classify, counterexample, forAll, frequency, ioProperty,
                     listOf1, scale, suchThat, (===))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (property, testProperty)
import           Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests = testGroup "Ouroboros.Consensus.Util.LeakyBucket" [
  testProperty "play a bit" prop_playABit,
  testProperty "play too long" prop_playTooLong,
  testProperty "play too long harmless" prop_playTooLongHarmless,
  testProperty "wait almost too long" (prop_noRefill (-1)),
  testProperty "wait just too long" (prop_noRefill 1),
  testProperty "pause for a time" prop_playWithPause,
  testProperty "resume too quickly" prop_playWithPauseTooLong,
  testProperty "propagates exceptions" prop_propagateExceptions,
  testProperty "propagates exceptions (IO)" prop_propagateExceptionsIO,
  testProperty "catch exception" prop_catchException,
  adjustQuickCheckTests (* 10) $ testProperty "random" prop_random
  ]

--------------------------------------------------------------------------------
-- Dummy configuration
--------------------------------------------------------------------------------

newtype Capacity = Capacity { unCapacity :: Rational }
  deriving Show

instance Arbitrary Capacity where
  arbitrary = Capacity <$> arbitrary `suchThat` (> 0)

newtype Rate = Rate { unRate :: Rational }
  deriving Show

instance Arbitrary Rate where
  arbitrary = Rate <$> arbitrary `suchThat` (> 0)

newtype FillOnOverflow = FillOnOverflow { unFillOnOverflow :: Bool }
  deriving Show

instance Arbitrary FillOnOverflow where
  arbitrary = FillOnOverflow <$> arbitrary

-- | Whether to throw on empty bucket.
newtype ThrowOnEmpty = ThrowOnEmpty { unThrowOnEmpty :: Bool }
  deriving (Eq, Show)

instance Arbitrary ThrowOnEmpty where
  arbitrary = ThrowOnEmpty <$> arbitrary

data TestConfig = TestConfig
  { testCapacity     :: Rational,
    testRate         :: Rational,
    testThrowOnEmpty :: Bool
  }
  deriving (Eq, Show)

instance Arbitrary TestConfig where
  arbitrary =
    TestConfig
      <$> (unCapacity <$> arbitrary)
      <*> (unRate <$> arbitrary)
      <*> (unThrowOnEmpty <$> arbitrary)

data EmptyBucket = EmptyBucket
  deriving (Eq, Show)

instance Exception EmptyBucket

-- | Make an actual configuration from a test configuration.
mkConfig :: MonadThrow m => TestConfig -> Config m
mkConfig TestConfig {testCapacity, testRate, testThrowOnEmpty} =
  Config
    { capacity = testCapacity,
      rate = testRate,
      fillOnOverflow = True,
      onEmpty =
        if testThrowOnEmpty
          then (throwIO EmptyBucket)
          else (pure ())
    }

-- | Make a configuration that fills on overflow and throws 'EmptyBucket' on
-- empty bucket.
configThrow :: MonadThrow m => Capacity -> Rate -> Config m
configThrow (Capacity testCapacity) (Rate testRate) =
  mkConfig TestConfig{testCapacity, testRate, testThrowOnEmpty = True}

-- | A configuration with capacity and rate 1, that fills on overflow and throws
-- 'EmptyBucket' on empty bucket.
config11Throw :: MonadThrow m => Config m
config11Throw = configThrow (Capacity 1) (Rate 1)

-- | Make a configuration that fills on overflow and does nothing on empty
-- bucket.
configPure :: MonadThrow m => Capacity -> Rate -> Config m
configPure (Capacity testCapacity) (Rate testRate) =
  mkConfig TestConfig{testCapacity, testRate, testThrowOnEmpty = False}

-- | A configuration with capacity 1 and rate 1, that fills on overflow and does
-- nothing on empty bucket.
config11Pure :: MonadThrow m => Config m
config11Pure = configPure (Capacity 1) (Rate 1)

-- | Strip the configuration from a 'State', so as to make it comparable,
-- showable, etc.
stripConfig :: State cfg -> State ()
stripConfig state = state{config=()}

-- | 'evalAgainstBucket' followed by 'stripConfig'.
stripEvalAgainstBucket ::
  (MonadDelay m, MonadAsync m, MonadFork m, MonadMask m) =>
  Config m ->
  (Handlers m -> m a) ->
  m (State ())
stripEvalAgainstBucket config action = stripConfig <$> evalAgainstBucket config action

-- | Alias for 'runSimOrThrow' by analogy to 'ioProperty'.
ioSimProperty :: forall a. (forall s. IOSim s a) -> a
ioSimProperty = runSimOrThrow

-- | QuickCheck helper to check that a code threw the given exception.
shouldThrow :: (MonadCatch m, Show a, Exception e, Eq e) => m a -> e -> m Property
shouldThrow a e =
  try a <&> \case
    Left exn
      | fromException exn == Just e -> property True
      | otherwise -> counterexample ("Expected exception " ++ show e ++ "; got exception " ++ show exn) False
    Right result -> counterexample ("Expected exception " ++ show e ++ "; got " ++ show result) False

-- | QuickCheck helper to check that a code evaluated to the given value.
shouldEvaluateTo :: (MonadCatch m, Eq a, Show a) => m a -> a -> m Property
shouldEvaluateTo a v =
  try a <&> \case
    Right result
      | result == v -> property True
      | otherwise -> counterexample ("Expected " ++ show v ++ "; got " ++ show result) False
    Left (exn :: SomeException) -> counterexample ("Expected " ++ show v ++ "; got exception " ++ displayException exn) False

-- | Number of picoseconds in a second (@10^12@).
picosecondsPerSecond :: Integer
picosecondsPerSecond = 1_000_000_000_000

--------------------------------------------------------------------------------
-- Simple properties
--------------------------------------------------------------------------------

-- | One test case where we wait a bit, then fill, then wait some more. We then
-- should observe a state with a positive level.
prop_playABit :: Property
prop_playABit =
  ioSimProperty $
    stripEvalAgainstBucket config11Throw (\handlers -> do
      threadDelay 0.5
      void $ fill handlers 67
      threadDelay 0.9
    ) `shouldEvaluateTo` State{level = 1 % 10, time = Time 1.4, paused = False, config = ()}

-- | One test case similar to 'prop_playABit' but we wait a bit too long and
-- should observe the triggering of the 'onEmpty' action.
prop_playTooLong :: Property
prop_playTooLong =
  ioSimProperty $
    stripEvalAgainstBucket config11Throw (\handlers -> do
      threadDelay 0.5
      void $ fill handlers 67
      threadDelay 1.1
    ) `shouldThrow` EmptyBucket

-- | One test case similar to 'prop_playTooLong' but 'onEmpty' does nothing and
-- therefore we should still observe a state at the end.
prop_playTooLongHarmless :: Property
prop_playTooLongHarmless =
  ioSimProperty $
    stripEvalAgainstBucket config11Pure (\handlers -> do
      threadDelay 0.5
      void $ fill handlers 67
      threadDelay 1.1
    ) `shouldEvaluateTo` State{level = 0, time = Time 1.6, paused = False, config = ()}

prop_playWithPause :: Property
prop_playWithPause =
  ioSimProperty $
    stripEvalAgainstBucket config11Throw (\handlers -> do
      threadDelay 0.5
      setPaused handlers True
      threadDelay 1.5
      setPaused handlers False
      threadDelay 0.4
    ) `shouldEvaluateTo` State{level = 1 % 10, time = Time 2.4, paused = False, config = ()}

prop_playWithPauseTooLong :: Property
prop_playWithPauseTooLong =
  ioSimProperty $
    stripEvalAgainstBucket config11Throw (\handlers -> do
      threadDelay 0.5
      setPaused handlers True
      threadDelay 1.5
      setPaused handlers False
      threadDelay 0.6
    ) `shouldThrow` EmptyBucket

-- | A bunch of test cases where we wait exactly as much as the bucket runs
-- except for a given offset. If the offset is negative, we should get a
-- state. If the offset is positive, we should get an exception. NOTE: Do not
-- use an offset of @0@. NOTE: Considering the precision, we *need* IOSim for
-- this test.
prop_noRefill :: Integer -> Capacity -> Rate -> Property
prop_noRefill offset capacity@(Capacity c) rate@(Rate r) = do
  -- NOTE: The @-1@ is to ensure that we do not test the situation where the
  -- bucket empties at the *exact* same time (curtesy of IOSim) as the action.
  let ps = floor (c / r * fromInteger picosecondsPerSecond) + offset
      time = picosecondsToDiffTime ps
      level = c - (ps % picosecondsPerSecond) * r
  if
    | offset < 0 ->
      ioSimProperty $
        stripEvalAgainstBucket (configThrow capacity rate) (\_ -> threadDelay time)
        `shouldEvaluateTo` State{level, time = Time time, paused = False, config = ()}
    | offset > 0 ->
      ioSimProperty $
        stripEvalAgainstBucket (configThrow capacity rate) (\_ -> threadDelay time)
        `shouldThrow` EmptyBucket
    | otherwise ->
      error "prop_noRefill: do not use an offset of 0"

--------------------------------------------------------------------------------
-- Exception propagation
--------------------------------------------------------------------------------

-- | A dummy exception that we will use to outrun the bucket.
data NoPlumberException = NoPlumberException
  deriving (Eq, Show)
instance Exception NoPlumberException

-- | One test to check that throwing an exception in the action does propagate
-- outside of @*AgainstBucket@.
prop_propagateExceptions :: Property
prop_propagateExceptions =
  ioSimProperty $
    stripEvalAgainstBucket config11Throw (\_ -> throwIO NoPlumberException)
      `shouldThrow`
    NoPlumberException

-- | Same as 'prop_propagateExceptions' except it runs in IO.
prop_propagateExceptionsIO :: Property
prop_propagateExceptionsIO =
  ioProperty $
    stripEvalAgainstBucket config11Throw (\_ -> throwIO NoPlumberException)
      `shouldThrow`
    NoPlumberException

-- | One test to show that we can catch the 'EmptyBucket' exception from the
-- action itself, but that it is not wrapped in 'ExceptionInLinkedThread'.
prop_catchException :: Property
prop_catchException =
  ioSimProperty $
    execAgainstBucket config11Throw (\_ -> try $ threadDelay 1000)
      `shouldEvaluateTo`
    Left EmptyBucket

--------------------------------------------------------------------------------
-- Against a model
--------------------------------------------------------------------------------

-- | Abstract “actions” to be run. We can either wait by some time or refill the
-- bucket by some value.
data Action = ThreadDelay DiffTime | Fill Rational | SetPaused Bool
  deriving (Eq, Show)

-- | Random generation of 'Action's. The scales and frequencies are taken such
-- that we explore as many interesting cases as possible.
genAction :: Gen Action
genAction = frequency [
  (1, ThreadDelay . picosecondsToDiffTime <$> scale (* fromInteger picosecondsPerSecond) (arbitrary `suchThat` (>= 0))),
  (1, Fill <$> scale (* 1_000_000_000_000_000) (arbitrary `suchThat` (>= 0))),
  (1, SetPaused <$> arbitrary)
  ]

-- | How to run the 'Action's in a monad.
applyActions :: MonadDelay m => Handlers m -> [Action] -> m ()
applyActions handlers = mapM_ $ \case
  ThreadDelay t -> threadDelay t
  Fill t -> void $ fill handlers t
  SetPaused p -> setPaused handlers p

-- | A model of what we expect the 'Action's to lead to, either an 'EmptyBucket'
-- exception (if the bucket won the race) or a 'State' (otherwise).
modelActions :: TestConfig -> [Action] -> Either EmptyBucket (State TestConfig)
modelActions config =
  foldM go $ State{level = testCapacity config, time = Time 0, paused = False, config}
  where
    go :: State TestConfig -> Action -> Either EmptyBucket (State TestConfig)
    go state@State{time, level, paused, config=TestConfig{testCapacity, testRate, testThrowOnEmpty}} = \case
      Fill t ->
        Right state{level = min testCapacity (level + t)}
      ThreadDelay t ->
        let newTime = addTime t time
            newLevel = if paused then level else max 0 (level - diffTimeToSecondsRational t * testRate)
         in if newLevel <= 0 && testThrowOnEmpty
              then Left EmptyBucket
              else Right state{time = newTime, level = newLevel}
      SetPaused p ->
        Right state{paused = p}

-- | A bunch of test cases where we generate a list of 'Action's ,run them via
-- 'applyActions' and compare the result to that of 'modelActions'.
prop_random :: TestConfig -> Property
prop_random testConfig =
  forAll (listOf1 genAction) $ \actions ->
    let modelResult = modelActions testConfig actions
        nbActions = length actions
     in classify (isLeft modelResult) "bucket finished empty" $
        classify (isRight modelResult) "bucket finished non-empty" $
        classify (nbActions <= 10) "<= 10 actions" $
        classify (10 < nbActions && nbActions <= 20) "11-20 actions" $
        classify (20 < nbActions && nbActions <= 50) "21-50 actions" $
        classify (50 < nbActions) "> 50 actions" $
        runSimOrThrow (
          try $ stripEvalAgainstBucket (mkConfig testConfig) $
            flip applyActions actions
        ) === (stripConfig <$> modelResult)
