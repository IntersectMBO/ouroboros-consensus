{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains various tests for the leaky bucket. Some, prefixed by
-- “play”, are simple, manual tests; two concern (non-)propagation of exceptions
-- between the bucket thread and the action's thread; the last one compares a
-- run of the actual bucket implementation against a model.
module Test.Ouroboros.Consensus.Util.LeakyBucket.Tests (tests) where

import           Control.Monad (foldM, void)
import           Control.Monad.Class.MonadTimer (MonadTimer)
import           Control.Monad.IOSim (IOSim, runSimOrThrow)
import           Data.Either (isLeft, isRight)
import           Data.Functor ((<&>))
import           Data.List (intersperse)
import           Data.Ratio ((%))
import           Data.Time.Clock (DiffTime, picosecondsToDiffTime)
import           Ouroboros.Consensus.Util.IOLike (Exception (displayException),
                     MonadAsync, MonadCatch (try), MonadDelay, MonadFork,
                     MonadMask, MonadSTM, MonadThrow (throwIO), NoThunks,
                     SomeException, Time (Time), addTime, fromException,
                     threadDelay)
import           Ouroboros.Consensus.Util.LeakyBucket
import           Test.QuickCheck (Arbitrary (arbitrary), Gen, Property,
                     classify, counterexample, forAllShrinkBlind, frequency,
                     ioProperty, liftArbitrary2, listOf1, scale, shrinkList,
                     suchThat)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (property, testProperty)
import           Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests = testGroup "Ouroboros.Consensus.Util.LeakyBucket" [
  testProperty "play a bit" prop_playABit,
  testProperty "play too long" prop_playTooLong,
  testProperty "play too long harmless" prop_playTooLongHarmless,
  testProperty "play with pause" prop_playWithPause,
  testProperty "play with pause too long" prop_playWithPauseTooLong,
  testProperty "wait almost too long" (prop_noRefill False),
  testProperty "wait just too long" (prop_noRefill True),
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

data TestState = TestState
  { testLevel  :: Rational,
    testTime   :: Time,
    testPaused :: Bool
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
configThrow :: Capacity -> Rate -> TestConfig
configThrow (Capacity testCapacity) (Rate testRate) =
  TestConfig{testCapacity, testRate, testThrowOnEmpty = True}

-- | A configuration with capacity and rate 1, that fills on overflow and throws
-- 'EmptyBucket' on empty bucket.
config11Throw :: TestConfig
config11Throw = configThrow (Capacity 1) (Rate 1)

-- | Make a configuration that fills on overflow and does nothing on empty
-- bucket.
configPure :: Capacity -> Rate -> TestConfig
configPure (Capacity testCapacity) (Rate testRate) =
  TestConfig{testCapacity, testRate, testThrowOnEmpty = False}

-- | A configuration with capacity 1 and rate 1, that fills on overflow and does
-- nothing on empty bucket.
config11Pure :: TestConfig
config11Pure = configPure (Capacity 1) (Rate 1)

stateToTestState :: State m -> TestState
stateToTestState State{level, time, paused} =
  TestState{testLevel = level, testTime = time, testPaused = paused}

-- | 'execAgainstBucket' except it takes a 'TestConfig'.
testExecAgainstBucket ::
  ( MonadDelay m,
    MonadAsync m,
    MonadFork m,
    MonadMask m,
    MonadTimer m,
    NoThunks (m ())
  ) =>
  TestConfig ->
  (Handlers m -> m a) ->
  m a
testExecAgainstBucket testConfig action =
  execAgainstBucket (mkConfig testConfig) action

-- | 'evalAgainstBucket' except it takes a 'TestConfig' and returns a 'TestState'.
testEvalAgainstBucket ::
  ( MonadDelay m,
    MonadAsync m,
    MonadFork m,
    MonadMask m,
    MonadTimer m,
    NoThunks (m ())
  ) =>
  TestConfig ->
  (Handlers m -> m a) ->
  m TestState
testEvalAgainstBucket testConfig action =
  stateToTestState <$> evalAgainstBucket (mkConfig testConfig) action

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

--------------------------------------------------------------------------------
-- Simple properties
--------------------------------------------------------------------------------

-- | One test case where we wait a bit, then fill, then wait some more. We then
-- should observe a state with a positive level.
prop_playABit :: Property
prop_playABit =
  ioSimProperty $
    testEvalAgainstBucket config11Throw (\handlers -> do
      threadDelay 0.5
      void $ fill' handlers 67
      threadDelay 0.9
    ) `shouldEvaluateTo` TestState{testLevel = 1 % 10, testTime = Time 1.4, testPaused = False}

-- | One test case similar to 'prop_playABit' but we wait a bit too long and
-- should observe the triggering of the 'onEmpty' action.
prop_playTooLong :: Property
prop_playTooLong =
  ioSimProperty $
    testEvalAgainstBucket config11Throw (\handlers -> do
      threadDelay 0.5
      void $ fill' handlers 67
      threadDelay 1.1
    ) `shouldThrow` EmptyBucket

-- | One test case similar to 'prop_playTooLong' but 'onEmpty' does nothing and
-- therefore we should still observe a state at the end.
prop_playTooLongHarmless :: Property
prop_playTooLongHarmless =
  ioSimProperty $
    testEvalAgainstBucket config11Pure (\handlers -> do
      threadDelay 0.5
      void $ fill' handlers 67
      threadDelay 1.1
    ) `shouldEvaluateTo` TestState{testLevel = 0, testTime = Time 1.6, testPaused = False}

prop_playWithPause :: Property
prop_playWithPause =
  ioSimProperty $
    testEvalAgainstBucket config11Throw (\handlers -> do
      threadDelay 0.5
      setPaused' handlers True
      threadDelay 1.5
      setPaused' handlers False
      threadDelay 0.4
    ) `shouldEvaluateTo` TestState{testLevel = 1 % 10, testTime = Time 2.4, testPaused = False}

prop_playWithPauseTooLong :: Property
prop_playWithPauseTooLong =
  ioSimProperty $
    testEvalAgainstBucket config11Throw (\handlers -> do
      threadDelay 0.5
      setPaused' handlers True
      threadDelay 1.5
      setPaused' handlers False
      threadDelay 0.6
    ) `shouldThrow` EmptyBucket

-- | A bunch of test cases where we wait exactly as much as the bucket runs
-- except for a given offset. If the offset is negative, we should get a
-- state. If the offset is positive, we should get an exception. NOTE: Do not
-- use an offset of @0@. NOTE: Considering the precision, we *need* IOSim for
-- this test.
prop_noRefill :: Bool -> Capacity -> Rate -> Property
prop_noRefill tooLong capacity@(Capacity c) rate@(Rate r) = do
  -- NOTE: The @-1@ is to ensure that we do not test the situation where the
  -- bucket empties at the *exact* same time (curtesy of IOSim) as the action.
  let ps =
        floor (c / r * fromInteger picosecondsPerSecond)
          + (if tooLong then 1 else -1) * microsecondsPerSecond
      time = picosecondsToDiffTime ps
      level = c - (ps % picosecondsPerSecond) * r
  if tooLong
    then
      ioSimProperty $
        testEvalAgainstBucket (configThrow capacity rate) (\_ -> threadDelay time)
          `shouldThrow` EmptyBucket
    else
      ioSimProperty $
        testEvalAgainstBucket (configThrow capacity rate) (\_ -> threadDelay time)
          `shouldEvaluateTo` TestState {testLevel = level, testTime = Time time, testPaused = False}

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
    testEvalAgainstBucket config11Throw (\_ -> throwIO NoPlumberException)
      `shouldThrow`
    NoPlumberException

-- | Same as 'prop_propagateExceptions' except it runs in IO.
prop_propagateExceptionsIO :: Property
prop_propagateExceptionsIO =
  ioProperty $
    testEvalAgainstBucket config11Throw (\_ -> throwIO NoPlumberException)
      `shouldThrow`
    NoPlumberException

-- | One test to show that we can catch the 'EmptyBucket' exception from the
-- action itself, but that it is not wrapped in 'ExceptionInLinkedThread'.
prop_catchException :: Property
prop_catchException =
  ioSimProperty $
    testExecAgainstBucket config11Throw (\_ -> try $ threadDelay 1000)
      `shouldEvaluateTo`
    Left EmptyBucket

--------------------------------------------------------------------------------
-- Against a model
--------------------------------------------------------------------------------

-- | Abstract “actions” to be run. We can either wait by some time or refill the
-- bucket by some value.
data Action
  = Wait DiffTime
  | Fill Rational
  | SetPaused Bool
  | -- | Set the configuration, then wait the given time. Setting the
    -- configuration without waiting can lead to poorly defined situations.
    SetConfigWait TestConfig DiffTime
  deriving (Eq, Show)

-- | Random generation of 'Action's. The scales and frequencies are taken such
-- that we explore as many interesting cases as possible.
genAction :: Gen Action
genAction =
  frequency
    [ (1, Wait <$> genDelay),
      (1, Fill <$> scale (* 1_000_000_000_000_000) (arbitrary `suchThat` (>= 0))),
      (1, SetPaused <$> arbitrary),
      (1, SetConfigWait <$> arbitrary <*> genDelay)
    ]
  where
    genDelay = picosecondsToDiffTime <$> scale (* fromInteger picosecondsPerSecond) (arbitrary `suchThat` (>= 0))

-- | How to run the 'Action's in a monad.
applyActions :: (MonadDelay m, MonadThrow m, MonadSTM m) => Handlers m -> [Action] -> m ()
applyActions handlers = mapM_ $ \case
  Wait t -> threadDelay t
  Fill t -> void $ fill' handlers t
  SetPaused p -> setPaused' handlers p
  SetConfigWait cfg t -> do
    updateConfig' handlers $ (\(l, _) -> (l, mkConfig cfg))
    threadDelay t

-- | A model of what we expect the 'Action's to lead to, either an 'EmptyBucket'
-- exception (if the bucket won the race) or a 'State' (otherwise).
modelActions :: TestConfig -> [Action] -> Either EmptyBucket TestState
modelActions testConfig =
  (snd <$>) . foldM go (testConfig, TestState {testLevel = testCapacity testConfig, testTime = Time 0, testPaused = False})
  where
    go :: (TestConfig, TestState) -> Action -> Either EmptyBucket (TestConfig, TestState)
    go (config@TestConfig {testCapacity, testRate, testThrowOnEmpty}, state@TestState {testTime, testLevel, testPaused}) = \case
      Fill t ->
        Right (config, state {testLevel = clamp (0, testCapacity) (testLevel + t)})
      Wait t ->
        let newTime = addTime t testTime
            newLevel =
              if testPaused
                then testLevel
                else clamp (0, testCapacity) (testLevel - diffTimeToSecondsRational t * testRate)
         in if newLevel <= 0 && testThrowOnEmpty
              then Left EmptyBucket
              else Right (config, state {testTime = newTime, testLevel = newLevel})
      SetPaused newPaused ->
        Right (config, state {testPaused = newPaused})
      SetConfigWait newConfig@TestConfig {testCapacity = newTestCapacity} t ->
        go (newConfig, state {testLevel = clamp (0, newTestCapacity) testLevel}) (Wait t)

-- | A bunch of test cases where we generate a list of 'Action's ,run them via
-- 'applyActions' and compare the result to that of 'modelActions'.
prop_random :: Property
prop_random =
  forAllShrinkBlind
    (liftArbitrary2 arbitrary (listOf1 genAction))
    (traverse (shrinkList (const [])))
    $ \(testConfig, actions) ->
      let result =
            runSimOrThrow
              ( try $
                  testEvalAgainstBucket testConfig $
                    flip applyActions actions
              )
          modelResult = modelActions testConfig actions
          nbActions = length actions
       in classify (isLeft modelResult) "bucket finished empty" $
          classify (isRight modelResult) "bucket finished non-empty" $
          classify (nbActions <= 10) "<= 10 actions" $
          classify (10 < nbActions && nbActions <= 20) "11-20 actions" $
          classify (20 < nbActions && nbActions <= 50) "21-50 actions" $
          classify (50 < nbActions) "> 50 actions" $
          counterexample ("Config: " ++ show testConfig) $
          counterexample ("Actions:\n" ++ (concat $ intersperse "\n" $ map ((" - " ++) . show) actions)) $
          counterexample ("Result: " ++ show result) $
          counterexample ("Model:  " ++ show modelResult) $
          result == modelResult

-- NOTE: Needed for GHC 8
clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) x = min high (max low x)
