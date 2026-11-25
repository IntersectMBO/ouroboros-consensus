{-# LANGUAGE RankNTypes #-}

module Test.Consensus.Leios.LeiosFetch.Wait (tests) where

import Control.Monad (forM_, replicateM_, void)
import Control.Monad.Class.MonadAsync (cancel, wait, withAsync)
import Control.Monad.Class.MonadFork (labelThisThread)
import Control.Monad.Class.MonadTest (exploreRaces)
import Control.Monad.Class.MonadSTM (atomically)
import Control.Monad.Class.MonadTime.SI (DiffTime, Time (Time), getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Control.Monad.IOSim
  ( IOSim
  , SimEventType (..)
  , SimTrace
  , exploreSimTrace
  , runSimOrThrow
  , runSimTrace
  , traceEvents
  , traceResult
  )
import qualified Data.Sequence as Seq
import Ouroboros.Consensus.Leios.Types
  (LeiosPoint (..), endorserBlockHash, mkEndorserBlock)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Config (defaultLeiosFetchStaticEnv)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Request
  (LeiosBlockRequest (..), LeiosFetchRequest (..))
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.RequestState as L
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Runtime as R
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Timing as T
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Wait
import qualified Ouroboros.Consensus.Util.IOLike as IOLike
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (Property, chooseInt, counterexample, forAll, property, testProperty)
import Test.Util.Orphans.IOLike ()

tests :: TestTree
tests =
  testGroup
    "coordinator wait"
    [ testCase "no-op runtime release floods do not create planning wakeups" testNoopRuntimeFlood
    , testCase "real runtime credit release wakes before the fixed deadline" testRuntimeRelease
    , testProperty
        "readiness racing timer registration is neither lost nor leaked"
        propRegistrationRace
    , testCase "readiness at 100ms wakes before the deadline and fallback" testReadiness
    , testCase "legacy parking consumes readiness without an idle timer" testLegacyPark
    , testCase "deadline at 150ms wakes before the idle fallback" testDeadline
    , testCase "idle fallback wakes at 500ms in SI seconds" testFallback
    , testCase "a distant deadline arms only the bounded idle fallback" testDistantDeadline
    , testCase "already-ready notifications coalesce without allocating a timer" testCoalescing
    , testCase
        "readiness wins over an already-due deadline without allocating a timer"
        testAlreadyDue
    , testCase "deadline wins a tie with the fallback" testFallbackTie
    , testCase "readiness during planning remains pending for the next wait" testDuringPlanning
    , testCase "replacing the selected deadline cancels the obsolete timer" testReplacement
    , testCase "readiness flooding does not postpone the absolute deadline" testFlood
    , testCase "handled timing-ledger deadline is not rearmed in a busy loop" testHandled
    , testCase "readiness winner cancels its pending timer before returning" testReadyCleanup
    , testCase "deadline and fallback winners leave no outstanding timers" testTimerCleanup
    , testCase "shutdown cancels the timer owned by the interrupted waiter" testShutdown
    , testProperty "generated early readiness wakes at its publication time" propReadyTimes
    ]

runtimeFixture :: IOSim s (R.Runtime (IOSim s) Int, L.AttemptKey Int)
runtimeFixture = do
  ready <- atomically newReadiness
  rt <- R.newRuntime defaultLeiosFetchStaticEnv ready (pure False)
  commands <- IOLike.uncheckedNewTVarM Seq.empty
  client <- atomically (R.openClient rt 0 False commands (pure Nothing))
  let p = MkLeiosPoint 0 (endorserBlockHash (mkEndorserBlock []))
      req = LeiosBlockRequest (MkLeiosBlockRequest p 1000 maxBound)
  result <- atomically (R.enqueueRequest client (at 0) req)
  key <- either (fail . show) pure result
  atomically (awaitReadiness ready)
  pure (rt, key)

-- Count actual returns from the production wait, rather than assuming one
-- planning pass per attempted notification. The clock/expiry model is the same
-- fixed absolute deadline used by the existing flood regression below.
testNoopRuntimeFlood :: Assertion
testNoopRuntimeFlood = do
  let simulation = labeled $ do
        (rt, key) <- runtimeFixture
        atomically $ R.releaseAttempt rt key >> awaitReadiness (R.runtimeReady rt)
        let producer =
              labelThisThread "stale-release"
                >> replicateM_ 100 (threadDelay 0.005 >> atomically (R.releaseAttempt rt key))
            loop clock wakes = do
              _ <- waitForWakeup (R.runtimeReady rt) (clockDeadline clock)
              now <- getMonotonicTime
              let (fired, next) = T.expire now clock
              if fired then pure (now, wakes + 1) else loop next (wakes + 1)
        withAsync producer $ \_ -> loop initialClock (0 :: Int)
  runSimOrThrow simulation @?= (at 0.2, 1)
  assertNoTimerLeaks (runSimTrace (fst <$> simulation))

testRuntimeRelease :: Assertion
testRuntimeRelease = do
  let simulation = labeled $ do
        (rt, key) <- runtimeFixture
        withAsync (threadDelay 0.1 >> atomically (R.releaseAttempt rt key)) $ \released -> do
          first <- observedWait (R.runtimeReady rt) (Just (at 0.2))
          wait released
          atomically (R.releaseAttempt rt key)
          second <- observedWait (R.runtimeReady rt) (Just (at 0.2))
          pure (first, second)
  runSimOrThrow simulation
    @?= ((at 0.1, Wakeup ReadinessWake True), (at 0.2, Wakeup DeadlineWake False))
  assertNoTimerLeaks (runSimTrace (fst . snd <$> simulation))

propRegistrationRace :: Property
propRegistrationRace = exploreSimTrace id (exploreRaces >> simulation) $ \_ tr ->
  let created =
        [timer | (_, _, Just "fetch-wait", EventTimerCreated timer _ _) <- traceEvents tr]
      cancelled = [timer | (_, _, _, EventTimerCancelled timer) <- traceEvents tr]
   in case traceResult False tr of
        Left failure -> counterexample (show failure) False
        Right result ->
          counterexample (show result) $
            property
              ( result == (at 0, Wakeup ReadinessWake True)
                  && all (\timer -> elem timer cancelled) created
              )
 where
  simulation :: IOSim s (Time, Wakeup)
  simulation = labeled $ do
    ready <- atomically newReadiness
    withAsync (atomically (notifyReadiness ready)) $ \producer -> do
      result <- observedWait ready (Just (at 0.2))
      wait producer
      pure result

at :: Rational -> Time
at = Time . fromRational

labeled :: IOSim s a -> IOSim s a
labeled action = labelThisThread "fetch-wait" >> action

notifyAfter :: Readiness (IOSim s) -> DiffTime -> IOSim s ()
notifyAfter ready delay = do
  labelThisThread "producer"
  threadDelay delay
  atomically (notifyReadiness ready)

observedWait :: Readiness (IOSim s) -> Maybe Time -> IOSim s (Time, Wakeup)
observedWait ready deadline = do
  wakeup <- waitForWakeup ready deadline
  now <- getMonotonicTime
  pure (now, wakeup)

testReadiness :: Assertion
testReadiness =
  runSimOrThrow
    ( labeled $ do
        ready <- atomically newReadiness
        withAsync (notifyAfter ready 0.1) $ \_ -> do
          observed@(now, _) <- observedWait ready (clockDeadline initialClock)
          let (fired, next) = T.expire now initialClock
          pure (observed, fired, clockDeadline next)
    )
    @?= ((at 0.1, Wakeup ReadinessWake True), False, Just (at 0.2))

testDeadline :: Assertion
testDeadline =
  runSimOrThrow
    ( labeled $ do
        ready <- atomically newReadiness
        observedWait ready (Just (at 0.15))
    )
    @?= (at 0.15, Wakeup DeadlineWake False)

testFallback :: Assertion
testFallback =
  runSimOrThrow
    ( labeled $ do
        ready <- atomically newReadiness
        observedWait ready Nothing
    )
    @?= (at 0.5, Wakeup FallbackWake False)

testDistantDeadline :: Assertion
testDistantDeadline = do
  let simulation = labeled $ do
        ready <- atomically newReadiness
        observedWait ready (Just (at (10 ^ (12 :: Int))))
  runSimOrThrow simulation @?= (at 0.5, Wakeup FallbackWake False)
  assertNoTimerLeaks (runSimTrace (fst <$> simulation))

testCoalescing :: Assertion
testCoalescing = do
  let simulation = labeled $ do
        ready <- atomically newReadiness
        atomically (replicateM_ 1000 (notifyReadiness ready))
        first <- observedWait ready Nothing
        second <- observedWait ready Nothing
        pure (first, second)
  runSimOrThrow simulation
    @?= ((at 0, Wakeup ReadinessWake True), (at 0.5, Wakeup FallbackWake False))
  let created = [() | (_, _, Just "fetch-wait", EventTimerCreated{}) <- traceEvents (runSimTrace simulation)]
  length created @?= 1 -- Only the second, idle wait needs a registration.

testAlreadyDue :: Assertion
testAlreadyDue = do
  let simulation = labeled $ do
        ready <- atomically newReadiness
        atomically (notifyReadiness ready)
        observedWait ready (Just (at 0))
      trace = runSimTrace (fst <$> simulation)
  runSimOrThrow simulation @?= (at 0, Wakeup ReadinessWake True)
  [() | (_, _, Just "fetch-wait", EventTimerCreated{}) <- traceEvents trace] @?= []

testFallbackTie :: Assertion
testFallbackTie =
  runSimOrThrow
    ( labeled $ do
        ready <- atomically newReadiness
        observedWait ready (Just (at 0.5))
    )
    @?= (at 0.5, Wakeup DeadlineWake False)

testDuringPlanning :: Assertion
testDuringPlanning = do
  let result = runSimOrThrow $ labeled $ do
        ready <- atomically newReadiness
        atomically (notifyReadiness ready)
        first <- observedWait ready Nothing
        withAsync (notifyAfter ready 0.01) $ \_ -> do
          labelThisThread "planning"
          threadDelay 0.02
          labelThisThread "fetch-wait"
          second <- observedWait ready Nothing
          third <- observedWait ready Nothing
          pure (first, second, third)
  result
    @?= ( (at 0, Wakeup ReadinessWake True)
        , (at 0.02, Wakeup ReadinessWake True)
        , (at 0.52, Wakeup FallbackWake False)
        )

replacement :: IOSim s (Time, (Wakeup, Wakeup))
replacement = labeled $ do
  ready <- atomically newReadiness
  withAsync (notifyAfter ready 0.1) $ \_ -> do
    first <- waitForWakeup ready (Just (at 0.2))
    second <- waitForWakeup ready (Just (at 0.4))
    now <- getMonotonicTime
    pure (now, (first, second))

testReplacement :: Assertion
testReplacement = do
  runSimOrThrow replacement @?= (at 0.4, (Wakeup ReadinessWake True, Wakeup DeadlineWake False))
  let trace = runSimTrace (fst <$> replacement)
      owned = [timer | (_, _, Just "fetch-wait", EventTimerCreated timer _ _) <- traceEvents trace]
      cancelled = [t | (t, _, _, EventTimerCancelled timer) <- traceEvents trace, timer `elem` owned]
      fired = [t | (t, _, _, EventTimerFired timer) <- traceEvents trace, timer `elem` owned]
  -- Cancelling during cleanup may also be traced after a timer already fired.
  filter (< at 0.4) cancelled @?= [at 0.1]
  fired @?= [at 0.4]
  assertNoTimerLeaks trace

initialClock :: T.Clock
initialClock =
  T.activate
    Nothing
    Nothing
    (T.sent (at 0) True (T.newClock (T.Snapshot 0.1 0.05 0.1 T.KeepAliveSample)))

clockDeadline :: T.Clock -> Maybe Time
clockDeadline c = if T.clockFired c then Nothing else T.deadline c

testFlood :: Assertion
testFlood = do
  let result = runSimOrThrow $ labeled $ do
        ready <- atomically newReadiness
        let producer =
              labelThisThread "producer"
                >> replicateM_ 100 (threadDelay 0.005 >> atomically (notifyReadiness ready))
            loop model = do
              _ <- waitForWakeup ready (clockDeadline model)
              now <- getMonotonicTime
              let (fired, next) = T.expire now model
              if fired then pure (now, clockDeadline next) else loop next
        withAsync producer $ \_ -> loop initialClock
  result @?= (at 0.2, Nothing)

testHandled :: Assertion
testHandled = do
  let result = runSimOrThrow $ labeled $ do
        ready <- atomically newReadiness
        first <- waitForWakeup ready (clockDeadline initialClock)
        now <- getMonotonicTime
        let (fired, next) = T.expire now initialClock
        second <- observedWait ready (clockDeadline next)
        pure (first, fired, second)
  result @?= (Wakeup DeadlineWake False, True, (at 0.7, Wakeup FallbackWake False))

assertNoTimerLeaks :: SimTrace Time -> Assertion
assertNoTimerLeaks trace = case traceResult False trace of
  Left failure -> assertFailure (show failure)
  Right finished -> do
    let events = traceEvents trace
        created = [timer | (_, _, Just "fetch-wait", EventTimerCreated timer _ _) <- events]
        ended =
          [(timer, t) | (t, _, _, EventTimerCancelled timer) <- events]
            ++ [(timer, t) | (t, _, _, EventTimerFired timer) <- events]
    assertBool "blocking wait registered a timer" (not (null created))
    assertBool "each wait-owned timer ends before the wait returns" $
      all (\timer -> maybe False (<= finished) (lookup timer ended)) created

testReadyCleanup :: Assertion
testReadyCleanup = do
  let trace = runSimTrace $ labeled $ do
        ready <- atomically newReadiness
        withAsync (notifyAfter ready 0.1) $ \_ -> do
          _ <- waitForWakeup ready (Just (at 0.4))
          getMonotonicTime
  assertNoTimerLeaks trace
  [t | (t, _, Just "fetch-wait", EventTimerCancelled{}) <- traceEvents trace] @?= [at 0.1]

testTimerCleanup :: Assertion
testTimerCleanup = forM_ [Nothing, Just (at 0.15)] $ \deadline ->
  assertNoTimerLeaks $ runSimTrace $ labeled $ do
    ready <- atomically newReadiness
    _ <- waitForWakeup ready deadline
    getMonotonicTime

testShutdown :: Assertion
testShutdown = do
  let trace = runSimTrace $ do
        labelThisThread "controller"
        ready <- atomically newReadiness
        withAsync (labeled (void (waitForWakeup ready (Just (at 0.4))))) $ \waiter -> do
          threadDelay 0.1
          cancel waiter
          getMonotonicTime
  assertNoTimerLeaks trace
  [t | (t, _, Just "fetch-wait", EventTimerCancelled{}) <- traceEvents trace] @?= [at 0.1]

propReadyTimes :: Property
propReadyTimes = forAll (chooseInt (1, 499)) $ \millis ->
  let delay = fromIntegral millis / 1000
      result = runSimOrThrow $ labeled $ do
        ready <- atomically newReadiness
        withAsync (notifyAfter ready delay) $ \_ -> observedWait ready Nothing
   in counterexample (show result) $ property (result == (Time delay, Wakeup ReadinessWake True))

testLegacyPark :: Assertion
testLegacyPark = do
  let simulation = labeled $ do
        ready <- atomically newReadiness
        withAsync (notifyAfter ready 0.1) $ \_ -> do
          atomically (awaitReadiness ready)
          now <- getMonotonicTime
          atomically (notifyReadiness ready)
          wakeup <- waitForWakeup ready Nothing
          pure (now, wakeup)
  runSimOrThrow simulation @?= (at 0.1, Wakeup ReadinessWake True)
  let created = [() | (_, _, Just "fetch-wait", EventTimerCreated{}) <- traceEvents (runSimTrace simulation)]
  created @?= []
