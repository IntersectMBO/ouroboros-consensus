{-# LANGUAGE RankNTypes #-}

module Test.Consensus.Leios.LeiosFetch.Wait (tests) where

import Control.Monad (forM_, replicateM_, void)
import Control.Monad.Class.MonadAsync (cancel, withAsync)
import Control.Monad.Class.MonadFork (labelThisThread)
import Control.Monad.Class.MonadSTM (atomically)
import Control.Monad.Class.MonadTime.SI (DiffTime, Time (Time), getMonotonicTime)
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Control.Monad.IOSim
  ( IOSim
  , SimEventType (..)
  , SimTrace
  , runSimOrThrow
  , runSimTrace
  , traceEvents
  , traceResult
  )
import Data.Ratio ((%))
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Deadline
  ( RttObservation (..)
  , conservativeRtt
  )
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Timing
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Wait
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck (Property, chooseInt, counterexample, forAll, property, testProperty)

tests :: TestTree
tests =
  testGroup
    "atomic wait"
    [ testCase "readiness at 100ms wakes before the deadline and fallback" testReadiness
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
    , testCase "sub-microsecond deadline rounds up instead of waking early" testResolution
    , testCase "readiness winner cancels its pending timer before returning" testReadyCleanup
    , testCase "deadline and fallback winners leave no outstanding timers" testTimerCleanup
    , testCase "shutdown cancels the timer owned by the interrupted waiter" testShutdown
    , testProperty "generated early readiness wakes at its publication time" propReadyTimes
    ]

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
        withAsync (notifyAfter ready 0.1) $ \_ -> observedWait ready (Just (at 0.15))
    )
    @?= (at 0.1, Wakeup ReadinessWake True)

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

-- The timing ledger, not the wait helper, retires a handled trigger. Its ACK
-- guard is sufficient to test the shared absolute-deadline integration here.
initialTiming :: TimingState Int
initialTiming = either (error . show) id $ do
  let key = RequestKey (MkLeiosPeerLifetimeId 7 0) (FetchSessionToken 0) (WireOrdinal 0)
      rtt = conservativeRtt (at 0) (Just (RttObservation 0.1 (at 0)))
  registered <-
    registerTiming key (TrackAck rtt) (newTimingState (MkLeiosPeerLifetimeId 7 0) (FetchSessionToken 0))
  sent <- recordSend key (at 0) registered
  snd <$> reconcileTiming (at 0) sent

testFlood :: Assertion
testFlood = do
  let result = runSimOrThrow $ labeled $ do
        ready <- atomically newReadiness
        let producer = do
              labelThisThread "producer"
              replicateM_ 100 (threadDelay 0.005 >> atomically (notifyReadiness ready))
            loop model = do
              _ <- waitForWakeup ready (snd <$> nextDeadline model)
              now <- getMonotonicTime
              case reconcileTiming now model of
                Left err -> error (show err)
                Right ([], next) -> loop next
                Right (events, next) -> pure (now, length events, nextDeadline next)
        withAsync producer $ \_ -> loop initialTiming
  result @?= (at 0.15, 1, Nothing)

testHandled :: Assertion
testHandled = do
  let result = runSimOrThrow $ labeled $ do
        ready <- atomically newReadiness
        first <- waitForWakeup ready (snd <$> nextDeadline initialTiming)
        now <- getMonotonicTime
        let (events, next) = either (error . show) id (reconcileTiming now initialTiming)
        second <- observedWait ready (snd <$> nextDeadline next)
        pure (first, length events, second)
  result @?= (Wakeup DeadlineWake False, 1, (at 0.65, Wakeup FallbackWake False))

testResolution :: Assertion
testResolution =
  runSimOrThrow
    ( labeled $ do
        ready <- atomically newReadiness
        observedWait ready (Just (at (1 % 1000000000000)))
    )
    @?= (at (1 % 1000000), Wakeup DeadlineWake False)

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
