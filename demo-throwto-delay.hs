{-# LANGUAGE BangPatterns #-}
-- | Demonstrates why a race-based iterateUntilOrTimeout cannot reliably
-- bound execution time even when each step contains a quick IO action.
--
-- Compile and run:
--   ghc -O2 -threaded demo-throwto-delay.hs \
--       -package async -package stm -package time -package io-classes
--   ./demo-throwto-delay +RTS -N2
module Main where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Data.Time.Clock (getCurrentTime, diffUTCTime, DiffTime)

-- NOINLINE prevents GHC from evaluating this at compile time.
-- Without it, foldl' over a constant range is precomputed and the loop
-- disappears entirely — making the step sub-microsecond.
{-# NOINLINE tightSum #-}
tightSum :: Int -> Int
tightSum n = go 0 n
 where
  go :: Int -> Int -> Int
  go !acc 0 = acc
  go !acc k = go (acc + k) (k - 1)

data CallDoneType = TimerDone | StepperDone deriving Show

iterateUntilOrTimeoutNaive
  :: DiffTime
  -> a
  -> (a -> IO (Bool, a))
  -> IO (a, CallDoneType)
iterateUntilOrTimeoutNaive delay initSt stepAction = do
  stVar  <- newTVarIO initSt
  result <- race (threadDelay delay) (goStepper stVar)
  lastSt <- atomically $ readTVar stVar
  pure (lastSt, either (const TimerDone) (const StepperDone) result)
 where
  goStepper stVar = do
    stVal <- atomically $ readTVar stVar
    (isDone, stVal') <- stepAction stVal
    atomically $ writeTVar stVar stVal'
    if isDone then pure () else goStepper stVar

-- | Step that mirrors the structure of computeSnapshot3:
--   1. A quick IO action (getMonotonicTime / Debug.trace) — IS an interruptible
--      point, but the timer almost never fires here because it's sub-millisecond.
--   2. A tight non-allocating computation — NO safe points.
--
-- GHC with -O2 fuses 'foldl\'' over [1..N :: Int] into an unboxed counter loop
-- in CPU registers with zero heap allocation. No allocation = no GC heap checks
-- = no async-exception safe points. The timer fires during this loop, but
-- 'throwTo' cannot deliver until the loop exits and the next 'atomically' runs.
heavyStep :: Int -> IO (Bool, Int)
heavyStep n = do
  -- Quick IO: in the real system this was getMonotonicTime / Debug.trace.
  -- print IS interruptible, but the 10ms budget rarely fires here (<1ms).
  print n
  -- Non-allocating tight loop via a NOINLINE function (~150ms).
  -- GHC compiles tightSum to an unboxed counter in CPU registers.
  -- No heap allocation = no GC heap-check safe points = throwTo blocks.
  -- n varies each call so GHC cannot memo-share the result as a CAF.
  let !r = tightSum (n + 200_000_000)
  pure (r < 0, n + 1)   -- r >= 0 always; stepper runs until timer

main :: IO ()
main = do
  let budget = 0.01 :: DiffTime   -- 10ms: fires well inside the ~150ms loop

  putStrLn "=== throwTo delay: quick IO + non-allocating computation ==="
  putStrLn ""
  putStrLn   "Step structure:"
  putStrLn   "  1. print n                  -- quick IO, interruptible"
  putStrLn   "                                (timer rarely fires here: <1ms)"
  putStrLn   "  2. tightSum (n+200_000_000) -- non-allocating tail-recursive loop, ~100ms"
  putStrLn   "                                (unboxed Int# in registers, no heap alloc)"
  putStrLn   "                                (no heap check = no safe point inside loop)"
  putStrLn   "                                (timer fires here; throwTo blocks)"
  putStrLn   "  3. atomically writeTVar     -- interruptible; exception delivered here"
  putStrLn ""
  putStrLn $ "Budget:  " <> show budget
  putStrLn   "Expected elapsed: ~10ms"
  putStrLn   "Actual elapsed:   ~110ms (throwTo waits for safe point after loop)"
  putStrLn ""

  t0 <- getCurrentTime
  (steps, doneType) <- iterateUntilOrTimeoutNaive budget (0 :: Int) heavyStep
  t1 <- getCurrentTime

  let elapsedMs = round (realToFrac (diffUTCTime t1 t0) * 1000 :: Double) :: Int
  putStrLn $ "Result:  " <> show doneType
  putStrLn $ "Steps:   " <> show steps
  putStrLn $ "Elapsed: " <> show elapsedMs <> " ms  (budget was 10 ms)"
