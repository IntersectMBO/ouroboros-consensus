{-# LANGUAGE BangPatterns #-}

-- | Demonstrates how masking state affects race-based timeout via throwTo.
--
-- Compile and run:
--   ghc -O2 -threaded demo-throwto-delay.hs \
--       -package async -package stm -package time -package io-classes \
--       -o demo-throwto-delay
--   ./demo-throwto-delay +RTS -N2
module Main where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM
import Control.Exception
  ( MaskingState (..)
  , getMaskingState
  , mask_
  , uninterruptibleMask_
  )
import Control.Monad (unless)
import Control.Monad.Class.MonadTimer.SI (threadDelay)
import Data.Time.Clock (DiffTime, diffUTCTime, getCurrentTime)

-- NOINLINE prevents GHC from constant-folding the result at compile time.
-- Zero heap allocation inside the loop — no GC heap-check safe points.
{-# NOINLINE tightSum #-}
tightSum :: Int -> Int
tightSum n = go 0 n
 where
  go !acc 0 = acc
  go !acc k = go (acc + k) (k - 1)

data DoneType = TimerDone | StepperDone deriving (Eq, Show)

-- | Masking state propagates into spawned threads via withAsync's restore:
--   withAsync action inner =
--     mask $ \restore -> async (restore action) >>= \a -> restore (inner a)
iterateUntilOrTimeout ::
  DiffTime -> a -> (a -> IO (Bool, a)) -> IO (a, DoneType)
iterateUntilOrTimeout delay initSt stepAction = do
  stVar <- newTVarIO initSt
  result <- race (threadDelay delay) (goStepper stVar)
  st <- atomically (readTVar stVar)
  pure (st, either (const TimerDone) (const StepperDone) result)
 where
  goStepper stVar = do
    st <- atomically (readTVar stVar) -- non-retrying: NOT interruptible
    (done, st') <- stepAction st
    atomically (writeTVar stVar st') -- non-retrying: NOT interruptible
    unless done $ goStepper stVar

heavyStep :: Int -> Int -> IO (Bool, Int)
heavyStep maxSteps n = do
  ms <- getMaskingState
  putStr $ "[" <> show ms <> " n=" <> show n <> "] "
  let !_r = tightSum (n + 200_000_000)
  pure (n + 1 >= maxSteps, n + 1)

runTest :: String -> IO (Int, DoneType) -> IO ()
runTest label run = do
  putStr $ label <> ": "
  t0 <- getCurrentTime
  (n, dt) <- run
  t1 <- getCurrentTime
  let ms = round (realToFrac (diffUTCTime t1 t0) * 1000 :: Double) :: Int
  putStrLn $ show dt <> " " <> show n <> " step(s) " <> show ms <> " ms"

main :: IO ()
main = do
  let budget = 0.01 :: DiffTime

  runTest "Unmasked            " $
    iterateUntilOrTimeout budget 0 (heavyStep 10)

  -- Under MaskedInterruptible, throwTo only delivers at operations that
  -- ACTUALLY block (retrying atomically, blocking takeMVar, threadDelay).
  -- Non-retrying atomically commits immediately and does not qualify.
  runTest "MaskedInterruptible " $
    mask_ $
      iterateUntilOrTimeout budget 0 (heavyStep 10)

  -- Under MaskedUninterruptible, throwTo never delivers.
  -- race's cancel blocks until the target exits the mask region.
  -- Step cap of 1 prevents deadlock.
  runTest "MaskedUninterruptible" $
    uninterruptibleMask_ $
      iterateUntilOrTimeout budget 0 (heavyStep 10)
