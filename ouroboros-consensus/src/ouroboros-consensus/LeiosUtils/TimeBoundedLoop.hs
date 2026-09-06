{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Time-bounded iteration.
--
-- This is based on simple single-threaded iterative time budgeting
-- and as such not suitable for defensive timeouts since no attempts
-- are made at stopping the computation that breached the time limit.
module LeiosUtils.TimeBoundedLoop
  ( iterateUntilOrTimeout
  , DoneType (..)
  , StepCount
  , iterateUntilOrTimeout_
  , foldOrTimeout
  , propIterImmediateEndOnInit
  , propIterTimeoutOverrunBoundedByOneStep
  , propFoldEmptyListTerminates
  , propFoldStepsMatchConsumed
  , propFoldEndReachedIffConsumedAll
  , propFoldJustFoldl
  ) where

import Cardano.Prelude (lift)
import Control.Monad (unless, when)
import Control.Monad.Class.MonadTime.SI (MonadMonotonicTime (getMonotonicTime), diffTime)
import Control.Monad.Class.MonadTimer.SI (DiffTime, MonadDelay (..))
import Control.Monad.Except (ExceptT, throwError)
import qualified Data.Foldable as Foldable

-- | Why time capped loop stopped.
data DoneType
  = -- | The time budget was exhausted before the end of computation was reached.
    TimeoutReached
  | -- | The computation reached an end before the time budget was exhausted.
    EndReached
  deriving stock (Show, Eq)

-- | Number of 'stepAction' invocations performed.
type StepCount = Int

-- | @let (val, steps, done) = iterateUntilOrTimeout ttl endReached stepAction initSt@ runs
-- 'stepAction' on a state repeatedly, with 'initSt' as the initial state value, until either
-- 'endReached' holds or the wall-clock budget 'ttl' is exceeded.
-- Returns the final result `val`, the 'steps' it took to compute it and 'done' denoting
-- whether the computation terminated due to the provided end condition or timeout.
--
-- __Strict__ on the accumulator.
--
-- __Single-threaded__: everything runs in one thread.  If 'stepAction' blocks
-- longer than 'ttl', the timeout is detected only on the /next/ iteration —
-- the in-flight call is not cancelled.  The total elapsed time can therefore
-- exceed @ttl@ by at most one step duration (see 'propIterTimeoutOverrunBoundedByOneStep').
--
-- If the 'endReached' holds on the 'initSt' the loop returns immediately with
-- @steps == 0@ and @done == EndReached@ (see 'propIterImmediateEndOnInit').
--
-- Naming reflects https://hackage.haskell.org/package/monad-loops-0.4.3/docs/Control-Monad-Loops.html#v:iterateUntilM
iterateUntilOrTimeout ::
  MonadMonotonicTime m =>
  -- | Wall-clock budget
  DiffTime ->
  -- | End predicate; when @True@, stops with 'EndReached'
  (a -> Bool) ->
  -- | One step of work
  (a -> m a) ->
  -- | Initial state
  a ->
  m (a, StepCount, DoneType)
iterateUntilOrTimeout ttl endReached stepAction initSt = do
  startTime <- getMonotonicTime
  go 0 startTime initSt
 where
  go !n startTime !st = do
    if endReached st
      then pure (st, n, EndReached)
      else do
        now <- getMonotonicTime
        if now `diffTime` startTime >= ttl
          then pure (st, n, TimeoutReached)
          else do
            st' <- stepAction st
            go (n + 1) startTime st'

-- | Like 'iterateUntilOrTimeout' but discards the step count and 'DoneType',
-- returning only the final state. Use when you don't need to distinguish a
-- timeout from a normal completion.
iterateUntilOrTimeout_ ::
  MonadMonotonicTime m =>
  -- | Wall-clock budget
  DiffTime ->
  -- | End predicate; when @True@, stops with 'EndReached'
  (a -> Bool) ->
  -- | One step of work
  (a -> m a) ->
  -- | Initial state
  a ->
  m a
iterateUntilOrTimeout_ ttl endReached stepAction st = (\(res, _, _) -> res) <$> iterateUntilOrTimeout ttl endReached stepAction st

-- | @let (val, remaining, steps, done) = foldOrTimeout ttl foldAction initSt xs@ runs a time capped
-- left fold using 'iterateUntilOrTimeout'.
-- Returns the final result in 'val', the 'remaining' elements of 'xs' that wasn't processed,
-- the 'steps' it took to compute it and whether the
-- computation terminated due to the input 'xs' being fully traversed or 'ttl' timeout.
--
-- __Strict__ on the accumulator.
--
-- Naming reflects https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Control-Monad.html#v:foldM
--
-- Properties: propFoldEmptyListTerminates, propFoldEndReachedIffConsumedAll, propFoldStepsMatchConsumed
foldOrTimeout ::
  MonadMonotonicTime m =>
  DiffTime ->
  (b -> a -> m b) ->
  b ->
  [a] ->
  m (b, [a], StepCount, DoneType)
foldOrTimeout ttl foldAction initSt xs =
  (\((xsLeft, finalSt), steps, doneType) -> (finalSt, xsLeft, steps, doneType))
    <$> iterateUntilOrTimeout
      ttl
      (\(xsLeft, !_st) -> null xsLeft)
      ( \(xsLeft, !st) -> case xsLeft of
          (x : xsLeft') -> do
            st' <- foldAction st x
            return (xsLeft', st')
          _ -> return (xsLeft, st)
      )
      (xs, initSt)

type FoldResult b a = (b, [a], StepCount, DoneType)

-- | 'foldOrTimeout' on an empty list terminates immediately: no change, zero steps,
-- 'EndReached', and an empty remainder.
propFoldEmptyListTerminates ::
  (MonadMonotonicTime m, Eq b) =>
  DiffTime ->
  (b -> a -> m b) ->
  b ->
  ExceptT (String, FoldResult b a) m (FoldResult b a)
propFoldEmptyListTerminates ttl foldAction initSt = do
  res@(val, remaining, steps, done) <- lift $ foldOrTimeout ttl foldAction initSt []
  unless (null remaining) $ throwError $ ("remaining must be empty", res)
  unless (steps == 0) $ throwError $ ("steps must be zero", res)
  unless (done == EndReached) $ throwError $ ("done must be EndReached", res)
  unless (val == initSt) $ throwError $ ("val must be initSt", res)
  return res

-- | The step count always equals the number of elements consumed.
propFoldStepsMatchConsumed ::
  MonadMonotonicTime m =>
  DiffTime ->
  (b -> a -> m b) ->
  b ->
  [a] ->
  ExceptT (String, FoldResult b a) m (FoldResult b a)
propFoldStepsMatchConsumed ttl foldAction initSt xs = do
  res@(_, remaining, steps, _) <- lift $ foldOrTimeout ttl foldAction initSt xs
  unless (steps == length xs - length remaining) $
    throwError $
      ("steps must be length xs - length remaining", res)
  return res

-- | 'EndReached' if and only if the entire input was consumed.
propFoldEndReachedIffConsumedAll ::
  MonadMonotonicTime m =>
  DiffTime ->
  (b -> a -> m b) ->
  b ->
  [a] ->
  ExceptT (String, FoldResult b a) m (FoldResult b a)
propFoldEndReachedIffConsumedAll ttl foldAction initSt xs = do
  res@(_, remaining, _, done) <- lift $ foldOrTimeout ttl foldAction initSt xs
  when (done == EndReached) $
    unless (null remaining) $
      throwError $
        ("remaining must be empty when end is reached", res)
  return res

-- | When 'foldAction' is pure then it's just a "Foldable.foldl'" on processed 'xs'
propFoldJustFoldl ::
  (MonadMonotonicTime m, Eq b) =>
  DiffTime ->
  (b -> a -> b) ->
  b ->
  [a] ->
  ExceptT (String, FoldResult b a) m (FoldResult b a)
propFoldJustFoldl ttl foldFn initSt xs = do
  res@(val, _remaining, steps, _done) <-
    lift $ foldOrTimeout ttl (\st x -> pure $ foldFn st x) initSt xs
  unless (val == Foldable.foldl' foldFn initSt (take steps xs)) $
    throwError $
      ("val must be foldl over processed xs", res)
  return res

type IterResult b = (b, StepCount, DoneType)

-- | If 'endReached' holds on the initial state, 'iterateUntilOrTimeout' returns
-- immediately with zero steps and 'EndReached', regardless of the budget.
propIterImmediateEndOnInit ::
  (MonadMonotonicTime m, Eq a) =>
  DiffTime ->
  (a -> m a) ->
  a ->
  ExceptT (String, IterResult a) m (IterResult a)
propIterImmediateEndOnInit ttl stepAction initSt = do
  res@(val, steps, doneType) <- lift $ iterateUntilOrTimeout ttl (const True) stepAction initSt
  unless (steps == 0) $ throwError $ ("steps must be zero", res)
  unless (doneType == EndReached) $ throwError $ ("done must be end reached", res)
  unless (val == initSt) $ throwError $ ("val must be initial st", res)
  return res

-- | The total elapsed time never exceeds @ttl + stepDuration@: the timeout is
-- detected at the top of the loop, so at most one in-flight step can overshoot
-- before the budget breach is observed.
propIterTimeoutOverrunBoundedByOneStep ::
  MonadDelay m =>
  DiffTime ->
  DiffTime ->
  ExceptT (String, (DiffTime, IterResult ())) m (IterResult ())
propIterTimeoutOverrunBoundedByOneStep ttl stepActionDelay = do
  start <- lift getMonotonicTime
  res@(_, steps, doneType) <-
    lift $
      iterateUntilOrTimeout
        ttl
        (const False)
        (const $ threadDelay stepActionDelay)
        ()
  end <- lift getMonotonicTime
  let duration = end `diffTime` start

  unless (doneType == TimeoutReached) $
    throwError $
      ("done type must be due to timeout", (duration, res))
  unless (duration >= max ttl (fromIntegral steps * stepActionDelay)) $
    throwError $
      ( "duration must be equal or longer than max between ttl and steps * stepActionDelay"
      , (duration, res)
      )
  unless (duration <= ttl + stepActionDelay) $
    throwError $
      ("duration must be equal or shorter than ttl + stepActionDelay", (duration, res))

  return res
