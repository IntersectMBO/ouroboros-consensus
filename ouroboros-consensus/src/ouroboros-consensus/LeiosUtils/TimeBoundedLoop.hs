{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Time-bounded iteration.
module LeiosUtils.TimeBoundedLoop
  ( iterateUntilOrTimeout
  , DoneType (..)
  , StepCount
  , iterateUntilOrTimeout'
  , propImmediateEndOnInit
  , propTimeoutOverrunBoundedByOneStep
  ) where

import Control.Monad.Class.MonadTime.SI (MonadMonotonicTime (getMonotonicTime), diffTime)
import Control.Monad.Class.MonadTimer.SI (DiffTime, MonadDelay (..))

-- | Why 'iterateUntilOrTimeout' stopped.
data DoneType
  = -- | The time budget was exhausted before the end of computation was reached.
    TimeoutReached
  | -- | The computation reached an end before the time budget was exhausted.
    EndReached
  deriving stock (Show, Eq)

-- | Number of 'stepAction' invocations performed.
type StepCount = Int

-- | @iterateUntilOrTimeout ttl endReached stepAction initSt@ runs
-- @stepAction@ on a state repeatedly until either @endReached@ holds or the
-- wall-clock budget @ttl@ is exceeded.
--
-- The check order per iteration is:
--
--   1. End check (@endReached@).
--   2. Timeout check (against @ttl@).
--   3. Step (@stepAction@).
--
-- In particular, if @endReached initSt@ is already @True@ on entry, the
-- function returns immediately with 'EndReached' and @'StepCount' = 0@.
--
-- __Single-threaded__: everything runs in one thread.  If @stepAction@ blocks
-- longer than @ttl@, the timeout is detected only on the /next/ iteration —
-- the in-flight call is not cancelled.  The total elapsed time can therefore
-- exceed @ttl@ by at most one step duration.
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

iterateUntilOrTimeout' ::
  MonadMonotonicTime m =>
  -- | Wall-clock budget
  DiffTime ->
  -- | End predicate; when @True@, stops with 'EndReached'
  (a -> Bool) ->
  -- | One step of work,
  (a -> m a) ->
  -- | Initial state
  a ->
  m a
iterateUntilOrTimeout' ttl endReached stepAction st = (\(res, _, _) -> res) <$> iterateUntilOrTimeout ttl endReached stepAction st

propImmediateEndOnInit ::
  MonadMonotonicTime m =>
  -- | Wall-clock budget
  DiffTime ->
  -- | One step of work
  (a -> m a) ->
  -- | Initial state
  a ->
  m Bool
propImmediateEndOnInit ttl stepAction st = do
  (_, steps, doneType) <- iterateUntilOrTimeout ttl (const True) stepAction st
  return $ steps == 0 && doneType == EndReached

propTimeoutOverrunBoundedByOneStep ::
  MonadDelay m =>
  -- | Wall-clock budget
  DiffTime ->
  -- | Duration of one step of work
  DiffTime ->
  m Bool
propTimeoutOverrunBoundedByOneStep ttl stepActionDelay = do
  start <- getMonotonicTime
  (_, steps, doneType) <-
    iterateUntilOrTimeout ttl (const False) (const $ threadDelay stepActionDelay) ()
  end <- getMonotonicTime
  let duration = end `diffTime` start

  return $
    doneType == TimeoutReached
      && duration >= max ttl (fromIntegral steps * stepActionDelay)
      && duration < ttl + stepActionDelay
