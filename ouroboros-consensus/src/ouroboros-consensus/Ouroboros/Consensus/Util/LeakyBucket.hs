{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module implements a “leaky bucket”. One defines a bucket with a
-- capacity and a leaking rate; a race (in the sense of Async) starts against
-- the bucket which leaks at the given rate. The user is provided with a
-- function to refill the bucket by a certain amount. If the bucket ever goes
-- empty, both threads are cancelled.
--
-- This can be used for instance to enforce a minimal rate of a peer: they race
-- against the bucket and refill the bucket by a certain amount whenever they do
-- a “good” action.
--
-- NOTE: Even though the imagery is the same, this is different from what is
-- usually called a \“token bucket\” or \“leaky bucket\” in the litterature
-- where it is mostly used for rate limiting.
--
-- REVIEW: Could be used as leaky bucket used for rate limiting algorithms. All
-- the infrastructure is here (put 'onEmpty' to @pure ()@ and you're good to go)
-- but it has not been tested with that purpose in mind.
--
-- $leakyBucketDesign
module Ouroboros.Consensus.Util.LeakyBucket (
    Config (..)
  , Handlers (..)
  , State (..)
  , atomicallyWithMonotonicTime
  , diffTimeToSecondsRational
  , dummyConfig
  , evalAgainstBucket
  , execAgainstBucket
  , execAgainstBucket'
  , fill'
  , microsecondsPerSecond
  , picosecondsPerSecond
  , runAgainstBucket
  , secondsRationalToDiffTime
  , setPaused'
  , updateConfig'
  ) where

import           Control.Exception (assert)
import           Control.Monad (forever, void, when)
import qualified Control.Monad.Class.MonadSTM.Internal as TVar
import           Control.Monad.Class.MonadTimer (MonadTimer, registerDelay)
import           Control.Monad.Class.MonadTimer.SI (diffTimeToMicrosecondsAsInt)
import           Data.Ratio ((%))
import           Data.Time.Clock (diffTimeToPicoseconds)
import           GHC.Generics (Generic)
import           Ouroboros.Consensus.Util.IOLike hiding (killThread)
import           Ouroboros.Consensus.Util.STM (blockUntilChanged)
import           Prelude hiding (init)

-- | Configuration of a leaky bucket.
data Config m = Config
  { -- | Initial and maximal capacity of the bucket, in number of tokens.
    capacity       :: !Rational,
    -- | Tokens per second leaking off the bucket.
    rate           :: !Rational,
    -- | Whether to fill to capacity on overflow or to do nothing.
    fillOnOverflow :: !Bool,
    -- | A monadic action to trigger when the bucket is empty.
    onEmpty        :: !(m ())
  }
  deriving (Generic)

deriving instance NoThunks (m ()) => NoThunks (Config m)

-- | A configuration for a bucket that does nothing.
dummyConfig :: (Applicative m) => Config m
dummyConfig =
  Config
    { capacity = 0,
      rate = 0,
      fillOnOverflow = True,
      onEmpty = pure ()
    }

-- | State of a leaky bucket, giving the level and the associated time.
data State m = State
  { level            :: !Rational,
    time             :: !Time,
    paused           :: !Bool,
    configGeneration :: !Int,
    config           :: !(Config m)
  }
  deriving (Generic)

deriving instance (NoThunks (m ())) => NoThunks (State m)

-- | A bucket is simply a TVar of a state. The state carries a 'Config' and an
-- integer (a “generation”) to detect changes in the configuration.
type Bucket m = StrictTVar m (State m)

-- | Whether filling the bucket overflew.
data FillResult = Overflew | DidNotOverflow

-- | The handlers to a bucket: contains the API to interact with a running
-- bucket. All the endpoints are STM but require the current time; the easy way
-- to provide this being 'atomicallyWithMonotonicTime'.
data Handlers m = Handlers
  { -- | Refill the bucket by the given amount and returns whether the bucket
    -- overflew. The bucket may silently get filled to full capacity or not get
    -- filled depending on 'fillOnOverflow'.
    fill ::
      !( Rational ->
         Time ->
         STM m FillResult
       ),
    -- | Pause or resume the bucket. Pausing stops the bucket from leaking until
    -- it is resumed. It is still possible to fill it during that time. @setPaused
    -- True@ and @setPaused False@ are idempotent.
    setPaused ::
      !( Bool ->
         Time ->
         STM m ()
       ),
    -- | Dynamically update the level and configuration of the bucket. Updating
    -- the level matters if the capacity changes, in particular. If updating
    -- leave the bucket empty, the action is triggered immediately.
    updateConfig ::
      !( ((Rational, Config m) -> (Rational, Config m)) ->
         Time ->
         STM m ()
       )
  }

-- | Variant of 'fill' already wrapped in 'atomicallyWithMonotonicTime'.
fill' ::
  ( MonadMonotonicTime m,
    MonadSTM m
  ) =>
  Handlers m ->
  Rational ->
  m FillResult
fill' h r = atomicallyWithMonotonicTime $ fill h r

-- | Variant of 'setPaused' already wrapped in 'atomicallyWithMonotonicTime'.
setPaused' ::
  ( MonadMonotonicTime m,
    MonadSTM m
  ) =>
  Handlers m ->
  Bool ->
  m ()
setPaused' h p = atomicallyWithMonotonicTime $ setPaused h p

-- | Variant of 'updateConfig' already wrapped in 'atomicallyWithMonotonicTime'.
updateConfig' ::
  ( MonadMonotonicTime m,
    MonadSTM m
  ) =>
  Handlers m ->
  ((Rational, Config m) -> (Rational, Config m)) ->
  m ()
updateConfig' h f = atomicallyWithMonotonicTime $ updateConfig h f

-- | Create a bucket with the given configuration, then run the action against
-- that bucket. Returns when the action terminates or the bucket empties. In the
-- first case, return the value returned by the action. In the second case,
-- return @Nothing@.
execAgainstBucket ::
  ( MonadDelay m,
    MonadAsync m,
    MonadFork m,
    MonadMask m,
    MonadTimer m,
    NoThunks (m ())
  ) =>
  Config m ->
  (Handlers m -> m a) ->
  m a
execAgainstBucket config action = snd <$> runAgainstBucket config action

-- | Variant of 'execAgainstBucket' that uses a dummy configuration. This only
-- makes sense for actions that use 'updateConfig'.
execAgainstBucket' ::
  ( MonadDelay m,
    MonadAsync m,
    MonadFork m,
    MonadMask m,
    MonadTimer m,
    NoThunks (m ())
  ) =>
  (Handlers m -> m a) ->
  m a
execAgainstBucket' action =
  execAgainstBucket dummyConfig action

-- | Same as 'execAgainstBucket' but returns the 'State' of the bucket when the
-- action terminates. Exposed for testing purposes.
evalAgainstBucket ::
  (MonadDelay m, MonadAsync m, MonadFork m, MonadMask m, MonadTimer m, NoThunks (m ())
  ) =>
  Config m ->
  (Handlers m -> m a) ->
  m (State m)
evalAgainstBucket config action = fst <$> runAgainstBucket config action

-- | Same as 'execAgainstBucket' but also returns the 'State' of the bucket when
-- the action terminates. Exposed for testing purposes.
runAgainstBucket ::
  forall m a.
  ( MonadDelay m,
    MonadAsync m,
    MonadFork m,
    MonadMask m,
    MonadTimer m,
    NoThunks (m ())
  ) =>
  Config m ->
  (Handlers m -> m a) ->
  m (State m, a)
runAgainstBucket config action = do
  leakingPeriodVersionTMVar <- atomically newEmptyTMVar -- see note [Leaky bucket design].
  tid <- myThreadId
  bucket <- init config
  withAsync (leak (readTMVar leakingPeriodVersionTMVar) tid bucket) $ \_ -> do
    atomicallyWithMonotonicTime $ maybeStartThread Nothing leakingPeriodVersionTMVar bucket
    result <-
      action $
        Handlers
          { fill = \r t -> (snd <$>) $ snapshotFill bucket r t,
            setPaused = setPaused bucket,
            updateConfig = updateConfig leakingPeriodVersionTMVar bucket
          }
    state <- atomicallyWithMonotonicTime $ snapshot bucket
    pure (state, result)
  where
    -- Start the thread (that is, write to its 'leakingPeriodVersionTMVar') if it is useful.
    -- Takes a potential old value of the 'leakingPeriodVersionTMVar' as first argument,
    -- which will be increased to help differentiate between restarts.
    maybeStartThread :: Maybe Int -> StrictTMVar m Int -> Bucket m -> Time -> STM m ()
    maybeStartThread mLeakingPeriodVersion leakingPeriodVersionTMVar bucket time = do
      State {config = Config {rate}} <- snapshot bucket time
      when (rate > 0) $ void $ tryPutTMVar leakingPeriodVersionTMVar $ maybe 0 (+ 1) mLeakingPeriodVersion

    setPaused :: Bucket m -> Bool -> Time -> STM m ()
    setPaused bucket paused time = do
      newState <- snapshot bucket time
      writeTVar bucket newState {paused}

    updateConfig ::
      StrictTMVar m Int ->
      Bucket m ->
      ((Rational, Config m) -> (Rational, Config m)) ->
      Time ->
      STM m ()
    updateConfig leakingPeriodVersionTMVar bucket f time = do
      State
        { level = oldLevel,
          paused,
          configGeneration = oldConfigGeneration,
          config = oldConfig
        } <-
        snapshot bucket time
      let (newLevel, newConfig) = f (oldLevel, oldConfig)
          Config {capacity = newCapacity} = newConfig
          newLevel' = clamp (0, newCapacity) newLevel
      writeTVar bucket $
        State
          { level = newLevel',
            time,
            paused,
            configGeneration = oldConfigGeneration + 1,
            config = newConfig
          }
      -- Ensure that 'leakingPeriodVersionTMVar' is empty, then maybe start the thread.
      mLeakingPeriodVersion <- tryTakeTMVar leakingPeriodVersionTMVar
      maybeStartThread mLeakingPeriodVersion leakingPeriodVersionTMVar bucket time

-- | Initialise a bucket given a configuration. The bucket starts full at the
-- time where one calls 'init'.
init ::
  (MonadMonotonicTime m, MonadSTM m, NoThunks (m ())) =>
  Config m ->
  m (Bucket m)
init config@Config {capacity} = do
  time <- getMonotonicTime
  newTVarIO $
    State
      { time,
        level = capacity,
        paused = False,
        configGeneration = 0,
        config = config
      }

-- $leakyBucketDesign
--
-- Note [Leaky bucket design]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The leaky bucket works by running the given action against a thread that
-- makes the bucket leak. Since it would be inefficient to actually
-- remove tokens one by one from the bucket, the 'leak' thread instead looks at
-- the current state of the bucket, computes how much time it would take for the
-- bucket to empty, and then wait that amount of time. Once the wait is over, it
-- recurses, looks at the new state of the bucket, etc. If tokens were given to
-- the bucket via the action, the bucket is not empty and the loop continues.
--
-- This description assumes that two things hold:
--
--  - the bucket must be leaking (ie. rate is strictly positive),
--  - the action can only increase the waiting time (eg. by giving tokens).
--
-- Neither of those properties hold in the general case. Indeed, it is possible
-- for the bucket to have a zero rate or even a negative one (for a more
-- traditional rate limiting bucket, for instance). Conversely, it is possible
-- for the action to lower the waiting time by changing the bucket configuration
-- to one where the rate is higher.
--
-- We fix both those issues with one mechanism, the @leakingPeriodVersionSTM@.
-- It is a computation returning an integer that identifies a version of the
-- configuration that controls the leaking period. If the computation blocks,
-- it means that no configuration has been determined yet.
-- The leak thread first waits until @leakingPeriodVersionSTM@ yields a
-- value, and only then proceeds as described above.
-- Additionally, while waiting for the bucket to empty, the thread monitors
-- for changes to the version of the leaking period, indicating either that the
-- thread should pause running if the @leakingPeriodVersionSTM@ starts blocking
-- again or that the configuration changed as that it might have to wait less
-- long.
--

-- | Neverending computation that runs 'onEmpty' whenever the bucket becomes
-- empty. See note [Leaky bucket design].
leak ::
  ( MonadDelay m,
    MonadCatch m,
    MonadFork m,
    MonadAsync m,
    MonadTimer m
  ) =>
  -- | A computation indicating the version of the configuration affecting the
  -- leaking period. Whenever the configuration changes, the returned integer
  -- must be incremented. While no configuration is available, the computation
  -- should block. Blocking is allowed at any time, and it will cause the
  -- leaking to pause.
  STM m Int ->
  -- | The 'ThreadId' of the action's thread, which is used to throw exceptions
  -- at it.
  ThreadId m ->
  Bucket m ->
  m ()
leak leakingPeriodVersionSTM actionThreadId bucket = forever $ do
      -- Block until we are allowed to run.
      leakingPeriodVersion <- atomically leakingPeriodVersionSTM
      -- NOTE: It is tempting to group this @atomically@ and
      -- @atomicallyWithMonotonicTime@ into one; however, because the former is
      -- blocking, the latter could get a _very_ inaccurate time, which we
      -- cannot afford.
      State {level, configGeneration = oldConfigGeneration, config = Config {rate, onEmpty}} <-
        atomicallyWithMonotonicTime $ snapshot bucket
      let timeToWait = secondsRationalToDiffTime (level / rate)
          timeToWaitMicroseconds = diffTimeToMicrosecondsAsInt timeToWait
      -- NOTE: It is possible that @timeToWait <= 1µs@ while @level > 0@ when
      -- @level@ is extremely small.
      if level <= 0 || timeToWaitMicroseconds <= 0
        then do
          handle (\(e :: SomeException) -> throwTo actionThreadId e) onEmpty
          -- We have run the action on empty, there is nothing left to do,
          -- unless someone changes the configuration.
          void $ atomically $ blockUntilChanged configGeneration oldConfigGeneration $ readTVar bucket
        else
          -- Wait for the bucket to empty, or for the thread to be stopped or
          -- restarted. Beware not to call 'registerDelay' with argument 0, that
          -- is ensure that @timeToWaitMicroseconds > 0@.
          assert (timeToWaitMicroseconds > 0) $ do
            varTimeout <- registerDelay timeToWaitMicroseconds
            atomically $
              (check =<< TVar.readTVar varTimeout)
                `orElse`
              (void $ blockUntilChanged id leakingPeriodVersion leakingPeriodVersionSTM)

-- | Take a snapshot of the bucket, that is compute its state at the current
-- time.
snapshot ::
  ( MonadSTM m
  ) =>
  Bucket m ->
  Time ->
  STM m (State m)
snapshot bucket newTime = fst <$> snapshotFill bucket 0 newTime

-- | Same as 'snapshot' but also adds the given quantity to the resulting
-- level and returns whether this action overflew the bucket.
--
-- REVIEW: What to do when 'toAdd' is negative?
snapshotFill ::
  ( MonadSTM m
  ) =>
  Bucket m ->
  Rational ->
  Time ->
  STM m (State m, FillResult)
snapshotFill bucket toAdd newTime = do
  State {level, time, paused, configGeneration, config = config} <- readTVar bucket
  let Config {rate, capacity, fillOnOverflow} = config
      elapsed = diffTime newTime time
      leaked = if paused then 0 else (diffTimeToSecondsRational elapsed * rate)
      levelLeaked = clamp (0, capacity) (level - leaked)
      levelFilled = clamp (0, capacity) (levelLeaked + toAdd)
      overflew = levelLeaked + toAdd > capacity
      newLevel = if not overflew || fillOnOverflow then levelFilled else levelLeaked
      !newState = State {time = newTime, level = newLevel, paused, configGeneration, config}
  writeTVar bucket newState
  pure (newState, if overflew then Overflew else DidNotOverflow)

-- | Convert a 'DiffTime' to a 'Rational' number of seconds. This is similar to
-- 'diffTimeToSeconds' but with picoseconds precision.
diffTimeToSecondsRational :: DiffTime -> Rational
diffTimeToSecondsRational = (% picosecondsPerSecond) . diffTimeToPicoseconds

-- | Alias of 'realToFrac' to make code more readable and typing more explicit.
secondsRationalToDiffTime :: Rational -> DiffTime
secondsRationalToDiffTime = realToFrac

-- | Helper around 'getMonotonicTime' and 'atomically'.
atomicallyWithMonotonicTime ::
  ( MonadMonotonicTime m,
    MonadSTM m
  ) =>
  (Time -> STM m b) ->
  m b
atomicallyWithMonotonicTime f =
  atomically . f =<< getMonotonicTime

-- NOTE: Needed for GHC 8
clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) x = min high (max low x)

-- | Number of microseconds in a second (@10^6@).
microsecondsPerSecond :: Integer
microsecondsPerSecond = 1_000_000

-- | Number of picoseconds in a second (@10^12@).
picosecondsPerSecond :: Integer
picosecondsPerSecond = 1_000_000_000_000
