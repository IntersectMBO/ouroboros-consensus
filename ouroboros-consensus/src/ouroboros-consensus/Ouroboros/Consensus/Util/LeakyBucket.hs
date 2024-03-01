{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
-- usually called a “token bucket” or “leaky bucket” in the litterature where it
-- is mostly used for rate limiting.
--
-- REVIEW: Could be used as leaky bucket used for rate limiting algorithms. All
-- the infrastructure is here (put 'onEmpty' to @pure ()@ and you're good to go)
-- but it has not been tested with that purpose in mind.
module Ouroboros.Consensus.Util.LeakyBucket (
    Config (..)
  , Handlers (..)
  , State (..)
  , diffTimeToSecondsRational
  , dummyConfig
  , evalAgainstBucket
  , execAgainstBucket
  , runAgainstBucket
  , secondsRationalToDiffTime
  ) where

import           Data.Ratio ((%))
import           Data.Time (DiffTime)
import           Data.Time.Clock (diffTimeToPicoseconds)
import           Ouroboros.Consensus.Util.IOLike
                     (MonadAsync (Async, async, uninterruptibleCancel),
                     MonadCatch (handle), MonadDelay (threadDelay),
                     MonadFork (throwTo), MonadMask, MonadMonotonicTime,
                     MonadSTM, MonadThread (ThreadId, myThreadId),
                     MonadThrow (finally), SomeException, StrictTVar, Time,
                     atomically, diffTime, getMonotonicTime, readTVar,
                     readTVarIO, uncheckedNewTVarM, writeTVar)
import           Prelude hiding (init)

-- | Configuration of a leaky bucket.
data Config m = Config
  { -- | Initial and maximal capacity of the bucket, in number of tokens.
    capacity       :: Rational,
    -- | Tokens per second leaking off the bucket.
    rate           :: Rational,
    -- | Whether to fill to capacity on overflow or to do nothing.
    fillOnOverflow :: Bool,
    -- | A monadic action to trigger when the bucket is empty.
    onEmpty        :: m ()
  }

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
data State cfg = State
  { level  :: Rational,
    time   :: Time,
    paused :: Bool,
    config :: cfg
  }
  deriving (Eq, Show)

-- | A bucket is simply a TVar of a state.
type Bucket m = StrictTVar m (State (Config m))

-- | Whether filling the bucket overflew.
newtype Overflew = Overflew Bool

-- | The handlers to a bucket: contains the API to interact with a running
-- bucket.
data Handlers m = Handlers
  { -- | Refill the bucket by the given amount and returns whether the bucket
    -- overflew. The bucket may silently get filled to full capacity or not get
    -- filled depending on 'fillOnOverflow'.
    fill         :: Rational -> m Overflew,
    -- | Pause or resume the bucket. Pausing stops the bucket from leaking until
    -- it is resumed. It is still possible to fill it during that time. @setPaused
    -- True@ and @setPaused False@ are idempotent.
    setPaused    :: Bool -> m (),
    -- | Dynamically update the configuration of the bucket.
    updateConfig :: (Config m -> Config m) -> m ()
  }

-- | Create a bucket with the given configuration, then run the action against
-- that bucket. Returns when the action terminates or the bucket empties. In the
-- first case, return the value returned by the action. In the second case,
-- return @Nothing@.
execAgainstBucket ::
  (MonadDelay m, MonadAsync m, MonadFork m, MonadMask m) =>
  Config m ->
  (Handlers m -> m a) ->
  m a
execAgainstBucket config action = snd <$> runAgainstBucket config action

-- | Same as 'execAgainstBucket' but returns the 'State' of the bucket when the
-- action terminates. Exposed for testing purposes.
evalAgainstBucket ::
  (MonadDelay m, MonadAsync m, MonadFork m, MonadMask m) =>
  Config m ->
  (Handlers m -> m a) ->
  m (State (Config m))
evalAgainstBucket config action = fst <$> runAgainstBucket config action

-- | Same as 'execAgainstBucket' but also returns the 'State' of the bucket when
-- the action terminates. Exposed for testing purposes.
runAgainstBucket ::
  forall m a.
  (MonadDelay m, MonadAsync m, MonadFork m, MonadMask m) =>
  Config m ->
  (Handlers m -> m a) ->
  m (State (Config m), a)
runAgainstBucket config action = do
  bucket <- init config
  tid <- myThreadId
  thread <- uncheckedNewTVarM Nothing
  startThread thread bucket tid
  finally
    ( do
        result <-
          action $
            Handlers
              { fill = (snd <$>) . snapshotFill bucket,
                setPaused = setPaused bucket,
                updateConfig = updateConfig thread bucket tid
              }
        state <- snapshot bucket
        pure (state, result)
    )
    (stopThread thread)
  where
    startThread :: StrictTVar m (Maybe (Async m ())) -> Bucket m -> ThreadId m -> m ()
    startThread thread bucket tid =
      readTVarIO thread >>= \case
        Just _ -> error "LeakyBucket: startThread called when a thread is already running"
        Nothing -> (atomically . writeTVar thread) =<< leak bucket tid

    stopThread :: StrictTVar m (Maybe (Async m ())) -> m ()
    stopThread thread =
      readTVarIO thread >>= \case
        Just thread' -> uninterruptibleCancel thread'
        Nothing -> pure ()

    setPaused :: Bucket m -> Bool -> m ()
    setPaused bucket paused = do
      newState <- snapshot bucket
      atomically $ writeTVar bucket newState {paused}

    updateConfig :: StrictTVar m (Maybe (Async m ())) -> Bucket m -> ThreadId m -> (Config m -> Config m) -> m ()
    updateConfig thread bucket tid = \f -> do
      -- FIXME: All of that should be in one STM transaction.
      State {level, time, paused, config = oldConfig} <- snapshot bucket
      let newConfig@Config {capacity = newCapacity, rate = newRate} = f oldConfig
          newLevel = min newCapacity level
      if
        | newRate <= 0 -> stopThread thread
        | newRate > rate oldConfig -> stopThread thread >> startThread thread bucket tid
        | otherwise -> pure ()
      atomically $ writeTVar bucket State {level = newLevel, time, paused, config = newConfig}

-- | Initialise a bucket given a configuration. The bucket starts full at the
-- time where one calls 'init'.
init :: (MonadMonotonicTime m, MonadSTM m) => Config m -> m (Bucket m)
init config@Config {capacity} = do
  time <- getMonotonicTime
  uncheckedNewTVarM $ State {time, level = capacity, paused = False, config}

-- | Monadic action that calls 'threadDelay' until the bucket is empty, then
-- returns @()@. It receives the 'ThreadId' argument of the action's thread,
-- which it uses to throw exceptions at it.
leak :: (MonadDelay m, MonadCatch m, MonadFork m, MonadAsync m) => Bucket m -> ThreadId m -> m (Maybe (Async m ()))
leak bucket actionThreadId = do
  State {config = Config {rate}} <- snapshot bucket
  if rate <= 0
    then pure Nothing
    else Just <$> async go
  where
    go = do
      State {level, config = Config {rate, onEmpty}} <- snapshot bucket
      let timeToWait = secondsRationalToDiffTime (level / rate)
      -- NOTE: It is possible that @timeToWait == 0@ while @level > 0@ when @level@
      -- is so tiny that @level / rate@ rounds down to 0 picoseconds. In that case,
      -- it is safe to assume that it is just zero.
      if level <= 0 || timeToWait == 0
        then handle (\(e :: SomeException) -> throwTo actionThreadId e) onEmpty
        else threadDelay timeToWait >> go

-- | Take a snapshot of the bucket, that is compute its state at the current
-- time.
snapshot :: (MonadSTM m, MonadMonotonicTime m) => Bucket m -> m (State (Config m))
snapshot bucket = fst <$> snapshotFill bucket 0

-- | Same as 'snapshot' but also adds the given quantity to the resulting
-- level and returns whether this action overflew the bucket.
--
-- REVIEW: What to do when 'toAdd' is negative?
--
-- REVIEW: Really, this should all be an STM transaction. Now there is the risk
-- that two snapshot-taking transactions interleave with the time measurement to
-- get a slightly imprecise state (which is not the worst because everything
-- should happen very fast). There is also the bigger risk that when we snapshot
-- and then do something (eg. in the 'setPaused' handler) we interleave with
-- something else. It cannot easily be an STM transaction, though, because we
-- need to measure the time, and @io-classes@'s STM does not allow running IO in
-- an STM.
snapshotFill :: (MonadSTM m, MonadMonotonicTime m) => Bucket m -> Rational -> m (State (Config m), Overflew)
snapshotFill bucket toAdd = do
  newTime <- getMonotonicTime
  atomically $ do
    State {level, time, paused, config} <- readTVar bucket
    let Config {rate, capacity, fillOnOverflow} = config
        elapsed = diffTime newTime time
        leaked = if paused then 0 else (diffTimeToSecondsRational elapsed * rate)
        levelLeaked = max 0 (level - leaked)
        levelFilled = min capacity (levelLeaked + toAdd)
        overflew = levelLeaked + toAdd > capacity
        newLevel = if not overflew || fillOnOverflow then levelFilled else levelLeaked
        newState = State {time = newTime, level = newLevel, paused, config}
    writeTVar bucket newState
    pure (newState, Overflew overflew)

-- | Convert a 'DiffTime' to a 'Rational' number of seconds. This is similar to
-- 'diffTimeToSeconds' but with picoseconds precision.
diffTimeToSecondsRational :: DiffTime -> Rational
diffTimeToSecondsRational = (% 1_000_000_000_000) . diffTimeToPicoseconds

-- | Alias of 'realToFrac' to make code more readable and typing more explicit.
secondsRationalToDiffTime :: Rational -> DiffTime
secondsRationalToDiffTime = realToFrac
