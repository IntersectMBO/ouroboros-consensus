{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | This module contains code that is generic to any “scheduled server” (think
-- scheduled ChainSync or BlockFetch server). A scheduled server keeps track of
-- the current state of a point schedule and wakes up when new ticks arise. It
-- processes as many messages there are via its domain-specific handlers; once
-- there is nothing new to process, or what needs to process requires a
-- different state of the point schedule, the scheduled server goes back to
-- sleep, awaiting another tick.
module Test.Consensus.PeerSimulator.ScheduledServer (
    ScheduledServer (..)
  , awaitOnlineState
  , ensureCurrentState
  , runHandler
  , runHandlerWithTrace
  ) where
import           Control.Tracer (Tracer (Tracer), traceWith)
import           Data.Foldable (traverse_)
import           Ouroboros.Consensus.Util.Condense (Condense (condense))
import           Ouroboros.Consensus.Util.IOLike (IOLike,
                     MonadSTM (STM, atomically))

data ScheduledServer m a =
  ScheduledServer {
    ssPeerId       :: String,
    ssCurrentState :: STM m (Maybe a),
    ssTickStarted  :: STM m (),
    ssTracer       :: Tracer m String
  }

nextTickState :: IOLike m => ScheduledServer m a -> m (Maybe a)
nextTickState ScheduledServer {ssCurrentState, ssTickStarted} =
  atomically (ssTickStarted >> ssCurrentState)

retryOffline :: IOLike m => ScheduledServer m a -> Maybe a -> m a
retryOffline server = maybe (awaitOnlineState server) pure

-- | Block until the peer simulator has updated the concurrency primitive that
-- indicates that it's this peer's server's turn in the point schedule.
-- If the new state is 'Nothing', the point schedule has declared this peer as
-- offline for the current tick, so it will not resume operation and wait for
-- the next update.
awaitOnlineState :: IOLike m => ScheduledServer m a -> m a
awaitOnlineState server =
  retryOffline server =<< nextTickState server

-- | Fetch the current state from the STM action, and if it is 'Nothing',
-- wait for the next tick to be triggered in 'awaitOnlineState'.
--
-- Since processing of a tick always ends when the handler finishes
-- after serving the last point, this function is only relevant for the
-- initial state update.
ensureCurrentState :: IOLike m => ScheduledServer m a -> m a
ensureCurrentState server =
  retryOffline server =<< atomically (ssCurrentState server)

-- | Handler functions are STM actions for the usual race condition reasons,
-- which means that they cannot emit trace messages.
--
-- For that reason, we allow them to return their messages alongside the
-- protocol result and emit them here.
runHandlerWithTrace ::
  IOLike m =>
  Tracer m String ->
  STM m (a, [String]) ->
  m a
runHandlerWithTrace tracer handler = do
  (result, handlerMessages) <- atomically handler
  traverse_ (traceWith tracer) handlerMessages
  pure result

-- | Run a peer server's message handler by fetching state from the scheduler's STM interface.
--
-- The handler is an STM action that returns a protocol result and log messages.
--
-- If the result is 'Nothing', the server's activity for the current tick is complete
-- and we listen for the scheduler's signal to start the next tick, which we continue without
-- updating the protocol handler (in @restart@).
--
-- Otherwise, the result is passed to @dispatchMessage@, which produces a native protocol handler
-- message with the server's continuation in it.
runHandler ::
  IOLike m =>
  Condense a =>
  ScheduledServer m a ->
  String ->
  (a -> STM m (Maybe msg, [String])) ->
  (msg -> m h) ->
  m h
runHandler server@ScheduledServer{ssTracer} handlerName handler dispatchMessage =
  run
  where
    run = do
      currentState <- ensureCurrentState server
      trace ("handling " ++ handlerName)
      trace ("  state is " ++ condense currentState)
      maybe restart dispatchMessage =<< runHandlerWithTrace handlerTracer (handler currentState)

    restart = do
      trace "  cannot serve at this point; waiting for node state and starting again"
      awaitOnlineState server *> run

    trace = traceWith ssTracer
    handlerTracer = Tracer $ \ msg -> traceWith ssTracer (handlerName ++ " | " ++ msg)
