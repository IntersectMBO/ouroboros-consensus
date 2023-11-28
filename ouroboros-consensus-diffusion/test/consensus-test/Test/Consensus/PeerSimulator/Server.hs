{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.PeerSimulator.Server (
    ScheduledServer (..)
  , awaitOnlineState
  , ensureCurrentState
  , runHandler
  , runHandlerWithTrace
  ) where
import           Control.Tracer (Tracer (Tracer), traceWith)
import           Data.Foldable (traverse_)
import           Ouroboros.Consensus.Util.Condense (Condense (condense))
import           Ouroboros.Consensus.Util.IOLike
import           Test.Consensus.PeerSimulator.Trace (traceUnitWith)

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
-- wait for the next tick to be triggered in 'awaitNextState'.
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

runHandler ::
  IOLike m =>
  Condense a =>
  ScheduledServer m a ->
  String ->
  (a -> STM m (Maybe msg, [String])) ->
  (msg -> m h) ->
  m h
runHandler server@ScheduledServer{ssTracer, ssPeerId} handlerName handler dispatchMessage =
  run
  where
    run = do
      currentState <- ensureCurrentState server
      trace ("handling " ++ handlerName)
      trace $ "  state is " ++ condense currentState
      maybe restart dispatchMessage =<< runHandlerWithTrace handlerTracer (handler currentState)

    restart = do
      trace "  cannot serve at this point; waiting for node state and starting again"
      awaitOnlineState server *> run

    trace = traceWith ssTracer
    handlerTracer = Tracer $ traceUnitWith ssTracer (handlerName ++ " handler for " ++ ssPeerId)
