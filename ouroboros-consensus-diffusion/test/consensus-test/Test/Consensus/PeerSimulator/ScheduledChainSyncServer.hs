{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | A ChainSync protocol server that allows external scheduling of its
-- operations, while deferring the implementation of the message handler
-- logic to a simplified, abstract interface provided as a parameter.
module Test.Consensus.PeerSimulator.ScheduledChainSyncServer (
    ChainSyncServerHandlers (..)
  , FindIntersect (..)
  , RequestNext (..)
  , ScheduledChainSyncServer (..)
  , runScheduledChainSyncServer
  ) where

import           Control.Tracer (Tracer (Tracer), traceWith)
import           Data.Foldable (traverse_)
import           Ouroboros.Consensus.Block.Abstract (Point (..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadSTM (STM),
                     atomically)
import           Ouroboros.Network.Block (Tip (..))
import           Ouroboros.Network.Protocol.ChainSync.Server
                     (ChainSyncServer (..),
                     ServerStIdle (ServerStIdle, recvMsgDoneClient, recvMsgFindIntersect, recvMsgRequestNext),
                     ServerStIntersect (SendMsgIntersectFound, SendMsgIntersectNotFound),
                     ServerStNext (SendMsgRollBackward, SendMsgRollForward))
import           Test.Consensus.PeerSimulator.Trace (terseHeader, traceUnitWith)
import           Test.Util.TestBlock (Header (..), TestBlock)

-- | Pure representation of the messages produced by the handler for the @StNext@
-- protocol state of a ChainSync server.
data RequestNext =
  RollForward (Header TestBlock) (Tip TestBlock)
  |
  RollBackward (Point TestBlock) (Tip TestBlock)
  |
  AwaitReply
  deriving (Eq, Show)

-- | Pure representation of the messages produced by the handler for the @StIntersect@
-- protocol state of a ChainSync server.
data FindIntersect =
  IntersectFound (Point TestBlock) (Tip TestBlock)
  |
  IntersectNotFound (Tip TestBlock)
  deriving (Eq, Show)

-- | Handlers for the request a ChainSync server might receive from a client.
-- These take an abstract argument that corresponds to the state of a point
-- schedule tick and return the simplified protocol message types.
--
-- See 'runHandlerWithTrace' for the meaning of @[String]@.
data ChainSyncServerHandlers m a =
  ChainSyncServerHandlers {
    csshRequestNext      :: a -> STM m (Maybe RequestNext, [String]),
    csshFindIntersection :: a -> [Point TestBlock] -> STM m (FindIntersect, [String])
  }

-- | Resources used by a ChainSync server mock.
data ScheduledChainSyncServer m a =
  ScheduledChainSyncServer {
    scssName           :: String,
    scssCurrentState   :: STM m (Maybe a),
    scssAwaitNextState :: STM m (Maybe a),
    scssHandlers       :: ChainSyncServerHandlers m a,
    scssTracer         :: Tracer m String
  }

-- | Block until the peer simulator has updated the concurrency primitive that
-- indicates that it's this peer's server's turn in the point schedule.
-- If the new state is 'Nothing', the point schedule has declared this peer as
-- offline for the current tick, so it will not resume operation and wait for
-- the next update.
awaitNextState ::
  IOLike m =>
  ScheduledChainSyncServer m a ->
  m a
awaitNextState server@ScheduledChainSyncServer{scssAwaitNextState} = do
  atomically scssAwaitNextState >>= \case
    Nothing       -> awaitNextState server
    Just resource -> pure resource

-- | Fetch the current state from the STM action, and if it is 'Nothing',
-- wait for the next tick to be triggered in 'awaitNextState'.
--
-- Since processing of a tick always ends when the RequestNext handler finishes
-- after serving the last header, this function is only relevant for the
-- initial state update.
ensureCurrentState ::
  IOLike m =>
  ScheduledChainSyncServer m a ->
  m a
ensureCurrentState server@ScheduledChainSyncServer{scssCurrentState} =
  atomically scssCurrentState >>= \case
    Nothing -> awaitNextState server
    Just resource -> pure resource

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

-- | Declare a mock ChainSync protocol server in its typed-protocols encoding
-- that halts and resumes operation in response to an external scheduler,
-- signalling via a blocking STM action that is sequenced by calling
-- 'awaitNextState' in 'recvMsgRequestNext' after the current state has been
-- fully processed, which is indicated by the handler for this message.
--
-- Handlers are supplied as a record of STM callbacks ('ChainSyncServerHandlers')
-- by the caller.
--
-- This architecture allows the server's behavior to be defined with a simple
-- interface separated from the scheduling and protocol plumbing infrastructure.
scheduledChainSyncServer ::
  Condense a =>
  IOLike m =>
  ScheduledChainSyncServer m a ->
  ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m ()
scheduledChainSyncServer server@ScheduledChainSyncServer {scssHandlers, scssTracer, scssName} =
  go
  where
    ChainSyncServerHandlers {csshRequestNext, csshFindIntersection} = scssHandlers

    go =
      ChainSyncServer $ pure ServerStIdle {
          recvMsgRequestNext
        , recvMsgFindIntersect
        , recvMsgDoneClient
      }

    recvMsgRequestNext = do
      currentState <- ensureCurrentState server
      trace "handling MsgRequestNext"
      trace $ "  state is " ++ condense currentState
      runHandlerWithTrace requestNextTracer (csshRequestNext currentState) >>= \case
        Just (RollForward header tip) -> do
          trace $ "  gotta serve " ++ terseHeader header
          trace $ "  tip is      " ++ condense tip
          trace "done handling MsgRequestNext"
          pure $ Left $ SendMsgRollForward header tip go
        Just (RollBackward point tip) -> do
          trace "done handling MsgRequestNext"
          pure $ Left $ SendMsgRollBackward point tip go
        Just AwaitReply -> do
          trace "done handling MsgRequestNext"
          pure $ Right $ do -- beginning of the continuation
            restart >>= \case
              -- If we get 'Right', then we still do not have anything to serve
              -- and we loop; what 'Right' contains is the continuation starting
              -- at 'do' above; by unwrapping the 'Right', we do not send
              -- another AwaitReply message (which Typed Protocols does not
              -- allow anyway).
              Right cont -> cont
              Left msg -> pure msg
        Nothing -> do
          trace "  cannot serve at this point; waiting for node state and starting again"
          restart
      where
        -- Yield control back to the scheduler, then wait for the next state and
        -- continue processing the client's current 'MsgRequestNext'.
        restart = awaitNextState server *> recvMsgRequestNext

    recvMsgFindIntersect pts = do
      currentState <- ensureCurrentState server
      trace "handling MsgFindIntersect"
      runHandlerWithTrace findIntersectTracer (csshFindIntersection currentState pts) >>= \case
        IntersectNotFound tip -> do
          trace "  no intersection found"
          trace "done handling MsgFindIntersect"
          pure $ SendMsgIntersectNotFound tip go
        IntersectFound intersection tip -> do
          trace $ "  intersection found: " ++ condense intersection
          trace "done handling MsgFindIntersect"
          pure $ SendMsgIntersectFound intersection tip go

    recvMsgDoneClient = do
      trace "received MsgDoneClient"
      pure ()

    trace = traceWith mainTracer
    requestNextTracer = Tracer $ traceUnitWith scssTracer ("RequestNext handler for " ++ scssName)
    findIntersectTracer = Tracer $ traceUnitWith scssTracer ("FindIntersect handler for " ++ scssName)
    mainTracer = Tracer $ traceUnitWith scssTracer ("ScheduledChainSyncServer " ++ scssName)

-- | Construct a ChainSync server for the peer simulator.
--
-- See 'scheduledChainSyncServer'.
runScheduledChainSyncServer ::
  Condense a =>
  IOLike m =>
  String ->
  STM m (Maybe a) ->
  STM m (Maybe a) ->
  Tracer m String ->
  ChainSyncServerHandlers m a ->
  ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m ()
runScheduledChainSyncServer scssName scssAwaitNextState scssCurrentState scssTracer scssHandlers =
  scheduledChainSyncServer ScheduledChainSyncServer {
    scssName,
    scssAwaitNextState,
    scssCurrentState,
    scssTracer,
    scssHandlers
  }
