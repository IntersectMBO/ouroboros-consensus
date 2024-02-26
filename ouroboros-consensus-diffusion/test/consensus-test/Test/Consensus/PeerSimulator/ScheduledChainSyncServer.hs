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
import           Ouroboros.Consensus.Block.Abstract (Point (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadSTM (STM))
import           Ouroboros.Network.Block (Tip (..))
import           Ouroboros.Network.Protocol.ChainSync.Server
                     (ChainSyncServer (..),
                     ServerStIdle (ServerStIdle, recvMsgDoneClient, recvMsgFindIntersect, recvMsgRequestNext),
                     ServerStIntersect (SendMsgIntersectFound, SendMsgIntersectNotFound),
                     ServerStNext (SendMsgRollBackward, SendMsgRollForward))
import           Test.Consensus.PeerSimulator.ScheduledServer
                     (ScheduledServer (..), awaitOnlineState, runHandler)
import           Test.Consensus.PeerSimulator.Trace
                     (TraceEvent (TraceScheduledChainSyncServerEvent),
                     TraceScheduledChainSyncServerEvent (..))
import           Test.Consensus.PointSchedule (NodeState)
import           Test.Consensus.PointSchedule.Peers (PeerId)
import           Test.Util.TestBlock (Header (..), TestBlock)

-- | Pure representation of the messages produced by the handler for the @StNext@
-- protocol state of a ChainSync server.
data RequestNext =
  RollForward (Header TestBlock) (Tip TestBlock)
  |
  RollBackward (Point TestBlock) (Tip TestBlock)
  |
  AwaitReply

-- | Pure representation of the messages produced by the handler for the @StIntersect@
-- protocol state of a ChainSync server.
data FindIntersect =
  IntersectFound (Point TestBlock) (Tip TestBlock)
  |
  IntersectNotFound (Tip TestBlock)

-- | Handlers for the request a ChainSync server might receive from a client.
-- These take an abstract argument that corresponds to the state of a point
-- schedule tick and return the simplified protocol message types.
--
-- See 'runHandlerWithTrace' for the meaning of @[String]@.
data ChainSyncServerHandlers m state =
  ChainSyncServerHandlers {
    csshRequestNext      :: state -> STM m (Maybe RequestNext, [TraceScheduledChainSyncServerEvent state TestBlock]),
    csshFindIntersection :: [Point TestBlock] -> state -> STM m (Maybe FindIntersect, [TraceScheduledChainSyncServerEvent state TestBlock])
  }

-- | Resources used by a ChainSync server mock.
data ScheduledChainSyncServer m state =
  ScheduledChainSyncServer {
    scssServer   :: ScheduledServer m state,
    scssTracer   :: Tracer m (TraceScheduledChainSyncServerEvent state TestBlock),
    scssHandlers :: ChainSyncServerHandlers m state
  }

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
  IOLike m =>
  ScheduledChainSyncServer m a ->
  ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m ()
scheduledChainSyncServer ScheduledChainSyncServer {scssHandlers, scssTracer, scssServer} =
  go
  where
    ChainSyncServerHandlers {csshRequestNext, csshFindIntersection} = scssHandlers

    go =
      ChainSyncServer $ pure ServerStIdle {
          recvMsgRequestNext
        , recvMsgFindIntersect
        , recvMsgDoneClient
      }

    recvMsgRequestNext =
      runHandler scssServer "MsgRequestNext" csshRequestNext scssTracer $ \case
        RollForward header tip -> do
          trace $ TraceRollForward header tip
          pure $ Left $ SendMsgRollForward header tip go
        RollBackward point tip -> do
          trace $ TraceRollBackward point tip
          pure $ Left $ SendMsgRollBackward point tip go
        AwaitReply ->
          pure $ Right $ do -- beginning of the continuation
            restart >>= \case
              -- If we get 'Right', then we still do not have anything to serve
              -- and we loop; what 'Right' contains is the continuation starting
              -- at 'do' above; by unwrapping the 'Right', we do not send
              -- another AwaitReply message (which Typed Protocols does not
              -- allow anyway).
              Right cont -> cont
              Left msg -> pure msg
      where
        -- Yield control back to the scheduler, then wait for the next state and
        -- continue processing the client's current 'MsgRequestNext'.
        restart = awaitOnlineState scssServer *> recvMsgRequestNext

    recvMsgFindIntersect pts =
      runHandler scssServer "MsgFindIntersect" (csshFindIntersection pts) scssTracer $ \case
        IntersectNotFound tip -> do
          trace TraceIntersectionNotFound
          pure $ SendMsgIntersectNotFound tip go
        IntersectFound intersection tip -> do
          trace $ TraceIntersectionFound intersection
          pure $ SendMsgIntersectFound intersection tip go

    recvMsgDoneClient =
      trace TraceClientIsDone

    trace = traceWith scssTracer

-- | Construct a ChainSync server for the peer simulator.
--
-- See 'scheduledChainSyncServer'.
runScheduledChainSyncServer ::
  IOLike m =>
  PeerId ->
  STM m () ->
  STM m (Maybe (NodeState TestBlock)) ->
  Tracer m (TraceEvent TestBlock) ->
  ChainSyncServerHandlers m (NodeState TestBlock) ->
  ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m ()
runScheduledChainSyncServer ssPeerId ssTickStarted ssCurrentState tracer scssHandlers =
  scheduledChainSyncServer ScheduledChainSyncServer {
    scssServer = ScheduledServer {
      ssPeerId,
      ssTickStarted,
      ssCurrentState,
      ssCommonTracer = Tracer (traceWith tracer . TraceScheduledChainSyncServerEvent ssPeerId . TraceHandlerEventCS)
    },
    scssTracer = Tracer (traceWith tracer . TraceScheduledChainSyncServerEvent ssPeerId),
    scssHandlers
  }
