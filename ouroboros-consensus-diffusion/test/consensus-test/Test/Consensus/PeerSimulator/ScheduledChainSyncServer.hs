{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | A ChainSync protocol server that allows external scheduling of its
-- operations, while deferring the implementation of the message handler
-- logic to a simplified, abstract interface provided as a parameter.
module Test.Consensus.PeerSimulator.ScheduledChainSyncServer
  ( ChainSyncServerHandlers (..)
  , FindIntersect (..)
  , RequestNext (..)
  , ScheduledChainSyncServer (..)
  , runScheduledChainSyncServer
  ) where

import Control.Tracer (Tracer (Tracer), traceWith)
import Ouroboros.Consensus.Block (Header)
import Ouroboros.Consensus.Block.Abstract (Point (..))
import Ouroboros.Consensus.Util.IOLike (IOLike, MonadSTM (STM))
import Ouroboros.Network.Block (Tip (..))
import Ouroboros.Network.Protocol.ChainSync.Server
  ( ChainSyncServer (..)
  , ServerStIdle (ServerStIdle, recvMsgDoneClient, recvMsgFindIntersect, recvMsgRequestNext)
  , ServerStIntersect (SendMsgIntersectFound, SendMsgIntersectNotFound)
  , ServerStNext (SendMsgRollBackward, SendMsgRollForward)
  )
import Test.Consensus.PeerSimulator.ScheduledServer
  ( ScheduledServer (..)
  , awaitOnlineState
  , runHandler
  )
import Test.Consensus.PeerSimulator.Trace
  ( TraceEvent (TraceScheduledChainSyncServerEvent)
  , TraceScheduledChainSyncServerEvent (..)
  )
import Test.Consensus.PointSchedule.NodeState (NodeState)
import Test.Consensus.PointSchedule.Peers (PeerId)

-- | Pure representation of the messages produced by the handler for the @StNext@
-- protocol state of a ChainSync server.
data RequestNext blk
  = RollForward (Header blk) (Tip blk)
  | RollBackward (Point blk) (Tip blk)
  | AwaitReply

-- | Pure representation of the messages produced by the handler for the @StIntersect@
-- protocol state of a ChainSync server.
data FindIntersect blk
  = IntersectFound (Point blk) (Tip blk)
  | IntersectNotFound (Tip blk)

-- | Handlers for the request a ChainSync server might receive from a client.
-- These take an abstract argument that corresponds to the state of a point
-- schedule tick and return the simplified protocol message types.
--
-- See 'runHandlerWithTrace' for the meaning of @[String]@.
data ChainSyncServerHandlers m state blk
  = ChainSyncServerHandlers
  { csshRequestNext ::
      state ->
      STM m (Maybe (RequestNext blk), [TraceScheduledChainSyncServerEvent state blk])
  , csshFindIntersection ::
      [Point blk] ->
      state ->
      STM m (Maybe (FindIntersect blk), [TraceScheduledChainSyncServerEvent state blk])
  }

-- | Resources used by a ChainSync server mock.
data ScheduledChainSyncServer m state blk
  = ScheduledChainSyncServer
  { scssServer :: ScheduledServer m state blk
  , scssTracer :: Tracer m (TraceScheduledChainSyncServerEvent state blk)
  , scssHandlers :: ChainSyncServerHandlers m state blk
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
  ScheduledChainSyncServer m a blk ->
  ChainSyncServer (Header blk) (Point blk) (Tip blk) m ()
scheduledChainSyncServer ScheduledChainSyncServer{scssHandlers, scssTracer, scssServer} =
  go
 where
  ChainSyncServerHandlers{csshRequestNext, csshFindIntersection} = scssHandlers

  go =
    ChainSyncServer $
      pure
        ServerStIdle
          { recvMsgRequestNext
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
        pure $ Right $ do
          -- beginning of the continuation
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
  STM m (Maybe (NodeState blk)) ->
  Tracer m (TraceEvent blk) ->
  ChainSyncServerHandlers m (NodeState blk) blk ->
  ChainSyncServer (Header blk) (Point blk) (Tip blk) m ()
runScheduledChainSyncServer ssPeerId ssTickStarted ssCurrentState tracer scssHandlers =
  scheduledChainSyncServer
    ScheduledChainSyncServer
      { scssServer =
          ScheduledServer
            { ssPeerId
            , ssTickStarted
            , ssCurrentState
            , ssCommonTracer =
                Tracer (traceWith tracer . TraceScheduledChainSyncServerEvent ssPeerId . TraceHandlerEventCS)
            }
      , scssTracer = Tracer (traceWith tracer . TraceScheduledChainSyncServerEvent ssPeerId)
      , scssHandlers
      }
