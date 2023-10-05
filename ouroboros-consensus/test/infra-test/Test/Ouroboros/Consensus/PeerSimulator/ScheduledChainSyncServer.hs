{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Ouroboros.Consensus.PeerSimulator.ScheduledChainSyncServer (
    ChainSyncServerHandlers (..)
  , FindIntersect (..)
  , RequestNext (..)
  , ScheduledChainSyncServer (..)
  , runScheduledChainSyncServer
  , scheduledChainSyncServer
  ) where

import           Control.Tracer (Tracer, traceWith)
import           Data.Functor (void)
import           Ouroboros.Consensus.Block.Abstract (Point (..))
import           Ouroboros.Consensus.Util.Condense (Condense (..))
import           Ouroboros.Consensus.Util.IOLike (IOLike, MonadSTM (STM),
                     StrictTVar, atomically, readTVarIO, uncheckedNewTVarM,
                     writeTVar)
import           Ouroboros.Network.Block (Tip (..))
import           Ouroboros.Network.Protocol.ChainSync.Server
                     (ChainSyncServer (..),
                     ServerStIdle (ServerStIdle, recvMsgDoneClient, recvMsgFindIntersect, recvMsgRequestNext),
                     ServerStIntersect (SendMsgIntersectFound, SendMsgIntersectNotFound),
                     ServerStNext (SendMsgRollBackward, SendMsgRollForward))
import           Test.Util.TestBlock (Header (..), TestBlock)

data RequestNext =
  RollForward (Header TestBlock) (Tip TestBlock)
  |
  RollBackward (Point TestBlock) (Tip TestBlock)
  deriving (Eq, Show)

data FindIntersect =
  IntersectFound (Point TestBlock) (Tip TestBlock)
  |
  IntersectNotFound (Tip TestBlock)
  deriving (Eq, Show)

data ChainSyncServerHandlers m a =
  ChainSyncServerHandlers {
    csshRequestNext      :: a -> m (Maybe RequestNext),
    csshFindIntersection :: a -> [Point TestBlock] -> m FindIntersect
  }

data ScheduledChainSyncServer m a =
  ScheduledChainSyncServer {
    scssCurrentState   :: StrictTVar m (Maybe a),
    scssAwaitNextState :: STM m (Maybe a),
    scssHandlers       :: ChainSyncServerHandlers m a,
    scssTracer         :: Tracer m String
  }

awaitNextState ::
  IOLike m =>
  ScheduledChainSyncServer m a ->
  m a
awaitNextState server@ScheduledChainSyncServer{..} = do
  newState <- atomically $ do
    newState <- scssAwaitNextState
    writeTVar scssCurrentState newState
    pure newState
  case newState of
    Nothing       -> awaitNextState server
    Just resource -> pure resource

ensureCurrentState ::
  IOLike m =>
  ScheduledChainSyncServer m a ->
  m a
ensureCurrentState server@ScheduledChainSyncServer{..} =
  readTVarIO scssCurrentState >>= \case
    Nothing -> awaitNextState server
    Just resource -> pure resource

scheduledChainSyncServer ::
  Condense a =>
  IOLike m =>
  ScheduledChainSyncServer m a ->
  ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m ()
scheduledChainSyncServer server@ScheduledChainSyncServer{scssHandlers = ChainSyncServerHandlers {..}, ..} =
  go
  where
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
      csshRequestNext currentState >>= \case
        Just (RollForward header tip) -> do
          trace $ "  gotta serve " ++ condense header
          trace $ "  tip is      " ++ condense tip
          trace "done handling MsgRequestNext"
          pure $ Left $ SendMsgRollForward header tip go
        Just (RollBackward point tip) -> do
          trace "done handling MsgRequestNext"
          pure $ Left $ (SendMsgRollBackward point tip) go
        Nothing -> do
          trace "  cannot serve at this point; waiting for node state and starting again"
          void $ awaitNextState server
          recvMsgRequestNext

    recvMsgFindIntersect pts = do
      currentState <- ensureCurrentState server
      trace "handling MsgFindIntersect"
      csshFindIntersection currentState pts >>= \case
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

    trace = traceWith scssTracer

runScheduledChainSyncServer ::
  Condense a =>
  IOLike m =>
  STM m (Maybe a) ->
  Tracer m String ->
  ChainSyncServerHandlers m a ->
  m (ChainSyncServer (Header TestBlock) (Point TestBlock) (Tip TestBlock) m ())
runScheduledChainSyncServer scssAwaitNextState scssTracer handlers = do
  scssCurrentState <- uncheckedNewTVarM Nothing
  pure $ scheduledChainSyncServer ScheduledChainSyncServer {
    scssHandlers = handlers,
    ..
  }
