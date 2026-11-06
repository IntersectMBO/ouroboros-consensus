{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The upstream side of LeiosNotify.
module Ouroboros.Consensus.MiniProtocol.LeiosNotify.Server
  ( ExnLeiosNotifyExcessiveRequests (..)
  , LeiosNotifyServerPeerLookahead
  , WhetherExcessiveRequests (..)
  , leiosNotifyServerPeerLookahead
  ) where

import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import Network.TypedProtocol.Peer.Server
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type
import Ouroboros.Consensus.Util.IOLike
  ( StrictTVar
  , newTVarIO
  , readTVar
  , writeTVar
  )

type LeiosNotifyServerPeerLookahead point announcement vote m =
  ServerLookaheadFixedSender (LeiosNotify point announcement vote) StIdle m ()

-- | Whether incrementing the count of outstanding LeiosNotify requests found the
-- peer already at its pipelining bound, i.e. requesting more than it is allowed.
data WhetherExcessiveRequests = ExcessiveRequests | NotExcessiveRequests

-- | Whether the client has sent 'MsgQuit'.
data WhetherQuitting = Quitting | NotYetQuitting
  deriving stock Generic
  deriving anyclass NoThunks

-- | Once the client sends 'MsgQuit', the server answers every outstanding
-- request with 'MsgCanceled', and then ends with 'MsgDone'.
leiosNotifyServerPeerLookahead ::
  forall m point announcement vote.
  (MonadThrow m, MonadSTM m) =>
  m WhetherExcessiveRequests ->
  -- | blocks until the next reply (announcement\/offer\/vote) is ready
  STM m (Message (LeiosNotify point announcement vote) StBusy StIdle) ->
  m (LeiosNotifyServerPeerLookahead point announcement vote m)
leiosNotifyServerPeerLookahead incr next = do
  quitVar <- newTVarIO NotYetQuitting
  pure $ ServerLookaheadFixedSender (responder quitVar) (start quitVar)
 where
  responder ::
    StrictTVar m WhetherQuitting ->
    Sender (LeiosNotify point announcement vote) VariableSender StBusy StIdle m
  responder quitVar =
    SenderEffect $
      atomically $
        orElse
          ( readTVar quitVar >>= \case
              Quitting -> pure $ SenderYield MsgCanceled SenderDone
              NotYetQuitting -> retry
          )
          (next <&> \msg -> SenderYield msg SenderDone)

  -- StIdle with nothing outstanding: receive the first request (or quit). A
  -- plain 'Await' is required here; 'AwaitLookahead' defers the StBusy->StIdle
  -- send, so it is only usable once we hold a request (i.e. are at StBusy).
  start ::
    StrictTVar m WhetherQuitting ->
    Server
      (LeiosNotify point announcement vote)
      (Lookahead Z (FixedSender StBusy StIdle))
      StIdle
      m
      ()
  start quitVar =
    Await $ \case
      MsgQuit -> Effect $ do
        atomically $ writeTVar quitVar Quitting
        pure $ Yield MsgDone $ Done ()
      MsgLeiosNotificationRequestNext ->
        Effect $ do
          incr >>= \case
            ExcessiveRequests -> throwIO ExnLeiosNotifyExcessiveRequests
            NotExcessiveRequests -> pure ()
          pure $ busy quitVar Zero

  -- StBusy with @n@ deferred sends outstanding: hand this reply off to the
  -- responder and look ahead to the next request (or done) in one step.
  busy ::
    forall n.
    StrictTVar m WhetherQuitting ->
    Nat n ->
    Server
      (LeiosNotify point announcement vote)
      (Lookahead n (FixedSender StBusy StIdle))
      StBusy
      m
      ()
  busy quitVar n =
    AwaitLookahead TheSender $ \case
      MsgQuit -> Effect $ do
        atomically (writeTVar quitVar Quitting)
        pure $ drain (Succ n)
      MsgLeiosNotificationRequestNext ->
        Effect $ do
          incr >>= \case
            ExcessiveRequests -> throwIO ExnLeiosNotifyExcessiveRequests
            NotExcessiveRequests -> pure ()
          pure $ busy quitVar (Succ n)

  -- on termination, flush the sends we've spawned, then send MsgDone.
  --
  -- Those sends are unblocked because the MsgQuit handler which lead us to this
  -- already set the quitVar flag.
  drain ::
    forall n.
    Nat n ->
    Server
      (LeiosNotify point announcement vote)
      (Lookahead n (FixedSender StBusy StIdle))
      StQuit
      m
      ()
  drain = \case
    Zero -> Yield MsgDone $ Done ()
    Succ j -> FlushSender Nothing (drain j)

data ExnLeiosNotifyExcessiveRequests = ExnLeiosNotifyExcessiveRequests
  deriving Show

instance Exception ExnLeiosNotifyExcessiveRequests
