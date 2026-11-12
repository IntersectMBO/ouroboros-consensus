{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The downstream side of LeiosFetch.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Client
  ( LeiosFetchClientPeerPipelined
  , SomeLeiosFetchJob (..)
  , LeiosFetchJobActions (..)
  , leiosFetchClientPeerPipelined
  , toLeiosFetchClientPeerPipelined
  ) where

import Control.Monad.Class.MonadSTM (MonadSTM (STM, atomically, orElse))
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import Network.TypedProtocol.Codec (PeerRole (..), StateTokenI)
import Network.TypedProtocol.Core
  ( IsPipelined (..)
  , N (..)
  , Nat (..)
  , ReflRelativeAgency (..)
  , natToInt
  )
import Network.TypedProtocol.Peer
  ( Peer (..)
  , PeerPipelined (..)
  , Receiver (..)
  )
import NoThunks.Class (NoThunks)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
  ( LeiosFetch (..)
  , Message (..)
  )
import Ouroboros.Consensus.Util.NormalForm.StrictTVar
  ( StrictTVar
  , newTVarIO
  , readTVarIO
  , writeTVar
  )

-- | Operations attached to the immutable request. Dispatch is rechecked after
-- waiting for a pipeline slot. The receive action binds its FIFO context before
-- decoding; handler cleanup also runs while draining.
data LeiosFetchJobActions m = LeiosFetchJobActions
  { beforeDispatch :: m Bool
  , beforeReceive :: m ()
  , withReplyHandler :: m () -> m ()
  }

-- | A request to send, and what to do with its reply.
data SomeLeiosFetchJob point eb tx m
  = forall st'.
    StateTokenI (StBusy st') =>
    MkSomeLeiosFetchJob
      (LeiosFetchJobActions m)
      (Message (LeiosFetch point eb tx) StIdle (StBusy st'))
      (Message (LeiosFetch point eb tx) (StBusy st') StIdle -> m ())

type LeiosFetchClientPeerPipelined point eb tx m a =
  PeerPipelined (LeiosFetch point eb tx) AsClient StIdle m a

toLeiosFetchClientPeerPipelined ::
  Peer (LeiosFetch point eb tx) AsClient (Pipelined Z C) StIdle m a ->
  LeiosFetchClientPeerPipelined point eb tx m a
toLeiosFetchClientPeerPipelined = PeerPipelined

data C = MkC

data WhetherDraining = AlreadyDraining | NotYetDraining
  deriving stock Generic
  deriving anyclass NoThunks

-- | A client that sends each job's request as soon as it has one, leaving at
-- most @depth@ outstanding, and hands each reply to its job. Once told to
-- stop, it ignores the replies that arrive afterwards, but one already being
-- handled is not interrupted. Then it sends 'MsgDone'.
--
-- If it could both stop and send a job, it stops.
leiosFetchClientPeerPipelined ::
  forall m point eb tx a.
  MonadSTM m =>
  -- | the most requests to leave outstanding at once
  Int ->
  -- | blocks until the client should stop
  STM m a ->
  -- | blocks until there is a next job
  STM m (SomeLeiosFetchJob point eb tx m) ->
  Peer (LeiosFetch point eb tx) AsClient (Pipelined Z C) StIdle m a
leiosFetchClientPeerPipelined depth awaitStop awaitJob =
  Effect $ do
    stop <- newTVarIO NotYetDraining
    pure $ go1 stop Zero
 where
  next :: m (Either a (SomeLeiosFetchJob point eb tx m))
  next = atomically $ (Left <$> awaitStop) `orElse` (Right <$> awaitJob)

  go1 ::
    StrictTVar m WhetherDraining ->
    Nat n ->
    Peer (LeiosFetch point eb tx) AsClient (Pipelined n C) StIdle m a
  go1 stop !n = case n of
    Zero -> Effect $ next <&> go2 stop Zero
    -- Collecting a reply only takes what the receiver has already handled, so
    -- each one ready is collected before blocking for what to do next.
    Succ m -> Collect (Just $ Effect $ next <&> go2 stop n) (\MkC -> go1 stop m)

  go2 ::
    StrictTVar m WhetherDraining ->
    Nat n ->
    Either a (SomeLeiosFetchJob point eb tx m) ->
    Peer (LeiosFetch point eb tx) AsClient (Pipelined n C) StIdle m a
  go2 stop !n = \case
    Left x -> Effect $ do
      atomically $ writeTVar stop AlreadyDraining
      pure $ drainThePipe x n
    Right job ->
      case n of
        Zero -> send stop n job
        Succ m
          -- At the limit, wait for a response rather than adding to the pipe.
          -- Sending anyway is what an unbounded client does, and a server that
          -- bounds what it will accept outstanding would drop the connection
          -- over it.
          | natToInt n >= depth -> Collect Nothing (\MkC -> send stop m job)
          | otherwise ->
              Collect
                (Just $ send stop n job)
                (\MkC -> send stop m job)

  send ::
    StrictTVar m WhetherDraining ->
    Nat n ->
    SomeLeiosFetchJob point eb tx m ->
    Peer (LeiosFetch point eb tx) AsClient (Pipelined n C) StIdle m a
  send stop !n (MkSomeLeiosFetchJob actions req k) = Effect $ do
    dispatch <- beforeDispatch actions
    pure $
      if dispatch
        then
          YieldPipelined
            ReflClientAgency
            req
            (ReceiverEffect $ beforeReceive actions >> pure (receiver stop (withReplyHandler actions) k))
            (go1 stop $ Succ n)
        else go1 stop n

  receiver ::
    StateTokenI (StBusy st') =>
    StrictTVar m WhetherDraining ->
    (m () -> m ()) ->
    (Message (LeiosFetch point eb tx) (StBusy st') StIdle -> m ()) ->
    Receiver (LeiosFetch point eb tx) AsClient (StBusy st') StIdle m C
  receiver stop wrap k =
    ReceiverAwait ReflServerAgency $ \msg -> case msg of
      MsgLeiosBlock{} -> handler stop wrap k msg
      MsgLeiosBlockTxs{} -> handler stop wrap k msg

  handler ::
    StrictTVar m WhetherDraining ->
    (m () -> m ()) ->
    (msg -> m ()) ->
    msg ->
    Receiver (LeiosFetch point eb tx) AsClient StIdle StIdle m C
  handler stop wrap k x = ReceiverEffect $ do
    wrap $
      readTVarIO stop >>= \case
        AlreadyDraining -> pure ()
        NotYetDraining -> k x
    pure $ ReceiverDone MkC

  drainThePipe :: a -> Nat n -> Peer (LeiosFetch point eb tx) AsClient (Pipelined n C) StIdle m a
  drainThePipe x = \case
    Zero ->
      Yield ReflClientAgency MsgDone $
        Done ReflNobodyAgency x
    Succ m ->
      Collect
        Nothing
        (\MkC -> drainThePipe x m)
