{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
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

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Functor ((<&>))
import Data.Primitive.MutVar (MutVar)
import qualified Data.Primitive.MutVar as Prim
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
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type
  ( LeiosFetch (..)
  , Message (..)
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
      (m (Message (LeiosFetch point eb tx) (StBusy st') StIdle -> m ()))

-- | Merely an abbreviation local to this module
type X point eb tx m a n =
  Peer (LeiosFetch point eb tx) AsClient (Pipelined n C) StIdle m a

type LeiosFetchClientPeerPipelined point eb tx m a =
  PeerPipelined (LeiosFetch point eb tx) AsClient StIdle m a

toLeiosFetchClientPeerPipelined ::
  Peer (LeiosFetch point eb tx) AsClient (Pipelined Z C) StIdle m a ->
  LeiosFetchClientPeerPipelined point eb tx m a
toLeiosFetchClientPeerPipelined = PeerPipelined

data C = MkC

data WhetherDraining = AlreadyDraining | NotYetDraining

-- | A client that sends each job's request as soon as it has one, leaving at
-- most @depth@ outstanding, and hands each reply to its job. Once told to
-- stop, it ignores the replies that arrive afterwards, but one already being
-- handled is not interrupted. Then it sends 'MsgDone'.
leiosFetchClientPeerPipelined ::
  forall m point eb tx a.
  PrimMonad m =>
  -- | the most requests to leave outstanding at once
  Int ->
  -- | either the return value or the next job, or a blocking request for those two
  m
    ( Either
        (m (Either a (SomeLeiosFetchJob point eb tx m)))
        (Either a (SomeLeiosFetchJob point eb tx m))
    ) ->
  Peer (LeiosFetch point eb tx) AsClient (Pipelined Z C) StIdle m a
leiosFetchClientPeerPipelined depth tryNext =
  Effect $ do
    stop <- Prim.newMutVar NotYetDraining
    pure $ go1 stop Zero
 where
  go1 :: MutVar (PrimState m) WhetherDraining -> Nat n -> X point eb tx m a n
  go1 stop !n =
    Effect $
      tryNext >>= \case
        -- no next instruction yet
        Left next ->
          case n of
            Zero -> next <&> go2 stop Zero
            Succ m ->
              pure $
                Collect
                  (Just $ Effect $ next <&> go2 stop n)
                  (\MkC -> go1 stop m)
        Right x -> pure $ go2 stop n x

  go2 ::
    MutVar (PrimState m) WhetherDraining ->
    Nat n ->
    Either a (SomeLeiosFetchJob point eb tx m) ->
    X point eb tx m a n
  go2 stop !n = \case
    Left x -> Effect $ do
      Prim.writeMutVar stop AlreadyDraining
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
    MutVar (PrimState m) WhetherDraining ->
    Nat n ->
    SomeLeiosFetchJob point eb tx m ->
    X point eb tx m a n
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
    MutVar (PrimState m) WhetherDraining ->
    (m () -> m ()) ->
    m (Message (LeiosFetch point eb tx) (StBusy st') StIdle -> m ()) ->
    Receiver (LeiosFetch point eb tx) AsClient (StBusy st') StIdle m C
  receiver stop wrap k =
    ReceiverAwait ReflServerAgency $ \msg -> case msg of
      MsgLeiosBlock{} -> handler stop wrap k msg
      MsgLeiosBlockTxs{} -> handler stop wrap k msg

  handler ::
    MutVar (PrimState m) WhetherDraining ->
    (m () -> m ()) ->
    m (msg -> m ()) ->
    msg ->
    Receiver (LeiosFetch point eb tx) AsClient StIdle StIdle m C
  handler stop wrap k x = ReceiverEffect $ do
    wrap $
      Prim.readMutVar stop >>= \case
        AlreadyDraining -> pure ()
        NotYetDraining -> k >>= ($ x)
    pure $ ReceiverDone MkC

  drainThePipe :: a -> Nat n -> X point eb tx m a n
  drainThePipe x = \case
    Zero ->
      Yield ReflClientAgency MsgDone $
        Done ReflNobodyAgency x
    Succ m ->
      Collect
        Nothing
        (\MkC -> drainThePipe x m)
