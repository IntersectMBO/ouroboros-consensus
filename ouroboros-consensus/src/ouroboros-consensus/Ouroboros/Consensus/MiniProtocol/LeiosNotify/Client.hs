{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The downstream side of LeiosNotify.
module Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client
  ( LeiosNotifyClientPeerPipelined
  , leiosNotifyClientPeerPipelined
  , leiosNotifyClientPeerQuitOnly
  , toLeiosNotifyClientPeerPipelined
  ) where

import Control.Monad (join)
import Control.Monad.Class.MonadSTM
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Functor ((<&>))
import Data.Primitive.MutVar (MutVar)
import qualified Data.Primitive.MutVar as Prim
import Network.TypedProtocol.Codec (PeerRole (..))
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
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type
import Ouroboros.Consensus.Util.IOLike
  ( StrictTVar
  , modifyTVar
  , newTVarIO
  , readTVar
  )

-- | A client that requests nothing: it waits until told to stop, then sends
-- 'MsgQuit' and awaits 'MsgDone'.
--
-- A temporary stub for the incremental port of @leios-prototype@ to @main@.
leiosNotifyClientPeerQuitOnly ::
  MonadSTM m =>
  -- | blocks until the client should stop
  STM m a ->
  Peer (LeiosNotify point announcement vote) AsClient NonPipelined StIdle m a
leiosNotifyClientPeerQuitOnly awaitStop =
  Effect $
    atomically awaitStop <&> \x ->
      Yield ReflClientAgency MsgQuit $
        Await ReflServerAgency $ \MsgDone ->
          Done ReflNobodyAgency x

-- | Merely an abbreviation local to this module
type X point announcement vote st m a n =
  Peer (LeiosNotify point announcement vote) AsClient (Pipelined n C) st m a

type LeiosNotifyClientPeerPipelined point announcement vote m a =
  PeerPipelined (LeiosNotify point announcement vote) AsClient StIdle m a

toLeiosNotifyClientPeerPipelined ::
  Peer (LeiosNotify point announcement vote) AsClient (Pipelined Z C) StIdle m a ->
  LeiosNotifyClientPeerPipelined point announcement vote m a
toLeiosNotifyClientPeerPipelined = PeerPipelined

data C = MkC

data WhetherDraining = AlreadyDraining | NotYetDraining

leiosNotifyClientPeerPipelined ::
  forall m point announcement vote a.
  (PrimMonad m, MonadSTM m) =>
  -- | either the return value or else the current max pipelining depth
  STM m (Either a Int) ->
  -- | INVARIANT: this will only be 'MsgCanceled' if the peer sent that before
  -- we sent 'MsgQuit'
  m (Message (LeiosNotify point announcement vote) StBusy StIdle -> m ()) ->
  Peer (LeiosNotify point announcement vote) AsClient (Pipelined Z C) StIdle m a
leiosNotifyClientPeerPipelined checkDone k0 =
  Effect $ do
    stop <- Prim.newMutVar NotYetDraining
    arrived <- newTVarIO 0
    pure $ go stop arrived Zero
 where
  go ::
    MutVar (PrimState m) WhetherDraining ->
    StrictTVar m Int ->
    Nat n ->
    X point announcement vote StIdle m a n
  go stop arrived !n =
    Effect $
      join @m $
        atomically $
          checkDone >>= \case
            Left x -> pure @(STM m) $ do
              Prim.writeMutVar stop AlreadyDraining
              pure @m $ case n of
                Zero ->
                  Yield ReflClientAgency MsgQuit $
                    Await ReflServerAgency $ \MsgDone ->
                      Done ReflNobodyAgency x
                Succ _ ->
                  YieldPipelined
                    ReflClientAgency
                    MsgQuit
                    (ReceiverDone MkC) -- note that this Receiver doesn't await the MsgDone response
                    $ drainThePipe x (Succ n)
            Right maxDepth ->
              case n of
                Zero -> pure @(STM m) $ pure @m $ sendAnother stop arrived n
                Succ p
                  | natToInt n < maxDepth ->
                      pure @(STM m) $ pure @m $ Collect (Just $ sendAnother stop arrived n) (collectOne p)
                  | otherwise -> do
                      readTVar arrived >>= check . (> 0)
                      -- We can only call Collect if it would only block
                      -- ephemerally. That way we're always able to send MsgQuit as
                      -- soon as the Diffusion Layer commands us to.
                      --
                      -- This STM noise should be upstreamed into a CollectSTM that
                      -- runs handles whichever happens first: the continuation
                      -- becomes known or a pipelined request's reply arrives.
                      pure @(STM m) $ pure @m $ Collect Nothing (collectOne p)
   where
    collectOne :: Nat p -> C -> X point announcement vote StIdle m a p
    collectOne p MkC = Effect $ do
      atomically $ modifyTVar arrived (subtract 1)
      pure $ go stop arrived p

  sendAnother ::
    MutVar (PrimState m) WhetherDraining ->
    StrictTVar m Int ->
    Nat n ->
    X point announcement vote StIdle m a n
  sendAnother stop arrived !n =
    YieldPipelined
      ReflClientAgency
      MsgLeiosNotificationRequestNext
      (receiver stop arrived)
      (go stop arrived $ Succ n)

  receiver ::
    MutVar (PrimState m) WhetherDraining ->
    StrictTVar m Int ->
    Receiver (LeiosNotify point announcement vote) AsClient StBusy StIdle m C
  receiver stop arrived =
    ReceiverAwait ReflServerAgency $ \msg -> case msg of
      MsgLeiosBlockAnnouncement{} -> handler stop arrived k0 msg
      MsgLeiosBlockOffer{} -> handler stop arrived k0 msg
      MsgLeiosBlockTxsOffer{} -> handler stop arrived k0 msg
      MsgLeiosVotes{} -> handler stop arrived k0 msg
      MsgCanceled{} -> handler stop arrived k0 msg

  handler ::
    MutVar (PrimState m) WhetherDraining ->
    StrictTVar m Int ->
    m (msg -> m ()) ->
    msg ->
    Receiver (LeiosNotify point announcement vote) AsClient StIdle StIdle m C
  handler stop arrived k x = ReceiverEffect $ do
    Prim.readMutVar stop >>= \case
      AlreadyDraining -> pure ()
      NotYetDraining -> k >>= ($ x)
    atomically $ modifyTVar arrived (+ 1)
    pure $ ReceiverDone MkC

  drainThePipe :: a -> Nat n -> X point announcement vote StQuit m a n
  drainThePipe x = \case
    Zero ->
      Await ReflServerAgency $ \MsgDone ->
        Done ReflNobodyAgency x
    Succ m ->
      Collect
        Nothing -- OK to block, since we're entirely passive now
        (\MkC -> drainThePipe x m)
