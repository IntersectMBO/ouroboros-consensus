{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | The downstream side of LeiosNotify.
module Ouroboros.Consensus.MiniProtocol.LeiosNotify.Client
  ( ExnLeiosNotifyUnexpectedCancel (..)
  , LeiosNotifyClientPeerPipelined
  , awaitImmTipCanForecastNow
  , leiosNotifyClientPeerDiscardingReplies
  , leiosNotifyClientPeerPipelined
  , toLeiosNotifyClientPeerPipelined
  ) where

import Control.Monad (join)
import Control.Monad.Class.MonadSTM
import Control.Monad.Class.MonadThrow
import Control.Monad.Except (runExcept)
import Control.Monad.Primitive (PrimMonad, PrimState)
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
import Ouroboros.Consensus.BlockchainTime (CurrentSlot (..))
import Ouroboros.Consensus.Config (TopLevelConfig, configLedger)
import Ouroboros.Consensus.Forecast (forecastFor)
import Ouroboros.Consensus.Ledger.Extended (ExtLedgerState, ledgerState)
import Ouroboros.Consensus.Ledger.SupportsProtocol (LedgerSupportsProtocol (..))
import Ouroboros.Consensus.MiniProtocol.LeiosNotify.Type
import Ouroboros.Consensus.Util.IOLike
  ( StrictTVar
  , modifyTVar
  , newTVarIO
  , readTVar
  )
import Ouroboros.Network.ControlMessage (ControlMessage (..), ControlMessageSTM)

-- | Block until the immutable tip's ledger state can forecast to the current
-- slot.
--
-- A LeiosNotify client judges the replies it receives by forecasting from the
-- immutable tip. Until that tip reaches approximately now, a fresh reply would
-- be beyond the forecast horizon, and a reply the client cannot judge is no
-- grounds to disconnect the peer. So a client sends no request until then.
--
-- The current slot comes from the volatile tip, whose horizon reaches further,
-- so gating on it alone would send requests too soon.
awaitImmTipCanForecastNow ::
  (MonadSTM m, LedgerSupportsProtocol blk) =>
  TopLevelConfig blk ->
  -- | the immutable tip's ledger state
  STM m (ExtLedgerState blk mk) ->
  STM m CurrentSlot ->
  STM m ()
awaitImmTipCanForecastNow cfg readImmutableLedger readCurrentSlot =
  readCurrentSlot >>= \case
    CurrentSlotUnknown -> retry
    CurrentSlot slot -> do
      immLedger <- readImmutableLedger
      case runExcept $
        forecastFor
          (ledgerViewForecastAt (configLedger cfg) (ledgerState immLedger))
          slot of
        Left _outsideRange -> retry
        Right _view -> pure ()

-- | A client that keeps @depth@ requests outstanding until the control message
-- says to terminate, and discards every reply. It sends no request while
-- @gate@ blocks; see 'awaitImmTipCanForecastNow'. It disconnects a peer that
-- cancels a request before the client has sent 'MsgQuit'.
--
-- A temporary stub for the incremental port of @leios-prototype@ to @main@.
leiosNotifyClientPeerDiscardingReplies ::
  (PrimMonad m, MonadSTM m, MonadThrow m) =>
  Int ->
  -- | blocks until the client may send requests
  STM m () ->
  ControlMessageSTM m ->
  LeiosNotifyClientPeerPipelined point announcement vote m ()
leiosNotifyClientPeerDiscardingReplies depth gate controlMessageSTM =
  toLeiosNotifyClientPeerPipelined $
    leiosNotifyClientPeerPipelined
      ( controlMessageSTM >>= \case
          Continue -> Right depth <$ gate
          -- Only warm mini-protocols receive this, and LeiosNotify is hot.
          Quiesce -> Right depth <$ gate
          Terminate -> pure (Left ())
      )
      ( pure $ \case
          MsgLeiosBlockAnnouncement{} -> pure ()
          MsgLeiosBlockOffer{} -> pure ()
          MsgLeiosBlockTxsOffer{} -> pure ()
          MsgLeiosVotes{} -> pure ()
          MsgCanceled -> throwIO MkExnLeiosNotifyUnexpectedCancel
      )

-- | The peer sent 'MsgCanceled' although the client had not sent 'MsgQuit'.
data ExnLeiosNotifyUnexpectedCancel = MkExnLeiosNotifyUnexpectedCancel
  deriving Show

instance Exception ExnLeiosNotifyUnexpectedCancel

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
