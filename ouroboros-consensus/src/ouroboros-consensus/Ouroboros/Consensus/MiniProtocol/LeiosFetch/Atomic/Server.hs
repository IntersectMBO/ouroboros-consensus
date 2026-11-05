{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Opt-in bounded lookahead server with one serialized preparation task and
-- one FIFO sender. Slow storage and output never run on request/cancel intake.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#5-cancellation-races-and-the-response-commit-point@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server
  ( AtomicServer
  , AtomicDataRequest (..)
  , PreparedReply (..)
  , AtomicRequestHandler (..)
  , withAtomicServer
  , atomicServerPeer
  , readAtomicServerSnapshot
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( MonadSTM (..)
  , STM
  , StrictTVar
  , modifyTVar
  , newTVarIO
  , readTVar
  , retry
  , throwSTM
  , writeTVar
  )
import Control.Monad (unless)
import Control.Monad.Class.MonadAsync (race)
import Control.Monad.Class.MonadThrow (SomeException, bracket, throwIO, try)
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Strict as V
import Data.Word (Word32)
import Network.TypedProtocol.Core (StateTokenI)
import Network.TypedProtocol.Peer.Server
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Server.State
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity (WireOrdinal)
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Type (bitmapOffsets)
import Ouroboros.Consensus.Util.IOLike (IOLike)

-- | Original request retained by its typed sender. No payload is prepared or
-- DB reader used by intake. The negotiated count bounds these descriptors.
data AtomicDataRequest point eb tx (busy :: LeiosFetchAtomicBusy point eb tx) where
  -- | One whole body, identified by the original request ordinal and point.
  ServeBody :: !WireOrdinal -> !point -> AtomicDataRequest point eb tx StBlock
  -- | Ordered whole transactions named by the original request's bitmaps.
  ServeTransactions ::
    !WireOrdinal -> !point -> !AtomicTxBitmaps -> AtomicDataRequest point eb tx StBlockTxs

-- | A normal response prepared under the kind's byte reservation. Byte counts
-- include encoded framing. The provider must bound reads before allocating;
-- the server checks the returned charge before commitment. Canceled responses
-- are selected by the server, never by the provider.
data PreparedReply point eb tx (busy :: LeiosFetchAtomicBusy point eb tx) where
  -- | Encoded response bytes and the indivisible body retaining its raw encoding.
  PreparedBody :: !Word32 -> !eb -> PreparedReply point eb tx StBlock
  -- | Encoded response bytes and the complete requested transaction vector.
  PreparedTransactions :: !Word32 -> !(V.Vector tx) -> PreparedReply point eb tx StBlockTxs

-- | One bounded preparation at a time, on a dedicated cancellable task. A
-- borrowed DB reader must be exclusive to this server for its whole lifetime.
newtype AtomicRequestHandler point eb tx m = AtomicRequestHandler
  { prepareAtomicReply ::
      forall busy. AtomicDataRequest point eb tx busy -> m (PreparedReply point eb tx busy)
  -- ^ Return a whole normal response or throw. Cancellation joins this action
  -- before another request can reuse the reader or its payload reservation.
  }

-- | Session-scoped metadata and a serialized payload provider. The handle is
-- single-use; its owning scope outlives the interpreter and preparation tasks.
data AtomicServer point eb tx m = AtomicServer
  { serverState :: !(StrictTVar m ServerState)
  -- ^ Intake, cancellation and reply selection share this one STM state.
  , serverHandler :: !(AtomicRequestHandler point eb tx m)
  -- ^ Uses only the preparation task, never the intake thread.
  }

-- | Own a server handle around its interpreter. The action must join any
-- interpreter it starts before returning, as the lookahead runner does. Cleanup
-- then retires metadata after the runner has joined preparation and output.
withAtomicServer ::
  IOLike m =>
  ServerLimits ->
  AtomicRequestHandler point eb tx m ->
  (AtomicServer point eb tx m -> m a) ->
  m a
withAtomicServer limits handler = bracket create retire
 where
  create = do
    initial <- either throwIO pure (newServerState limits)
    AtomicServer <$> newTVarIO initial <*> pure handler
  retire server = atomically $ modifyTVar (serverState server) stopServer

readAtomicServerSnapshot :: MonadSTM m => AtomicServer point eb tx m -> STM m ServerSnapshot
readAtomicServerSnapshot server = serverSnapshot <$> readTVar (serverState server)

-- | Variable-sender lookahead retains the busy kind of every original/ACK
-- exchange. Finished deferred senders are flushed during normal intake too,
-- keeping the framework queue, completion counter and type witness bounded.
atomicServerPeer ::
  forall point eb tx m.
  IOLike m =>
  AtomicServer point eb tx m -> ServerLookahead (LeiosFetchAtomic point eb tx) StIdle m ()
atomicServerPeer server = ServerLookahead $ Effect $ do
  update startServer
  pure $ Await (incoming Zero)
 where
  update f = atomically $ do
    state <- readTVar (serverState server)
    next <- either throwSTM pure (f state)
    writeTVar (serverState server) next

  incoming ::
    forall n st.
    Nat n ->
    Message (LeiosFetchAtomic point eb tx) StIdle st ->
    Server (LeiosFetchAtomic point eb tx) (Lookahead n VariableSender) st m ()
  incoming n message = Effect $ do
    update (acceptServerMessage message)
    pure $ case message of
      MsgDone -> drain n
      MsgLeiosBlockRequest ordinal point -> busy n (DataTicket (ServeBody ordinal point))
      MsgLeiosBlockTxsRequest ordinal point bitmaps -> busy n (DataTicket (ServeTransactions ordinal point bitmaps))
      MsgTryCancel ordinal _target -> busy n (AckTicket ordinal)

  busy ::
    forall n busy.
    StateTokenI (StBusy busy :: LeiosFetchAtomic point eb tx) =>
    Nat n ->
    Ticket point eb tx busy ->
    Server (LeiosFetchAtomic point eb tx) (Lookahead n VariableSender) (StBusy busy) m ()
  busy n ticket = case n of
    Zero -> enqueue Zero
    Succ previous -> FlushSender (Just (enqueue n)) (busy previous ticket)
   where
    enqueue ::
      forall j.
      Nat j ->
      Server (LeiosFetchAtomic point eb tx) (Lookahead j VariableSender) (StBusy busy) m ()
    enqueue count = AwaitLookahead (sender ticket) (incoming (Succ count))

  drain ::
    forall n.
    Nat n ->
    Server (LeiosFetchAtomic point eb tx) (Lookahead n VariableSender) StDone m ()
  drain Zero = Effect $ update finishServer >> pure (Done ())
  drain (Succ n) = FlushSender Nothing (drain n)

  sender ::
    forall busy.
    StateTokenI (StBusy busy :: LeiosFetchAtomic point eb tx) =>
    Ticket point eb tx busy ->
    Sender (LeiosFetchAtomic point eb tx) VariableSender (StBusy busy) StIdle m
  sender ticket = SenderEffect $ do
    response <- case ticket of
      DataTicket request -> prepare request
      AckTicket ordinal -> update (commitCancelAck ordinal) >> pure MsgAckTryCancel
    pure $ SenderYield response $ SenderEffect $ do
      update (replySent (ticketOrdinal ticket))
      pure SenderDone

  prepare ::
    forall busy.
    AtomicDataRequest point eb tx busy ->
    m (Message (LeiosFetchAtomic point eb tx) (StBusy busy) StIdle)
  prepare request = do
    let ordinal = dataOrdinal request
    needed <- atomically $ do
      state <- readTVar (serverState server)
      (needed, next) <- either throwSTM pure (beginPreparation ordinal state)
      writeTVar (serverState server) next
      pure needed
    prepared <-
      if not needed
        then pure Nothing
        else do
          result <-
            race
              (try (prepareAtomicReply (serverHandler server) request >>= validatePrepared request))
              ( atomically $ do
                  state <- readTVar (serverState server)
                  canceled <- either throwSTM pure (cancellationRequested ordinal state)
                  unless canceled retry
              )
          candidate <- case result of
            Right () -> pure Nothing
            Left (Right response) -> pure (Just response)
            Left (Left (err :: SomeException)) -> do
              canceled <-
                atomically $ readTVar (serverState server) >>= either throwSTM pure . cancellationRequested ordinal
              if canceled then pure Nothing else throwIO err
          -- race has joined both children, including a canceled DB operation.
          update (finishPreparation ordinal (preparedBytes <$> candidate))
          pure candidate
    choice <- atomically $ do
      state <- readTVar (serverState server)
      (choice, next) <- either throwSTM pure (commitDataReply ordinal state)
      writeTVar (serverState server) next
      pure choice
    case choice of
      CancelledReply -> pure (cancelledMessage request)
      NormalReply -> case prepared of
        Just response -> pure (normalMessage response)
        Nothing -> throwIO ServerInvalidPreparation

data Ticket point eb tx (busy :: LeiosFetchAtomicBusy point eb tx) where
  DataTicket :: AtomicDataRequest point eb tx busy -> Ticket point eb tx busy
  AckTicket :: !WireOrdinal -> Ticket point eb tx StCancel

ticketOrdinal :: Ticket point eb tx busy -> WireOrdinal
ticketOrdinal (DataTicket request) = dataOrdinal request
ticketOrdinal (AckTicket ordinal) = ordinal

dataOrdinal :: AtomicDataRequest point eb tx busy -> WireOrdinal
dataOrdinal (ServeBody ordinal _) = ordinal
dataOrdinal (ServeTransactions ordinal _ _) = ordinal

preparedBytes :: PreparedReply point eb tx busy -> Word32
preparedBytes (PreparedBody bytes _) = bytes
preparedBytes (PreparedTransactions bytes _) = bytes

validatePrepared ::
  IOLike m =>
  AtomicDataRequest point eb tx busy ->
  PreparedReply point eb tx busy ->
  m (PreparedReply point eb tx busy)
validatePrepared (ServeTransactions _ _ bitmaps) response@(PreparedTransactions _ txs)
  | V.null txs || V.length txs /= length (bitmapOffsets (NE.toList bitmaps)) =
      throwIO ServerInvalidReply
  | otherwise = pure response
validatePrepared ServeBody{} response@PreparedBody{} = pure response

normalMessage ::
  PreparedReply point eb tx busy -> Message (LeiosFetchAtomic point eb tx) (StBusy busy) StIdle
normalMessage (PreparedBody _ body) = MsgLeiosBlock body
normalMessage (PreparedTransactions _ txs) = MsgLeiosBlockTxs txs

cancelledMessage ::
  AtomicDataRequest point eb tx busy -> Message (LeiosFetchAtomic point eb tx) (StBusy busy) StIdle
cancelledMessage ServeBody{} = MsgBlockCancelled
cancelledMessage ServeTransactions{} = MsgBlockTxsCancelled
