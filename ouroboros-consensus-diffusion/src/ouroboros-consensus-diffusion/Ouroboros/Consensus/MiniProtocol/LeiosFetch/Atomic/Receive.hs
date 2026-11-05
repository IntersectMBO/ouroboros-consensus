{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | One immutable FIFO response binding, shared by incremental validation and
-- final typed settlement. No callback consults a replacement assignment.
--
-- Rationale: @docs/fetch-policy/cancellation-contract.md#validation-descriptors-and-progress-events@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Receive
  ( ReplyBinding (..)
  , ReplyHooks (..)
  , ReplyReceiver
  , ReplyCompletion (..)
  , ReplyOutcome (..)
  , ReceiveError (..)
  , newReplyReceiver
  , replyRequestKey
  , receiveReplyScope
  , completeReply
  ) where

import Control.Concurrent.Class.MonadSTM.Strict
  ( atomically
  , newTVarIO
  , readTVar
  , throwSTM
  , writeTVar
  )
import Control.Monad (unless, when)
import Control.Monad.Class.MonadThrow
  ( Exception
  , SomeException
  , catch
  , mask_
  , throwIO
  )
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector.Strict as V
import Data.Word (Word32)
import Ouroboros.Consensus.Leios.Types (EndorserBlock, LeiosPoint, LeiosTx)
import qualified Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Admission as A
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Descriptor
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Handoff
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Atomic.Type
import Ouroboros.Consensus.MiniProtocol.LeiosFetch.Identity (RequestKey)
import Ouroboros.Consensus.Util.IOLike (IOLike)
import Ouroboros.Network.Driver.Progress
import Ouroboros.Network.Protocol.Limits (smallByteLimit)

type Atomic = LeiosFetchAtomic LeiosPoint EndorserBlock LeiosTx

-- | Original data descriptor or the cancel command's own FIFO identity. A
-- cancel ACK does not settle its target's data response.
data ReplyBinding peer
  = DataReply !(StorageRequest peer)
  | CancelReply !(RequestKey peer)

-- | Bounded synchronous coordinator hooks. They must not wait for a database
-- write or append to an unbounded queue. The caller owns session cleanup on
-- abort; already handed-off storage claims remain node-owned.
data ReplyHooks m = ReplyHooks
  { replyStarted :: m ()
  -- ^ This pinned response is about to consume retained or fresh input.
  , replyInput :: InputObservation -> m ()
  -- ^ Original read observations, including timestamp-preserving trailing input.
  , replyAwaitInput :: m ()
  -- ^ Settle local validation gates before waiting for further channel input.
  , replyTransaction :: ProgressEvent LeiosTx -> Acceptance -> m ()
  -- ^ Authenticated and handed-off whole TX; only 'Useful' earns new service.
  , replyAborted :: SomeException -> m ()
  -- ^ At most once after a receive/typed-settlement failure. Close the session
  -- at the integrating client, preserving accepted storage claims.
  }

-- | A response scope and finalizer capturing the same original request. Install
-- a fresh receiver for each FIFO reply, before entering 'ReceiverAwait'.
data ReplyReceiver m peer = ReplyReceiver
  { replyRequestKey :: !(RequestKey peer)
  -- ^ Original lifetime, session and ordinal; never a current-assignment lookup.
  , receiveReplyScope :: forall (st :: Atomic). SingLeiosFetchAtomic st -> m (ReceiveScope m LeiosTx)
  -- ^ Check the busy state and acquire this receiver's scope once.
  , completeReply :: forall (st :: Atomic). Message Atomic st StIdle -> m ReplyCompletion
  -- ^ After successful driver receipt, settle the typed result exactly once.
  -- TX vectors are checked for count but neither ingested nor credited again.
  }

-- | The typed terminal result. A valid body can report an announcement-size
-- fault after it has been safely handed off; the caller applies the peer fault.
data ReplyOutcome
  = BodyReceived !BodyAcceptance
  | TransactionsReceived
  | DataCancelled
  | CancelAcknowledged
  deriving (Eq, Show)

-- | Wire receipt paired with its original typed result. Receipt times come from
-- input observation, independently of decoding, hashing or storage duration.
data ReplyCompletion = ReplyCompletion
  { completedReceipt :: !MessageReceipt
  -- ^ Final consumed byte and its original read timestamp.
  , completedOutcome :: !ReplyOutcome
  -- ^ Result of this exchange, independent of other overlapping attempts.
  }
  deriving (Eq, Show)

-- | A local binding/lifecycle mismatch or a failed immutable request check.
data ReceiveError
  = InvalidReceiveBound
  | WrongReceiveState
  | ReceiveAlreadyAcquired
  | ReceiveOutOfSequence
  | ReceiveWrongCount
  | ReceiveStorageError !StorageError
  deriving (Eq, Show)

instance Exception ReceiveError

-- | One scope is acquired, started, framed and settled in order. Failed
-- receives cannot be revived; a successful scope still owes typed settlement.
data ReplyPhase
  = Waiting
  | Acquired
  | Receiving !Word32
  | Framed !Word32 !MessageReceipt
  | Settling
  | Settled
  | Failed

-- | Reserve storage before constructing this binding. Decode-time work uses
-- only that descriptor and the bounded handoff; it does no DB lookup or wait.
-- The driver owns abort notification after scope acquisition. Typed settlement
-- runs under the same bounded/masked callback discipline and handles its own
-- abort notification because it runs after the driver has returned.
newReplyReceiver ::
  forall m owner peer.
  (IOLike m, Ord peer) =>
  StorageHandoff m owner peer ->
  ReplyBinding peer ->
  ReplyHooks m ->
  m (ReplyReceiver m peer)
newReplyReceiver handoff binding hooks = do
  (key, limits, kind) <- case binding of
    CancelReply key -> pure (key, ProgressLimits (fromIntegral smallByteLimit) 0 0, CancelKind)
    DataReply req -> do
      let prepared = storageRequestPrepared req
          EncodedBytes bytes = preparedResponseBytes prepared
          count = NE.length (preparedElements prepared)
      when
        ( bytes == 0
            || bytes > fromIntegral (maxBound :: Word32)
            || toInteger count > toInteger (maxBound :: Word32)
        )
        $ throwIO InvalidReceiveBound
      let (elements, kind) = case preparedDescriptor prepared of
            BodyRequest{} -> (0, BodyKind)
            BatchRequest{} -> (fromIntegral count, TxsKind)
      pure
        (storageRequestKey req, ProgressLimits (fromIntegral bytes) (fromIntegral bytes) elements, kind)
  phase <- newTVarIO Waiting
  let abort err = do
        notify <- atomically $ do
          state <- readTVar phase
          case state of
            Failed -> pure False
            Settled -> pure False
            _ -> writeTVar phase Failed >> pure True
        when notify (replyAborted hooks err)
      scope =
        ReceiveScope
          { receiveLimits = limits
          , receiveStarted = do
              atomically $ do
                state <- readTVar phase
                case state of
                  Acquired -> writeTVar phase (Receiving 0)
                  _ -> throwSTM ReceiveOutOfSequence
              replyStarted hooks
          , receiveInput = replyInput hooks
          , receiveAwaitInput = replyAwaitInput hooks
          , receiveProgress = \event -> do
              index <- atomically $ do
                state <- readTVar phase
                case state of
                  Receiving n | n == progressIndex event && n < progressElements limits -> pure n
                  _ -> throwSTM ReceiveOutOfSequence
              case binding of
                CancelReply{} -> throwIO WrongReceiveState
                DataReply req -> do
                  let WithByteSpan (tx, _, _) = progressElement event
                  accepted <- acceptTransaction handoff req index tx >>= storageResult
                  atomically $ writeTVar phase (Receiving (index + 1))
                  replyTransaction hooks event accepted
          , receiveComplete = \receipt -> atomically $ do
              state <- readTVar phase
              case state of
                Receiving count -> writeTVar phase (Framed count receipt)
                _ -> throwSTM ReceiveOutOfSequence
          , receiveAbort = abort
          }
      acquire :: forall (st :: Atomic). SingLeiosFetchAtomic st -> m (ReceiveScope m LeiosTx)
      acquire tok = do
        unless (matches kind tok) $ throwIO WrongReceiveState
        atomically $ do
          state <- readTVar phase
          case state of
            Waiting -> writeTVar phase Acquired
            _ -> throwSTM ReceiveAlreadyAcquired
        pure scope
      finish :: forall (st :: Atomic). Message Atomic st StIdle -> m ReplyCompletion
      finish message =
        mask_ $
          ( do
              (count, receipt) <- atomically $ do
                state <- readTVar phase
                case state of
                  Framed count receipt -> writeTVar phase Settling >> pure (count, receipt)
                  _ -> throwSTM ReceiveOutOfSequence
              (terminal, outcome) <- case (binding, message) of
                (DataReply req, MsgLeiosBlock body) | kind == BodyKind -> do
                  accepted <- acceptBody handoff req body >>= storageResult
                  pure (A.NormalTerminal, BodyReceived accepted)
                (DataReply{}, MsgLeiosBlockTxs txs) | kind == TxsKind -> do
                  unless (count == progressElements limits && toInteger (V.length txs) == toInteger count) $
                    throwIO ReceiveWrongCount
                  pure (A.NormalTerminal, TransactionsReceived)
                (DataReply{}, MsgBlockCancelled)
                  | kind == BodyKind && count == 0 ->
                      pure (A.CancelledTerminal, DataCancelled)
                (DataReply{}, MsgBlockTxsCancelled)
                  | kind == TxsKind && count == 0 ->
                      pure (A.CancelledTerminal, DataCancelled)
                (CancelReply{}, MsgAckTryCancel) -> pure (A.CancelAcknowledgement, CancelAcknowledged)
                _ -> throwIO WrongReceiveState
              atomically (finishStorageRequest handoff key terminal) >>= storageResult
              atomically $ writeTVar phase Settled
              pure (ReplyCompletion receipt outcome)
          )
            `catch` \(err :: SomeException) -> abort err >> throwIO err
  pure (ReplyReceiver key acquire finish)

data ReplyKind = BodyKind | TxsKind | CancelKind deriving Eq

matches :: ReplyKind -> SingLeiosFetchAtomic (st :: Atomic) -> Bool
matches BodyKind SingBlock = True
matches TxsKind SingBlockTxs = True
matches CancelKind SingCancel = True
matches _ _ = False

storageResult :: IOLike m => Either StorageError a -> m a
storageResult = either (throwIO . ReceiveStorageError) pure
