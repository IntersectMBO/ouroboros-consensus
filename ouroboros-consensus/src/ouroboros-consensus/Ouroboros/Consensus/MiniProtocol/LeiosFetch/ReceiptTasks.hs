{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Node lifetime ownership for ordinary fetch submissions and receipt tasks.
-- A peer may disappear while its accepted database write is still settling.
-- A receipt task waits for write confirmation, reconciles logical work and
-- releases the pending claim independently of that peer's reply handler.
-- Normal shutdown stops admission, drains producers and joins their receipts;
-- exceptional shutdown cancels and joins both before storage can close.
-- See @docs/fetch-policy/leiosfetch-improved.md#ownership-and-shutdown@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.ReceiptTasks
  ( ReceiptTasks
  , newReceiptTasks
  , receiptClosing
  , withReceiptSubmission
  , withReceiptSubmissionResult
  , withReceiptTransfer
  , forkReceipt
  , withReceiptTasks
  ) where

import qualified Control.Concurrent.Class.MonadSTM.Strict as S
import Control.Exception (AsyncException (ThreadKilled))
import Control.Monad (forM_, void, when)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Numeric.Natural (Natural)
import Ouroboros.Consensus.Util.IOLike

-- | Peer threads may submit work without membership of the node's registry.
-- One scoped supervisor delivers the first child failure; concurrent failures
-- cannot send repeated exceptions into teardown. Finished successful handles
-- are reaped on subsequent forks and contain no completed payloads.
newtype ReceiptTasks m = ReceiptTasks
  { state :: S.StrictTVar m (ReceiptState m)
  }

data ReceiptState m = ReceiptState
  { closing :: !Bool
  -- ^ Reject new producers and optional local work; accepted writes still drain.
  , aborting :: !Bool
  -- ^ Cancellation is in progress; its expected child exceptions are not faults.
  , nextId :: !Natural
  -- ^ Node-local identity for producers and child handles; never sent on wire.
  , producers :: !(Map.Map Natural (ThreadId m))
  -- ^ Handlers that may still transfer an accepted write to a receipt task.
  , children :: !(Map.Map Natural (Async m ()))
  -- ^ Joinable callbacks, installed before their start latch is released.
  , failure :: !(Maybe SomeException)
  -- ^ First fatal callback failure, retained only until the scope is torn down.
  }

newReceiptTasks :: IOLike m => m (ReceiptTasks m)
newReceiptTasks = ReceiptTasks <$> S.newTVarIO (ReceiptState False False 0 Map.empty Map.empty Nothing)

receiptClosing :: IOLike m => ReceiptTasks m -> STM m Bool
receiptClosing ReceiptTasks{state} = closing <$> S.readTVar state

-- | Register before a handler can submit, and unregister only after any
-- accepted promise has acquired its receipt owner. Closing and registration
-- share one transaction, so observing no producers closes the fork race.
withReceiptSubmission :: IOLike m => ReceiptTasks m -> m () -> m ()
withReceiptSubmission tasks action = void $ withReceiptSubmissionResult tasks action

-- | The result distinguishes rejected new work from an admitted producer.
-- Queue workers must not wait forever on a promise no producer installed.
withReceiptSubmissionResult :: IOLike m => ReceiptTasks m -> m a -> m (Maybe a)
withReceiptSubmissionResult ReceiptTasks{state} action = mask_ $ do
  tid <- myThreadId
  token <- atomically $ do
    st <- S.readTVar state
    if closing st
      then pure Nothing
      else do
        let key = nextId st
        S.writeTVar state st{nextId = key + 1, producers = Map.insert key tid (producers st)}
        pure (Just key)
  traverse
    ( \key ->
        action
          `finally` atomically (S.modifyTVar state $ \st -> st{producers = Map.delete key (producers st)})
    )
    token

-- | Transfer a writer-accepted operation to its receipt owner while still in a
-- registered producer. The action may record acceptance and install the child,
-- then must return without tracing or other work after installation. Returning
-- successfully means that owner is installed, even if it already finished.
--
-- An exception before installation closes admission and reports the first fault
-- through this scope's supervisor. In the same transaction, release the exact
-- accepted claim: the cleanup must not retry or throw, or imply confirmation.
-- 'Nothing' means fatal teardown owns the accepted write, never non-submission.
-- Already-aborting scopes keep their original cause and still run that cleanup.
withReceiptTransfer :: IOLike m => ReceiptTasks m -> STM m () -> m a -> m (Maybe a)
withReceiptTransfer tasks abandonAccepted transfer = mask_ $
  (Just <$> transfer) `catch` \(err :: SomeException) -> do
    atomically $ recordFailure tasks err >> abandonAccepted
    pure Nothing

recordFailure :: IOLike m => ReceiptTasks m -> SomeException -> STM m ()
recordFailure ReceiptTasks{state} err =
  S.modifyTVar state $ \st ->
    if aborting st || isJust (failure st) then st else st{closing = True, failure = Just err}

-- | Called within a registered submission, after writer acceptance. A start
-- latch prevents a fast child finishing before its handle is installed. A
-- closed scope still accepts this transfer from an already admitted producer.
forkReceipt :: IOLike m => ReceiptTasks m -> m () -> m ()
forkReceipt tasks@ReceiptTasks{state} action = mask_ $ do
  started <- S.newEmptyTMVarIO
  child <- asyncWithUnmask $ \unmask -> do
    labelThisThread "LeiosFetch.receipt"
    atomically $ S.readTMVar started
    unmask action `catch` \(err :: SomeException) ->
      atomically (recordFailure tasks err)
  ( do
      atomically $ do
        st <- S.readTVar state
        live <-
          Map.traverseMaybeWithKey
            (\_ a -> pollSTM a >>= \result -> pure $ case result of Just (Right ()) -> Nothing; _ -> Just a)
            (children st)
        let key = nextId st
        S.writeTVar state st{nextId = key + 1, children = Map.insert key child live}
        S.putTMVar started ()
    )
    `onException` cancel child

-- | Enclose the peer applications, inside the database lifetime. The scope is
-- one-shot. Normal drain may wait for a healthy slow writer; failures and
-- external interruption use cancellation, not an extra storage timeout.
withReceiptTasks :: IOLike m => ReceiptTasks m -> m a -> m a
withReceiptTasks ReceiptTasks{state} action = mask $ \restore -> do
  owner <- myThreadId
  let reportFailure = do
        err <- atomically $ do
          st <- S.readTVar state
          maybe S.retry pure (failure st)
        throwTo owner err
  withAsync reportFailure $ \supervisor -> do
    result <- restore action `onException` abort supervisor
    ( do
        atomically $ S.modifyTVar state (\st -> st{closing = True})
        pending <- atomically $ do
          st <- S.readTVar state
          S.check (Map.null (producers st))
          pure (Map.elems (children st))
        mapM_ wait pending
        atomically $ do
          st <- S.readTVar state
          -- If a child failed, let the one supervised delivery interrupt us.
          -- Throwing here as well could race a duplicate exception into cleanup.
          S.check (isNothing (failure st))
          S.writeTVar state st{children = Map.empty}
      )
      `onException` abort supervisor
    pure result
 where
  abort supervisor = do
    tids <- atomically $ do
      st <- S.readTVar state
      S.writeTVar state st{closing = True, aborting = True}
      pure (Map.elems (producers st))
    -- Stop/join the sole failure sender before beginning interruptible cleanup.
    cancel supervisor
    me <- myThreadId
    forM_ tids $ \tid -> when (tid /= me) (throwTo tid ThreadKilled)
    pending <- atomically $ do
      st <- S.readTVar state
      S.check (Map.null (producers st))
      pure (Map.elems (children st))
    mapM_ cancel pending
    atomically $ S.modifyTVar state (\st -> st{children = Map.empty})
