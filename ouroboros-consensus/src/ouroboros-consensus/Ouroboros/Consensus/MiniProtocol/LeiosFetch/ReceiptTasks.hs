{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Node lifetime ownership for ordinary fetch submissions and receipt tasks.
-- A peer may disappear while its accepted database write is still settling.
-- Normal shutdown stops admission, drains producers and joins their receipts;
-- exceptional shutdown cancels and joins both before storage can close.
-- See @docs/fetch-policy/leiosfetch-improved.md#ownership-and-shutdown@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.ReceiptTasks
  ( ReceiptTasks
  , newReceiptTasks
  , receiptClosing
  , withReceiptSubmission
  , forkReceipt
  , withReceiptTasks
  ) where

import qualified Control.Concurrent.Class.MonadSTM.Strict as S
import Control.Exception (AsyncException (ThreadKilled))
import Control.Monad (forM_, when)
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
withReceiptSubmission ReceiptTasks{state} action = mask_ $ do
  tid <- myThreadId
  token <- atomically $ do
    st <- S.readTVar state
    if closing st
      then pure Nothing
      else do
        let key = nextId st
        S.writeTVar state st{nextId = key + 1, producers = Map.insert key tid (producers st)}
        pure (Just key)
  forM_ token $ \key ->
    action
      `finally` atomically (S.modifyTVar state $ \st -> st{producers = Map.delete key (producers st)})

-- | Called within a registered submission, after writer acceptance. A start
-- latch prevents a fast child finishing before its handle is installed. A
-- closed scope still accepts this transfer from an already admitted producer.
forkReceipt :: IOLike m => ReceiptTasks m -> m () -> m ()
forkReceipt ReceiptTasks{state} action = mask_ $ do
  started <- S.newEmptyTMVarIO
  child <- asyncWithUnmask $ \unmask -> do
    labelThisThread "LeiosFetch.receipt"
    atomically $ S.readTMVar started
    unmask action `catch` \(err :: SomeException) ->
      atomically
        ( S.modifyTVar state $ \st ->
            if aborting st || isJust (failure st) then st else st{closing = True, failure = Just err}
        )
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
