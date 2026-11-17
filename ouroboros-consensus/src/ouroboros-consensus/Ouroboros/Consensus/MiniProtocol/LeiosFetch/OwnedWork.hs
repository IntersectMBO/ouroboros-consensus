{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bounded node-owned work admitted in the same STM transaction as the
-- announcement that creates it. The caller may disappear after admission;
-- processing and receipt settlement belong to the enclosing node lifetime.
-- See @docs/fetch-policy/leiosfetch-improved.md#ownership-and-shutdown@.
module Ouroboros.Consensus.MiniProtocol.LeiosFetch.OwnedWork
  ( OwnedWork
  , Admission (..)
  , WorkClosed (..)
  , newOwnedWork
  , submitWork
  , pruneCompleted
  , withOwnedWork
  , withOwnedWorkN
  , canSubmitWork
  ) where

import Control.Monad (forever, unless)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Ouroboros.Consensus.Util.IOLike

-- | Queued and running entries share a hard bound. A running entry retains its
-- identity until settlement, not merely until its worker starts. Only positively
-- completed keys are remembered afterwards; these contain no payload and do
-- not consume active capacity. Their owner prunes them at its retirement
-- frontier; if that frontier stalls, completion metadata can accumulate.
data OwnedWork m key payload = OwnedWork
  { state :: !(StrictTVar m (WorkState m key payload))
  , capacity :: !Int
  , externallyClosing :: !(STM m Bool)
  , workerLabel :: !String
  }

data WorkState m key payload = WorkState
  { stopped :: !Bool
  , failure :: !(Maybe SomeException)
  , entries :: !(Map.Map key (payload, StrictTMVar m ()))
  , queued :: !(Seq key)
  , completed :: !(Set.Set key)
  , retainCompletion :: !(key -> Bool)
  -- ^ Latest detached retirement frontier; never captures an old work state.
  }

-- | The first caller supplies the payload; other callers join its admission
-- result without changing its owner. The wait confirms the first phase
-- (writer admission or retirement for payloads), not durability.
data Admission m = Admission
  { newlyOwned :: !Bool
  , awaitAdmission :: STM m ()
  }

-- | The enclosing lifetime cannot admit or finish this dependent operation.
-- Callers receive closure; the worker's supervisor retains its original failure.
-- Normal drain still completes joins for already-admitted operations.
data WorkClosed = WorkClosed deriving Show

instance Exception WorkClosed

newOwnedWork :: IOLike m => String -> Int -> STM m Bool -> m (OwnedWork m key payload)
newOwnedWork label limit closing
  | limit <= 0 = error "newOwnedWork: non-positive capacity"
  | otherwise = do
      variable <- uncheckedNewTVarM (WorkState False Nothing Map.empty Seq.empty Set.empty (const True))
      pure (OwnedWork variable limit closing label)

-- | Admission can be composed with global acceptance. A full queue retries the
-- entire transaction, so no seen-only announcement can escape its owner.
submitWork ::
  (IOLike m, Ord key) => OwnedWork m key payload -> key -> payload -> STM m (Admission m)
submitWork work key payload = do
  st <- readTVar (state work)
  closing <- externallyClosing work
  unless (not closing && not (stopped st)) (throwSTM WorkClosed)
  case failure st of
    Just _ -> throwSTM WorkClosed
    Nothing -> pure ()
  case Map.lookup key (entries st) of
    Just (_, accepted) -> pure (Admission False (awaitResult work accepted))
    Nothing | Set.member key (completed st) -> pure (Admission False (pure ()))
    Nothing -> do
      check (Map.size (entries st) < capacity work)
      accepted <- newEmptyTMVar
      writeTVar
        (state work)
        st{entries = Map.insert key (payload, accepted) (entries st), queued = queued st |> key}
      pure (Admission True (awaitResult work accepted))

-- | Nonblocking admission predicate. Callers that must also hold an MVar can
-- release it before waiting for capacity, then recheck all ownership facts.
-- Existing entries are not capacity for a different payload of the same key.
-- False means temporary pressure; closing or failed work throws 'WorkClosed'
-- so a caller cannot remain parked waiting for capacity that will never return.
canSubmitWork :: (IOLike m, Ord key) => OwnedWork m key payload -> key -> STM m Bool
canSubmitWork work key = do
  st <- readTVar (state work)
  closing <- externallyClosing work
  unless (not closing && not (stopped st)) (throwSTM WorkClosed)
  case failure st of
    Just _ -> throwSTM WorkClosed
    Nothing ->
      pure
        ( Map.notMember key (entries st)
            && Map.size (entries st) < capacity work
        )

awaitResult :: IOLike m => OwnedWork m key payload -> StrictTMVar m () -> STM m ()
awaitResult work accepted = do
  st <- readTVar (state work)
  case failure st of
    Just _ -> throwSTM WorkClosed
    -- Closing admission alone does not abandon accepted operations. Their
    -- first-phase result may still arrive while the scope normally drains.
    Nothing -> readTMVar accepted

-- | Forget only completion evidence. Queued/running owners survive pruning
-- until their handler reconciles retirement; no old worker can erase a new one.
pruneCompleted :: IOLike m => OwnedWork m key payload -> (key -> Bool) -> STM m ()
pruneCompleted work keep = modifyTVar (state work) $ \st -> st{completed = Set.filter keep (completed st), retainCompletion = keep}

-- | Activate under the receipt supervisor, before exposing node applications.
-- Each handler returns a settlement action after writer admission, releasing
-- 'awaitAdmission' waiters. The forge callback itself returns at queue admission;
-- the entry remains owned until the returned settlement action finishes.
-- True means positively confirmed completion; empty/abandoned jobs are not proof.
-- Normal shutdown closes admission and drains; failure cancels/joins the worker.
withOwnedWork ::
  forall m key payload a.
  (IOLike m, Ord key) =>
  OwnedWork m key payload ->
  (key -> payload -> m (m Bool)) ->
  m a ->
  m a
withOwnedWork work = withOwnedWorkN 1 work (\_ _ -> pure ())

-- | A fixed worker group sharing the same queued/running capacity. The group
-- has one supervised parent: the first failure cancels and joins its siblings
-- before propagating, even when several handlers fail concurrently. The
-- nonblocking abandonment callback releases entry-scoped ownership on abort,
-- including payloads no worker started. It must be idempotent with running
-- handlers' finalizers and must not retry or reenter this queue.
withOwnedWorkN ::
  forall m key payload a.
  (IOLike m, Ord key) =>
  Int ->
  OwnedWork m key payload ->
  (key -> payload -> STM m ()) ->
  (key -> payload -> m (m Bool)) ->
  m a ->
  m a
withOwnedWorkN count work abandon process action
  | count <= 0 = error "withOwnedWorkN: non-positive worker count"
  | otherwise = mask $ \restore ->
      bracket (asyncWithUnmask (\unmask -> unmask (workers count))) cancel $ \child -> do
        link child
        result <- restore action `onException` abort
        ( do
            atomically $ modifyTVar (state work) (\st -> st{stopped = True})
            restore $ atomically $ readTVar (state work) >>= check . Map.null . entries
          )
          `onException` abort
        pure result
 where
  workers 1 = worker
  workers n = concurrently_ worker (workers (n - 1))
  worker = do
    labelThisThread (workerLabel work)
    forever $ do
      (key, payload, accepted) <- atomically $ do
        st <- readTVar (state work)
        case queued st of
          Empty -> retry
          key :<| rest -> case Map.lookup key (entries st) of
            Nothing -> error "OwnedWork: queued entry has no owner"
            Just (payload, accepted) -> do
              writeTVar (state work) st{queued = rest}
              pure (key, payload, accepted)
      ( do
          settle <- process key payload
          atomically $ putTMVar accepted ()
          confirmed <- settle
          atomically $ modifyTVar (state work) $ \st ->
            st
              { entries = Map.delete key (entries st)
              , completed =
                  if confirmed && retainCompletion st key then Set.insert key (completed st) else completed st
              }
        )
        `catch` \(err :: SomeException) -> do
          atomically $
            modifyTVar
              (state work)
              (\st -> st{stopped = True, failure = case failure st of Nothing -> Just err; existing -> existing})
          throwIO err
  abort = atomically $ do
    st <- readTVar (state work)
    mapM_ (\(key, (payload, _)) -> abandon key payload) (Map.toList (entries st))
    writeTVar
      (state work)
      st
        { stopped = True
        , failure = Just (toException WorkClosed)
        , entries = Map.empty
        , queued = Seq.empty
        , completed = Set.empty
        }
