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
  , trySubmitWork
  , pruneCompleted
  , withOwnedWork
  ) where

import Control.Monad (forever, unless, void)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Ouroboros.Consensus.Util.IOLike

-- | Queued and running entries share a hard bound. A running entry retains its
-- identity until settlement, not merely until its worker starts. Only positively
-- completed keys are remembered afterwards; these contain no payload.
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

-- | Admission or a join was interrupted by the enclosing lifetime closing.
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
    Just err -> throwSTM err
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

-- | Optional cache lookup must never park the metadata dispatcher behind slow
-- producer capacity. A miss here leaves ordinary network fetching eligible.
trySubmitWork :: (IOLike m, Ord key) => OwnedWork m key payload -> key -> payload -> STM m ()
trySubmitWork work key payload = do
  st <- readTVar (state work)
  closing <- externallyClosing work
  unless (closing || stopped st) $ void (submitWork work key payload) `orElse` pure ()

awaitResult :: IOLike m => OwnedWork m key payload -> StrictTMVar m () -> STM m ()
awaitResult work accepted = do
  st <- readTVar (state work)
  case failure st of
    Just err -> throwSTM err
    Nothing -> readTMVar accepted

-- | Forget only completion evidence. Queued/running owners survive pruning
-- until their handler reconciles retirement; no old worker can erase a new one.
pruneCompleted :: IOLike m => OwnedWork m key payload -> (key -> Bool) -> STM m ()
pruneCompleted work keep = modifyTVar (state work) $ \st -> st{completed = Set.filter keep (completed st), retainCompletion = keep}

-- | Activate under the receipt supervisor, before exposing node applications.
-- Each handler returns a settlement action after writer admission. Forge callers
-- regain control then, while the entry remains owned until that action finishes.
-- True means positively confirmed completion; empty/abandoned jobs are not proof.
-- Normal shutdown closes admission and drains; failure cancels/joins the worker.
withOwnedWork ::
  forall m key payload a.
  (IOLike m, Ord key) =>
  OwnedWork m key payload ->
  (key -> payload -> m (m Bool)) ->
  m a ->
  m a
withOwnedWork work process action = mask $ \restore ->
  bracket (asyncWithUnmask (\unmask -> unmask worker)) cancel $ \child -> do
    link child
    result <- restore action `onException` abort
    ( do
        atomically $ modifyTVar (state work) (\st -> st{stopped = True})
        restore $ atomically $ readTVar (state work) >>= check . Map.null . entries
      )
      `onException` abort
    pure result
 where
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
          atomically $ modifyTVar (state work) (\st -> st{stopped = True, failure = Just err})
          throwIO err
  abort = atomically $ modifyTVar (state work) $ \st ->
    st
      { stopped = True
      , failure = Just (toException WorkClosed)
      , entries = Map.empty
      , queued = Seq.empty
      , completed = Set.empty
      }
