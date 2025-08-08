{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Util.STM
  ( -- * 'Watcher'
    Watcher (..)
  , forkLinkedWatcher
  , withWatcher

    -- * Misc
  , Fingerprint (..)
  , WithFingerprint (..)
  , blockUntilAllJust
  , blockUntilChanged
  , blockUntilJust
  , runWhenJust

    -- * Simulate various monad stacks in STM
  , Sim (..)
  , simId
  , simStateT

    -- * withTMVar
  , withTMVar
  , withTMVarAnd
  ) where

import Control.Monad (void)
import Control.Monad.State (StateT (..))
import Control.ResourceRegistry
import Data.Void
import GHC.Stack
import Ouroboros.Consensus.Util.IOLike
import Ouroboros.Network.BlockFetch.ConsensusInterface
  ( Fingerprint (..)
  , WithFingerprint (..)
  )

{-------------------------------------------------------------------------------
  Misc
-------------------------------------------------------------------------------}

-- | Wait until the TVar changed
blockUntilChanged ::
  forall m a b.
  (MonadSTM m, Eq b) =>
  (a -> b) -> b -> STM m a -> STM m (a, b)
blockUntilChanged f b getA = do
  a <- getA
  let b' = f a
  if b' == b
    then retry
    else return (a, b')

-- | Spawn a new thread that waits for an STM value to become 'Just'
--
-- The thread will be linked to the registry.
runWhenJust ::
  IOLike m =>
  ResourceRegistry m ->
  -- | Label for the thread
  String ->
  STM m (Maybe a) ->
  (a -> m ()) ->
  m ()
runWhenJust registry label getMaybeA action =
  void $
    forkLinkedThread registry label $
      action =<< atomically (blockUntilJust getMaybeA)

blockUntilJust :: MonadSTM m => STM m (Maybe a) -> STM m a
blockUntilJust getMaybeA = do
  ma <- getMaybeA
  case ma of
    Nothing -> retry
    Just a -> return a

blockUntilAllJust :: MonadSTM m => [STM m (Maybe a)] -> STM m [a]
blockUntilAllJust = mapM blockUntilJust

{-------------------------------------------------------------------------------
  Simulate monad stacks
-------------------------------------------------------------------------------}

newtype Sim n m = Sim {runSim :: forall a. n a -> STM m a}

simId :: Sim (STM m) m
simId = Sim id

simStateT :: IOLike m => StrictTVar m st -> Sim n m -> Sim (StateT st n) m
simStateT stVar (Sim k) = Sim $ \(StateT f) -> do
  st <- readTVar stVar
  (a, st') <- k (f st)
  writeTVar stVar st'
  return a

{-------------------------------------------------------------------------------
  Watchers
-------------------------------------------------------------------------------}

-- | Specification for a thread that watches a variable, and reports interesting
-- changes.
--
-- NOTE: STM does not guarantee that 'wNotify' will /literally/ be called on
-- /every/ change: when the system is under heavy load, some updates may be
-- missed.
data Watcher m a fp = Watcher
  { wFingerprint :: a -> fp
  -- ^ Obtain a fingerprint from a value of the monitored variable.
  , wInitial :: Maybe fp
  -- ^ The initial fingerprint
  --
  -- If 'Nothing', the action is executed once immediately to obtain the
  -- initial fingerprint.
  , wNotify :: a -> m ()
  -- ^ An action executed each time the fingerprint changes.
  , wReader :: STM m a
  -- ^ The variable to monitor.
  }

-- | Execute a 'Watcher'
--
-- NOT EXPORTED
runWatcher ::
  forall m a fp.
  (IOLike m, Eq fp, HasCallStack) =>
  Watcher m a fp ->
  m Void
runWatcher watcher = do
  initB <- case mbInitFP of
    Just fp -> return fp
    Nothing -> do
      a <- atomically getA
      notify a
      return $ f a
  loop initB
 where
  Watcher
    { wFingerprint = f
    , wInitial = mbInitFP
    , wNotify = notify
    , wReader = getA
    } = watcher

  loop :: fp -> m Void
  loop fp = do
    (a, fp') <- atomically $ blockUntilChanged f fp getA
    notify a
    loop fp'

-- | Spawn a new thread that runs a 'Watcher'
--
-- The thread will be linked to the registry.
forkLinkedWatcher ::
  forall m a fp.
  (IOLike m, Eq fp, HasCallStack) =>
  ResourceRegistry m ->
  -- | Label for the thread
  String ->
  Watcher m a fp ->
  m (Thread m Void)
forkLinkedWatcher registry label watcher =
  forkLinkedThread registry label $ runWatcher watcher

-- | Spawn a new thread that runs a 'Watcher'
--
-- The thread is bracketed via 'withAsync' and 'link'ed.
--
-- We do not provide the 'Async' handle only because our anticipated use cases
-- don't need it.
withWatcher ::
  forall m a fp r.
  (IOLike m, Eq fp, HasCallStack) =>
  -- | Label for the thread
  String ->
  Watcher m a fp ->
  m r ->
  m r
withWatcher label watcher k =
  withAsync
    (do labelThisThread label; runWatcher watcher)
    (\h -> do link h; k)

{-------------------------------------------------------------------------------
  withTMVar
-------------------------------------------------------------------------------}

-- | Apply @f@ with the content of @tv@ as state, restoring the original value when an
-- exception occurs
withTMVar ::
  IOLike m =>
  StrictTMVar m a ->
  (a -> m (c, a)) ->
  m c
withTMVar tv f = withTMVarAnd tv (const $ pure ()) (\a -> const $ f a)

-- | Apply @f@ with the content of @tv@ as state, restoring the original value
-- when an exception occurs. Additionally run a @STM@ action when acquiring the
-- value.
withTMVarAnd ::
  IOLike m =>
  StrictTMVar m a ->
  -- | Additional STM action to run in the same atomically
  -- block as the TMVar is acquired
  (a -> STM m b) ->
  -- | Action
  (a -> b -> m (c, a)) ->
  m c
withTMVarAnd tv guard f =
  fst . fst
    <$> generalBracket
      ( atomically $ do
          istate <- takeTMVar tv
          guarded <- guard istate
          pure (istate, guarded)
      )
      ( \(origState, _) -> \case
          ExitCaseSuccess (_, newState) ->
            atomically $ putTMVar tv newState
          ExitCaseException _ ->
            atomically $ putTMVar tv origState
          ExitCaseAbort ->
            atomically $ putTMVar tv origState
      )
      (uncurry f)
