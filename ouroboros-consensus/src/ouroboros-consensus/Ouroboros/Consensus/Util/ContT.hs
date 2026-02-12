{-# LANGUAGE FlexibleContexts #-}

module Ouroboros.Consensus.Util.ContT
  ( tryContT
  , escapable
  , ContT (..)
  , evalContT
  , lift
  , liftContT
  , withReadAccessCont
  ) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.RAWLock
import Ouroboros.Consensus.Util.IOLike

withReadAccessCont ::
  (MonadSTM m, MonadCatch m, MonadThrow (STM m)) =>
  RAWLock m st ->
  (st -> ContT a m a) ->
  ContT r m a
withReadAccessCont lock action = lift $
  withReadAccess lock $
    \st -> runContT (action st) return

-- | Allocates a resource and returns:
-- 1. The Resource
-- 2. A Safe (Idempotent) Cleanup Action (to save for later)
-- 3. A "Disarm" Action (STM) to cancel the automatic local cleanup
escapable ::
  (MonadSTM m, MonadMask m) =>
  -- | Allocator
  m res ->
  -- | Releaser
  (res -> m ()) ->
  ContT r m (res, m (), STM m ())
escapable alloc release = ContT $ \callback -> do
  bracket
    ( do
        -- Create the "Run Once" guard for the cleanup. This ensures we never
        -- double-free, even if Global and Local try at once.
        hasRunVar <- atomically $ newTVar False
        res <- alloc

        let safeCleanup = do
              shouldRun <- atomically $ do
                ran <- readTVar hasRunVar
                unless ran $ writeTVar hasRunVar True
                return (not ran)
              when shouldRun (release res)

        -- Create the "Local Responsibility" flag
        isLocalVar <- atomically $ newTVar True
        pure (res, safeCleanup, isLocalVar)
    )
    ( \(_, safeCleanup, isLocalVar) -> do
        -- On local scope exit, check if we are still responsible
        mustClean <- atomically $ readTVar isLocalVar
        when mustClean safeCleanup
    )
    ( \(res, safeCleanup, isLocalVar) -> do
        -- Define the Disarm action
        let disarm = writeTVar isLocalVar False
        callback (res, safeCleanup, disarm)
    )

liftContT ::
  -- | Inner: Returns (m (Either e r))
  ContT (Either e r) m a ->
  -- | Outer: Returns (ExceptT e m r)
  ContT r (ExceptT e m) a
liftContT (ContT inner) = ContT $ \continuation ->
  ExceptT $ inner (runExceptT . continuation)

tryContT ::
  Monad m =>
  ContT (Either e r) (ExceptT e m) a ->
  ContT r m (Either e a)
tryContT (ContT run) = ContT $ \k -> do
  -- k :: Either e a -> m r

  let callback a = ExceptT $ do
        -- Run the outer continuation 'k' with the success value. This gives us
        -- the final result 'r'.
        finalResult <- k (Right a)
        return (Right (Right finalResult))

  -- Run the inner computation
  --    result :: Either e (Either e r)
  result <- runExceptT (run callback)

  case result of
    -- A. The ExceptT threw an exception (throwE)
    Left e -> k (Left e)
    -- B. The computation finished successfully
    Right (Right r) -> return r
    -- C. The computation finished, but technically returned a Left
    --    (This happens if the inner generic 'r' was instantiated to 'Left e')
    Right (Left e) -> k (Left e)
