{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Consensus.Util.EscapableResources
  ( -- * Escapable resources
    escapable
  , acquireEscapableHandleWithResource

    -- * Reexports
  , ContT (..)
  , evalContT
  , mapContT
  , mapCont
  , lift
  ) where

import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Cont
import Ouroboros.Consensus.Util.IOLike

escapable ::
  (MonadSTM m, MonadMask m) =>
  -- | Allocator
  m res ->
  -- | Releaser
  (res -> m ()) ->
  ContT r m (res, m (), STM m ())
escapable alloc release = ContT $ \callback -> do
  -- A. Create the "Run Once" guard for the cleanup
  --    This ensures we never double-free, even if Global and Local try at once.
  hasRunVar <- atomically $ newTVar False
  res <- alloc

  let safeCleanup = do
        shouldRun <- atomically $ do
          ran <- readTVar hasRunVar
          unless ran $ writeTVar hasRunVar True
          return (not ran)
        when shouldRun (release res)

  -- B. Create the "Local Responsibility" flag
  isLocalVar <- atomically $ newTVar True

  -- C. The Bracket
  bracket
    (return res)
    ( \_ -> do
        -- On local scope exit, check if we are still responsible
        mustClean <- atomically $ readTVar isLocalVar
        when mustClean safeCleanup
    )
    ( \_ -> do
        -- D. Define the Disarm action
        let disarm = writeTVar isLocalVar False

        -- Pass everything to the inner function
        callback (res, safeCleanup, disarm)
    )

-- | For example: opening a handle
type Alloc m res = m res

-- | For example: closing a handle
type Free m res = res -> m ()

-- | Opaque freeing action
type Free' m = m ()

-- | For example: storing the wrapper over the handle in some other component,
-- like one of the Databases.
type StoreSomewhereElse m serv = serv -> STM m ()

-- | Create the handle, ingesting functions for releasing the acquired resource
-- as well as store the wrapper in some global storage. This for example will be
-- `newInMemoryLedgerTablesHandle`
type CreateHandle r m res serv = res -> Free' m -> StoreSomewhereElse m serv -> ContT r m serv

acquireEscapableHandleWithResource ::
  (MonadSTM m, MonadMask m) =>
  Alloc m res ->
  Free m res ->
  StoreSomewhereElse m serv ->
  CreateHandle r m res serv ->
  ContT r m serv
acquireEscapableHandleWithResource alloc free steal toHandle = do
  (rawRes, safeCleanup, disarm) <- escapable alloc free
  toHandle rawRes safeCleanup (\s -> disarm >> steal s)
