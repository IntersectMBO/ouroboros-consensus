{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'StrictTVar's with 'NoThunks' invariants.
--
-- Custom invariants can still be specified in addition to the default
-- 'NoThunks' invariant. See 'newTVarWithInvariant' and
-- 'newTVarWithInvariantIO'.
--
-- Use the @checktvarinvariants@ cabal flag from the @strict-checked-vars@
-- package to enable or disable invariant checks at compile time.
--
-- The exports of this module (should) mirror the exports of the
-- "Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked" module from the
-- @strict-checked-vars@ package.
module Control.Concurrent.Class.MonadSTM.NormalForm.TVar (
    -- * StrictTVar
    newTVar
  , newTVarIO
  , newTVarWithInvariant
  , newTVarWithInvariantIO
    -- * Invariant
  , noThunksInvariant
    -- * Unchecked
  , uncheckedNewTVarM
    -- * Re-exports
  , module Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked hiding
                     (checkInvariant, newTVar, newTVarIO, newTVarWithInvariant,
                     newTVarWithInvariantIO)
import qualified Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked as Checked
import           Control.Monad.Class.MonadSTM as StrictSTM
import           GHC.Stack
import           NoThunks.Class (NoThunks (..))
import           NoThunks.Invariant (noThunksInvariant)

{-------------------------------------------------------------------------------
  StrictTVar
-------------------------------------------------------------------------------}

-- | Create a 'StrictTVar' with a 'NoThunks' invariant.
newTVar :: (HasCallStack, MonadSTM m, NoThunks a) => a -> STM m (StrictTVar m a)
newTVar = Checked.newTVarWithInvariant noThunksInvariant

-- | Create an 'StrictTVar' with a 'NoThunks' invariant.
newTVarIO :: (HasCallStack, MonadSTM m, NoThunks a) => a -> m (StrictTVar m a)
newTVarIO = Checked.newTVarWithInvariantIO noThunksInvariant

-- | Create a 'StrictTVar' with a custom invariant /and/ a 'NoThunks' invariant.
--
-- When both the custom and 'NoThunks' invariants are broken, only the error
-- related to the custom invariant is reported.
newTVarWithInvariant ::
     (HasCallStack, MonadSTM m, NoThunks a)
  => (a -> Maybe String)
  -> a
  -> STM m (StrictTVar m a)
newTVarWithInvariant inv =
    Checked.newTVarWithInvariant (\x -> inv x <> noThunksInvariant x)

-- | Create a 'StrictTVar' with a custom invariant /and/ a 'NoThunks' invariant.
--
-- When both the custom and 'NoThunks' invariants are broken, only the error
-- related to the custom invariant is reported.
newTVarWithInvariantIO ::
     (HasCallStack, MonadSTM m, NoThunks a)
  => (a -> Maybe String)
  -> a
  -> m (StrictTVar m a)
newTVarWithInvariantIO inv =
    Checked.newTVarWithInvariantIO (\x -> inv x <> noThunksInvariant x)

{-------------------------------------------------------------------------------
  NoThunks instance
-------------------------------------------------------------------------------}

instance NoThunks a => NoThunks (StrictTVar IO a) where
  showTypeOf _ = "StrictTVar IO"
  wNoThunks ctxt tv = do
      -- We can't use @atomically $ readTVar ..@ here, as that will lead to a
      -- "Control.Concurrent.STM.atomically was nested" exception.
      a <- readTVarIO tv
      noThunks ctxt a

{-------------------------------------------------------------------------------
  Unchecked
-------------------------------------------------------------------------------}

-- | Like 'newTVarIO', but without a 'NoThunks' invariant.
uncheckedNewTVarM :: MonadSTM m => a -> m (StrictTVar m a)
uncheckedNewTVarM = Checked.newTVarIO
