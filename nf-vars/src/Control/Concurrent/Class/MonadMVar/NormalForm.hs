{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | 'StrictMVar's with 'NoThunks' invariants.
--
-- Custom invariants can still be specified in addition to the default
-- 'NoThunks' invariant. See 'newMVarWithInvariant' and
-- 'newEmptyMVarWithInvariant'.
--
-- Use the @checkmvarinvariants@ cabal flag from the @strict-checked-vars@
-- package to enable or disable invariant checks at compile time.
--
-- The exports of this module (should) mirror the exports of the
-- "Control.Concurrent.Class.MonadMVar.Strict.Checked" module from the
-- @strict-checked-vars@ package.
module Control.Concurrent.Class.MonadMVar.NormalForm (
    -- * StrictMVar
    newEmptyMVar
  , newEmptyMVarWithInvariant
  , newMVar
  , newMVarWithInvariant
    -- * Invariant
  , noThunksInvariant
    -- * Unchecked
  , uncheckedNewEmptyMVar
  , uncheckedNewMVar
    -- * Re-exports
  , module Control.Concurrent.Class.MonadMVar.Strict.Checked
  ) where

import           Control.Concurrent.Class.MonadMVar (MonadInspectMVar (..))
import           Control.Concurrent.Class.MonadMVar.Strict.Checked hiding
                     (newEmptyMVar, newEmptyMVarWithInvariant, newMVar,
                     newMVarWithInvariant)
import qualified Control.Concurrent.Class.MonadMVar.Strict.Checked as Checked
import           Data.Proxy (Proxy (..))
import           GHC.Stack (HasCallStack)
import           NoThunks.Class (NoThunks (..))
import           NoThunks.Invariant

{-------------------------------------------------------------------------------
  StrictMVar
-------------------------------------------------------------------------------}

-- | Create a 'StrictMVar' with a 'NoThunks' invariant.
newMVar :: (HasCallStack, MonadMVar m, NoThunks a) => a -> m (StrictMVar m a)
newMVar = Checked.newMVarWithInvariant noThunksInvariant

-- | Create an empty 'StrictMVar' with a 'NoThunks' invariant.
newEmptyMVar :: (MonadMVar m, NoThunks a) => m (StrictMVar m a)
newEmptyMVar = Checked.newEmptyMVarWithInvariant noThunksInvariant

-- | Create a 'StrictMVar' with a custom invariant /and/ a 'NoThunks' invariant.
--
-- When both the custom and 'NoThunks' invariants are broken, only the error
-- related to the custom invariant is reported.
newMVarWithInvariant ::
     (HasCallStack, MonadMVar m, NoThunks a)
  => (a -> Maybe String)
  -> a
  -> m (StrictMVar m a)
newMVarWithInvariant inv =
    Checked.newMVarWithInvariant (\x -> inv x <> noThunksInvariant x)

-- | Create an empty 'StrictMVar' with a custom invariant /and/ a 'NoThunks'
-- invariant.
--
-- When both the custom and 'NoThunks' invariants are broken, only the error
-- related to the custom invariant is reported.
newEmptyMVarWithInvariant ::
     (MonadMVar m, NoThunks a)
  => (a -> Maybe String)
  -> m (StrictMVar m a)
newEmptyMVarWithInvariant inv =
    Checked.newEmptyMVarWithInvariant (\x -> inv x <> noThunksInvariant x)

{-------------------------------------------------------------------------------
  NoThunks instance
-------------------------------------------------------------------------------}

instance NoThunks a => NoThunks (StrictMVar IO a) where
  showTypeOf _ = "StrictMVar IO"
  wNoThunks ctxt mvar = do
      aMay <- inspectMVar (Proxy :: Proxy IO) (toLazyMVar mvar)
      noThunks ctxt aMay

{-------------------------------------------------------------------------------
  Unchecked
-------------------------------------------------------------------------------}

-- | Like 'newMVar', but without a 'NoThunks' invariant.
uncheckedNewMVar :: MonadMVar m => a -> m (StrictMVar m a)
uncheckedNewMVar = Checked.newMVar

-- | Like 'newEmptyMVar', but without a 'NoThunks' invariant.
uncheckedNewEmptyMVar :: MonadMVar m => m (StrictMVar m a)
uncheckedNewEmptyMVar = Checked.newEmptyMVar
