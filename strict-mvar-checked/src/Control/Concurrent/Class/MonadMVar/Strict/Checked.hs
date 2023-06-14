{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | This module corresponds to "Control.Concurrent.MVar" in the @base@ package.
--
-- This module can be used as a drop-in replacement for
-- "Control.Concurrent.Class.MonadMVar.Strict", but not the other way around.
module Control.Concurrent.Class.MonadMVar.Strict.Checked (
    -- * StrictMVar
    LazyMVar
  , StrictMVar
  , castStrictMVar
  , fromLazyMVar
  , isEmptyMVar
  , modifyMVar
  , modifyMVarMasked
  , modifyMVarMasked_
  , modifyMVar_
  , newEmptyMVar
  , newEmptyMVarWithInvariant
  , newMVar
  , newMVarWithInvariant
  , putMVar
  , readMVar
  , swapMVar
  , takeMVar
  , toLazyMVar
  , tryPutMVar
  , tryReadMVar
  , tryTakeMVar
  , withMVar
  , withMVarMasked
    -- * Re-exports
  , MonadMVar
  ) where

import           Control.Concurrent.Class.MonadMVar (MonadMVar)
import qualified Control.Concurrent.Class.MonadMVar as Lazy
import           GHC.Stack (HasCallStack)

--
-- StrictMVar
--

type LazyMVar m = Lazy.MVar m

-- | A strict MVar with invariant checking.
--
-- There is a weaker invariant for a 'StrictMVar' than for a 'StrictTVar' (see
-- the @strict-stm@ package): although all functions that modify the
-- 'StrictMVar' check the invariant, we do /not/ guarantee that the value inside
-- the 'StrictMVar' always satisfies the invariant. Instead, we /do/ guarantee
-- that if the 'StrictMVar' is updated with a value that does not satisfy the
-- invariant, an exception is thrown. The reason for this weaker guarantee is
-- that leaving an 'MVar' empty can lead to very hard to debug "blocked
-- indefinitely" problems.
data StrictMVar m a = StrictMVar {
    -- | The invariant that is checked whenever the 'StrictMVar' is updated.
    invariant :: !(a -> Maybe String)
  , mvar      :: !(LazyMVar m a)
  }

castStrictMVar :: LazyMVar m ~ LazyMVar n
               => StrictMVar m a -> StrictMVar n a
castStrictMVar v = StrictMVar (invariant v) (mvar v)

-- | Get the underlying @MVar@
--
-- Since we obviously can not guarantee that updates to this 'LazyMVar' will be
-- strict, this should be used with caution.
--
-- Similarly, we can not guarantee that updates to this 'LazyMVar' do not break
-- the original invariant that the 'StrictMVar' held.
toLazyMVar :: StrictMVar m a -> LazyMVar m a
toLazyMVar = mvar

-- | Create a 'StrictMVar' from a 'LazyMVar'
--
-- It is not guaranteed that the 'LazyMVar' contains a value that is in WHNF, so
-- there is no guarantee that the resulting 'StrictMVar' contains a value that
-- is in WHNF. This should be used with caution.
--
-- The resulting 'StrictMVar' has a trivial invariant.
fromLazyMVar :: Lazy.MVar m a -> StrictMVar m a
fromLazyMVar = StrictMVar (const Nothing)

newEmptyMVar :: MonadMVar m => m (StrictMVar m a)
newEmptyMVar = fromLazyMVar <$> Lazy.newEmptyMVar

newEmptyMVarWithInvariant :: MonadMVar m
                          => (a -> Maybe String)
                          -> m (StrictMVar m a)
newEmptyMVarWithInvariant inv = StrictMVar inv <$> Lazy.newEmptyMVar

newMVar :: MonadMVar m => a -> m (StrictMVar m a)
newMVar !a = fromLazyMVar <$> Lazy.newMVar a

newMVarWithInvariant :: (HasCallStack, MonadMVar m)
                     => (a -> Maybe String)
                     -> a
                     -> m (StrictMVar m a)
newMVarWithInvariant inv !a =
  checkInvariant (inv a) $
  StrictMVar inv <$> Lazy.newMVar a

takeMVar :: MonadMVar m => StrictMVar m a -> m a
takeMVar = Lazy.takeMVar . mvar

putMVar :: (HasCallStack, MonadMVar m) => StrictMVar m a -> a -> m ()
putMVar v !a = do
  Lazy.putMVar (mvar v) a
  checkInvariant (invariant v a) $ pure ()

readMVar :: MonadMVar m => StrictMVar m a -> m a
readMVar v = Lazy.readMVar (mvar v)

swapMVar :: (HasCallStack, MonadMVar m) => StrictMVar m a -> a -> m a
swapMVar v !a = do
  oldValue <- Lazy.swapMVar (mvar v) a
  checkInvariant (invariant v a) $ pure oldValue

tryTakeMVar :: MonadMVar m => StrictMVar m a -> m (Maybe a)
tryTakeMVar v = Lazy.tryTakeMVar (mvar v)

tryPutMVar :: (HasCallStack, MonadMVar m) => StrictMVar m a -> a -> m Bool
tryPutMVar v !a = do
  didPut <- Lazy.tryPutMVar (mvar v) a
  checkInvariant (invariant v a) $ pure didPut

isEmptyMVar :: MonadMVar m => StrictMVar m a -> m Bool
isEmptyMVar v = Lazy.isEmptyMVar (mvar v)

withMVar :: MonadMVar m => StrictMVar m a -> (a -> m b) -> m b
withMVar v = Lazy.withMVar (mvar v)

withMVarMasked :: MonadMVar m => StrictMVar m a -> (a -> m b) -> m b
withMVarMasked v = Lazy.withMVarMasked (mvar v)

-- | 'modifyMVar_' is defined in terms of 'modifyMVar'.
modifyMVar_ :: (HasCallStack, MonadMVar m)
            => StrictMVar m a
            -> (a -> m a)
            -> m ()
modifyMVar_ v io = modifyMVar v io'
  where io' a = (,()) <$> io a

modifyMVar :: (HasCallStack, MonadMVar m)
           => StrictMVar m a
           -> (a -> m (a,b))
           -> m b
modifyMVar v io = do
    (a', b) <- Lazy.modifyMVar (mvar v) io'
    checkInvariant (invariant v a') $ pure b
  where
    io' a = do
      (!a', b) <- io a
      -- Returning @a'@ along with @b@ allows us to check the invariant /after/
      -- filling in the MVar.
      pure (a' , (a', b))

-- | 'modifyMVarMasked_' is defined in terms of 'modifyMVarMasked'.
modifyMVarMasked_ :: (HasCallStack, MonadMVar m)
                  => StrictMVar m a
                  -> (a -> m a)
                  -> m ()
modifyMVarMasked_ v io = modifyMVar v io'
  where io' a = (,()) <$> io a

modifyMVarMasked :: (HasCallStack, MonadMVar m)
                 => StrictMVar m a
                 -> (a -> m (a,b))
                 -> m b
modifyMVarMasked v io = do
    (a', b) <- Lazy.modifyMVar (mvar v) io'
    checkInvariant (invariant v a') $ pure b
  where
    io' a = do
      (!a', b) <- io a
      -- Returning @a'@ along with @b@ allows us to check the invariant /after/
      -- filling in the MVar.
      pure (a', (a', b))

tryReadMVar :: MonadMVar m => StrictMVar m a -> m (Maybe a)
tryReadMVar v = Lazy.tryReadMVar (mvar v)

--
-- Dealing with invariants
--

-- | Check invariant
--
-- @checkInvariant mErr x@ is equal to @x@ if @mErr == Nothing@, and throws an
-- error @err@ if @mErr == Just err@.
checkInvariant :: HasCallStack => Maybe String -> a -> a
checkInvariant Nothing    k = k
checkInvariant (Just err) _ = error $ "StrictMVar invariant violation: " ++ err
