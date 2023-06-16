{-# LANGUAGE TypeFamilies #-}

-- | This module corresponds to "Control.Concurrent.STM.TVar" in the @stm@ package.
--
-- This module can be used as a drop-in replacement for
-- "Control.Concurrent.Class.MonadSTM.Strict.TVar", but not the other way
-- around.
module Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked (
    -- * StrictTVar
    LazyTVar
  , StrictTVar
  , castStrictTVar
  , fromLazyTVar
  , modifyTVar
  , newTVar
  , newTVarIO
  , newTVarWithInvariant
  , newTVarWithInvariantIO
  , readTVar
  , readTVarIO
  , stateTVar
  , swapTVar
  , toLazyTVar
  , writeTVar
    -- * MonadLabelSTM
  , labelTVar
  , labelTVarIO
    -- * MonadTraceSTM
  , traceTVar
  , traceTVarIO
  ) where

import           Control.Concurrent.Class.MonadSTM (InspectMonad,
                     MonadLabelledSTM, MonadSTM, MonadTraceSTM, STM, TraceValue,
                     atomically)
import qualified Control.Concurrent.Class.MonadSTM.Strict.TVar as Strict
import           GHC.Stack (HasCallStack)

{-------------------------------------------------------------------------------
  StrictTVar
-------------------------------------------------------------------------------}

type LazyTVar m = Strict.LazyTVar m

data StrictTVar m a = StrictTVar {
    -- | Invariant checked whenever updating the 'StrictTVar'.
    invariant :: !(a -> Maybe String)
  , tvar      :: !(Strict.StrictTVar m a)
  }

castStrictTVar :: LazyTVar m ~ LazyTVar n
               => StrictTVar m a -> StrictTVar n a
castStrictTVar v = StrictTVar (invariant v) (Strict.castStrictTVar $ tvar v)

-- | Get the underlying @TVar@
--
-- Since we obviously cannot guarantee that updates to this 'LazyTVar' will be
-- strict, this should be used with caution.
--
-- Similarly, we can not guarantee that updates to this 'LazyTVar' do not break
-- the original invariant that the 'StrictTVar' held.
toLazyTVar :: StrictTVar m a -> LazyTVar m a
toLazyTVar = Strict.toLazyTVar . tvar

-- | Create a 'StrictMVar' from a 'LazyMVar'
--
-- It is not guaranteed that the 'LazyTVar' contains a value that is in WHNF, so
-- there is no guarantee that the resulting 'StrictTVar' contains a value that
-- is in WHNF. This should be used with caution.
--
-- The resulting 'StrictTVar' has a trivial invariant.
fromLazyTVar :: LazyTVar m a -> StrictTVar m a
fromLazyTVar = StrictTVar (const Nothing) . Strict.fromLazyTVar

newTVar :: MonadSTM m => a -> STM m (StrictTVar m a)
newTVar a = StrictTVar (const Nothing) <$> Strict.newTVar a

newTVarIO :: MonadSTM m => a -> m (StrictTVar m a)
newTVarIO = newTVarWithInvariantIO (const Nothing)

newTVarWithInvariant :: (MonadSTM m, HasCallStack)
                     => (a -> Maybe String)
                     -> a
                     -> STM m (StrictTVar m a)
newTVarWithInvariant  inv a =
    checkInvariant (inv a) $
    StrictTVar inv <$> Strict.newTVar a

newTVarWithInvariantIO :: (MonadSTM m, HasCallStack)
                       => (a -> Maybe String)
                       -> a
                       -> m (StrictTVar m a)
newTVarWithInvariantIO  inv a =
    checkInvariant (inv a) $
    StrictTVar inv <$> Strict.newTVarIO a

readTVar :: MonadSTM m => StrictTVar m a -> STM m a
readTVar = Strict.readTVar . tvar

readTVarIO :: MonadSTM m => StrictTVar m a -> m a
readTVarIO = Strict.readTVarIO . tvar

writeTVar :: (MonadSTM m, HasCallStack) => StrictTVar m a -> a -> STM m ()
writeTVar v a =
    checkInvariant (invariant v a) $
    Strict.writeTVar (tvar v) a

modifyTVar :: MonadSTM m => StrictTVar m a -> (a -> a) -> STM m ()
modifyTVar v f = readTVar v >>= writeTVar v . f

stateTVar :: MonadSTM m => StrictTVar m s -> (s -> (a, s)) -> STM m a
stateTVar v f = do
    a <- readTVar v
    let (b, a') = f a
    writeTVar v a'
    return b

swapTVar :: MonadSTM m => StrictTVar m a -> a -> STM m a
swapTVar v a' = do
    a <- readTVar v
    writeTVar v a'
    return a

--
-- Dealing with invariants
--

-- | Check invariant
--
-- @checkInvariant mErr x@ is equal to @x@ if @mErr == Nothing@, and throws an
-- error @err@ if @mErr == Just err@.
checkInvariant :: HasCallStack => Maybe String -> a -> a
checkInvariant Nothing    k = k
checkInvariant (Just err) _ = error $ "StrictTVar invariant violation: " ++ err

{-------------------------------------------------------------------------------
  MonadLabelledSTM
-------------------------------------------------------------------------------}

labelTVar :: MonadLabelledSTM m => StrictTVar m a -> String -> STM m ()
labelTVar = Strict.labelTVar . tvar

labelTVarIO :: MonadLabelledSTM m => StrictTVar m a -> String -> m ()
labelTVarIO v = atomically . labelTVar v

{-------------------------------------------------------------------------------
  MonadTraceSTM
-------------------------------------------------------------------------------}

traceTVar :: MonadTraceSTM m
          => proxy m
          -> StrictTVar m a
          -> (Maybe a -> a -> InspectMonad m TraceValue)
          -> STM m ()
traceTVar p = Strict.traceTVar p . tvar

traceTVarIO :: MonadTraceSTM m
            => StrictTVar m a
            -> (Maybe a -> a -> InspectMonad m TraceValue)
            -> m ()
traceTVarIO = Strict.traceTVarIO . tvar
