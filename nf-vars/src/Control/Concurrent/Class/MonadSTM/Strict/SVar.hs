{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Control.Concurrent.Class.MonadSTM.Strict.SVar (
    castStrictSVar
  , isEmptySVar
  , modifySVar
  , modifySVar_
  , newEmptySVar
  , newEmptySVarWithInvariant
  , newSVar
  , newSVarWithInvariant
  , putSVar
  , readSVar
  , readSVarSTM
  , swapSVar
  , takeSVar
  , tryPutSVar
  , tryReadSVar
  , tryTakeSVar
  , updateSVar
  , updateSVar_
    -- * constructors exported for benefit of tests
  , StrictSVar (..)
  ) where

import           Control.Concurrent.Class.MonadSTM
import qualified Control.Concurrent.Class.MonadSTM as Lazy
import           Control.Concurrent.Class.MonadSTM.Strict.TVar.Checked
                     (checkInvariant)
import           Control.Monad (when)
import           Control.Monad.Class.MonadThrow (ExitCase (..), MonadCatch,
                     generalBracket)
import           GHC.Stack
import           NoThunks.Class (NoThunks (..))

{-------------------------------------------------------------------------------
  Strict SVar
-------------------------------------------------------------------------------}

-- | Strict SVar (modelled using a lazy 'Lazy.TMVar' under the hood)
--
-- The 'StrictSVar' API is slightly stronger than the usual 'SVar' one, as we
-- offer a primitive to read the value of the SVar even if it is empty (in which
-- case we will return the oldest known stale one). See 'readSVarSTM'.
--
-- There is a weaker invariant for a 'StrictSVar' than for a 'StrictTVar':
-- although all functions that modify the 'StrictSVar' check the invariant, we
-- do /not/ guarantee that the value inside the 'StrictSVar' always satisfies
-- the invariant. Instead, we /do/ guarantee that if the 'StrictSVar' is updated
-- with a value that does not satisfy the invariant, an exception is thrown. The
-- reason for this weaker guarantee is that leaving an 'SVar' empty can lead to
-- very hard to debug "blocked indefinitely" problems.
--
-- This is also the reason we do not offer support for an invariant in
-- 'StrictTMVar': if we throw an exception from an STM transaction, the STM
-- transaction is not executed, and so we would not even be able to provide the
-- weaker guarantee that we provide for 'StrictSVar'.
data StrictSVar m a = StrictSVar
  { invariant :: !(a -> Maybe String)
    -- ^ Invariant checked whenever updating the 'StrictSVar'.
  , tmvar     :: !(Lazy.TMVar m a)
    -- ^ The main TMVar supporting this 'StrictSVar'
  , tvar      :: !(Lazy.TVar m a)
    -- ^ TVar for supporting 'readSVarSTM'
    --
    -- This TVar is always kept up to date with the 'Lazy.TMVar', but holds on
    -- the old value of the 'Lazy.TMVar' when it is empty. This is very useful
    -- to support single writer/many reader scenarios.
    --
    -- NOTE: We should always update the 'tmvar' before the 'tvar' so that if
    -- the update to the 'tmvar' fails, the 'tvar is left unchanged.
  }

castStrictSVar :: ( Lazy.TMVar m ~ Lazy.TMVar n
                  , Lazy.TVar  m ~ Lazy.TVar  n
                  )
               => StrictSVar m a -> StrictSVar n a
castStrictSVar StrictSVar{..} = StrictSVar{..}

newSVar :: MonadSTM m => a -> m (StrictSVar m a)
newSVar = newSVarWithInvariant (const Nothing)

newSVarWithInvariant :: (MonadSTM m, HasCallStack)
                     => (a -> Maybe String)  -- ^ Invariant (expect 'Nothing')
                     -> a
                     -> m (StrictSVar m a)
newSVarWithInvariant invariant !a =
    checkInvariant (invariant a) $
    StrictSVar invariant <$> Lazy.newTMVarIO a <*> Lazy.newTVarIO a

newEmptySVar :: MonadSTM m => a -> m (StrictSVar m a)
newEmptySVar = newEmptySVarWithInvariant (const Nothing)

-- | Create an initially empty 'StrictSVar'
--
-- NOTE: Since 'readSVarSTM' allows to read the 'StrictSVar' even when it is
-- empty, we need an initial value of @a@ even though the 'StrictSVar' starts
-- out empty. However, we are /NOT/ strict in this value, to allow it to be
-- @error@.
newEmptySVarWithInvariant :: MonadSTM m
                          => (a -> Maybe String)  -- ^ Invariant (expect 'Nothing')
                          -> a                    -- ^ The initial stale value
                          -> m (StrictSVar m a)
newEmptySVarWithInvariant invariant stale =
    StrictSVar invariant <$> Lazy.newEmptyTMVarIO <*> Lazy.newTVarIO stale

takeSVar :: MonadSTM m => StrictSVar m a -> m a
takeSVar StrictSVar { tmvar } = atomically $ Lazy.takeTMVar tmvar

tryTakeSVar :: MonadSTM m => StrictSVar m a -> m (Maybe a)
tryTakeSVar StrictSVar { tmvar } = atomically $ Lazy.tryTakeTMVar tmvar

putSVar :: (MonadSTM m, HasCallStack) => StrictSVar m a -> a -> m ()
putSVar StrictSVar { tmvar, tvar, invariant } !a = do
    atomically $ do
        Lazy.putTMVar tmvar a
        Lazy.writeTVar tvar a
    checkInvariant (invariant a) $ return ()

tryPutSVar :: (MonadSTM m, HasCallStack) => StrictSVar m a -> a -> m Bool
tryPutSVar StrictSVar { tmvar, tvar, invariant } !a = do
    didPut <- atomically $ do
        didPut <- Lazy.tryPutTMVar tmvar a
        when didPut $ Lazy.writeTVar tvar a
        return didPut
    checkInvariant (invariant a) $ return didPut

readSVar :: MonadSTM m => StrictSVar m a -> m a
readSVar StrictSVar { tmvar } = atomically $ Lazy.readTMVar tmvar

tryReadSVar :: MonadSTM m => StrictSVar m a -> m (Maybe a)
tryReadSVar StrictSVar { tmvar } = atomically $ Lazy.tryReadTMVar tmvar

-- | Read the possibly-stale value of the @SVar@
--
-- Will return the current value of the @SVar@ if it non-empty, or the last
-- known value otherwise.
readSVarSTM :: MonadSTM m => StrictSVar m a -> STM m a
readSVarSTM StrictSVar { tmvar, tvar } = do
    ma <- Lazy.tryReadTMVar tmvar
    case ma of
      Just a  -> return a
      Nothing -> Lazy.readTVar tvar

-- | Swap value of a 'StrictSVar'
--
-- NOTE: Since swapping the value can't leave the 'StrictSVar' empty, we
-- /could/ check the invariant first and only then swap. We nonetheless swap
-- first and check the invariant after to keep the semantics the same with
-- 'putSVar', otherwise it will be difficult to understand when a 'StrictSVar'
-- is updated and when it is not.
swapSVar :: (MonadSTM m, HasCallStack) => StrictSVar m a -> a -> m a
swapSVar StrictSVar { tmvar, tvar, invariant } !a = do
    oldValue <- atomically $ do
        oldValue <- Lazy.swapTMVar tmvar a
        Lazy.writeTVar tvar a
        return oldValue
    checkInvariant (invariant a) $ return oldValue

isEmptySVar :: MonadSTM m => StrictSVar m a -> m Bool
isEmptySVar StrictSVar { tmvar } = atomically $ Lazy.isEmptyTMVar tmvar

updateSVar :: (MonadSTM m, HasCallStack) => StrictSVar m a -> (a -> (a, b)) -> m b
updateSVar StrictSVar { tmvar, tvar, invariant } f = do
    -- it's not unreasonable to assume that forcing !(!a', b) inside the
    -- atomically block will force the new value before putting it into the
    -- SVar, but although the value in the tuple is forced, there's actually
    -- a thin closure constructed that just points to the forced value which
    -- is what GHC returns in the constructed tuple (so it is actually a thunk,
    -- albeit a trivial one!). in order to ensure that we're forcing the value
    -- inside the SVar before calling checkInvariant, we need an additional
    -- bang outside the atomically block, which will correctly force a' before
    -- checkInvariant looks to see if it's been evaluated or not. without this
    -- change, it's possible to put a lazy value inside a StrictSVar (though
    -- it's unlikely to occur in production environments because this
    -- intermediate unforced closure is optimized away at -O1 and above).
    (!a', b) <- atomically $ do
        a <- Lazy.takeTMVar tmvar
        let !(!a', b) = f a
        Lazy.putTMVar tmvar a'
        Lazy.writeTVar tvar a'
        -- To exactly see what we mean, compile this module with `-ddump-stg-final`
        -- and look for the definition of the closure that is placed as the first
        -- item in the tuple returned here
        return (a', b)
    checkInvariant (invariant a') $ return b

updateSVar_ :: (MonadSTM m, HasCallStack) => StrictSVar m a -> (a -> a) -> m ()
updateSVar_ var f = updateSVar var ((, ()) . f)

modifySVar :: (MonadSTM m, MonadCatch m, HasCallStack)
           => StrictSVar m a -> (a -> m (a, b)) -> m b
modifySVar var action =
    snd . fst <$> generalBracket (takeSVar var) putBack action
  where
    putBack a ec = case ec of
      ExitCaseSuccess (a', _) -> putSVar var a'
      ExitCaseException _ex   -> putSVar var a
      ExitCaseAbort           -> putSVar var a

modifySVar_ :: (MonadSTM m, MonadCatch m, HasCallStack)
            => StrictSVar m a -> (a -> m a) -> m ()
modifySVar_ var action = modifySVar var (fmap (, ()) . action)

{-------------------------------------------------------------------------------
  NoThunks
-------------------------------------------------------------------------------}

instance NoThunks a => NoThunks (StrictSVar IO a) where
  showTypeOf _ = "StrictSVar IO"
  wNoThunks ctxt StrictSVar { tvar } = do
      -- We can't use @atomically $ readTVar ..@ here, as that will lead to a
      -- "Control.Concurrent.STM.atomically was nested" exception.
      a <- readTVarIO tvar
      noThunks ctxt a
