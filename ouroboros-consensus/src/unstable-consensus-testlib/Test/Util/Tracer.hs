{-# LANGUAGE ScopedTypeVariables #-}
module Test.Util.Tracer (
    recordingTracerIORef
  , recordingTracerM
  , recordingTracerTVar
  ) where

import           Control.Tracer
import           Data.IORef
import           Ouroboros.Consensus.Util.IOLike
import           System.IO.Unsafe (unsafePerformIO)

-- | Create a 'Tracer' that stores all events in an 'IORef' that is atomically
-- updated. The second return value lets you obtain the events recorded so far
-- (from oldest to newest). Obtaining the events does not erase them.
recordingTracerIORef :: IO (Tracer IO ev, IO [ev])
recordingTracerIORef = newIORef [] >>= \ref -> return
    ( Tracer $ \ev -> atomicModifyIORef' ref $ \evs -> (ev:evs, ())
    , reverse <$> readIORef ref
    )

-- | Create a 'Tracer' that stores all events in a 'TVar' that is atomically
-- updated. The second return value lets you obtain the events recorded so far
-- (from oldest to newest). Obtaining the events does not erase them.
recordingTracerTVar :: MonadSTM m => m (Tracer m ev, m [ev])
recordingTracerTVar = uncheckedNewTVarM [] >>= \ref -> return
    ( Tracer $ \ev -> atomically $ modifyTVar ref (ev:)
    , atomically $ reverse <$> readTVar ref
    )

-- | Like 'recordingTracerIORef', but lifts IO to an arbitrary applicative.
-- This is useful to record events without changing the scheduling during a
-- test.
recordingTracerM :: forall m ev. Monad m => m (Tracer m ev, m [ev])
recordingTracerM = do
  (tr, get) <- liftIOtoM recordingTracerIORef
  pure (natTracer liftIOtoM tr, liftIOtoM get)
  where
    liftIOtoM :: IO a -> m a
    liftIOtoM m = do
      -- The fictitious state is only used to force @unsafePerformIO@ to run @m@
      -- every time @liftIOtoM m@ is evaluated.
      s <- getStateM
      case unsafePerformIO $ (,) s <$> m of
        (_, r) -> pure r

    -- We mark this function as NOINLINE to ensure the compiler cannot reason
    -- that two calls of @getStateM@ might yield the same value.
    {-# NOINLINE getStateM #-}
    getStateM = pure True
