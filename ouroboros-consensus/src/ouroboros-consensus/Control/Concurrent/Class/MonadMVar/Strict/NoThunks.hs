{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Concurrent.Class.MonadMVar.Strict.NoThunks (
    -- * StrictMVars with NoThunks invariants
    newEmptyMVar
  , newEmptyMVarWithInvariant
  , newMVar
  , newMVarWithInvariant
    -- * Re-exports
  , module StrictMVar
  ) where

#if CHECK_MVAR_INVARIANTS
import qualified Control.Concurrent.Class.MonadMVar.Strict.Checked as StrictMVar
import           Control.Concurrent.Class.MonadMVar.Strict.Checked as StrictMVar hiding (newMVar, newMVarWithInvariant, newEmptyMVarWithInvariant, newEmptyMVar)
#else
import qualified Control.Concurrent.Class.MonadMVar.Strict as StrictMVar
import           Control.Concurrent.Class.MonadMVar.Strict as StrictMVar hiding (newMVar, newMVarWithInvariant, newEmptyMVarWithInvariant, newEmptyMVar)
#endif

import           Control.Applicative ((<|>))
import           NoThunks.Class (NoThunks (..), unsafeNoThunks)

noThunksInvariant :: NoThunks a => a -> Maybe String
noThunksInvariant = fmap show . unsafeNoThunks

newMVar :: (MonadMVar m, NoThunks a) => a -> m (StrictMVar m a)
newMVar = StrictMVar.newMVarWithInvariant noThunksInvariant

newMVarWithInvariant ::
     (MonadMVar m, NoThunks a)
  => (a -> Maybe String)
  -> a
  -> m (StrictMVar m a)
newMVarWithInvariant inv =
    StrictMVar.newMVarWithInvariant (\x -> inv x <|> noThunksInvariant x)

newEmptyMVar :: (MonadMVar m, NoThunks a) => m (StrictMVar m a)
newEmptyMVar = StrictMVar.newEmptyMVarWithInvariant noThunksInvariant

newEmptyMVarWithInvariant ::
     (MonadMVar m, NoThunks a)
  => (a -> Maybe String)
  -> m (StrictMVar m a)
newEmptyMVarWithInvariant inv =
    StrictMVar.newEmptyMVarWithInvariant (\x -> inv x <|> noThunksInvariant x)

instance NoThunks a => NoThunks (StrictMVar IO a) where
  showTypeOf _ = "StrictMVar IO"
  wNoThunks ctxt mvar = do
      a <- StrictMVar.readMVar mvar
      noThunks ctxt a
