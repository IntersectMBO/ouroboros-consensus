{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ouroboros.Consensus.Util.MonadMVar.NormalForm (
    -- * StrictMVars with NoThunks invariants
    newEmptyMVar
  , newMVar
    -- * Re-exports
  , module Control.Concurrent.Class.MonadMVar.Strict.Checked
  ) where

import           Control.Concurrent.Class.MonadMVar.Strict.Checked hiding
                     (newEmptyMVar, newEmptyMVarWithInvariant, newMVar,
                     newMVarWithInvariant)
import qualified Control.Concurrent.Class.MonadMVar.Strict.Checked as Checked
import           NoThunks.Class (NoThunks (..), unsafeNoThunks)

noThunksInvariant :: NoThunks a => a -> Maybe String
noThunksInvariant = fmap show . unsafeNoThunks

trivialInvariant :: NoThunks a => a -> Maybe String
trivialInvariant = const Nothing

newMVar :: (MonadMVar m, NoThunks a) => a -> m (StrictMVar m a)
newEmptyMVar :: (MonadMVar m, NoThunks a) => m (StrictMVar m a)

#if CHECK_MVAR_INVARIANTS
newMVar      = Checked.newMVarWithInvariant noThunksInvariant
newEmptyMVar = Checked.newEmptyMVarWithInvariant noThunksInvariant
#else
newMVar      = Checked.newMVarWithInvariant trivialInvariant
newEmptyMVar = Checked.newEmptyMVarWithInvariant trivialInvariant
#endif

instance NoThunks a => NoThunks (StrictMVar IO a) where
  showTypeOf _ = "StrictMVar IO"
  wNoThunks ctxt mvar = do
      a <- readMVar mvar
      noThunks ctxt a
