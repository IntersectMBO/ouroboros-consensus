module Control.Concurrent.Class.MonadSTM.NormalForm.SVar (
    module Control.Concurrent.Class.MonadSTM.Strict.SVar
  , newEmptySVar
  , newSVar
    -- * Temporary
  , uncheckedNewEmptySVar
  , uncheckedNewSVar
  ) where

import           Control.Concurrent.Class.MonadSTM.Strict.SVar hiding
                     (newEmptySVar, newEmptySVarWithInvariant, newSVar,
                     newSVarWithInvariant)
import qualified Control.Concurrent.Class.MonadSTM.Strict.SVar as Strict
import           Control.Monad.Class.MonadSTM as StrictSTM hiding (traceTVar,
                     traceTVarIO)
import           GHC.Stack
import           NoThunks.Class (NoThunks (..), unsafeNoThunks)

-- TODO: use strict versions of 'TQueue' and 'TBQueue'.  Previously the
-- 'Control.Monad.Class.MonadSTM.Strict' was imported which
-- exported lazy 'TQueue' and 'TBQueue',  I (@coot) think that the intention was
-- to use strict versions.

{-------------------------------------------------------------------------------
  Wrappers that check for thunks
-------------------------------------------------------------------------------}

newSVar :: (MonadSTM m, HasCallStack, NoThunks a)
        => a -> m (StrictSVar m a)
newSVar = Strict.newSVarWithInvariant (fmap show . unsafeNoThunks)

newEmptySVar :: (MonadSTM m, NoThunks a) => a -> m (StrictSVar m a)
newEmptySVar = Strict.newEmptySVarWithInvariant (fmap show . unsafeNoThunks)

{-------------------------------------------------------------------------------
  Unchecked wrappers (where we don't check for thunks)

  These will eventually be removed.
-------------------------------------------------------------------------------}

uncheckedNewSVar :: MonadSTM m => a -> m (StrictSVar m a)
uncheckedNewSVar = Strict.newSVar

uncheckedNewEmptySVar :: MonadSTM m => a -> m (StrictSVar m a)
uncheckedNewEmptySVar = Strict.newEmptySVar
