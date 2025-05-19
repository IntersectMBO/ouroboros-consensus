module Ouroboros.Consensus.Util.MonadSTM.NormalForm
  ( module LazySTM
  , module Ouroboros.Consensus.Util.MonadSTM.StrictSVar
  , module StrictSTM
  , newEmptySVar
  , newSVar

    -- * Temporary
  , uncheckedNewEmptySVar
  , uncheckedNewSVar
  ) where

import Control.Concurrent.Class.MonadSTM.Strict.TMVar as StrictSTM hiding
  ( newTMVar
  , newTMVarIO
  , traceTMVar
  , traceTMVarIO
  )
import Control.Concurrent.Class.MonadSTM.TBQueue as LazySTM
import Control.Concurrent.Class.MonadSTM.TQueue as LazySTM
import Control.Monad.Class.MonadSTM as StrictSTM hiding
  ( traceTVar
  , traceTVarIO
  )
import GHC.Stack
import NoThunks.Class (NoThunks (..), unsafeNoThunks)
import Ouroboros.Consensus.Util.MonadSTM.StrictSVar hiding
  ( newEmptySVar
  , newEmptySVarWithInvariant
  , newSVar
  , newSVarWithInvariant
  )
import qualified Ouroboros.Consensus.Util.MonadSTM.StrictSVar as Strict

-- TODO: use strict versions of 'TQueue' and 'TBQueue'.  Previously the
-- 'Control.Monad.Class.MonadSTM.Strict' was imported which
-- exported lazy 'TQueue' and 'TBQueue',  I (@coot) think that the intention was
-- to use strict versions.

{-------------------------------------------------------------------------------
  Wrappers that check for thunks
-------------------------------------------------------------------------------}

newSVar ::
  (MonadSTM m, HasCallStack, NoThunks a) =>
  a -> m (StrictSVar m a)
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
