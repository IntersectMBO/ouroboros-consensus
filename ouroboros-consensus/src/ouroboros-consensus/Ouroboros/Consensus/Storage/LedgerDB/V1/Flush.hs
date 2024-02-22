module Ouroboros.Consensus.Storage.LedgerDB.V1.Flush (
    flushIntoBackingStore
  , flushLedgerDB
  ) where

import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore
import           Ouroboros.Consensus.Storage.LedgerDB.V1.DbChangelog
import           Ouroboros.Consensus.Util.IOLike

flushLedgerDB :: (MonadSTM m, GetTip l, HasLedgerTables l)
              => StrictTVar m (DbChangelog l)
              -> LedgerBackingStore m l
              -> m ()
flushLedgerDB chlogVar bstore = do
  diffs <- atomically $ do
    ldb' <- readTVar chlogVar
    let (toFlush, toKeep) = splitForFlushing ldb'
    case toFlush of
      Nothing -> pure ()
      Just {} -> writeTVar chlogVar toKeep
    pure toFlush
  mapM_ (flushIntoBackingStore bstore) diffs

-- | Flush **all the changes in this DbChangelog** into the backing store
--
-- Note that 'flush' must have been called to split the 'DbChangelog' on the
-- immutable tip and produce two 'DbChangelog's, one to flush and one to keep.
--
-- The write lock must be held before calling this function.
flushIntoBackingStore :: LedgerBackingStore m l -> DiffsToFlush l -> m ()
flushIntoBackingStore backingStore dblog =
  bsWrite
    backingStore
    (toFlushSlot dblog)
    (toFlushDiffs dblog)
