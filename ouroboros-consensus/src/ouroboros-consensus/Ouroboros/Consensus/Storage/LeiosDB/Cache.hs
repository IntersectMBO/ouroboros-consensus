-- | Order cache retirement before physical LeiosDB collection.
module Ouroboros.Consensus.Storage.LeiosDB.Cache
  ( withCacheGarbageCollection
  ) where

import Ouroboros.Consensus.Leios.TxCache.API (LeiosTxCache (..))
import Ouroboros.Consensus.Storage.LeiosDB.API (LeiosDbHandle (..))

-- | Install before handing this handle to any GC producer. Retirement advances
-- the cache's monotonic publication frontier before rows can disappear. Late
-- receipts therefore cannot replace a newer shared-TX location with a retired
-- one. Cache locations are hints; existing guarded local fills still verify
-- the source when the writer executes them.
-- See @docs/fetch-policy/leiosfetch-improved.md#durable-confirmation-and-garbage-collection@.
withCacheGarbageCollection :: Monad m => LeiosTxCache m a v b -> LeiosDbHandle m -> LeiosDbHandle m
withCacheGarbageCollection cache db =
  db
    { garbageCollect = \slot -> evictOlderThan cache slot >> garbageCollect db slot
    }
