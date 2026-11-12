-- | Order cache retirement before LeiosDB promotion.
module Ouroboros.Consensus.Storage.LeiosDB.Cache
  ( withCacheRetirement
  ) where

import Ouroboros.Consensus.Leios.TxCache.API (LeiosTxCache, evictOlderThan)
import Ouroboros.Consensus.Storage.LeiosDB.API (LeiosDbHandle (..))

-- | Install before handing this handle to any promotion producer, independently
-- of the node's immutable-tip worker. Advance the cache's monotonic publication
-- frontier before promotion changes authoritative reads, releasing the cache
-- lock before storage IO. Late receipts cannot replace a newer shared-TX
-- location with a retired one. Cache pruning deletes no stored bytes; the
-- writer still verifies sources named by previously captured fill descriptors.
-- See @docs/fetch-policy/leiosfetch-improved.md#durable-confirmation-and-garbage-collection@.
withCacheRetirement :: Monad m => LeiosTxCache m a v b -> LeiosDbHandle m -> LeiosDbHandle m
withCacheRetirement cache db =
  db
    { leiosDbPromoteToImmutable = \slot points ->
        evictOlderThan cache slot >> leiosDbPromoteToImmutable db slot points
    }
