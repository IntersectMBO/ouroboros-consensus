-- | Memory-only, bounded index of durable transaction locations. The cache
-- never retains transaction payloads or proves target acquisition. Consumers
-- reserve local handoff capacity and await guarded database-copy receipts.
-- Rationale: @docs/fetch-policy/cancellation-contract.md#deduplication-and-durable-acquisition@.
module Ouroboros.Consensus.Leios.TxCache
  ( module Ouroboros.Consensus.Leios.TxCache.API
  , newOptimizedCache
  , newReferenceCache
  ) where

import Ouroboros.Consensus.Leios.TxCache.API
import Ouroboros.Consensus.Leios.TxCache.Optimized (newOptimizedCache)
import Ouroboros.Consensus.Leios.TxCache.Reference (newReferenceCache)
