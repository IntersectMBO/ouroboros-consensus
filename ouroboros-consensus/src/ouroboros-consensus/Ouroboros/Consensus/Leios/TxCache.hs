{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

-- | The LeiosTxCache tracks txs that were acquired because a /recent/ EB
-- referenced them.
--
-- The LeiosTxCacheIndex records two facts about each tx it holds: whether it has
-- already been validated (by the Mempool or by the LeiosVoting thread), and
-- where a LeiosDB write of it landed.
--
-- The index is in-memory so that latency-critical consumers (LeiosFetch,
-- LeiosVoting) can query it with constantly low latency; it /supplants/ the
-- by-hash membership check that the fetch logic would otherwise do against the
-- LeiosDb. The size of the LeiosTxCache is bounded first and foremost by the
-- requirement that its index fits comfortably in-memory, even in a worst case.
--
-- This module is the umbrella: it re-exports the
-- "Ouroboros.Consensus.Leios.TxCache.API" interface and the handle factories:
-- 'newPureLeiosTxCache' from "Ouroboros.Consensus.Leios.TxCache.Reference" and
-- 'newHashTableLeiosTxCache' from "Ouroboros.Consensus.Leios.TxCache.Optimized".
--
-- == Availability, validation and durability
--
-- The cache retains reference metadata for the 128 freshest body-bearing
-- points and indexes only transactions it holds. Announcements do not retain
-- points; several points and bodies may reference the same transaction. See
-- @docs/fetch-policy/leiosfetch-improved.md#cache-retention@.
--
-- Reference metadata does not establish target-body or closure possession.
-- Fetch publishes held/validated state and target locations only for positively
-- confirmed offsets. Validation facts captured from the mempool remain local
-- preparation facts until the corresponding write is confirmed.
--
-- Locations are fill proposals, not storage pins: GC can remove a source before
-- its target write. The database checks a fill against the target reference,
-- so a stale or wrong location costs a fetch, never a wrong transaction. Only
-- the target receipt establishes which offsets the target actually holds.
-- Cache eviction makes publication for that point a no-op without invalidating
-- an otherwise live positive receipt or redirecting it through another point.
--
-- Logical acquisition and its subscribers independently filter retirement.
-- See @docs/fetch-policy/leiosfetch-improved.md#offers-and-local-possession@
-- and @docs/fetch-policy/leiosfetch-improved.md#ownership-and-shutdown@.
module Ouroboros.Consensus.Leios.TxCache
  ( module Ouroboros.Consensus.Leios.TxCache.API
  , newPureLeiosTxCache
  , newHashTableLeiosTxCache
  , nullLeiosTxCache
  ) where

import qualified Data.Foldable as Foldable
import Data.Maybe.Strict (StrictMaybe (SNothing))
import qualified Data.Set as Set
import Ouroboros.Consensus.Leios.TxCache.API
import Ouroboros.Consensus.Leios.TxCache.Optimized (newHashTableLeiosTxCache)
import qualified Ouroboros.Consensus.Leios.TxCache.Reference as Pure
import Ouroboros.Consensus.Util.IOLike (IOLike, evaluate)
import qualified Ouroboros.Consensus.Util.NormalForm.StrictMVar as MVar

-- | A handle backed by the pure reference index behind an MVar.
--
-- The table shift only sizes 'ibsCacheCapacity' reporting -- the pure index has
-- no allocation of its own: pass what the production table is allocated with,
-- 'defaultLeiosTxCacheShift', unless comparing against a differently sized
-- 'newHashTableLeiosTxCache'.
newPureLeiosTxCache ::
  (IOLike m, ReferencesTxsByHash b) =>
  Int ->
  m (LeiosTxCache m a v b)
newPureLeiosTxCache nshift = do
  var <- MVar.uncheckedNewMVar Pure.emptyLeiosTxCacheIndex
  pure
    LeiosTxCache
      { evictOlderThanFoldl' = \boundary nil snoc ->
          MVar.modifyMVar var $ \idx -> do
            let (idx', evPoints, w) = Pure.evictOlderThanFoldl' boundary nil snoc idx
            -- Computed now, under the lock, as for 'insertBodyAndFoldl''.
            _ <- evaluate evPoints
            _ <- evaluate w
            pure (idx', (evPoints, w))
      , insertBodyAndFoldl' = \point b nil snoc ->
          MVar.modifyMVar var $ \idx -> do
            let (idx', (summary, w)) = Pure.insertBodyAndFoldl' (2 ^ nshift) point b nil snoc idx
            -- Computed now, under the lock, rather than when the caller first
            -- reads them, which would retain this snapshot of the index until
            -- then.
            mapM_ evaluate summary
            _ <- evaluate w
            pure (idx', (summary, w))
      , classifyTxs = \point txs ->
          MVar.withMVar var $ \idx -> evaluate $ Pure.classifyTxs point txs idx
      , insertLandedTxs = \point landed ->
          MVar.modifyMVar var $ \idx -> do
            let step (!idx', !fab) (MkLandedTx off txh sz st) =
                  let (idx'', prior) = Pure.insertLandedTx point off txh st idx'
                   in (idx'', fab <> bucketTxArrival prior sz)
                (landedIdx, arrival) = Foldable.foldl' step (idx, mempty) landed
            _ <- evaluate arrival
            pure (landedIdx, arrival)
      , withLookupTx = \k ->
          -- Holds the lock, as 'withLookupTx' promises, though reading a
          -- snapshot of the index would do.
          MVar.withMVar var $ \idx -> k $ \txh -> pure $! Pure.lookupTx txh idx
      }

-- | A handle whose every operation is inert: bodies are never retained,
-- summarised nor their txs folded over, landed txs are counted evicted, and
-- lookups always miss. For
-- contexts that don't maintain the cache, such as tools that replay or forge
-- blocks.
nullLeiosTxCache :: Monad m => LeiosTxCache m a v b
nullLeiosTxCache =
  LeiosTxCache
    { evictOlderThanFoldl' = \_boundary nil _snoc -> pure (Set.empty, nil)
    , insertBodyAndFoldl' = \_point _b nil _snoc -> pure (Nothing, nil)
    , classifyTxs = \_point txs ->
        pure $! Foldable.foldl' (\acc (_txh, size) -> acc <> bucketTxArrival TxEvicted size) mempty txs
    , insertLandedTxs = \_point landed ->
        pure (foldMap (bucketTxArrival TxEvicted . ltSize) landed)
    , withLookupTx = \k -> k (\_txh -> pure SNothing)
    }
