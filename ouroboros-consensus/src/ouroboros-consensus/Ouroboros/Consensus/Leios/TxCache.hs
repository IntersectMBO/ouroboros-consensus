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
-- "Ouroboros.Consensus.Leios.TxCache.API" interface and the handle factory
-- 'newPureLeiosTxCache', which wraps
-- "Ouroboros.Consensus.Leios.TxCache.Reference".
--
-- == What it promises about the LeiosDb
--
-- Nothing: its records are hints. A location it records for a tx need not hold
-- that tx in the LeiosDb. Once a point is older than the LeiosDb's promoted
-- slot, the LeiosDb's garbage collection deletes the point's txs, and a later
-- write of txs to that point stores none of them, though it settles as usual.
-- The cache evicts that point when LeiosFetch prunes to the immutable tip
-- ('evictOlderThanFoldl''), which can come after either.
--
-- Offering a recorded location to the LeiosDb as a fill is safe anyway: the
-- LeiosDb copies a fill only if its bytes hash to the tx the body lists at that
-- offset (see 'Ouroboros.Consensus.Storage.LeiosDB.API.writeEbBody'), so a
-- stale or wrong location costs a fetch, never a wrong tx.
module Ouroboros.Consensus.Leios.TxCache
  ( module Ouroboros.Consensus.Leios.TxCache.API
  , newPureLeiosTxCache
  , nullLeiosTxCache
  ) where

import qualified Data.Foldable as Foldable
import Data.Maybe.Strict (StrictMaybe (SNothing))
import qualified Data.Set as Set
import Ouroboros.Consensus.Leios.TxCache.API
import qualified Ouroboros.Consensus.Leios.TxCache.Reference as Pure
import Ouroboros.Consensus.Util.IOLike (IOLike, evaluate)
import qualified Ouroboros.Consensus.Util.NormalForm.StrictMVar as MVar

-- | A handle backed by the pure reference index behind an MVar.
--
-- The table shift only sizes 'ibsCacheCapacity' reporting -- the pure index has
-- no allocation of its own: pass 'defaultLeiosTxCacheShift'.
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
    , insertLandedTxs = \_point landed ->
        pure (foldMap (bucketTxArrival TxEvicted . ltSize) landed)
    , withLookupTx = \k -> k (\_txh -> pure SNothing)
    }
