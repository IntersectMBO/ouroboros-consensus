{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

-- | The LeiosTxCache tracks txs that were acquired because a /recent/ EB
-- referenced them.
--
-- The LeiosTxCacheIndex records two facts about each tracked tx: whether it is
-- already acquired and whether it has already been validated (by the Mempool or
-- by the LeiosVoting thread).
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
-- The cache retains the bodies taken in at the 128 freshest body-bearing
-- points. Announcements do not retain cache points. Several points may share
-- one body, and distinct bodies may share TX references. See
-- @docs/fetch-policy/leiosfetch-improved.md#cache-retention@.
--
-- Packed bodies and validation facts are not proof that an individual target
-- point durably holds a body or closure. A verified body
-- can enter this cache before its database write; a mempool transaction can be
-- known valid without having a target location in LeiosDB.
--
-- Fetch settlement publishes target locations only from confirmed writes,
-- subject to retirement fencing. Locations are fill proposals rather than
-- storage pins: GC can make a source unavailable before the target write runs.
-- A missing fill leaves that offset fetchable; only the target write's receipt
-- establishes which transactions the target holds.
--
-- Logical acquisition and its subscribers also filter retirement. Cache hits
-- alone must never announce target-body or closure acquisition. See
-- @docs/fetch-policy/leiosfetch-improved.md#offers-and-local-possession@
-- and @docs/fetch-policy/leiosfetch-improved.md#ownership-and-shutdown@
-- for the ordinary fetch ownership path.
module Ouroboros.Consensus.Leios.TxCache
  ( module Ouroboros.Consensus.Leios.TxCache.API
  , newPureLeiosTxCache
  , newHashTableLeiosTxCache
  , nullLeiosTxCache
  ) where

import qualified Control.Concurrent.Class.MonadMVar as MVar
import qualified Data.Set as Set
import Ouroboros.Consensus.Leios.TxCache.API
import Ouroboros.Consensus.Leios.TxCache.Optimized (newHashTableLeiosTxCache)
import qualified Ouroboros.Consensus.Leios.TxCache.Reference as Pure
import Ouroboros.Consensus.Util.IOLike (IOLike)

-- | A handle backed by the pure reference index behind an MVar.
--
-- The table shift only sizes 'ibsCacheLoad' reporting -- the pure index has no
-- allocation of its own: pass what the production table is allocated with,
-- 'defaultLeiosTxCacheShift', unless comparing against a differently sized
-- 'newHashTableLeiosTxCache'.
newPureLeiosTxCache ::
  (IOLike m, ReferencesTxsByHash b) =>
  Int ->
  m (LeiosTxCache m a v b)
newPureLeiosTxCache nshift = do
  var <- MVar.newMVar Pure.emptyLeiosTxCacheIndex
  pure
    LeiosTxCache
      { evictOlderThan = \boundary ->
          MVar.modifyMVar var $ \idx ->
            let (idx', evEbs, evTxs) = Pure.evictOlderThan boundary idx
             in pure (idx', (evEbs, evTxs))
      , insertBody = \point b nil snoc ->
          MVar.modifyMVar var $ \idx -> pure (Pure.insertBody (2 ^ nshift) point b nil snoc idx)
      , lookupBody = \ebh -> do
          idx <- MVar.readMVar var
          pure $! Pure.lookupBody ebh idx
      , withLockedInsertUnappliedTx = \k ->
          MVar.modifyMVar var $ \idx ->
            k
              (idx, mempty)
              ( \(!idx', !fab) txh sz a ->
                  let (idx'', prior) = Pure.insertUnappliedTx txh a idx'
                      fab' = fab <> bucketTxArrival prior sz
                   in idx'' `seq` fab' `seq` pure (idx'', fab')
              )
      , withLockedInsertAppliedTx = \k ->
          MVar.modifyMVar_ var $ \idx ->
            k idx (\idx' txh v -> pure $! Pure.insertAppliedTx txh v idx')
      , withLookupTx = \k -> do
          idx <- MVar.readMVar var
          k $ \txh -> pure $! Pure.lookupTx txh idx
      , setTxLocations = \point offTxs ->
          MVar.modifyMVar_ var $ \idx -> pure $! Pure.setTxLocations point offTxs idx
      }

-- | A handle whose every operation is inert: bodies are never retained,
-- summarised nor their txs folded over, tx insertions do nothing, and
-- lookups always miss. For
-- contexts that don't maintain the cache, such as tools that replay or forge
-- blocks.
nullLeiosTxCache :: Applicative m => LeiosTxCache m a v b
nullLeiosTxCache =
  LeiosTxCache
    { evictOlderThan = \_boundary -> pure (Set.empty, Set.empty)
    , insertBody = \_point _b nil _snoc -> pure (Nothing, nil)
    , lookupBody = \_ebh -> pure Nothing
    , withLockedInsertUnappliedTx = \k -> k mempty (\fab _txh sz _a -> pure (fab <> bucketTxArrival TxWasUntracked sz))
    , withLockedInsertAppliedTx = \k -> k () (\w _txh _v -> pure w)
    , withLookupTx = \k -> k (\_txh -> pure Nothing)
    , setTxLocations = \_point _offTxs -> pure ()
    }
