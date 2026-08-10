{-# LANGUAGE Rank2Types #-}

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
-- This module is the umbrella: it re-exports the "LeiosTxCache.API" interface
-- and both handle factories, 'newPureLeiosTxCache' from
-- "LeiosTxCache.Reference" and 'newHashTableLeiosTxCache' from
-- "LeiosTxCache.Optimized".
--
-- == Invariants
--
-- (TODO This is written as if LeiosNotify writes announcements to the LeiosDb,
-- but it doesn't already... and I'm not sure it will?)
--
-- - INVARIANT: an EB announcement in the LeiosTxCacheIndex is in the LeiosDb
--
-- - INVARIANT: 'LeiosTxCache.API.BodyAlreadyInserted' EbBody is in the LeiosDb
--   /and pinned/
--
-- - INVARIANT: a 'Pure.TxAlreadyInserted' tx is in the LeiosDb /and pinned/
--
-- These invariants are maintained as follows.
--
-- - LeiosNotify\/LeiosFetch\/the block forge inserts an EB
--   announcement\/body\/tx into the LeiosDb /before/ it inserts into the
--   LeiosTxCacheIndex.
--
-- - An EB announcement\/body is inserted into the LeiosDb\/LeiosTxCacheIndex
--   before its body\/txs are inserted (unless it's shared with an earlier
--   announcement\/body).
--
-- - Within the LeiosDb, announcements\/bodies pin the bodies\/txs they refer
--   to.
--
-- - The ChainDB evicts "too old" announcements from the LeiosTxCacheIndex
--   /before/ it prunes them from the LeiosDb.
--
-- That ensures an object's lifetime inside the LeiosTxCacheIndex is contained
-- within that object's lifetime within the LeiosDb.
--
-- LeiosFetch checks the LeiosTxCacheIndex to see if it needs to fetch an EB
-- body\/closure promised by some EB announcement\/body. If the
-- LeiosTxCacheIndex reports
-- 'LeiosTxCache.API.BodyAlreadyInserted'\/'Pure.TxAlreadyInserted', then
-- LeiosFetch won't fetch it. Therefore, it's crucial that the body\/tx is
-- actually in the LeiosDb.
--
-- It is still possible to see a LeiosTxCache hit when processing some
-- announcement\/body and then later fail to read it from the LeiosDb. But
-- because of the LeiosDb pinning, the only way a LeiosTxCacheIndex hit could
-- precede a failed LeiosDb read is if /the announcement itself has also been
-- pruned/ from the LeiosDb, in which case whatever logic issued the failed read
-- can soundly short-circuit: it was attempting to process an orphaned EB.
module LeiosTxCache
  ( module LeiosTxCache.API
  , newPureLeiosTxCache
  , newHashTableLeiosTxCache
  , nullLeiosTxCache
  ) where

import qualified Control.Concurrent.Class.MonadMVar as MVar
import qualified Data.Set as Set
import LeiosTxCache.API
import LeiosTxCache.Optimized (newHashTableLeiosTxCache)
import qualified LeiosTxCache.Reference as Pure
import Ouroboros.Consensus.Util.IOLike (IOLike)

-- | A handle backed by the pure reference index behind an MVar.
newPureLeiosTxCache ::
  (IOLike m, ReferencesTxsByHash b) =>
  m (LeiosTxCache m a v b)
newPureLeiosTxCache = do
  var <- MVar.newMVar Pure.emptyLeiosTxCacheIndex
  pure
    LeiosTxCache
      { insertAnnouncement = \slot rbh ebh ->
          MVar.modifyMVar var $ \idx ->
            let (idx', evEbs, evTxs) = Pure.insertAnnouncement slot rbh ebh idx
             in pure (idx', (evEbs, evTxs))
      , evictOlderThan = \boundary ->
          MVar.modifyMVar var $ \idx ->
            let (idx', evEbs, evTxs) = Pure.evictOlderThan boundary idx
             in pure (idx', (evEbs, evTxs))
      , insertBody = \ebh b nil snoc ->
          MVar.modifyMVar var $ \idx -> pure (Pure.insertBody ebh b nil snoc idx)
      , lookupBody = \ebh -> do
          idx <- MVar.readMVar var
          pure $! Pure.lookupBody ebh idx
      , withLockedInsertUnappliedTx = \k ->
          MVar.modifyMVar_ var $ \idx ->
            k idx (\idx' txh a -> pure $! Pure.insertUnappliedTx txh a idx')
      , withLockedInsertAppliedTx = \k ->
          MVar.modifyMVar_ var $ \idx ->
            k idx (\idx' txh v -> pure $! Pure.insertAppliedTx txh v idx')
      , withLookupTx = \k -> do
          idx <- MVar.readMVar var
          k $ \txh -> pure $! Pure.lookupTx txh idx
      }

-- | A handle whose every operation is inert: announcements evict nothing, bodies
-- are never summarised, tx insertions do nothing, and lookups always miss. For
-- forge\/replay contexts that don't maintain the cache (e.g.
-- "Cardano.Tools.DBSynthesizer").
nullLeiosTxCache :: Applicative m => LeiosTxCache m a v b
nullLeiosTxCache =
  LeiosTxCache
    { insertAnnouncement = \_slot _rbh _ebh -> pure (Set.empty, Set.empty)
    , evictOlderThan = \_boundary -> pure (Set.empty, Set.empty)
    , insertBody = \_ebh _b _nil _snoc -> pure Nothing
    , lookupBody = \_ebh -> pure Nothing
    , withLockedInsertUnappliedTx = \k -> k () (\w _txh _a -> pure w)
    , withLockedInsertAppliedTx = \k -> k () (\w _txh _v -> pure w)
    , withLookupTx = \k -> k (\_txh -> pure Nothing)
    }
