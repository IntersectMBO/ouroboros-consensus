{-# LANGUAGE Rank2Types #-}

-- | The LeiosTxCache tracks txs that were acquired because a /recent/ EB
-- referenced them.
--
-- The LeiosTxCacheIndex records two facts about each tx the LeiosTxCache is
-- tracking: whether the tx is already acquired and whether it has already been
-- validated (either by the Mempool or by the LeiosVoting thread).
--
-- The index is in-memory so that other components (LeiosFetch and LeiosVoting)
-- can query it with constantly low latency. The size of the LeiosTxCache is
-- bounded first and foremost by the requirement that its index fits comfortably
-- in-memory, even in a worst-case.
--
-- Beyond its index, the LeiosTxCache also "contains" the bytes of the txs it
-- claims were already acquired. In the currently implementation these bytes are
-- sure to be present in the LeiosDb: they're written there before the
-- LeiosTxCacheIndex is updated, and the LeiosDb's eviction is certainly later,
-- since the LeiosTxCache holds at most much less than k blocks, and the LeiosDb
-- only evicts data that is unreachable from the immutable tip.
--
-- In the future, we may prefer for the LeiosTxCache to also own the bytes of
-- the txs it contains, in order to decouple it from the LeiosDb (which might
-- allow improvements to the LeiosDb's other responsibilites, eg, its GC
-- times). For considerations of how the LeiosTxCache should manage the tx bytes
-- itself, see $backingStore.
--
-- This module is the umbrella: it re-exports the interface ("LeiosTxCache.API")
-- and both handle factories ('newPureLeiosTxCache' from "LeiosTxCache.Reference"
-- and 'newHashTableLeiosTxCache' from "LeiosTxCache.Optimized").
module LeiosTxCache
  ( module LeiosTxCache.API
  , newPureLeiosTxCache
  , newHashTableLeiosTxCache
  , nullLeiosTxCache

    -- $backingStore
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
      , insertBody = \ebh b ->
          MVar.modifyMVar var $ \idx -> pure (Pure.insertBody ebh b idx)
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
    , insertBody = \_ebh _b -> pure Nothing
    , withLockedInsertUnappliedTx = \k -> k () (\w _txh _a -> pure w)
    , withLockedInsertAppliedTx = \k -> k () (\w _txh _v -> pure w)
    , withLookupTx = \k -> k (\_txh -> pure Nothing)
    }

-- $backingStore
--
-- = A dedicated backing store for the LeiosTxCache's tx bytes
--
-- Note on scope: everything here concerns how a LeiosTxCache handle stores (or
-- declines to store) the tx bytes, which is independent of how the index is
-- implemented — hence it lives at the interface level (the handle type is in
-- "LeiosTxCache.API") rather than beside either implementation
-- ("LeiosTxCache.Reference" or "LeiosTxCache.Optimized"). Some details below are
-- nonetheless phrased in terms of the hash-table implementation, since that is the
-- one intended for production use.
--
-- == What we store today
--
-- The MutableHashTable above indexes txs by hash; the node instantiates it as a
-- presence/refcount index only (the value is a refcount plus a 2-bit state tag).
-- The tx /bytes/ are not stored here — they already live in the LeiosDb's txs
-- table. So with today's LeiosDb the LeiosTxCache is a "for free" in-memory index
-- over that on-disk table: a hit says the LeiosDb has the tx (and gives its cache
-- state); a miss means the LeiosDb doesn't /necessarily/ contain it. Because the
-- LeiosTxCache is tuned to be "big enough", paying the re-fetch/re-validation
-- costs for such misses is acceptable.
--
-- == Why a separate store later
--
-- Eventually we may want the LeiosTxCache to own its tx bytes in a dedicated
-- backing store rather than piggy-backing on the LeiosDb, so the two are not
-- coupled: the LeiosDb keeps its own eviction policy, schema, and durability
-- guarantees without the cache imposing synchronization or extra constraints, and
-- the cache can be tuned purely as a bounded, lossy accelerator. The rest of this
-- note sketches such a store.
--
-- == The bounds
--
-- Two Leios rules bound a single EB, and one policy bounds the window:
--
--   * An EB body is a list of tx hashes, capped at ~512 kB on the wire. Each entry
--     is a 32-byte hash plus the tx's size (~34 B total), so an EB
--     references at most ~512000 / 34 ~= 15058 txs. (The CBOR-exact bound,
--     LeiosDemoTypes.maxTxsPerEb, is 13888; 15058 is the encoding-independent
--     ceiling we size against.)
--
--   * An EB's cumulative referenced-tx bytes are separately capped at 12 MB.
--
--   * At most 128 EBs are retained in the cache at once. (The derivation of 128
--     is out of scope here.)
--
-- Therefore the cache holds at most:
--
-- > 128 * 15058             = 1,927,424 ~2M txs
-- > 128 *  12 MB = 1,536 MB = 1.536 GB      bytes of tx
--
-- Individual txs are ~50 B .. 16384 B. An adversary controls tx sizes, which
-- txs appear in which EBs, and how txs are shared across EBs.
--
-- == Option A: ~1.5 GB in RAM
--
-- If ~1.5 GB of RAM is affordable, keep the bytes resident. But an in-RAM store of
-- adversary-chosen variable-size, variable-lifetime blobs must resist an adversary
-- who maximizes fragmentation and per-operation latency. That calls for a
-- sophisticated allocator: a manually-managed, handle-based, compacting,
-- segregated-fits allocator with bitmapped slabs and O(1) incremental evacuation,
-- to strictly bound worst-case fragmentation while keeping latency ~constant.
-- Correct, but intricate.
--
-- == Option B: 2x ~1.5 GB on disk (preferred, for simplicity)
--
-- Two ~1.5 GB spaces on disk (~3 GB) is definitely affordable — disk is the cheap
-- axis. And with a full spare space, "defragment" degenerates to "copy the live
-- set into the empty side," so fragmentation is structurally zero and the total
-- footprint is ≤ 2x the live set no matter what the adversary does — no clever
-- allocator needed. The EBs' FIFO lifetime and bounded per-EB size then make
-- bounded-work-per-EB easy to argue. This is a plain two-space (semi-space)
-- copying collector.
--
-- The index must be in-memory, so LeiosFetch, LeiosVote, etc can
-- make low-latency decisions. But the bytes of the cached txs can be slower to
-- access on-disk---they'll still (generaly) be faster than network's fetching.
--
-- == No durability, and no VM buffering
--
-- The cache is losable: on an ungraceful termination (or even a graceful one) it
-- may vanish, at the cost of misses on everything it held. A miss is never a
-- correctness problem, but nor is it always a cheap lookup elsewhere: some
-- consumers can't afford to check whether the tx is really absent, so they assume
-- the worst and re-fetch and/or re-validate. Losing the cache therefore just costs
-- those bounded penalties until ongoing Leios traffic re-populates it. Hence there
-- is no durability layer at all: no fsync, WAL, journaling, recovery, torn-write
-- handling, or crash checksums.
--
-- Nor do we need the OS page cache to buffer it: the access pattern is fully
-- predictable (bump-allocate, FIFO-drain, copy-on-promote), so the page cache adds
-- nothing but would pull ~3 GB into RAM and pressure the VM. Hence lean toward
-- @O_DIRECT@ I/O — bypass the page cache, keep the spaces on disk, and pay only
-- explicit, bounded transfers with no paging pressure. The in-RAM footprint is
-- then just the hash-table index plus small I/O buffers.
--
-- == Two-space details
--
--   * Two spaces, each sized for the max live set (1.536 GB); ~3 GB total, ≤ 2x
--     overhead. At any time one space is "active" (being filled) and the other is
--     "draining".
--
--   * New tx bytes are bump-allocated into the active space.
--
--   * The hash-table value gains the tx's location alongside its refcount + state
--     tag: a 1-bit space tag, a ~31-bit offset (covers 1.536 GB), and a ~14-bit size
--     (covers 16 kB) — all still packing into the one Word64.
--
--   * Promote-on-new-reference: when an active-space EB references (via
--     'insertBody') a tx whose stored bytes are still in the draining space, copy
--     those bytes into the active space and update the location. This copies
--     exactly the txs a not-yet-evicted EB depends on — i.e. exactly those that
--     survive the current cycle — so there is no over-copy. Txs referenced only by
--     draining-space EBs are never promoted.
--
--   * Eviction: when the oldest EB ages out (FIFO), decrement its txs' refcounts
--     and drop those that reach zero — exactly the refcount cascade the pure index
--     already performs (evictOldest / decBody above), with no copying. Its only
--     new, GC-related duty is bookkeeping for Flip (below): as each dying tx is
--     dropped, decrement the live-tx count of the space it occupied.
--
--   * Flip: a live-tx count per space is what tells us a space is empty, and
--     eviction (above) is the operation that decrements it. The count also tracks
--     bump-allocation into the active space and promotion (which moves one tx
--     from draining to active). When the draining space's count reaches zero —
--     every tx it held has been promoted out or has died — flip. The flip is purely
--     logical: swap the two roles and reset the newly-active space's bump pointer to
--     0. Its bytes are all dead, so new bump-allocations simply overwrite them in
--     place — no zeroing, no data movement, no reclaiming disk. Both files stay
--     fully allocated (preallocate once, reuse forever): we have already budgeted the
--     2x reserve, and handing blocks back only to re-grow them next cycle would just
--     churn filesystem allocation for no gain. Because promotion never does extra
--     copying, the active space only ever holds live txs, so its bump pointer never
--     exceeds the max live set — one 1.536 GB space per side suffices and the
--     flip-at-zero trigger is sound.
--
--   * Bounded work per EB: an EB references ≤ 12 MB of txs, so both the promote
--     copy (per active-space EB processed) and the eviction pass (per draining EB)
--     touch ≤ 12 MB. That is bounded and ~constant — a few ms of memcpy in RAM, a
--     bounded direct-I/O transfer on disk — so no incremental-evacuation chunking
--     is needed; the 12 MB per-EB cap already does the job Option A's allocator had
--     to include so much complexity to achieve.
--
--   * Restart: start empty and re-warm from ongoing Leios traffic, paying the
--     miss penalties above until it refills. A clean-shutdown snapshot (persist
--     the store, the index, and the announcement/body maps, mutually consistent)
--     is an optional optimization to avoid a cold start — never required, and the
--     only situation in which the store and index must agree on disk.
--
--   * Corruption detection: since the store is keyed by TxHash, a content hash,
--     every consumer of the actual tx-bytes is gated by the cheap integrity
--     check: re-hash the returned bytes against the key (no separate checksum
--     needed). So corruption is always detected at the point of use.
--
--   * Corruption recovery: treat it as fatal. The Cardano node detects hardware
--     failure, but is not expected to compensate for it. A node with a failing
--     disk is not a healthy participant.
--
-- A note on the flip: promote-on-new-reference spreads the copy work across the
-- cycle. The textbook alternative is to copy the entire live set at once when the
-- active space fills (walking the index) and flip then — simpler logic, no
-- promotion bookkeeping, but one larger pause per flip. Same 2x either way.
