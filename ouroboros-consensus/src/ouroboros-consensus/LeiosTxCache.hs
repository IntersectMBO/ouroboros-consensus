{-# LANGUAGE Rank2Types #-}

-- | The LeiosTxCache tracks txs that were acquired because a /recent/ EB
-- referenced them.
--
-- The LeiosTxCacheIndex records two facts about each tracked tx: whether it is
-- already acquired and whether it has already been validated (by the Mempool or
-- by the LeiosVoting thread).
--
-- The index is in-memory so that latency-critical consumers (LeiosFetch,
-- LeiosVoting) can query it with constantly low latency; its eventual purpose is
-- to /supplant/ the by-hash membership check that @filterMissingWork@ does
-- against the LeiosDb. The size of the LeiosTxCache is bounded first and foremost
-- by the requirement that its index fits comfortably in-memory, even in a worst
-- case.
--
-- This module is the umbrella: it re-exports the "LeiosTxCache.API" interface
-- and both handle factories, 'newPureLeiosTxCache' from
-- "LeiosTxCache.Reference" and 'newHashTableLeiosTxCache' from
-- "LeiosTxCache.Optimized".
--
-- == INVARIANT: an AlreadyAcquired tx is in the LeiosDb and will be for hours
--
-- Challenge: it's possible that LeiosFetch finds some of an EB's TxHashes in
-- the LeiosTxCacheIndex but then is unable to read those txs from the LeiosDb.
-- This happens when the LeiosTxCacheIndex contains an EB that has an age close
-- enough to the immutable tip that it could be pruned from the LeiosDb after
-- LeiosFetch sees the cache hit but before its subsequent reads finish. There
-- are many potential solutions.
--
-- - Simply detect and recover. This is feasible, but undesirable. When
--   processing an EB arrival, LeiosFetch divides it into a set of jobs, where
--   each job is a set of txs the node needs to fetch. LeiosFetch is already
--   very complicated, so I don't want to add the complexity of subsequently
--   adding some jobs to compensate for some of the LeiosTxCacheIndex hits
--   ending up stale due to a hit-prune race. And I also don't want to add
--   latency by waiting for the hit-driven lookups to finish before finalizing
--   the job set.
--
-- - Use MVCC. Our current LeiosDb implementations (in-memory and SQLite) both
--   happen to provide persistence (eg open a read transaction before querying
--   the LeiosTxCacheIndex). However, MVCC is a sophisticated feature which we'd
--   rather not require of the LeiosDb. It's not clear to me that any other
--   component already does, so I'd rather not have the LeiosTxCacheIndex impose
--   that constraint on LeiosDb.
--
-- - Also keep the LeiosTxCache's backing store in RAM. Even if we had
--   zero-overhead for GC, this would require up to 1.536 GB of RAM. That seems
--   like too much, since the fundamental purpose of the LeiosTxCache is merely
--   to prevent having to refetch the data /from peers/---some disk latency is
--   completely fine.
--
-- - Have LeiosFetch pin the txs as side-effect of looking them up in the
--   LeiosTxCacheIndex.
--
--     - While the LeiosTxCache is backed by the LeiosDb, this requires
--       undesirable coupling between the LeiosDb's pruning logic and the
--       LeiosTxCacheIndex.
--
--     - If the LeiosTxCache were instead backed by its own bespoke independent
--       (on-disk) storage, then this would be more tenable. But that's still
--       undesirable complexity to engineer if we don't actually have to.
--
-- - Rely on hours of slack between the LeiosTxCacheIndex hit and the tx being
--   pruned from the LeiosTxCache's backing store. Until recently, we had
--   assumed there was slack.
--
--     - Recall that Linear Leios must not prune an EB until all of its
--       announcements are older than the immutable tip.
--
--     - The LeiosTxCacheIndex must have very low latency (because it's used in
--       each LeiosFetch decision logic iteration), so it must be an in-memory
--       hash table, so it can't be particularly large, so it can't contain too
--       many txs (at least 32-bytes just for each TxHash, doubled for <50% load
--       factor), and so it can't contain too many EBs (up to ~15000 TxHashes
--       per EB).
--
--     - Specifically, 128 EBs seems sufficient to almost-always mitigate
--       inter-continental Mempool fragmentation.
--
--     - And since 128 EBs should arise in approximately 45 minutes on average,
--       any EB in the LeiosTxCacheIndex won't be pruned for /several/
--       /hours/---surely the LeiosFetch logic will issue and finish its reads
--       within that slack.
--
--         - The only reason LeiosFetch wouldn't is if the process were deprived
--           of CPU (eg put to sleep) for several hours.
--
--         - But in that case, its TCP connections are almost certainly dead
--           when it awakes, so the LeiosFetch reads will finish before enough
--           blocks could be fetched and selected to prune the relevant EB from
--           the backing store.
--
--         - Even if it weren't, any node that's being slept for hours is not a
--           critical node for the network, so a very rare crash is tolerable.
--
--     - However, that argument is spoiled by the fact that there's no lower
--       bound on the arrival rate of EBs. In the simplest case, there might
--       merely be less load than Praos can handle, so no EBs are /needed/. As a
--       result, the up-to-128 EBs in the LeiosTxCacheIndex might include some
--       with ages near/greater than the immutable tip.
--
-- Solution: continue to rely on there being hours of slack, but moreover
-- actively ensure that slack. In particular, evict EBs as they get "too old",
-- regardless of whether new EBs have been arriving. For example, the
-- LeiosTxCacheIndex should evict any EBs that are older than the youngest X RBs
-- on the current selection, for X≥128.
--
-- TODO The current code assumes 128 ≪ k, but that's not true on testnets,
-- etc. We should add a 'min' call somehwere.
--
-- == Coupling to the LeiosDb
--
-- Today the LeiosDb's txs table is keyed by tx hash, so the cache's free ride is
-- effortless: a tracked tx is found by its hash. But that same de-duplication is
-- what would make the LeiosDb's (not-yet-written) GC costly — pruning a tx shared
-- by several EBs needs refcounts or scans. Were the LeiosDb to key txs by
-- @(EbHash, offset)@ instead (no de-duplication; GC becomes "delete an EB's rows
-- when the EB is deleted"), its GC would be trivial, at the cost of by-hash
-- lookup — which is fine, since the cache is the only by-hash reader, so long as
-- it carries each tracked tx's location itself. See $withoutDedup.
--
-- Owning the tx bytes outright — rather than reading them from the LeiosDb at all
-- — is a further, narrower step, worthwhile only if the LeiosDb read path is ever
-- measured too slow. See $backingStore.
module LeiosTxCache
  ( module LeiosTxCache.API
  , newPureLeiosTxCache
  , newHashTableLeiosTxCache
  , nullLeiosTxCache

    -- $withoutDedup

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
      , evictOlderThan = \boundary ->
          MVar.modifyMVar var $ \idx ->
            let (idx', evEbs, evTxs) = Pure.evictOlderThan boundary idx
             in pure (idx', (evEbs, evTxs))
      , insertBody = \ebh b ->
          MVar.modifyMVar var $ \idx -> pure (Pure.insertBody ebh b idx)
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
    , insertBody = \_ebh _b -> pure Nothing
    , lookupBody = \_ebh -> pure Nothing
    , withLockedInsertUnappliedTx = \k -> k () (\w _txh _a -> pure w)
    , withLockedInsertAppliedTx = \k -> k () (\w _txh _v -> pure w)
    , withLookupTx = \k -> k (\_txh -> pure Nothing)
    }

-- $withoutDedup
--
-- = Supporting a LeiosDb that is not de-duplicated
--
-- If the LeiosDb stops de-duplicating txs — storing each EB's txs alongside the
-- EB and keying them by @(EbHash, offset)@ rather than by tx hash — its GC
-- collapses to "delete an EB's rows when the EB is deleted": no refcounts, no
-- scans. The price is that the LeiosDb can no longer be queried by tx hash; since
-- the LeiosTxCache is (or will be) the only by-hash reader, that is acceptable so
-- long as the cache can name a live location for each tx it tracks. This variant
-- does so.
--
-- == The location: a freelisted TxCacheEbId
--
-- Each tracked tx's value gains a location: which EB holds its bytes, plus the
-- offset within that EB. Naming the EB by its 32-byte EbHash would add ~34 bytes
-- to every hash-table slot (~140 MB over the 2^22 slots), so instead each EB in
-- the cache is assigned a small stable id — a @TxCacheEbId@, ~2 bytes — from a
-- freelist, and the value stores @(TxCacheEbId, offset)@. That packs into the
-- existing Word64 alongside the 2-bit state tag, so the location costs no extra
-- per-slot memory. Reading the bytes resolves the id to its EbHash and hits the
-- LeiosDb by @(EbHash, offset)@.
--
-- Ids come from a freelist and are recycled on eviction. Recycling is always safe
-- (see the eviction rule below): when an EB is evicted, no surviving tx still
-- points at it, so its id has zero inbound references and is immediately reusable
-- — even under the out-of-order arrival of announcements that would make a naive
-- monotonic id unstable.
--
-- == The location /replaces/ the refcount
--
-- A tx's stored location is the /youngest/ EB (by slot) that references it,
-- maintained exactly where the refcount is bumped today: in 'insertBody', while
-- walking the new EB's txs, an already-acquired tx's location is overwritten iff
-- the new EB is younger than its current one. No reverse index is needed — the
-- update is local to the insert.
--
-- This youngest-EB location subsumes the refcount entirely. The refcount only
-- ever answered "when does this tx die?", and "when its youngest referencing EB
-- is evicted" answers that exactly: eviction is FIFO by slot, so the youngest
-- referencer is the last to go. Hence the value holds a location + tag and /no/
-- refcount, and eviction deletes a tx precisely when the EB its location names is
-- evicted.
--
-- == Eviction becomes slot-granular
--
-- For "youngest EB" to be well-defined, eviction must remove EBs a whole slot at a
-- time, never just one EB from within a slot. The module header's rule — evict any
-- EB older than the youngest X RBs on the current selection — is already at slot
-- granularity, since its threshold falls between two slots. The only wrinkle is the
-- 128-EB cap: hitting the count exactly would otherwise split the oldest slot by
-- the least RbHash, so that tiebreaker is dropped and the cap too evicts whole
-- slots — possibly leaving fewer than 128 EBs, since 128 was always a cap, not a
-- floor. With whole-slot eviction the youngest slot is unambiguous: when slot @s@
-- is evicted, every EB at @s@ goes, so every tx whose youngest referencing slot is
-- @s@ dies exactly then.

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
-- one intended for production use. It is also orthogonal to $withoutDedup: the
-- sketch below keeps today's per-tx refcount, which $withoutDedup would replace
-- with a youngest-EB location, but either variant can be adopted without the other.
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
-- == When a separate store would help
--
-- $withoutDedup already decouples the cache from the LeiosDb's GC /without/ owning
-- any bytes: it lets the LeiosDb be non-de-duplicated (trivial GC) while the cache
-- supplies the location. So the remaining — and narrower — reason to own the bytes
-- is /read latency/: if reading a tx out of the LeiosDb's on-disk
-- @(EbHash, offset)@ storage is ever measured too slow for a latency-critical
-- consumer, a dedicated in-process store avoids that hop. This is contingency
-- planning for a bottleneck that has not been observed (and "No durability" below
-- argues on-disk still beats a network re-fetch); the rest of this note sketches
-- such a store should it ever be warranted.
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
--   * At most 128 EBs are retained in the cache at once. (The module header's
--     invariant covers the eviction rule and the choice of 128.)
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
-- access on-disk---they'll still (generally) be faster than fetching over the
-- network.
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
