# Cardano Mempool — Overview of How It Currently Works

*A working document describing the Cardano node's mempool as implemented in
`IntersectMBO/ouroboros-consensus`. Intended as a starting point for discussions
about formalisation and proposed rule changes. Will be kept up to date as we
verify points against the source.*

**Last updated:** 2026-05-06
**Source repo:** <https://github.com/IntersectMBO/ouroboros-consensus>
**Source path:** `ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Mempool/`
**Source ref:** `main` (read on 2026-05-06)
**Files read in detail:** `API.hs`, `Update.hs`, `Init.hs`, `Impl/Common.hs`,
`Query.hs`, `Capacity.hs`, `TxSeq.hs` (haddock).

---

## 1. The shape of the data structure

**Ticket numbers** (`TicketNo`, a monotonically increasing integer) are
assigned to each transaction at the moment it enters the mempool. They are
never reused: once a transaction is removed, its number is retired and the
high-water mark `isLastTicketNo` is never decremented. Tickets give external
readers — most importantly, peers streaming new transactions via
`snapshotTxsAfter ticketNo` — a stable cursor into the sequence: "give me
everything that arrived after ticket N."

The mempool is **not** a strict FIFO queue — it is an *ordered sequence* with
revalidation, admission control, and timeout-based rejection. The core data
structure (`TxSeq`, `Mempool/TxSeq.hs`) is a finger tree of
`(Tx, TicketNo, TxMeasure)` triples, where ticket numbers are allocated
**monotonically** as transactions enter.

So:

- **Insertion order is preserved** for the lifetime of a tx in the mempool.
- The structure supports removal from any position and split/lookup by ticket —
  it is richer than a simple queue.
- Tickets are how external readers (e.g. peers streaming new txs) track their
  progress: `snapshotTxsAfter ticketNo` returns "everything new since I last
  looked".

The conceptual state (the `InternalState` record, `Impl/Common.hs:104`) is:

```
(L, slot, tip, [t₁, t₂, …, tₙ], lastTicket, capacity)
```

where:

- `L` (`isLedgerState`) is the ticked ledger state **after** applying all
  pending txs `t₁..tₙ` to the ledger state pointed to by the chain tip. 
  This is what new arrivals are validated against.
- `tip` (`isTip`) is the chain tip — a `(SlotNo, HeaderHash)`
  pair identifying the block that pending `t₁..tₙ` get validated against. Contains
  no ledger data; used only for cheap equality checks (has the chain moved?).
- `slot` (`isSlotNo`) is `succ tipSlot` — the mempool optimistically tickets
  for the next slot. (TODO comment in code suggests this should become
  time-based; see `Impl/Common.hs:332`.)
- `lastTicket` (`isLastTicketNo`) is the all-time max for ticket numbers.
- `capacity` (`isCapacity`) is the current capacity bound on raw transaction 
bytes, script `ExUnits`, and reference script bytes.

**Invariant from the source** (paraphrased from `Impl/Common.hs:140`):
`isLedgerState` is the ledger resulting from applying the txs in `isTxs`
against the ledger identified by `isTip`, ticked to `isSlotNo`.

## 2. Getting in: `addTx` (`Update.hs:67`)

When a transaction arrives:

1. **Fairness queueing.** The tx is tagged with `AddTxOnBehalfOf` — either
   `AddTxForRemotePeer` or `AddTxForLocalClient`. There are two MVars
   (`mpEnvAddTxsRemoteFifo`, `mpEnvAddTxsAllFifo`) implementing a 2-level FIFO:
   remotes must take both, locals only the second. This biases service toward
   local clients while keeping fairness within each class. From the haddock:
   "If there are N remote peers and M local clients, each local client gets
   weight 1/(M+1), while all of the N remote peers together also get total
   weight 1/(M+1)" — i.e. one local client ≈ all remotes combined.
2. **Validation.** Inside the lock, `pureTryAddTx` (`Update.hs:298`) validates
   the tx against the **post-state of the mempool** (`isLedgerState`), not
   against the chain tip directly. So a tx that depends on an earlier mempool
   tx will validate fine.
3. **Capacity check.** Performed *before* committing the new tx (it has access
   to the post-validation state). The check is:
   `(currentSize + txMeasure) ≤ capacity` — multi-dimensional (`Data.Measure`).
   There is also an overflow guard.
4. **Outcomes** (`TriedToAddTx`, `Update.hs:134`):
   - **Valid + fits:** `Processed` → tx is appended at the back with a fresh
     ticket; `isLedgerState` is updated by prepending the tx's diff;
     `isTxKeys`, `isTxValues`, `isTxIds` are all updated. Result:
     `MempoolTxAdded`.
   - **Invalid:** `NotProcessed` → `MempoolTxRejected`. Tx never enters.
   - **Doesn't fit:** `NotEnoughSpaceLeft` → caller (`doAddTx'`) **retries**
     with `additionalCheck` that blocks via `check` until the mempool size
     changes. Confirmed: **no eviction of incumbents to make room.** The
     blocking retry holds the FIFO MVar, so the order in which threads got
     the lock is the order in which they get to retry.
5. **Concurrency:** the FIFO MVar serialises competing `addTx` threads at
   tx granularity, not batch granularity, so multiple submitters interleave.

### 2a. Validation timeouts 

The mempool has a configurable `MempoolTimeoutConfig` (`API.hs:276`) with
three thresholds:

- **`mempoolTimeoutSoft`** — if validation took longer than this, the tx is
  *rejected* (returned as `MempoolRejectedByTimeoutSoft`) rather than added.
  Only effective for local clients (`Intervene` mode); from Conway onward.
- **`mempoolTimeoutHard`** — if validation took longer than this, the call
  *throws* `MkExnMempoolTimeout` (intended to disconnect the peer).
- **`mempoolTimeoutCapacity`** — a capacity component measured in cumulative
  validation time (a `DiffTimeMeasure` summed across all txs in the mempool).
  Once exceeded, no further txs are admitted.

So validation duration is part of the capacity vector — the mempool can be
"full" in bytes, in script ExUnits, *or* in cumulative validation time.

### 2b. `WhetherToIntervene` (`Update.hs:128`)

Local clients are added with `Intervene`; remotes with `DoNotIntervene`. This
controls whether the ledger may report extra rejection details (e.g. soft-
timeout rejection) — local clients get more information back. Does not affect
which txs are admitted, only error reporting.

## 3. Getting out

There are **three** removal paths. There is **no fee-based reordering, no
replace-by-fee, no priority queue.** Ordering is purely arrival order, modulo
drops from revalidation.

### 3a. Sync with ledger (`implSyncWithLedger`, `Update.hs:517`)

Triggered by a `Watcher` thread keyed on the ledger tip point (`Init.hs:65`).
The watcher uses STM `retry` semantics: it wakes up exactly when the tip
changes (event-driven, not polling).

Inside the sync:

1. Reads the *current* tip again inside the same STM transaction, to handle
   the race where the tip changed again between watcher wake-up and lock
   acquisition. (Detailed comment at `Update.hs:540` explains the
   interleaving.)
2. **Skip-fast path:** if `isTip == currentTip ∧ isSlotNo == newSlot`, do
   nothing — emit `TraceMempoolSyncNotNeeded`.
3. **Otherwise:** call `revalidateTxsFor` (`Impl/Common.hs:403`), which
   reapplies every tx in `isTxs`, in order, against the new ticked ledger
   state. Txs that fail are dropped; survivors keep their relative order.
4. The capacity is recomputed from the new ledger state; the fresh
   `InternalState` replaces the old one.

The fingerprint for the watcher is the tip *point* (`Init.hs:81`) — equality
check is cheap.

### 3b. Manual removal (`implRemoveTxsEvenIfValid`, `Update.hs:439`)

Given a non-empty set of `GenTxId`s:

1. Filter them out of `isTxs`.
2. Call `revalidateTxsFor` on the survivors. **Note:** even though the call
   site believes the survivors are still valid, revalidation runs anyway —
   so dependent txs that relied on a removed tx will themselves be dropped
   and reported in `TraceMempoolManuallyRemovedTxs`'s second list.

### 3c. Capacity pressure (admission control only)

Confirmed: `addTx` **blocks** when full, retrying when the mempool size
changes. **Incumbents are never evicted.** The blocked thread holds the
fairness MVar, preserving order.

There is one subtlety: capacity itself can shrink (it's recomputed from the
new ledger state on every sync). When it shrinks below the current size, the
mempool stays *over* capacity — incumbents are not removed to satisfy the new
limit. From the source comment (`Impl/Common.hs:108`): "We let the
transactions get removed in the normal way: by becoming invalid w.r.t. the
updated ledger state. We treat a Mempool /over/ capacity in the same way as a
Mempool /at/ capacity."

So an over-capacity mempool simply rejects new arrivals until it drains.

## 4. Reading from it: snapshots

### 4a. `getSnapshot` (`Init.hs:117`)

Pure STM read of the cached `InternalState`. Returns a `MempoolSnapshot` over
the current `isTxs`, `isTip`, `isSlotNo`. Does **not** touch the ledger or
revalidate. Used by tx-submission to peers.

### 4b. `getSnapshotFor` (`implGetSnapshotFor`, `Query.hs:14`)

Used by the block forger to get a snapshot validated against a specific
ledger state (the one it is forging on top of, possibly ticked to a different
slot). Three cases:

- **Cache hit** (same tip *and* same slot): return the cached snapshot
  directly.
- **Same tip, different slot:** reuse the cached `isTxValues` (the UTxO entries
  the txs depend on), call `computeSnapshot` (`Impl/Common.hs:459`) to
  re-reapply the txs against the requested ticked state.
- **Different tip:** read the required UTxO values from the LedgerDB
  (`readUntickedTables`), then `computeSnapshot`.

Critically, `getSnapshotFor` **does not mutate the cached state** — it
computes a fresh snapshot and returns it. The cache only changes via `addTx`,
`syncWithLedger`, or `removeTxsEvenIfValid`.

The `MempoolSnapshot` itself (`Impl/Common.hs:491`) supports:

- `snapshotTxs` — full ordered list, oldest → newest.
- `snapshotTxsAfter ticketNo` — txs with ticket > ticketNo (used by peer
  streaming).
- `snapshotTake limit` — txs whose cumulative size fits within `limit`
  (used by the forger to fill a block).
- `snapshotLookupTx ticketNo`, `snapshotHasTx txId`, `snapshotMempoolSize`,
  `snapshotSlotNo`, `snapshotStateHash`, `snapshotPoint`.

## 5. Key invariants 

The following are the load-bearing invariants — these are what a formal model
must preserve, and what proposed rule changes must continue to satisfy (or
explicitly break, with justification).

1. **Sequential validity.** For cached state `L`, tip-point `tip`, slot
   `s = succ(slotOf tip)`, and tx list `[t₁..tₙ]`: the fold of `applyTx` over
   `[t₁..tₙ]` starting from `(tip ticked to s)` succeeds at every step and
   yields `L`. This is preserved by `addTx`, restored by `syncWithLedger` and
   `removeTxsEvenIfValid`.
2. **Order preservation.** Removals (from any of the three paths) never
   reorder the survivors. `revalidateTxsFor` builds the new sequence by
   folding over the input list left-to-right, preserving order.
3. **Ticket monotonicity.** `isLastTicketNo` strictly increases. Once
   assigned to a tx, a ticket number is never reused, even after the tx is
   removed (`isLastTicketNo` is preserved across `revalidateTxsFor`).
4. **Cached derived fields.** `isTxIds = {txId t | t ∈ isTxs}`,
   `isTxKeys = ⋃ getTransactionKeySets (isTxs)`,
   `isTxValues = readTables(isTip, isTxKeys)`. All three must be in sync
   with `isTxs`.
5. **Capacity relation.** `isCapacity = computeMempoolCapacity(cfg, ledgerAt(isTip))`,
   which is `2 × blockCapacity` by default (or a multiple-of-blocks override).
   **Note:** `Σ measure(tᵢ) ≤ isCapacity` is *not* a strict invariant — it can
   be violated transiently when capacity shrinks across a sync. `addTx`
   maintains it on insertion; `syncWithLedger` may break it momentarily.

## 6. Capacity in detail (`Capacity.hs`)

- **Default:** `2 × blockCapacityTxMeasure(ledger)`. So if a block can hold
  ~88kB of txs, the mempool holds ~176kB.
- **Override:** `MempoolCapacityBytesOverride bytes` is rounded *up* to the
  nearest multiple of one block's capacity (with a min of 1 block). So the
  capacity is always a positive integer multiple of one block's worth.
- Capacity is a `TxMeasure` — multi-dimensional. For Conway this includes
  byte size, script ExUnits, reference-script byte size, and (via
  `mempoolTimeoutCapacity`) cumulative validation time.

## 7. Operations summary (cheat sheet)

| Operation              | Effect                                                                | Module / function                          |
|------------------------|-----------------------------------------------------------------------|--------------------------------------------|
| `addTx`                | Validate against `isLedgerState`; append if valid; block if full      | `API.hs:118`, `Update.hs:67`               |
| `addTxs` / `addLocalTxs` | Convenience wrappers — `mapM (addTx wti)` for a list                | `API.hs:356`, `API.hs:371`                 |
| `syncWithLedger`       | On tip change: revalidate all txs against new chain tip; drop invalid | `Update.hs:517`, watcher in `Init.hs:65`   |
| `removeTxsEvenIfValid` | Drop named txs; revalidate the rest (cascading drops possible)        | `Update.hs:439`                            |
| `getSnapshot`          | Pure STM view of cached state; does not touch the ledger              | `Init.hs:117`, snapshot at `Impl/Common.hs:491` |
| `getSnapshotFor`       | Snapshot validated against a specific ledger state (for forging)      | `Query.hs:14`                              |
| `getCapacity`          | Pure STM read of `isCapacity`                                         | `Init.hs:119`                              |
| `testSyncWithLedger`   | Test-only: forces a sync with the current chain tip                   | `Init.hs:120`                              |
| `testTryAddTx`         | Test-only: returns `Nothing` instead of blocking when full            | `Init.hs:121`                              |

## 8. Subtleties worth knowing

- **The mempool is allowed to be over-capacity** transiently (when capacity
  shrinks across a tip change). New arrivals are still rejected; drainage
  happens via revalidation drops or block inclusion.
- **The cached ledger state is ticked** to `succ tipSlot` already — so when
  `addTx` validates a tx against `isLedgerState`, it's effectively validating
  it for inclusion in the *next* block, not the current tip's block. (TODO in
  source: this should be time-based, not slot-based.)
- **`removeTxsEvenIfValid` may cascade.** Even if you only ask to remove tx
  `t`, any tx `t'` whose validity depended on `t`'s outputs will also be
  dropped on the revalidation pass. The trace event reports both lists.
- **There are validation timeouts.** A tx that takes too long to validate is
  either soft-rejected (local) or causes a peer disconnect (remote/hard).
  Validation time is part of the capacity vector.
- **`getSnapshotFor` is read-only with respect to the cached state.** The
  forger asking for a snapshot at a specific slot does not move the mempool's
  cache pointer. The cache only advances via the watcher / explicit ops.
- **No replace-by-fee, no priority, no eviction.** Order is purely arrival
  order. The only way a tx leaves is: included in a block (and dropped on
  next sync), invalidated by a sync, manually removed, or — on shutdown.
- **Ticket numbers are persistent identifiers** for the lifetime of a tx in
  the mempool, but tickets do *not* survive removal+readd: a removed-and-
  resubmitted tx gets a fresh, larger ticket number.

## 9. Open questions / things still to verify

The major mechanical questions are now resolved. Remaining items are mostly
about edge cases and the Forker / UTxO-HD interaction:

- [ ] **Forker semantics.** `MempoolEnv` holds a `ReadOnlyForker` MVar, swapped
  on every successful sync. What exactly does the forker provide? My current
  reading is "consistent read-access to the LedgerDB at a specific point",
  needed because UTxO state lives partly on disk in UTxO-HD. Worth a read of
  `Storage.LedgerDB.Forker`.
- [ ] **`ValidatedTxWithDiffs` caching.** Each pending tx caches its
  `LedgerTables blk DiffMK` — the diff it produces. Comment at
  `Impl/Common.hs:84` says these diffs "cannot be stale" under current
  UTxO-HD assumptions. Worth understanding what would break that.
- [ ] **`tickLedgerState` slot choice.** Currently uses `succ tipSlot`. The
  TODO at `Impl/Common.hs:332` says it should be time-based. Implications for
  any rule that depends on slot — e.g. tx validity intervals.
- [ ] **Behaviour during ledger rollback.** If the chain rolls back across
  blocks that contained mempool-resubmitted txs, what happens? `syncWithLedger`
  will revalidate, but the previously-included txs need to come back from
  somewhere — probably the diffusion layer re-supplies them.

## 10. Glossary

- **`InternalState`** — the cached state record (`Impl/Common.hs:104`).
- **`MempoolEnv`** — the wiring (MVars, tracer, ledger interface, etc.;
  `Impl/Common.hs:262`).
- **`MempoolSnapshot`** — immutable view, returned by `getSnapshot` /
  `getSnapshotFor` (`API.hs`).
- **`TxSeq`** — finger-tree sequence of `TxTicket`s (`TxSeq.hs`).
- **`TxTicket`** — `(tx, ticketNo, measure)` triple — the unit stored in
  `TxSeq`.
- **`TicketNo`** — monotonically allocated integer; identifies a tx's
  position for streaming readers.
- **`Forker`** — a read-only handle to the LedgerDB at a specific point;
  used to read UTxO values for tx validation.
- **`ValidatedTxWithDiffs`** — a validated tx plus the cached
  `LedgerTables blk DiffMK` it produced.

---

## 11. Sketch Agda formalisation

**Status:** sketch — not type-checked. Intended as a starting point for
discussion, not a finished spec. Postulates the validation mechanics and the
underlying types; models the three core operations and the key invariants.

The model omits, deliberately:

- **Concurrency / fairness MVars** — modelled implicitly: each operation is a
  pure transition on the state. The 2-level FIFO and local-vs-remote
  weighting are an implementation detail of the *scheduler*, not of the data
  structure.
- **Validation timeouts** — `mempoolTimeoutSoft/Hard/Capacity` could be added
  as an extra component in `Measure`, but are skipped here for clarity.
- **`WhetherToIntervene`** — only changes error reporting, not which txs are
  admitted.
- **The `Forker` / cached `isTxKeys`/`isTxValues`** — these are performance
  caches over the ledger; the abstract `applyTx` here is total over the
  full ledger state.
- **Cascading removal** — `removeTxs` here drops the named txs and lets
  revalidation drop dependents; behaviour matches `implRemoveTxsEvenIfValid`.

```agda
module Mempool where

open import Data.Bool       using (Bool; true; false; if_then_else_; _∧_)
open import Data.List       using (List; []; _∷_; _++_; foldl; filter; map)
open import Data.Maybe      using (Maybe; just; nothing)
open import Data.Nat        using (ℕ; zero; suc; _≤_; _+_)
open import Data.Product    using (_×_; _,_; proj₁; proj₂)
open import Relation.Binary.PropositionalEquality using (_≡_; refl)

------------------------------------------------------------------------
-- 1. Postulated types and the validation oracle
------------------------------------------------------------------------

-- Abstract types we don't model internally.
postulate
  Tx          : Set
  TxId        : Set
  LedgerState : Set            -- already ticked to the next slot
  TipPoint    : Set            -- identifies a chain tip
  Capacity    : Set            -- multi-dimensional (bytes, ExUnits, …)
  Measure     : Set            -- the size/cost of a single tx

  txId        : Tx → TxId
  measure     : Tx → Measure

  -- Decidable identity on TxIds, used by removeTxs.
  _≟Id_       : TxId → TxId → Bool

  -- Capacity arithmetic. Total to keep the spec simple.
  zeroSize    : Measure
  addSize     : Measure → Measure → Measure
  fits        : Measure → Capacity → Bool   -- "current + new ≤ cap"

  -- The validation oracle: try to apply a tx to a ledger state.
  -- `nothing` means the tx is invalid in this state.
  applyTx     : LedgerState → Tx → Maybe LedgerState

  -- Recover the ledger state at a given tip, ticked to the next slot.
  -- Models tickLedgerState ∘ getCurrentLedgerState.
  ledgerAt    : TipPoint → LedgerState

  -- Compute the capacity for a given tip (default: 2× block capacity).
  capacityAt  : TipPoint → Capacity

------------------------------------------------------------------------
-- 2. Tickets and the TxSeq
------------------------------------------------------------------------

TicketNo : Set
TicketNo = ℕ

record TxTicket : Set where
  constructor mkTicket
  field
    tx     : Tx
    ticket : TicketNo
    msr    : Measure
open TxTicket

-- The mempool's tx sequence. The Haskell code uses a finger tree for
-- O(log n) splits; we model it as a plain list — the *order* is what
-- matters for the spec, not the asymptotic complexity.
TxSeq : Set
TxSeq = List TxTicket

------------------------------------------------------------------------
-- 3. The mempool state
------------------------------------------------------------------------

-- Mirrors `InternalState` from Impl/Common.hs:104, minus the caches
-- (isTxIds, isTxKeys, isTxValues) which are derivable.
record Mempool : Set where
  constructor mkMempool
  field
    txs        : TxSeq        -- the pending transactions, in order
    ledger     : LedgerState  -- isLedgerState: post-state of all txs
    tip        : TipPoint     -- isTip: chain tip txs were validated against
    lastTicket : TicketNo     -- isLastTicketNo: high-water mark
    cap        : Capacity     -- isCapacity (snapshot at tip)
open Mempool

emptyAt : TipPoint → Mempool
emptyAt p = mkMempool [] (ledgerAt p) p 0 (capacityAt p)

------------------------------------------------------------------------
-- 4. Sequential validity — the load-bearing invariant
------------------------------------------------------------------------

-- Fold applyTx over a tx list; returns the final state iff every step
-- succeeds.
applyAll : LedgerState → List Tx → Maybe LedgerState
applyAll s []       = just s
applyAll s (t ∷ ts) with applyTx s t
... | nothing = nothing
... | just s′ = applyAll s′ ts

-- The tx list applies cleanly to `ledgerAt tip`, yielding `ledger`.
SequentiallyValid : Mempool → Set
SequentiallyValid m =
  applyAll (ledgerAt (tip m)) (map tx (txs m)) ≡ just (ledger m)

-- The capacity bound. Note this is *not* an invariant — it can be
-- transiently violated when the ledger shrinks capacity across a sync.
WithinCapacity : Mempool → Set
WithinCapacity m =
  fits (foldl addSize zeroSize (map msr (txs m))) (cap m) ≡ true

-- Tickets strictly increase along the sequence and are bounded by
-- lastTicket. Together these give "tickets are never reused".
data TicketsMonotone : TicketNo → TxSeq → Set where
  tm-[]  : ∀ {n} → TicketsMonotone n []
  tm-∷   : ∀ {n t ts} → n ≤ ticket t → TicketsMonotone (suc (ticket t)) ts
                       → TicketsMonotone n (t ∷ ts)

------------------------------------------------------------------------
-- 5. addTx
------------------------------------------------------------------------

-- The capacity check from pureTryAddTx (Update.hs:344). Returns true iff
-- adding `m` would still fit in `c` given the current usage.
fitsWith : Capacity → Measure → Measure → Bool
fitsWith c current new = fits (addSize current new) c

currentSize : Mempool → Measure
currentSize m = foldl addSize zeroSize (map msr (txs m))

-- Mirrors the result of doAddTx (Update.hs:177). `Blocked` is the
-- "no space, retry later" case — at the data-structure level it leaves
-- the mempool unchanged. The fact that addTx *blocks* the calling thread
-- is a property of the harness, not of this transition relation.
data AddResult : Set where
  Added    : Mempool → AddResult
  Rejected : Mempool → AddResult         -- invalid; mempool unchanged
  Blocked  : Mempool → AddResult         -- full; mempool unchanged

addTx : Tx → Mempool → AddResult
addTx t m with fitsWith (cap m) (currentSize m) (measure t)
... | false = Blocked m
... | true  with applyTx (ledger m) t
...   | nothing  = Rejected m
...   | just ℓ′  =
        let n′ = suc (lastTicket m)
            tk = mkTicket t n′ (measure t)
        in Added (mkMempool (txs m ++ tk ∷ [])
                            ℓ′
                            (tip m)
                            n′
                            (cap m))

------------------------------------------------------------------------
-- 6. revalidateTxsFor — the workhorse for sync and removeTxs
------------------------------------------------------------------------

-- Reapply a list of txs in order against a fresh ledger state, dropping
-- any that fail. Survivors keep their relative order and their original
-- ticket numbers. Mirrors revalidateTxsFor (Impl/Common.hs:403).
revalidate : LedgerState → TxSeq → LedgerState × TxSeq
revalidate s []          = s , []
revalidate s (tk ∷ tks)  with applyTx s (tx tk)
... | nothing = revalidate s tks                         -- drop tk
... | just s′ =
       let ℓ″ , kept = revalidate s′ tks
       in ℓ″ , tk ∷ kept                                 -- keep tk

------------------------------------------------------------------------
-- 7. syncWithLedger
------------------------------------------------------------------------

-- A tip change triggers a re-sync. Skip-fast path (same tip) is omitted
-- here — it's a performance optimisation, observationally identical to
-- a no-op when the tip is unchanged.
syncWithLedger : TipPoint → Mempool → Mempool
syncWithLedger newTip m =
  let s₀         = ledgerAt newTip
      ℓ′ , kept  = revalidate s₀ (txs m)
  in mkMempool kept ℓ′ newTip (lastTicket m) (capacityAt newTip)

------------------------------------------------------------------------
-- 8. removeTxsEvenIfValid
------------------------------------------------------------------------

-- Drop any tx whose id appears in the removal set, then revalidate the
-- rest. Cascading drops happen naturally via revalidate: a tx that
-- depended on a removed one will fail applyTx and be dropped.
inSet : List TxId → TxId → Bool
inSet []        _   = false
inSet (i ∷ is)  i′  = if i ≟Id i′ then true else inSet is i′

removeTxs : List TxId → Mempool → Mempool
removeTxs ids m =
  let kept0      = filter (λ tk → if inSet ids (txId (tx tk))
                                  then false else true) (txs m)
      s₀         = ledgerAt (tip m)
      ℓ′ , kept  = revalidate s₀ kept0
  in mkMempool kept ℓ′ (tip m) (lastTicket m) (cap m)

------------------------------------------------------------------------
-- 9. Snapshots (read-only views)
------------------------------------------------------------------------

record Snapshot : Set where
  constructor mkSnap
  field
    snapTxs    : TxSeq
    snapTip    : TipPoint
    snapLedger : LedgerState

getSnapshot : Mempool → Snapshot
getSnapshot m = mkSnap (txs m) (tip m) (ledger m)

-- snapshotTxsAfter from Impl/Common.hs:518 — txs strictly after a ticket.
txsAfter : TicketNo → Snapshot → TxSeq
txsAfter n = filter (λ tk → suc n ≤? ticket tk) ∘ Snapshot.snapTxs
  where
    postulate _≤?_ : ℕ → ℕ → Bool
              ∘    : ∀ {A B C : Set} → (B → C) → (A → B) → A → C

-- getSnapshotFor (Query.hs:14) is a *read-only* operation: it returns a
-- snapshot for a different ledger state without mutating the mempool's
-- cache. We model the three branches as one function.
getSnapshotFor : TipPoint → Mempool → Snapshot
getSnapshotFor p m =
  let s₀ = ledgerAt p
      _ , kept = revalidate s₀ (txs m)
  in mkSnap kept p s₀
  -- The Haskell impl has three cases (cache hit / same-tip / different-tip)
  -- for performance; observationally they all agree with this definition.

------------------------------------------------------------------------
-- 10. Properties to prove (left as exercises / theorems)
------------------------------------------------------------------------

-- These are the obligations a formal proof would discharge. None of
-- them are proved here — they are the targets.

postulate
  -- addTx preserves sequential validity.
  addTx-preserves-valid :
    ∀ (t : Tx) (m : Mempool) →
    SequentiallyValid m →
    (case addTx t m of λ where
       (Added m′)    → SequentiallyValid m′
       (Rejected m′) → SequentiallyValid m′
       (Blocked  m′) → SequentiallyValid m′)

  -- syncWithLedger restores sequential validity for the new tip.
  sync-establishes-valid :
    ∀ (p : TipPoint) (m : Mempool) →
    SequentiallyValid (syncWithLedger p m)

  -- removeTxs preserves sequential validity.
  remove-preserves-valid :
    ∀ (ids : List TxId) (m : Mempool) →
    SequentiallyValid m →
    SequentiallyValid (removeTxs ids m)

  -- All three operations preserve relative order of the txs they keep.
  --   (formal statement: survivors are a subsequence of the original)
  -- All three operations preserve ticket monotonicity.
  -- addTx strictly increases lastTicket on `Added`; preserves it otherwise.
```

### Notes on this sketch

- **What's faithful:** the three operations, the order-preserving
  revalidation, the cached post-state ledger, the explicit
  sequentially-valid invariant, the read-only nature of `getSnapshotFor`,
  ticket monotonicity, the fact that capacity *can* be exceeded transiently.
- **What's abstracted:** UTxO-HD's keys/values/diffs caching, the Forker, the
  `Watcher` thread, validation timeouts, fairness MVars, the
  `WhetherToIntervene` flag, multi-dimensional `Measure` arithmetic.
- **What's missing for a real proof:** the `_≤?_` and `∘` near `txsAfter`
  are sloppy postulates to keep the sketch terse. Real Agda would import
  these from the standard library.
- **Likely first targets if we develop this further:**
  1. Prove `sync-establishes-valid` from the definition of `revalidate`.
  2. Prove `addTx-preserves-valid` (the `Added` case is the only
     non-trivial one).
  3. State and prove a "subsequence" property: every operation's
     output `txs` is a subsequence of its input plus possibly one new
     element at the end (for `addTx`).

## Changelog

- **2026-05-06** — Initial draft from haddock + first read of `API.hs`,
  `Update.hs`, `TxSeq.hs` on `main`.
- **2026-05-06** — Verified all major open questions by reading
  `Update.hs`, `Init.hs`, `Impl/Common.hs`, `Query.hs`, `Capacity.hs`
  in full. Resolved: full-capacity blocks (no eviction); `isLedgerState` is
  the *post-state* of the mempool, ticked to `succ tipSlot`; `getSnapshotFor`
  is non-mutating with three cache strategies; background sync is
  event-driven on tip-point change. Added new sections: validation timeouts,
  fairness MVar pair, `WhetherToIntervene`, capacity formula, cascading
  removals, over-capacity behaviour. Added a glossary. Tightened invariants
  to distinguish sequential validity (always holds) from capacity bound
  (transient violations possible).
- **2026-05-06** — Added §11: sketch Agda formalisation. Postulates Tx,
  LedgerState, applyTx, Capacity. Models addTx / syncWithLedger /
  removeTxsEvenIfValid as pure transitions, plus snapshots. States the
  sequential-validity invariant explicitly; lists target theorems as
  postulates. Not type-checked.