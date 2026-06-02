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

```text
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

open import Agda.Primitive        using (Level; lzero; lsuc)
open import Agda.Builtin.Bool     using (Bool; true; false)
open import Agda.Builtin.List     using (List; []; _∷_)
open import Agda.Builtin.Maybe    using (Maybe; just; nothing)
open import Agda.Builtin.Nat
  using (zero; suc; _+_) renaming (Nat to ℕ)
open import Agda.Builtin.Equality using (_≡_; refl)

-- Helpers absent from Agda.Builtin; defined here for the sketch.
_∧_ : Bool → Bool → Bool
false ∧ _ = false
true  ∧ b = b

if_then_else_ : {A : Set} → Bool → A → A → A
if true  then t else _ = t
if false then _ else f = f

infixr 4 _++_
_++_ : {A : Set} → List A → List A → List A
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

map : {A B : Set} → (A → B) → List A → List B
map _ []       = []
map f (x ∷ xs) = f x ∷ map f xs

filter : {A : Set} → (A → Bool) → List A → List A
filter _ []       = []
filter p (x ∷ xs) = if p x then x ∷ filter p xs else filter p xs

foldl : {A B : Set} → (B → A → B) → B → List A → B
foldl _ acc []       = acc
foldl f acc (x ∷ xs) = foldl f (f acc x) xs

data _≤_ : ℕ → ℕ → Set where
  z≤n : {n : ℕ}           → zero  ≤ n
  s≤s : {m n : ℕ} → m ≤ n → suc m ≤ suc n

record _×_ (A B : Set) : Set where
  constructor _,_
  field proj₁ : A
        proj₂ : B
open _×_
infixr 4 _,_

case_of_ : {ℓ₁ ℓ₂ : Level} {A : Set ℓ₁} {B : Set ℓ₂} → A → (A → B) → B
case x of f = f x

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

postulate _≤?_ : ℕ → ℕ → Bool

_∘_ : {A B C : Set} → (B → C) → (A → B) → A → C
(f ∘ g) x = f (g x)

-- snapshotTxsAfter from Impl/Common.hs:518 — txs strictly after a ticket.
txsAfter : TicketNo → Snapshot → TxSeq
txsAfter n s = filter (λ tk → suc n ≤? ticket tk) (Snapshot.snapTxs s)

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

## 12. Proposed extension: two-lane mempool with EB/RB (Ranking Block) block types

**Status:** sketch — not type-checked. Extends §11; reuses `applyAll`,
`revalidate`, `TxSeq`, `TicketNo`, `Measure`, `fits`, `inSet` from there.

### Design summary

Two block types are introduced:

- **EB (Endorsement Block):** a block that is circulated peer-to-peer
  like a transaction before it reaches the chain. It carries a list of
  transactions that have *already been validated* and so need only
  re-application (no script re-execution) to update the ledger state.
  At most one EB per slot is valid; this constraint is enforced by
  checks in the EB itself and is not rechecked here. An EB only reaches
  the chain by being referenced inside a Ranking Block. An EB may
  contain transactions from either lane in any order.

- **RB (Ranking Block):** the on-chain block. Its payload is either an
  `EBId` (reference to a previously-circulated EB) or a list of
  priority-lane transactions. The referenced EB must have been built on
  the same chain tip as the Ranking Block.

Two transaction lanes replace the single sequence:

- **Priority lane:** validated against `ebLedger` (chain tip + EB
  re-application). Fills Ranking Blocks (RBs). Admission stops when the
  lane reaches one RB's worth of transactions (by byte size or either
  ExUnit dimension).

- **Regular lane:** validated against `fastLedger` (chain tip + EB +
  priority txs). Can fill EBs alongside priority txs. When the regular
  lane is full, incoming regular txs are discarded but downloading
  continues; the node keeps accepting txs until the priority lane is full.

Capacity rules:

- **Ranking Block (RB) / priority-lane limit:** one block's `TxMeasure`
  as given by the protocol parameters (byte size, script ExUnits memory,
  script ExUnits CPU, reference-script bytes).
- **EB limit:** no protocol-parameter-based limit for now.
  The priority-lane cap (one RB's worth) is used as a provisional
  bound when filling an EB. **TODO:** this design choice is provisional;
  EBs may be permitted to be larger in future.
- **Regular-lane limit:** separate capacity, policy TBD (e.g. 2× block).

```agda
module Mempool2Lane where

-- All types, postulates, and helpers from §11 (module Mempool, same file)
-- are in scope without import: Mempool2Lane is nested inside Mempool by
-- Agda's layout rules, so the parent scope is directly inherited.

-- Sum type used only in §12 (return type of forgeBlock2).
data _⊎_ (A B : Set) : Set where
  inl : A → A ⊎ B
  inr : B → A ⊎ B

------------------------------------------------------------------------
-- 1. New types
------------------------------------------------------------------------

data Lane : Set where
  Priority : Lane
  Regular  : Lane

postulate
  EBId      : Set
  _≟EBId_   : EBId → EBId → Bool
  _≟Tip_    : TipPoint → TipPoint → Bool

record EB : Set where
  constructor mkEB
  field
    ebId   : EBId
    ebTip  : TipPoint    -- must equal Mempool2.tip when the EB is accepted
    ebTxs  : List Tx     -- previously validated; re-apply only

data RBContent : Set where
  RBFromEB   : EBId    → RBContent   -- on-chain result: apply the named EB
  RBFromPrio : List Tx → RBContent   -- on-chain result: apply this tx list

record RB : Set where
  constructor mkRB
  field
    rbTip     : TipPoint
    rbContent : RBContent

-- Re-application: skips script execution (txs were already validated).
-- Only safe for txs taken from a previously accepted EB.
postulate reapplyAll : LedgerState → List Tx → Maybe LedgerState

------------------------------------------------------------------------
-- 2. Capacity postulates
------------------------------------------------------------------------

postulate
  -- 1 × blockCapacityTxMeasure from the protocol parameters.
  -- Used as the admission limit for the priority lane (and hence for
  -- RBs and, provisionally, for EBs — see TODO above).
  rbCapacityAt  : TipPoint → Capacity

  -- Separate capacity for the regular lane; policy TBD.
  regCapacityAt : TipPoint → Capacity

------------------------------------------------------------------------
-- 3. Two-lane mempool state
------------------------------------------------------------------------

-- Mirrors a two-lane InternalState.  The three cached ledger states
-- form a strict stack:
--
--   ledgerAt(tip)
--      │  reapplyAll (ebTxs currentEB)
--      ▼
--   ebLedger          ← base for priority-lane validation
--      │  applyAll priorityTxs
--      ▼
--   fastLedger        ← base for regular-lane validation
--      │  applyAll regularTxs
--      ▼
--   ledger
--
-- When currentEB = nothing, ebLedger = ledgerAt(tip).

record Mempool2 : Set where
  constructor mkMempool2
  field
    tip            : TipPoint
    -- EB layer
    currentEB      : Maybe EB
    ebLedger       : LedgerState
    -- Priority (fast) lane
    priorityTxs    : TxSeq
    fastLedger     : LedgerState
    lastPrioTicket : TicketNo
    prioCap        : Capacity        -- = rbCapacityAt tip
    -- Regular lane
    regularTxs     : TxSeq
    ledger         : LedgerState
    lastRegTicket  : TicketNo
    regCap         : Capacity        -- = regCapacityAt tip
open Mempool2

emptyAt2 : TipPoint → Mempool2
emptyAt2 p =
  mkMempool2 p nothing (ledgerAt p)
             [] (ledgerAt p) 0 (rbCapacityAt  p)
             [] (ledgerAt p) 0 (regCapacityAt p)

------------------------------------------------------------------------
-- 4. Three-layer sequential validity
------------------------------------------------------------------------

-- Layer 1: EB is consistent with the chain tip.
EBLayerValid : Mempool2 → Set
EBLayerValid m with currentEB m
... | nothing  = ebLedger m ≡ ledgerAt (tip m)
... | just eb  = reapplyAll (ledgerAt (tip m)) (EB.ebTxs eb)
                   ≡ just (ebLedger m)

-- Layer 2: priority lane is consistent with the EB post-state.
PriorityLayerValid : Mempool2 → Set
PriorityLayerValid m =
  applyAll (ebLedger m) (map tx (priorityTxs m)) ≡ just (fastLedger m)

-- Layer 3: regular lane is consistent with the fast-lane post-state.
RegularLayerValid : Mempool2 → Set
RegularLayerValid m =
  applyAll (fastLedger m) (map tx (regularTxs m)) ≡ just (ledger m)

-- NB: RegularLayerValid depends on PriorityLayerValid through fastLedger.
-- Removing a priority tx invalidates both Layer 2 and Layer 3.

------------------------------------------------------------------------
-- 5. Size helpers
------------------------------------------------------------------------

currentPrioSize : Mempool2 → Measure
currentPrioSize m = foldl addSize zeroSize (map msr (priorityTxs m))

currentRegSize : Mempool2 → Measure
currentRegSize m = foldl addSize zeroSize (map msr (regularTxs m))

fitsWith2 : Capacity → Measure → Measure → Bool
fitsWith2 c cur new = fits (addSize cur new) c

------------------------------------------------------------------------
-- 6. addTx — lane-aware admission
------------------------------------------------------------------------

-- Priority lane: validated against ebLedger.
-- Blocked when the lane has reached one RB's worth (prioCap).
-- Caller should stop submitting priority txs on Blocked.
--
-- Regular lane: validated against fastLedger.
-- Blocked when regCap is reached; caller discards the tx and continues
-- downloading — the node keeps going until the priority lane is full.

data AddResult2 : Set where
  Added    : Mempool2 → AddResult2
  Rejected : Mempool2 → AddResult2   -- invalid in this ledger state
  Blocked  : Mempool2 → AddResult2   -- lane full; mempool unchanged

addTx2 : Lane → Tx → Mempool2 → AddResult2

addTx2 Priority t m
  with fitsWith2 (prioCap m) (currentPrioSize m) (measure t)
... | false = Blocked m
... | true  with applyTx (ebLedger m) t
...   | nothing = Rejected m
...   | just ℓ′ =
        let n′ = suc (lastPrioTicket m)
            tk = mkTicket t n′ (measure t)
        in Added (mkMempool2
             (tip m) (currentEB m) (ebLedger m)
             (priorityTxs m ++ tk ∷ []) ℓ′ n′ (prioCap m)
             (regularTxs m) (ledger m) (lastRegTicket m) (regCap m))

addTx2 Regular t m
  with fitsWith2 (regCap m) (currentRegSize m) (measure t)
... | false = Blocked m
... | true  with applyTx (fastLedger m) t
...   | nothing = Rejected m
...   | just ℓ′ =
        let n′ = suc (lastRegTicket m)
            tk = mkTicket t n′ (measure t)
        in Added (mkMempool2
             (tip m) (currentEB m) (ebLedger m)
             (priorityTxs m) (fastLedger m) (lastPrioTicket m) (prioCap m)
             (regularTxs m ++ tk ∷ []) ℓ′ n′ (regCap m))

------------------------------------------------------------------------
-- 7. addEB — receive an EB from a peer
------------------------------------------------------------------------

-- An EB arrives like a transaction: downloaded from peers, not yet
-- on-chain.  Its transactions are re-applied (not fully validated) to
-- derive the new ebLedger.  Both lanes are then revalidated against the
-- new stack.  A stale EB (wrong tip) or an internally inconsistent EB
-- (reapplyAll fails) is silently discarded.

addEB : EB → Mempool2 → Maybe Mempool2
addEB eb m with EB.ebTip eb ≟Tip tip m
... | false = nothing    -- stale: EB targets a different tip
... | true  with reapplyAll (ledgerAt (tip m)) (EB.ebTxs eb)
...   | nothing = nothing    -- EB's tx list fails re-application; discard
...   | just ℓ_eb =
          let ℓ_fast , prio′ = revalidate ℓ_eb   (priorityTxs m)
              ℓ_reg  , reg′  = revalidate ℓ_fast (regularTxs m)
          in just (mkMempool2
               (tip m) (just eb) ℓ_eb
               prio′ ℓ_fast (lastPrioTicket m) (prioCap m)
               reg′  ℓ_reg  (lastRegTicket m)  (regCap m))

------------------------------------------------------------------------
-- 8. syncWithLedger — tip change discards the current EB
------------------------------------------------------------------------

-- When the chain tip advances, the EB (which was built for the old tip)
-- is no longer valid and is discarded.  Both lanes are revalidated from
-- scratch against the new bare tip ledger state.

syncWithLedger2 : TipPoint → Mempool2 → Mempool2
syncWithLedger2 newTip m =
  let s₀             = ledgerAt newTip
      ℓ_fast , prio′ = revalidate s₀      (priorityTxs m)
      ℓ_reg  , reg′  = revalidate ℓ_fast (regularTxs m)
  in mkMempool2
       newTip nothing s₀
       prio′ ℓ_fast (lastPrioTicket m) (rbCapacityAt  newTip)
       reg′  ℓ_reg  (lastRegTicket m)  (regCapacityAt newTip)

------------------------------------------------------------------------
-- 9. removeTxsEvenIfValid — lane-aware
------------------------------------------------------------------------

-- Removing a priority tx changes fastLedger, so the regular lane must
-- also be revalidated (cascading).  Removing a regular tx only affects
-- the regular lane; the priority lane and fastLedger are unchanged.

removePrioTxs : List TxId → Mempool2 → Mempool2
removePrioTxs ids m =
  let kept0          = filter (λ tk → if inSet ids (txId (tx tk))
                                      then false else true)
                              (priorityTxs m)
      ℓ_fast , prio′ = revalidate (ebLedger m)  kept0
      ℓ_reg  , reg′  = revalidate ℓ_fast        (regularTxs m)
  in mkMempool2
       (tip m) (currentEB m) (ebLedger m)
       prio′ ℓ_fast (lastPrioTicket m) (prioCap m)
       reg′  ℓ_reg  (lastRegTicket m)  (regCap m)

removeRegTxs : List TxId → Mempool2 → Mempool2
removeRegTxs ids m =
  let kept0         = filter (λ tk → if inSet ids (txId (tx tk))
                                     then false else true)
                             (regularTxs m)
      ℓ_reg , reg′  = revalidate (fastLedger m) kept0
  in mkMempool2
       (tip m) (currentEB m) (ebLedger m)
       (priorityTxs m) (fastLedger m) (lastPrioTicket m) (prioCap m)
       reg′ ℓ_reg (lastRegTicket m) (regCap m)

------------------------------------------------------------------------
-- 10. Block forging
------------------------------------------------------------------------

-- Unspecified policy deciding whether to produce an EB or a Ranking Block (RB).
postulate shouldForgeEB : Mempool2 → Bool

-- EB content: any transactions from either lane, in any order.
-- TODO: no protocol-parameter-based size limit on EBs for now.
postulate ebSnapshot : Mempool2 → List Tx

-- Split a TxSeq at a capacity bound (mirrors snapshotTake from §11).
postulate splitAtCap : Capacity → TxSeq → TxSeq × TxSeq

-- Ranking Block (RB) content when not referencing an EB: priority txs up to prioCap.
rbSnapshot : Mempool2 → RBContent
rbSnapshot m =
  RBFromPrio (map tx (proj₁ (splitAtCap (prioCap m) (priorityTxs m))))

-- When referencing an existing EB the caller supplies the EBId directly;
-- we model only the tx-list path here.
postulate freshEBId : EBId

forgeBlock2 : Mempool2 → EB ⊎ RB
forgeBlock2 m =
  if shouldForgeEB m
  then inl (mkEB freshEBId (tip m) (ebSnapshot m))
  else inr (mkRB (tip m) (rbSnapshot m))
```

### Revalidation cascade

| Event | EB layer | Priority lane | Regular lane |
|---|---|---|---|
| `addEB` (new EB accepted) | reapply | revalidate | revalidate |
| priority tx added | — | extend | — |
| priority tx removed | — | revalidate | revalidate |
| regular tx added | — | — | extend |
| regular tx removed | — | — | revalidate |
| `syncWithLedger` (tip change) | discard | revalidate | revalidate |

### Notes on this sketch

- **`reapplyAll` vs `applyAll`:** EB txs use `reapplyAll` (cheaper,
  no script re-execution) because they were already fully validated
  when the EB was created. Newly submitted txs continue to use `applyTx`
  (full validation) on first admission.
- **Independent ticket counters:** `lastPrioTicket` and `lastRegTicket`
  are separate; a priority tx and a regular tx can both hold ticket
  number 1. Any snapshot API (e.g. `snapshotTxsAfter`) must therefore
  take a `Lane` argument.
- **Regular-lane Blocked vs Priority-lane Blocked:** both return
  `Blocked`, but the caller's reaction differs: a `Blocked` on the
  priority lane means *stop downloading*; a `Blocked` on the regular
  lane means *discard this tx and try the next one* — keep going until
  the priority lane is full.
- **EB capacity TODO:** the provisional use of `prioCap` (one Ranking
  Block's worth) as the EB size bound is a policy placeholder. The
  formal model isolates this in `ebSnapshot` so the bound can be
  changed without touching the state invariants or the admission logic.

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