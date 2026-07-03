# Cardano Mempool — Linear Leios Adaptation

*A design sketch for adapting the Praos mempool to Linear Leios
(CIP-164). Single lane, no priority/regular distinction; that split is
a further extension described in `MempoolLeiosPricing.lagda.md`.*

**This is one of three sibling documents:**

1. **`Mempool.lagda.md`** — the current Praos-era mempool.
2. **`MempoolLeios.lagda.md`** *(this file)* — proposed Linear Leios
   adaptation, still a single lane, aligned with CIP-164.
3. **`MempoolLeiosPricing.lagda.md`** — tiered-pricing extension layered
   on top of this document. Adds priority/regular lanes.

**Last updated:** 2026-06-09
**Primary reference:** CIP-164 Ouroboros Linear Leios,
<https://github.com/cardano-foundation/CIPs/tree/master/CIP-0164>
**Secondary reference:** Linear Leios simulation README,
<https://github.com/input-output-hk/ouroboros-leios/blob/main/sim-rs/implementations/LINEAR_LEIOS.md>

## 0. What changes from Praos (executive summary)

The Praos mempool holds a single sequence of unconfirmed transactions
validated against the chain-tip ledger, and hands the slot leader a
snapshot at forge time. Linear Leios keeps that shape but layers three
new chain-side interactions on top:

1. **Endorser Blocks (EBs).** In any slot where a stake pool wins the
   Praos slot lottery, it may *optionally* also produce an EB alongside
   its Ranking Block (RB). EBs carry transactions that did not fit in
   the RB body. There is **no separate lottery for EBs** — EB
   production is opt-in and adaptive per slot, tightly coupled to RB
   leadership.
2. **Vote/certificate flow.** An EB announced by `RB` does not enter
   the ledger on its own. A stake-based voting committee validates the
   EB during a voting period. If enough votes accumulate, the EB is
   *certifiable*. A later ranking block `RB'` may include an aggregated
   certificate for the EB in place of its own transaction body;
   inclusion of that certificate is what makes the EB's transactions
   part of the on-chain ledger.
3. **A minimum inclusion delay.** A certificate for an EB announced by
   `RB` can only be included in a later `RB'` if `RB'` is at least
   `3·L_hdr + L_vote + L_diff` slots after `RB`. If the immediately-
   following RB is produced before that delay elapses, the EB is
   *discarded* — its certificate can never be included, and its
   transactions do not reach the ledger through Leios.

Consequences for the mempool:

- **Speculative post-EB ledger state.** When the mempool holds an EB
  (either self-produced and awaiting certification, or a peer's EB it
  is optimistic about), it maintains a prospective ledger `ebLedger`
  that equals `ledger + heldEB.txs`. Mempool transactions are
  validated on top of `ebLedger` when it exists — so the mempool is
  ready to move the tip forward at zero cost if the held EB ends up
  being certified into a later RB (Scenario B in §4).
- **New events:** `addEB` (peer announces an EB), `seeRBBody` (RB
  body with plain transactions lands on-chain), `seeRBCert` (RB body
  with an EB certificate lands on-chain), `discardEB` (explicit drop
  of a stale held EB).
- **Extended `forgeBlock`:** now returns an RB plus an optional
  overflow EB, instead of just an RB.
- **Extended `syncWithLedger`:** the new tip may have advanced by an
  EB's transactions if the new RB carried a certificate. `stillLive`
  gates whether a held EB survives the tip advance.
- **New optional cache:** a reuse-optimization set `S = seenEBs` of
  transactions already seen in some EB. Purely a work-avoidance
  cache; not required for correctness.
- **No two-lane / no priority classification.** All mempool
  transactions are treated identically. The CIP's implicit preference
  is "RBs first, EBs only for overflow" — modeled here as a forge-time
  partition, not as a mempool-time split. That's what
  `MempoolLeiosPricing.lagda.md` changes.

### Naming note

This document adopts a naming convention that differs from
`Mempool.lagda.md` (Praos), where `ledger` denotes the mempool's
final post-tx working state:

| Concept | Praos (`Mempool.lagda.md`) | Leios (this file) |
|---|---|---|
| chain tip ledger state | *implicit via `tip`* | `ledger` |
| held EB (if any) | — | `heldEB : Maybe EB` |
| tip + held EB applied | — | `ebLedger : Maybe LedgerState` |
| mempool working state | `ledger` | `updatedLedger` |

The convention was chosen so the pricing extension can add
`priorityUpdatedLedger` and `regularUpdatedLedger` in the natural
place, and so `ebLedger` shifts from `Maybe` (held or not) to a
clearly-parallel field name.

## 1. Block production

### 1a. One lottery, two possible outputs

The lottery is the standard Praos VRF slot-leader election. The winner
produces:

- **A Ranking Block (RB).** Same role as a Praos block: extends the
  chain, is what fork choice picks between.
- **An Endorser Block (EB), optionally.** Produced *only if* the
  leader has transactions that could not fit into the RB body. EBs
  must be non-empty (CIP-164: "Empty EBs should not be announced").

Every RB header may announce at most one EB. Every EB is announced by
exactly one RB.

### 1b. What an RB carries in its body

Each RB body is *either* a list of transactions *or* an aggregated
certificate for a previous EB. **Cert and txs are mutually exclusive**
— CIP-164: "When a certificate is included, no further transactions
are allowed in the RB". The Agda `RBBody` type in §6 makes this a sum,
so there is no valid RB whose body carries both.

- **RB with a tx body.** This is the Praos case, extended to allow
  announcing an EB in the RB header alongside the body.
- **RB with a certificate.** The RB body contains no transactions; it
  contains only the certificate for the previously-announced EB (see
  §2).

### 1c. What an EB carries

An EB carries a list of transactions. Its transactions must:

- Form a valid extension of the announcing RB — that is, be valid
  against `ledgerAt(announcingRB)` (the ledger state after applying the
  RB body).
- Be non-empty.
- Respect the EB-specific size and script budgets: `S_EB` (structure
  size, including tx reference bytes), `S_EB-tx` (total referenced tx
  size), and per-EB Plutus step and memory limits. These are
  deliberately per-EB (not per-tx) — a further Plutus workload allowance
  that RBs alone do not carry.

Each EB extends the announcing RB directly; there is no EB-to-EB
chaining.

## 2. EB transactions reach the ledger via a certificate

The full sequence of a Leios-mediated inclusion:

1. **Slot N.** RB `R` is produced. It announces EB `E`, which carries
   `k` transactions that did not fit into `R`'s body.
2. **Voting period.** During a fixed protocol window after slot `N`,
   the elected voting committee validates `E`'s transactions against
   `ledgerAt(R)`. Members who deem `E` valid cast votes.
3. **Certification.** Once votes representing stake above the threshold
   parameter `τ` have been collected, an aggregated certificate `C(E)`
   can be built. This certificate is a succinct proof that `E` was
   validly endorsed.
4. **Chain inclusion.** A later ranking block `R'` — where `R'` is at
   least `3·L_hdr + L_vote + L_diff` slots after `R` — may include
   `C(E)` in its body in place of `R'`'s own transaction body. When
   `R'` is adopted, `E`'s transactions become part of the ledger.
5. **Discard.** If the immediate next RB after `R` is produced before
   the minimum delay elapses, the EB's certificate cannot be included
   in that RB. The CIP treats this as *discarding* the EB — its
   transactions never reach the ledger through Leios.

The mempool does not participate in voting; that runs in a separate
consensus-layer component. The mempool only needs to react to the
chain-side outcomes: an EB was announced (`addEB`), an RB body with
plain transactions was adopted (`seeRBBody`), an RB body with a
certificate was adopted (`seeRBCert`), or a held EB became
unrecoverable (`discardEB`).

## 3. State

Conceptually the Leios mempool carries the same core state as the
Praos `InternalState` (`Impl/Common.hs:104`), extended with a held EB
and its prospective ledger:

- `tip : TipPoint` — current chain tip.
- `ledger : LedgerState` — `ledgerAt(tip)`; the ledger state of the
  current chain tip. Cached for convenience.
- `heldEB : Maybe EB` — the EB, if any, that the node currently holds.
  Populated by `addEB` when the node adopts a peer's EB, or after
  forging one of its own. Cleared by `discardEB`, by `syncWithLedger`
  when `stillLive` reports the certification window has closed, or by
  `seeRBCert` (in either the matching or non-matching branch).
- `ebLedger : Maybe LedgerState` — the prospective ledger state
  `ledger + heldEB.ebTxs` if `heldEB` is `just`, or `nothing` if
  `heldEB` is `nothing`. The two fields move together; `ebLedger` is a
  cache of the pre-application so that mempool-side validation of
  `txs` can happen against the post-EB state.
- `txs : TxSeq` — the sequence of validated transactions.
- `updatedLedger : LedgerState` — the mempool's working state,
  `fromMaybe ledger ebLedger + txs`. This is what new incoming
  transactions are validated against (mirroring the Praos incremental
  ledger).
- `lastTicket : TicketNo` — for streaming to peers, same role as
  Praos.
- `capacity : Capacity` — same as Praos.
- `seenEBs : SeenSet` — the reuse-optimization set from CIP-164.
  Contains transactions previously encountered inside some EB, so
  that `addTx` may skip full script re-execution when a match is hit.
  Correctness must hold with `seenEBs = ∅`; it is a pure
  optimization.

### Invariants

The state carries three related invariants:

- **`ebLedger` reflects `heldEB`:** `heldEB = nothing` implies
  `ebLedger = nothing`; `heldEB = just e` implies `ebLedger = just
  (ledger + e.ebTxs)`.
- **`ledger` reflects `tip`:** `ledger ≡ ledgerAt tip`.
- **`updatedLedger` reflects `txs`:** `fst (reapplyAllTk base txs) ≡
  updatedLedger`, where `base = fromMaybe ledger ebLedger`.

## 4. Events

### 4a. `addTx` — validate against the working state

Validation, capacity check, sequence append. Same shape as Praos
`addTx` (`Update.hs:67`). The transaction is validated against
`updatedLedger` (the cumulative mempool post-state), not against `ledger`
directly — so `t` sees prior mempool transactions *and*, if a held EB
is present, the effect of that EB.

The reuse optimization: if `t` is already in `seenEBs` with the
validation flag set for the current `ebLedger`, the script work has
already been done in an equivalent ledger context. An implementation
may skip re-execution and just run UTxO / structural checks. This does
not change observable behaviour; it only affects CPU cost.

### 4b. `addEB` — peer announces an EB

A peer announces an EB `E`. The mempool decides via `shouldHold`
whether to adopt `E` as the held EB. If it adopts:

1. Set `heldEB := just E`, `ebLedger := just (reapplyAll ledger
   E.ebTxs)`.
2. Revalidate `txs` against the new base (`fromMaybe ledger ebLedger`).
   Drop any tx that no longer applies (e.g., a tx that conflicts with
   `E.ebTxs`).
3. Rebuild `updatedLedger` from the survivors.

If it does not adopt (e.g., we already hold a different EB from the
same parent RB), only `seenEBs` is updated.

Cost: O(|E.ebTxs| + |txs|) on adoption.

### 4c. `discardEB` — explicit drop of a held EB

Clears the held EB and rebuilds. Used when an implementation-side
policy decides the held EB is unrecoverable before `syncWithLedger`
would otherwise catch it. Steps:

1. `heldEB := nothing`, `ebLedger := nothing`.
2. Revalidate `txs` against `ledger`. Drop any tx that depended on
   `E.ebTxs` for its validity.
3. Rebuild `updatedLedger` from the survivors.

Cost: O(|txs|).

### 4d. `seeRBBody` — RB with a tx body lands

The chain extends with an RB `R'` whose body is a list of
transactions `ts`, at new tip `p`. The mempool:

1. Advances the tip: `tip := p`, `ledger := ledgerAt p`.
2. Drops every mempool transaction whose `TxId` appears in `ts` (they
   are now on-chain).
3. Decides whether the held EB survives: `stillLive p` for the held
   `E`. If not, treats the held EB as discarded (steps as in §4c).
4. Rebuilds `ebLedger` from the new `ledger` and the (possibly
   cleared) `heldEB`.
5. Revalidates remaining `txs` against the new base. Cascading
   failures possible.

The eager drop in step 2 is the CIP's "When a node receives an RB
body, it immediately removes all referenced/conflicting transactions
from its mempool."

Cost: O(|txs| + |ts|).

### 4e. `seeRBCert` — RB with an EB certificate lands

The chain extends with RB `R'` at new tip `p` whose body is a
certificate for a previously-announced EB `E`. When `R'` is adopted,
`E`'s transactions are applied to the ledger, so `ledgerAt p = old
ledger + E.ebTxs`. Two sub-cases:

**Scenario B: `E` matches our `heldEB`.** By the state invariant, old
`ebLedger.value = old ledger + heldEB.ebTxs = ledgerAt p`. The
mempool has been pre-validating against exactly this state, so:

1. `tip := p`.
2. `ledger := old ebLedger.value` (bit-identical to `ledgerAt p`).
3. `heldEB := nothing`, `ebLedger := nothing`.
4. `updatedLedger := old updatedLedger` (unchanged).
5. `txs := old txs` (unchanged; by the disjointness argument in §5,
   no tx in `txs` overlaps with `heldEB.ebTxs`).

Cost: O(1). No `applyTx` / `reapplyAll` calls. The spec describes
this as "revalidation against old `ebLedger`", but every step of that
revalidation succeeds bit-identically to the input; an implementation
may skip it entirely.

**Scenario A: `E` does not match our `heldEB`.** Some other EB was
certified. Our held EB is discarded (either it was never going to be
certified, or the timing is wrong now). This document specifies the
**phase-1** behaviour: unconditional pruning of `E.ebTxs` from `txs`
followed by full revalidation. A future phase may explore partial
revalidation (e.g., skip when we can prove `E.ebTxs` is input-
disjoint from the held EB and from `txs`), but the canonical
invariant is full revalidation.

1. `tip := p`, `ledger := ledgerAt p`.
2. `heldEB := nothing`, `ebLedger := nothing`.
3. Drop txs in `E.ebTxs` from the sequence (they are now on-chain).
4. Revalidate remaining `txs` against `ledger`. Cascade drops
   possible.
5. Rebuild `updatedLedger`.

Cost: O(|txs| + |E.ebTxs|).

### 4f. `syncWithLedger` — generic tip advance

Fallback for tip changes that are not covered by an explicit
`seeRBBody` / `seeRBCert` (e.g., during initial catchup or a multi-
block reorg). Rebuilds the state from `ledgerAt(newTip)`:

1. `tip := p`, `ledger := ledgerAt p`.
2. If `heldEB` present and `stillLive p heldEB` is `false`, clear it.
   Otherwise recompute `ebLedger` against the new `ledger`.
3. Revalidate `txs` against the new base and rebuild `updatedLedger`.

Cost: O(|txs| + |heldEB.ebTxs|).

### 4g. `forgeBlock`

```text
forgeBlock : MempoolL → (RB, Maybe EB)
```

`forgeBlock` internally reapplies mempool contents against `ledger`
before splitting into RB and EB bodies. This is what makes it safe
to call regardless of whether `heldEB` is present: the emitted RB
body will apply on-chain against `ledger` (not `baseLedger`), so any
tx that speculatively depended on `heldEB.ebTxs` must be dropped
from the output. The mempool *state* is unchanged by this filter —
such txs remain in `txs`, still valid under `baseLedger`, and
become forgeable in a later slot once `heldEB` is resolved (either
certified via `seeRBCert` Scenario B — after which they are valid
under `ledger` too — or discarded via `discardEB` / a stale-`heldEB`
`syncWithLedger` — after which they either survive the revalidation
against `ledger` or are dropped from the mempool).

Procedure:

1. Revalidate `txs` against `ledger` (the chain-tip state, without
   `heldEB`). Any tx dependent on `heldEB.ebTxs` drops out here.
   Call the survivors `validTxs`.
2. Split `validTxs` by RB capacity: prefix `rbTxs`, tail
   `overflow0`.
3. Compute `rbLedger = ledger + rbTxs` (this is `ledgerAt(newRB)`).
   Emit RB `R` with body `rbTxs`.
4. Revalidate `overflow0` against `rbLedger` and drop failures.
   Filter what remains by EB per-tx capacity (`S_EB-tx`, per-EB
   Plutus). Call the survivors `overflow'`.
5. If `overflow'` is non-empty, emit EB `E` with body `overflow'`,
   announced by `R`. Otherwise emit `R` with no announced EB.

Cost: 2 × O(|txs|) reapplications at forge time — one against
`ledger`, one against `rbLedger`.

The certificate-inclusion path (RB body is a certificate, no EB
announced) is a separate entry point driven by the consensus layer;
not modeled in this sketch.

### 4h. Revalidation cascade summary

| Event | `heldEB` / `ebLedger` | `txs` / `updatedLedger` | Cost |
|---|---|---|---|
| `addTx t` | — | validate `t`, extend | O(1) |
| `addEB e` (adopt) | set `heldEB`, rebuild `ebLedger` | revalidate against new base | O(\|txs\| + \|e.ebTxs\|) |
| `discardEB` | clear | revalidate against `ledger` | O(\|txs\|) |
| `seeRBBody ts p` | maybe discard via `stillLive` | drop referenced + revalidate | O(\|txs\| + \|ts\|) |
| `seeRBCert e p` **(match)** | clear | **bit-identical rename** | **O(1)** |
| `seeRBCert e p` **(no match)** | discard | drop `e.ebTxs` + revalidate | O(\|txs\| + \|e.ebTxs\|) |
| `syncWithLedger p` | maybe discard via `stillLive` | revalidate | O(\|txs\| + \|heldEB.ebTxs\|) |

## 5. Constraints from CIP-164 and disjointness lemma

Constraints the model must respect (all from CIP-164):

- **Cert/tx exclusivity.** An RB body is either a certificate or a
  list of transactions, never both.
- **EB is non-empty.** Never announce an EB with zero transactions.
- **EB validates against announcing RB.** Every tx in an EB must
  be valid against `ledgerAt(announcingRB)`. Enforced by the forge-
  time revalidation in §4g step 3.
- **Per-EB size and Plutus budgets.** `S_EB`, `S_EB-tx`, per-EB Plutus
  step and memory limits. Distinct from per-RB limits.
- **Minimum inclusion delay.** A certificate for `E` announced by
  `R` may only be included in `R'` when `R'` is at least
  `3·L_hdr + L_vote + L_diff` slots after `R`. Below the delay, `E` is
  discarded. Captured abstractly as `stillLive` here.
- **One EB per announcing RB.** Fixed by RB header format.
- **Same-pool announcement.** "EBs are produced by the same stake
  pool that created the corresponding announcing RB." Enforced at
  block-production time, not by the mempool.
- **No EB-to-EB chaining.** Each EB extends its announcing RB, not
  another EB.

### Disjointness lemma (used by Scenario B)

**Claim.** In every reachable state with `heldEB = just E`, the txs
in `txs` and the txs in `E.ebTxs` are disjoint by `TxId`.

**Argument.** By induction on the state transitions that can set
`heldEB` to `just E`:

- `addEB E` (adoption): revalidates `txs` against the new base
  `ledger + E.ebTxs`. Any pre-existing tx in `txs` that duplicated a
  tx in `E.ebTxs` would fail as double-spend and be dropped. New
  incoming txs (via `addTx`) then validate against `updatedLedger`,
  which has `E.ebTxs` applied, so they cannot re-introduce a
  duplicate.
- Own-forge adoption (the mempool sets `heldEB` to its own EB after
  the announcing RB is adopted): the announcing RB drops the RB-body
  txs from `txs` (via `seeRBBody`), so at the moment of adoption the
  mempool's `txs` no longer contains the RB body. The EB body is the
  overflow of that same forge: by the block-production invariant it
  is disjoint from the RB body. After adoption, `addTx` cannot admit
  a duplicate of an EB tx because validation is against
  `updatedLedger`, which has the EB applied.

The lemma is what makes Scenario B a no-cost rename: no tx in `txs`
needs to be dropped when `E`'s txs land on-chain via cert.

## 6. Sketch Agda formalisation

Self-contained; only `Agda.Builtin.*` / `Agda.Primitive`. Comments
mark the moving parts.

```agda
open import Agda.Primitive        using (Level; lzero; lsuc)
open import Agda.Builtin.Bool     using (Bool; true; false)
open import Agda.Builtin.List     using (List; []; _∷_)
open import Agda.Builtin.Maybe    using (Maybe; just; nothing)
open import Agda.Builtin.Nat
  using (zero; suc; _+_) renaming (Nat to ℕ)
open import Agda.Builtin.Equality using (_≡_; refl)

_∧_ : Bool → Bool → Bool
true  ∧ b = b
false ∧ _ = false

if_then_else_ : {A : Set} → Bool → A → A → A
if true  then t else _ = t
if false then _ else e = e

infixr 4 _++_
_++_ : {A : Set} → List A → List A → List A
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

map : {A B : Set} → (A → B) → List A → List B
map f []       = []
map f (x ∷ xs) = f x ∷ map f xs

filter : {A : Set} → (A → Bool) → List A → List A
filter p []       = []
filter p (x ∷ xs) = if p x then x ∷ filter p xs else filter p xs

record _×_ (A B : Set) : Set where
  constructor _,_
  field
    fst : A
    snd : B
open _×_
infixr 4 _,_

case_of_ : {ℓ₁ ℓ₂ : Level} {A : Set ℓ₁} {B : Set ℓ₂}
         → A → (A → B) → B
case x of f = f x

fromMaybe : {A : Set} → A → Maybe A → A
fromMaybe d nothing  = d
fromMaybe _ (just x) = x

module MempoolLeios where

  ----------------------------------------------------------------------
  -- 1. Postulated primitives.
  ----------------------------------------------------------------------

  postulate
    Tx           : Set
    TxId         : Set
    LedgerState  : Set
    TipPoint     : Set
    TicketNo     : Set
    Capacity     : Set
    EBId         : Set

    txId         : Tx → TxId
    _≟TxId_      : TxId → TxId → Bool
    inTxIds      : List TxId → TxId → Bool

    applyTx      : LedgerState → Tx → Maybe LedgerState
    reapplyAll   : LedgerState → List Tx → LedgerState × List Tx
    ledgerAt     : TipPoint → LedgerState

    measure      : Tx → Capacity
    capacityAt   : TipPoint → Capacity
    fitsWith     : Capacity → Capacity → Capacity → Bool
    ebCap        : TipPoint → Capacity
    ebFits       : Capacity → Bool
    freshTicket  : TicketNo → TicketNo
    freshEBId    : EBId → EBId

  ----------------------------------------------------------------------
  -- 2. Endorser Blocks and Ranking Blocks
  ----------------------------------------------------------------------

  record EB : Set where
    constructor mkEB
    field
      ebId   : EBId
      ebTip  : TipPoint
      ebTxs  : List Tx
  open EB

  -- Cert/tx exclusivity from CIP-164 is captured by the choice being a
  -- sum: only one variant may be present.
  data RBBody : Set where
    RBTxs  : List Tx → RBBody
    RBCert : EBId    → RBBody

  record RB : Set where
    constructor mkRB
    field
      rbTip   : TipPoint
      rbBody  : RBBody
      rbAnnEB : Maybe EBId
  open RB

  postulate
    _≟EBId_ : EBId → EBId → Bool

  ----------------------------------------------------------------------
  -- 3. Ticket record and TxSeq
  ----------------------------------------------------------------------

  record TxTicket : Set where
    constructor mkTicket
    field
      tx       : Tx
      ticket   : TicketNo
      sizeTx   : Capacity
  open TxTicket

  TxSeq : Set
  TxSeq = List TxTicket

  reapplyAllTk : LedgerState → TxSeq → LedgerState × TxSeq
  reapplyAllTk ℓ tks =
    let ls , plain = reapplyAll ℓ (map tx tks)
        rebuild : List Tx → TxSeq → TxSeq
        rebuild [] _ = []
        rebuild (t ∷ ts) [] = []
        rebuild (t ∷ ts) (tk ∷ tks) =
          if _≟TxId_ (txId t) (txId (tx tk))
          then tk ∷ rebuild ts tks
          else rebuild (t ∷ ts) tks
    in ls , rebuild plain tks

  postulate
    seqSize : TxSeq → Capacity

  ----------------------------------------------------------------------
  -- 4. Reuse cache
  ----------------------------------------------------------------------

  postulate
    SeenSet    : Set
    emptySeen  : SeenSet
    seenAddEB  : SeenSet → List Tx → SeenSet
    seenClear  : SeenSet → SeenSet

  ----------------------------------------------------------------------
  -- 5. The mempool state
  --
  --   ledger        : chain tip's ledger, = ledgerAt tip
  --   heldEB        : the EB we are speculatively pre-applying, or none
  --   ebLedger      : ledger + heldEB.ebTxs if heldEB is just, else none
  --   txs           : validated mempool sequence
  --   updatedLedger : (fromMaybe ledger ebLedger) + txs
  ----------------------------------------------------------------------

  record MempoolL : Set where
    constructor mkMempoolL
    field
      tip           : TipPoint
      ledger        : LedgerState
      heldEB        : Maybe EB
      ebLedger      : Maybe LedgerState
      txs           : TxSeq
      updatedLedger : LedgerState
      lastTicket    : TicketNo
      capacity      : Capacity
      seenEBs       : SeenSet
  open MempoolL

  -- Convenience: the "base" ledger against which txs is validated.
  baseLedger : MempoolL → LedgerState
  baseLedger m = fromMaybe (ledger m) (ebLedger m)

  ----------------------------------------------------------------------
  -- 6. Invariants
  ----------------------------------------------------------------------

  postulate
    LedgerAtTip :
      (m : MempoolL) →
      ledger m ≡ ledgerAt (tip m)

    EBLedgerConsistent :
      (m : MempoolL) →
      case heldEB m of λ where
        nothing  → ebLedger m ≡ nothing
        (just e) → ebLedger m ≡
                   just (fst (reapplyAll (ledger m) (ebTxs e)))

    TxsValid :
      (m : MempoolL) →
      fst (reapplyAllTk (baseLedger m) (txs m)) ≡ updatedLedger m

  ----------------------------------------------------------------------
  -- 7. addTx (validates against updatedLedger)
  ----------------------------------------------------------------------

  data AddResult : Set where
    Added    : MempoolL → AddResult
    Rejected : MempoolL → AddResult
    Blocked  : MempoolL → AddResult

  addTx : Tx → MempoolL → AddResult
  addTx t m
    with fitsWith (capacity m) (seqSize (txs m)) (measure t)
  ... | false = Blocked m
  ... | true  with applyTx (updatedLedger m) t
  ...   | nothing  = Rejected m
  ...   | just ℓ′ =
          let n′ = freshTicket (lastTicket m)
              tk = mkTicket t n′ (measure t)
          in Added (mkMempoolL
               (tip m) (ledger m) (heldEB m) (ebLedger m)
               (txs m ++ tk ∷ []) ℓ′ n′ (capacity m) (seenEBs m))

  ----------------------------------------------------------------------
  -- 8. addEB — peer announces an EB
  --
  -- If shouldHold, adopt: recompute ebLedger and revalidate txs on
  -- the new base.  If not, only update seenEBs.
  ----------------------------------------------------------------------

  postulate
    shouldHold : MempoolL → EB → Bool

  addEB : EB → MempoolL → MempoolL
  addEB e m =
    if shouldHold m e
    then
      let ebL′        = fst (reapplyAll (ledger m) (ebTxs e))
          ℓ′ , txs′   = reapplyAllTk ebL′ (txs m)
      in mkMempoolL
           (tip m) (ledger m) (just e) (just ebL′)
           txs′ ℓ′ (lastTicket m) (capacity m)
           (seenAddEB (seenEBs m) (ebTxs e))
    else
      mkMempoolL
        (tip m) (ledger m) (heldEB m) (ebLedger m)
        (txs m) (updatedLedger m) (lastTicket m) (capacity m)
        (seenAddEB (seenEBs m) (ebTxs e))

  ----------------------------------------------------------------------
  -- 9. discardEB — explicit drop of the held EB
  ----------------------------------------------------------------------

  discardEB : MempoolL → MempoolL
  discardEB m =
    let ℓ′ , txs′ = reapplyAllTk (ledger m) (txs m)
    in mkMempoolL
         (tip m) (ledger m) nothing nothing
         txs′ ℓ′ (lastTicket m) (capacity m) (seenEBs m)

  ----------------------------------------------------------------------
  -- 10. seeRBBody — RB with a tx body lands
  ----------------------------------------------------------------------

  postulate
    stillLive : TipPoint → EB → Bool

  seeRBBody : List Tx → TipPoint → MempoolL → MempoolL
  seeRBBody rbTxs p m =
    let ids   = map txId rbTxs
        kept  = filter (λ tk → if inTxIds ids (txId (tx tk))
                                then false else true) (txs m)
        ledger′ = ledgerAt p
        held′ = case heldEB m of λ where
                  nothing  → nothing
                  (just e) → if stillLive p e then just e else nothing
        ebL′ = case held′ of λ where
                  nothing  → nothing
                  (just e) → just (fst (reapplyAll ledger′ (ebTxs e)))
        base′        = fromMaybe ledger′ ebL′
        ℓ′ , txs′    = reapplyAllTk base′ kept
    in mkMempoolL
         p ledger′ held′ ebL′
         txs′ ℓ′ (lastTicket m) (capacityAt p)
         (seenClear (seenEBs m))

  ----------------------------------------------------------------------
  -- 11. seeRBCert — RB with an EB certificate lands
  --
  --   Scenario B (cert matches heldEB): bit-identical rename.
  --   Scenario A (cert names a different EB): full revalidation.
  ----------------------------------------------------------------------

  seeRBCert : EB → TipPoint → MempoolL → MempoolL
  seeRBCert e p m =
    let matches =
          case heldEB m of λ where
            nothing  → false
            (just h) → _≟EBId_ (ebId h) (ebId e)
    in if matches
       then
         -- Scenario B: no ledger op needed.  By the state invariant,
         -- old ebLedger.value ≡ ledgerAt p.  ledger, ebLedger, heldEB
         -- reshuffle; txs / updatedLedger are unchanged.
         mkMempoolL
           p (fromMaybe (ledger m) (ebLedger m)) nothing nothing
           (txs m) (updatedLedger m) (lastTicket m) (capacityAt p)
           (seenClear (seenEBs m))
       else
         -- Scenario A: e's txs are now on-chain; drop them; discard
         -- our heldEB (some other EB was certified in this RB, ours
         -- won't be).
         let ids   = map txId (ebTxs e)
             kept  = filter (λ tk → if inTxIds ids (txId (tx tk))
                                     then false else true) (txs m)
             ledger′     = ledgerAt p
             ℓ′ , txs′   = reapplyAllTk ledger′ kept
         in mkMempoolL
              p ledger′ nothing nothing
              txs′ ℓ′ (lastTicket m) (capacityAt p)
              (seenClear (seenEBs m))

  ----------------------------------------------------------------------
  -- 12. syncWithLedger — generic tip advance
  ----------------------------------------------------------------------

  syncWithLedger : TipPoint → MempoolL → MempoolL
  syncWithLedger p m =
    let ledger′ = ledgerAt p
        held′ = case heldEB m of λ where
                  nothing  → nothing
                  (just e) → if stillLive p e then just e else nothing
        ebL′ = case held′ of λ where
                  nothing  → nothing
                  (just e) → just (fst (reapplyAll ledger′ (ebTxs e)))
        base′        = fromMaybe ledger′ ebL′
        ℓ′ , txs′    = reapplyAllTk base′ (txs m)
    in mkMempoolL
         p ledger′ held′ ebL′
         txs′ ℓ′ (lastTicket m) (capacityAt p)
         (seenClear (seenEBs m))

  ----------------------------------------------------------------------
  -- 13. Block forging
  ----------------------------------------------------------------------

  postulate
    splitAtCap  : Capacity → TxSeq → TxSeq × TxSeq
    nonEmpty    : TxSeq → Bool
    ebNonEmpty  : List Tx → Bool

  -- Safe to call regardless of `heldEB`.  See §4g for the argument.
  -- The mempool state is unchanged; the two reapplyAllTk calls
  -- produce the emitted block only.
  forgeBlock : MempoolL → RB × Maybe EB
  forgeBlock m =
    let -- 1. Revalidate mempool contents against `ledger` (not
        --    `baseLedger`).  Drops any tx that depended on heldEB.ebTxs.
        _ , validTxs         = reapplyAllTk (ledger m) (txs m)
        -- 2. Split by RB capacity.
        rbTxs , overflow0    = splitAtCap (capacity m) validTxs
        -- 3. rbLedger = ledgerAt(newRB) = ledger + rbTxs.
        rbLedger , _         = reapplyAllTk (ledger m) rbTxs
        -- 4. Revalidate overflow against post-RB state.
        _ , ebOverflow       = reapplyAllTk rbLedger overflow0
        overflow′            = filter (λ tk → ebFits (measure (tx tk))) ebOverflow
        anyOverflow          = nonEmpty overflow′
        newEBId              = freshEBId (freshTicket (lastTicket m))
        maybeEB              = if anyOverflow
                                then just (mkEB newEBId (tip m)
                                                 (map tx overflow′))
                                else nothing
        rbAnn                = case maybeEB of λ where
                                 nothing  → nothing
                                 (just e) → just (ebId e)
        rb                   = mkRB (tip m) (RBTxs (map tx rbTxs)) rbAnn
    in rb , maybeEB
```

### Notes on this sketch

- **Postulates.** All ledger primitives, all policy predicates
  (`shouldHold`, `stillLive`, `ebFits`, `ebCap`), and the reuse cache
  abstract type. The model verifies structural wiring, not ledger
  correctness.
- **Scenario B in code.** `seeRBCert` when `matches = true` does not
  call `reapplyAll` at all — it only rebinds fields. The disjointness
  lemma (§5) is what justifies the unchanged `txs`.
- **What is not modeled.** The vote/certificate construction; the RB
  producer's decision to include an available certificate; the exact
  `stillLive` clock; reorgs / rollbacks.

## 7. Open questions

1. **Held-EB selection policy.** When two peers announce different EBs
   from the same parent RB, which does the local node hold? The CIP
   allows at most one valid EB per slot to be certified; local policy
   picks the one to help propagate. Modeled abstractly as `shouldHold`.
2. **`seenEBs` invalidation.** How aggressively should the reuse cache
   be trimmed on tip changes? Full clear is safe; entry-by-entry
   invalidation is a possible optimization.
3. **Own-EB adoption.** When the local slot leader forges an EB, when
   exactly does it set `heldEB` to that EB — at forge time, or after
   the announcing RB is adopted? The disjointness argument in §5
   assumes the latter.
4. **Interaction with the pricing extension.** The Leios mempool here
   has one tx sequence; the pricing extension adds a second lane.
   `MempoolLeiosPricing.lagda.md` retains `ledger` + `ebLedger` in
   the same shape and adds `priorityUpdatedLedger` and
   `regularUpdatedLedger` where this file has `updatedLedger`.

## Changelog

- **2026-06-09** — Initial version. Extracted the Linear Leios
  adaptation from the earlier `Mempool.lagda.md` §12 and rewrote it
  as a self-contained single-lane spec aligned with CIP-164.
- **2026-06-09 (later)** — Added `ebLedger : Maybe LedgerState` and
  the disjointness lemma so that `seeRBCert` in the matching-cert
  case is a zero-cost rename (Scenario B). Reworked the state record
  to expose the chain tip's ledger explicitly (`ledger`) and renamed
  the post-tx working state to `updatedLedger`. Added `discardEB` as
  an explicit handler and the revalidation-cascade summary table.
- **2026-06-09 (later still)** — Fixed `forgeBlock` for the
  heldEB-at-forge case: the handler now reapplies `txs` against
  `ledger` (not `baseLedger`) before splitting into RB body, and
  reapplies the overflow against `ledger + rbTxs` for the announced
  EB body. Dropped the earlier phase-1 "heldEB = nothing"
  precondition. Cost at forge: 2 × O(|txs|). Mempool state is
  unchanged by the call; txs that fail the ledger revalidation
  remain in `txs` (still valid under `baseLedger`) and become
  forgeable once `heldEB` is resolved.
