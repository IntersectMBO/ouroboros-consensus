# Cardano Mempool — Linear Leios with Tiered Pricing

*A design sketch for adding tiered (priority / regular) pricing to the
Linear Leios mempool. Two lanes: priority-lane transactions are
destined for a Ranking Block body, regular-lane transactions for the
overflow Endorser Block. Self-contained; every place where behaviour
differs from `MempoolLeios.lagda.md` is called out in a comment
prefixed **`-- CHG:`** (change relative to Leios) or **`-- NEW:`**
(new in this document).*

**This is one of three sibling documents:**

1. **`Mempool.lagda.md`** — the current Praos-era mempool.
2. **`MempoolLeios.lagda.md`** — proposed Linear Leios adaptation
   (single lane, CIP-164-aligned).
3. **`MempoolLeiosPricing.lagda.md`** *(this file)* — tiered-pricing
   extension layered on top of the Leios mempool.

**Last updated:** 2026-06-09
**Primary reference:** CIP-164 Ouroboros Linear Leios,
<https://github.com/cardano-foundation/CIPs/tree/master/CIP-0164>
**Sibling ref:** `MempoolLeios.lagda.md` in this directory (shared
context on Leios EB/RB semantics; not a build-time dependency).

## 0. What changes from `MempoolLeios` (executive summary)

The Leios mempool holds a single sequence of transactions validated
against `updatedLedger = (ledger + heldEB.txs) + txs`. The tiered-
pricing mempool splits that sequence into two lanes:

- **Priority lane** (`priorityTxs`) — transactions that pay the higher
  tier and are guaranteed a place in an RB body if room exists at the
  next forging opportunity.
- **Regular lane** (`regularTxs`) — transactions that pay the lower
  tier and are eligible only for the overflow EB.

This split is a *mempool-side* extension. **CIP-164 does not define
any priority / regular distinction**; it only expresses an implicit
preference ("EBs should only be announced if a transaction cannot be
included in the base RB… the protocol will naturally incentivize
usage of RBs over EBs"). This document commits to a stronger,
explicit contract: the lane a transaction lives in determines *which
kind of block* it can end up in, and validation is arranged so that
the priority lane sees a ledger state that already accounts for the
EB the mempool currently holds.

### Structural differences vs. `MempoolLeios`

| Concept | `MempoolLeios` | This file |
|---|---|---|
| chain tip cache | `ledger` | `ledger` (unchanged) |
| held EB | `heldEB : Maybe EB` | `heldEB : Maybe EB` (unchanged) |
| tip + held EB applied | `ebLedger : Maybe LedgerState` | `ebLedger : Maybe LedgerState` (unchanged) |
| mempool working state | `updatedLedger` | *split into two:* `priorityUpdatedLedger`, `regularUpdatedLedger` |
| tx sequence | `txs` | *split into two:* `priorityTxs`, `regularTxs` |
| capacity | `capacity` | *split into two:* `prioCap`, `regCap` |
| ticket counter | `lastTicket` | *split into two:* `lastPrioTicket`, `lastRegTicket` |
| reuse cache | `seenEBs` | `seenEBs` (unchanged) |

So the *ledger stack* (`ledger`, `heldEB`, `ebLedger`) is imported
without change, and everything below it — the working state, the tx
sequence, the capacity, and the tickets — is doubled. That layout is
what makes the pricing extension a genuine extension rather than an
architectural break.

### Behavioural differences vs. `MempoolLeios`

- **Admission (`addTx`).** Now takes a `Lane` argument. Priority
  admissions cascade: any change to `priorityUpdatedLedger` triggers
  regular-lane revalidation (see §1 for the CIP-mirror argument).
- **EB acceptance (`addEB`).** Same as Leios in spirit — recompute
  `ebLedger`, revalidate — but now the revalidation runs through both
  lanes in sequence.
- **Chain events (`seeRBBody`, `seeRBCert`, `syncWithLedger`).**
  Same shape as Leios. `seeRBCert`'s **Scenario B** (cert matches
  our held EB) is still a bit-identical rename; the two lanes'
  working states are preserved as-is, cost O(1).
- **Discard (`discardEB`).** Same event as Leios; cascades through
  both lanes.
- **Block forging (`forgeBlock`).** Priority lane → RB body;
  regular lane → overflow EB body. The split is not a forge-time
  partition as it is in Leios; the lanes are stored separately by
  design.

## 1. Design summary

**Priority lane.** Transactions submitted with the higher tier. Each
priority tx is validated against `priorityUpdatedLedger = (ledger +
heldEB.txs) + all prior priority txs`, so it sees the cumulative
effect of the priorities already admitted plus the EB currently held.
`prioCap` is the total `TxMeasure` of a single Ranking Block, taken
from protocol parameters.

**Regular lane.** Transactions submitted with the lower tier. Each
regular tx is validated against `regularUpdatedLedger =
priorityUpdatedLedger + all prior regular txs` — the cumulative
regular-lane post-state. `regCap` is a separate EB capacity derived
from the CIP's per-EB caps (`S_EB`, `S_EB-tx`, per-EB Plutus limits).

**Held EB.** The node keeps at most one EB (`heldEB`) — either its
own recently-forged EB awaiting a certificate window, or a peer's
announced EB whose eventual certification is worth pre-validating
for. When an EB is held, `ebLedger = just (ledger + heldEB.ebTxs)`;
otherwise `ebLedger = nothing`.

**Application order.** Chain semantics fix a single canonical
application order — `ledgerAt(oldTip) + certified EB (if any) + RB
body`, and if a later RB's certificate applies our held EB, its
transactions land before any RB body priorities from that later RB.
This mempool mirrors that order in its layered ledger states so that
every stored transaction is valid against the exact state it will
meet on-chain.

### Capacity rules

- **Ranking Block / priority-lane limit.** One block's `TxMeasure`
  from protocol parameters: byte size, script ExUnits mem, script
  ExUnits CPU, reference-script bytes.
- **EB / regular-lane limit.** CIP-164's per-EB caps: `S_EB`
  (structure), `S_EB-tx` (referenced txs), per-EB Plutus step and
  memory. These are distinct dimensions from the RB caps.

### Block production

The lottery is the standard Praos VRF slot-leader election — a single
lottery, not one per block kind. Its winner produces:

- **An RB.** Body is either `priorityTxs` (a plain-tx body) or a
  certificate for a previously-announced EB. These are mutually
  exclusive (CIP-164: "when a certificate is included, no further
  transactions are allowed in the RB").
- **An EB, optionally.** Body is drawn from `regularTxs`, plus any
  priority-lane overflow that did not fit within `prioCap` in the
  RB body. Announced in the RB header. Must be non-empty (CIP-164:
  "empty EBs should not be announced").

**Priority-fee refund when a priority tx lands in an EB.** A
priority-tier transaction that ends up in an EB body — rather than
in the RB body — has paid the higher priority-lane fee but has
*not* received priority service (which the pricing model defines as
direct RB inclusion at its announcing slot; EB inclusion is subject
to the vote/certificate flow and the minimum inclusion delay). In
that case the tx receives a refund equal to the priority-vs-regular
fee differential, credited by the ledger rule that applies the EB
body. The mempool itself does not compute the refund; the ledger
does, using the tx's lane tag carried in the tx body (see the
pricing spec in the Cardano ledger repo for the exact refund rule).
`forgeBlock` need only preserve the lane tag on each emitted tx so
the ledger rule can identify EB-landed priority txs at application
time.

An announced EB does not affect the ledger by itself. It goes through
the CIP-164 vote/certificate flow: the elected voting committee
validates it against `ledgerAt(announcingRB)`, votes are aggregated,
and a *later* RB `R'` — at least `3·L_hdr + L_vote + L_diff` slots
after the announcing RB — may include the certificate in its body.
Only when `R'` is adopted do the EB's (regular-lane) transactions
become on-chain.

If the immediate next RB after the announcing RB is produced before
the minimum delay elapses, CIP-164 discards the EB — its
transactions never reach the ledger through Leios. From the
mempool's perspective, `syncWithLedger` handles this by clearing
`heldEB` when the local `stillLive` predicate reports the
certification window has closed.

**`forgeBlock` with a held EB.** `forgeBlock` in §2 reapplies each
lane against `ledger` (not `baseLedger`) before splitting into RB
and EB bodies, so it is safe to call regardless of `heldEB`. The
emitted RB body applies on-chain against `ledger`, and the reapply
step drops any priority tx that speculatively depended on
`heldEB.ebTxs`. The mempool *state* is unchanged; such txs stay in
`priorityTxs`, still valid under `baseLedger`, and become forgeable
once `heldEB` is resolved (either certified via Scenario B —
after which they are valid under the new `ledger` too — or
discarded, after which they either survive the cascade or drop out).
The regular lane is handled symmetrically: its contents are
reapplied against the post-RB state `ledgerAt(newRB) = ledger +
rbTxs`. Cost: 2 × O(|priorityTxs|) + O(|regularTxs|) at forge time.

### Considered variant: EB suppression under light load

A variant under consideration would gate EB emission on mempool
fullness. `forgeBlock` would emit **no EB** (`maybeEB = nothing`,
`rbAnnEB = nothing`) whenever the combined mempool contents
(priority + regular) sit at or below **half the RB capacity in
every dimension** — i.e., for every dimension `d ∈ {byte size,
ExUnits mem, ExUnits CPU, ref-script bytes}`:

```
seqSize (priorityTxs ++ regularTxs) [d]  ≤  prioCap [d] / 2
```

Rationale: EBs carry real costs (a voting round, a certification
delay, and additional propagation load), so they only earn their
keep when there is genuinely more than one RB's worth of pressure
in at least one dimension. Under this threshold, all txs comfortably
fit the RB body and announcing an EB would either be near-empty
(colliding with CIP-164's "empty EBs should not be announced" rule)
or duplicate what the RB already carries.

Effect on the current spec if adopted: one additional guard in the
`anyEB` computation of `forgeBlock`, no state or invariant changes.
The threshold value (`/ 2`) is provisional — a further tuning
question is whether to make it protocol-parameterised.

### Peer transaction exchange (network side)

The bulk of these docs describes the mempool's local state machine.
The tx-submission mini-protocol — how transactions cross the wire
between peers — lives in a separate layer and is only sketched here.
The pricing mempool imposes two lane-aware requirements on that
layer:

- **Inbound routing by lane tag.** Each transaction on the wire
  carries a lane tag (either explicitly in the tx-submission frame
  or implicitly via a tx-body annotation such as the tier-selecting
  fee bid). The receiving node reads the tag and dispatches to
  `addTx Priority` or `addTx Regular` accordingly. Without lane
  awareness on the wire, a priority-tier tx received from a peer
  could end up in the wrong lane and lose priority service; the
  pricing model is unenforceable end-to-end.

- **Outbound: priority-first, with fallback.** The local node's
  tx-fetch policy toward each peer is: keep asking for
  priority-lane txs, and only fall back to requesting regular-lane
  txs when the priority request comes back empty (or when the
  peer has no more priority txs to offer). This biases scarce
  network capacity toward the tier that pays for it and matches
  the local mempool's own priority-over-regular preference in
  admission and block production. The wire format for the
  bifurcated pull is out of scope for this doc; the mempool
  merely commits to the *policy* that its outbound requests are
  priority-first with regular-only fallback on empty.

Streaming to peers via `snapshotTxsAfter` (Praos §4 in
`Mempool.lagda.md`) becomes lane-aware in the same way: a peer's
cursor is a pair `(lastPrioTicket, lastRegTicket)`, and the peer
chooses which lane's cursor to advance based on the pull policy
above.

### Revalidation cascade

Which event revalidates which layer:

| Event | `heldEB` / `ebLedger` | Priority lane | Regular lane | Cost |
|---|---|---|---|---|
| `addEB` (adopt peer EB) | set, rebuild | revalidate | revalidate | O(\|prio\| + \|reg\| + \|EB\|) |
| priority tx added | — | extend | revalidate | O(\|reg\|) |
| priority tx removed | — | revalidate | revalidate | O(\|prio\| + \|reg\|) |
| regular tx added | — | — | extend | O(1) |
| regular tx removed | — | — | revalidate | O(\|reg\|) |
| `discardEB` | clear | revalidate | revalidate | O(\|prio\| + \|reg\|) |
| `seeRBBody ts p` | maybe discard via `stillLive` | drop referenced + revalidate | drop referenced + revalidate | O(\|prio\| + \|reg\| + \|ts\|) |
| `seeRBCert e p` **(match)** | clear | **bit-identical rename** | **bit-identical rename** | **O(1)** |
| `seeRBCert e p` **(no match)** | discard | drop `e.ebTxs` + revalidate | drop `e.ebTxs` + revalidate | O(\|prio\| + \|reg\| + \|e.ebTxs\|) |
| `syncWithLedger p` | maybe discard via `stillLive` | revalidate | revalidate | O(\|prio\| + \|reg\| + \|heldEB.ebTxs\|) |

The **"priority tx added → regular revalidate"** entry is required,
not an optimization choice. The chain semantics fix a canonical
application order (`ebLedger + priorities + regulars`) and the regular
lane's validity invariant is "valid against `priorityUpdatedLedger =
ebLedger + priorities`". Any change to `priorityUpdatedLedger` —
including a single new priority tx — re-opens that invariant. Even
when a new priority tx is input-disjoint from every regular tx, the
regular lane is re-applied on top of the updated
`priorityUpdatedLedger` so the spec never depends on a case-by-case
commutativity argument over the full ledger state (governance, stake,
parameter updates, script reference reads, etc.). An implementation
is free to fast-path provably independent additions, but the
canonical invariant is unconditional revalidation.

#### Alternative: commutative admission ("option 1")

A leaner rule for `addTx Priority t` would validate `t` against
**both** `priorityUpdatedLedger` and `regularUpdatedLedger`, admitting
only if both succeed. On success, the regular lane provably remains
valid after the state shift `priorityUpdatedLedger → applyTx
priorityUpdatedLedger t`, so the O(|regs|) regular revalidation drops
away. Admission is O(|regs|) at check time (running the tx against
both bases) but the state transition itself becomes O(1).

The soundness of this rule depends on **transaction commutativity**:
`t` composed after the regular sequence must produce the same ledger
state as the regular sequence composed after `t`. Cardano txs in
general do *not* commute (they can share stake credentials, reference
inputs, governance targets, protocol-parameter updates, script
context reads, etc.), so option 1 requires structural constraints on
priority txs to guarantee commutativity across every ledger dimension.

The constraints and the commutativity proof itself are being worked
out in
<https://github.com/IntersectMBO/formal-ledger-specifications/compare/polina/commutativity?expand=1>.
Until those constraints are pinned down, **this spec stays with
option 2** (the unconditional-revalidate rule in the cascade table
above). Once the proof lands, we may switch — the switch is local to
`addTx Priority`: the `priority tx added` row becomes `— / extend /
—`, the `Cost` column drops to O(|regs|) at admission and O(1) for
the state update, and the `RegularLayerValid` invariant relies on
the commutativity theorem rather than a direct reapply.

The **Scenario B row** (`seeRBCert (match)`) is a bit-identical
rename: because the mempool has been pre-validating both lanes
against a state that includes `heldEB.ebTxs`, and `ledgerAt(newTip)`
equals exactly that state when the cert is for our held EB, no
`applyTx` / `reapplyAll` calls are required. Only the field bindings
shift (`ledger := old ebLedger.value`, `heldEB := nothing`,
`ebLedger := nothing`, everything else unchanged).

The **Scenario A row** (`seeRBCert (no match)`) is the **phase-1**
behaviour: unconditional pruning of `E.ebTxs` from both lanes,
followed by full revalidation. A future phase may explore partial
revalidation (skip when we can prove `E.ebTxs` is input-disjoint
from `heldEB.ebTxs`, `priorityTxs`, and `regularTxs`), but the
canonical invariant for phase 1 is full revalidation.

### CIP-164 constraints (re-checked here)

Same list as in `MempoolLeios.lagda.md`, all still binding:

- Cert/tx exclusivity in RB body.
- EB is non-empty.
- EB validates against `ledgerAt(announcingRB)`.
- Per-EB size and Plutus budgets.
- Minimum inclusion delay `3·L_hdr + L_vote + L_diff`.
- One EB per announcing RB; same pool announces both.
- No EB-to-EB chaining.

None of these are changed by the lane split; they apply to the RB
and EB the two lanes produce, respectively.

## 2. Sketch Agda formalisation

Self-contained; only `Agda.Builtin.*` / `Agda.Primitive`. Comments
prefixed **`-- CHG:`** (changed from Leios) or **`-- NEW:`** (does
not exist in Leios) mark the delta.

```agda
open import Agda.Primitive        using (Level; lzero; lsuc)
open import Agda.Builtin.Bool     using (Bool; true; false)
open import Agda.Builtin.List     using (List; []; _∷_)
open import Agda.Builtin.Maybe    using (Maybe; just; nothing)
open import Agda.Builtin.Nat
  using (zero; suc; _+_) renaming (Nat to ℕ)
open import Agda.Builtin.Equality using (_≡_; refl)

-- Shared helpers (identical to those in MempoolLeios.lagda.md).

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

module MempoolLeiosPricing where

  ----------------------------------------------------------------------
  -- 1. Postulated primitives.
  --    CHG: split of the single `capacityAt` into two lane-specific
  --    caps (`prioCapAt` for RB body limit, `regCapAt` for EB body
  --    limit).  All other primitives are as in MempoolLeios.
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
    fitsWith     : Capacity → Capacity → Capacity → Bool
    -- CHG: replaces MempoolLeios's single `capacityAt` and `ebCap`.
    prioCapAt    : TipPoint → Capacity
    regCapAt     : TipPoint → Capacity
    freshTicket  : TicketNo → TicketNo
    freshEBId    : EBId → EBId

  ----------------------------------------------------------------------
  -- 2. Endorser Blocks and Ranking Blocks (unchanged from MempoolLeios).
  ----------------------------------------------------------------------

  record EB : Set where
    constructor mkEB
    field
      ebId   : EBId
      ebTip  : TipPoint
      ebTxs  : List Tx
  open EB

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
  -- 3. Ticket record and TxSeq (unchanged from MempoolLeios).
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
  -- 4. Reuse cache (unchanged from MempoolLeios).
  ----------------------------------------------------------------------

  postulate
    SeenSet    : Set
    emptySeen  : SeenSet
    seenAddEB  : SeenSet → List Tx → SeenSet
    seenClear  : SeenSet → SeenSet

  ----------------------------------------------------------------------
  -- 5. Lane tag — NEW: does not exist in MempoolLeios.
  ----------------------------------------------------------------------

  data Lane : Set where
    Priority : Lane
    Regular  : Lane

  ----------------------------------------------------------------------
  -- 6. The mempool state
  --
  --    The ledger-stack top three fields (`ledger`, `heldEB`,
  --    `ebLedger`) match MempoolLeios exactly.  Everything else
  --    doubles.
  --
  --      Leios field              Pricing analogue
  --      -----------------------  -----------------------------------
  --      ledger                   ledger                     (same)
  --      heldEB                   heldEB                     (same)
  --      ebLedger                 ebLedger                   (same)
  --      txs                      priorityTxs, regularTxs    (split)
  --      updatedLedger            priorityUpdatedLedger,
  --                               regularUpdatedLedger        (split)
  --      lastTicket               lastPrioTicket, lastRegTicket
  --      capacity                 prioCap, regCap
  --      seenEBs                  seenEBs                    (same)
  ----------------------------------------------------------------------

  record MempoolLP : Set where
    constructor mkMempoolLP
    field
      tip                   : TipPoint
      ledger                : LedgerState        -- ledgerAt tip
      heldEB                : Maybe EB
      ebLedger              : Maybe LedgerState  -- ledger + heldEB.ebTxs

      -- NEW: priority lane, replaces Leios's `txs`.
      priorityTxs           : TxSeq
      -- NEW: priority working state, = (fromMaybe ledger ebLedger) + prio.
      -- Same role as Leios's `updatedLedger` — this is what a new
      -- priority tx validates against.
      priorityUpdatedLedger : LedgerState
      lastPrioTicket        : TicketNo           -- CHG: was lastTicket
      prioCap               : Capacity           -- CHG: was capacity (RB TxMeasure)

      -- NEW: regular lane.
      regularTxs            : TxSeq
      -- NEW: regular working state, = priorityUpdatedLedger + regs.
      -- What a new regular tx validates against.
      regularUpdatedLedger  : LedgerState
      lastRegTicket         : TicketNo
      regCap                : Capacity           -- EB-specific cap

      seenEBs               : SeenSet
  open MempoolLP

  -- Convenience: the base ledger for priority-lane validation.
  baseLedger : MempoolLP → LedgerState
  baseLedger m = fromMaybe (ledger m) (ebLedger m)

  ----------------------------------------------------------------------
  -- 7. Invariants
  --
  --   Two of the three ledger-stack invariants are inherited from
  --   MempoolLeios verbatim.  The tx-sequence invariant becomes a
  --   layered chain because we now have two lanes.
  ----------------------------------------------------------------------

  postulate
    LedgerAtTip :
      (m : MempoolLP) →
      ledger m ≡ ledgerAt (tip m)

    EBLedgerConsistent :
      (m : MempoolLP) →
      case heldEB m of λ where
        nothing  → ebLedger m ≡ nothing
        (just e) → ebLedger m ≡
                   just (fst (reapplyAll (ledger m) (ebTxs e)))

    -- NEW: replaces MempoolLeios's single TxsValid.
    PriorityLayerValid :
      (m : MempoolLP) →
      fst (reapplyAllTk (baseLedger m) (priorityTxs m))
      ≡ priorityUpdatedLedger m

    -- NEW: second half of the layered invariant.
    RegularLayerValid :
      (m : MempoolLP) →
      fst (reapplyAllTk (priorityUpdatedLedger m) (regularTxs m))
      ≡ regularUpdatedLedger m

  ----------------------------------------------------------------------
  -- 8. addTx — CHG: now takes a Lane.
  --
  --    Priority lane: validated against `priorityUpdatedLedger`
  --      (cumulative).  Any successful admission updates
  --      `priorityUpdatedLedger` and REVALIDATES the regular lane.
  --      This is the Leios-compat invariant: the regular lane must
  --      always be valid against `ebLedger + priorities`, and
  --      priorities have just changed.
  --
  --    Regular lane: validated against `regularUpdatedLedger`
  --      (cumulative regular post-state); admission does not touch
  --      the priority lane.
  ----------------------------------------------------------------------

  data AddResult : Set where
    Added    : MempoolLP → AddResult
    Rejected : MempoolLP → AddResult
    Blocked  : MempoolLP → AddResult

  addTx : Lane → Tx → MempoolLP → AddResult

  addTx Priority t m
    with fitsWith (prioCap m) (seqSize (priorityTxs m)) (measure t)
  ... | false = Blocked m
  ... | true  with applyTx (priorityUpdatedLedger m) t
  ...   | nothing = Rejected m
  ...   | just ℓ_prio′ =
          let n′            = freshTicket (lastPrioTicket m)
              tk            = mkTicket t n′ (measure t)
              -- CRUCIAL: regular lane revalidates against new
              -- priorityUpdatedLedger.  See §1 "priority tx added →
              -- regular revalidate".
              ℓ_reg′ , reg′ = reapplyAllTk ℓ_prio′ (regularTxs m)
          in Added (mkMempoolLP
               (tip m) (ledger m) (heldEB m) (ebLedger m)
               (priorityTxs m ++ tk ∷ []) ℓ_prio′ n′ (prioCap m)
               reg′ ℓ_reg′ (lastRegTicket m) (regCap m)
               (seenEBs m))

  addTx Regular t m
    with fitsWith (regCap m) (seqSize (regularTxs m)) (measure t)
  ... | false = Blocked m
  ... | true  with applyTx (regularUpdatedLedger m) t
  ...   | nothing = Rejected m
  ...   | just ℓ_reg′ =
          let n′ = freshTicket (lastRegTicket m)
              tk = mkTicket t n′ (measure t)
          in Added (mkMempoolLP
               (tip m) (ledger m) (heldEB m) (ebLedger m)
               (priorityTxs m) (priorityUpdatedLedger m)
               (lastPrioTicket m) (prioCap m)
               (regularTxs m ++ tk ∷ []) ℓ_reg′ n′ (regCap m)
               (seenEBs m))

  ----------------------------------------------------------------------
  -- 9. addEB — CHG: cascades through both lanes.
  --
  --    Same shape as Leios's `addEB` (rebuild ebLedger, revalidate),
  --    but revalidation now flows priority-lane → regular-lane in
  --    sequence.
  ----------------------------------------------------------------------

  postulate
    shouldHold : MempoolLP → EB → Bool

  addEB : EB → MempoolLP → MempoolLP
  addEB e m =
    if shouldHold m e
    then
      let ebL′            = fst (reapplyAll (ledger m) (ebTxs e))
          ℓ_prio′ , prio′ = reapplyAllTk ebL′  (priorityTxs m)
          ℓ_reg′  , reg′  = reapplyAllTk ℓ_prio′ (regularTxs m)
      in mkMempoolLP
           (tip m) (ledger m) (just e) (just ebL′)
           prio′ ℓ_prio′ (lastPrioTicket m) (prioCap m)
           reg′  ℓ_reg′  (lastRegTicket m)  (regCap m)
           (seenAddEB (seenEBs m) (ebTxs e))
    else
      mkMempoolLP
        (tip m) (ledger m) (heldEB m) (ebLedger m)
        (priorityTxs m) (priorityUpdatedLedger m)
        (lastPrioTicket m) (prioCap m)
        (regularTxs m) (regularUpdatedLedger m)
        (lastRegTicket m) (regCap m)
        (seenAddEB (seenEBs m) (ebTxs e))

  ----------------------------------------------------------------------
  -- 10. discardEB — NEW as an explicit event (implicit in Leios's
  --     syncWithLedger via `stillLive`, but exposed as its own
  --     handler here because it is the one "expensive undo" of a
  --     prior addEB in the pricing model).
  ----------------------------------------------------------------------

  discardEB : MempoolLP → MempoolLP
  discardEB m =
    let ℓ_prio′ , prio′ = reapplyAllTk (ledger m)  (priorityTxs m)
        ℓ_reg′  , reg′  = reapplyAllTk ℓ_prio′     (regularTxs m)
    in mkMempoolLP
         (tip m) (ledger m) nothing nothing
         prio′ ℓ_prio′ (lastPrioTicket m) (prioCap m)
         reg′  ℓ_reg′  (lastRegTicket m)  (regCap m)
         (seenEBs m)

  ----------------------------------------------------------------------
  -- 11. seeRBBody — CHG: drops referenced txs from BOTH lanes.
  ----------------------------------------------------------------------

  postulate
    stillLive : TipPoint → EB → Bool

  seeRBBody : List Tx → TipPoint → MempoolLP → MempoolLP
  seeRBBody rbTxs p m =
    let ids   = map txId rbTxs
        keep  = λ tk → if inTxIds ids (txId (tx tk)) then false else true
        prio0 = filter keep (priorityTxs m)
        reg0  = filter keep (regularTxs m)
        ledger′ = ledgerAt p
        held′ = case heldEB m of λ where
                  nothing  → nothing
                  (just e) → if stillLive p e then just e else nothing
        ebL′ = case held′ of λ where
                  nothing  → nothing
                  (just e) → just (fst (reapplyAll ledger′ (ebTxs e)))
        base′            = fromMaybe ledger′ ebL′
        ℓ_prio′ , prio′  = reapplyAllTk base′   prio0
        ℓ_reg′  , reg′   = reapplyAllTk ℓ_prio′ reg0
    in mkMempoolLP
         p ledger′ held′ ebL′
         prio′ ℓ_prio′ (lastPrioTicket m) (prioCapAt p)
         reg′  ℓ_reg′  (lastRegTicket m)  (regCapAt  p)
         (seenClear (seenEBs m))

  ----------------------------------------------------------------------
  -- 12. seeRBCert — CHG: as in MempoolLeios, but the Scenario B
  --     rename preserves BOTH lanes at zero cost, and the Scenario A
  --     path cascades revalidation through both lanes.
  ----------------------------------------------------------------------

  seeRBCert : EB → TipPoint → MempoolLP → MempoolLP
  seeRBCert e p m =
    let matches =
          case heldEB m of λ where
            nothing  → false
            (just h) → _≟EBId_ (ebId h) (ebId e)
    in if matches
       then
         -- Scenario B: no reapplyAll calls.  By the state invariant,
         -- old ebLedger.value ≡ ledgerAt p.  Both
         -- priorityUpdatedLedger and regularUpdatedLedger are still
         -- valid working states; only the ledger-stack fields
         -- shift.  Priority and regular txs / caps / tickets pass
         -- through unchanged.
         mkMempoolLP
           p (fromMaybe (ledger m) (ebLedger m)) nothing nothing
           (priorityTxs m) (priorityUpdatedLedger m)
           (lastPrioTicket m) (prioCapAt p)
           (regularTxs m) (regularUpdatedLedger m)
           (lastRegTicket m) (regCapAt p)
           (seenClear (seenEBs m))
       else
         -- Scenario A: e's txs are now on-chain; drop them from
         -- both lanes; discard our heldEB.
         let ids   = map txId (ebTxs e)
             keep  = λ tk → if inTxIds ids (txId (tx tk)) then false else true
             prio0 = filter keep (priorityTxs m)
             reg0  = filter keep (regularTxs m)
             ledger′            = ledgerAt p
             ℓ_prio′ , prio′    = reapplyAllTk ledger′ prio0
             ℓ_reg′  , reg′     = reapplyAllTk ℓ_prio′ reg0
         in mkMempoolLP
              p ledger′ nothing nothing
              prio′ ℓ_prio′ (lastPrioTicket m) (prioCapAt p)
              reg′  ℓ_reg′  (lastRegTicket m)  (regCapAt  p)
              (seenClear (seenEBs m))

  ----------------------------------------------------------------------
  -- 13. syncWithLedger — CHG: rebuilds all four ledger states.
  ----------------------------------------------------------------------

  syncWithLedger : TipPoint → MempoolLP → MempoolLP
  syncWithLedger p m =
    let ledger′ = ledgerAt p
        held′ = case heldEB m of λ where
                  nothing  → nothing
                  (just e) → if stillLive p e then just e else nothing
        ebL′ = case held′ of λ where
                  nothing  → nothing
                  (just e) → just (fst (reapplyAll ledger′ (ebTxs e)))
        base′            = fromMaybe ledger′ ebL′
        ℓ_prio′ , prio′  = reapplyAllTk base′   (priorityTxs m)
        ℓ_reg′  , reg′   = reapplyAllTk ℓ_prio′ (regularTxs m)
    in mkMempoolLP
         p ledger′ held′ ebL′
         prio′ ℓ_prio′ (lastPrioTicket m) (prioCapAt p)
         reg′  ℓ_reg′  (lastRegTicket m)  (regCapAt  p)
         (seenClear (seenEBs m))

  ----------------------------------------------------------------------
  -- 14. Block forging — CHG: RB body is drawn from the priority
  --     lane, EB body from the regular lane.
  ----------------------------------------------------------------------

  postulate
    splitAtCap  : Capacity → TxSeq → TxSeq × TxSeq
    nonEmpty    : TxSeq → Bool
    ebNonEmpty  : List Tx → Bool

  -- Safe to call regardless of `heldEB`.  Each lane is reapplied
  -- against the state it will actually meet on-chain: priorities
  -- against `ledger` (RB body applies there), then the EB body
  -- (priority overflow followed by regulars) against `rbLedger =
  -- ledger + rbTxs`.  Priority overflow that did not fit in the RB
  -- body flows into the EB body; the ledger will refund the
  -- priority-vs-regular fee differential to any priority tx that
  -- lands in the EB (see §1 "Priority-fee refund").
  -- The mempool state is unchanged; the reapplyAllTk calls produce
  -- the emitted block only.
  forgeBlock : MempoolLP → RB × Maybe EB
  forgeBlock m =
    let -- 1. Revalidate priorities against `ledger` (not baseLedger).
        _ , validPrio           = reapplyAllTk (ledger m) (priorityTxs m)
        rbTxs , prioOverflow    = splitAtCap (prioCap m) validPrio
        -- 2. Post-RB state = ledgerAt(newRB).
        rbLedger , _            = reapplyAllTk (ledger m) rbTxs
        -- 3. EB body candidates: priority overflow first (they paid
        --    the higher tier), then regulars.  Revalidate the whole
        --    combined sequence against rbLedger; some may drop.
        ebCandidates            = prioOverflow ++ regularTxs m
        _ , validEB             = reapplyAllTk rbLedger ebCandidates
        ebTxs′ , _              = splitAtCap (regCap m) validEB
        anyEB                   = ebNonEmpty (map tx ebTxs′)
        newEBId                 = freshEBId (freshTicket (lastPrioTicket m))
        maybeEB                 = if anyEB
                                    then just (mkEB newEBId (tip m)
                                                     (map tx ebTxs′))
                                    else nothing
        rbAnn                   = case maybeEB of λ where
                                    nothing  → nothing
                                    (just e) → just (ebId e)
        rb                      = mkRB (tip m) (RBTxs (map tx rbTxs)) rbAnn
    in rb , maybeEB
```

### Notes on this sketch

- **Postulates.** Same set as `MempoolLeios.lagda.md`, with
  `capacityAt` and `ebCap` replaced by lane-specific `prioCapAt` /
  `regCapAt`.
- **Scenario B in code.** `seeRBCert` when `matches = true` does not
  call `reapplyAll` at all — it only rebinds the ledger-stack fields
  and preserves both lanes' working states. The disjointness lemma
  (in `MempoolLeios.lagda.md` §5) generalises unchanged: neither
  lane may contain a duplicate of a `heldEB.ebTxs` entry.
- **What is not modeled** (same list as `MempoolLeios.lagda.md`):
  the vote/certificate construction; the certificate-inclusion path
  in `forgeBlock`; the exact `stillLive` clock; reorgs.

## 3. Open questions

Everything from `MempoolLeios.lagda.md` §7 still applies. Additional
open questions specific to this document:

1. **Pricing signal.** How does a submitter tell the mempool which
   lane to route a transaction into? Options: an explicit lane tag
   on the tx submission RPC, a threshold on the fee bid, or a
   per-tx `TxMeasure` classifier. Not fixed here.
2. **Independent-tx fast path for `addTx Priority`.** The
   revalidation of the regular lane on every priority admission is a
   canonical-invariant requirement, but an implementation may skip it
   when it can prove independence (disjoint inputs, reference inputs,
   collateral, stake certs, governance targets, and parameter
   effects). The cost/benefit depends on the shape of real workloads.
3. **Held-EB selection with two lanes.** Which peer EB should the
   node hold if several arrive from the same announcing RB? The
   choice affects `ebLedger` and therefore the fate of every priority
   tx currently in the lane. Modeled abstractly as `shouldHold`.

## Changelog

- **2026-06-09** — Initial version. Extracted the two-lane content
  from the earlier `Mempool.lagda.md` §12, corrected the CIP-164
  alignment (RB body carries a certificate, not an EB reference;
  short EB lifetime; discard rule).
- **2026-06-09 (later)** — Aligned the ledger-stack naming with
  `MempoolLeios.lagda.md`: renamed `fastLedger` →
  `priorityUpdatedLedger`, `ledger` (post-regular) →
  `regularUpdatedLedger`, `currentEB` → `heldEB`, added new
  `ledger : LedgerState` for the chain tip cache, changed
  `ebLedger : LedgerState` → `ebLedger : Maybe LedgerState`. Added
  Scenario B (matching-cert) bit-identical rename in `seeRBCert`.
  Added `discardEB` handler. Updated the revalidation-cascade table
  with explicit rows for Scenario A / B and `discardEB`.
- **2026-06-09 (later still)** — Fixed `forgeBlock` for the
  heldEB-at-forge case: priorities are reapplied against `ledger`
  (not `baseLedger`) before splitting into the RB body, and
  regulars are reapplied against `ledger + rbTxs` (the actual
  post-RB ledger state) before splitting into the announced EB
  body. Dropped the earlier phase-1 "heldEB = nothing" precondition.
  Cost at forge: 2 × O(|priorityTxs|) + O(|regularTxs|). Mempool
  state is unchanged; txs that fail the ledger revalidation remain
  in their lane (still valid under `baseLedger` /
  `priorityUpdatedLedger`) and become forgeable once `heldEB` is
  resolved.
- **2026-06-09 (last)** — Added two design notes: (a) `forgeBlock`
  now emits priority-lane *overflow* into the EB body ahead of
  regular txs, so a priority tx that does not fit `prioCap`
  reaches the chain via the announced EB rather than being
  discarded from the forged block; the ledger applies a priority-
  vs-regular fee-differential refund to any priority tx landing in
  an EB body (mempool preserves lane tag; ledger computes the
  refund). (b) Documented that this spec commits to option 2
  (unconditional regular-lane revalidation on priority admission);
  the commutativity-based option 1 alternative is described inline
  with a pointer to the in-progress proof at
  `IntersectMBO/formal-ledger-specifications:polina/commutativity`.
- **2026-06-09 (also)** — Added §1 "Peer transaction exchange
  (network side)" documenting two lane-aware requirements on the
  tx-submission mini-protocol layer: (i) inbound txs must carry a
  lane tag so the receiver can dispatch to `addTx Priority` or
  `addTx Regular`, and (ii) outbound pull is priority-first with
  regular-only fallback on empty. Also notes that peer streaming
  cursors become per-lane pairs.
- **2026-06-09 (final)** — Added §1 "Considered variant: EB
  suppression under light load", a design variant under
  consideration in which `forgeBlock` announces no EB when the
  combined mempool sits at or below half the RB capacity in every
  dimension. Motivation: EBs carry real costs (voting round,
  certification delay, propagation), so they should only be
  produced when the mempool has more than one RB's worth of
  pressure in at least one dimension. Threshold value provisional;
  a further tuning question is whether to make it protocol-
  parameterised.
