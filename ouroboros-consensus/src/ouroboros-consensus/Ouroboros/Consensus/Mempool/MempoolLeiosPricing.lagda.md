# Cardano Mempool — Linear Leios with Tiered Pricing

*A design sketch for adding tiered (priority / standard) pricing to the
Linear Leios mempool. Two tiers: priority-tier transactions are
destined for a Ranking Block body, standard-tier transactions for the
overflow Endorser Block. Self-contained; every place where behaviour
differs from `MempoolLeios.lagda.md` is called out in a comment
prefixed **`-- CHG:`** (change relative to Leios) or **`-- NEW:`**
(new in this document).*

**This is one of three sibling documents:**

1. **`Mempool.lagda.md`** — the current Praos-era mempool.
2. **`MempoolLeios.lagda.md`** — proposed Linear Leios adaptation
   (single tier, CIP-164-aligned).
3. **`MempoolLeiosPricing.lagda.md`** *(this file)* — tiered-pricing
   extension layered on top of the Leios mempool.

**Last updated:** 2026-07-29
**Primary reference:** CIP-164 Ouroboros Linear Leios,
<https://github.com/cardano-foundation/CIPs/tree/master/CIP-0164>
**Sibling ref:** `MempoolLeios.lagda.md` in this directory (shared
context on Leios EB/RB semantics; not a build-time dependency).

## 0. What changes from `MempoolLeios` (executive summary)

The Leios mempool holds a single sequence of transactions validated
against `updatedLedger = (ledger + heldEB.txs) + txs`. The tiered-
pricing mempool splits that sequence into two tiers:

- **Priority tier** (`priorityTxs`) — transactions that pay the priority-tier fee
  and are guaranteed a place in an RB body if room exists at the
  next forging opportunity.
- **Standard tier** (`standardTxs`) — transactions that pay the standard-tier fee
  and are eligible only for the overflow EB.

This split is a *mempool-side* extension. **CIP-164 does not define
any priority / standard distinction**; it only expresses an implicit
preference ("EBs should only be announced if a transaction cannot be
included in the base RB… the protocol will naturally incentivize
usage of RBs over EBs"). This document commits to a stronger,
explicit contract: the tier a transaction lives in determines *which
kind of block* it can end up in, and validation is arranged so that
the priority tier sees a ledger state that already accounts for the
EB the mempool currently holds.

### Structural differences vs. `MempoolLeios`

| Concept | `MempoolLeios` | This file |
|---|---|---|
| chain tip cache | `ledger` | `ledger` (unchanged) |
| held EB | `heldEB : Maybe EB` | `heldEB : Maybe EB` (unchanged) |
| tip + held EB applied | `ebLedger : Maybe LedgerState` | `ebLedger : Maybe LedgerState` (unchanged) |
| mempool working state | `updatedLedger` | *split into two:* `priorityUpdatedLedger`, `standardUpdatedLedger` |
| tx sequence | `txs` | *split into two:* `priorityTxs`, `standardTxs` |
| capacity | `capacity` | *split into two:* `priorityCap`, `standardCap` |
| ticket counter | `lastTicket` | *split into two:* `lastPriorityTicket`, `lastStandardTicket` |
| reuse cache | `seenEBs` | `seenEBs` (unchanged) |

So the *ledger stack* (`ledger`, `heldEB`, `ebLedger`) is imported
without change, and everything below it — the working state, the tx
sequence, the capacity, and the tickets — is doubled. That layout is
what makes the pricing extension a genuine extension rather than an
architectural break.

### Behavioural differences vs. `MempoolLeios`

- **Admission (`addTx`).** Now takes a `Tier` argument. Priority
  admissions cascade: any change to `priorityUpdatedLedger` triggers
  standard-tier revalidation (see §1 for the CIP-mirror argument).
- **EB acceptance (`addEB`).** Same as Leios in spirit — recompute
  `ebLedger`, revalidate — but now the revalidation runs through both
  tiers in sequence.
- **Chain events (`seeRBBody`, `seeRBCert`, `syncWithLedger`).**
  Same shape as Leios. `seeRBCert`'s **Scenario B** (cert matches
  our held EB) is a *tick-and-rename*: `ledger` always comes from
  `ledgerAt p` (the certifying RB's block-level updates are never
  skipped), and mid-epoch — provided no tx in either tier can have
  expired — both tiers' working states are preserved by an O(1)
  `tickTo`. On an epoch boundary or possible expiry, both tiers are
  reapplied in order (see MempoolLeios §4e/§5).
- **Discard (`discardEB`).** Same event as Leios; cascades through
  both tiers.
- **Block forging (`forgeBlock`).** Priority tier → RB body;
  standard tier → overflow EB body. The split is not a forge-time
  partition as it is in Leios; the tiers are stored separately by
  design.

## 1. Design summary

**Priority tier.** Transactions submitted to the priority tier (the more expensive fee class). Each
priority tx is validated against `priorityUpdatedLedger = (ledger +
heldEB.txs) + all prior priority txs`, so it sees the cumulative
effect of the priority txs already admitted plus the EB currently held.
`priorityCap` is the total `TxMeasure` of a single Ranking Block, taken
from protocol parameters.

**Standard tier.** Transactions submitted to the standard tier (the less expensive fee class). Each
standard tx is validated against `standardUpdatedLedger =
priorityUpdatedLedger + all prior standard txs` — the cumulative
standard-tier post-state. `standardCap` is a separate EB capacity derived
from the CIP's per-EB caps (`S_EB`, `S_EB-tx`, per-EB Plutus limits).

**Held EB.** The node keeps at most one EB (`heldEB`) — either its
own recently-forged EB awaiting a certificate window, or a peer's
announced EB whose eventual certification is worth pre-validating
for. When an EB is held, `ebLedger = just (ledger + heldEB.ebTxs)`;
otherwise `ebLedger = nothing`.

**Application order.** Chain semantics fix a single canonical
application order — `ledgerAt(oldTip) + certified EB (if any) + RB
body`, and if a later RB's certificate applies our held EB, its
transactions land before any RB body priority txs from that later RB.
This mempool mirrors that order in its layered ledger states so that
every stored transaction is valid against the exact state it will
meet on-chain.

### Capacity rules

- **Ranking Block / priority-tier limit.** One block's `TxMeasure`
  from protocol parameters: byte size, script ExUnits mem, script
  ExUnits CPU, reference-script bytes.
- **EB / standard-tier limit.** CIP-164's per-EB caps: `S_EB`
  (structure), `S_EB-tx` (referenced txs), per-EB Plutus step and
  memory. These are distinct dimensions from the RB caps.

### Block production

The lottery is the standard Praos VRF slot-leader election — a single
lottery, not one per block kind. Its winner produces:

- **An RB.** Body is either `priorityTxs` (a plain-tx body) or a
  certificate for a previously-announced EB. These are mutually
  exclusive (CIP-164: "when a certificate is included, no further
  transactions are allowed in the RB").
- **An EB, optionally.** Body is drawn from `standardTxs`, plus any
  priority-tier overflow that did not fit within `priorityCap` in the
  RB body. Announced in the RB header. Must be non-empty (CIP-164:
  "empty EBs should not be announced"). Additionally, `forgeBlock`
  suppresses EB emission under **light load** — unless no EB has been
  announced for `ageScape` slots, in which case the EB is forced out
  even below the floor (**age escape**) — see the subsection below.

**Fee on a priority tx that lands in an EB.** A priority-tier transaction that
ends up in an EB body — rather than in the RB body — has paid for priority
service (direct RB inclusion at its announcing slot) but did not
receive it (EB inclusion is subject to the vote/certificate flow and
the minimum inclusion delay). The ledger charges and refunds it on the
tier it *actually* lands in (standard, for an EB), **not** its claimed
(priority) tier: with `actualCoeff = standardCoeff`, if the tx named a
`feeChangeAddr` it is charged the standard-tier fee and refunded
`txFee − standardCoeff·minfee` to that address; if it named none, the
excess above `minfee` is donated to the treasury instead (no refund).
The *admission check*, by contrast, used the tx's **claimed** (priority)
tier (`tier.tierCoeff·minfee ≤ txFee`). The mempool itself does not
compute this; it only preserves the tier tag on each emitted tx, and
the ledger's fee split does the actual-tier charge/refund (see
`Utxo.lagda.md` / `Tiers.lagda.md` in the Cardano ledger repo).

An announced EB does not affect the ledger by itself. It goes through
the CIP-164 vote/certificate flow: the elected voting committee
validates it against `ledgerAt(announcingRB)`, votes are aggregated,
and a *later* RB `R'` — at least `3·L_hdr + L_vote + L_diff` slots
after the announcing RB — may include the certificate in its body.
Only when `R'` is adopted do the EB's (standard-tier) transactions
become on-chain.

If the immediate next RB after the announcing RB is produced before
the minimum delay elapses, CIP-164 discards the EB — its
transactions never reach the ledger through Leios. From the
mempool's perspective, `syncWithLedger` handles this by clearing
`heldEB` when the local `stillLive` predicate reports the
certification window has closed.

**`forgeBlock` with a held EB.** `forgeBlock` in §2 reapplies each
tier against `ledger` (not `baseLedger`) before splitting into RB
and EB bodies, so it is safe to call regardless of `heldEB`. The
emitted RB body applies on-chain against `ledger`, and the reapply
step drops any priority tx that speculatively depended on
`heldEB.ebTxs`. The mempool *state* is unchanged; such txs stay in
`priorityTxs`, still valid under `baseLedger`, and become forgeable
once `heldEB` is resolved (either certified via Scenario B —
after which they are valid under the new `ledger` too — or
discarded, after which they either survive the cascade or drop out).
The standard tier is handled symmetrically: its contents are
reapplied against the post-RB state `ledgerAt(newRB) = ledger +
rbTxs`. Cost: 2 × O(|priorityTxs|) + O(|standardTxs|) at forge time.

### EB suppression under light load

`forgeBlock` emits **no EB** (`maybeEB = nothing`, `rbAnnEB =
nothing`) whenever the **EB body** it just built (`ebTxs′`) sits
**below `ebFloor` in every dimension** — i.e., below the fullness floor
everywhere — for `d ∈ {byte size, ExUnits mem, ExUnits CPU, ref-script
bytes}`:

```text
seqSize ebTxs′ [d]  <  ebFloor [d]       (for every d)
```

where `ebFloor = ½ · full RB` is the EB-fullness **floor** (see §2).
This is a *lower* bound, distinct from `standardCap`, the CIP-164 per-EB
**capacity** (`S_EB`, …) that bounds the EB body from *above* via
`splitAtCap`. The floor is a design choice (not a CIP-164 requirement
beyond "no empty EBs") and a candidate protocol parameter; for it to
be reachable we need `ebFloor ≤ standardCap` in every dimension.

Rationale: EBs carry real costs (a voting round, a certification
delay, and additional propagation load), so they only earn their keep
when the overflow that lands in the EB reaches the floor in some
dimension. A near-empty EB (colliding with CIP-164's "empty EBs should
not be announced" rule) is never announced.

**Age escape (`ageScape`).** Suppression is bounded in time: for a
constant `ageScape` (a number of slots; a candidate protocol
parameter), if **no EB has been announced on the chain for
`ageScape` slots** as of the forging tip, the light-load suppression
is overridden and `forgeBlock` announces the EB even though it is
below `ebFloor`. (The EB must still be non-empty — the escape relaxes
the floor, never the CIP-164 no-empty-EBs rule.) Rationale: without
it, under sustained light load a standard-tier tx could sit in the
mempool indefinitely, since standard txs reach the chain *only*
through EBs; `ageScape` bounds that worst-case latency. **Ledger
alignment:** the ledger's floor check carries the same escape —
`sdChecks EB` (`Tiers.lagda.md` in `formal-ledger-specifications`)
accepts an under-floor EB when `ebOverdue slot` holds (`ageScape` /
`ebOverdue` live in `AbstractFunctions`; `DIVUP`'s environment gains
the block's slot) — so a forced EB validates and can certify (see
Open question 4).

Implementation: two guards in the `anyEB` computation of
`forgeBlock` (see §2), postulated in the Agda sketch as
`underHalfRB : Capacity → Capacity → Bool` (light load) and
`ebOverdue : TipPoint → Bool` (age escape, true iff no EB announced
within the last `ageScape` slots).

This suppression rule is the exact complement of the ledger's EB
validity check (`sdChecks` for `EB` blocks, enforced in `BBODY` via
`DIVUP` in `formal-ledger-specifications`), so an EB is suppressed here
iff it would be rejected there. Three things are matched on both sides:

1. *Reference-script bytes.* One of the four dimensions here; the
   ledger has a matching `totalRefScriptSize` accumulator in `SDPolicy`,
   checked against `maxRefScriptSizePerBlock` in `sdChecks`.
2. *Threshold = `ebFloor` = ½ a full RB.* Both sides compare against
   `ebFloor` directly, so there is no `/ 2` and no rounding question;
   the ledger writes `total ≥ ebFloor` (encoded, until `ebFloor` is a
   protocol parameter, as `2·total ≥ maxBlock`), suppression here is
   `size < ebFloor`.
3. *Same measured object and quantifier.* Both measure the **EB body**
   (`ebTxs′` here; the EB block's totals there). Suppress/reject only
   when small in **every** dimension (`underHalfRB` conjunction here;
   the dual disjunction in `sdChecks EB` there). **This "small in every
   dimension" choice is probably up for discussion** — the alternative
   is to require ≥ `ebFloor` in every dimension (reject/suppress if
   small in any one).

(Separately, the CIP-164 per-EB *capacity* `standardCap` is not yet enforced
by the ledger — the ledger bounds the EB only by this floor / `maxBlock`.
Enforcing the per-EB upper bound ledger-side is a TODO.)

### Peer transaction exchange (network side)

The bulk of these docs describes the mempool's local state machine.
The tx-submission mini-protocol — how transactions cross the wire
between peers — lives in a separate layer and is only sketched here.
The pricing mempool imposes two tier-aware requirements on that
layer:

- **Inbound routing by tier tag.** Each transaction on the wire
  carries a tier tag (either explicitly in the tx-submission frame
  or implicitly via a tx-body annotation such as the tier-selecting
  fee bid). The receiving node reads the tag and dispatches to
  `addTx Priority` or `addTx Standard` accordingly. Without tier
  awareness on the wire, a priority-tier tx received from a peer
  could end up in the wrong tier and lose priority service; the
  pricing model is unenforceable end-to-end.

- **Outbound: priority-first, with fallback.** The local node's
  tx-fetch policy toward each peer is: keep asking for
  priority-tier txs, and only fall back to requesting standard-tier
  txs when the priority request comes back empty (or when the
  peer has no more priority txs to offer). This biases scarce
  network capacity toward the tier that pays for it and matches
  the local mempool's own priority-over-standard preference in
  admission and block production. The wire format for the
  bifurcated pull is out of scope for this doc; the mempool
  merely commits to the *policy* that its outbound requests are
  priority-first with standard-only fallback on empty.

Streaming to peers via `snapshotTxsAfter` (Praos §4 in
`Mempool.lagda.md`) becomes tier-aware in the same way: a peer's
cursor is a pair `(lastPriorityTicket, lastStandardTicket)`, and the peer
chooses which tier's cursor to advance based on the pull policy
above.

### Revalidation cascade

Which event revalidates which layer:

| Event | `heldEB` / `ebLedger` | Priority tier | Standard tier | Cost |
|---|---|---|---|---|
| `addEB` (adopt peer EB) | set, rebuild | revalidate | revalidate | O(\|priority\| + \|standard\| + \|EB\|) |
| priority tx added | — | extend | revalidate | O(\|standard\|) |
| priority tx removed | — | revalidate | revalidate | O(\|priority\| + \|standard\|) |
| standard tx added | — | — | extend | O(1) |
| standard tx removed | — | — | revalidate | O(\|standard\|) |
| `discardEB` | clear | revalidate | revalidate | O(\|priority\| + \|standard\|) |
| `seeRBBody ts p` | maybe discard via `stillLive` | drop referenced + revalidate | drop referenced + revalidate | O(\|priority\| + \|standard\| + \|ts\|) |
| `seeRBCert e p` **(match, mid-epoch, no expiry)** | clear | **tick-and-rename** | **tick-and-rename** | **O(1)** |
| `seeRBCert e p` **(match, epoch boundary / expiry)** | clear | revalidate against `ledgerAt p` | revalidate | O(\|priority\| + \|standard\|) |
| `seeRBCert e p` **(no match)** | discard | drop `e.ebTxs` + revalidate | drop `e.ebTxs` + revalidate | O(\|priority\| + \|standard\| + \|e.ebTxs\|) |
| `syncWithLedger p` | maybe discard via `stillLive` | revalidate | revalidate | O(\|priority\| + \|standard\| + \|heldEB.ebTxs\|) |

The **"priority tx added → standard revalidate"** entry is required,
not an optimization choice. The chain semantics fix a canonical
application order (`ebLedger + priority txs + standard txs`) and the standard
tier's validity invariant is "valid against `priorityUpdatedLedger =
ebLedger + priority txs`". Any change to `priorityUpdatedLedger` —
including a single new priority tx — re-opens that invariant. Even
when a new priority tx is input-disjoint from every standard tx, the
standard tier is re-applied on top of the updated
`priorityUpdatedLedger` so the spec never depends on a case-by-case
commutativity argument over the full ledger state (governance, stake,
parameter updates, script reference reads, etc.). An implementation
is free to priority-path provably independent additions, but the
canonical invariant is unconditional revalidation.

#### Alternative: commutative admission ("option 1")

A leaner rule for `addTx Priority t` would validate `t` against
**both** `priorityUpdatedLedger` and `standardUpdatedLedger`, admitting
only if both succeed. On success, the standard tier provably remains
valid after the state shift `priorityUpdatedLedger → applyTx
priorityUpdatedLedger t`, so the O(|standardTxs|) standard revalidation drops
away. Admission is O(|standardTxs|) at check time (running the tx against
both bases) but the state transition itself becomes O(1).

The soundness of this rule depends on **transaction commutativity**:
`t` composed after the standard sequence must produce the same ledger
state as the standard sequence composed after `t`. Cardano txs in
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
—`, the `Cost` column drops to O(|standardTxs|) at admission and O(1) for
the state update, and the `StandardLayerValid` invariant relies on
the commutativity theorem rather than a direct reapply.

The **Scenario B rows** (`seeRBCert (match)`) follow MempoolLeios
§4e. The certifying RB still applies block-level updates — it ticks
the ledger to its slot before the certified txs are applied as
attested (cert-application assumption, MempoolLeios §5) — so
`ledgerAt p = tickTo p (old ledger) + E.ebTxs`, and `ledger` is
always taken from `ledgerAt p`, never renamed from `ebLedger`.
Mid-epoch, if no tx in *either* tier can have expired (cached
watermark per tier), the tick-commutation lemma lets both working
states survive as O(1) ticks: `priorityUpdatedLedger := tickTo p
(old priorityUpdatedLedger)`, `standardUpdatedLedger := tickTo p (old
standardUpdatedLedger)`, both tx sequences unchanged. On an epoch
boundary or possible expiry, both tiers are reapplied in order
(priority against `ledgerAt p`, standard against the result) — even
assuming the certified EB is valid, the mempool's own txs must be
re-applied there. No drop-filter over `E.ebTxs` is needed in either
path (disjointness lemma).

The **Scenario A row** (`seeRBCert (no match)`) is the **phase-1**
behaviour: unconditional pruning of `E.ebTxs` from both tiers,
followed by full revalidation. A future phase may explore partial
revalidation (skip when we can prove `E.ebTxs` is input-disjoint
from `heldEB.ebTxs`, `priorityTxs`, and `standardTxs`), but the
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

None of these are changed by the tier split; they apply to the RB
and EB the two tiers produce, respectively.

## 2. Sketch Agda formalisation

Self-contained; only `Agda.Builtin.*` / `Agda.Primitive`. Comments
prefixed **`-- CHG:`** (changed from Leios) or **`-- NEW:`** (does
not exist in Leios) mark the delta.

```agda
module MempoolLeiosPricing where

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

----------------------------------------------------------------------
-- 1. Postulated primitives.
--    CHG: split of the single `capacityAt` into two tier-specific
--    caps (`priorityCapAt` for RB body limit, `standardCapAt` for EB body
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
  priorityCapAt    : TipPoint → Capacity
  -- standardCapAt is the EB *capacity* (upper bound): the CIP-164 per-EB caps
  -- (S_EB, S_EB-tx, per-EB Plutus). Used to cap the EB body via splitAtCap.
  standardCapAt     : TipPoint → Capacity
  -- NEW: ebFloorAt is the EB-fullness *floor* (lower bound) — a SEPARATE
  -- quantity from standardCap. An EB must reach it in some dimension or it is
  -- suppressed (here) / rejected (ledger). Intended value: ½ a full RB
  -- (½ · priorityCapAt) per dimension. This floor is a design choice — NOT a
  -- CIP-164 requirement (the CIP only forbids empty EBs) — so it is probably
  -- up for discussion, and is a candidate protocol parameter. For the floor
  -- to be reachable we need ebFloor ≤ standardCap in every dimension.
  ebFloorAt    : TipPoint → Capacity
  -- NEW: light-load predicate for EB suppression (see §1).
  -- underHalfRB size cap ≡ true iff size[d] < cap[d] for every dimension d
  -- (byte size, ExUnits mem, ExUnits CPU, ref-script bytes). Applied to the EB body
  -- with cap = ebFloor (= ½ a full RB), so it reads "the EB body is below the
  -- fullness floor in every dimension" — the complement of the ledger's sdChecks EB
  -- (valid iff total ≥ ebFloor in some dimension). The ½ lives in ebFloor, so there
  -- is no doubling/rounding. Conjunction (suppress only when small in *every*
  -- dimension) — matching the ledger's dual disjunction. NOTE "small in every
  -- dimension" is probably up for discussion (vs. requiring ≥ ebFloor in every
  -- dimension). The ref-script-bytes dimension matches the ledger's totalRefScriptSize.
  underHalfRB  : Capacity → Capacity → Bool
  -- NEW: EB-age escape (see §1).  ageScape is a constant number of
  -- slots (a candidate protocol parameter).  ebOverdue p ≡ true iff
  -- no EB has been announced on the chain within the last ageScape
  -- slots as of tip p.  When overdue, forgeBlock announces the EB
  -- even below the fullness floor (light-load suppression is
  -- overridden; the EB must still be non-empty).  The ledger-side
  -- floor check must carry the same escape or the forced EB would
  -- fail validation.
  ageScape     : ℕ
  ebOverdue    : TipPoint → Bool
  freshTicket  : TicketNo → TicketNo
  freshEBId    : TicketNo → EBId

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
  in ls , rebuild plain tks
  where
    rebuild : List Tx → TxSeq → TxSeq
    rebuild [] _ = []
    rebuild (t ∷ ts) [] = []
    rebuild (t ∷ ts) (tk ∷ tks) =
      if _≟TxId_ (txId t) (txId (tx tk))
      then tk ∷ rebuild ts tks
      else rebuild (t ∷ ts) tks

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
-- 5. Tier tag — NEW: does not exist in MempoolLeios.
----------------------------------------------------------------------

data Tier : Set where
  Priority : Tier
  Standard  : Tier

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
--      txs                      priorityTxs, standardTxs    (split)
--      updatedLedger            priorityUpdatedLedger,
--                               standardUpdatedLedger        (split)
--      lastTicket               lastPriorityTicket, lastStandardTicket
--      capacity                 priorityCap, standardCap
--      seenEBs                  seenEBs                    (same)
----------------------------------------------------------------------

record MempoolLP : Set where
  constructor mkMempoolLP
  field
    tip                   : TipPoint
    ledger                : LedgerState        -- ledgerAt tip
    heldEB                : Maybe EB
    ebLedger              : Maybe LedgerState  -- ledger + heldEB.ebTxs

    -- NEW: priority tier, replaces Leios's `txs`.
    priorityTxs           : TxSeq
    -- NEW: priority working state, = (fromMaybe ledger ebLedger) + priority.
    -- Same role as Leios's `updatedLedger` — this is what a new
    -- priority tx validates against.
    priorityUpdatedLedger : LedgerState
    lastPriorityTicket        : TicketNo           -- CHG: was lastTicket
    priorityCap               : Capacity           -- CHG: was capacity (RB TxMeasure)

    -- NEW: standard tier.
    standardTxs            : TxSeq
    -- NEW: standard working state, = priorityUpdatedLedger + standard txs.
    -- What a new standard tx validates against.
    standardUpdatedLedger  : LedgerState
    lastStandardTicket         : TicketNo
    standardCap                : Capacity           -- EB-specific cap

    seenEBs               : SeenSet
open MempoolLP

-- Convenience: the base ledger for priority-tier validation.
baseLedger : MempoolLP → LedgerState
baseLedger m = fromMaybe (ledger m) (ebLedger m)

----------------------------------------------------------------------
-- 7. Invariants
--
--   Two of the three ledger-stack invariants are inherited from
--   MempoolLeios verbatim.  The tx-sequence invariant becomes a
--   layered chain because we now have two tiers.
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
  StandardLayerValid :
    (m : MempoolLP) →
    fst (reapplyAllTk (priorityUpdatedLedger m) (standardTxs m))
    ≡ standardUpdatedLedger m

----------------------------------------------------------------------
-- 8. addTx — CHG: now takes a Tier.
--
--    Priority tier: validated against `priorityUpdatedLedger`
--      (cumulative).  Any successful admission updates
--      `priorityUpdatedLedger` and REVALIDATES the standard tier.
--      This is the Leios-compat invariant: the standard tier must
--      always be valid against `ebLedger + priority txs`, and
--      priority txs have just changed.
--
--    Standard tier: validated against `standardUpdatedLedger`
--      (cumulative standard post-state); admission does not touch
--      the priority tier.
----------------------------------------------------------------------

data AddResult : Set where
  Added    : MempoolLP → AddResult
  Rejected : MempoolLP → AddResult
  Blocked  : MempoolLP → AddResult

addTx : Tier → Tx → MempoolLP → AddResult

addTx Priority t m
  with fitsWith (priorityCap m) (seqSize (priorityTxs m)) (measure t)
... | false = Blocked m
... | true  with applyTx (priorityUpdatedLedger m) t
...   | nothing = Rejected m
...   | just ℓ_priority′ =
        let n′            = freshTicket (lastPriorityTicket m)
            tk            = mkTicket t n′ (measure t)
            -- CRUCIAL: standard tier revalidates against new
            -- priorityUpdatedLedger.  See §1 "priority tx added →
            -- standard revalidate".
            ℓ_standard′ , standard′ = reapplyAllTk ℓ_priority′ (standardTxs m)
        in Added (mkMempoolLP
             (tip m) (ledger m) (heldEB m) (ebLedger m)
             (priorityTxs m ++ tk ∷ []) ℓ_priority′ n′ (priorityCap m)
             standard′ ℓ_standard′ (lastStandardTicket m) (standardCap m)
             (seenEBs m))

addTx Standard t m
  with fitsWith (standardCap m) (seqSize (standardTxs m)) (measure t)
... | false = Blocked m
... | true  with applyTx (standardUpdatedLedger m) t
...   | nothing = Rejected m
...   | just ℓ_standard′ =
        let n′ = freshTicket (lastStandardTicket m)
            tk = mkTicket t n′ (measure t)
        in Added (mkMempoolLP
             (tip m) (ledger m) (heldEB m) (ebLedger m)
             (priorityTxs m) (priorityUpdatedLedger m)
             (lastPriorityTicket m) (priorityCap m)
             (standardTxs m ++ tk ∷ []) ℓ_standard′ n′ (standardCap m)
             (seenEBs m))

----------------------------------------------------------------------
-- 9. addEB — CHG: cascades through both tiers.
--
--    Same shape as Leios's `addEB` (rebuild ebLedger, revalidate),
--    but revalidation now flows priority-tier → standard-tier in
--    sequence.
----------------------------------------------------------------------

postulate
  shouldHold : MempoolLP → EB → Bool

addEB : EB → MempoolLP → MempoolLP
addEB e m =
  if shouldHold m e
  then (let ebL′            = fst (reapplyAll (ledger m) (ebTxs e))
            ℓ_priority′ , priority′ = reapplyAllTk ebL′  (priorityTxs m)
            ℓ_standard′ , standard′ = reapplyAllTk ℓ_priority′ (standardTxs m)
        in mkMempoolLP
             (tip m) (ledger m) (just e) (just ebL′)
             priority′ ℓ_priority′ (lastPriorityTicket m) (priorityCap m)
             standard′ ℓ_standard′ (lastStandardTicket m) (standardCap m)
             (seenAddEB (seenEBs m) (ebTxs e)))
  else
    mkMempoolLP
      (tip m) (ledger m) (heldEB m) (ebLedger m)
      (priorityTxs m) (priorityUpdatedLedger m)
      (lastPriorityTicket m) (priorityCap m)
      (standardTxs m) (standardUpdatedLedger m)
      (lastStandardTicket m) (standardCap m)
      (seenAddEB (seenEBs m) (ebTxs e))

----------------------------------------------------------------------
-- 10. discardEB — NEW as an explicit event (implicit in Leios's
--     syncWithLedger via `stillLive`, but exposed as its own
--     handler here because it is the one "expensive undo" of a
--     prior addEB in the pricing model).
----------------------------------------------------------------------

discardEB : MempoolLP → MempoolLP
discardEB m =
  let ℓ_priority′ , priority′ = reapplyAllTk (ledger m)  (priorityTxs m)
      ℓ_standard′  , standard′  = reapplyAllTk ℓ_priority′     (standardTxs m)
  in mkMempoolLP
       (tip m) (ledger m) nothing nothing
       priority′ ℓ_priority′ (lastPriorityTicket m) (priorityCap m)
       standard′  ℓ_standard′  (lastStandardTicket m)  (standardCap m)
       (seenEBs m)

----------------------------------------------------------------------
-- 11. seeRBBody — CHG: drops referenced txs from BOTH tiers.
----------------------------------------------------------------------

postulate
  stillLive : TipPoint → EB → Bool

seeRBBody : List Tx → TipPoint → MempoolLP → MempoolLP
seeRBBody rbTxs p m =
  let ids   = map txId rbTxs
      keep  = λ tk → if inTxIds ids (txId (tx tk)) then false else true
      priority0 = filter keep (priorityTxs m)
      standard0  = filter keep (standardTxs m)
      ledger′ = ledgerAt p
      held′ = case heldEB m of λ where
                nothing  → nothing
                (just e) → if stillLive p e then just e else nothing
      ebL′ = case held′ of λ where
                nothing  → nothing
                (just e) → just (fst (reapplyAll ledger′ (ebTxs e)))
      base′            = fromMaybe ledger′ ebL′
      ℓ_priority′ , priority′  = reapplyAllTk base′   priority0
      ℓ_standard′  , standard′   = reapplyAllTk ℓ_priority′ standard0
  in mkMempoolLP
       p ledger′ held′ ebL′
       priority′ ℓ_priority′ (lastPriorityTicket m) (priorityCapAt p)
       standard′  ℓ_standard′  (lastStandardTicket m)  (standardCapAt  p)
       (seenClear (seenEBs m))

----------------------------------------------------------------------
-- 12. seeRBCert — CHG: as in MempoolLeios.  The certifying RB still
--     ticks the ledger to its slot, so ledgerAt p = tickTo p (old
--     ledger) + e.ebTxs (certified txs applied as attested,
--     MempoolLeios §5).  Scenario B: ledger always comes from
--     ledgerAt p; mid-epoch with no possible expiry in EITHER tier,
--     both working states survive as O(1) ticks; on an epoch
--     boundary (or possible expiry) both tiers must be reapplied
--     in order — even assuming the EB is valid.  Scenario A:
--     full revalidation through both tiers.
----------------------------------------------------------------------

postulate
  -- Block-level update only (slot counter, nonce, reward pulser;
  -- epoch work when crossing a boundary).  No tx application.
  tickTo      : TipPoint → LedgerState → LedgerState
  -- O(1) guard: does p stay inside the old tip's epoch?
  sameEpoch   : TipPoint → TipPoint → Bool
  -- O(1) guard: no tx in the sequence has a validity-interval
  -- upper bound below slot p.  Implementable as a cached watermark
  -- (minimum upper bound across the sequence, maintained per addTx;
  -- one watermark per tier).
  noneExpired : TipPoint → TxSeq → Bool

seeRBCert : EB → TipPoint → MempoolLP → MempoolLP
seeRBCert e p m =
  let matches =
        case heldEB m of λ where
          nothing  → false
          (just h) → _≟EBId_ (ebId h) (ebId e)
      ledger′ = ledgerAt p   -- block-level updates included; free,
                             -- materialized when the node adopted R'
  in if matches
     then
       (if sameEpoch (tip m) p
             ∧ (noneExpired p (priorityTxs m)
             ∧  noneExpired p (standardTxs m))
        then
          -- Scenario B, tick-rename path (O(1)): by the
          -- tick-commutation lemma (MempoolLeios §5) both working
          -- states are ticked in place; both tx sequences, tickets
          -- pass through unchanged (disjointness + noneExpired).
          mkMempoolLP
            p ledger′ nothing nothing
            (priorityTxs m) (tickTo p (priorityUpdatedLedger m))
            (lastPriorityTicket m) (priorityCapAt p)
            (standardTxs m) (tickTo p (standardUpdatedLedger m))
            (lastStandardTicket m) (standardCapAt p)
            (seenClear (seenEBs m))
        else
          -- Scenario B, reapply path (O(|priority| + |standard|)):
          -- epoch boundary crossed or a tx may have expired — the
          -- mempool's own txs must be reapplied against the new
          -- ledger, in tier order.  No drop-filter over e.ebTxs
          -- (disjointness lemma); drops from expiry / epoch-boundary
          -- rule changes are possible.
          let ℓ_priority′ , priority′ = reapplyAllTk ledger′ (priorityTxs m)
              ℓ_standard′  , standard′  = reapplyAllTk ℓ_priority′ (standardTxs m)
          in mkMempoolLP
               p ledger′ nothing nothing
               priority′ ℓ_priority′ (lastPriorityTicket m) (priorityCapAt p)
               standard′  ℓ_standard′  (lastStandardTicket m)  (standardCapAt  p)
               (seenClear (seenEBs m)))
     else
       -- Scenario A: e's txs are now on-chain; drop them from
       -- both tiers; discard our heldEB.
       let ids   = map txId (ebTxs e)
           keep  = λ tk → if inTxIds ids (txId (tx tk)) then false else true
           priority0 = filter keep (priorityTxs m)
           standard0  = filter keep (standardTxs m)
           ledger′            = ledgerAt p
           ℓ_priority′ , priority′    = reapplyAllTk ledger′ priority0
           ℓ_standard′  , standard′     = reapplyAllTk ℓ_priority′ standard0
       in mkMempoolLP
            p ledger′ nothing nothing
            priority′ ℓ_priority′ (lastPriorityTicket m) (priorityCapAt p)
            standard′  ℓ_standard′  (lastStandardTicket m)  (standardCapAt  p)
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
      ℓ_priority′ , priority′  = reapplyAllTk base′   (priorityTxs m)
      ℓ_standard′  , standard′   = reapplyAllTk ℓ_priority′ (standardTxs m)
  in mkMempoolLP
       p ledger′ held′ ebL′
       priority′ ℓ_priority′ (lastPriorityTicket m) (priorityCapAt p)
       standard′  ℓ_standard′  (lastStandardTicket m)  (standardCapAt  p)
       (seenClear (seenEBs m))

----------------------------------------------------------------------
-- 14. Block forging — CHG: RB body is drawn from the priority
--     tier, EB body from the standard tier.
----------------------------------------------------------------------

postulate
  splitAtCap  : Capacity → TxSeq → TxSeq × TxSeq
  nonEmpty    : TxSeq → Bool
  ebNonEmpty  : List Tx → Bool

-- Safe to call regardless of `heldEB`.  Each tier is reapplied
-- against the state it will actually meet on-chain: priority txs
-- against `ledger` (RB body applies there), then the EB body
-- (priority overflow followed by standard txs) against `rbLedger =
-- ledger + rbTxs`.  Priority overflow that did not fit in the RB
-- body flows into the EB body; the ledger then charges an EB-landed
-- priority tx on its ACTUAL (standard) tier — refunding the difference to a
-- feeChangeAddr if it named one, else donating the excess to the
-- treasury (see §1 "Fee on a priority tx that lands in an EB").
-- The mempool state is unchanged; the reapplyAllTk calls produce
-- the emitted block only.
forgeBlock : MempoolLP → RB × Maybe EB
forgeBlock m =
  let -- 1. Revalidate priority txs against `ledger` (not baseLedger).
      _ , validPrio           = reapplyAllTk (ledger m) (priorityTxs m)
      rbTxs , priorityOverflow    = splitAtCap (priorityCap m) validPrio
      -- 2. Post-RB state = ledgerAt(newRB).
      rbLedger , _            = reapplyAllTk (ledger m) rbTxs
      -- 3. EB body candidates: priority overflow first (they paid
      --    the priority-tier fee), then standard txs.  Revalidate the whole
      --    combined sequence against rbLedger; some may drop.
      ebCandidates            = priorityOverflow ++ standardTxs m
      _ , validEB             = reapplyAllTk rbLedger ebCandidates
      ebTxs′ , _              = splitAtCap (standardCap m) validEB
      -- 4. Light-load EB suppression (see §1): measure the EB body
      --    itself (ebTxs′) against the fullness floor ebFloor = ½ a
      --    full RB. If the body is below ebFloor in every dimension,
      --    do not announce an EB. (ebFloor is the fullness *floor* — a
      --    lower bound, distinct from standardCap, the CIP-164 per-EB
      --    *capacity* upper bound used above in splitAtCap. Measuring
      --    ebTxs′ — the actual EB body, which is what the ledger's
      --    sdChecks sees — keeps this the exact complement of the
      --    ledger check.)
      --    NEW: age escape — if no EB has been announced on the
      --    chain for ageScape slots (ebOverdue), suppression is
      --    overridden and the EB is announced even below the floor.
      --    The EB must still be non-empty.
      lightLoad               = underHalfRB (seqSize ebTxs′) (ebFloorAt (tip m))
      suppress                = if ebOverdue (tip m)
                                  then false
                                  else lightLoad
      anyEB                   = if suppress
                                  then false
                                  else ebNonEmpty (map tx ebTxs′)
      newEBId                 = freshEBId (freshTicket (lastPriorityTicket m))
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
  `capacityAt` and `ebCap` replaced by tier-specific `priorityCapAt` /
  `standardCapAt`.
- **Scenario B in code.** `seeRBCert` when `matches = true` takes
  `ledger` from `ledgerAt p` in both paths — the certifying RB's
  block-level updates (tick) are never skipped. The O(1) path
  requires `sameEpoch` and `noneExpired` for *both* tiers, ticks the
  two working states via `tickTo`, and keeps both tx sequences; the
  fallback path reapplies both tiers in order. The disjointness
  lemma, cert-application assumption, and tick-commutation lemma
  (all in `MempoolLeios.lagda.md` §5) generalise unchanged: neither
  tier may contain a duplicate of a `heldEB.ebTxs` entry, so no
  drop-filter over `e.ebTxs` is needed in either path.
- **What is not modeled** (same list as `MempoolLeios.lagda.md`):
  the vote/certificate construction; the certificate-inclusion path
  in `forgeBlock`; the exact `stillLive` clock; reorgs.

## 3. Open questions

Everything from `MempoolLeios.lagda.md` §7 still applies. Additional
open questions specific to this document:

1. **Pricing signal.** How does a submitter tell the mempool which
   tier to route a transaction into? Options: an explicit tier tag
   on the tx submission RPC, a threshold on the fee bid, or a
   per-tx `TxMeasure` classifier. Not fixed here.
2. **Independent-tx priority path for `addTx Priority`.** The
   revalidation of the standard tier on every priority admission is a
   canonical-invariant requirement, but an implementation may skip it
   when it can prove independence (disjoint inputs, reference inputs,
   collateral, stake certs, governance targets, and parameter
   effects). The cost/benefit depends on the shape of real workloads.
3. **Held-EB selection with two tiers.** Which peer EB should the
   node hold if several arrive from the same announcing RB? The
   choice affects `ebLedger` and therefore the fate of every priority
   tx currently in the tier. Modeled abstractly as `shouldHold`.
4. **EB-fullness floor: alignment with the ledger.** The fullness
   **floor** `ebFloor` (= ½ a full RB) is a lower bound, distinct from
   the CIP-164 per-EB **capacity** `standardCap` (upper bound). The floor
   check here is the exact complement of the ledger's EB validity check
   (`sdChecks` for `EB`, via `BBODY`/`DIVUP` in
   `formal-ledger-specifications`): both measure the **EB body** against
   `ebFloor` over the same four dimensions (byte size, ExUnits mem/CPU,
   ref-script bytes — the ledger's `totalRefScriptSize` vs
   `maxRefScriptSizePerBlock`), and suppress/reject only when it is
   small in **every** dimension, so an EB is suppressed here iff
   rejected there. Still open: (i) whether "small in every dimension" is
   the right quantifier — the alternative requires ≥ `ebFloor` in
   *every* dimension (reject if small in any), and is **up for
   discussion**; (ii) whether `ebFloor` should be a protocol parameter;
   (iii) enforcing the CIP-164 per-EB *capacity* (`standardCap` / `S_EB`,
   the upper bound) **ledger-side** — currently only the mempool caps
   the EB body by `standardCap`; the ledger bounds it only by the floor;
   (iv) the **age escape** is mirrored ledger-side
   (`formal-ledger-specifications`: `sdChecks EB` accepts an
   under-floor EB when `ebOverdue slot` holds; `ageScape` / `ebOverdue`
   are abstract in `AbstractFunctions`; `DIVUP`'s environment gains the
   block's slot, threaded from `BBODY`). Still open there: the voting
   committee must evaluate `ebOverdue` identically (same chain, same
   tip, same `ageScape`), and `ageScape` itself is a candidate
   protocol parameter.

## Changelog

- **2026-06-09** — Initial version. Extracted the two-tier content
  from the earlier `Mempool.lagda.md` §12, corrected the CIP-164
  alignment (RB body carries a certificate, not an EB reference;
  short EB lifetime; discard rule).
- **2026-06-09 (later)** — Aligned the ledger-stack naming with
  `MempoolLeios.lagda.md`: renamed `priorityLedger` →
  `priorityUpdatedLedger`, `ledger` (post-standard) →
  `standardUpdatedLedger`, `currentEB` → `heldEB`, added new
  `ledger : LedgerState` for the chain tip cache, changed
  `ebLedger : LedgerState` → `ebLedger : Maybe LedgerState`. Added
  Scenario B (matching-cert) bit-identical rename in `seeRBCert`.
  Added `discardEB` handler. Updated the revalidation-cascade table
  with explicit rows for Scenario A / B and `discardEB`.
- **2026-06-09 (later still)** — Fixed `forgeBlock` for the
  heldEB-at-forge case: priority txs are reapplied against `ledger`
  (not `baseLedger`) before splitting into the RB body, and
  standard txs are reapplied against `ledger + rbTxs` (the actual
  post-RB ledger state) before splitting into the announced EB
  body. Dropped the earlier phase-1 "heldEB = nothing" precondition.
  Cost at forge: 2 × O(|priorityTxs|) + O(|standardTxs|). Mempool
  state is unchanged; txs that fail the ledger revalidation remain
  in their tier (still valid under `baseLedger` /
  `priorityUpdatedLedger`) and become forgeable once `heldEB` is
  resolved.
- **2026-06-09 (last)** — Added two design notes: (a) `forgeBlock`
  now emits priority-tier *overflow* into the EB body ahead of
  standard txs, so a priority tx that does not fit `priorityCap`
  reaches the chain via the announced EB rather than being
  discarded from the forged block; the ledger applies a priority-
  vs-standard fee-differential refund to any priority tx landing in
  an EB body (mempool preserves tier tag; ledger computes the
  refund). (b) Documented that this spec commits to option 2
  (unconditional standard-tier revalidation on priority admission);
  the commutativity-based option 1 alternative is described inline
  with a pointer to the in-progress proof at
  `IntersectMBO/formal-ledger-specifications:polina/commutativity`.
- **2026-06-09 (also)** — Added §1 "Peer transaction exchange
  (network side)" documenting two tier-aware requirements on the
  tx-submission mini-protocol layer: (i) inbound txs must carry a
  tier tag so the receiver can dispatch to `addTx Priority` or
  `addTx Standard`, and (ii) outbound pull is priority-first with
  standard-only fallback on empty. Also notes that peer streaming
  cursors become per-tier pairs.
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
- **2026-07-09** — Promoted the EB-suppression variant above to a
  committed part of the spec. §1 subsection retitled from
  "Considered variant: …" to "EB suppression under light load";
  block-production bullet updated to reference it. Agda
  `forgeBlock` gains a `lightLoad = underHalfRB combinedSize
  (priorityCap m)` guard on `anyEB`, so `maybeEB = nothing` whenever
  `seqSize (priorityTxs ++ standardTxs)` is at or below `priorityCap /
  2` in every dimension. New postulate `underHalfRB : Capacity →
  Capacity → Bool` for the pointwise-half predicate. Threshold
  still `/ 2` (provisional; protocol-parameterisation left as a
  tuning question).
- **2026-07-13** — Cross-referenced the `underHalfRB` EB-suppression
  rule with the ledger's EB validity check (`sdChecks`/`BBODY` in
  `formal-ledger-specifications`) and flagged two items to be done
  consistently on both sides: (1) reference-script bytes are a
  `Capacity` dimension here but not yet folded into the ledger's
  `totalSize`; (2) the half-threshold rounding is `⌊cap/2⌋` here vs.
  `⌈cap/2⌉` in the ledger (suppression is deliberately no looser than
  the ledger bound, so emitted EBs stay valid). Added Open question 4;
  notes in §1 and the Agda sketch. No behavioural change.
- **2026-07-13 (later)** — Resolved both items from the previous entry.
  (1) Rounding decided: **ceiling** on both sides — suppression is now
  `2·size < cap` (`size < ⌈cap/2⌉`) in every dimension, the per-dimension
  complement of the ledger's `2·total ≥ cap`. §1 formula and the
  `underHalfRB` comment updated accordingly; `forgeBlock` guard comment
  adjusted. (2) Reference-script bytes: the ledger now carries a
  matching `totalRefScriptSize` accumulator checked against
  `maxRefScriptSizePerBlock`, so both sides account for the same four
  dimensions. These two are aligned, but the suppression/rejection
  relation is *not* a global iff — Open question 4 now records the
  remaining `/ 2`→protocol-parameter and quantifier/measurement (∀ over
  the whole mempool here vs. ∃ over the EB body in the ledger) questions.
  No behavioural change to the Agda sketch (the predicates were already
  abstract).
- **2026-07-13 (later still)** — Quantifier aligned: the ledger's
  `sdChecks EB` was changed to a disjunction (reject only when under-half
  in *every* dimension), matching `underHalfRB`'s conjunction here. So
  reject/suppress now agree on quantifier, rounding (ceiling) and all
  four dimensions. Recorded a NOTE on both sides that this "small in
  every dimension" rule is **probably up for discussion** (vs. requiring
  ≥half in every dimension). The whole-mempool-vs-EB-body measurement gap
  remains the sole residual mismatch (Open question 4). No behavioural
  change here.
- **2026-07-13 (final)** — Closed the measurement gap and simplified the
  threshold. `forgeBlock`'s suppression now measures the **EB body**
  (`ebTxs′`), not `combinedSize`, against **`standardCap`** (the EB capacity,
  set to ½ a full RB and — noted — a candidate protocol parameter),
  dropping the `priorityCap / 2` form: `lightLoad = underHalfRB (seqSize
  ebTxs′) (standardCap m)`. Since the ½ lives in `standardCap`, there is no
  doubling/rounding; `underHalfRB` reverts to `size[d] < cap[d]`. This
  makes suppression the exact complement of the ledger's `sdChecks EB`
  (both measure the EB body against `standardCap`), so an EB is suppressed
  here iff the ledger would reject it. `combinedSize` removed; §1,
  `standardCapAt`, `underHalfRB`, and Open question 4 updated. This *is* a
  behavioural change to the sketch (suppression predicate now over
  `ebTxs′`).
- **2026-07-13 (last)** — Un-conflated capacity vs floor. `standardCap`
  (`standardCapAt`) is restored to its CIP-164 meaning — the per-EB
  *capacity* (upper bound, `S_EB` etc.) that caps the EB body via
  `splitAtCap`. A NEW postulate `ebFloorAt : TipPoint → Capacity` is the
  EB-fullness *floor* (lower bound, = ½ a full RB), used by the
  suppression guard: `underHalfRB (seqSize ebTxs′) (ebFloorAt (tip m))`.
  The floor is a design choice (not a CIP-164 requirement beyond "no
  empty EBs"), still up for discussion, and a candidate protocol
  parameter; reachability needs `ebFloor ≤ standardCap`. §1, the postulate
  comments, and Open question 4 updated; noted that enforcing the
  CIP-164 per-EB capacity ledger-side is a TODO.
- **2026-07-24** — Terminology alignment: fast → priority, slow →
  standard, lane → tier, applied throughout this document and the
  siblings (identifiers, prose, and historical changelog entries
  alike, so the old terms no longer appear anywhere). Also removed
  the last "higher tier" / "lower tier" phrasings in favour of
  priority / standard.
- **2026-07-24 (later)** — Fixed `seeRBCert` Scenario B to apply the
  certifying RB's block-level updates, mirroring
  `MempoolLeios.lagda.md`: `ledger` now always comes from
  `ledgerAt p` (ticked to `R'`'s slot) instead of being renamed from
  `ebLedger`. The O(1) path is now a guarded *tick-and-rename*
  (`sameEpoch` ∧ `noneExpired` per tier; both working states ticked
  via `tickTo`); on an epoch boundary or possible tx expiry both
  tiers are reapplied in order — required even under the assumption
  that a certified EB is valid (that assumption only removes
  re-validation of the EB's own txs, per the cert-application
  assumption in `MempoolLeios.lagda.md` §5). New postulates:
  `tickTo`, `sameEpoch`, `noneExpired`. Cascade table gained
  separate match rows for the two paths.
- **2026-07-25** — Terminology: the second tier is **standard**, not
  "regular" (previous day's rename applied "regular"; all occurrences
  — prose, identifiers such as `standardTxs` / `standardUpdatedLedger`
  / `standardCap` / `lastStandardTicket` / `addTx Standard`, and
  changelog entries — now read standard). The tier vocabulary is fixed
  as **priority / standard**.
- **2026-07-29** — Added the **EB age escape**: a constant `ageScape`
  (slots, candidate protocol parameter) bounds light-load EB
  suppression in time. If no EB has been announced on the chain for
  `ageScape` slots (`ebOverdue`, new postulate alongside `ageScape`),
  `forgeBlock` announces the EB even below the fullness floor
  `ebFloor` (still never empty). Guards against unbounded
  standard-tier latency under sustained light load. §1 subsection,
  `forgeBlock` step 4 (`suppress` guard), and Open question 4 updated
  — the ledger-side floor check needs the same escape. Confirmed the
  base Leios spec has no underfull-EB constraint to begin with
  (`forgeBlock` there emits an EB for any non-empty overflow), so
  nothing to remove on that side.
- **2026-07-29 (later)** — Ledger-side age escape implemented in
  `formal-ledger-specifications` (branch `polina/dynamic`):
  `AbstractFunctions` gains `ageScape : ℕ` and the chain-history
  oracle `ebOverdue : Slot → Bool`; `sdChecks` takes the block's slot
  and its `EB` case gains the disjunct `ebOverdue slot ≡ true`;
  `DIVUP`'s environment becomes `PParams × BlockType × Slot`, with
  the slot threaded from `bhb .slot` in `BBODY`. The
  suppressed-iff-rejected complement between `underHalfRB` (mempool)
  and `sdChecks EB` (ledger) is preserved, escape included.
