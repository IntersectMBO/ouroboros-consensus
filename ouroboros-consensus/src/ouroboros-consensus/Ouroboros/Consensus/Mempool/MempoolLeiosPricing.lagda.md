# Cardano Mempool — Linear Leios with Tiered Pricing

*A design sketch for adding tiered (urgent / standard) pricing to the
Linear Leios mempool. Two tiers: urgent-tier transactions are
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

**Last updated:** 2026-07-30
**Primary reference:** CIP-164 Ouroboros Linear Leios,
<https://github.com/cardano-foundation/CIPs/tree/master/CIP-0164>
**Sibling ref:** `MempoolLeios.lagda.md` in this directory (shared
context on Leios EB/RB semantics; not a build-time dependency).

## 0. What changes from `MempoolLeios` (executive summary)

The Leios mempool holds a single sequence of transactions validated
against `updatedLedger = (ledger + heldEB.txs) + txs`. The tiered-
pricing mempool splits that sequence into two tiers:

- **Urgent tier** (`urgentTxs`) — transactions that pay the urgent-tier fee
  and are guaranteed a place in an RB body if room exists at the
  next forging opportunity.
- **Standard tier** (`standardTxs`) — transactions that pay the standard-tier fee
  and are eligible only for the overflow EB.

This split is a *mempool-side* extension. **CIP-164 does not define
any urgent / standard distinction**; it only expresses an implicit
preference ("EBs should only be announced if a transaction cannot be
included in the base RB… the protocol will naturally incentivize
usage of RBs over EBs"). This document commits to a stronger,
explicit contract: the tier a transaction lives in determines *which
kind of block* it can end up in, and validation is arranged so that
the urgent tier sees a ledger state that already accounts for the
EB the mempool currently holds.

### Structural differences vs. `MempoolLeios`

| Concept | `MempoolLeios` | This file |
|---|---|---|
| chain tip cache | `ledger` | `ledger` (unchanged) |
| held EB | `heldEB : Maybe EB` | `heldEB : Maybe EB` (unchanged) |
| tip + held EB applied | `ebLedger : Maybe LedgerState` | `ebLedger : Maybe LedgerState` (unchanged) |
| mempool working state | `updatedLedger` | *split into two:* `urgentUpdatedLedger`, `standardUpdatedLedger` |
| tx sequence | `txs` | *split into two:* `urgentTxs`, `standardTxs` |
| capacity | `capacity` | *split into two:* `urgentCap`, `standardCap` |
| ticket counter | `lastTicket` | *split into two:* `lastUrgentTicket`, `lastStandardTicket` |
| reuse cache | `seenEBs` | `seenEBs` (unchanged) |

So the *ledger stack* (`ledger`, `heldEB`, `ebLedger`) is imported
without change, and everything below it — the working state, the tx
sequence, the capacity, and the tickets — is doubled. That layout is
what makes the pricing extension a genuine extension rather than an
architectural break.

### Behavioural differences vs. `MempoolLeios`

- **Admission (`addTx`).** Now takes a `Tier` argument. The urgent
  tier accepts **any transaction shape**, gated on conflict rather
  than structure: an urgent tx must have a footprint disjoint from the
  standard queue's (`noConflict (conflictIx m) t`), and is validated
  **first at the back of the standard queue**
  (`standardUpdatedLedger`, the full mempool post-state), then at its
  insertion point (`urgentUpdatedLedger`). Those together discharge
  the commutativity theorem's hypotheses, so the state update is O(1)
  in queue length with **no standard-tier revalidation** (see §1). A
  conflicting urgent tx is **discarded**, never admitted by evicting
  standard txs.
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
- **Block forging (`forgeBlock`).** Urgent tier → RB body;
  standard tier → overflow EB body. The split is not a forge-time
  partition as it is in Leios; the tiers are stored separately by
  design.

## 1. Design summary

**Urgent tier.** Transactions submitted to the urgent tier (the
more expensive fee class). **Any transaction shape may be admitted**;
what is required is that it does not conflict with the standard queue,
which is what the commutativity theorem needs (see "Conflict-gated
admission" below). Each urgent tx is
validated **first** against `standardUpdatedLedger` — the **back of
the standard queue**, i.e. the full mempool post-state — **then**
against `urgentUpdatedLedger`, its actual insertion point; on
success it is appended to `urgentTxs` with both working ledgers
updated by those same two single tx applications. The commutativity
theorem is what lets a tx validated *after* the standard sequence be
inserted *before* it. `urgentCap` is the total `TxMeasure` of a
single Ranking Block, taken from protocol parameters.

**Standard tier.** Transactions submitted to the standard tier (the less expensive fee class). Each
standard tx is validated against `standardUpdatedLedger =
urgentUpdatedLedger + all prior standard txs` — the cumulative
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
transactions land before any RB body urgent txs from that later RB.
This mempool mirrors that order in its layered ledger states so that
every stored transaction is valid against the exact state it will
meet on-chain.

### Capacity rules

- **Ranking Block / urgent-tier limit.** One block's `TxMeasure`
  from protocol parameters: byte size, script ExUnits mem, script
  ExUnits CPU, reference-script bytes.
- **EB / standard-tier limit.** CIP-164's per-EB caps: `S_EB`
  (structure), `S_EB-tx` (referenced txs), per-EB Plutus step and
  memory. These are distinct dimensions from the RB caps.

### Block production

The lottery is the standard Praos VRF slot-leader election — a single
lottery, not one per block kind. Its winner produces:

- **An RB.** Body is either `urgentTxs` (a plain-tx body) or a
  certificate for a previously-announced EB. These are mutually
  exclusive (CIP-164: "when a certificate is included, no further
  transactions are allowed in the RB").
- **An EB, optionally.** Body is drawn from `standardTxs`, plus any
  urgent-tier overflow that did not fit within `urgentCap` in the
  RB body. Announced in the RB header. Must be non-empty (CIP-164:
  "empty EBs should not be announced"). Additionally, `forgeBlock`
  suppresses EB emission under **light load** — unless no EB has been
  announced for `ebAgeEscape` slots, in which case the EB is forced out
  even below the floor (**age escape**) — see the subsection below.

**Fee on an urgent tx that lands in an EB.** An urgent-tier transaction that
ends up in an EB body — rather than in the RB body — has paid for urgent
service (direct RB inclusion at its announcing slot) but did not
receive it (EB inclusion is subject to the vote/certificate flow and
the minimum inclusion delay). The ledger charges and refunds it on the
tier it *actually* lands in (standard, for an EB), **not** its claimed
(urgent) tier: with `actualCoeff = rawCoeff standardTier`, if the tx named a
`feeChangeAddr` it is charged the standard-tier fee and refunded
`txFee − minfeeAt actualCoeff minfee` to that address; if it named none, the
excess above `minfee` is donated to the treasury instead (no refund).
A transaction declares no coefficient of its own, so there is only one fee
constraint and it is on the actual tier: `minfeeAt actualCoeff minfee ≤ txFee`.
The mempool itself does not compute this; it only preserves the tier tag on
each emitted tx, and the ledger's fee split does the actual-tier
charge/refund (see `Utxo.lagda.md` / `Tiers.lagda.md` in the Cardano ledger
repo).

**Fixed-point coefficients.** Every tier coefficient is a natural scaled by
`tierScale = 10 ^ tierDec` (`tierDec = 6`), so a stored coefficient `c` denotes
the real number `c / tierScale`. Coefficients are therefore never multiplied
directly into a fee: the ledger's single conversion is
`minfeeAt c base = ⌈ base · c / tierScale ⌉`, and every fee comparison above goes
through it. This matters to the mempool because its admission and selection
checks must agree with the ledger's *exactly*, not up to rounding: a check
written in real arithmetic as `quote × (1 + stepBound)` can differ from the
ledger's floored value by a lovelace, which at the boundary means admitting a
transaction the ledger will reject, or evicting one that was valid. Any
implementation of the headroom rules below must evaluate them against
`minfeeAt` on integer coefficients rather than on rationals.

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
step drops any urgent tx that speculatively depended on
`heldEB.ebTxs`. The mempool *state* is unchanged; such txs stay in
`urgentTxs`, still valid under `baseLedger`, and become forgeable
once `heldEB` is resolved (either certified via Scenario B —
after which they are valid under the new `ledger` too — or
discarded, after which they either survive the cascade or drop out).
The standard tier is handled symmetrically: its contents are
reapplied against the post-RB state `ledgerAt(newRB) = ledger +
rbTxs`. Cost: 2 × O(|urgentTxs|) + O(|standardTxs|) at forge time.

### EB suppression under light load

`forgeBlock` emits **no EB** (`maybeEB = nothing`, `rbAnnEB =
nothing`) whenever the **EB body** it just built (`ebTxs′`) sits
**below `ebThreshold` in every dimension** — i.e., below the fullness floor
everywhere — for `d ∈ {byte size, ExUnits mem, ExUnits CPU, ref-script
bytes}`:

```text
seqSize ebTxs′ [d]  <  ebThreshold [d]       (for every d)
```

where `ebThreshold = ½ · full RB` is the EB-fullness **floor** (see §2).
This is a *lower* bound, distinct from `standardCap`, the CIP-164 per-EB
**capacity** (`S_EB`, …) that bounds the EB body from *above* via
`splitAtCap`. The floor is a design choice (not a CIP-164 requirement
beyond "no empty EBs") and a candidate protocol parameter; for it to
be reachable we need `ebThreshold ≤ standardCap` in every dimension.

Rationale: EBs carry real costs (a voting round, a certification
delay, and additional propagation load), so they only earn their keep
when the overflow that lands in the EB reaches the floor in some
dimension. A near-empty EB (colliding with CIP-164's "empty EBs should
not be announced" rule) is never announced.

**Age escape (`ebAgeEscape`).** Suppression is bounded, and it is
bounded in **Ranking Blocks, not slots**: for a protocol parameter
`ebAgeEscape` (K; recommended 10), if at least `ebAgeEscape` Ranking
Blocks have been produced since an EB certificate last entered the
chain, the light-load suppression is overridden and `forgeBlock`
announces the EB even though it is below the announcement threshold.
(The EB must still be non-empty — the escape relaxes the threshold,
never the CIP-164 no-empty-EBs rule.) Rationale: without it, under
sustained light load a standard-tier tx could sit in the mempool
indefinitely, since standard txs reach the chain *only* through EBs;
`ebAgeEscape` bounds that worst-case latency. Counting in Ranking
Blocks rather than slots measures the escape in the same resource it
relieves — RB space consumed by certificates — and makes it
insensitive to empty slots.

**Ledger alignment:** the ledger's threshold check carries the same
escape — `sdChecks EB` (`Tiers.lagda.md` in
`formal-ledger-specifications`) accepts a below-threshold EB when
`suc rbsSinceCert ≥ ebAgeEscape`, where `rbsSinceCert` is a counter in
`SDPolicy` incremented once per non-certificate Ranking Block and reset
whenever a certificate is accepted (the `suc` counts the
certificate-bearing RB now being validated, which carries no payload
and so never increments the counter itself). `ebAgeEscape` is a
protocol parameter; the counter is ledger state. Neither is a
chain-history oracle: the previous design used
`ebOverdue : Slot → Bool` in `AbstractFunctions`, which needed
unbounded lookback to decide, and `DIVUP`'s environment no longer
carries the block's slot. So a forced EB validates and can certify
(see Open question 4).

Implementation: two guards in the `anyEB` computation of
`forgeBlock` (see §2), postulated in the Agda sketch as
`underThreshold : Capacity → Capacity → Bool` (light load) and
`ebOverdue : TipPoint → Bool` (age escape). The mempool-side
`ebOverdue` stays a `TipPoint` query because a forging node reads the
count off its own selected chain rather than from ledger state; it is
true iff at least `ebAgeEscape` Ranking Blocks separate the tip from
the last certificate.

This suppression rule is the exact complement of the ledger's EB
validity check (`sdChecks` for `EB` blocks, enforced in `BBODY` via
`DIVUP` in `formal-ledger-specifications`), so an EB is suppressed here
iff it would be rejected there. Three things are matched on both sides:

1. *Reference-script bytes.* One of the four dimensions here; the
   ledger has a matching `totalRefScriptSize` accumulator in `SDPolicy`,
   checked against `maxRefScriptSizePerBlock` in `sdChecks`.
2. *Threshold = `thresholdFraction` of a full RB, per dimension.* No
   longer a hardcoded ½: the fraction is
   `max(1 − urgentTarget, 1/2)`, derived from the `urgentTarget`
   protocol parameter, because the resource a certificate displaces is a
   non-certificate Ranking Block carrying urgent traffic — a lower
   urgent target runs RBs deliberately emptier, so certificates must be
   rarer and qualifying EBs correspondingly fuller. At the recommended
   `urgentTarget = 1/2` the fraction is ½, so the historical behaviour
   is the default. Both sides compare against the same fraction, and
   both avoid division: with `urgentTarget = p/q`, the ledger encodes
   `x ≥ thresholdFraction · L` as the integer pair
   `q·x ≥ (q−p)·L` and `2·x ≥ L`, so there is no rounding question.
   Suppression here is the negation of the same test.
3. *Same measured object and quantifier.* Both measure the **EB body**
   (`ebTxs′` here; the EB block's totals there). Suppress/reject only
   when small in **every** dimension (`underThreshold` conjunction here;
   the dual disjunction in `sdChecks EB` there). **This "small in every
   dimension" choice is probably up for discussion** — the alternative
   is to require ≥ `ebThreshold` in every dimension (reject/suppress if
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
  `addTx Urgent` or `addTx Standard` accordingly (an urgent-tagged tx
  that conflicts with the local standard queue is Rejected at
  admission — and since queues differ between nodes, the same tx may
  be admitted by one peer and rejected by another).
  Without tier
  awareness on the wire, an urgent-tier tx received from a peer
  could end up in the wrong tier and lose urgent service; the
  pricing model is unenforceable end-to-end.

- **Outbound: urgent-first, with fallback.** The local node's
  tx-fetch policy toward each peer is: keep asking for
  urgent-tier txs, and only fall back to requesting standard-tier
  txs when the urgent request comes back empty (or when the
  peer has no more urgent txs to offer). This biases scarce
  network capacity toward the tier that pays for it and matches
  the local mempool's own urgent-over-standard preference in
  admission and block production. The wire format for the
  bifurcated pull is out of scope for this doc; the mempool
  merely commits to the *policy* that its outbound requests are
  urgent-first with standard-only fallback on empty.

Streaming to peers via `snapshotTxsAfter` (Praos §4 in
`Mempool.lagda.md`) becomes tier-aware in the same way: a peer's
cursor is a pair `(lastUrgentTicket, lastStandardTicket)`, and the peer
chooses which tier's cursor to advance based on the pull policy
above.

### Revalidation cascade

Which event revalidates which layer:

| Event | `heldEB` / `ebLedger` | Urgent tier | Standard tier | Cost |
|---|---|---|---|---|
| `addEB` (adopt peer EB) | set, rebuild | revalidate | revalidate | O(\|urgent\| + \|standard\| + \|EB\|) |
| urgent tx added (conflict-free) | — | extend | — (commutes) | O(1) + O(\|footprint\|) |
| urgent tx removed | — | revalidate | revalidate | O(\|urgent\| + \|standard\|) |
| standard tx added | — | — | extend | O(1) |
| standard tx removed | — | — | revalidate | O(\|standard\|) |
| `discardEB` | clear | revalidate | revalidate | O(\|urgent\| + \|standard\|) |
| `seeRBBody ts p` | maybe discard via `stillLive` | drop referenced + revalidate | drop referenced + revalidate | O(\|urgent\| + \|standard\| + \|ts\|) |
| `seeRBCert e p` **(match, mid-epoch, no expiry)** | clear | **tick-and-rename** | **tick-and-rename** | **O(1)** |
| `seeRBCert e p` **(match, epoch boundary / expiry)** | clear | revalidate against `ledgerAt p` | revalidate | O(\|urgent\| + \|standard\|) |
| `seeRBCert e p` **(no match)** | discard | drop `e.ebTxs` + revalidate | drop `e.ebTxs` + revalidate | O(\|urgent\| + \|standard\| + \|e.ebTxs\|) |
| `syncWithLedger p` | maybe discard via `stillLive` | revalidate | revalidate | O(\|urgent\| + \|standard\| + \|heldEB.ebTxs\|) |

The **"urgent tx added"** row is the one place where the standard
tier is *not* revalidated, and that exemption is earned, not assumed.
The chain semantics fix a canonical application order (`ebLedger +
urgent txs + standard txs`) and the standard tier's validity
invariant is "valid against `urgentUpdatedLedger = ebLedger +
urgent txs`" — so in general any change to `urgentUpdatedLedger`
re-opens that invariant, which is why every *other* event that
touches the urgent tier (`addEB`, urgent tx removed,
`discardEB`, `seeRBBody`, `seeRBCert` no-match / epoch-boundary
paths, `syncWithLedger`) still revalidates the standard tier
unconditionally. Admission alone is exempt, and the exemption is
earned per transaction by the conflict check below rather than
granted to a whole class of transactions.

#### Conflict-gated admission

`addTx Urgent t` admits only if `t`'s footprint is **disjoint from
the standard queue's**. The queue's footprint is maintained
incrementally as `conflictIx`, one counted multiset per conflict
source — the things that make two Cardano transactions fail to
commute:

```text
    nonConsumedReads : TxIn                ⇀ ℕ   -- reference inputs, unspent
                                                 -- collateral, inputs of
                                                 -- phase-2-invalid txs
    credentials      : Credential          ⇀ ℕ   -- certificate targets and
                                                 -- withdrawal credentials
    deposits         : DepositPurpose      ⇀ ℕ
    voteTargets      : GovActionId × Voter ⇀ ℕ
```

`noConflict (conflictIx m) t` tests `t`'s footprint against each,
costing `O(|footprint of t|)` lookups. On a conflict `t` is
`Rejected`: **the incoming urgent tx is discarded rather than
evicting standard txs.**

This replaces an earlier design that gated on structural predicates
— `SimpleTx` on the incoming tx (no certs, withdrawals, reference
inputs or gov votes) and `SpendOnly` on every standard tx. Those
made the theorem's hypotheses hold *vacuously* by banning whole
transaction shapes from a tier, which over-approximates badly: a tx
carrying a certificate cannot commute past a standard queue touching
the same credential, but commutes perfectly well past one that does
not. Gating on the actual conflict admits the second case and keeps
soundness in the first. It also removes a contradiction — `SimpleTx`
barred governance-proposing txs from the urgent tier, while
Praos-mode fallback feeds Ranking Blocks from the urgent queue
alone, so a standard-tier proposal would be censored for the whole
fallback. Under conflict gating any shape may be urgent.

The tx is then checked by **two single-tx validations**, in this
order:

1. **at the back of the standard queue** — `applyTx
   (standardUpdatedLedger m) t`, the full mempool post-state. Success
   means `t` conflicts with nothing already in the mempool, in either
   tier.
2. **at the back of the urgent queue** — `applyTx
   (urgentUpdatedLedger m) t`, the tx's actual insertion point
   (*before* the whole standard sequence). This second check is not
   redundant: `t` could spend an output *created by* a standard tx,
   making it valid at the full post-state yet invalid — and
   non-commuting — at the insertion point.

Note the first validation runs at the back of the *standard* queue,
not the urgent queue: for a conflict-free `t` valid at **both** bases
the **commutativity theorem** applies, and it is what makes the O(1)
trick sound:

- no standard tx is invalidated by inserting `t` before the standard
  sequence; and
- `(urgentUpdatedLedger + t) + standardTxs ≡ standardUpdatedLedger
  + t`, so both working ledgers are updated by the two applications
  already computed — no standard-tier revalidation, state transition
  O(1).

Where a conflict exists the theorem fails — Cardano txs in general do
*not* commute (shared stake credentials, reference inputs,
governance targets, protocol-parameter updates, script context
reads, etc.). Each multiset above keys exactly one of those failure
modes, so the disjointness test is a case-by-case independence check
rather than a blanket exclusion.

One class resists keying. **Governance proposals and DRep
(de)registration re-filter all pending votes**, so they do not commute
past *any* vote and there is no pairwise key to test. These are handled
by tier restriction instead: they may only ever be urgent, which is
also what the CIP requires for the Praos-fallback reason above. Their
global scope still has to be respected at admission — an incoming
proposal conflicts with any vote standing in the standard queue, i.e.
`noConflict` treats a non-empty `voteTargets` as a conflict for such a
tx.

The commutativity proof is at
<https://github.com/IntersectMBO/formal-ledger-specifications/compare/polina/txcomm?expand=1>;
this spec invokes it as an assumption, and the `StandardLayerValid`
invariant is maintained at admission by the theorem rather than by a
direct reapply (invariant `IxMatchesStandard` in §7 records that the
index really is the standard queue's footprint, which is what makes
`noConflict` a sound proxy for the theorem's hypotheses).

The **Scenario B rows** (`seeRBCert (match)`) follow MempoolLeios
§4e. The certifying RB still applies block-level updates — it ticks
the ledger to its slot before the certified txs are applied as
attested (cert-application assumption, MempoolLeios §5) — so
`ledgerAt p = tickTo p (old ledger) + E.ebTxs`, and `ledger` is
always taken from `ledgerAt p`, never renamed from `ebLedger`.
Mid-epoch, if no tx in *either* tier can have expired (cached
watermark per tier), the tick-commutation lemma lets both working
states survive as O(1) ticks: `urgentUpdatedLedger := tickTo p
(old urgentUpdatedLedger)`, `standardUpdatedLedger := tickTo p (old
standardUpdatedLedger)`, both tx sequences unchanged. On an epoch
boundary or possible expiry, both tiers are reapplied in order
(urgent against `ledgerAt p`, standard against the result) — even
assuming the certified EB is valid, the mempool's own txs must be
re-applied there. No drop-filter over `E.ebTxs` is needed in either
path (disjointness lemma).

The **Scenario A row** (`seeRBCert (no match)`) is the **phase-1**
behaviour: unconditional pruning of `E.ebTxs` from both tiers,
followed by full revalidation. A future phase may explore partial
revalidation (skip when we can prove `E.ebTxs` is input-disjoint
from `heldEB.ebTxs`, `urgentTxs`, and `standardTxs`), but the
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

foldl : {A B : Set} → (B → A → B) → B → List A → B
foldl f b []       = b
foldl f b (x ∷ xs) = foldl f (f b x) xs

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
--    caps (`urgentCapAt` for RB body limit, `standardCapAt` for EB body
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
  urgentCapAt    : TipPoint → Capacity
  -- standardCapAt is the EB *capacity* (upper bound): the CIP-164 per-EB caps
  -- (S_EB, S_EB-tx, per-EB Plutus). Used to cap the EB body via splitAtCap.
  standardCapAt     : TipPoint → Capacity
  -- NEW: ebThresholdAt is the EB-fullness *floor* (lower bound) — a SEPARATE
  -- quantity from standardCap. An EB must reach it in some dimension or it is
  -- suppressed (here) / rejected (ledger). Intended value: ½ a full RB
  -- (½ · urgentCapAt) per dimension. This floor is a design choice — NOT a
  -- CIP-164 requirement (the CIP only forbids empty EBs) — so it is probably
  -- up for discussion, and is a candidate protocol parameter. For the floor
  -- to be reachable we need ebThreshold ≤ standardCap in every dimension.
  ebThresholdAt    : TipPoint → Capacity
  -- NEW: light-load predicate for EB suppression (see §1).
  -- underThreshold size cap ≡ true iff size[d] < cap[d] for every dimension d
  -- (byte size, ExUnits mem, ExUnits CPU, ref-script bytes). Applied to the EB body
  -- with cap = ebThreshold (= ½ a full RB), so it reads "the EB body is below the
  -- fullness floor in every dimension" — the complement of the ledger's sdChecks EB
  -- (valid iff total ≥ ebThreshold in some dimension). The ½ lives in ebThreshold, so there
  -- is no doubling/rounding. Conjunction (suppress only when small in *every*
  -- dimension) — matching the ledger's dual disjunction. NOTE "small in every
  -- dimension" is probably up for discussion (vs. requiring ≥ ebThreshold in every
  -- dimension). The ref-script-bytes dimension matches the ledger's totalRefScriptSize.
  underThreshold  : Capacity → Capacity → Bool
  -- NEW: EB-age escape (see §1).  ebAgeEscape is a constant number of
  -- slots (a candidate protocol parameter).  ebOverdue p ≡ true iff
  -- no EB has been announced on the chain within the last ebAgeEscape
  -- slots as of tip p.  When overdue, forgeBlock announces the EB
  -- even below the fullness floor (light-load suppression is
  -- overridden; the EB must still be non-empty).  The ledger-side
  -- floor check carries the same escape (sdChecks EB in
  -- formal-ledger-specifications), so the forced EB validates.
  ebAgeEscape     : ℕ
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
  Urgent : Tier
  Standard  : Tier

----------------------------------------------------------------------
-- Conflict index — NEW.  Replaces the earlier structural predicates
-- `SimpleTx` (on the incoming urgent tx) and `SpendOnly` (on every
-- standard tx).
--
-- Those predicates were a CONSERVATIVE STAND-IN for the commutativity
-- theorem's hypotheses: they banned whole transaction *shapes* from a
-- tier so the hypotheses held vacuously.  That over-approximates badly
-- — a transaction carrying a certificate cannot commute past a
-- standard queue that touches the same credential, but commutes fine
-- past one that does not — and it contradicted the requirement that
-- governance-proposing transactions live in the URGENT tier (they are
-- barred by `noGovProps`, yet Praos-mode fallback feeds RBs from the
-- urgent queue alone, so putting them in the standard tier censors
-- governance for the fallback's duration).
--
-- We instead discharge the hypotheses DIRECTLY, per admission, by
-- testing the incoming transaction's footprint against a counted
-- multiset index maintained alongside the standard queue.  One
-- multiset per conflict source, each keyed by what makes two
-- transactions non-commuting:
--
--   nonConsumedReads : TxIn                  ⇀ ℕ  -- reference inputs, unspent
--                                                 -- collateral, and inputs of
--                                                 -- phase-2-invalid txs
--   credentials      : Credential            ⇀ ℕ  -- certificate targets and
--                                                 -- withdrawal credentials
--   deposits         : DepositPurpose        ⇀ ℕ
--   voteTargets      : GovActionId × Voter   ⇀ ℕ
--   drepVoters       : Credential            ⇀ ℕ  -- DRep credentials the queue
--                                                 -- votes with (see below)
--   drepDelegatees   : Credential            ⇀ ℕ  -- DRep credentials the queue
--                                                 -- delegates to (see below)
--
-- Counts are incremented on standard admission and decremented on
-- standard removal, so the index is always exactly the union of the
-- standard queue's footprints.  Admission then costs
-- O(|footprint of the incoming tx|) lookups rather than O(|standard|)
-- revalidations, and no transaction shape is excluded a priori.
--
-- `drepVoters` and `drepDelegatees` exist for the one governance
-- interaction that survives confining proposals and DRep certificates to
-- the urgent lane: an urgent tx that DEREGISTERS a DRep does not commute
-- past a standard tx that VOTES with it or DELEGATES to it.
-- Deregistration shrinks `dom dreps`, and (a) the ledger re-filters
-- recorded votes by membership in that set — so with the urgent tx last
-- the standard vote is recorded then filtered away, while with it moved
-- ahead a trailing standard vote is recorded after the filter and
-- survives; and (b) `DELEG-delegate` requires the delegatee DRep to be
-- registered, so a standard delegation that was valid with the urgent tx
-- last becomes premise-invalid with it moved ahead.  Registration and
-- proposals only ever GROW the sets the standard queue reads, so they
-- need no key.  These correspond to `QueueCompat.disjDregVotes` and
-- `QueueCompat.disjDregDelegs` in the commutativity proof.
----------------------------------------------------------------------

postulate
  ConflictIndex : Set
  emptyIx       : ConflictIndex
  ixAdd         : ConflictIndex → Tx → ConflictIndex
  ixRemove      : ConflictIndex → Tx → ConflictIndex
  -- Disjointness of the incoming URGENT tx's footprint from every
  -- multiset above, including `dregCreds tx` against both `drepVoters`
  -- and `drepDelegatees`.
  -- True ⇒ the tx commutes past the whole standard queue, discharging
  -- `All (QueueCompat tx) standardTxs`, so no standard tx is
  -- invalidated by the insertion.
  noConflict    : ConflictIndex → Tx → Bool
  -- The standard-lane admission gate: no governance proposals and no
  -- DRep (de)registration certificates.  Votes ARE permitted — they
  -- change neither `dom govSt` nor `dom dreps` and are handled pairwise
  -- by the vote-target index.  This is `GovDomStable` in the proof, and
  -- it is a LANE RESTRICTION rather than a conflict check: a proposal
  -- created by a standard tx would have a GovActionId that does not
  -- exist until it is applied, so an urgent tx voting on it has no key
  -- to be checked against.  Confining proposals and DRep certificates
  -- to the urgent lane removes that case by construction — and the
  -- urgent lane is the right one, since Praos-mode fallback feeds RBs
  -- from the urgent queue alone.
  govDomStable  : Tx → Bool

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
--      txs                      urgentTxs, standardTxs    (split)
--      updatedLedger            urgentUpdatedLedger,
--                               standardUpdatedLedger        (split)
--      lastTicket               lastUrgentTicket, lastStandardTicket
--      capacity                 urgentCap, standardCap
--      seenEBs                  seenEBs                    (same)
----------------------------------------------------------------------

record MempoolLP : Set where
  constructor mkMempoolLP
  field
    tip                   : TipPoint
    ledger                : LedgerState        -- ledgerAt tip
    heldEB                : Maybe EB
    ebLedger              : Maybe LedgerState  -- ledger + heldEB.ebTxs

    -- NEW: urgent tier, replaces Leios's `txs`.  No structural
    -- restriction on membership: admission is gated by a conflict
    -- check against the standard queue, not by transaction shape (§8).
    urgentTxs           : TxSeq
    -- NEW: urgent working state, = (fromMaybe ledger ebLedger) + urgent.
    -- Same role as Leios's `updatedLedger`.  A new urgent tx's
    -- SECOND validation runs here (its insertion point); its FIRST
    -- runs against standardUpdatedLedger (see §8).
    urgentUpdatedLedger : LedgerState
    lastUrgentTicket        : TicketNo           -- CHG: was lastTicket
    urgentCap               : Capacity           -- CHG: was capacity (RB TxMeasure)

    -- NEW: standard tier.
    standardTxs            : TxSeq
    -- NEW: standard working state, = urgentUpdatedLedger + standard txs.
    -- What a new standard tx validates against.
    standardUpdatedLedger  : LedgerState
    lastStandardTicket         : TicketNo
    standardCap                : Capacity           -- EB-specific cap
    -- NEW: the union of the standard queue's conflict footprints,
    -- maintained incrementally (ixAdd on standard admission, ixRemove
    -- on standard removal).  Invariant IxMatchesStandard, §7.
    conflictIx                 : ConflictIndex

    seenEBs               : SeenSet
open MempoolLP

-- Convenience: the base ledger for urgent-tier validation.
baseLedger : MempoolLP → LedgerState
baseLedger m = fromMaybe (ledger m) (ebLedger m)

-- Rebuild the conflict index from a standard queue.  Used at every
-- event that revalidates or filters the standard queue wholesale;
-- incremental ixAdd/ixRemove is only for single-tx admission/removal.
-- This is definitionally the RHS of invariant IxMatchesStandard (§7).
ixOf : TxSeq → ConflictIndex
ixOf sq = foldl ixAdd emptyIx (map tx sq)

----------------------------------------------------------------------
-- 7. Invariants
--
--   Two of the three ledger-stack invariants are inherited from
--   MempoolLeios verbatim.  The tx-sequence invariant becomes a
--   layered chain because we now have two tiers, plus a NEW
--   index invariant (IxMatchesStandard: the conflict index is exactly
--   the standard queue's footprint union).  There is deliberately NO
--   tier-membership invariant: any transaction shape may be urgent,
--   because admission is gated on conflict rather than on shape.
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
  UrgentLayerValid :
    (m : MempoolLP) →
    fst (reapplyAllTk (baseLedger m) (urgentTxs m))
    ≡ urgentUpdatedLedger m

  -- NEW: second half of the layered invariant.  NOTE: after an
  -- urgent admission this equality is maintained by the
  -- commutativity theorem, whose hypotheses the admission-time
  -- conflict check discharges (see §1), not by a direct reapply;
  -- every other event maintains it by direct reapply.
  StandardLayerValid :
    (m : MempoolLP) →
    fst (reapplyAllTk (urgentUpdatedLedger m) (standardTxs m))
    ≡ standardUpdatedLedger m

  -- NEW: the conflict index is exactly the union of the standard
  -- queue's footprints.  This is what makes `noConflict (conflictIx m)`
  -- a sound proxy for "commutes past every standard tx": maintained by
  -- ixAdd at standard admission and ixRemove at standard removal, and
  -- rebuilt whenever the standard queue is revalidated wholesale.
  IxMatchesStandard :
    (m : MempoolLP) →
    ixOf (standardTxs m) ≡ conflictIx m

----------------------------------------------------------------------
-- 8. addTx — CHG: now takes a Tier.
--
--    Urgent tier: ANY transaction shape may enter — admission is
--      gated on CONFLICT, not on structure.  Three checks:
--      (1) two single-tx validations, FIRST at the BACK OF THE
--      STANDARD QUEUE (`standardUpdatedLedger`, the full mempool
--      post-state), THEN at the back of the urgent queue
--      (`urgentUpdatedLedger`, the insertion point — t must not
--      depend on standard-queue outputs); and (2) a conflict check
--      of t's footprint against `conflictIx`, the union of the
--      standard queue's footprints.  Together these discharge the
--      commutativity theorem's hypotheses (§1) for THIS t against
--      THIS queue, so no standard tx is invalidated by the insertion,
--      the standard tier needs NO revalidation, and the update is
--      O(1) in the queue length — O(|footprint of t|) in total.
--      On any conflict t is Rejected: the incoming urgent tx is
--      discarded rather than evicting standard txs.
--      The standard tier's invariant ("valid against `ebLedger +
--      urgent txs`") is maintained by the theorem, not by reapply.
--
--    Standard tier: `govDomStable t` — no governance action proposals
--      and no DRep (de)registration; those belong to the urgent lane
--      (votes are fine here) — then validated against
--      `standardUpdatedLedger` (cumulative standard post-state).  NO
--      conflict check: a standard tx is appended and validated at the
--      place it will actually be applied, so nothing reorders it and it
--      need commute with nothing.  Admission does not touch the urgent
--      tier, but DOES extend `conflictIx` with t's footprint (ixAdd),
--      so later urgent admissions see it.  Any Tx (simple or not) may enter.
----------------------------------------------------------------------

data AddResult : Set where
  Added    : MempoolLP → AddResult
  Rejected : MempoolLP → AddResult
  Blocked  : MempoolLP → AddResult

addTx : Tier → Tx → MempoolLP → AddResult

addTx Urgent t m
  -- NEW: conflict gate, replacing the old `SimpleTx t` structural
  -- gate.  Disjointness of t's footprint from the standard queue's is
  -- what licenses moving t ahead of that queue; no transaction shape
  -- is excluded a priori, so governance-proposing and
  -- DRep-(de)registration txs can (and per the CIP must) be urgent.
  with noConflict (conflictIx m) t
... | false = Rejected m   -- conflicts with the standard queue: discard
                           -- the incoming urgent tx rather than evict.
... | true  with fitsWith (urgentCap m) (seqSize (urgentTxs m)) (measure t)
...   | false = Blocked m
                -- CHG: validated at the back of the STANDARD queue
                -- (standardUpdatedLedger, the full mempool
                -- post-state), NOT the back of the urgent queue.
...   | true  with applyTx (standardUpdatedLedger m) t
...     | nothing = Rejected m
                    -- Second validation: at the tx's actual insertion
                    -- point, the back of the URGENT queue.  Needed
                    -- even after the first check succeeds — t could
                    -- spend an output CREATED by a standard tx,
                    -- making it valid at the full post-state yet
                    -- invalid before the standard sequence (and
                    -- non-commuting).
...     | just ℓ_standard′ with applyTx (urgentUpdatedLedger m) t
...       | nothing = Rejected m
...       | just ℓ_urgent′ =
            -- t is conflict-free and valid at BOTH bases, so by the
            -- commutativity theorem (§1): no standard tx is
            -- invalidated by inserting t before the standard
            -- sequence, and ℓ_urgent′ + standardTxs ≡ ℓ_standard′.
            -- NO standard-tier revalidation: the state update is
            -- O(1) (the two single-tx applications above).
            let n′            = freshTicket (lastUrgentTicket m)
                tk            = mkTicket t n′ (measure t)
            in Added (mkMempoolLP
                 (tip m) (ledger m) (heldEB m) (ebLedger m)
                 (urgentTxs m ++ tk ∷ []) ℓ_urgent′ n′ (urgentCap m)
                 (standardTxs m) ℓ_standard′ (lastStandardTicket m) (standardCap m)
                 (conflictIx m)   -- unchanged: only standard txs feed the index
                 (seenEBs m))

addTx Standard t m
  -- NEW: lane restriction.  Governance action proposals and DRep
  -- (de)registration belong to the urgent lane; votes are fine here.
  -- This is the ONLY new gate: a standard tx is appended and validated
  -- exactly where it will be applied, so nothing reorders it and it
  -- owes no commutation to anything.  It may conflict freely with
  -- urgent txs, which are applied ahead of it regardless.
  with govDomStable t
... | false = Rejected m
... | true  with fitsWith (standardCap m) (seqSize (standardTxs m)) (measure t)
...   | false = Blocked m
...   | true  with applyTx (standardUpdatedLedger m) t
...     | nothing = Rejected m
...     | just ℓ_standard′ =
        let n′ = freshTicket (lastStandardTicket m)
            tk = mkTicket t n′ (measure t)
        in Added (mkMempoolLP
             (tip m) (ledger m) (heldEB m) (ebLedger m)
             (urgentTxs m) (urgentUpdatedLedger m)
             (lastUrgentTicket m) (urgentCap m)
             (standardTxs m ++ tk ∷ []) ℓ_standard′ n′ (standardCap m)
             (ixAdd (conflictIx m) t)   -- NEW: t joins the standard footprint
             (seenEBs m))

----------------------------------------------------------------------
-- 9. addEB — CHG: cascades through both tiers.
--
--    Same shape as Leios's `addEB` (rebuild ebLedger, revalidate),
--    but revalidation now flows urgent-tier → standard-tier in
--    sequence.
----------------------------------------------------------------------

postulate
  shouldHold : MempoolLP → EB → Bool

addEB : EB → MempoolLP → MempoolLP
addEB e m =
  if shouldHold m e
  then (let ebL′            = fst (reapplyAll (ledger m) (ebTxs e))
            ℓ_urgent′ , urgent′ = reapplyAllTk ebL′  (urgentTxs m)
            ℓ_standard′ , standard′ = reapplyAllTk ℓ_urgent′ (standardTxs m)
        in mkMempoolLP
             (tip m) (ledger m) (just e) (just ebL′)
             urgent′ ℓ_urgent′ (lastUrgentTicket m) (urgentCap m)
             standard′ ℓ_standard′ (lastStandardTicket m) (standardCap m)
             (ixOf standard′)
             (seenAddEB (seenEBs m) (ebTxs e)))
  else
    mkMempoolLP
      (tip m) (ledger m) (heldEB m) (ebLedger m)
      (urgentTxs m) (urgentUpdatedLedger m)
      (lastUrgentTicket m) (urgentCap m)
      (standardTxs m) (standardUpdatedLedger m)
      (lastStandardTicket m) (standardCap m)
      (conflictIx m)
      (seenAddEB (seenEBs m) (ebTxs e))

----------------------------------------------------------------------
-- 10. discardEB — NEW as an explicit event (implicit in Leios's
--     syncWithLedger via `stillLive`, but exposed as its own
--     handler here because it is the one "expensive undo" of a
--     prior addEB in the pricing model).
----------------------------------------------------------------------

discardEB : MempoolLP → MempoolLP
discardEB m =
  let ℓ_urgent′ , urgent′ = reapplyAllTk (ledger m)  (urgentTxs m)
      ℓ_standard′  , standard′  = reapplyAllTk ℓ_urgent′     (standardTxs m)
  in mkMempoolLP
       (tip m) (ledger m) nothing nothing
       urgent′ ℓ_urgent′ (lastUrgentTicket m) (urgentCap m)
       standard′  ℓ_standard′  (lastStandardTicket m)  (standardCap m)
       (ixOf standard′)
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
      urgent0 = filter keep (urgentTxs m)
      standard0  = filter keep (standardTxs m)
      ledger′ = ledgerAt p
      held′ = case heldEB m of λ where
                nothing  → nothing
                (just e) → if stillLive p e then just e else nothing
      ebL′ = case held′ of λ where
                nothing  → nothing
                (just e) → just (fst (reapplyAll ledger′ (ebTxs e)))
      base′            = fromMaybe ledger′ ebL′
      ℓ_urgent′ , urgent′  = reapplyAllTk base′   urgent0
      ℓ_standard′  , standard′   = reapplyAllTk ℓ_urgent′ standard0
  in mkMempoolLP
       p ledger′ held′ ebL′
       urgent′ ℓ_urgent′ (lastUrgentTicket m) (urgentCapAt p)
       standard′  ℓ_standard′  (lastStandardTicket m)  (standardCapAt  p)
       (ixOf standard′)
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
             ∧ (noneExpired p (urgentTxs m)
             ∧  noneExpired p (standardTxs m))
        then
          -- Scenario B, tick-rename path (O(1)): by the
          -- tick-commutation lemma (MempoolLeios §5) both working
          -- states are ticked in place; both tx sequences, tickets
          -- pass through unchanged (disjointness + noneExpired).
          mkMempoolLP
            p ledger′ nothing nothing
            (urgentTxs m) (tickTo p (urgentUpdatedLedger m))
            (lastUrgentTicket m) (urgentCapAt p)
            (standardTxs m) (tickTo p (standardUpdatedLedger m))
            (lastStandardTicket m) (standardCapAt p)
            (conflictIx m)
            (seenClear (seenEBs m))
        else
          -- Scenario B, reapply path (O(|urgent| + |standard|)):
          -- epoch boundary crossed or a tx may have expired — the
          -- mempool's own txs must be reapplied against the new
          -- ledger, in tier order.  No drop-filter over e.ebTxs
          -- (disjointness lemma); drops from expiry / epoch-boundary
          -- rule changes are possible.
          let ℓ_urgent′ , urgent′ = reapplyAllTk ledger′ (urgentTxs m)
              ℓ_standard′  , standard′  = reapplyAllTk ℓ_urgent′ (standardTxs m)
          in mkMempoolLP
               p ledger′ nothing nothing
               urgent′ ℓ_urgent′ (lastUrgentTicket m) (urgentCapAt p)
               standard′  ℓ_standard′  (lastStandardTicket m)  (standardCapAt  p)
               (ixOf standard′)
               (seenClear (seenEBs m)))
     else
       -- Scenario A: e's txs are now on-chain; drop them from
       -- both tiers; discard our heldEB.
       let ids   = map txId (ebTxs e)
           keep  = λ tk → if inTxIds ids (txId (tx tk)) then false else true
           urgent0 = filter keep (urgentTxs m)
           standard0  = filter keep (standardTxs m)
           ledger′            = ledgerAt p
           ℓ_urgent′ , urgent′    = reapplyAllTk ledger′ urgent0
           ℓ_standard′  , standard′     = reapplyAllTk ℓ_urgent′ standard0
       in mkMempoolLP
            p ledger′ nothing nothing
            urgent′ ℓ_urgent′ (lastUrgentTicket m) (urgentCapAt p)
            standard′  ℓ_standard′  (lastStandardTicket m)  (standardCapAt  p)
            (ixOf standard′)
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
      ℓ_urgent′ , urgent′  = reapplyAllTk base′   (urgentTxs m)
      ℓ_standard′  , standard′   = reapplyAllTk ℓ_urgent′ (standardTxs m)
  in mkMempoolLP
       p ledger′ held′ ebL′
       urgent′ ℓ_urgent′ (lastUrgentTicket m) (urgentCapAt p)
       standard′  ℓ_standard′  (lastStandardTicket m)  (standardCapAt  p)
       (ixOf standard′)
       (seenClear (seenEBs m))

----------------------------------------------------------------------
-- 14. Block forging — CHG: RB body is drawn from the urgent
--     tier, EB body from the standard tier.
----------------------------------------------------------------------

postulate
  splitAtCap  : Capacity → TxSeq → TxSeq × TxSeq
  nonEmpty    : TxSeq → Bool
  ebNonEmpty  : List Tx → Bool

-- Safe to call regardless of `heldEB`.  Each tier is reapplied
-- against the state it will actually meet on-chain: urgent txs
-- against `ledger` (RB body applies there), then the EB body
-- (urgent overflow followed by standard txs) against `rbLedger =
-- ledger + rbTxs`.  Urgent overflow that did not fit in the RB
-- body flows into the EB body; the ledger then charges an EB-landed
-- urgent tx on its ACTUAL (standard) tier — refunding the difference to a
-- feeChangeAddr if it named one, else donating the excess to the
-- treasury (see §1 "Fee on an urgent tx that lands in an EB").
-- The mempool state is unchanged; the reapplyAllTk calls produce
-- the emitted block only.
forgeBlock : MempoolLP → RB × Maybe EB
forgeBlock m =
  let -- 1. Revalidate urgent txs against `ledger` (not baseLedger).
      _ , validPrio           = reapplyAllTk (ledger m) (urgentTxs m)
      rbTxs , urgentOverflow    = splitAtCap (urgentCap m) validPrio
      -- 2. Post-RB state = ledgerAt(newRB).
      rbLedger , _            = reapplyAllTk (ledger m) rbTxs
      -- 3. EB body candidates: urgent overflow first (they paid
      --    the urgent-tier fee), then standard txs.  Revalidate the whole
      --    combined sequence against rbLedger; some may drop.
      ebCandidates            = urgentOverflow ++ standardTxs m
      _ , validEB             = reapplyAllTk rbLedger ebCandidates
      ebTxs′ , _              = splitAtCap (standardCap m) validEB
      -- 4. Light-load EB suppression (see §1): measure the EB body
      --    itself (ebTxs′) against the fullness floor ebThreshold = ½ a
      --    full RB. If the body is below ebThreshold in every dimension,
      --    do not announce an EB. (ebThreshold is the fullness *floor* — a
      --    lower bound, distinct from standardCap, the CIP-164 per-EB
      --    *capacity* upper bound used above in splitAtCap. Measuring
      --    ebTxs′ — the actual EB body, which is what the ledger's
      --    sdChecks sees — keeps this the exact complement of the
      --    ledger check.)
      --    NEW: age escape — if no EB has been announced on the
      --    chain for ebAgeEscape slots (ebOverdue), suppression is
      --    overridden and the EB is announced even below the floor.
      --    The EB must still be non-empty.
      lightLoad               = underThreshold (seqSize ebTxs′) (ebThresholdAt (tip m))
      suppress                = if ebOverdue (tip m)
                                  then false
                                  else lightLoad
      anyEB                   = if suppress
                                  then false
                                  else ebNonEmpty (map tx ebTxs′)
      newEBId                 = freshEBId (freshTicket (lastUrgentTicket m))
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
  `capacityAt` and `ebCap` replaced by tier-specific `urgentCapAt` /
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
2. **Exact conflict-key set.** Urgent admission relies on the
   commutativity theorem (§1), whose hypotheses `noConflict` is meant
   to discharge. Both `ConflictIndex` and `noConflict` are abstract
   here; the anticipated keying (non-consumed reads by `TxIn`,
   certificate targets and withdrawal credentials by `Credential`,
   deposit keys by `DepositPurpose`, vote targets by
   `(GovActionId, Voter)`, plus global scope for proposals and DRep
   (de)registration) must be confirmed *sufficient* — every way two
   txs can fail to commute is keyed — by the proof in
   `IntersectMBO/formal-ledger-specifications:polina/txcomm`. A missed
   conflict class is a soundness bug, not a missed optimisation.
   Also open: whether the ledger should *enforce* the predicate for
   RB-body txs, or whether it stays a mempool-side admission rule
   (a non-simple tx in an RB body is then merely a forgone
   optimization for the forger, not a validity issue).
3. **Held-EB selection with two tiers.** Which peer EB should the
   node hold if several arrive from the same announcing RB? The
   choice affects `ebLedger` and therefore the fate of every urgent
   tx currently in the tier. Modeled abstractly as `shouldHold`.
4. **EB-fullness floor: alignment with the ledger.** The fullness
   **floor** `ebThreshold` (= ½ a full RB) is a lower bound, distinct from
   the CIP-164 per-EB **capacity** `standardCap` (upper bound). The floor
   check here is the exact complement of the ledger's EB validity check
   (`sdChecks` for `EB`, via `BBODY`/`DIVUP` in
   `formal-ledger-specifications`): both measure the **EB body** against
   `ebThreshold` over the same four dimensions (byte size, ExUnits mem/CPU,
   ref-script bytes — the ledger's `totalRefScriptSize` vs
   `maxRefScriptSizePerBlock`), and suppress/reject only when it is
   small in **every** dimension, so an EB is suppressed here iff
   rejected there. Still open: (i) whether "small in every dimension" is
   the right quantifier — the alternative requires ≥ `ebThreshold` in
   *every* dimension (reject if small in any), and is **up for
   discussion**; (ii) whether `ebThreshold` should be a protocol parameter;
   (iii) enforcing the CIP-164 per-EB *capacity* (`standardCap` / `S_EB`,
   the upper bound) **ledger-side** — currently only the mempool caps
   the EB body by `standardCap`; the ledger bounds it only by the floor;
   (iv) the **age escape** is mirrored ledger-side
   (`formal-ledger-specifications`: `sdChecks EB` accepts an
   under-floor EB when `ebOverdue slot` holds; `ebAgeEscape` / `ebOverdue`
   are abstract in `AbstractFunctions`; `DIVUP`'s environment gains the
   block's slot, threaded from `BBODY`). Still open there: the voting
   committee must evaluate `ebOverdue` identically (same chain, same
   tip, same `ebAgeEscape`), and `ebAgeEscape` itself is a candidate
   protocol parameter.

## Changelog

- **2026-06-09** — Initial version. Extracted the two-tier content
  from the earlier `Mempool.lagda.md` §12, corrected the CIP-164
  alignment (RB body carries a certificate, not an EB reference;
  short EB lifetime; discard rule).
- **2026-06-09 (later)** — Aligned the ledger-stack naming with
  `MempoolLeios.lagda.md`: renamed `urgentLedger` →
  `urgentUpdatedLedger`, `ledger` (post-standard) →
  `standardUpdatedLedger`, `currentEB` → `heldEB`, added new
  `ledger : LedgerState` for the chain tip cache, changed
  `ebLedger : LedgerState` → `ebLedger : Maybe LedgerState`. Added
  Scenario B (matching-cert) bit-identical rename in `seeRBCert`.
  Added `discardEB` handler. Updated the revalidation-cascade table
  with explicit rows for Scenario A / B and `discardEB`.
- **2026-06-09 (later still)** — Fixed `forgeBlock` for the
  heldEB-at-forge case: urgent txs are reapplied against `ledger`
  (not `baseLedger`) before splitting into the RB body, and
  standard txs are reapplied against `ledger + rbTxs` (the actual
  post-RB ledger state) before splitting into the announced EB
  body. Dropped the earlier phase-1 "heldEB = nothing" precondition.
  Cost at forge: 2 × O(|urgentTxs|) + O(|standardTxs|). Mempool
  state is unchanged; txs that fail the ledger revalidation remain
  in their tier (still valid under `baseLedger` /
  `urgentUpdatedLedger`) and become forgeable once `heldEB` is
  resolved.
- **2026-06-09 (last)** — Added two design notes: (a) `forgeBlock`
  now emits urgent-tier *overflow* into the EB body ahead of
  standard txs, so an urgent tx that does not fit `urgentCap`
  reaches the chain via the announced EB rather than being
  discarded from the forged block; the ledger applies an urgent-
  vs-standard fee-differential refund to any urgent tx landing in
  an EB body (mempool preserves tier tag; ledger computes the
  refund). (b) Documented that this spec commits to option 2
  (unconditional standard-tier revalidation on urgent admission);
  the commutativity-based option 1 alternative is described inline
  with a pointer to the in-progress proof at
  `IntersectMBO/formal-ledger-specifications:polina/commutativity`.
- **2026-06-09 (also)** — Added §1 "Peer transaction exchange
  (network side)" documenting two tier-aware requirements on the
  tx-submission mini-protocol layer: (i) inbound txs must carry a
  tier tag so the receiver can dispatch to `addTx Urgent` or
  `addTx Standard`, and (ii) outbound pull is urgent-first with
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
  `forgeBlock` gains a `lightLoad = underThreshold combinedSize
  (urgentCap m)` guard on `anyEB`, so `maybeEB = nothing` whenever
  `seqSize (urgentTxs ++ standardTxs)` is at or below `urgentCap /
  2` in every dimension. New postulate `underThreshold : Capacity →
  Capacity → Bool` for the pointwise-half predicate. Threshold
  still `/ 2` (provisional; protocol-parameterisation left as a
  tuning question).
- **2026-07-13** — Cross-referenced the `underThreshold` EB-suppression
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
  `underThreshold` comment updated accordingly; `forgeBlock` guard comment
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
  in *every* dimension), matching `underThreshold`'s conjunction here. So
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
  dropping the `urgentCap / 2` form: `lightLoad = underThreshold (seqSize
  ebTxs′) (standardCap m)`. Since the ½ lives in `standardCap`, there is no
  doubling/rounding; `underThreshold` reverts to `size[d] < cap[d]`. This
  makes suppression the exact complement of the ledger's `sdChecks EB`
  (both measure the EB body against `standardCap`), so an EB is suppressed
  here iff the ledger would reject it. `combinedSize` removed; §1,
  `standardCapAt`, `underThreshold`, and Open question 4 updated. This *is* a
  behavioural change to the sketch (suppression predicate now over
  `ebTxs′`).
- **2026-07-13 (last)** — Un-conflated capacity vs floor. `standardCap`
  (`standardCapAt`) is restored to its CIP-164 meaning — the per-EB
  *capacity* (upper bound, `S_EB` etc.) that caps the EB body via
  `splitAtCap`. A NEW postulate `ebThresholdAt : TipPoint → Capacity` is the
  EB-fullness *floor* (lower bound, = ½ a full RB), used by the
  suppression guard: `underThreshold (seqSize ebTxs′) (ebThresholdAt (tip m))`.
  The floor is a design choice (not a CIP-164 requirement beyond "no
  empty EBs"), still up for discussion, and a candidate protocol
  parameter; reachability needs `ebThreshold ≤ standardCap`. §1, the postulate
  comments, and Open question 4 updated; noted that enforcing the
  CIP-164 per-EB capacity ledger-side is a TODO.
- **2026-07-24** — Terminology alignment: fast → urgent, slow →
  standard, lane → tier, applied throughout this document and the
  siblings (identifiers, prose, and historical changelog entries
  alike, so the old terms no longer appear anywhere). Also removed
  the last "higher tier" / "lower tier" phrasings in favour of
  urgent / standard.
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
  as **urgent / standard**.
- **2026-07-29** — Added the **EB age escape**: a constant `ebAgeEscape`
  (slots, candidate protocol parameter) bounds light-load EB
  suppression in time. If no EB has been announced on the chain for
  `ebAgeEscape` slots (`ebOverdue`, new postulate alongside `ebAgeEscape`),
  `forgeBlock` announces the EB even below the fullness floor
  `ebThreshold` (still never empty). Guards against unbounded
  standard-tier latency under sustained light load. §1 subsection,
  `forgeBlock` step 4 (`suppress` guard), and Open question 4 updated
  — the ledger-side floor check needs the same escape. Confirmed the
  base Leios spec has no underfull-EB constraint to begin with
  (`forgeBlock` there emits an EB for any non-empty overflow), so
  nothing to remove on that side.
- **2026-07-29 (later)** — Ledger-side age escape implemented in
  `formal-ledger-specifications` (branch `polina/dynamic`):
  `AbstractFunctions` gains `ebAgeEscape : ℕ` and the chain-history
  oracle `ebOverdue : Slot → Bool`; `sdChecks` takes the block's slot
  and its `EB` case gains the disjunct `ebOverdue slot ≡ true`;
  `DIVUP`'s environment becomes `PParams × BlockType × Slot`, with
  the slot threaded from `bhb .slot` in `BBODY`. The
  suppressed-iff-rejected complement between `underThreshold` (mempool)
  and `sdChecks EB` (ledger) is preserved, escape included.
  *(Superseded — see the entry below: the escape is now counted in
  Ranking Blocks via ledger state, not in slots via an oracle.)*
- **2026-07-30** — Switched urgent admission to **commutative
  admission**, gated on a new `SimpleTx` predicate. `addTx Urgent` now
  performs two single-tx validations — **first at the back of the
  standard queue** (`standardUpdatedLedger`, the full mempool
  post-state; previously the first validation ran at the back of the
  urgent queue), then at the insertion point (`urgentUpdatedLedger`,
  still needed since a tx spending a standard tx's output is valid at
  the full post-state but not at the insertion point) — and drops the
  standard-tier revalidation entirely. Cascade-table row "urgent tx
  added" is now `— / extend / — (commutes)`; all other urgent-touching
  events still revalidate the standard tier unconditionally.
  `StandardLayerValid` is maintained at admission by the theorem
  rather than by direct reapply. Rewrote the former "option 1 /
  option 2" discussion as the adopted rule.
  *(The `SimpleTx` gate is superseded — see the entry below.)*
- **2026-08-21** — Replaced the structural admission gates with
  **conflict-gated admission**. `SimpleTx` (on the incoming urgent tx)
  and `SpendOnly` (on every standard tx) are gone, along with the
  `UrgentSimple` invariant. In their place `MempoolLP` carries
  `conflictIx : ConflictIndex` — the union of the standard queue's
  conflict footprints, keyed by non-consumed reads (`TxIn`),
  certificate targets and withdrawal credentials (`Credential`),
  deposit keys (`DepositPurpose`) and vote targets
  (`GovActionId × Voter`) — maintained by `ixAdd` at standard
  admission and rebuilt by `ixOf` wherever the standard queue is
  revalidated wholesale (new invariant `IxMatchesStandard`).
  `addTx Urgent` now gates on `noConflict (conflictIx m) t` instead of
  `SimpleTx t`, so **any transaction shape may be urgent** provided it
  does not conflict, and a conflicting urgent tx is discarded rather
  than admitted by evicting standard txs. Rationale: the structural
  predicates made the theorem's hypotheses hold vacuously by banning
  shapes, which over-approximates (a certificate-bearing tx commutes
  fine past a queue that does not touch its credential) and, worse,
  contradicted the CIP — `noGovProps` barred governance proposals from
  the urgent tier, while Praos-mode fallback feeds Ranking Blocks from
  the urgent queue alone, so a standard-tier proposal would be
  censored for the fallback's duration. Proposals and DRep
  (de)registration conflict *globally* (they re-filter all pending
  votes) so they have no pairwise key; they are urgent-only by tier
  restriction, and `noConflict` treats a non-empty `voteTargets` as a
  conflict for them. Open question 2 now asks for the exact
  conflict-key set rather than the `SimpleTx` field-set, and the proof
  reference is `polina/txcomm`.
