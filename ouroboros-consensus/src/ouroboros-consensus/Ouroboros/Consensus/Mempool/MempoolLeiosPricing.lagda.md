# Cardano Mempool — Linear Leios with Tiered Pricing

*A design sketch for adding tiered (fast / slow) pricing to the
Linear Leios mempool. Two tiers: fast-tier transactions are
destined for a Ranking Block body, slow-tier transactions for the
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

**Last updated:** 2026-06-09
**Primary reference:** CIP-164 Ouroboros Linear Leios,
<https://github.com/cardano-foundation/CIPs/tree/master/CIP-0164>
**Sibling ref:** `MempoolLeios.lagda.md` in this directory (shared
context on Leios EB/RB semantics; not a build-time dependency).

## 0. What changes from `MempoolLeios` (executive summary)

The Leios mempool holds a single sequence of transactions validated
against `updatedLedger = (ledger + heldEB.txs) + txs`. The tiered-
pricing mempool splits that sequence into two tiers:

- **Fast tier** (`fastTxs`) — transactions that pay the higher
  tier and are guaranteed a place in an RB body if room exists at the
  next forging opportunity.
- **Slow tier** (`slowTxs`) — transactions that pay the lower
  tier and are eligible only for the overflow EB.

This split is a *mempool-side* extension. **CIP-164 does not define
any fast / slow distinction**; it only expresses an implicit
preference ("EBs should only be announced if a transaction cannot be
included in the base RB… the protocol will naturally incentivize
usage of RBs over EBs"). This document commits to a stronger,
explicit contract: the tier a transaction lives in determines *which
kind of block* it can end up in, and validation is arranged so that
the fast tier sees a ledger state that already accounts for the
EB the mempool currently holds.

### Structural differences vs. `MempoolLeios`

| Concept | `MempoolLeios` | This file |
|---|---|---|
| chain tip cache | `ledger` | `ledger` (unchanged) |
| held EB | `heldEB : Maybe EB` | `heldEB : Maybe EB` (unchanged) |
| tip + held EB applied | `ebLedger : Maybe LedgerState` | `ebLedger : Maybe LedgerState` (unchanged) |
| mempool working state | `updatedLedger` | *split into two:* `fastUpdatedLedger`, `slowUpdatedLedger` |
| tx sequence | `txs` | *split into two:* `fastTxs`, `slowTxs` |
| capacity | `capacity` | *split into two:* `fastCap`, `slowCap` |
| ticket counter | `lastTicket` | *split into two:* `lastFastTicket`, `lastSlowTicket` |
| reuse cache | `seenEBs` | `seenEBs` (unchanged) |

So the *ledger stack* (`ledger`, `heldEB`, `ebLedger`) is imported
without change, and everything below it — the working state, the tx
sequence, the capacity, and the tickets — is doubled. That layout is
what makes the pricing extension a genuine extension rather than an
architectural break.

### Behavioural differences vs. `MempoolLeios`

- **Admission (`addTx`).** Now takes a `Tier` argument. Fast
  admissions cascade: any change to `fastUpdatedLedger` triggers
  slow-tier revalidation (see §1 for the CIP-mirror argument).
- **EB acceptance (`addEB`).** Same as Leios in spirit — recompute
  `ebLedger`, revalidate — but now the revalidation runs through both
  tiers in sequence.
- **Chain events (`seeRBBody`, `seeRBCert`, `syncWithLedger`).**
  Same shape as Leios. `seeRBCert`'s **Scenario B** (cert matches
  our held EB) is still a bit-identical rename; the two tiers'
  working states are preserved as-is, cost O(1).
- **Discard (`discardEB`).** Same event as Leios; cascades through
  both tiers.
- **Block forging (`forgeBlock`).** Fast tier → RB body;
  slow tier → overflow EB body. The split is not a forge-time
  partition as it is in Leios; the tiers are stored separately by
  design.

## 1. Design summary

**Fast tier.** Transactions submitted with the higher tier. Each
fast tx is validated against `fastUpdatedLedger = (ledger +
heldEB.txs) + all prior fast txs`, so it sees the cumulative
effect of the fasts already admitted plus the EB currently held.
`fastCap` is the total `TxMeasure` of a single Ranking Block, taken
from protocol parameters.

**Slow tier.** Transactions submitted with the lower tier. Each
slow tx is validated against `slowUpdatedLedger =
fastUpdatedLedger + all prior slow txs` — the cumulative
slow-tier post-state. `slowCap` is a separate EB capacity derived
from the CIP's per-EB caps (`S_EB`, `S_EB-tx`, per-EB Plutus limits).

**Held EB.** The node keeps at most one EB (`heldEB`) — either its
own recently-forged EB awaiting a certificate window, or a peer's
announced EB whose eventual certification is worth pre-validating
for. When an EB is held, `ebLedger = just (ledger + heldEB.ebTxs)`;
otherwise `ebLedger = nothing`.

**Application order.** Chain semantics fix a single canonical
application order — `ledgerAt(oldTip) + certified EB (if any) + RB
body`, and if a later RB's certificate applies our held EB, its
transactions land before any RB body fasts from that later RB.
This mempool mirrors that order in its layered ledger states so that
every stored transaction is valid against the exact state it will
meet on-chain.

### Capacity rules

- **Ranking Block / fast-tier limit.** One block's `TxMeasure`
  from protocol parameters: byte size, script ExUnits mem, script
  ExUnits CPU, reference-script bytes.
- **EB / slow-tier limit.** CIP-164's per-EB caps: `S_EB`
  (structure), `S_EB-tx` (referenced txs), per-EB Plutus step and
  memory. These are distinct dimensions from the RB caps.

### Block production

The lottery is the standard Praos VRF slot-leader election — a single
lottery, not one per block kind. Its winner produces:

- **An RB.** Body is either `fastTxs` (a plain-tx body) or a
  certificate for a previously-announced EB. These are mutually
  exclusive (CIP-164: "when a certificate is included, no further
  transactions are allowed in the RB").
- **An EB, optionally.** Body is drawn from `slowTxs`, plus any
  fast-tier overflow that did not fit within `fastCap` in the
  RB body. Announced in the RB header. Must be non-empty (CIP-164:
  "empty EBs should not be announced"). Additionally, `forgeBlock`
  suppresses EB emission under **light load** — see the subsection
  below.

**Fee on a fast tx that lands in an EB.** A fast-tier transaction that
ends up in an EB body — rather than in the RB body — has paid for fast
service (direct RB inclusion at its announcing slot) but did not
receive it (EB inclusion is subject to the vote/certificate flow and
the minimum inclusion delay). The ledger charges and refunds it on the
tier it *actually* lands in (slow, for an EB), **not** its claimed
(fast) tier: with `actualCoeff = slowCoeff`, if the tx named a
`feeChangeAddr` it is charged the slow-tier fee and refunded
`txFee − slowCoeff·minfee` to that address; if it named none, the
excess above `minfee` is donated to the treasury instead (no refund).
The *admission check*, by contrast, used the tx's **claimed** (fast)
tier (`tier.tierCoeff·minfee ≤ txFee`). The mempool itself does not
compute this; it only preserves the tier tag on each emitted tx, and
the ledger's fee split does the actual-tier charge/refund (see
`Utxo.lagda.md` / `Tiers.lagda.md` in the Cardano ledger repo).

An announced EB does not affect the ledger by itself. It goes through
the CIP-164 vote/certificate flow: the elected voting committee
validates it against `ledgerAt(announcingRB)`, votes are aggregated,
and a *later* RB `R'` — at least `3·L_hdr + L_vote + L_diff` slots
after the announcing RB — may include the certificate in its body.
Only when `R'` is adopted do the EB's (slow-tier) transactions
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
step drops any fast tx that speculatively depended on
`heldEB.ebTxs`. The mempool *state* is unchanged; such txs stay in
`fastTxs`, still valid under `baseLedger`, and become forgeable
once `heldEB` is resolved (either certified via Scenario B —
after which they are valid under the new `ledger` too — or
discarded, after which they either survive the cascade or drop out).
The slow tier is handled symmetrically: its contents are
reapplied against the post-RB state `ledgerAt(newRB) = ledger +
rbTxs`. Cost: 2 × O(|fastTxs|) + O(|slowTxs|) at forge time.

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
This is a *lower* bound, distinct from `slowCap`, the CIP-164 per-EB
**capacity** (`S_EB`, …) that bounds the EB body from *above* via
`splitAtCap`. The floor is a design choice (not a CIP-164 requirement
beyond "no empty EBs") and a candidate protocol parameter; for it to
be reachable we need `ebFloor ≤ slowCap` in every dimension.

Rationale: EBs carry real costs (a voting round, a certification
delay, and additional propagation load), so they only earn their keep
when the overflow that lands in the EB reaches the floor in some
dimension. A near-empty EB (colliding with CIP-164's "empty EBs should
not be announced" rule) is never announced.

Implementation: one additional guard in the `anyEB` computation of
`forgeBlock` (see §2), postulated in the Agda sketch as
`underHalfRB : Capacity → Capacity → Bool`.

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

(Separately, the CIP-164 per-EB *capacity* `slowCap` is not yet enforced
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
  `addTx Fast` or `addTx Slow` accordingly. Without tier
  awareness on the wire, a fast-tier tx received from a peer
  could end up in the wrong tier and lose fast service; the
  pricing model is unenforceable end-to-end.

- **Outbound: fast-first, with fallback.** The local node's
  tx-fetch policy toward each peer is: keep asking for
  fast-tier txs, and only fall back to requesting slow-tier
  txs when the fast request comes back empty (or when the
  peer has no more fast txs to offer). This biases scarce
  network capacity toward the tier that pays for it and matches
  the local mempool's own fast-over-slow preference in
  admission and block production. The wire format for the
  bifurcated pull is out of scope for this doc; the mempool
  merely commits to the *policy* that its outbound requests are
  fast-first with slow-only fallback on empty.

Streaming to peers via `snapshotTxsAfter` (Praos §4 in
`Mempool.lagda.md`) becomes tier-aware in the same way: a peer's
cursor is a pair `(lastFastTicket, lastSlowTicket)`, and the peer
chooses which tier's cursor to advance based on the pull policy
above.

### Revalidation cascade

Which event revalidates which layer:

| Event | `heldEB` / `ebLedger` | Fast tier | Slow tier | Cost |
|---|---|---|---|---|
| `addEB` (adopt peer EB) | set, rebuild | revalidate | revalidate | O(\|fast\| + \|slow\| + \|EB\|) |
| fast tx added | — | extend | revalidate | O(\|slow\|) |
| fast tx removed | — | revalidate | revalidate | O(\|fast\| + \|slow\|) |
| slow tx added | — | — | extend | O(1) |
| slow tx removed | — | — | revalidate | O(\|slow\|) |
| `discardEB` | clear | revalidate | revalidate | O(\|fast\| + \|slow\|) |
| `seeRBBody ts p` | maybe discard via `stillLive` | drop referenced + revalidate | drop referenced + revalidate | O(\|fast\| + \|slow\| + \|ts\|) |
| `seeRBCert e p` **(match)** | clear | **bit-identical rename** | **bit-identical rename** | **O(1)** |
| `seeRBCert e p` **(no match)** | discard | drop `e.ebTxs` + revalidate | drop `e.ebTxs` + revalidate | O(\|fast\| + \|slow\| + \|e.ebTxs\|) |
| `syncWithLedger p` | maybe discard via `stillLive` | revalidate | revalidate | O(\|fast\| + \|slow\| + \|heldEB.ebTxs\|) |

The **"fast tx added → slow revalidate"** entry is required,
not an optimization choice. The chain semantics fix a canonical
application order (`ebLedger + fasts + slows`) and the slow
tier's validity invariant is "valid against `fastUpdatedLedger =
ebLedger + fasts`". Any change to `fastUpdatedLedger` —
including a single new fast tx — re-opens that invariant. Even
when a new fast tx is input-disjoint from every slow tx, the
slow tier is re-applied on top of the updated
`fastUpdatedLedger` so the spec never depends on a case-by-case
commutativity argument over the full ledger state (governance, stake,
parameter updates, script reference reads, etc.). An implementation
is free to fast-path provably independent additions, but the
canonical invariant is unconditional revalidation.

#### Alternative: commutative admission ("option 1")

A leaner rule for `addTx Fast t` would validate `t` against
**both** `fastUpdatedLedger` and `slowUpdatedLedger`, admitting
only if both succeed. On success, the slow tier provably remains
valid after the state shift `fastUpdatedLedger → applyTx
fastUpdatedLedger t`, so the O(|slows|) slow revalidation drops
away. Admission is O(|slows|) at check time (running the tx against
both bases) but the state transition itself becomes O(1).

The soundness of this rule depends on **transaction commutativity**:
`t` composed after the slow sequence must produce the same ledger
state as the slow sequence composed after `t`. Cardano txs in
general do *not* commute (they can share stake credentials, reference
inputs, governance targets, protocol-parameter updates, script
context reads, etc.), so option 1 requires structural constraints on
fast txs to guarantee commutativity across every ledger dimension.

The constraints and the commutativity proof itself are being worked
out in
<https://github.com/IntersectMBO/formal-ledger-specifications/compare/polina/commutativity?expand=1>.
Until those constraints are pinned down, **this spec stays with
option 2** (the unconditional-revalidate rule in the cascade table
above). Once the proof lands, we may switch — the switch is local to
`addTx Fast`: the `fast tx added` row becomes `— / extend /
—`, the `Cost` column drops to O(|slows|) at admission and O(1) for
the state update, and the `SlowLayerValid` invariant relies on
the commutativity theorem rather than a direct reapply.

The **Scenario B row** (`seeRBCert (match)`) is a bit-identical
rename: because the mempool has been pre-validating both tiers
against a state that includes `heldEB.ebTxs`, and `ledgerAt(newTip)`
equals exactly that state when the cert is for our held EB, no
`applyTx` / `reapplyAll` calls are required. Only the field bindings
shift (`ledger := old ebLedger.value`, `heldEB := nothing`,
`ebLedger := nothing`, everything else unchanged).

The **Scenario A row** (`seeRBCert (no match)`) is the **phase-1**
behaviour: unconditional pruning of `E.ebTxs` from both tiers,
followed by full revalidation. A future phase may explore partial
revalidation (skip when we can prove `E.ebTxs` is input-disjoint
from `heldEB.ebTxs`, `fastTxs`, and `slowTxs`), but the
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
--    caps (`fastCapAt` for RB body limit, `slowCapAt` for EB body
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
  fastCapAt    : TipPoint → Capacity
  -- slowCapAt is the EB *capacity* (upper bound): the CIP-164 per-EB caps
  -- (S_EB, S_EB-tx, per-EB Plutus). Used to cap the EB body via splitAtCap.
  slowCapAt     : TipPoint → Capacity
  -- NEW: ebFloorAt is the EB-fullness *floor* (lower bound) — a SEPARATE
  -- quantity from slowCap. An EB must reach it in some dimension or it is
  -- suppressed (here) / rejected (ledger). Intended value: ½ a full RB
  -- (½ · fastCapAt) per dimension. This floor is a design choice — NOT a
  -- CIP-164 requirement (the CIP only forbids empty EBs) — so it is probably
  -- up for discussion, and is a candidate protocol parameter. For the floor
  -- to be reachable we need ebFloor ≤ slowCap in every dimension.
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
  Fast : Tier
  Slow  : Tier

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
--      txs                      fastTxs, slowTxs    (split)
--      updatedLedger            fastUpdatedLedger,
--                               slowUpdatedLedger        (split)
--      lastTicket               lastFastTicket, lastSlowTicket
--      capacity                 fastCap, slowCap
--      seenEBs                  seenEBs                    (same)
----------------------------------------------------------------------

record MempoolLP : Set where
  constructor mkMempoolLP
  field
    tip                   : TipPoint
    ledger                : LedgerState        -- ledgerAt tip
    heldEB                : Maybe EB
    ebLedger              : Maybe LedgerState  -- ledger + heldEB.ebTxs

    -- NEW: fast tier, replaces Leios's `txs`.
    fastTxs           : TxSeq
    -- NEW: fast working state, = (fromMaybe ledger ebLedger) + fast.
    -- Same role as Leios's `updatedLedger` — this is what a new
    -- fast tx validates against.
    fastUpdatedLedger : LedgerState
    lastFastTicket        : TicketNo           -- CHG: was lastTicket
    fastCap               : Capacity           -- CHG: was capacity (RB TxMeasure)

    -- NEW: slow tier.
    slowTxs            : TxSeq
    -- NEW: slow working state, = fastUpdatedLedger + slows.
    -- What a new slow tx validates against.
    slowUpdatedLedger  : LedgerState
    lastSlowTicket         : TicketNo
    slowCap                : Capacity           -- EB-specific cap

    seenEBs               : SeenSet
open MempoolLP

-- Convenience: the base ledger for fast-tier validation.
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
  FastLayerValid :
    (m : MempoolLP) →
    fst (reapplyAllTk (baseLedger m) (fastTxs m))
    ≡ fastUpdatedLedger m

  -- NEW: second half of the layered invariant.
  SlowLayerValid :
    (m : MempoolLP) →
    fst (reapplyAllTk (fastUpdatedLedger m) (slowTxs m))
    ≡ slowUpdatedLedger m

----------------------------------------------------------------------
-- 8. addTx — CHG: now takes a Tier.
--
--    Fast tier: validated against `fastUpdatedLedger`
--      (cumulative).  Any successful admission updates
--      `fastUpdatedLedger` and REVALIDATES the slow tier.
--      This is the Leios-compat invariant: the slow tier must
--      always be valid against `ebLedger + fasts`, and
--      fasts have just changed.
--
--    Slow tier: validated against `slowUpdatedLedger`
--      (cumulative slow post-state); admission does not touch
--      the fast tier.
----------------------------------------------------------------------

data AddResult : Set where
  Added    : MempoolLP → AddResult
  Rejected : MempoolLP → AddResult
  Blocked  : MempoolLP → AddResult

addTx : Tier → Tx → MempoolLP → AddResult

addTx Fast t m
  with fitsWith (fastCap m) (seqSize (fastTxs m)) (measure t)
... | false = Blocked m
... | true  with applyTx (fastUpdatedLedger m) t
...   | nothing = Rejected m
...   | just ℓ_fast′ =
        let n′            = freshTicket (lastFastTicket m)
            tk            = mkTicket t n′ (measure t)
            -- CRUCIAL: slow tier revalidates against new
            -- fastUpdatedLedger.  See §1 "fast tx added →
            -- slow revalidate".
            ℓ_slow′ , slow′ = reapplyAllTk ℓ_fast′ (slowTxs m)
        in Added (mkMempoolLP
             (tip m) (ledger m) (heldEB m) (ebLedger m)
             (fastTxs m ++ tk ∷ []) ℓ_fast′ n′ (fastCap m)
             slow′ ℓ_slow′ (lastSlowTicket m) (slowCap m)
             (seenEBs m))

addTx Slow t m
  with fitsWith (slowCap m) (seqSize (slowTxs m)) (measure t)
... | false = Blocked m
... | true  with applyTx (slowUpdatedLedger m) t
...   | nothing = Rejected m
...   | just ℓ_slow′ =
        let n′ = freshTicket (lastSlowTicket m)
            tk = mkTicket t n′ (measure t)
        in Added (mkMempoolLP
             (tip m) (ledger m) (heldEB m) (ebLedger m)
             (fastTxs m) (fastUpdatedLedger m)
             (lastFastTicket m) (fastCap m)
             (slowTxs m ++ tk ∷ []) ℓ_slow′ n′ (slowCap m)
             (seenEBs m))

----------------------------------------------------------------------
-- 9. addEB — CHG: cascades through both tiers.
--
--    Same shape as Leios's `addEB` (rebuild ebLedger, revalidate),
--    but revalidation now flows fast-tier → slow-tier in
--    sequence.
----------------------------------------------------------------------

postulate
  shouldHold : MempoolLP → EB → Bool

addEB : EB → MempoolLP → MempoolLP
addEB e m =
  if shouldHold m e
  then (let ebL′            = fst (reapplyAll (ledger m) (ebTxs e))
            ℓ_fast′ , fast′ = reapplyAllTk ebL′  (fastTxs m)
            ℓ_slow′ , slow′ = reapplyAllTk ℓ_fast′ (slowTxs m)
        in mkMempoolLP
             (tip m) (ledger m) (just e) (just ebL′)
             fast′ ℓ_fast′ (lastFastTicket m) (fastCap m)
             slow′ ℓ_slow′ (lastSlowTicket m) (slowCap m)
             (seenAddEB (seenEBs m) (ebTxs e)))
  else
    mkMempoolLP
      (tip m) (ledger m) (heldEB m) (ebLedger m)
      (fastTxs m) (fastUpdatedLedger m)
      (lastFastTicket m) (fastCap m)
      (slowTxs m) (slowUpdatedLedger m)
      (lastSlowTicket m) (slowCap m)
      (seenAddEB (seenEBs m) (ebTxs e))

----------------------------------------------------------------------
-- 10. discardEB — NEW as an explicit event (implicit in Leios's
--     syncWithLedger via `stillLive`, but exposed as its own
--     handler here because it is the one "expensive undo" of a
--     prior addEB in the pricing model).
----------------------------------------------------------------------

discardEB : MempoolLP → MempoolLP
discardEB m =
  let ℓ_fast′ , fast′ = reapplyAllTk (ledger m)  (fastTxs m)
      ℓ_slow′  , slow′  = reapplyAllTk ℓ_fast′     (slowTxs m)
  in mkMempoolLP
       (tip m) (ledger m) nothing nothing
       fast′ ℓ_fast′ (lastFastTicket m) (fastCap m)
       slow′  ℓ_slow′  (lastSlowTicket m)  (slowCap m)
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
      fast0 = filter keep (fastTxs m)
      slow0  = filter keep (slowTxs m)
      ledger′ = ledgerAt p
      held′ = case heldEB m of λ where
                nothing  → nothing
                (just e) → if stillLive p e then just e else nothing
      ebL′ = case held′ of λ where
                nothing  → nothing
                (just e) → just (fst (reapplyAll ledger′ (ebTxs e)))
      base′            = fromMaybe ledger′ ebL′
      ℓ_fast′ , fast′  = reapplyAllTk base′   fast0
      ℓ_slow′  , slow′   = reapplyAllTk ℓ_fast′ slow0
  in mkMempoolLP
       p ledger′ held′ ebL′
       fast′ ℓ_fast′ (lastFastTicket m) (fastCapAt p)
       slow′  ℓ_slow′  (lastSlowTicket m)  (slowCapAt  p)
       (seenClear (seenEBs m))

----------------------------------------------------------------------
-- 12. seeRBCert — CHG: as in MempoolLeios, but the Scenario B
--     rename preserves BOTH tiers at zero cost, and the Scenario A
--     path cascades revalidation through both tiers.
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
       -- fastUpdatedLedger and slowUpdatedLedger are still
       -- valid working states; only the ledger-stack fields
       -- shift.  Fast and slow txs / caps / tickets pass
       -- through unchanged.
       mkMempoolLP
         p (fromMaybe (ledger m) (ebLedger m)) nothing nothing
         (fastTxs m) (fastUpdatedLedger m)
         (lastFastTicket m) (fastCapAt p)
         (slowTxs m) (slowUpdatedLedger m)
         (lastSlowTicket m) (slowCapAt p)
         (seenClear (seenEBs m))
     else
       -- Scenario A: e's txs are now on-chain; drop them from
       -- both tiers; discard our heldEB.
       let ids   = map txId (ebTxs e)
           keep  = λ tk → if inTxIds ids (txId (tx tk)) then false else true
           fast0 = filter keep (fastTxs m)
           slow0  = filter keep (slowTxs m)
           ledger′            = ledgerAt p
           ℓ_fast′ , fast′    = reapplyAllTk ledger′ fast0
           ℓ_slow′  , slow′     = reapplyAllTk ℓ_fast′ slow0
       in mkMempoolLP
            p ledger′ nothing nothing
            fast′ ℓ_fast′ (lastFastTicket m) (fastCapAt p)
            slow′  ℓ_slow′  (lastSlowTicket m)  (slowCapAt  p)
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
      ℓ_fast′ , fast′  = reapplyAllTk base′   (fastTxs m)
      ℓ_slow′  , slow′   = reapplyAllTk ℓ_fast′ (slowTxs m)
  in mkMempoolLP
       p ledger′ held′ ebL′
       fast′ ℓ_fast′ (lastFastTicket m) (fastCapAt p)
       slow′  ℓ_slow′  (lastSlowTicket m)  (slowCapAt  p)
       (seenClear (seenEBs m))

----------------------------------------------------------------------
-- 14. Block forging — CHG: RB body is drawn from the fast
--     tier, EB body from the slow tier.
----------------------------------------------------------------------

postulate
  splitAtCap  : Capacity → TxSeq → TxSeq × TxSeq
  nonEmpty    : TxSeq → Bool
  ebNonEmpty  : List Tx → Bool

-- Safe to call regardless of `heldEB`.  Each tier is reapplied
-- against the state it will actually meet on-chain: fasts
-- against `ledger` (RB body applies there), then the EB body
-- (fast overflow followed by slows) against `rbLedger =
-- ledger + rbTxs`.  Fast overflow that did not fit in the RB
-- body flows into the EB body; the ledger then charges an EB-landed
-- fast tx on its ACTUAL (slow) tier — refunding the difference to a
-- feeChangeAddr if it named one, else donating the excess to the
-- treasury (see §1 "Fee on a fast tx that lands in an EB").
-- The mempool state is unchanged; the reapplyAllTk calls produce
-- the emitted block only.
forgeBlock : MempoolLP → RB × Maybe EB
forgeBlock m =
  let -- 1. Revalidate fasts against `ledger` (not baseLedger).
      _ , validPrio           = reapplyAllTk (ledger m) (fastTxs m)
      rbTxs , fastOverflow    = splitAtCap (fastCap m) validPrio
      -- 2. Post-RB state = ledgerAt(newRB).
      rbLedger , _            = reapplyAllTk (ledger m) rbTxs
      -- 3. EB body candidates: fast overflow first (they paid
      --    the higher tier), then slows.  Revalidate the whole
      --    combined sequence against rbLedger; some may drop.
      ebCandidates            = fastOverflow ++ slowTxs m
      _ , validEB             = reapplyAllTk rbLedger ebCandidates
      ebTxs′ , _              = splitAtCap (slowCap m) validEB
      -- 4. Light-load EB suppression (see §1): measure the EB body
      --    itself (ebTxs′) against the fullness floor ebFloor = ½ a
      --    full RB. If the body is below ebFloor in every dimension,
      --    do not announce an EB. (ebFloor is the fullness *floor* — a
      --    lower bound, distinct from slowCap, the CIP-164 per-EB
      --    *capacity* upper bound used above in splitAtCap. Measuring
      --    ebTxs′ — the actual EB body, which is what the ledger's
      --    sdChecks sees — keeps this the exact complement of the
      --    ledger check.)
      lightLoad               = underHalfRB (seqSize ebTxs′) (ebFloorAt (tip m))
      anyEB                   = if lightLoad
                                  then false
                                  else ebNonEmpty (map tx ebTxs′)
      newEBId                 = freshEBId (freshTicket (lastFastTicket m))
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
  `capacityAt` and `ebCap` replaced by tier-specific `fastCapAt` /
  `slowCapAt`.
- **Scenario B in code.** `seeRBCert` when `matches = true` does not
  call `reapplyAll` at all — it only rebinds the ledger-stack fields
  and preserves both tiers' working states. The disjointness lemma
  (in `MempoolLeios.lagda.md` §5) generalises unchanged: neither
  tier may contain a duplicate of a `heldEB.ebTxs` entry.
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
2. **Independent-tx fast path for `addTx Fast`.** The
   revalidation of the slow tier on every fast admission is a
   canonical-invariant requirement, but an implementation may skip it
   when it can prove independence (disjoint inputs, reference inputs,
   collateral, stake certs, governance targets, and parameter
   effects). The cost/benefit depends on the shape of real workloads.
3. **Held-EB selection with two tiers.** Which peer EB should the
   node hold if several arrive from the same announcing RB? The
   choice affects `ebLedger` and therefore the fate of every fast
   tx currently in the tier. Modeled abstractly as `shouldHold`.
4. **EB-fullness floor: alignment with the ledger.** The fullness
   **floor** `ebFloor` (= ½ a full RB) is a lower bound, distinct from
   the CIP-164 per-EB **capacity** `slowCap` (upper bound). The floor
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
   (iii) enforcing the CIP-164 per-EB *capacity* (`slowCap` / `S_EB`,
   the upper bound) **ledger-side** — currently only the mempool caps
   the EB body by `slowCap`; the ledger bounds it only by the floor.

## Changelog

- **2026-06-09** — Initial version. Extracted the two-tier content
  from the earlier `Mempool.lagda.md` §12, corrected the CIP-164
  alignment (RB body carries a certificate, not an EB reference;
  short EB lifetime; discard rule).
- **2026-06-09 (later)** — Aligned the ledger-stack naming with
  `MempoolLeios.lagda.md`: renamed `fastLedger` →
  `fastUpdatedLedger`, `ledger` (post-slow) →
  `slowUpdatedLedger`, `currentEB` → `heldEB`, added new
  `ledger : LedgerState` for the chain tip cache, changed
  `ebLedger : LedgerState` → `ebLedger : Maybe LedgerState`. Added
  Scenario B (matching-cert) bit-identical rename in `seeRBCert`.
  Added `discardEB` handler. Updated the revalidation-cascade table
  with explicit rows for Scenario A / B and `discardEB`.
- **2026-06-09 (later still)** — Fixed `forgeBlock` for the
  heldEB-at-forge case: fasts are reapplied against `ledger`
  (not `baseLedger`) before splitting into the RB body, and
  slows are reapplied against `ledger + rbTxs` (the actual
  post-RB ledger state) before splitting into the announced EB
  body. Dropped the earlier phase-1 "heldEB = nothing" precondition.
  Cost at forge: 2 × O(|fastTxs|) + O(|slowTxs|). Mempool
  state is unchanged; txs that fail the ledger revalidation remain
  in their tier (still valid under `baseLedger` /
  `fastUpdatedLedger`) and become forgeable once `heldEB` is
  resolved.
- **2026-06-09 (last)** — Added two design notes: (a) `forgeBlock`
  now emits fast-tier *overflow* into the EB body ahead of
  slow txs, so a fast tx that does not fit `fastCap`
  reaches the chain via the announced EB rather than being
  discarded from the forged block; the ledger applies a fast-
  vs-slow fee-differential refund to any fast tx landing in
  an EB body (mempool preserves tier tag; ledger computes the
  refund). (b) Documented that this spec commits to option 2
  (unconditional slow-tier revalidation on fast admission);
  the commutativity-based option 1 alternative is described inline
  with a pointer to the in-progress proof at
  `IntersectMBO/formal-ledger-specifications:polina/commutativity`.
- **2026-06-09 (also)** — Added §1 "Peer transaction exchange
  (network side)" documenting two tier-aware requirements on the
  tx-submission mini-protocol layer: (i) inbound txs must carry a
  tier tag so the receiver can dispatch to `addTx Fast` or
  `addTx Slow`, and (ii) outbound pull is fast-first with
  slow-only fallback on empty. Also notes that peer streaming
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
  (fastCap m)` guard on `anyEB`, so `maybeEB = nothing` whenever
  `seqSize (fastTxs ++ slowTxs)` is at or below `fastCap /
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
  (`ebTxs′`), not `combinedSize`, against **`slowCap`** (the EB capacity,
  set to ½ a full RB and — noted — a candidate protocol parameter),
  dropping the `fastCap / 2` form: `lightLoad = underHalfRB (seqSize
  ebTxs′) (slowCap m)`. Since the ½ lives in `slowCap`, there is no
  doubling/rounding; `underHalfRB` reverts to `size[d] < cap[d]`. This
  makes suppression the exact complement of the ledger's `sdChecks EB`
  (both measure the EB body against `slowCap`), so an EB is suppressed
  here iff the ledger would reject it. `combinedSize` removed; §1,
  `slowCapAt`, `underHalfRB`, and Open question 4 updated. This *is* a
  behavioural change to the sketch (suppression predicate now over
  `ebTxs′`).
- **2026-07-13 (last)** — Un-conflated capacity vs floor. `slowCap`
  (`slowCapAt`) is restored to its CIP-164 meaning — the per-EB
  *capacity* (upper bound, `S_EB` etc.) that caps the EB body via
  `splitAtCap`. A NEW postulate `ebFloorAt : TipPoint → Capacity` is the
  EB-fullness *floor* (lower bound, = ½ a full RB), used by the
  suppression guard: `underHalfRB (seqSize ebTxs′) (ebFloorAt (tip m))`.
  The floor is a design choice (not a CIP-164 requirement beyond "no
  empty EBs"), still up for discussion, and a candidate protocol
  parameter; reachability needs `ebFloor ≤ slowCap`. §1, the postulate
  comments, and Open question 4 updated; noted that enforcing the
  CIP-164 per-EB capacity ledger-side is a TODO.
