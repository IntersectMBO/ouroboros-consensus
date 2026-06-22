# UTxO-HD `js/utxo-hd-4` — the `mk`-skin review intermediate (plan)

**Portable plan. Self-contained.** A fresh Claude on another machine should be
able to pick this up with only: this repo at branch `js/utxo-hd-4` (the finished,
green, `mk`-free tree), and this file. Read [`utxo-hd-4.md`](./utxo-hd-4.md)
(design record, esp. decisions 1–14), [`notes2.md`](./notes2.md) (design source
of truth), and [`next-steps.md`](./next-steps.md) (what the branch did) for the
redesign itself. **This file is only about the review scaffolding**, not the
redesign.

---

## Why this exists

`js/utxo-hd-4` removes the `mk :: MapKind` parameter from `LedgerState` and
carries on-disk tables as opaque `Keys`/`Values`/`Diff blk` payloads. The diff
vs the review base (`e6fad0630`, prepare-11.1) is ~196 source files / ~12k
changed lines and is hard to review as one unit.

We are building an **intermediate tree `I`** in which the ledger state carries a
`mk` parameter *again* — but `mk` is a **thin newtype skin over the new opaque
payloads**, not the old machinery. The point: re-dressing the finished design in
`prepare-11.1`'s `mk` *vocabulary* makes `prepare-11.1 → I` cancel all the
vocabulary churn, leaving roughly the genuine structural redesign — which is the
right thing for a human to read. Then `I → HEAD` is the mechanical skin-strip.

**Review artifacts produced at the end:**
- `git diff e6fad0630 <I-tip>` — the **semantic** diff (small-ish; the conceptual core).
- `git diff <I-tip> js/utxo-hd-4` — the **mechanical** strip (trust-by-inspection).

**`I` is throwaway.** It is review scaffolding; the branch that merges is the
original `js/utxo-hd-4` (HEAD). The skin commits are dropped before merge.

**Scope (decided): the five production libraries only** —
`lib:{ouroboros-consensus, diffusion, protocol, cardano, lsm}`. The testlibs
(`unstable-*`), tools, test-suites and benchmarks are **deliberately left red** in
`I`: they are themselves "recipe" code with little review value, and `I` is
throwaway so it need not build them. Only the five libs need to typecheck for
`prepare-11.1 → I` to be a trustworthy review artifact.

### Measured expectation (from two spikes, see end of file)
- ~51% of the branch churn is in `Port …` commits (vocabulary-dominated → mostly cancels).
- On **Storage**, ~80–85% of the churn cancels under the skin; ~15–20% is the
  genuine structural residue (range-read rework, `duplicateWithDiffs` arity, the
  `(state, diff)` apply flow). Expect a similar shape elsewhere.

### Cost / risk (eyes open)
Building `I` is **"the original `mk`-removal run in reverse"**: re-adding `mk` to
the `LedgerState`/`Ticked`/`ExtLedgerState` data families forces editing **every
`data instance` lib-wide in one shot — no green checkpoint until nearly done**.
Estimate ~2–4 days for `lib:ouroboros-consensus`, plus cardano + testlibs. The
type-level feasibility is **confirmed** (no `unsafeCoerce`, no kind-hell — see
spikes), but it was validated on a type model, **not a full build**; the residual
risk is the real `sop-core` `NS`/`Telescope` + `AllowAmbiguousTypes` interaction.

---

## Ground rules (do not forget these offline)

- **Commits UNSIGNED:** `git -c commit.gpgsign=false commit -m "…"` (GPG/pinentry
  hangs here).
- **Do NOT push** to GitHub. Syncing to your own private remote is your call.
- **Commit messages must not mention AI/Claude.** Prefix `[UTxO-HD][skin]`.
- **NEVER use `unsafeCoerce` or `unsafePerformIO`.** If the skin appears to need
  them, STOP — that means the design is wrong; reconsider, don't reach for them.
- TODOs in source are tagged `TODO @js`.
- Work on branch **`js/utxo-hd-4-skin`** (off `js/utxo-hd-4`). Leave
  `js/utxo-hd-4` untouched — it is the merge target.

---

## The skin (the single design decision that drives everything)

Add to `Ouroboros/Consensus/Ledger/Basics.hs`. **`l = blk` (a `Type`)** — this is
the only well-kinded reading and it is the clean one (a thin newtype layer over
the *existing* opaque associated types; no canonical machinery, no coercions):

```haskell
type MapKind        = Type -> Type
type LedgerStateKind = MapKind -> Type

newtype LedgerTables l mk = LedgerTables (mk l)

data    EmptyMK  l = EmptyMK
newtype KeysMK   l = KeysMK   (Keys   l)   -- Keys/Values/Diff are the CURRENT
newtype ValuesMK l = ValuesMK (Values l)   -- associated types on
newtype DiffMK   l = DiffMK   (Diff   l)   -- BlockSupportsUTxOHD
-- so  LedgerTables blk ValuesMK ≅ Values blk,  etc.
```

**Resolved during Phase 1 (grounded in `git show e6fad0630`):** `prepare-11.1` is *already*
`blk`-indexed for tables — `newtype LedgerTables blk mk` (`Type -> MapKind ->
Type`), `type family TxIn blk`, and the handle is `LedgerTablesHandle m l blk`
with `read :: l blk EmptyMK -> LedgerTables blk KeysMK -> m (LedgerTables blk
ValuesMK)` (functor `l` applied to `blk`+`mk`, tables `blk`-indexed). So the clean
`l = blk` skin reproduces `prepare-11.1`'s signatures **verbatim** and is also the
feasible one — the two spikes were *not* in conflict (Spike A's `blk` model =
`prepare-11.1`'s shape). The only divergence is single-arg `mk` (`KeysMK blk = KeysMK
(Keys blk)`) vs `prepare-11.1`'s two-arg (`KeysMK k v = Set k`), invisible in applied
positions. `HasLedgerTables` is a per-`blk` class over `LedgerState blk mk` (the
spike's shape); `ExtLedgerState` gets its own handling in `Extended.hs`.

**Crucial nuances:**

1. **Single-arg `mk`, unlike `prepare-11.1`.** `prepare-11.1`'s `MapKind` is two-arg (`EmptyMK k
   v`, `ValuesMK k v = ValuesMK (Map k v)`). Ours is one-arg (`KeysMK l`). In
   *applied* form the signatures read identically — `LedgerState blk EmptyMK`,
   `LedgerTables blk DiffMK` — so they **cancel textually** against `prepare-11.1`. We do
   NOT resurrect `prepare-11.1`'s `CanMapMK`/`CanMapKeysMK`/`ZeroableMK`/`mapKeysMK`
   combinator zoo; that machinery stays deleted in `I` and shows up in
   `prepare-11.1 → I` as a deletion. **That is intended, bounded residue** — the
   redesign genuinely deleted it.

2. **Revert decision 1.** The HFC telescope functor goes back to `Flip
   LedgerState mk` (`prepare-11.1`'s known-good shape). The telescope itself stays
   **table-free** (`NS (Flip LedgerState EmptyMK) xs`); the running `mk` lives in
   the **HFC-level tables field beside the telescope** (`NS WrapValues xs` etc.),
   exactly as `prepare-11.1` arranges it. Spike A proved both body-level failures vanish
   under this arrangement. So `Flip`/`unFlip`/`FlipTickedLedgerState` come back.

3. **The Shelley `shelleyLedgerTables` field comes back** on `LedgerState
   (ShelleyBlock …) mk`, holding `LedgerTables (ShelleyBlock …) mk`.

4. Also restore the `HasLedgerTables` vocabulary the call sites use —
   `projectLedgerTables` / `withLedgerTables` / `forgetLedgerTables` /
   `emptyLedgerTables` — over the skin, so the call sites cancel against `prepare-11.1`.
   Defer this to Phase 1 (its methods mention `l mk`).

**Build target for each file:** drive `git diff e6fad0630:<file> <file>` toward
zero *except* for the genuine structural changes. Keep `git show e6fad0630:<file>`
open and match `prepare-11.1`'s signature text where the redesign didn't truly change the
shape.

---

## Order of attack (the all-at-once edit is unavoidable)

> There is **no green checkpoint** between Phase 0 and the end of Phase 1. Adding
> `mk` to the `LedgerState` data family breaks every `data instance` lib-wide
> simultaneously. Lean on HLS per-file + the breakage grep, not on a green
> ghciwatch, until Phase 1 lands. Same red-stretch profile as the original
> `mk`-removal, run in reverse.

**Phase 0 — skin types (GREEN, self-contained).** Add the 6 skin definitions
above to `Basics.hs` + exports. They are unused so far ⇒ lib stays green. Commit.
*(Status: see bottom.)*

**Phase 1 — re-add `mk` to the state functors (THE red stretch).**
- `Basics.hs`: `data family LedgerState blk` → `data family LedgerState blk (mk
  :: MapKind)`; `TickedLedgerState`; `type LedgerState :: Type -> MapKind -> Type`.
- `Ledger/Extended.hs`: `ExtLedgerState blk` → `ExtLedgerState blk mk`.
- Add the `HasLedgerTables`/`CanStowLedgerTables`/`CanUpgradeLedgerTables`-style
  classes back over the skin (only what the call sites actually use).
- Re-add `mk` to **every `data instance LedgerState`/`Ticked` lib-wide in one
  shot** — Byron/mock trivial, Shelley with `shelleyLedgerTables`, HFC with the
  `NS` tables field + `Flip` functor. Grep: `data instance LedgerState`,
  `data instance Ticked`.
- Restore `Flip`/`FlipTickedLedgerState`/`unFlip` in `TypeFamilyWrappers` + HFC
  `State`/`Basics`/`Ledger`.
- `GetTip`, `Eq`/`Show`/`NoThunks` quantified-over-`mk` instances.

**Phase 2 — abstract apply path (`Ledger/Abstract`, `IsLedger`, `ApplyBlock`).**
Re-dress to `prepare-11.1`'s signatures over the skin:
- `applyChainTick :: … -> l EmptyMK -> Ticked l DiffMK` (re-bundle the current
  `(ticked, diff)` return into the ticked state's `DiffMK` field).
- `applyBlock`/`tickThenApply`: `… -> Ticked l ValuesMK -> … l DiffMK` (or
  `TrackingMK` to match `prepare-11.1` — check `prepare-11.1`'s exact result mk).
- `blockKeys` ↔ `prepare-11.1`'s `getBlockKeySets :: … -> LedgerTables l KeysMK` shape.

**Phase 3 — Storage (`Storage/LedgerDB/**`).** Per Spike B: re-wrap the handle
record (`read :: … -> LedgerTables l KeysMK -> m (LedgerTables l ValuesMK)`),
`Forker`, `LedgerSeq`, the LedgerDB API, snapshots, V2 InMemory backend. **Leave**
the structural residue (it does not cancel and is the review target): the
`EraRangeReader`/`RangeReadTables` range-read rework, `duplicateWithDiffs` going
from `Diff` to `prepare-11.1`'s two-state shape (can't be faked — stays different), the
explicit `blockKeys` extraction.

**Phase 4 — HFC combinator, Mempool, Node, MiniProtocol.** Re-dress signatures.
HFC is the hard part; the `Flip` functor + sibling tables field (decision above)
is the load-bearing arrangement.

**Phase 5 — cardano lib (byron/shelley/cardano).** Re-dress. Lots of files,
mostly mechanical once the lib shape is settled. **Testlibs / tools /
test-suites are out of scope** (left red — see "Scope" above).

**Phase 6 — verify `I` typechecks** (lib, then `diffusion`, `lsm`, `cardano`).
Full green not strictly required for review, but the closer to green, the more
trustworthy `prepare-11.1 → I` is. Tests need not pass (it is throwaway).

**Phase 7 — produce + sanity-check the review artifacts.**
```
git diff --stat e6fad0630 js/utxo-hd-4-skin   # semantic — want this much smaller than the full diff
git diff --stat js/utxo-hd-4-skin js/utxo-hd-4 # mechanical strip — want this "obviously vocabulary"
```
Confirm the semantic diff is dominated by the genuine redesign (HFC canonical→NS,
the new `forward`/`BlockSupportsUTxOHD` surface, Storage range-reads, mempool,
serialisation) and not by `mk`-vocabulary noise.

---

## Verify loop

ghciwatch (authoritative, whole-lib reload). **Watch only the lib source dir, not
`.`** Run in background, no trailing `&`:
```
ghciwatch --command "cabal repl lib:ouroboros-consensus" \
          --watch ouroboros-consensus/src/ouroboros-consensus \
          --error-file /tmp/ghciwatch-skin.errors \
          --reload-glob '!dist-newstyle' --reload-glob '!../dist-newstyle' \
          > /tmp/ghciwatch-skin.log 2>&1
```
`cat /tmp/ghciwatch-skin.errors` after each save (`All good (N modules)` = clean).
HLS in-editor diagnostics are faster per-file during the red stretch.

Breakage-surface grep (the reverse of the removal grep — find sites still missing
the skin):
```
grep -rn 'data instance LedgerState\|data instance Ticked\|LedgerState blk ->\|ExtLedgerState blk ->' \
  ouroboros-consensus/src/ouroboros-consensus
```

---

## Spike findings (embedded so this file is self-contained)

Two spikes (type-model + reasoning, **not** full builds) settled feasibility.
WIP patches: `mk-skin-spike-hfc.patch`, `mk-skin-spike-storage.patch` (copied into
the repo root alongside this file).

**Spike A — HFC / "indexing by `l`": GO.**
- Well-kinded only under `l = blk`; then the skin is a thin newtype over the
  existing opaque types. No `unsafeCoerce`, no `AllowAmbiguousTypes` needed for
  the skin itself (the branch already lives with `AllowAmbiguousTypes` for the
  non-injective families).
- Body failures (`withLedgerTables` on the telescope; rebuilding a telescope
  element at the wrong mk) **both vanish** when the telescope stays table-free and
  the running `mk` lives in the sibling HFC tables field — i.e. revert decision 1
  to `prepare-11.1`'s `Flip LedgerState mk` functor.
- Unverified: real `sop-core` `NS`/`Telescope` with `All`/`SListI`; re-quantified
  `GetTip`/`NoThunks`/serialisation instances.

**Spike B — Storage / "rearranging tuples": ~80–85% cancels.**
- Of ~650 changed Storage lines, ~85% become byte-identical to `prepare-11.1` once
  re-dressed (the 148 `mk`-on-state lines dominate; the re-dressed handle `read`
  field was confirmed byte-identical to `e6fad0630`).
- Irreducible residue (the genuine Storage redesign, ~15–20%): `readRange`/
  `readAll` → `RangeReadTables`/`EraRangeReader`/`EraRangeReaderProvider`/
  `withEraRangeReader` (decision 10); `duplicateWithDiffs` arity (2 states → 1
  `Diff` — the skin cannot fabricate states it doesn't have); explicit `blockKeys`
  + `(state, diff)` apply flow; the `LedgerSeq` haddock rewrite.
- The cancellation is gated on the global `mk`-on-state kind change (Phase 1) —
  i.e. it is mechanical per-site but only after the all-at-once edit.

---

## Status

- [x] **Phase 0** — skin types in `Basics.hs` (+ exports), lib green (239 modules). *Done: the 6 skin defs + export group; unused so far, so lib stayed green.*
### ⚠ Finding (Phase 1): `prepare-11.1`'s quantified MK constraints are infeasible single-arg

`prepare-11.1`'s `IsLedger` requires `forall mk. EqMK mk => Eq (l blk mk)` (and Show/
NoThunks). That works only because `prepare-11.1`'s `mk` is *two-arg* (`mk k v`), so the
payload is the plain type *variables* `k`/`v`. The skin's `mk` is *single-arg*
(forced — the HFC has no `TxIn`/`TxOut`), so each payload is a type-family
application (`Keys blk`), and **GHC forbids type families in quantified
constraints** (`GHC-22979`). So `EqMK`/`ShowMK`/`NoThunksMK` are *not* restored;
`IsLedger` uses **concrete-`mk` constraints** (`Eq (l blk EmptyMK)`, …) instead.
Consequence: that one superclass block does not cancel against `prepare-11.1` (a small,
localised residue). Add more concrete map-kinds there if downstream needs them.

- [~] **Phase 1** — `mk` on state functors lib-wide (red stretch) ← current. **Foundation in `Basics.hs` done:** `MapKind`/`LedgerStateKind`/`StateKind` kind vocab; `LedgerTables`/`EmptyMK`/`KeysMK`/`ValuesMK`/`DiffMK` skin newtypes; `HasLedgerTables` class + `forgetLedgerTables`/`emptyLedgerTables`; `data family LedgerState blk mk` (kind `Type -> LedgerStateKind`); `LedgerCfg :: StateKind -> Type -> Type`; `decodeValues :: LedgerState blk EmptyMK -> …`.
  - **`Basics.hs` now internally consistent** (`IsLedger` re-kinded to `StateKind` + concrete-mk constraints; `GetTip :: LedgerStateKind -> Constraint`, `getTip :: l mk -> Point l`; `applyChainTickLedgerResult`/`applyChainTick` re-dressed to `l blk EmptyMK -> (LedgerResult blk) (Ticked l blk DiffMK)` matching `prepare-11.1`; `QuantifiedConstraints` enabled). Errors have moved downstream.
  - **NEXT — `Ledger/Abstract.hs` (the `ApplyBlock` apply-path re-dress).** The branch carries values as an explicit `Values blk` param and returns `(l blk, Diff blk)`; `prepare-11.1` carries them in the ticked state and returns the diff in the ticked state's tables. Re-dress to `prepare-11.1` (verified shapes):
    - `applyBlockLedgerResultWithValidation :: … -> blk -> Ticked l blk ValuesMK -> Except (LedgerErr l blk) (LedgerResult blk (l blk DiffMK))` (drop the `Values blk ->` arg; `Ticked l blk` → `Ticked l blk ValuesMK`; `(l blk, Diff blk)` → `l blk DiffMK`). Same for `applyBlockLedgerResult`, `reapplyBlockLedgerResult`, `defaultApplyBlockLedgerResult`, `defaultReapplyBlockLedgerResult`.
    - Restore `class GetBlockKeySets blk where getBlockKeySets :: blk -> LedgerTables blk KeysMK` (the branch replaced it with `blockKeys :: blk -> Keys blk` on `BlockSupportsUTxOHD`; for cancellation, restore `getBlockKeySets` as a `LedgerTables … KeysMK` wrapper over `blockKeys`).
    - `tickThenApply`/`tickThenReapply`/`applyDiffForKeys` exist in `prepare-11.1`'s `Abstract.hs` and thread values through the ticked state — re-dress to `prepare-11.1`'s bodies.
    - **Per-instance follow-on (the bulk):** every `ApplyBlock` instance (Shelley, HFC, Byron, mock, dual) unwraps `ValuesMK` at entry (`projectLedgerTables`) and wraps the diff into `DiffMK` at exit (`withLedgerTables`) — the core logic is unchanged; only the boundary packaging moves ("rearranging tuples").
  - **Apply-path altitude DECIDED (important):** the skin re-dresses only the **state `mk`-vocabulary** (`l blk` → `l blk EmptyMK`, etc.); it **keeps the branch's genuine apply restructuring visible** (explicit `Values blk` param, `(state, Diff)` tuples, `forward`) — that is redesign, not packaging, and *should* show in the review. So `applyChainTick` stays `… l blk EmptyMK -> (Ticked l blk EmptyMK, Diff blk)` (tuple, not prepare-11.1's bundled `DiffMK`), and `ApplyBlock` keeps `Values blk -> Ticked l blk EmptyMK -> … (l blk EmptyMK, Diff blk)`. **Bodies are unchanged — only state type positions get `EmptyMK`.**

  - **DONE so far (committed, all WIP-red) — the entire abstract ledger layer:** `Basics`, `Abstract` (apply-path), the core `Ledger/*` (`CommonProtocolParams`/`SupportsPeerSelection`/`SupportsPeras` = poly `mk`; `Inspect` = `mk1`/`mk2`; `SupportsMempool` = `EmptyMK`; `SupportsProtocol` = `EmptyMK`), `Block/Forging`, `Mempool/Capacity`, `Forecast` (`b mk`), `HeaderValidation`, `BlockchainTime/WallClock/HardFork`, `MiniProtocol/ChainSync/Client/InFutureCheck`, `HardFork/Combinator/State/Types` (`TranslateLedgerState` field + `CrossEraForecaster`'s `state x EmptyMK`), `HardFork/Combinator/Translation`, **`Ledger/Extended`** (`ExtLedgerState blk mk` data type + `Ticked` instance + deriving with `Eq/Show/NoThunks (LedgerState blk mk)` constraints + apply helper + disk codecs with `forall blk mk.`; `typeRep` uses `Proxy @(ExtLedgerState blk)` to dodge `Typeable mk`).
  - **Two reusable techniques learned:** (1) **stable-read loop** to beat ghciwatch staleness — wait until the error file is *both* not-"still compiling" *and* unchanged between two polls (a plain "not compiling" check races the reload). (2) grep over-matches functor-unapplied sites (`LedgerErr LedgerState blk`, `IsLedger LedgerState blk`, instance heads) — those are **correct, leave them**; only `LedgerState blk`/`Ticked … blk`/`ExtLedgerState blk` in *value/field* position need `mk`.
  - **CURRENT FRONTIER:** the **Storage subsystem** (`Storage/LedgerDB/Forker` ×5, `Snapshots` ×4 — these parametrise over `l :: StateKind` and `ExtLedgerState`, so thread `l … mk`/`ExtLedgerState blk mk`; Storage spike says ~85% cancels), plus `HeaderStateHistory`, `Node/ProtocolInfo`, and a last straggler in `Extended`.
  - **The mechanical rhythm (repeat to the end):** save → `cat /tmp/ghciwatch-skin.errors` → for each `LedgerState blk`/`TickedLedgerState blk` flagged (`Expecting one more argument`), add the right `mk`: **poly `mk`** for read-only accessors, **`EmptyMK`** for tableless ticked/committed states, **`mk1`/`mk2`** for before/after pairs, **`ValuesMK`/`DiffMK`** only where the redesign genuinely carries tables in the state (rare — usually it carries them separately). `sed -i 's/TickedLedgerState blk\b\( *->\)/TickedLedgerState blk EmptyMK\1/g'` per file is the workhorse. Commit per few modules. GHC masks downstream behind the first failing module, so errors march outward one wavefront at a time.
  - **CURRENT FRONTIER:** `Forecast.hs`, `HardFork/Abstract.hs`, `Mempool/API.hs` (incl. the `ForgeInUnknownSlot`/`ForgeInKnownSlot` data-constructor fields — those carry `LedgerState blk`/`TickedLedgerState blk`, add `EmptyMK`).
  - **THEN, in dependency order:** the rest of the lib sweep → `Extended.hs` (`ExtLedgerState blk mk` + tables wrapper), `TypeFamilyWrappers.hs` (restore `Flip`/`FlipTickedLedgerState`), the 4 lib `data instance`s (HFC `Basics`/`Ledger`, `Dual`, `LedgerSeq` — the **non-mechanical** ones: Shelley-style tables field for concrete blocks, `NS`+`Flip` telescope for HFC), `Storage/*`, `Node/*`, `MiniProtocol/*`; then the `diffusion`/`lsm`/`cardano` libs. Use `git show e6fad0630:<file>` per file as the cancellation target.
  - **Reference:** `mk-skin-spike-hfc.patch` (validated interface shapes); `git show e6fad0630:<file>` per file = the cancellation target.
- [ ] Phase 2 — abstract apply path
- [ ] Phase 3 — Storage
- [ ] Phase 4 — HFC / Mempool / Node / MiniProtocol
- [ ] Phase 5 — cardano + testlibs
- [ ] Phase 6 — verify `I` typechecks
- [ ] Phase 7 — produce review artifacts

Update this checklist + a one-line note per phase as you go, so the plan stays the
migration record across machines.
