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

We are building an **intermediate tree `I`** (branch `js/utxo-hd-4-skin`) in which
the ledger state carries a `mk` parameter *again* — but `mk` is a **thin newtype
skin over the new opaque payloads**, not the old machinery.

**The goal is ISOLATION of syntactic vs semantic changes — NOT line reduction.**
Re-dressing the finished design in `prepare-11.1`'s `mk` *vocabulary* splits the
review into two diffs:
- `git diff e6fad0630 <I-tip>` — the **semantic** diff: the genuine redesign
  expressed *in prepare-11.1's `mk` syntax*, with **zero mk-removal churn mixed
  in** (mk is still present, like prepare-11.1).
- `git diff <I-tip> js/utxo-hd-4` — the **syntactic strip**: a *purely mechanical*
  mk-removal pass, trust-by-inspection (or regenerable).

⚠️ **Do not expect `e6 → I` to be smaller than `e6 → full`.** It is roughly the
same size — the redesign is simply large. What the skin buys is the clean
*factoring*: the mk-parameter syntax swap is pulled out into the strip, so the
semantic diff is no longer interleaved with mk-removal noise. See "Result" below
for the measured confirmation.

**`I` is throwaway.** It is review scaffolding; the branch that merges is the
original `js/utxo-hd-4` (HEAD). The skin commits are dropped before merge.

**Scope (decided): the five production libraries only** —
`lib:{ouroboros-consensus, diffusion, protocol, cardano, lsm}`. The testlibs
(`unstable-*`), tools, test-suites and benchmarks are **deliberately left red** in
`I`: they are themselves "recipe" code with little review value, and `I` is
throwaway so it need not build them. Only the five libs need to typecheck for
`prepare-11.1 → I` to be a trustworthy review artifact.

### Early spike estimates (turned out partly misleading — see "Result")
The two spikes suggested ~51% of churn is vocabulary (cancellable) and Storage
~85% cancels. **Caveat:** those figures conflated *line cancellation* (which the
consensus lib barely shows — it IS the structural redesign) with *isolation*
(which works). The 51% was the testlibs/ports (out of scope); 85% was Storage
specifically. Read them as "isolation is feasible," not "the diff shrinks."

### Cost / risk (eyes open)
Building `I` is **"the original `mk`-removal run in reverse"**: re-adding `mk` to
the `LedgerState`/`Ticked`/`ExtLedgerState` data families breaks **every
`data instance` lib-wide in one shot — no green checkpoint until nearly done**.
Type-level feasibility **confirmed in practice** (the whole consensus lib + 2 more
libs are green with **zero `unsafeCoerce`/`unsafePerformIO`**); the spike's
unverified risks (real `sop-core` `NS`/`Telescope`, `AllowAmbiguousTypes`) did not
bite.

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

## RESULT (the isolation goal — achieved)

The success criterion is **isolation of syntactic vs semantic**, not line reduction.
Verified on `lib:ouroboros-consensus`: the strip `js/utxo-hd-4-skin → js/utxo-hd-4`
(881 churn) is **purely syntactic** — word-diff shows every changed token is an
`mk`-vocabulary removal or its direct fragment (surviving `blk`, unwrapped
`unFlip`/`getFlipTickedLedgerState` inner expr, `StateKind`→`(Type -> Type)`
kind-sig reverts, removed skin pragma/comment/constraint). No reordered args, no
changed calls, no behavioural edits. So `e6fad0630 → skin` carries the genuine
redesign **in prepare-11.1's `mk` syntax with zero mk-removal churn mixed in**,
and the strip is a trust-by-inspection mechanical pass. (Line count of `e6 → skin`
≈ `e6 → full` — irrelevant; the redesign is simply large. What matters is it's
cleanly factored from the syntax swap.)

**Measured per lib (line cancellation vs isolation):**
- `lib:ouroboros-consensus`: full `e6→utxo-hd-4` = 5,419 churn; semantic `e6→skin`
  = 5,190 (~96% — barely smaller, because this lib *is* the structural redesign:
  ~1,369 lines are the `Tables/` machinery deletion alone). Strip = 881, and it's
  **purely syntactic** (the isolation win).
- `diffusion`: full diff is only 28 churn — never a review problem; skin moot.
- `lsm`: 1 module, 4 `EmptyMK` additions — trivial.
- `cardano`: expected to cancel *best* (Shelley regains `shelleyLedgerTables`
  -in-state, matching prepare-11.1) — in progress.

Takeaway: **isolation is the deliverable and it works; line-shrink does not happen
for the libs and was never the right metric.**

## Status & decisions (current)

**Branch `js/utxo-hd-4-skin`** (off untouched merge target `js/utxo-hd-4` @ `341ab6f55`).
Only `mk-skin-plan.md` is tracked as a non-code file. Safety backup of the
pre-history-scrub tip: branch `skin-backup-prefilter` (delete once happy).

### Done
- [x] **`lib:ouroboros-consensus`** — fully skinned, `All good (239 modules)`,
      **zero `unsafeCoerce`/`unsafePerformIO`**.
- [x] **`diffusion`** — green-skinned, integrated.
- [x] **`lsm`** — green-skinned, integrated. (Also fixed a latent consensus-lib bug:
      the streaming `Yield`/`Sink` aliases must be `l blk EmptyMK`, not `l blk` —
      only the LSM backend threads a handle through them, so it surfaced there.)
- [x] **History scrubbed.** An early *non-isolated* agent's `git add -A` folded the
      repo-root working files (design notes, decks, logs, spike patches) into the
      branch. Removed from ALL commits via `git filter-branch` (kept `mk-skin-plan.md`;
      `.gitattributes` reset to base throughout, the user's `diff=cbor` change kept as
      a pending working-tree modification). Code byte-identical pre/post scrub.
      **Lesson: run sweep agents worktree-isolated, or scope every `git add` — never `-A`.**

- [x] **`protocol`** (9 modules) — needed ZERO source changes (it never references
      `LedgerState`/tables); green once the skinned lib built.
- [x] **`cardano`** (57 modules) — green-skinned, integrated (cherry-picked the 4
      cardano commits onto the scrubbed tip). Shelley `shelleyLedgerTables`-in-state
      reintroduced; `CanHardFork` era-translation `Flip`/`Comp` wrapping restored at
      `EmptyMK`. Artifacts: full `e6→utxo-hd-4` 3,648 churn; semantic `e6→skin` 3,554
      (~97%); strip 270 churn (mostly syntactic). **No `HasLedgerTables` instances
      needed** for Shelley/Byron/Cardano/HFC — nothing in the 5 libs requires them.

### ✅ ALL FIVE LIBS GREEN — the skin is complete
`cabal build` of `lib:ouroboros-consensus`, `diffusion`, `lsm`, `protocol`,
`cardano` all succeed on `js/utxo-hd-4-skin`. Zero `unsafeCoerce`/`unsafePerformIO`.

### Remaining
- [ ] **Review artifacts (per lib):** `git diff e6fad0630 <skin-tip>` (semantic) +
      `git diff <skin-tip> js/utxo-hd-4` (syntactic strip). Measured: consensus lib
      semantic ~5.2k churn / strip 881 (purely syntactic); cardano semantic ~3.6k /
      strip 270; diffusion 28 total; lsm trivial.
- [ ] **Cleanup:** delete `skin-backup-prefilter` + `refs/original/*` (filter-branch
      backup) once the history scrub is accepted.

### Decisions (load-bearing)
1. **Skin shape:** single-arg `mk`, `l = blk`; thin newtype over the existing opaque
   `Keys/Values/Diff`. prepare-11.1's `CanMapMK`/combinator zoo is NOT resurrected
   (stays deleted — intended, shows in `e6→skin`).
2. **HFC = option 2 (phantom `mk`).** HFC `LedgerState (HardForkBlock xs) mk` has `mk`
   phantom; telescope functor is `Flip LedgerState EmptyMK` (`FlipTickedLedgerState
   EmptyMK` for ticked). `Flip` had to return regardless (a 2-arg `LedgerState x`
   isn't a `Type`), but pinned at `EmptyMK` — no `mk`-threading through HFC bodies,
   clean strip. The genuine canonical→`NS WrapValues` tables change correctly shows in
   `e6→skin`.
3. **EqMK infeasible single-arg:** prepare-11.1's `forall mk. EqMK mk => Eq (l blk mk)`
   needs two-arg `mk` (plain type vars); single-arg's payload is a type family
   (`Keys blk`), and GHC forbids type families in quantified constraints (`GHC-22979`).
   So `IsLedger` uses **concrete-`mk` constraints** (`Eq (l blk EmptyMK)`, …). That one
   superclass block doesn't cancel — small localised residue.
4. **Apply-path altitude:** re-dress only the **state `mk`-vocabulary**; KEEP the
   branch's genuine apply restructuring visible (explicit `Values blk` param,
   `(state, Diff)` tuples, `forward`). So `applyChainTick` stays
   `… l blk EmptyMK -> (Ticked l blk EmptyMK, Diff blk)` (a tuple — NOT prepare-11.1's
   bundled `DiffMK`); only state positions get `EmptyMK`, bodies unchanged. That
   restructuring is redesign and SHOULD appear in `e6→skin`. (This supersedes the
   earlier "Phase 2" note above that suggested bundling into `DiffMK`.)
5. **Cardano:** Shelley `LedgerState (ShelleyBlock …) mk` regains `shelleyLedgerTables`
   -in-state (matching prepare-11.1); Byron void/phantom. **One documented divergence
   to review:** `transPraosLS` (CanHardFork) — prepare-11.1 keeps it poly-`mk` with
   `shelleyLedgerTables = coerce tb`, but the single-arg skin's `coerce` fails (nominal
   role crossing `TPraos`→`Praos`); it's only ever called at `EmptyMK` (telescope
   state), so it was specialized to `EmptyMK` + `emptyLedgerTables`. Honest and
   correct, but a small intentional deviation from the e6 text (shows in `e6→skin`).
6. **Scope:** the 5 production libs only; testlibs/tools/test-suites left red.

### Mechanical rhythm (for the cardano sweep + any continuation)
Per module: save → **stable-read** the ghciwatch error file (wait until NOT "still
compiling" AND unchanged between two polls — a plain check races the reload) → for
each flagged `LedgerState`/`Ticked`/`ExtLedgerState` in **value/field/functor**
position add the right `mk`: **`EmptyMK`** (tableless states), **poly `mk`**
(read-only accessors), **`Flip <F> EmptyMK`** (`NS`/`Current`/`HardForkState`/
`Product` functor args, with `unFlip`/`getFlipTickedLedgerState`/`Flip` in bodies).
Add `EmptyMK` to explicit import lists where missing. **Leave alone**
functor-unapplied/constraint/instance-head sites (`LedgerErr l blk`, `IsLedger l blk`,
`LedgerCfg l blk`, `GetTip (l blk)`). Template per file: `git show e6fad0630:<file>`
(it does this at `mk`; substitute `mk → EmptyMK`). Commit per module-group.

> Note: the "Order of attack" / "Phase N" sections above are the *original plan*,
> kept for context; this section is the authoritative current state.
