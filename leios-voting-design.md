# Leios voting: how to gate it on the active era

## Scope of this analysis

Leios voting (and the surrounding subsystems — vote state, EB store, fetching,
mini-protocol clients/servers) only does useful work when the active ledger era
is Leios-enabled (today: Conway). This document compares three approaches for
expressing that gate.

This is **not exhaustive** and `getLeiosCommittee` is only a first entry point
for discovering potential mechanisms (`HasLeiosVoting` class, `LeiosVoting`
handle, `initNodeKernelHook`). Each approach is drafted on a branch that makes
an always-on, abstract voting thread to be only running when Leios is enabled.
The branch diffs are the source of truth; this document points at them and
summarises the trade-offs.

**In scope:**

- Where the "is this era Leios-enabled?" decision is encoded.
- How the kernel-side voting thread learns the answer at runtime.
- How the same answer flows to peer subsystems (mini-protocol handlers, shared
  state) wired around it.

**Out of scope:**

- Actual committee selection from the ledger state
- Evolution of data types after initial rollout
- Deprecation of Leios subsystems once Leios eras are historical

**Operating assumption** We are willing to ship a node where Leios-related
clients, servers, storage and threads are *always wired* and become
operationally active only once the active era is Leios-enabled. This mirrors how
`BlockForging` is handled today — structurally per-era, but only the active
era's instance ever does useful work.

**Adjacent Leios extension points already in the codebase.** Two
patterns are already active for per-era Leios code; the consistency story
is genuinely split.

- *Forge loop* — extended via `BlockForging`'s `forgeBlock` field, i.e. the
  handle pattern. Some Leios forge-loop logic that lives in the abstract
  part of `NodeKernel` today (mempool snapshotting, leiosdb writing) is
  expected to move into per-era `forgeBlock` over time.
- *Block validation* — extended via the `ResolveLeiosBlock` class, i.e. the
  typeclass pattern. ChainDB calls it generically and the per-era / per-
  block-type instances (mock, Byron, ShelleyBlock per-era) supply the
  Leios-specific work; the `CardanoBlock` instance pattern-matches
  `BlockConway` to gate.

So voting's choice of mechanism does *not* keep Leios at one pathway — the
codebase already has two. The question is which one of the existing
mechanisms is the better fit for voting (or whether voting earns a third).

## Approaches

All three branches sit on top of the same parent commit `e43bc4d1e Improve
threadnet test output`.

### A. Type class ([`ch1bo/leios-voting-class`](https://github.com/input-output-hk/ouroboros-consensus/compare/e43bc4d1e...ch1bo/leios-voting-class))

```haskell
class HasLeiosVoting blk where
  getLeiosCommittee :: LedgerState blk EmptyMK -> Maybe Committee
  getLeiosCommittee _ = Nothing  -- default: abstain

instance HasLeiosVoting (ShelleyBlock (Praos c) ConwayEra) where
  getLeiosCommittee _ = Just (error "FIXME: from ledger state")

instance HasLeiosVoting (ShelleyBlock (TPraos c) AllegraEra)  -- empty: Nothing
-- … one per non-Conway era, plus mock/Byron/BlockA/B …
```

Per-era instances declare each era's stance. A single HFC dispatch
instance combines them. The class is a `RunNode` superclass.

**Pros:**

- Forgetting a Conway-era instance is a compile error.
- HFC dispatch comes for free via `All HasLeiosVoting xs`.
- Constraint-based wiring is consistent with existing era-aware classes
  (`LedgerSupportsProtocol`, `BlockSupportsDiffusionPipelining`,
  `SupportedNetworkProtocolVersion`).

**Cons:**

- Every block type in the codebase must own an instance: mock blocks, Byron,
  DualByron, HFC test `BlockA` / `BlockB`, six per-era Shelley instances. Most
  are empty / inert definitions.
- The class becomes a `RunNode` superclass, so the `HasLeiosVoting blk`
  constraint propagates through the kernel, the test framework, and the
  diffusion layer.
- The HFC dispatch instance is an orphan, hosted in a new
  `Ouroboros.Consensus.HardFork.Combinator.Leios` module, pulling Leios into HFC
  core.

### B. Handle ([`ch1bo/leios-voting-handle`](https://github.com/input-output-hk/ouroboros-consensus/compare/e43bc4d1e...ch1bo/leios-voting-handle))

```haskell
newtype LeiosVoting blk = LeiosVoting
  { getLeiosCommittee :: LedgerState blk EmptyMK -> Maybe Committee
  }

conwayLeiosVoting :: LeiosVoting (ShelleyBlock (Praos c) ConwayEra)
conwayLeiosVoting =
  LeiosVoting
    { getLeiosCommittee = \_ ->
        Just (error "FIXME: from ledger state")
    }

cardanoLeiosVoting :: LeiosVoting (CardanoBlock c)
cardanoLeiosVoting =
  hardForkLeiosVoting (conwayLeiosVoting `OptNP.at` IS (IS (IS (IS (IS (IS IZ))))))
```

The handle is constructed per-era (only `conwayLeiosVoting` is non-trivial),
assembled into the HFC-level handle by `hardForkLeiosVoting`, and threaded
through `RunNodeArgs` / `mkNodeKernelArgs` / `NodeKernelArgs` /
`TestNodeInitialization` to the voting thread. Mirrors the `BlockForging`
pattern.

**Pros:**

- No instance proliferation. Mock blocks, Byron, BlockA/B, etc. need zero lines.
- The "only Conway votes" decision is one explicit `OptNP.at` line in
  `protocolInfoCardano`, grep-able in one spot.
- Handles are mockable in tests by passing an arbitrary value.
- Mirrors `BlockForging`'s lifecycle — known pattern in the codebase.

**Cons:**

- Most plumbing of the three options. New field on `NodeKernelArgs`,
  `RunNodeArgs`, `TestNodeInitialization`; threaded through `loop` and
  `forkNode` in the threadnet harness; `mkNodeKernelArgs` gains an argument.
- HFC dispatch helper (`hardForkLeiosVoting`) is hand-written (~25 lines) where
  the class version got it for free.
- Forgetting Conway in the OptNP construction compiles — silent abstain at
  runtime.
- Under the inert-but-present assumption, the bundle has only one field today;
  the per-era handle reduces to a trivial wrapper around a single function.

### C. Concrete hook ([`ch1bo/leios-voting-concrete`](https://github.com/input-output-hk/ouroboros-consensus/compare/e43bc4d1e...ch1bo/leios-voting-concrete))

```haskell
-- Cardano-specific; lives in Ouroboros.Consensus.Cardano.Node.Leios
getLeiosCommittee :: CardanoLedgerState c mk -> Maybe Committee
getLeiosCommittee = \case
  LedgerStateConway _conwayLs ->
    Just (error "FIXME: from ledger state") 
  _ -> Nothing
```

`runLeiosVoting` calls `getLeiosCommittee` against the current ledger inside
the loop and abstains on `Nothing`. The kernel exposes a single
`initNodeKernelHook :: ResourceRegistry m -> NodeKernel m … blk -> m ()`
field; cardano callers set it to `cardanoLeiosNodeKernelHook`, non-cardano
callers default to `\_ _ -> pure ()`.

**Pros:**

- One new module owns all the cardano-specific Leios threading; readers can
  follow it without hopping through HFC machinery or class instances.
- The era gate is a single `case` arm; adding a new Leios-enabled era is one
  extra arm.
- Mini-protocols, vote state and EB store stay always-wired (under the
  inert-but-present assumption), so no second wiring point needs to be kept in
  sync with the era gate.
- Consistent with existing cardano-specific glue in
  `Ouroboros.Consensus.Cardano.*`.

**Cons:**

- Concrete code over `CardanoBlock StandardCrypto`. A non-cardano chain that
  wants to run Leios cannot reuse this code; it would need its own hook.
- The `initNodeKernelHook` field is added on every `NodeKernelArgs` construction
  site (~11 locations); each picks either the cardano hook or the no-op default.
- The hook fires after `initNodeKernel`, so anything that needs to be wired
  *before* the kernel exists (e.g. a hypothetical era-conditional mini-protocol)
  still has no dedicated extension point — but under the inert-but-present
  assumption this is not currently needed.
- Forgetting to hit the Conway arm compiles — silent abstain at runtime.

## Decision criteria

In priority order:

1. **Simplicity** — fewest moving parts, shortest path from "the active era is
   X" to "voter does Y", measured both locally (this one feature) and globally
   (across all Leios-adjacent per-era code we expect to touch).
2. **Test surface** — least code that the test framework has to know about and
   that test authors have to populate when adding a new test.
3. **Consistency with existing mechanism** — the codebase has both patterns
   for era-conditional behaviour generally (typeclass-shaped:
   `LedgerSupportsProtocol`, `BlockSupportsDiffusionPipelining`; handle-
   shaped: `BlockForging`). For *Leios-adjacent per-era code* specifically,
   both patterns are already in active use: handle (`BlockForging.forgeBlock`)
   for forge-loop extension, and class (`ResolveLeiosBlock`) for ChainDB
   block validation extension. Voting's choice of mechanism therefore
   doesn't reduce mechanism count — it picks which existing precedent it
   joins, or it adds a third.

## What we need now

The runtime requirement is: a single ledger-state check ("is the active era
Conway?") gates whether the voting loop emits a vote on each acquired EB.
Everything else — vote state, EB store, mini-protocol handlers — is constructed
unconditionally and goes idle when the era is not Leios-enabled.

Against the criteria:

- **Simplicity (local):** option C expresses the runtime check as a `case` arm
  in the one module that owns the voting code. Options A and B move that
  decision into the type system / `OptNP` construction respectively, with no
  observable behavioural benefit *for voting alone* today.
- **Simplicity (global):** Leios already has two extension mechanisms in
  active use (handle via `BlockForging`, class via `ResolveLeiosBlock`).
  Options A and B each join one of them and don't increase the mechanism
  count; option C adds a third (post-construction kernel hook).
- **Test surface:** options A and B both add fields the test framework must
  thread (`HasLeiosVoting` superclass on `RunNode` via constraints;
  `LeiosVoting` on `TestNodeInitialization`). Option C adds one
  `initNodeKernelHook` field on `NodeKernelArgs`, which test authors set to the
  no-op default and forget about. Note that A's "instance per block type"
  cost is not new burden — `ResolveLeiosBlock` already imposes the same on
  the same set of block types.
- **Consistency:** A and B are tied. A is consistent with `ResolveLeiosBlock`
  (Leios-adjacent class, ledger/era-shaped, single method); B is consistent
  with `BlockForging` (Leios-adjacent handle, kernel-side, per-era
  construction). Voting's structural shape (single function from ledger
  state to `Maybe Committee`) is closer to `ResolveLeiosBlock`; voting's
  wiring point (kernel thread spawned alongside the forge thread) is closer
  to `BlockForging`. C is consistent with neither and matches `rnNodeKernelHook`
  precedent for generic post-construction wiring instead.

**No clean winner.** The criteria split:

- C wins local simplicity and test surface decisively.
- A and B win global simplicity (no new mechanism category) and consistency
  with existing Leios per-era extension points.
- Between A and B: structural shape favors A (mirrors `ResolveLeiosBlock`);
  wiring location favors B (mirrors `BlockForging`).

Strict reading of the priority order (simplicity → test surface →
consistency) tips the recommendation back to **option C**: it dominates the
two highest-priority criteria, and the consistency loss is to a third
mechanism category rather than a duplicate. If global simplicity (avoiding
a third mechanism) is weighted higher than local simplicity, **A or B**
become preferable, with the choice between them coming down to taste —
"voting is class-shaped per-era logic" (→ A, sibling of `ResolveLeiosBlock`)
or "voting is handle-shaped kernel-side logic" (→ B, sibling of
`BlockForging`).

## What would change the call

- A second non-Cardano chain wants to run Leios with its own committee
  selection. Option B (or A) becomes more attractive — option C would need a
  parallel hook per chain.
- A future Leios subsystem genuinely cannot be "wired but inert" and must be
  conditionally constructed at startup. Option B's bundle handle fits that case.
- Compile-time enforcement that every era declares its Leios stance becomes a
  hard requirement. Option A is the only one that delivers it.
