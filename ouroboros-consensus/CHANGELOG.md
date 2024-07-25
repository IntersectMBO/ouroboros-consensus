# Ouroboros-consensus core Changelog

# Changelog entries

<a id='changelog-0.20.0.0'></a>
## 0.20.0.0 — 2024-07-02

### Breaking

- Added `TickedLedgerState` argument to `txMeasure`.

<a id='changelog-0.19.1.0'></a>
## 0.19.1.0 — 2024-06-26

### Non-Breaking

- Track size of txs' ref scripts in mempool.

<a id='changelog-0.19.0.0'></a>
## 0.19.0.0 — 2024-06-19

### Patch

- Updated dependencies, but no changes to the interface.

### Non-Breaking

- Fixed GDD implementation. (still disabled by default)

- ImmutableDB: added `headerToTip`.

### Breaking

- Implemented a first version of CSJ (ChainSync Jumping). (disabled by default)

- Added `getHashForSlot` to the internal ImmutableDB API.

- ImmutableDB `blockToTip`: relaxed constraints.

<a id='changelog-0.18.0.0'></a>
## 0.18.0.0 — 2024-05-13

### Non-Breaking

- Fixed LoE implementation. (still disabled by default)

- ChainSync client: removed redundant intersection check with selection (we
  already do that on every RollForward).

- Un-orphan instances for `Condense` and `HeaderHash (Ticked l)`.

- Provide `NoThunks` instances for:
  - `Sum a`,
  - `RAWLock m st`,
  - `StrictTVar (WithEarlyExit m) a`,
  - `StrictSVar (WithEarlyExit m) a`
- Added `Complete` and `Incomplete` type aliases for arguments.
- Implement `HTrans` instance for `HardForkState`
- `SomeSecond` became poly-kinded.

### Breaking

- Added new `BlockSupportsDiffusionPipelining` class (as well as supporting data
  types), which is a superclass constraint of `SingleEraBlock` and `RunNode`.
  Added the new necessary instances.

- Update BlockFetch punishment logic for `BlockSupportsDiffusionPipelining`.

- Update ChainSel tentative header logic for `BlockSupportsDiffusionPipelining`.

- Remove now-obsolete `Ouroboros.Consensus.Util.TentativeState` module.

- Added the `eraGenesisWin` to `EraParams` and adapted serialisation for
  backwards-compatibility. Also added corresponding support to the HFC
  interpreter.

- Implemented a first version of the GDD (Genesis Density Disconnect) governor.
  (disabled by default)

- Refactored ChainSync client argument passing.

- Introduced new `ChainOrder` (with `preferCandidate`) class for `SelectView`s,
  and add necessary instances. Adapted `preferAnchoredCandidate` to use
  `preferCandidate` instead of relying on `preferAnchoredFragment`.

- Tweak the ChainDB arguments:
  - Remove unused fields in `CDB`:
    - `cdbTraceLedger` this was *always* set to nullTracer, furthermore it would trace the whole LedgerDB.
    - `cdbChunkInfo` was never accessed from the ChainDB.
    - `cdbCheckIntegrity` was never accessed from the ChainDB.
  - Transform `ChainDbArgs` into an isomorphic product of the different arguments of the inner databases.
  - Define most common operations on `ChainDbArgs` as separate functions: `ensureValidateAll`, `updateTracer` and `updateDiskPolicyArgs`
- Tweak the LgrDB arguments:
  - `LgrDB.cfg` and `LgrDbArgs.lgrConfig` are now `LedgerDbCfg (ExtLedgerState blk)` instead of `TopLevelConfig blk`.
  - `defaultArgs` no longer expects a filesystem.
- Tweak the ImmutableDB arguments:
  - `defaultArgs` no longer expects a filesystem.
- Tweak the VolatileDB arguments:
  - `defaultArgs` no longer expects a filesystem.
- Hide the `Identity`/`Defaults` types in `Ouroboros.Consensus.Util.Args` in favor of `Complete`/`Incomplete`.
- Expose `noDefault` to replace `NoDefault`.

- New `TranslateProto` class moved from `ouroboros-consensus-protocol`.

- Renamed `QueryLedger` class to `BlockSupportsLedgerQuery`.
- `StreamAPI` was moved to the new `Ouroboros.Consensus.Storage.ImmutableDB.Stream` module.
   - A `StreamAPI` now can stream specific block components.
   - `NextBlock` was renamed to `NextItem`.
- Removed unused `Ouroboros.Consensus.Util.TraceSize`.
- Removed unused `assertEqWithMessage` function.
- `Mempool.removeTxs` now expects a `NonEmpty (GenTxId blk)` as an argument.
- VolatileDB traces were tweaked
    - `VolatileDB.OpenedVolatileDB` trace message now includes the maximum slot seen.
    - Added `VolatileDB.ClosedDB`.
- Deleted `Ouroboros.Consensus.Util.Some` in favour of `Ouroboros.Network.Protocol.LocalStateQuery.Codec.Some`.

<a id='changelog-0.17.0.0'></a>
## 0.17.0.0 - 2024-04-03

### Breaking

- Implement lightweight checkpointing [#449](https://github.com/IntersectMBO/ouroboros-consensus/issues/449).
  A validation to help nodes follow the historical chain. A new field `topLevelConfigCheckpoints`
  has been added to the `TopLevelConfig` record, with a new type `CheckpointsMap`.
- LoP: run the ChainSync client against a leaky bucket
- Add `ConvertRawTxId` and require it for `SingleEraBlock`.


<a id='changelog-0.16.0.0'></a>
## 0.16.0.0 — 2024-02-23

### Non-Breaking

- Integrate with network-packages and io-sim 1.4.1 packages
- Bump dependencies version bounds

### Breaking

- `cdbDiskPolicy :: DiskPolicy` for `ChainDbArgs` is replaced by `cdbDiskPolicyArgs :: DiskPolicyArgs`
- similarly, `lgrDiskPolicy :: DiskPolicy` for `LgrDbArgs` is replaced by `lgrDiskPolicyArgs :: DiskPolicyArgs`
- `defaultDiskPolicy` is renamed to `mkDiskPolicy` and requires `DiskPolicyArgs` instead of a `SnapshotInterval`

- Mempool API: generalize types of `addTxs` and `addLocalTxs` to any
  `Traversable`.

- Added `cdbsHasFSGsmDB` to the ChainDB args, for the GSM's persistent marker
  file.

- Added arguments to `bracketChainSyncClient` and `ChainSync.DynamicEnv` for
  tracking idling peers.

- Added arguments to `readFetchModeDefault` for sensitivity to the GSM state:
  when using bootstrap peers, simply mimic the GSM state. Otherwise, fall back
  to the legacy logic.

<a id='changelog-0.15.0.0'></a>
## 0.15.0.0 — 2024-01-29

### Patch

- Updated to GHC 9.8.1.
- Updated `ouroboros-network-protocols` 0.6 -> 0.7.

### Non-Breaking

- Update dependency on `nothunks` to `^>=0.1.5`.

### Breaking

- Added a new `InFutureCheck` to the ChainSync client, which requires
  additional arguments to the 'chainSyncClient' definition. The node no longer
  propagates headers/blocks from the future: a ChainSync client thread now
  sleeps until the received header is no longer from the future.

- Bundled up the arguments to the ChainSync client into new record types,
  `ConfigEnv` and `DynamicEnv`.

- Also introduced a `SomeHeaderInFutureCheck` that binds the existential type
  variables separately from the actual payload of the `HeaderInFutureCheck`
  record type.

- Bump `strict-checked-vars` to `^>= 0.2`. Normal-form `StrictTVar`s are
  refactored and moved to their own modules. Normal-form `StrictTVar`s and
  `StrictMVar`s are exported by the `IOLike` module.

- Removed `EraNodeToNodeVersion`, replacing it with `WrapNodeToNodeVersion`.

- Mempool: add reason for transaction removal to `TraceMempoolRemoveTxs`. This
  can be used in the node to enrich the trace output.

<a id='changelog-0.14.0.0'></a>
## 0.14.0.0 — 2023-11-30

### Non-Breaking

- New internal testing module.

- Update to `io-sim 1.3.1.0`.
- Update index-state for `ouroboros-network 0.10.1.0` and
  `ouroboros-network-api 0.6.1.0`.

### Breaking

 - ChainSync client: remove redundant `DoesntFit` exception

<a id='changelog-0.13.0.1'></a>
## 0.13.0.1 — 2023-11-14

### Patch

- Update to `vector ^>=0.13`

<a id='changelog-0.13.0.0'></a>
## 0.13.0.0 — 2023-10-26

### Patch

- Replace all occurrences of `Ticked (LedgerView X)` with `LedgerView X`.

### Non-Breaking

- Added `ChainGenerators`.
  See `checkAdversarialChain` and `checkHonestChain` for the invariants these generators ensure.

- Add `castRealPoint` utility function.

- Export `HardForkSelectView` from
  `Ouroboros.Consensus.HardFork.Combinator.Protocol` (and hence, also from
  `Ouroboros.Consensus.HardFork.Combinator`).

### Breaking

- Remove `Ticked` from the return type of `forecastFor`.
- Remove `Ticked (LedgerView X)` data family instances.
- Remove `Ticked (K a x)` data family instance.
- Remove `WrapTickedLedgerView`.
- Rename `tickedLedgerView` field of `TickedExtLedgerState` to `ledgerView`.

- Rename `NewTipInfo` (contained in the trace constructors
  `AddedToCurrentChain`/`SwitchedToAFork`) to `SelectionChangedInfo`, and add
  the `SelectView`s of the old and the new tip. Concrete motivation is that
  these contain the tie-breaker VRF which is very useful to have at hand in
  various cases.

- Renamed `TriggerHardForkNever` to `TriggerHardForkNotDuringThisExecution`.

<a id='changelog-0.12.0.0'></a>
## 0.12.0.0 — 2023-09-27

### Breaking

- Refactorings in unstable test libraries.

<a id='changelog-0.11.0.0'></a>
## 0.11.0.0 — 2023-09-06

### Patch

- Use `ouroboros-network-0.9.0.0`.
- Use `io-classes-1.2` and `strict-checked-vars-0.1.0.3`.

- Use `strict-checked-vars-0.1.0.4`.

### Non-Breaking

- Add `StrictMVar`s with default `NoThunks` invariants
    `Control.Concurrent.Class.MonadMVar.NormalForm`.

### Breaking

- Removed the orphaned `NoThunk` instance for `Time` defined in `si-timers`
  package.

- Replace `StrictSVar`s by `StrictMVar`s where possible.

<a id='changelog-0.10.0.1'></a>
## 0.10.0.1 — 2023-08-21

### Patch

- Removed the `expose-sublibs` cabal flag, since Cabal/Nix handled it poorly.
- Instead, added a `unstable-` prefix to the name of each sublibrary, to
  strongly indicate that we ignore them when evolving the package's version.

<a id='changelog-0.10.0.0'></a>
## 0.10.0.0 — 2023-08-18

### Patch

- Update `fs-api` dependency to `^>=0.2`

### Non-Breaking

- Add new `mempool-test-utils` public library containing utilities for opening a
  mocked mempool.

- Add `ProtocolParams` data family to `Ouroboros.Consensus.Node.ProtocolInfo`.
- Add `PerEraProtocolParams` newtype to
  `Ouroboros.Consensus.HardFork.Combinator.AcrossEras`.

### Breaking

- Remove `groupOn` and `groupSplit` from `Ouroboros.Consensus.Util`.

<a id='changelog-0.9.0.0'></a>
## 0.9.0.0 — 2023-07-06

### Non-Breaking

- Change the behaviour of `addBlockRunner` so that it notifies all blocked threads if interrupted.

- Add `closeBlocksToAdd` function

### Breaking

- Remove the `pInfoBlockForging` record field from the `ProtocolInfo` type.

- Remove `ProtocolInfo` monad parameter

- Change `AddBlockPromise` API
  - `blockProcessed` now wraps the return value in a new `Processed` type. This is needed
  for improving the async exception safety.

- Change `BlockToAdd` API
  - `varBlockProcessed` now wraps the return value in a new `Processed` type. This is needed
  for improving the async exception safety.

<a id='changelog-0.8.0.0'></a>
## 0.8.0.0 — 2023-06-23

### Patch

- Don't depend on cardano-ledger-binary

- Require `fs-sim >= 0.2` in test libraries.

### Non-Breaking

- Call `cryptoInit` in `defaultMainWithTestEnv`

- Always force new value of StrictMVar before calling putTMVar in updateMVar

- Fix the mempool benchmarks.

- The `pure @(NonEmpty xs)` implementation was unlawful; this has been fixed by
  making it return an `a` for every `xs` (similar to `ZipList`).

### Breaking

- Remove `ConnectionId` `Condense` instance.

- Rename the `StrictMVar` type to `StrictSVar`. Rename related definitions and
  variables to mention `SVar` instead of `MVar`. Rename the `StrictMVar` module
  to `StrictSVar`.

- `IOLike m` now requires `MonadCatch (STM m)` instead of just `MonadThrow (STM m)`.

<a id='changelog-0.7.0.0'></a>
## 0.7.0.0 — 2023-05-19

### Patch

- Remove deprecated modules from `consensus-testlib`.
  * `Test.Util.Blob`
  * `Test.Util.Classify`
  * `Test.Util.FS.Sim.*`
- Remove deprecated modules from the main `ouroboros-consensus` library.
  * `Ouroboros.Consensus.HardFork.Combinator.Util.*`
  * `Ouroboros.Consensus.Mempool.Impl`
  * `Ouroboros.Consensus.Mempool.TxLimits`
  * `Ouroboros.Consensus.Mempool.Impl.Pure`
  * `Ouroboros.Consensus.Mempool.Impl.Types`
  * `Ouroboros.Consensus.Storage.IO`
  * `Ouroboros.Consensus.Storage.FS.*`
  * `Ouroboros.Consensus.Storage.LedgerDB.InMemory`
  * `Ouroboros.Consensus.Storage.LedgerDB.OnDisk`
  * `Ouroboros.Consensus.Storage.LedgerDB.Types`
  * `Ouroboros.Consensus.Util.Counting`
  * `Ouroboros.Consensus.Util.OptNP`
  * `Ouroboros.Consensus.Util.SOP`
- Remove deprecated definitions from non-deprecated modules in the main
  `ouroboros-consensus` library:
  * `Ouroboros.Consensus.Mempool.API`: `MempoolCapacityBytes`,
    `MempoolCapacityBytesOverride`, `MempoolSize`, `TraceEventMempool`,
    `computeMempoolCapacity`.
  * `Ouroboros.Consensus.Storage.ChainDB.Impl.Types`: `TraceLedgerEvent`.
- In the main `ouroboros-consensus` library, remove exports that were only there
  to make deprecated modules compile.
  * `Ouroboros.Consensus.Mempool.Update`: `pureRemoveTxs`, `pureSyncWithLedger`.
  * `Ouroboros.Consensus.Mempool.Impl.Common`: `initInternalState`.

### Non-Breaking

- Map unreleased `NodeToClientV_16` version.

### Breaking

- Renamed `TranslateForecast` to `CrossEraForecaster` and `translateLedgerView`
  to `crossEraForecast`.

<a id='changelog-0.6.0.0'></a>
## 0.6.0.0 — 2023-04-28

### Non-Breaking

- Update `io-sim` dependency to 1.1.0.0.

- Update `ouroboros-network` dependency.

### Breaking

- Remove function `tryAddTxs` from the mempool API. The implementation (Shelly Era)
  of this function relied on the fairness of 'service-in-random-order', and
  endeavoured to maximally fill the mempool. Since the Babbage Era there is an
  increased variation in representational size of transactions for a given cost
  of processing. This means that, under certain conditions, representationally
  large transactions could be stalled in progress between mempools.
  This function was replaced by `addTx`.
- Add a `addTx` function to the mempool API. This function tries to add a single
  transaction and blocks if the mempool can not accept the given transaction.
  This means that entry to a mempool is now a (per-peer) FIFO. This also ensure
  that transactions will always progress, irrespective of size.
  The refactoring introduces two FIFO queues. Remote clients have to queue in both
  of them, whereas local clients only have to queue in the local clients' queue.
  This gives local clients a higher precedence to get into their local mempool under
  heavy load situations.

<a id='changelog-0.5.0.0'></a>
## 0.5.0.0 - 2023-04-24

### Breaking

- Apply new organization of Consensus packages. Absorb the testing packages and
  tutorials.

<a id='changelog-0.4.0.0'></a>
## 0.4.0.0 — 2023-04-10

### Patch

- `ouroboros-consensus` and `ouroboros-consensus-diffusion`: Since the
  filesystem API that lives in `ouroboros-consensus` will live in the `fs-api`
  package for now on, start depending on `fs-api`, and change imports
  accordingly.

- Collapse all imports into one group in every file.
- Adapt to relocation of SOP-related `Util` modules.

### Non-Breaking

- Move `Util` modules that are related only to SOP to `Data.SOP`. Deprecate the
  following modules:

  - `Ouroboros.Consensus.HardFork.Combinator.Util.DerivingVia` ->
    `Ouroboros.Consensus.HardFork.Lifting`
  - `Ouroboros.Consensus.HardFork.Combinator.Util.Functors` ->
    `Data.SOP.Functors`
  - `Ouroboros.Consensus.HardFork.Combinator.Util.InPairs` ->
    `Data.SOP.InPairs`
  - `Ouroboros.Consensus.HardFork.Combinator.Util.Match` ->
    `Data.SOP.Match`
  - `Ouroboros.Consensus.HardFork.Combinator.Util.Telescope` ->
    `Data.SOP.Telescope`
  - `Ouroboros.Consensus.Util.Counting` ->
    `Data.SOP.Counting`
  - `Ouroboros.Consensus.Util.OptNP` ->
    `Data.SOP.OptNP`
  - `Ouroboros.Consensus.Util.SOP` -> split into `Data.SOP.Index`,
    `Data.SOP.Lenses`, `Data.SOP.NonEmpty` and some functions moved to
    `Data.SOP.Strict`

### Breaking

- `ouroboros-consensus`: Move the filesystem API that lives under
  `Ouroboros.Consensus.Storage.FS` and `Ouroboros.Consensus.Storage.IO` to a new
  package called `fs-api`. The original modules become deprecated.

<a id='changelog-0.3.1.0'></a>
## 0.3.1.0 — 2023-03-07

### Non-Breaking

- Add `mkCapacityBytesOverride`, a convenience function to create an override
  for the mempool capacity using the provided number bytes.

- Fix version bounds for the bundle.

- Deprecate the `Test.Util.Classify` module from `ouroboros-consensus-test` in
  favour of the `Test.StateMachine.Labelling` module from
  `quickcheck-state-machine`.

<a id='changelog-0.3.0.0'></a>
## 0.3.0.0 — 2023-02-27

### Breaking

- `Ouroboros.Consensus.Storage.LedgerDB.*` and `Ouroboros.Consensus.Mempool.*`
  modules now have deprecation warnings for the previously exposed API to ease
  updates downstream. Old modules have deprecation headers and also every
  function and type exposed is now an alias to the right entity coupled together
  with a deprecation warning.

<a id='changelog-0.2.1.0'></a>
## 0.2.1.0 — 2023-02-23

### Non-Breaking

- Exposed the `Pushing` newtype wrappers for the tracing of the `LedgerDB`

<a id='changelog-0.2.0.0'></a>
## 0.2.0.0 — 2023-02-09

### Non-Breaking

- Reorganized `Ouroboros.Consensus.Storage.LedgerDB.*` modules. Old modules have
  a deprecation warning for downstream users but otherwise they still export the
  same functionality.

- Added `NodeToClientV_15`, to support the `Conway` era.

- Reorganization on the `Mempool` modules. Stub deprecated modules are in place
  which should ensure that no code breaks downstream just yet. Clients should
  directly import `Ouroboros.Consensus.Mempool`.

### Breaking

- Remove redundant proxy argument for `ledgerDbTip`.

- Removed the `idx` type variable on the `Mempool` and `MempoolSnapshot`
  datatypes in favour of using `TicketNo` always.

- `Ouroboros.Consensus.Node` and `Ouroboros.Consensus.Network` hierarchies of
  modules where moved from `ouroboros-consensus` to
  `ouroboros-consensus-diffusion` package.

<a id='changelog-0.1.0.2'></a>
## 0.1.0.2 — 2023-01-25

### Patch

- Version bump on ledger-agnostic packages to move in lockstep.

---

### Archaeological remark

Before following a more structured release process, we tracked most significant
changes affecting downstream users in the
[interface-CHANGELOG.md](https://github.com/IntersectMBO/ouroboros-consensus/blob/8d8329e4dd41404439b7cd30629fcce427679212/docs/website/docs/interface-CHANGELOG.md).
