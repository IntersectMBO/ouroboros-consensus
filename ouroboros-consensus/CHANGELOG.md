# Ouroboros-consensus core Changelog

# Changelog entries

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
[interface-CHANGELOG.md](https://github.com/input-output-hk/ouroboros-consensus/blob/8d8329e4dd41404439b7cd30629fcce427679212/docs/website/docs/interface-CHANGELOG.md).
