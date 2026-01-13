# Ouroboros-consensus core Changelog

# Changelog entries

<a id='changelog-0.28.0.3'></a>
## 0.28.0.3 -- 2025-12-16

### Patch

- Fix leaky read lock acquisition that could lead to whole node deadlock.

- Force snapshot tracer to avoid retaining the Genesis state.

<a id='changelog-0.28.0.2'></a>
## 0.28.0.2 -- 2025-10-29

### Patch

- Ensure Mempool always deallocates stale forkers, or rather that it does not
  try to allocate a new one unless completely necessary and closes the old one
  in the process.
- Fix double read lock acquisition when opening a forker in LedgerDB V1.
- Ensure the read lock acquired when opening a forker is released in the presence of exceptions.
- Ensure the forker is tracked in the resource registry from the start and it will only deallocate the read lock once.
- Force the BackingStore tracer evaluation.

<a id='changelog-0.28.0.1'></a>
## 0.28.0.1 -- 2025-10-23

### Patch

- Backport #1603: Streaming of ledger tables in `snapshot-converter`

<a id='changelog-0.28.0.0'></a>
## 0.28.0.0 -- 2025-09-29

### Patch

- Changed ChainSel to reprocess LoE-delayed blocks even when LoE is disabled.

- Changed GDD to trigger chain selection when caught-up. In certain edge cases,
  this enables the node to promptly select a better chain right after concluding
  that it is caught-up.

- Changed the V2 LedgerDB `LedgerTablesHandle`s to actually be closed in all
  cases. With the current (only) backend (in-memory), this doesn't matter, but
  on-disk backends (like LSM trees) need this.

- Fix serialization of `TriggerHardForkNotDuringThisExecution`.

- LedgerDB.V2: prevent race condition when creating snapshots.

- The backing store of the V1 LedgerDB was only tracked in the
  resource registry if we were starting from Genesis. Now the backing
  store will be properly tracked in the resource registry even when we
  start from a snapshot.

- Closing the LedgerDB will no longer release all the open forkers,
  but instead invalidate them by emptying the ldbForkers map, so that
  the only possible operation that could be performed is closing them
  in the LedgerDB clients, such as ChainSel or the forging loop.

- Closing the forker is idempotent, and it was performed both when
  calling `forkerClose` as well as when the resource registry of the
  LedgerDB client was going out of scope. Now, `forkerClose` will
  release the resource from the registry so this won't run twice.

- The mempool will now carry its own forker instead of acquiring one on each
  revalidation. This particularly implies that the mempool will no longer
  re-sync under the hood while trying to add a transaction, and only the
  background thread will perform such a re-sync.

- The mempool now has its own registry in which it allocates forkers. The
  background thread was moved to this inner registry such that it can access the
  mempool internal registry, but an action to cancel it will still live in the
  outer registry, to ensure the thread is closed before we attempt to close the
  mempool internal registry. Otherwise we would run into a race condition if the
  background thread would attempt a resync while the internal registry was being
  closed.

- Changed ChainSel reprocessing of blocks postponed by the Limited of Eagerness
  (Genesis), which in particular should be more efficient.

### Non-Breaking

- Only open BackingStore ValueHandles in V1 when we perform a UTxO operation.

- Support `NodeToClientV_22`.
- Using `io-classes-1.8.0.1`

- Ensure uncommitted forkers do not leak Ledger tables handles.

- Gate `NoThunks` invariant checks behind the `expensive-invariants` build to allow for:
  + No invariant checking in production
  + Cheap (domain-specific) invariant checking in regular CI
  + Cheap and expensive invariant checking in nightly CI

### Breaking

- Use new mlocked KES API to represent KES sign keys internally. This ensures
  that KES keys are securely erased when replaced with a newer evolution or a
  fresh key, and that they will not spill to disk or swap. See
  https://github.com/IntersectMBO/cardano-base/pull/255.
- Add `finalize` method to `BlockForging`, and use it where necessary to clean
  up when a block forging thread terminates (see `forkLinkedWatcherFinalize`)

- LedgerDB: added new trace events (enabling new tests).

- Delete `Ouroboros.Consensus.HardFork.Combinator.Compat`

- ChainDB internals: changed type of `FollowerHandle.fhSwitchFork`.

- Removed `getLedgerTablesAtFor` from the ChainDB API. Clients now have to
  actually open a forker and manage it.

- Changed pruning of immutable ledger states to happen on LedgerDB garbage
  collection instead of directly on every block adoption. This is purely an
  internal refactoring (with breaking API changes) supporting predictable ledger
  snapshotting.

- Avoid maintaining volatile ledger states during ledger replay, making it
  slightly more efficient.

- Changed `SelectView` to be a data type instead of an associated type of
  `ConsensusProtocol`, which is the combination of a `BlockNo` and a
  `TiebreakerView`, which is a new associated type of `ConsensusProtocol`. This
  makes it explicit that `ouroboros-consensus` is targeting longest chain
  protocols.

  - Removed `PBftSelectView`, use `SelectView PBft` instead.

  - Removed `HardForkSelectView`, use `SelectView (HardForkProtocol xs)` instead.

- Changed `AddedReprocessLoEBlocksToQueue` to take an `Enclosing' Word` (the queue size), just like `AddedBlockToQueue`.

- `PoppedBlockFromQueue` no longer takes an `Enclosing`. `PoppedBlockFromQueue RisingEdge` is replaced by `PoppingFromQueue`.

- Using `LedgerRelayAccessPoint` rather than `RelayAccessPoint` in `Ouroboros.Consensus.Ledger.SupportsPeerSelection`.

- Changed `SelectionChangedInfo.newTipTrigger` (contained in tracing types) to
  be a `Maybe` to accurately reflect Ouroboros Genesis-related (Limit on
  Eagerness) triggered chain selection.

- Renamed `IgnoreBlockOlderThanK` to `IgnoreBlockOlderThanImmTip` for future-proofing.
- Renamed and simplified `olderThanK` to `olderThanImmTip`.

- LedgerDB: generalized over the criterion used to determine which states are
  volatile/immutable, in preparation for Ouroboros Peras.

  Concretely, `LedgerDB.openDB` takes a new argument, `GetVolatileSuffix m blk`.
  For Praos behavior, use `praosGetVolatileSuffix`.

- `forkerRangeRead` now returns also the maximal key found in the backend.

- Group snapshot management functions in the new datatype `SnapshotManager`.

- Delete `LedgerSupportsOnDiskLedgerDB` constraint and created `LedgerSupports(V1|V2)LedgerDB`.

- Introduce `Ouroboros.Consensus.Block.SupportsPeras` with types related to Peras.
  - All new types are re-exported through `Ouroboros.Consensus.Block`.
- Introduce `Ouroboros.Consensus.Peras.Weight` with weight computation related types and functions for chains and fragments.
- Introduce a new benchmark suite `PerasCertDB-bench`
- Add property tests and benchmarks for weight computation on chain and fragments

<a id='changelog-0.27.0.0'></a>
## 0.27.0.0 -- 2025-05-15

### Patch

- Fix potential race condition in `Follower` forwarding.

- Bugfix in Ouroboros Genesis. Added the @GsmState@ argument to the
  @registerClient@ function for the ChainSync Jumping optimization. If the node
  is in the @GSM.CaughtUp@ state, new peers now immediately disengage from CSJ.

- Bump `ouroboros-network` packages dependencies.

### Non-Breaking

- Maintain a parallel selection with time annotations since the
  `ConsensusBlockFetchInterface` uses the same type argument for ChainSync
  candidates and the current selection.

- Drop GHC 8.10 support.

### Breaking

- Define `HeaderWithTime`.

- Use `HeaderWithTime` for the ChainSync candidates.

- Remove `SlotForgeTimeOracle` and its use in the BlockFetch client interface,
  as we no longer translate time in the BlockFetch client.

- Rename `GetLedgerConfig` to `DebugLedgerConfig`, indicating that it is a debug
  query, meaning that compatibility across node versions is not guaranteed.

  Please let us know if you want to rely on this query and would benefit from
  proper backwards-compatibility guarantees.

<a id='changelog-0.26.0.0'></a>
## 0.26.0.0 -- 2025-04-21

### Breaking

- Define new class `SerializeTablesWithHint` to allow decoding with
  sharing in the Cardano case.
- `valuesMKDecoder` and `valuesMKEncoder` now require a
  `SerializeTablesHint`, i.e. a ledger state.
- Remove requirement for `MemPack (TxOut l)` in favor of
  `SerializeTablesWithHint`.
- Added new class `MemPackTxOut x` for implying `MemPack (TxOut
  (LedgerState x))`.

<a id='changelog-0.25.0.0'></a>
## 0.25.0.0 -- 2025-04-16

### Non-Breaking

- Decrease the maximum permissible clock skew from 5s to 2s, this time for real.

### Breaking

- Implement the UTxO-HD feature. See the documentation in [the
  webpage](https://ouroboros-consensus.cardano.intersectmbo.org/docs/references/miscellaneous/utxo-hd/).
  - The `LedgerState` type family changed its kind to `Type -> (Type -> Type) ->
    Type` to account for `MapKind`s.
  - `LedgerTables` store a `MapKind` of `TxIn l` to `TxOut l`.
  - The `HardFork` block's `TxIn` is `CanonicalTxIn` which is stable across
    eras.
  - Applying blocks and ticking consume `ValuesMK` and produce `DiffMK`.
  - The LedgerDB has three implementations: `V1InMemory`, `V1LMDB` and
    `V2InMemory`. The first one is not intended to be used in production.
  - The LedgerDB keeps track of differences from blocks and flushes them to the
    backend.
  - Snapshots have changed in format, now there are two files in each snapshot
    instead of one.
  - To evaluate forks, the LedgerDB exposes the `Forker` and the
    `ReadOnlyForker` interfaces.
  - `BlockQuery` is now parametrized by a `QueryFootprint` type.
  - `HardFork` `BlockQuery` are now processed slightly different than single era
    `BlockQuery`, see `BlockSupportsHFLedgerQuery`.
  - The mempool state is now held in a `TMVar` instead of a `TVar` to be able to
    acquire it, read values from the backend and update it.

<a id='changelog-0.24.0.0'></a>
## 0.24.0.0 -- 2025-04-03

### Non-Breaking

- Define some functions in terms of SOP combinators leveraging the recent
  simplification of `Index` in `sop-extras` package.

### Breaking

- Add method `blockQueryIsSupportedOnVersion` to `BlockSupportsLedgerQuery`.
- Export new function `querySupportedVersions`.
- Split out `ImmutableEraParams` so that the test blocks don't have to
  instantiate the `SingleEraBlock` omnibus.
- Remove the `NoThunks Bimap` orphan instance (it's now upstream in
  the `resource-registry` library).

<a id='changelog-0.23.0.0'></a>
## 0.23.0.0 -- 2025-03-25

### Breaking

- Removed legacy `HardForkSpecificNodeToClientVersion`s and related code.

- Add `TxMeasureMetrics (TxMeasure blk)` constraint to `CanHardFork`

- Expose `ValidationPolicy` and `ComputeLedgerEvents` when calling
  ledger rules for block application and ticking. This allows the user
  to choose any validation policy form the `small-steps` package.

- Make the type of `Nary.inject` more precise.
  The old type involves oracular data, so any required changes downstream are almost certainly limited to testing code.

- `SecurityParam` is now `NonZero` as needed by Ledger.

- Added a new CSJ tracer to ChainSync client interface.

### Non-Breaking

- Remove redundant derived Typeable instances.

- Bump upper bound on `base` dependency.

- Use new `NodeToClientV_20`.

### Patch

- Label the leaky bucket thread.

<a id='changelog-0.22.0.0'></a>
## 0.22.0.0 -- 2025-01-08

### Patch

* Remove upper bound on `cardano-ledger-core`

* Use [`resource-registry`](https://hackage.haskell.org/package/resource-registry).

### Breaking

- Integrated new bulk sync BlockFetch logic.

- CSJ: implemented rotation of dynamos.

- ChainDB: let the BlockFetch client add blocks asynchronously

- GDD: added rate limit

- Tweaked certain edge cases in the GDD and ChainSync client ([#1179](https://github.com/IntersectMBO/ouroboros-consensus/pull/1179))

- Remove `cdbFutureBlocks` from `ChainDbEnv`.
- Remove `BlockInTheFuture`, `ChainSelectionForFutureBlock`, `CandidateContainsFutureBlocks`, and `CandidateContainsFutureBlocksExceedingClockSkew` from `TraceAddBlockEvent`.
- Remove `cdbCheckInFuture` from `CBD`.
- Remove `cdbsCheckInFuture` from `ChainDbSpecificArgs`.
- Remove `CheckInFuture m blk` argument from `completeChainDbArgs`.
- Remove `CheckInFuture m blk` argument from `initialChainSelection`.
- Remove `cdbsCheckInFuture` from `ChainDbSpecificArgs`.
- Delete module `Ouroboros.Consensus.Fragment.InFuture`. `ClockSkew` functions live now in `Ouroboros.Consensus.MiniProtocol.ChainSync.Client.InFutureCheck`.
* Remove ``InvalidBlockReason`, since it was now simply wrapping up `ExtValidationError`.

- Updated to `typed-protocols-0.3.0.0`.
- The `ChainSync` client now requires `MoandLabelledSTM` constraint.
- `NodeToClientV_19` was added in `ouroboros-network-api-0.11`.

- Drop NodeToClient versions < 16.

- When writing a ledger state snapshot to disk, calculate the state's CRC32 checksum and write it to a separate file, which is named the same as the snapshot file, plus the `.checksum` extension.
- When reading a snapshot file in `readSnapshot`, calculate its checksum and compare it to the value in the corresponding `.checksum` file. Return an error if the checksum is different or invalid. Issue a warning if the checksum file does not exist, but still initialise the ledger DB.
- To support the previous item, change the error type of the `readSnapshot` from `ReadIncrementalErr` to the extended `ReadSnaphotErr`.
- Checksumming the snapshots is controlled via the `doChecksum :: Flag "DoDiskSnapshotChecksum"` parameter of `initFromSnapshot`. Ultimately, this parameter comes from the Node's configuration file via the `DiskPolicy` data type.
- Extend the `DiskPolicyArgs` data type to enable the node to pass `Flag "DoDiskSnapshotChecksum"` to Consensus.

* Use [`rawlock`](https://hackage.haskell.org/package/rawlock) instead of the in-tree implementation.

### Non-breaking

- Make `Ouroboros.Consensus.Util.CBOR.readIncremental` optionally compute the checksum of the data as it is read.
- Introduce an explicit `Ord` instance for `DiskSnapshot` that compares the values on `dsNumber`.
- Introduce a new utility newtype `Flag` to represent type-safe boolean flags. See ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Util.hs.
- Use `Flag "DoDiskSnapshotChecksum"` to control the check of the snapshot checksum file in `takeSnapshot`, `readSnapshot` and `writeSnapshot`.

<a id='changelog-0.21.0.0'></a>
## 0.21.0.0 -- 2024-10-14

### Patch

- Used new `AF.splitAtSlot` function in GDD.

- Replace `bracketOnError` with `modifyMVar` in the Cached Index implementation.

### Non-Breaking

- Added a `Serialise ByteSize32` instance.

- Decrease the maximum permissible clock skew from 5s to 2s.

- Implement `Monad(LabelledSTM,InspectSTM,TraceSTM,Say)` instances for `WithEarlyExit`.

### Breaking

- Consolidate `TxLimits` in the mempool.
     - Remove `Mempool.`getTxSize`; the snapshot interface contains byte sizes
       now.

     - Transaction size, block capacity, and mempool capacity are
       multi-dimensional vectors (`ExUnits`, etc), instead of merely bytes:
       `TxMeasure`.

     - A transaction cannot be added if it would push any component of the size
       over that component of the mempool capacity.

     - The mempool capacity override is still specified in terms of bytes, but
       the magnitude is interpreted via division as a block count, rounded up.

- Pass a correctly-sized prefix of the mempool to the forging functions,
  instead of its entire contents. The mempool's finger tree is best way to find
  that cutoff.

- Added functionality to disallow historical `MsgRollBackward`s and
  `MsgAwaitReply`s in the ChainSync client.

- ChainDB: allow to trigger chain selection synchronously

- Removed `RealPoint` argument from `ReplayFromSnapshot`. Use the `ReplayStart`
  field instead.

- Made `TraceGDDEvent` into a sum type, added a new terse constructor for when
  we disconnect from peers (this is supposed to get a high severity in the
  tracing system).

- Add TraceMempoolSynced to TraceEventMempool for tracing mempool sync time.

- Added `DiffusionPipeliningSupport` to Config module. Since o-network does not track
  any longer whether pipelining support is enabled for a particular `NodeToNodeVersion`
  this capability was moved into the consensus layer to preserve generality.
  - mkBlockFetchConsensusInterface and bracketChainSyncClient signatures were adapted
    to leverage the new type.

<a id='changelog-0.20.1.0'></a>
## 0.20.1.0 -- 2024-08-26

### Non-Breaking

- Bump to `nothunks` 0.2
- Add `NoThunks` orphan instance for `NoGenesis era`
- Add BlockSupportsSanityCheck to check for common configuration issues which may manifest themselves in unusual but not necessarily immediately obvious ways. For now it only checks that `k` is the same across all eras.
- Mempool: also changed to use the more conservative value of Ledger's
  `maxRefScriptSizePerBlock`, ie 1MiB, that was decided on for Conway.
- The error `MissingBlock(EmptySlot)` now exposes more information when thrown.

### Breaking

- Update `txSize` to return `SizeInBytes` instead of `TxSizeInBytes`
- Expose `SizeInBytes` instead of `TxSizeInBytes`
- Update `mpEnvTxSize`and `txSizesInBytes` to return `SizeInBytes` instead of `TxSizeInBytes`
- `initMempoolEnv`, `extendVRNew`, `openMempool`, `openMempoolWithoutSyncThread`, `implAddTx`, `implTryAddTx` and `openMockedMempool` now require callback `(GenTx blk -> SizeInBytes)` instead of `(GenTx blk -> TxSizeInBytes)`
- Update `txTicketTxSizeInBytes`,`mSizeBytes`'s types from `TxSizeInBytes` to `SizeInBytes`
- `splitAfterTxSize`, `splitAfterTxSizeSpec` requires `SizeInBytes` instead of `TxSizeInBytes`
- Refactored internals of the CSJ and GDD.
- Improved the behavior of the LoE in certain edge cases.
- Remove the capacity override from forging functions.
- Remove `PerEraProtocolParams` newtype.
- Remove `ProtocolParams` data family.
- `completeChainDbArgs` now requires two file-systems. This allows to place the
  immutable data (which doesn't need to be stored in a very performant device)
  somewhere else than the volatile data.
- New `ChainDB.TraceEvent(TraceLastShutdownUnclean)` trace message to be emitted
  when the `clean` marker is missing and the node will therefore revalidate all
  the data.
- `TookSnapshot` event now carries a `EnclosingTimed` field to trace how much
  time it took to make the snapshot.

<a id='changelog-0.20.0.0'></a>
## 0.20.0.0 -- 2024-07-02

### Breaking

- Added `TickedLedgerState` argument to `txMeasure`.

<a id='changelog-0.19.1.0'></a>
## 0.19.1.0 -- 2024-06-26

### Non-Breaking

- Track size of txs' ref scripts in mempool.

<a id='changelog-0.19.0.0'></a>
## 0.19.0.0 -- 2024-06-19

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
## 0.18.0.0 -- 2024-05-13

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
## 0.16.0.0 -- 2024-02-23

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
## 0.15.0.0 -- 2024-01-29

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
## 0.14.0.0 -- 2023-11-30

### Non-Breaking

- New internal testing module.

- Update to `io-sim 1.3.1.0`.
- Update index-state for `ouroboros-network 0.10.1.0` and
  `ouroboros-network-api 0.6.1.0`.

### Breaking

 - ChainSync client: remove redundant `DoesntFit` exception

<a id='changelog-0.13.0.1'></a>
## 0.13.0.1 -- 2023-11-14

### Patch

- Update to `vector ^>=0.13`

<a id='changelog-0.13.0.0'></a>
## 0.13.0.0 -- 2023-10-26

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
## 0.12.0.0 -- 2023-09-27

### Breaking

- Refactorings in unstable test libraries.

<a id='changelog-0.11.0.0'></a>
## 0.11.0.0 -- 2023-09-06

### Patch

- Use `ouroboros-network-0.9.0.0`.
- Use `io-classes-1.2` and `strict-checked-vars-0.1.0.3`.

- Use `strict-checked-vars-0.1.0.4`.

### Non-Breaking

- Add `StrictMVar`s with default `NoThunks` invariants
    `Ouroboros.Consensus.Util.NormalForm.StrictMVar`.

### Breaking

- Removed the orphaned `NoThunk` instance for `Time` defined in `si-timers`
  package.

- Replace `StrictSVar`s by `StrictMVar`s where possible.

<a id='changelog-0.10.0.1'></a>
## 0.10.0.1 -- 2023-08-21

### Patch

- Removed the `expose-sublibs` cabal flag, since Cabal/Nix handled it poorly.
- Instead, added a `unstable-` prefix to the name of each sublibrary, to
  strongly indicate that we ignore them when evolving the package's version.

<a id='changelog-0.10.0.0'></a>
## 0.10.0.0 -- 2023-08-18

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
## 0.9.0.0 -- 2023-07-06

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
## 0.8.0.0 -- 2023-06-23

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
## 0.7.0.0 -- 2023-05-19

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
## 0.6.0.0 -- 2023-04-28

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
## 0.4.0.0 -- 2023-04-10

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
## 0.3.1.0 -- 2023-03-07

### Non-Breaking

- Add `mkCapacityBytesOverride`, a convenience function to create an override
  for the mempool capacity using the provided number bytes.

- Fix version bounds for the bundle.

- Deprecate the `Test.Util.Classify` module from `ouroboros-consensus-test` in
  favour of the `Test.StateMachine.Labelling` module from
  `quickcheck-state-machine`.

<a id='changelog-0.3.0.0'></a>
## 0.3.0.0 -- 2023-02-27

### Breaking

- `Ouroboros.Consensus.Storage.LedgerDB.*` and `Ouroboros.Consensus.Mempool.*`
  modules now have deprecation warnings for the previously exposed API to ease
  updates downstream. Old modules have deprecation headers and also every
  function and type exposed is now an alias to the right entity coupled together
  with a deprecation warning.

<a id='changelog-0.2.1.0'></a>
## 0.2.1.0 -- 2023-02-23

### Non-Breaking

- Exposed the `Pushing` newtype wrappers for the tracing of the `LedgerDB`

<a id='changelog-0.2.0.0'></a>
## 0.2.0.0 -- 2023-02-09

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
## 0.1.0.2 -- 2023-01-25

### Patch

- Version bump on ledger-agnostic packages to move in lockstep.

---

### Archaeological remark

Before following a more structured release process, we tracked most significant
changes affecting downstream users in the
[interface-CHANGELOG.md](https://github.com/IntersectMBO/ouroboros-consensus/blob/8d8329e4dd41404439b7cd30629fcce427679212/docs/website/docs/interface-CHANGELOG.md).
