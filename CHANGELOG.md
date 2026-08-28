# Ouroboros-consensus Changelog

The changelog is organized by the main four libraries: Core, Protocol, Diffusion
and Cardano. Each library has their own Patch, Non-Breaking and Breaking
sections.

# Changelog entries

<a id='changelog-4.2.0.1'></a>
## 4.2.0.1 -- 2026-08-28

### Patch

- Disable IOWait time accounting, see [well-typed/blockio-uring#55](github.com/well-typed/blockio-uring/issues/55).

<a id='changelog-4.2.0.0'></a>
## 4.2.0.0 -- 2026-08-27

### Breaking

- `LedgerDB.tryTakeSnapshot` no longer writes any snapshot itself: it only
  enqueues a `SnapshotRequest` on the new `LedgerDB.snapshotRequestQueue`, which
  the ChainDB serves in a dedicated background thread. Accordingly, it lost its
  "copy blocks" and "random delay" arguments, which are now supplied by that
  thread.
- `ChainDB.Internal.intTryTakeSnapshot` lost its arguments for the same reason;
  it now enqueues a request and serves it synchronously, without copying blocks
  to the ImmutableDB or delaying.

### Patch

- Decouple LedgerDB garbage collection from (randomly delayed) snapshotting.

<a id='changelog-4.1.0.0'></a>
## 4.1.0.0 -- 2026-08-11

### Breaking

- Added `hardForkEqGenTxId` and `hardForkCompareGenTxId` to the `CanHardFork`
  class. The `Eq` and `Ord` instances for `OneEraGenTxId` (and hence for
  `TxId (GenTx (HardForkBlock xs))`) now delegate to them, so each hard fork
  chooses how to compare its transaction ids. The methods have no default, so
  existing `CanHardFork` instances must supply them; the exported `rawHashNS`
  is the raw-hash implementation the non-optimizing instances reuse.
- The Mithril snapshot policy (which is also the default policy) now takes a
  snapshot every `40 * k` slots with no offset, instead of every 432,000 slots
  with an offset of 388,800. On mainnet this is one snapshot a day, one of every
  five landing on a Shelley epoch boundary.
- `sfaInterval` is now a `SnapshotInterval`, which is either
  `DefaultSnapshotInterval` (`40 * k` slots, resolved via
  `resolveSnapshotInterval` once the `SecurityParam` is known) or an explicit
  `RequestedSnapshotInterval`.
- `defaultSnapshotPolicy` and `sanityCheckSnapshotPolicyArgs` now take a
  `SecurityParam`.

### Non-Breaking

- Added `snapshotFromIS`, which builds a `MempoolSnapshot` from the mempool's
  internal state in constant time by reusing the transaction sequence
  (`isTxs`) and the cached transaction ids (`isTxIds`) it already maintains.

### Patch

- Made those `Eq`/`Ord` comparisons allocation-free for the Cardano eras, on
  every comparison rather than only same-era ones. The Cardano instance reads
  the transaction id hash as four machine words and compares them in registers,
  instead of serialising both ids to their raw hash. The ordering is unchanged.
- `getSnapshot` and the fast path of `getSnapshotFor` no longer rebuild the
  mempool contents on every call. This was quadratic in the size of the mempool.

<a id='changelog-4.0.0.0'></a>
## 4.0.0.0 -- 2026-07-30

### Breaking

- LedgerDB: implemented *predictable* snapshots, i.e. different nodes with the
  same configuration will now create snapshots for the same slots.
  See 'SnapshotPolicyArgs' for more details.
- Added support for `NodeToNodeV_16`
- Rely on a new version of `ouroboros-network` with support for ObjectDiffusion mini-protocol
- Modify `Ouroboros.Consensus{.Node,.Node.Tracer,.Network.NodeToNode}` to wire-in PerasCertDiffusion similarly to other mini-protocols (e.g. TX-submission)
- Add modules `Ouroboros.Consensus.MiniProtocol.ObjectDiffusion{.Inbound,.Outbound}` with implementations of the ObjectDiffusion protocol (quite similar/inspired from TX-submission, except that client = inbound, server = outbound)
- Add module `Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API` defining `ObjectPool{Reader,Writer}` interfaces, through which ObjectDiffusion accesses/stores the objects to send/that have been received.
- Add modules `Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.PerasCert` and `Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasCert` containing definitions specific to `PerasCert` diffusion through the ObjectDiffusion mini-protocol
- Modify `Ouroboros.Consensus.Node.Serialisation` to add CBOR serialisation (`SerialiseNodeToNode`) for `Point blk`, `Tip blk`, and `PerasCert blk`
Introduced a configurable randomised delay before taking ledger state snapshots.
- Renamed `ldbLastSnapshotWrite` to `ldbLastSnapshotRequestedAt`
- Added a `delay` argument to `implTryTakeSnapshot`.
- Renamed `ledgerDbMaintenaceThread` to `ledgerDbMaintenanceThread`
- `PerasVoteDB` exceptions are now given proper handlers at the node level
- `PerasCertDB`: API reworked entirely, with more consistency w.r.t. in-memory/on-disk side effects
- `ChainDB` API: remove `getPerasCertSnapshot`, add `getLatestPerasCertSeen`, `getLatestPerasCertOnChainRound`, `getPerasCertsAfter`, `getPerasCertIds`, `addPerasVoteWithAsyncCertHandling`, `getPerasVotesAfter`, `getPerasVoteIds`
- `ChainDB` state: now contains a `PerasVoteDB` field (in addition to the already existing `PerasCertDB` one). Modify `ChainDbArgs` accordingly
- Eta-expand `l` over `blk` for some classes and data/type families. In particular
  - `LedgerErr lblk -> LedgerErr l blk`
  - `LedgerCfg lblk -> LedgerCfg l blk`
  - `AuxLedgerEvent lblk -> AuxLedgerEvent blk`
  - `Ticked lblk -> Ticked l blk`
  - `IsLedger lblk -> IsLedger l blk`
  - `ApplyBlock lblk blk -> ApplyBlock l blk`
- Index `LedgerTables`, `TxIn`, and `TxOut` by `blk` instead of by the ledger-state shape `l`. Combined with the prior eta-expansion, this collapses the duplicate `LedgerState`/`Ticked LedgerState`/`ExtLedgerState` instances into one.
  - `TxIn  :: Type -> Type`, `TxOut :: Type -> Type` — keyed by blk
  - `LedgerTables blk mk` — was LedgerTables l mk
  - `HasLedgerTables l blk` — separate l and blk parameters
  - `LedgerTablesAreTrivial l blk`, `SerializeTablesWithHint l blk` — same reshape
  - `IndexedMemPack l blk a` — was IndexedMemPack idx a
  - New `GetBlockKeySets blk` class — `getBlockKeySets` split out of `ApplyBlock`. Necessary to avoid `Proxy l` or `AllowAmbiguousTypes` as `getBlockKeySets` doesn't mention `l` anymore.
  - Removed: `MemPackIdx`, `SameUtxoTypes`, `castLedgerTables`, `TrivialLedgerTables`
  - Remove the V1 LedgerDB and the LMDB backing store. V2 is now the only LedgerDB flavor.
    - Deleted modules: `Ouroboros.Consensus.Storage.LedgerDB.V1` and all its submodules (`V1.Args`, `V1.BackingStore`, `V1.BackingStore.API`, `V1.BackingStore.Impl.InMemory`, `V1.DbChangelog`, `V1.DiffSeq`, `V1.Forker`, `V1.Lock`, `V1.Snapshots`).
    - Deleted public sublibrary `ouroboros-consensus:lmdb` (which exposed `V1.BackingStore.Impl.LMDB`, `.Bridge`, `.Status`).
    - Removed `SeqDiffMK` and the `SerializeTablesHint` type family.
    - Removed the `tryFlush` field of `LedgerDB`.
    - Removed the V1 alternatives `LedgerDbBackendArgsV1` (of `LedgerDbBackendArgs`) and `FlavorImplSpecificTraceV1` (of `FlavorImplSpecificTrace`); both types are now `newtype`s with only the V2 constructor.
    - Removed the db-analyser `--lmdb` flag and the `V1LMDB` constructor of `LedgerDBBackend`.
    - Removed the snapshot-converter `--monitor-lmdb-snapshots-in` / `--input-lmdb` / `--output-lmdb` flags and the `LMDB` constructor of `StandaloneFormat`.
  - Type changes
    - `SnapshotManager m n blk st` → `SnapshotManager m blk st` — V1 took snapshots in `ReadLocked m`, so the second monad parameter is no longer needed. `initialize`, `snapshotsMapM_`, `destroySnapshots`, and `trimSnapshots` lose the `n` parameter
  accordingly.
    - `SerializeTablesWithHint` methods (`encodeTablesWithHint` / `decodeTablesWithHint`) and their `default*` / `trivial*` helpers now take `l blk EmptyMK` directly, instead of `SerializeTablesHint l (LedgerTables blk ValuesMK)`.
    - `openDB` and `Cardano.Tools.DBAnalyser.Run.openLedgerDB` no longer require the `LedgerDbSerialiseConstraints blk` constraint.
- Change ChainDB's `addPerasVoteSync`, `addPerasVoteWithAsyncCertHandling`, `addPerasCertSync`, `addPerasCertAsync` return types to provide explicit information about the outcome of the operation.
- LSM arguments now include a second optional filepath to which, if provided,
  the LSM backend will export snapshots. This is intended to be used with the
  snapshot-converter daemon mode to read LSM snapshots without interfering with
  the running LSM Session.
- Renamed `Ouroboros.Consensus.Protocol.Praos.Views.LedgerView` to `PraosLedgerView`,
  with the fields renamed from `lv*` to `plv*`.
- Removed `PraosEnvelopeError` from the exports of `Ouroboros.Consensus.Shelley.Protocol.Praos`.
- `EnvelopeCheckError (Praos c)` and `EnvelopeCheckError (TPraos c)` are both now
  `Ouroboros.Consensus.Shelley.Protocol.EnvelopeChecks.EnvelopeError`.
- Removed modules `Ouroboros.Consensus.Protocol.Praos.Header` and
  `Ouroboros.Consensus.Protocol.Praos.VRF`. They are now provided by the `cardano-protocol` package (as `Cardano.Protocol.Praos.BlockHeader` and `Cardano.Protocol.Praos.VRF`); import from there instead.
- Change `ShelleyBasedEra` class superclass constraints:
  - change `SL.ApplyBlock era` to `SL.ApplyTick era`.
  - change `SL.GetLedgerView era` to `SL.EraForecast era`.
  - remove `NoThunks (PredicateFailure (EraRule "BBODY" era))`.
- `ShelleyCompatible proto era` has three new superclass constraints:
  - `EncCBORGroup (SL.BlockBody era)`,
  - `SL.EraBlockHeader (ShelleyProtocolHeader proto) era`,
  - `SL.ApplyBlock (ShelleyProtocolHeader proto) era`.
- Remove `ShelleyCompatible (TPraos c) BabbageEra`, `ShelleyCompatible (TPraos c) ConwayEra`,
  and `ShelleyCompatible (TPraos c) DijkstraEra` instances from `Ouroboros.Consensus.Shelley.HFEras`.
  These eras now run under Praos only.
- Remove the `ProtocolHeaderSupportsLedger` class from `Ouroboros.Consensus.Shelley.Protocol.Abstract`.
- `LedgerSupportsProtocol (ShelleyBlock (TPraos crypto) era)` instance now requires `SL.ShelleyEraForecast era`.
- `LedgerSupportsProtocol (ShelleyBlock (Praos crypto) era)` instance no longer requires
  `ShelleyCompatible (TPraos crypto) era` and instead requires `SL.EraForecast era`.
- `protocolInfoCardano`, `protocolInfoShelley` and `protocolInfoTPraosShelleyBased`
  now take an additional initial argument `SomeHasFS m` and returns
  inside `m`. Their callback for creating blocks also returns values in `m`.
- The constraint on the following functions tightens from `Applicative m` to `Monad m`:
  - `Ouroboros.Consensus.Storage.ImmutableDB.Impl.defaultArgs`
  - `Ouroboros.Consensus.Storage.LedgerDB.Args.defaultArgs`
  - `Ouroboros.Consensus.Storage.VolatileDB.Impl.defaultArgs`
  - `Ouroboros.Consensus.Storage.PerasCertDB.Impl.defaultArgs`
  - `Ouroboros.Consensus.Storage.PerasVoteDB.Impl.defaultArgs`
  - `Ouroboros.Consensus.Storage.ChainDB.Impl.Args.updateTracer`
  - `Ouroboros.Consensus.Storage.ChainDB.Impl.fromChainDbEnv`
  - `Ouroboros.Consensus.Util.Enclose.encloseWith`
  - `Ouroboros.Consensus.Storage.LedgerDB.decorateReplayTracerWithGoal`
  - `Ouroboros.Consensus.Storage.LedgerDB.decorateReplayTracerWithStart`
- `Ouroboros.Consensus.Network.NodeToClient.showTracers`,
  `Ouroboros.Consensus.Network.NodeToNode.showTracers`, and
  `Ouroboros.Consensus.Node.Tracers.showTracers` now require `Monad m`.
- Remove the `NoThunks FsPath` orphan instance from `Ouroboros.Consensus.Util.Orphans`;
  it is now provided upstream by `cardano-ledger` (`Cardano.Ledger.Orphans` in
  `cardano-ledger-core`).
- Remove the orphan `Measure ()` instance from
  `Ouroboros.Consensus.Ledger.SupportsMempool`.
- `HasBLSContext` instances for `SIGN` and `VRF` now use `minSigPoPDST` as their base context.
- Upgrade lower bounds for Node 11.1 integration of upstream packages `cardano-base`,
  `cardano-ledger`, `kes-agent`, `ouroboros-network`, `validation`
- Removed `lgrStartSnapshot` from `LedgerDB.LedgerDbArgs`. It was only ever set to
  `Nothing` and was meant for db-analyser, which now selects its starting snapshot
  via the replay goal instead. `initialize` no longer takes a `Maybe DiskSnapshot`.
- Refactored `ConvertRawHash` type class:
  + Introduced `HashSize :: Nat` associated type.
  + Changed `fromRawHash` and `fromShortRawHash` methods to enforce announced size, returning `Maybe` upon failure.
  + Introduced `unsafeFromRawHash` and `unsafeFromShortRawHash` for backwards compatibility.
- Define the default snapshot policy to be Mithril's snapshot policy.
- Delete `OverrideOrDefault` and `provideDefault`.

### Non-Breaking

- Update `Test.ThreadNet.Network` in `unstable-diffusion-testlib` accordingly to the changes made in `Ouroboros.Consensus.Network.NodeToNode`
- add `cdbSnapshotDelayRNG` field to `ChainDbEnv`.
- add `cdbsSnapshotDelayRNG` to `ChainDbSpecificArgs`.
- add `onDiskSnapshotDelayRange` to `SnapshotPolicy`.
- add LedgerDB snapshot delay trace events: `SnapshotRequestDelayed` and `SnapshotRequestCompleted`.
- Implemented generic voting committee interface.
- Implemented pure weighted Fait-Accompli logic.
- Implemented local sortition check for non-persistent seats.
- Implemented wFA^LS voting committee instance.
- Implemented EveryoneVotes voting committee instance.
- Implemented BLS-based crypto helpers to instantiate voting committee implementations.
- `PerasVoteDB` API: expose exceptions that can be thrown by the VoteDB when a vote is added (instead of exporting them from the `Impl.hs` file)
- `ChainDB` helpers: add `addPerasVoteSync`
- `ChainDB` `Background` module: add `garbageCollectPeras`
- `ObjectPoolWriter` instances for `Peras{Cert,Vote}DB` now check that an object is not already present in the DB before trying to validate it to save on expensive validation calls
- Improve generation strategy (more granular) for blocks in statemachine tests of the `ChainDB`
- Expose `getPerasRoundVoteStateMaxTargetedSlot` in the `Peras.Vote.Aggregation` module, for gargabe collection purposes
- Introduce `Ouroboros.Consensus.Util.Bitmap` providing `ByteString`-based compact bitmaps.
- Define `PerasBLSCrypto` scheme with support for all the voting committee superclasses.
- Define concrete `PerasVote` and `PerasCert` types using BLS signatures.
- Define `PerasVoteCompatibleWithVotingCommittee` and `PerasCertCompatibleWithVotingCommittee` type classes with conversions between concrete Peras types and their abstract voting committee counterparts.
- Instantiate `VotingCommitteeSupportsPeras` for both `WFALS` and `EveryoneVotes`.
- Add sanity checks for `SnapshotPolicyArgs` configurations. The node now validates snapshot policy settings on startup and warns about suspicious configurations such as inverted delay ranges, negative delays, disabled or excessively large rate limits, zero on-disk snapshots, and snapshot intervals that do not divide the Cardano mainnet epoch length (breaking Mithril compatibility).
- Introduce `Bytes32RealPoint` for real points with 32byte header hashes.
- Add module `Ouroboros.Consensus.Shelley.Protocol.EnvelopeChecks`; consolidates
  envelope-check logic previously inlined in `Ouroboros.Consensus.Shelley.Protocol.{Praos,TPraos}`.
- Add `forecastToPraosLedgerView` to `Ouroboros.Consensus.Protocol.Praos.Views`.
- Add `Ouroboros.Network.Tx.HasRawTxId` instances for the transaction-id type of every block:
  `ByronBlock`, `ShelleyBlock`, `HardForkBlock`, `DualBlock`, and `SimpleBlock`.
  To match, the `RunNode` class now has a `HasRawTxId (TxId (GenTx blk))` superclass.
- The Shelley ledger queries `GetFilteredDelegationsAndRewardAccounts`,
  `GetStakeDelegDeposits`, `GetFilteredVoteDelegatees` and `GetPoolDistr2` are now
  answered by the corresponding `cardano-ledger` functions
  (`queryStakePoolDelegsAndRewards`, `queryAccountsDeposits`, `queryDRepDelegatees`,
  `querySetSnapshotStakePoolDistr`) instead of being implemented in consensus. The
  query results have not changed.
- Add `mithrilSnapshotPolicyArgs`, which specifies when Mithril should take ledger state snapshots.
- Extend `implForgeCert` with assertions that verify that ordering function
  is compatible with the one used during certificates verification

### Patch

- Create mempool snapshots out of valid transactions instead of a Mempool InternalState.
- Define sensible default values for the `PerasParams` that were previously left as `error "yet undefined"`, following the guidelines given in the [Peras CIP](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0140) and [Peras design document](https://tweag.github.io/cardano-peras/peras-design.pdf).
- Updates on the LSM-trees ecosystem:
  - blockio-uring 0.1.0.3 -> 0.2.0.0
  - blockio 0.1.1.1 -> 0.2.0.0
  - lsm-trees 1.0.0.1 -> 1.1.0.0
- Update to `fs-sim 0.5.0.0`.
- Update to `QuickCheck 2.18`.

<a id='changelog-3.0.1.0'></a>
## 3.0.1.0 -- 2026-04-14

### Non-Breaking

- Add PerasMaxCertRound protocol parameter.
- Add Peras certificate inclusion rules.

### Patch

- Ignore exceptions when deleting a snapshot.
- Allow for empty UTxO tables in tables streaming (which is only relevant for Byron snapshots)
- Fix directory management in LSM-trees snapshot conversion.

<a id='changelog-3.0.0.0'></a>
## 3.0.0.0 -- 2026-04-07

### Breaking

- `reapplyTx` now doesn't compute the differences, and instead relies on `reapplyTxs` already having them cached.
- Removed `ComputeDiffs` in favor of `WhatToDoWithTxDiffs`.

### Non-Breaking

- Don't use `WithTempRegistry` in the VolatileDB open routine.

### Patch

- Transaction differences are cached the first time the transaction is validated, and re-used when revalidating the mempool on forging and syncing with the LedgerDB.

<a id='changelog-2.0.0.0'></a>
## 2.0.0.0 -- 2026-03-31

### Breaking

- ChainDB API: remove `getReadOnlyForkerAtPoint` in favor of
  `allocInRegistryReadOnlyForkerAtPoint`, `openReadOnlyForkerAtPoint` and
  `withReadOnlyForkerAtPoint`.
- LedgerDB API: remove `getForkerAtTarget` in favor of `openForkerAtTarget`
  which no longer expects a `ResourceRegistry`.
- LedgerDB initialization now runs in `WithTempRegistry st m` for tracking the
  LedgerDB resources.
- LedgerDB API: rename `mkLSMArgs` to `mkLSMArgsIO`
- `ValidateArgs`: remove `resourceReg` for a continuation `onSuccess` that will
  be run if the candidate is fully valid.
- The continuation in `localStateQueryServer` now returns a `ResourceKey` as it
  should allocate the resource in a `ResourceRegistry`.
- The Mempool no longer uses `ResourceRegistry` anywhere.

### Non-Breaking

- Bump upper bound on io-classes dependency.

### Patch

- Updated bound on `ouroboros-network` to `^>=1.1`.

<a id='changelog-1.0.0.0'></a>
## 1.0.0.0 -- 2026-03-16

### Breaking

- Added `txWireSize` method to `TxLimits` class to provide
  a CBOR-encoded transaction size as it is when transmitted
  over the network.

- Implement `txWireSize` of `TxLimits` instantiations for Byron and Shelley
- New `ShelleyNodeToClientVersion15`, which support retrieving all (not only big) ledger peers by `GetLedgerPeerSnapshot`.
- New `CardanoNodeToClientVersion19` which maps to `ShelleyNodeToClientVersion15`.

- Added `txLogicTracer` and `txCountersTrace` to `Tracers'`.
- Added `rnTxSubmissionLogicVersion` and `rnTxSubmissionInitDelay` fields to `RunNodeArgs`.
- Added new `getTxChannelsVar`, `getSharedTxStateVar` and `getTxMempoolSem` fields to `NodeKernel`.

- Implement LSM-trees backend for LedgerDB V2 handles.
- Define new `LedgerDbBackendArgs` that will be provided by the node.
- Drop `Eq (Ouroboros.Consensus.Storage.ChainDb.Impl.Types.TraceEvent blk)` instance.
- Delete unused `Ouroboros.Consensus.Storage.LedgerDB.V1.Args.defaultLedgerDbFlavorArgs`.
- LedgerDB V2 forker reading functions now also receive a LedgerState to deserialize values from LSM trees.
- Expose `indexedPackByteArray` and define new `indexedUnpack` mirroring `unpack` from the `mempack` package.

- `srnLdbFlavorArgs` was renamed to `srnLedgerDbBackendArgs` and changed its type to `LedgerDBBackendArgs`.

- Add modules `Ouroboros.Consensus.Storage.PerasCertDB{,.API,.Impl}`, notably defining the types`PerasCertDB`, `PerasCertSnapshot` (read-only snapshot of certs contained in the DB), and `AddPerasCertResult`; alongside their respective methods
- Add modules `Test.Ouroboros.Storage.PerasCertDB{,.StateMachine,.Model}` for q-s-m testing of the `PerasCertDB` datatype. The corresponding tests are included in the test suite defined by `Test.Ouroboros.Storage`

- Make the `ChainDB` aware of the `PerasCertDB`, and modify the chain selection function accordingly. In practice, it means that the candidate fragment is now selected based on its Peras weight, instead of its length.

  Note that if Peras is disabled (which is the default), there is no observable difference.

- Add module `Ouroboros.Consensus.Peras.SelectView`, which introduces a `WeightedSelectView` to correctly measure the length of a chain fragment.

- Change HFC types so that:
  - `EraParams` now records an optional Peras round length.
  - `Bound` now records an optional Peras round number.
  - `Serialize` instances for `EraParams` and `Bound` now account for these
    optional parameters in a backwards-compatible manner.
- Add two new top-level queries:
    ```haskell
    perasRoundNoToSlot :: PerasRoundNo -> Qry SlotNo
    slotToPerasRoundNo :: SlotNo -> Qry PerasRoundNo
    ```
- Add round-trip test between `perasRoundNoToSlot` and `slotToPerasRoundNo`.
- Add Peras-specific test for `perasRoundNoToSlot`.

- In module `Ouroboros.Consensus.Node.GSM`, `GSMView` now has a monadic `getCandidateOverSelection :: STM m (selection -> chainSyncState -> CandidateVersusSelection)` instead of the previous pure `candidateOverSelection`. This is due to the fact that chain comparisons now depend on the set of Peras certificates (if Peras is enabled).

- Add `eraPerasRoundLength` parameters to `{Byron,Shelley}EraParams` structs.

- Flip serialization of `TxIx` in Mempack, to ensure lexicographic order on the
  serialized form matches the Haskell Ord, allowing for incremental streaming of
  values among backends. Note this happens at the same time as the versioning of
  the LedgerTables codec which will induce a replay of the chain.

- Version ledger tables encoding. Define `TablesCodecVersion1`.
- InMemory snapshots used to store the tables in `<snap>/tables/tvar`. Now they
  store the tables in `<snap>/tables`.

- Extracted `Ouroboros.Consensus.Storage.LedgerDB.V2.LSM` into a separate sub-library `ouroboros-consensus-lsm`.
- Extracted `Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Impl.LMDB*` into a separate sub-library `ouroboros-consensus-lmdb`.
- Define `Ouroboros.Consensus.Storage.LedgerDB.V2.Backend.Backend` class that allow for interaction with backends mainly opening them, and define instances for all existing V2 backends.
- Define `Ouroboros.Consensus.Storage.LedgerDB.V1.BackingStore.Backend` class that allow for interaction with backends mainly opening them, and define instances for all existing V1 backends.
- Define `Ouroboros.Consensus.Storage.LedgerDB.APIStreamingBackend` class that allows for streaming in/out from a backend, and define instances for all existing backends.
- Delete `LedgerDbFlavorArgs`. Repurpose `LedgerDbBackendArgs` for the arguments needed by the `Backend` classes to initialize a backend, and expect it in `LedgerDbArgs`.
- Delete `V1.FlavorImplSpecificTrace` and `V2.FlavorImplSpecificTrace` and use `Backend(SomeBackendTrace)` for those.

- Expect `srnLedgerDbBackendArgs :: LedgerDbBackendArgs m blk` as an argument, type which changed semantics in the abstract layer.

- `Ouroboros.Consensus.Storage.LedgerDB.(V1.BackingStore|V2).Backend(Trace)` no longer depends on the running monad `m`.

- Legacy snapshots will be rejected and deleted, instead of crashing consensus.

- Add `rnFeatureFlags` field to `RunNodeArgs` to store enabled experimental feature flags.

- Allow `cardano-node` to integrate `kes-agent`: make the `PraosCredentialsAgent` constructor of `PraosCredentialsSource` usable by removing `Void`.

- Change V2 LedgerDB trace types to include enclosing times.

- Store previous epoch nonce in PraosState and adapt its serialization format.

- ChainDB API: add a new function `waitForImmutableBlock`.
- ImmutableDB API: add a new function `getBlockAtOrAfterPoint_`.

- LSM-trees backend is now able to track the size of the tables. This is exposed
  via `tablesSize`, which changed from `Maybe Int` to `Int`.

- Add `ReasonForSwitch` to `ChainOrder` class, modify `preferCandidate` to
  return `ShouldSwitch (ReasonForSwitch tv)`.
- Implement reasons for switching for `SelectView`
  (`SelectViewReasonForSwitch`: length or tiebreak), `WeightedSelectView`
  (`WeightedSelectViewReasonForSwitch`: weight or tiebreak), `WithEmptyFragment`
  (`WithEmptyFragmentReasonForSwitch`: empty or not, or tiebreak).
- Modify `AddedToCurrentChain` and `SwitchedToAFork` traces to include the
  reason for switching.
- Chain selection now carries the potential reasons for switching, and
  eventually emits the reason for performing the switch in `AddedToCurrentChain`
  or `SwitchedToAFork` traces.

- Added `mkMempoolPredicateFailure` method to `LedgerSupportsMempool`.
- Added `ExnMempoolTimeout` exception to Mempool.
- Added `addTestTx` field to Mempool `API` record type, for testing only.
- Added `MempoolTimeoutConfig`, `DiffTimeMeasure`, `TxMeasureWithDiffTime` types.
- Added the `DiffTimeMeasure` component alongside the `TxMeasure` that the
  Mempool's finger-tree maintains, measuring how much monotonic clock each tx
  took to validate.
- Enriched argument of `snapshotTake` method in the `MempoolSnapshot` record
  type. You can pass `InfiniteDiffTimeMeasure` if you don't care about limiting
  the new dimension.
- Added `Maybe MempoolTimeoutConfig` argument to `openMempool`. Passing
  `Nothing` disables the timeout.

- Added `rnMempoolTimeoutConfig` argument to `RunNodeArgs`.
- Added `TxMeasureWithDiffTime` argument to the `TraceForgedBlock` event.

- Remove unused `blk` type variable in Forkers and related types.

- Add `MempoolRejectionDetails` to `TraceMempoolRejectedTx` (so defensive
  mempool rejections are easy to spot and `CountM` downstream).

- Adapt Praos' Common definitions for returning `ShouldSwitch` in Chain selection.
- Define reasons why a Praos Chain selection would switch to a fork (`PraosReasonForSwitch`).

- Adapt to the fact that block decoders may fail, i.e. change the block annotated decoder types from `Lazy.ByteString -> ShelleyBlock proto era` to `Lazy.ByteString -> Either Plain.DecoderError (ShelleyBlock proto era)`. Very importantly, while `Header` decoding still cannot fail, it has to use the same low-level decoding functions from the Ledger and Networking layers; hence, we have to introduce an `error` call into the `decodeShelleyHeader` to account for an impossible case of `Header` decoding failing and make types match. We aim to remove this error call as soon as possible.
- Introduce transaction levels, adapting to the nested transactions feature in Ledger. Every occurrence of Ledger's `SL.Tx` becomes `SL.Tx SL.TopTx`.
- The serialisation of `ApplyTxError` changes in `NodeToClientV_16`-`NodeToClientV_23` for all eras except Conway (the current era at the moment). This is due to changes in the `ApplyTxError` type in Ledger. This should not impact users submitting transactions in the current era (Conway) which is the expected use case.
- The on-disk serialisation of `LedgerState` and `ExtLedgerState` changed due to the changed in Ledger. This requires a chain replay.

- Rename: `mkMempoolPredicateFailure` to `mkMempoolApplyTxErr`, `mkMkMempoolShelleyPredicateFailure` to `mkEraMkMempoolApplyTxError`.

- Removed unused `Ord` instance for `DiskSnapshot`

- Add `LedgerSupportsPeras` constraint to `SingleEraBlock`.
- Update serialization of Shelley ledger state to encode/decode the new field.

- Add module `Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.API` defining `ObjectPool{Reader,Writer}` interfaces, through which ObjectDiffusion will access/store the objects to send/that have been received.
- Add module `Ouroboros.Consensus.MiniProtocol.ObjectDiffusion.ObjectPool.PerasCert` containing implementation of `ObjectPool` interfaces for `PerasCert(DB)`.

- Cleanup Chain selection:
  - Removed `Ouroboros.Consensus.Fragment.Validated` as it was only an
    indirection over `Ouroboros.Consensus.Fragment.ValidatedDiff`.
  - Initial chain selection now also performs the commit on the forker and
    returns only the selected fragment of headers.
  - Deleted `ChainAndLedger` as it was an unnecessary indirection.
  - Deleted `validateCandidate` as it was an unnecessary indirection to
    `ledgerValidateCandidate` which was now renamed to `validatedCandidate`.
- Cleanup LedgerDB:
  - `validateFork` allows for `l` other than `ExtLedgerState`.
  - `validateFork` expects a non-empty list of headers.
  - `ValidateArgs` now expects an `l` that can be different to `ExtLedgerState`.
  - `Ap` has been simplified to monomorphize the constraints. `applyBlock` and
    related functions now run on an appropriate monad.
  - Classes `ThrowsLedgerErrors` and `ResolvesBlocks` have been deletes as
    unnecessary.
  - `MonadBase` scattered constraints have been removed as unnecessary.
  - `AnnLedgerError` carries a point to the latest valid block instead of a
    forker.

- Updated to `typed-protocols-1.2`, which requires the introduction of a bunch of `NFData` constraints.
- Updated to `cardano-crypto-class-2.3`
- Remove the implementation of the `GetStakeSnapshots` ledger query, which is now imported from `cardano-ledger`. The result type, `SnapShots`, was moved to Ledger as well.
- Update to `random-1.3.1`.

- Absorbed `ouroboros-consensus-lsm` into `ouroboros-consensus:lsm`.
- Absorbed `ouroboros-consensus-lmdb` into `ouroboros-consensus:lmdb`.
- Absorbed `ouroboros-consensus-protocol` into `ouroboros-consensus:protocol`.
- Absorbed `ouroboros-consensus-diffusion` into `ouroboros-consensus:diffusion`.
- Absorbed `ouroboros-consensus-cardano` into `ouroboros-consensus:cardano`.

- Use `RawBlockHash` in `LedgerPeersConsensusInterface` and `GetLedgerPeerSnapshot` query.
- Update N2C part of `latestReleasedNodeVersion` to `NodeToClientV_23`.

### Non-Breaking

- new `GetLedgerPeerSnapshot'` query, for which a backwards-compatible pattern is provided.

- Provide `txWireSize` to TxSubmission protocol

- Small refactoring inside doValidateKESSignature to make it more readable.

- Update code using `EraParams` now that it has a new field `eraPerasRoundLength` for Byron and Shelley eras.

- `EraSummary` is now indirectly Peras-aware via `EraParams`:
  - In a valid summary, Peras round length must divide the epoch size.

- Committing a forker will move the handles to the registry of the LedgerDB. The
  discarded fork will be queued to be released by the `garbageCollect` logic.

- Expose `Ouroboros.Consensus.Storage.LedgerDB.(V1.BackingStore|V2).Backend(Trace)` constructors.

- Allow generating k>2 in ChainDB state machine tests on the fly.
- Improve chances of switching to a shorter chain in ChainDB state machine tests.

- Introduce `Ouroboros.Consensus.Peras.Params` module.
- Introduce `WithArrivalTime` combinator.
- Refactor `HasPerasCertX` field projection typeclasses.
- Add `getLatestCertSeen` method to the PerasCertDB API.

- Add plumbing to provide a SystemTime to the PerasCertDB to record certificate
  arrival times.

- Define Peras votes and their corresponding certificate forging API.

- Add explainable boolean predicate DSL and evaluator.
- Add pure Peras voting rules and mocked up conformance tests.

- Optimize Block Header hash computation and as a result also remove redundant `Crypto` constraint on the `headerHash` function.
- Add `HashAnnotated` instance for `Header`
- Add `MemoHashIndex` type family instance for `HeaderRaw`

- Add Peras vote aggregation logic.
- Add PerasVoteDB API and in-memory implementation.
- Add ObjectDiffusion instances for Peras votes using the PerasVotedDB as backend.

- Add instances for `ReasonForSwitchByTiebreaker` for Praos and TPraos.

- Update to `kes-agent-1.2` and `kes-agent-crypto-1.1`

- The serialisation of `LedgerConfig` has changed in `NodeToClientV_23`.

- Add new `QueryDRepsDelegations` query.

- Add `getPerasCertInBlock` method to `BlockSupportsPeras` typeclass.
- Extend Shelley ledger state to keep track of the latest Peras certificate round number seen on chain.
- Add `LedgerSupportsPeras` typeclass.
- Add instance `LedgerSupportsPeras` for the extended Shelley ledger state.
- Add instance `LedgerSupportsPeras` for `HardForkBlock`.
- Add trivial empty `LedgerSupportsPeras` for other blocks that do not support Peras.
- Add `getLatestPerasCertOnChainRound` method to `ChainDB` API.

- Add `FromCBOR` and `ToCBOR` instances for `OneEraHash`

- Added a complete set of pattern synonyms for `EraIndex (CardanoEras c)` in
  `Ouroboros.Consensus.Cardano.Block` in the `ouroboros-consensus:cardano`
  library.

### Patch

- Added size limits for keep-alive server
- Addes size limits for peer-sharing client & server

- Bump `cardano-ledger-core` dependency to 1.19.

- Bump ledger dependencies:
  - `cardano-ledger-allegra` 1.8 → 1.9
  - `cardano-ledger-alonzo` 1.14 → 1.15
  - `cardano-ledger-api` 1.12 → 1.13
  - `cardano-ledger-conway` 1.20 → 1.21
  - `cardano-ledger-core` 1.18 → 1.19
  - `cardano-ledger-dijkstra` 0.1 → 0.2

- Ensure the initial handle allocated by opening a forker is deallocated in all
  situations.

- Bump to `resource-registry ^>= 0.2`.

- LSM-trees database directory is now created on startup.

- Ensure the `LedgerDbArgs` are garbage collected once we start the LedgerDB.
- Ensure the `ProtocolInfo` is garbage collected once we start Consensus.

- Ensure the `ProtocolInfo` is garbage collected once we start Consensus.

- Fix a race condition between chain switches and LedgerDB.V1 forker acquisition.

- Ensure Mempool always deallocates stale forkers, or rather that it does not
  try to allocate a new one unless completely necessary and closes the old one
  in the process.

- The Mempool sync thread was allocated in the top level registry in order to
  ensure it would be cancelled before the mempool registry was shutting
  down. This was solved in `resource-registry-0.2.0.0`.

- Fix double read lock acquisition when opening a forker in LedgerDB V1.
- Ensure the read lock acquired when opening a forker is released in the presence of exceptions.
- Ensure the forker is tracked in the resource registry from the start and it will only deallocate the read lock once.

- Make forker tracers more informative, with enclosing times.

- Fix leaky read lock acquisition that could lead to whole node deadlock.

- Force snapshot tracer to avoid retaining the Genesis state.

- Emit eventlog markers on snapshot and genesis loading.

- Avoid retention of intermediate closures in LSM-trees LedgerDB handles.
- Keep track of resources to release in InMemory LedgerDB handles.
- Ensure initial LedgerState is not retained when replaying the chain.

- Integrating upstream Breaking changes for the Mempool timeout.

- Adapt to `preferAnchoredCandidate` returning `ShouldSwitch` instead of `Bool`.

- Bump `cardano-protocol-tpraos` to 1.5.

- Downgrade hard mempool timeouts to soft timeouts for local clients.

- Do not delete snapshots if they are permanent even if they are from the
  future.

- Permanent (suffixed) snapshots at genesis will not be deleted but they won't
  be used either.

- Fix leaked handles on uncommitted forkers in V2 LedgerDB.


# Archeological note

Before joining all packages into one, the changelogs were kept in separate files for each package:

- [ouroboros-consensus/CHANGELOG.md](./ouroboros-consensus/CHANGELOG.md)
- [ouroboros-consensus-diffusion/CHANGELOG.md](./ouroboros-consensus-diffusion/CHANGELOG.md)
- [ouroboros-consensus-cardano/CHANGELOG.md](./ouroboros-consensus-cardano/CHANGELOG.md)
- [ouroboros-consensus-protocol/CHANGELOG.md](./ouroboros-consensus-protocol/CHANGELOG.md)
