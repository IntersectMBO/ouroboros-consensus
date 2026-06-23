# Ouroboros-consensus Changelog

The changelog is organized by the main four libraries: Core, Protocol, Diffusion
and Cardano. Each library has their own Patch, Non-Breaking and Breaking
sections.

# Changelog entries

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
