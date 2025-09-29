# Ouroboros-consensus-diffusion Changelog

# Changelog entries

<a id='changelog-0.24.0.0'></a>
## 0.24.0.0 -- 2025-09-29

### Non-Breaking

- Ensure that block forging threads finalize their keys when shutting down.
- Adds a `kesAgentClientTracer` for tracing `KESAgentClientTrace` events

### Breaking

- `Ouroboros.Consensus.Network.NodeToNode.mkApps` takes `StdGen` as an argument, which is passed to `chain-sync` client.
- `LowLevelRunNodeArgs` and `StdRunNodeArgs` were changed to match `ouroboros-network-0.22`
- `NetworkP2PMode` was removed (non-p2p mode is removed from `ouroboros-network`).
- `Ouroboros.Consensus.Node.stdRunDataDiffusion` was changed to match `Cardano.Network.Diffusion` API.

- Added tracers `GsmEventInitializedInCaughtUp` and `GsmEventInitializedInPreSyncing` for the initial Genesis State Machine state.

<a id='changelog-0.23.0.0'></a>
## 0.23.0.0 -- 2025-05-15

### Patch

- Bump `ouroboros-network` packages dependencies.

### Non-Breaking

- Drop GHC 8.10 support.

### Breaking

- Use `HeaderWithTime` for the ChainSync candidates

<a id='changelog-0.22.0.1'></a>
## 0.22.0.1 -- 2025-04-21

### Patch

- Update tests to use `SerializeTablesWithHint`.

<a id='changelog-0.22.0.0'></a>
## 0.22.0.0 -- 2025-04-16

### Breaking

- Implement the UTxO-HD feature. See the documentation in [the
  webpage](https://ouroboros-consensus.cardano.intersectmbo.org/docs/references/miscellaneous/utxo-hd/).
  - `hStateQueryServer` now needs a `ResourceRegistry` to allocate `Forker`s.
  - `DiskPolicyArgs` was transformed into `SnapshotPolicyArgs`.
  - `StdRunNodeArgs` got two new fields:
    - `srnQueryBatchSize` to specify how many entries read on each batch when
      answering queries.
    - `srnLdbFlavorArgs` to select the LedgerDB backend.
  - The forging loop uses a `ReadOnlyForker` to get the ledger state of the
    block on top of which it should forge and the mempool snapshot.

<a id='changelog-0.21.0.1'></a>
## 0.21.0.1 -- 2025-04-03

### Patch

- Tighten lower bounds on `ouroboros-consensus` for the sake of
  testlibs.

<a id='changelog-0.21.0.0'></a>
## 0.21.0.0 -- 2025-03-25

### Breaking

- Added peer sharing tracer to NTN tracers

- Update to latest `ouroboros-network` release:
    | Package                     | Version |
    |-----------------------------|---------|
    | network-mux                 | 0.7     |
    | ouroboros-network           | 0.20.1  |
    | ouroboros-network-api       | 0.13    |
    | ouroboros-network-framework | 0.17    |
    | ouroboros-network-protocols | 0.14    |

- Added a new CSJ tracer to ChainSync client interface.
- Renamed the existing tracer in `Ouroboros.Consensus.Node.Tracers.Tracers`.

### Non-Breaking

- Bump upper bound on `base` dependency.

- Expose blockchain time as `getBlockchainTime :: BlockchainTime m` in the `NodeKernel`.

### Patch

- Use `OmitLedgerEvents` when ticking blocks in the forging loop

- Label the DbLock acquisition thread.

<a id='changelog-0.20.0.0'></a>
## 0.20.0.0 -- 2025-02-10

### Breaking

- Updated to `ouroboros-network-0.19.0.2` & `ouroboros-network-framework-0.16`.
- `runWith` and `LowLevelRunNodeArgs` are no longer polymorphic in version
   data.
- `NodeToNode.initiator`, `NodeToNode.initiatorAndResponder` take negotiated
  `NodeToNodeVersionData` as an argument instead of `PeerSharing` (config
   option).
- `NodeToClient.responder` take negotiated `NodeToClientVersionData` as an
   argument.

<a id='changelog-0.19.0.0'></a>
## 0.19.0.0 -- 2025-01-08

### Patch

- Remove references to `Ouroboros.Consensus.Fragment.InFuture`.

- Depend on `network-mux` from `ouroboros-network` and use its types.

* Use [`resource-registry`](https://hackage.haskell.org/package/resource-registry).

### Breaking

- Adapted to Genesis-related changes in `ouroboros-consensus` ([#1179](https://github.com/IntersectMBO/ouroboros-consensus/pull/1179)).

- Remove `CheckInFuture m blk` argument from `openChainDB`.

- Updated to `typed-protocols-0.3.0.0`
- Added `KeepAlive` tracer to `Tracers'` data type.

<a id='changelog-0.18.0.0'></a>
## 0.18.0.0 -- 2024-10-14

### Patch

- Updates for the `TxLimits` mempool consolidation.

### Breaking

- Adapted to ChainSync client changes due to new message historicity check.

- `getDiffusionPipeliningSupport` :: `DiffusionPipeliningSupport` was added to `NodeKernelArgs`
  and `NodeKernel` to enable gracefully handling some types of protocol errors when diffusion
  is ran with pipelining enabled.

<a id='changelog-0.17.1.0'></a>
## 0.17.1.0 -- 2024-08-26

### Non-Breaking

- Propagate `ouroboros-network` types `NumTxIdsToAck` and `SizeInBytes`
- Adds a Tracer for startup sanity check warnings in Ouroboros.Consensus.Node.Tracers (see BlockSupportsSanityCheck in ouroboros-consensus)
- Emit `ChainDB.TraceEvent(TraceLastShutdownUnclean)` when the `clean` marker is
  missing and the node will therefore revalidate all the data.

### Breaking

- Integrated all Genesis components into the NodeKernel. In particular,
  `RunNodeArgs` now has a new field

  ```haskell
  rnGenesisConfig :: GenesisConfig
  ```

  This can be set to `Ouroboros.Consensus.Node.Genesis.disableGenesisConfig` to
  keep the Praos behavior, or to `enableGenesisConfigDefault` to enable Genesis
  with preliminary parameter choices.
- The `StdRunNodeArgs(srnDatabasePath)` argument becomes of type `NodeDatabasePaths`
  which will allow storing the immutable db (which doesn't need to be in a very
  performant device) somewhere different than the volatile data.

<a id='changelog-0.17.0.1'></a>
## 0.17.0.1 -- 2024-06-26

### Patch

- Add trivial `txRefScriptSize` definitions

<a id='changelog-0.17.0.0'></a>
## 0.17.0.0 -- 2024-06-19

### Patch

- Updated dependencies; no local changes required.

### Breaking

- Implemented a first version of CSJ (ChainSync Jumping). (disabled by default)

<a id='changelog-0.16.0.0'></a>
## 0.16.0.0 -- 2024-05-13

### Patch

- Internal changes in tests.

### Non-Breaking

- Implemented the Honest Availability Assumption properly (both for
  Praos/"Genesis Lite" and Genesis) based on newly exposed state by the
  diffusion layer.

- Upgraded to `ouroboros-network-0.16`

### Breaking

- Accounted for a refactoring of the ChainSync client parameters.

- `ChainDbArgs` re-exported by `Ouroboros.Consensus.Node` had breaking changes upstream. See `ouroboros-consensus`' changelog for details.
- Removed `mkChainDbArgs`.
- New `llrnMkHasFS` field in `LowLevelRunNodeArgs`

- Removed `llrnRunDataDiffusion`'s unused `ResourceRegistry` argument.

<a id='changelog-0.15.0.0'></a>
## 0.15.0.0 -- 2024-04-03

NOTE: version jumps from `0.13.0.0` to `0.15.0.0` because `0.14.0.0` was created in a [branch](https://github.com/IntersectMBO/ouroboros-consensus/pull/1042) containing backported fixes for Node 8.9.

### Breaking

- Updated `ouroboros-consensus-diffusion` to use `ouroboros-network-0.14.0.0`.
  `LowLevelRounNodeArgs` and `NodeKernel` records hold
  `PublicPeerSelectionState` variable.

<a id='changelog-0.13.0.0'></a>
## 0.13.0.0 -- 2024-04-03

NOTE: version jumps from `0.11.0.0` to `0.13.0.0` because `0.12.0.0` was created in a [branch](https://github.com/IntersectMBO/ouroboros-consensus/pull/997) containing backported fixes for Node 8.9.

### Patch

- Bugfix: DiskPolicyArgs were not being passed down to the ChainDB which resulted in default values for SnapshotInterval.

- Start using `safe-wild-cards` internally.

- LoP: run the ChainSync client against a leaky bucket.

### Breaking

- Integrate changes for lightweight checkpointing [#449](https://github.com/IntersectMBO/ouroboros-consensus/issues/449),
  which required adding a field to `TopLevelConfig`.

- Added `PeerSharingAPI` to `NodeKernel`

- Added `peerSharingRng` to `NodeKernelArgs`

- Refactored some diffusion functions to remove `computePeers` callback

### Non-Breaking

- Update network packages
  - `ouroboros-network`: `^>=0.13`
  - `ouroboros-network-framework`: `^>=0.12`

<a id='changelog-0.11.0.0'></a>
## 0.11.0.0 -- 2024-02-23

### Non-Breaking

- Added `getImmTipSlot` to `NodeKernel` exports.

- Integrate with network-packages and io-sim 1.4.1 packages
- Bump dependencies version bounds

### Breaking

- The field `srnSnapshotInterval :: SnapshotInterval` of `StdRunNodeArgs` is replaced by `srnDiskPolicyArgs :: DiskPolicyArgs`. `DiskPolicyArgs` is a product of `SnapshotInterval` and `NumOfDiskSnapshots`. To maintain current behavior the default value `DefaultNumOfDiskSnapshots` can be provided for the latter.

- Added the Genesis State Machine (GSM), though for now it is merely the
  simpler [Bootstrap Peers State
  Machine](https://ouroboros-consensus.cardano.intersectmbo.org/docs/references/miscellaneous/bootstrap_peers_IER/).

- Added `rnGetUseBootstrapPeers` to `RunNodeArgs`, for dynamically
  enabling/disabling the GSM. The proper GSM must always be running, despite
  the TVar it owns being ignored when it's disabled, since it may be enabled at
  any time.

- Added `llrnMaxCaughtUpAge` to the low-level args; defaults to 20min.

- Added `gsmTracer` to the node's tracers.

- Added `getNodeIdlers` to the `NodeKernel` interface; tracking peers that have
  last sent `MsgAwaitReply`.

<a id='changelog-0.10.0.0'></a>
## 0.10.0.0 -- 2024-01-29

### Patch

- Fix imports and type mismatches caused by `ouroboros-consensus` bumping
  `strict-checked-vars` to `^>= 0.2`.

- Removed `EraNodeToNodeVersion`, replacing it with `WrapNodeToNodeVersion`.

- Updated to GHC 9.8.1.
- Updated `ouroboros-network` dependencies:
  - `ouroboros-network` 0.10.2 -> 0.11
  - `ouroboros-network-frameworks` 0.10 -> 0.11
  - `ouroboros-network-protocols` 0.6.1 -> 0.7

### Non-Breaking

- Update network packages
  - `ouroboros-network`: `^>=0.10.2`
  - `ouroboros-network-api`: `^>=0.6.2`
  - `ouroboros-network-protocols`: `^>=0.6.1`

### Breaking

- Integrate the new `InFutureCheck` in the ChainSync client, which requires new
  fields in `NodeKernalArgs`.

- The upstream ChainSync client refactoring required changes to NodeKernelArgs
  and Handlers.

<a id='changelog-0.9.0.0'></a>
## 0.9.0.0 -- 2023-11-14

### Breaking

- Adds `NodeToNodeVersion` to the arguments of `defaultCodecs` and `runWith` functions.

- Adds `srnChainSyncTimeout` argument to `StdRunNodeArgs`.

<a id='changelog-0.8.0.2'></a>
## 0.8.0.2 -- 2023-10-26

### Patch

- In tests only: replace all occurrences of `WrapTickedLedgerView` and `TickedTrivial` with `WrapLedgerView` and `()`.

<a id='changelog-0.8.0.1'></a>
## 0.8.0.1 -- 2023-09-27

### Patch

- Update bound on `ouroboros-consensus` to account for changes in unstable test
  libraries.

<a id='changelog-0.8.0.0'></a>
## 0.8.0.0 -- 2023-09-06

### Patch

- Use `io-classes-1.2`.

### Breaking

- Use `ouroboros-network-0.9.0.0`. Types of some of functions changed:
    * `Ouroboros.Consensus.Network.NodeToClient.responder`
    * `Ouroboros.Consensus.Network.NodeToNode.Handlers`:
      - `hChainSynClient` accepts `IsBigLedgerPeer` argument;
      - `hPeerSharingClient` and `hPeerSharingServer` use `ConnectionId addr`
        instead of `addr`.
    * `Ouroboros.Consensus.Network.NodeToNode.{Client,Server}App`: receive
      network context which contains things like address, whether the peer is
      a big ledger peer, etc.  These changes propagate to the `Apps` type
      within the same module.
    * `Ouroboros.Consensus.Node.runWith` requires additional constraints, see
      `NetworkIO` and `NetworkAddr` type aliases within the module.

<a id='changelog-0.7.1.1'></a>
## 0.7.1.1 -- 2023-08-21

### Patch

- Removed the `expose-sublibs` cabal flag, since Cabal/Nix handled it poorly.
- Instead, added a `unstable-` prefix to the name of each sublibrary, to
  strongly indicate that we ignore them when evolving the package's version.

<a id='changelog-0.7.1.0'></a>
## 0.7.1.0 -- 2023-08-18

### Patch

- Update `fs-api` dependency to `^>=0.2`

### Non-Breaking

- Update to `ouroboros-network-framework` 0.7.0.0

<a id='changelog-0.7.0.0'></a>
## 0.7.0.0 -- 2023-07-06

### Non-Breaking

- Change how diffusion initializes block forging credentials: now there's a `TMVar` that
  monitors so we can enable/disable block forging dynamically.
  - There's also a `blockForgingController` that keeps an eye at this `TMVar`.

- Adds new trace `TraceAdoptionThreadDied SlotNo blk` to `TraceForgeEvent`

### Breaking

- Add a new argument to `RekeyM` type, due to the extraction of the block forging
  credentials over at `ouroboros-consensus`.
  - Refactor to accommodate this change

- Add block forging credentials to `TestNodeInitialization`

- Change the type signature of most functions to receive the block forging credentials as
  an argument, since it now doesn't come in the usual bundle.

- Add `setBlockForging` to `NodeKernel` which must be used to set / control
  block forging of the consensus layer.

<a id='changelog-0.6.1.0'></a>
## 0.6.1.0 -- 2023-06-23

### Patch

- Add lower bound on graphviz

- Allow ouroboros-network-0.8.0.0

- Require `fs-sim >= 0.2` in test libraries.

### Non-Breaking

- Update a comment in Ouroboros.Consensus.NodeKernel.forkBlockForging

<a id='changelog-0.6.0.0'></a>
## 0.6.0.0 -- 2023-05-19

### Non-Breaking

* Increase the minimum reconnection delay from 0s to 10s.

### Breaking

* `consensusStartupErrorTracer` field of `Tracers'` data type was renamed to `consensusErrorTracer`.
* `chainSyncServerHeaderTracer` field of `Tracers` was changed to use `TraceLabelPeer`.
* `blockFetchServerTracer` field of `Tracers` was changed to use `TraceLabelPeer`.

<a id='changelog-0.5.1.0'></a>
## 0.5.1.0 -- 2023-04-28

### Non-Breaking

- Update `io-sim` dependency to 1.1.0.0.

- Update `ouroboros-network` dependency.

<a id='changelog-0.5.0.0'></a>
## 0.5.0.0 -- 2023-04-24

### Breaking

- Apply new organization of Consensus packages. Absorb the ThreadNet tests and
  the ledger-agnostic test-suites that use them.

<a id='changelog-0.4.0.0'></a>
## 0.4.0.0 -- 2023-04-21

### Breaking

- Peer Sharing Integration:
  - Monomorphized `Handlers` client field types to `ConnectionId addr`;
    and added PeerSharing handlers.
  - Changed `mkHandlers` function to receive a function to compute peer sharing addresses;
  - Changed `Codecs` type and propagated changes to relevant functions (e.g. `defaultCodecs`);
  - Changed `Apps` type and propagated changes to relevant functions (e.g. `mkApps`);
  - `initiatorAndResponder` receives PeerSharing value;
  - Added PeerSharing field to `RunNodeArgs`;
  - Changed `runWith` to receive necessary parameters;
  - `NodeKernel` changes to incorporate PeerSharing miniprotocol (adds `PeerSharingRegistry`);

- Extract `ouroboros-consensus-diffusion` from the bundle of packages.

### Non-Breaking

- Renamed address type variables to more consistent naming

- Update chainsync timeout: Increase the minimum timeout from 90s to 135s and
  switch from picking from an array of 5 values to a range of timeouts. This
  change reduces the risk of synchronisation among nodes in the network.

### Patch

- `ouroboros-consensus-diffusion`: `ouroboros-network` packages version bumps.

## Before 0.4.0.0

Before this version, `ouroboros-consensus-diffusion` lived in a bundle of
packages with `ouroboros-consensus`, thus the changelog was the same.

---

### Archaeological remark

Before following a more structured release process, we tracked most significant
changes affecting downstream users in the
[interface-CHANGELOG.md](https://github.com/IntersectMBO/ouroboros-consensus/blob/8d8329e4dd41404439b7cd30629fcce427679212/docs/website/docs/interface-CHANGELOG.md).
