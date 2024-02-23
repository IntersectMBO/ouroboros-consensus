# Ouroboros-consensus-diffusion Changelog

# Changelog entries

<a id='changelog-0.11.0.0'></a>
## 0.11.0.0 — 2024-02-23

### Non-Breaking

- Added `getImmTipSlot` to `NodeKernel` exports.

- Integrate with network-packages and io-sim 1.4.1 packages
- Bump dependencies version bounds

### Breaking

- The field `srnSnapshotInterval :: SnapshotInterval` of `StdRunNodeArgs` is replaced by `srnDiskPolicyArgs :: DiskPolicyArgs`. `DiskPolicyArgs` is a product of `SnapshotInterval` and `NumOfDiskSnapshots`. To maintain current behavior the default value `DefaultNumOfDiskSnapshots` can be provided for the latter.

- Added the Genesis State Machine (GSM), though for now it is merely the
  simpler [Bootstrap Peers State
  Machine](https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/BootstrapPeersIER).

- Added `rnGetUseBootstrapPeers` to `RunNodeArgs`, for dynamically
  enabling/disabling the GSM. The proper GSM must always be running, despite
  the TVar it owns being ignored when it's disabled, since it may be enabled at
  any time.

- Added `llrnMaxCaughtUpAge` to the low-level args; defaults to 20min.

- Added `gsmTracer` to the node's tracers.

- Added `getNodeIdlers` to the `NodeKernel` interface; tracking peers that have
  last sent `MsgAwaitReply`.

<a id='changelog-0.10.0.0'></a>
## 0.10.0.0 — 2024-01-29

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
## 0.9.0.0 — 2023-11-14

### Breaking

- Adds `NodeToNodeVersion` to the arguments of `defaultCodecs` and `runWith` functions.

- Adds `srnChainSyncTimeout` argument to `StdRunNodeArgs`.

<a id='changelog-0.8.0.2'></a>
## 0.8.0.2 — 2023-10-26

### Patch

- In tests only: replace all occurrences of `WrapTickedLedgerView` and `TickedTrivial` with `WrapLedgerView` and `()`.

<a id='changelog-0.8.0.1'></a>
## 0.8.0.1 — 2023-09-27

### Patch

- Update bound on `ouroboros-consensus` to account for changes in unstable test
  libraries.

<a id='changelog-0.8.0.0'></a>
## 0.8.0.0 — 2023-09-06

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
## 0.7.1.1 — 2023-08-21

### Patch

- Removed the `expose-sublibs` cabal flag, since Cabal/Nix handled it poorly.
- Instead, added a `unstable-` prefix to the name of each sublibrary, to
  strongly indicate that we ignore them when evolving the package's version.

<a id='changelog-0.7.1.0'></a>
## 0.7.1.0 — 2023-08-18

### Patch

- Update `fs-api` dependency to `^>=0.2`

### Non-Breaking

- Update to `ouroboros-network-framework` 0.7.0.0

<a id='changelog-0.7.0.0'></a>
## 0.7.0.0 — 2023-07-06

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
## 0.6.1.0 — 2023-06-23

### Patch

- Add lower bound on graphviz

- Allow ouroboros-network-0.8.0.0

- Require `fs-sim >= 0.2` in test libraries.

### Non-Breaking

- Update a comment in Ouroboros.Consensus.NodeKernel.forkBlockForging

<a id='changelog-0.6.0.0'></a>
## 0.6.0.0 — 2023-05-19

### Non-Breaking

* Increase the minimum reconnection delay from 0s to 10s.

### Breaking

* `consensusStartupErrorTracer` field of `Tracers'` data type was renamed to `consensusErrorTracer`.
* `chainSyncServerHeaderTracer` field of `Tracers` was changed to use `TraceLabelPeer`.
* `blockFetchServerTracer` field of `Tracers` was changed to use `TraceLabelPeer`.

<a id='changelog-0.5.1.0'></a>
## 0.5.1.0 — 2023-04-28

### Non-Breaking

- Update `io-sim` dependency to 1.1.0.0.

- Update `ouroboros-network` dependency.

<a id='changelog-0.5.0.0'></a>
## 0.5.0.0 - 2023-04-24

### Breaking

- Apply new organization of Consensus packages. Absorb the ThreadNet tests and
  the ledger-agnostic test-suites that use them.

<a id='changelog-0.4.0.0'></a>
## 0.4.0.0 - 2023-04-21

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
