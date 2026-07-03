<!-- xrefcheck: ignore all -->

# Observing and Debugging Genesis Sync

This document maps the runtime behaviour of Ouroboros Genesis to the trace events a running node emits.
Use it to locate a sync stall against the Genesis components.
For the design of each component, see the [Genesis design doc](./genesis_design.md).

The operator-facing fixes referenced here — peer targets, the HAA gate, host networking, tracer setup — live on the Cardano developer portal, not in this repository.
This document names the cause and links to the portal for the configuration:
the [topology page](https://developers.cardano.org/docs/operators/node/topology) for peer and Genesis configuration, and the [new tracing system quick start](https://developers.cardano.org/docs/operators/monitoring/new-tracing-system/new-tracing-system) for enabling the tracers named below.

The trace namespaces here match mainline cardano-node.
Leaf names can change between node versions, so confirm them against the version you run.

## The Genesis feedback loop

Genesis sync is a feedback loop across several components, not a linear pipeline.
A stall is always somewhere in this loop.

- Each upstream peer runs a **ChainSync** client that downloads headers.
  The **Limit on Patience** (LoP) and **ChainSync Jumping** (CSJ) run inside that client.
- The **Genesis Density Disconnection** (GDD) rule reads the candidate fragments from all peers and disconnects any peer whose chain is not strictly denser than the best near the point where the candidates diverge.
  Equal density is enough to disconnect a peer: the honest chain is expected to be strictly denser, so a peer that only matches is treated as not honest.
  GDD maintains the **Limit on Eagerness** (LoE) anchor.
- The **LoE** pins chain selection to the LoE anchor, so the node does not select past the point where honest peers still disagree.
- **Devoted BlockFetch** (DBF, called *BulkSync BlockFetch* in the code) fetches blocks for the selected candidate; **ChainSel** validates them and the **ChainDB** adopts them.
- The **Genesis State Machine** (GSM) watches ChainDB progress and decides caught-up versus syncing.
  Its state feeds back into each ChainSync client's LoP bucket: LoP only charges a peer while the node is `Syncing`.

For the authoritative component graph and the design of each component, see [Components](./genesis_design.md#components) and [Component-level Designs](./genesis_design.md#component-level-designs) in the design doc.

## GSM states

The GSM (`Ouroboros.Consensus.Node.GsmState`) has three states: `PreSyncing`, `Syncing`, `CaughtUp`.
It runs in both Praos and Genesis mode.
CSJ and GDD are Genesis-only; a Praos-mode node never runs them.

The transition between `PreSyncing` and `Syncing` is governed by the **Honest Availability Assumption** (HAA).
The diffusion layer signals `TrustedStateWithExternalPeers` once the node has at least `MinBigLedgerPeersForTrustedState` (default 5) active big ledger peers.
That signal satisfies the HAA and lets the GSM move `PreSyncing → Syncing`.
Dropping below the threshold withdraws the signal and moves the GSM back.

## Reading a sync stall

Each GSM transition is traced under `Consensus.GSM`: `InitializedInPreSyncing` (or `InitializedInCaughtUp`) at startup, then `PreSyncingToSyncing`, `EnterCaughtUp`, and the reverse `SyncingToPreSyncing` / `LeaveCaughtUp`.
Where a fix is operational — config values, host routing, tracer setup — this section names the cause and points to the developer portal rather than repeating the configuration.

### Stuck in PreSyncing

The HAA is never satisfied.
This is almost always a peer-selection or networking issue, not a Genesis bug.

**Signature**: one `Consensus.GSM.InitializedInPreSyncing` at startup and no `PreSyncingToSyncing`.
Repeated `Net.Peers.Ledger.NotEnoughBigLedgerPeers`.
Repeated `Net.PeerSelection.Selection.PromoteColdBigLedgerPeerFailed` and `Net.ConnectionManager.Remote.ConnectError`.

**Causes**:

- **Too few reachable big ledger peers.** The network has fewer than `MinBigLedgerPeersForTrustedState` active big ledger peers, so `TrustedStateWithExternalPeers` is never signalled. This is common on small networks. See the portal's [topology page](https://developers.cardano.org/docs/operators/node/topology) for the peer targets and the HAA gate.
- **IPv6 route unreachable.** The node dials literal IPv6 addresses from the peer snapshot regardless of `--host-ipv6-addr`. That option only sets the local bind address and the DNS lookup family; it does not gate outbound dials. On a host with no IPv6 route the dial fails with `Network is unreachable`, shown as `ConnectError` against IPv6 addresses while all successful handshakes are IPv4. Fix the host routing or drop the IPv6 peers from the snapshot.
- **Peer snapshot exhaustion.** The snapshot lists no reachable peers.

### Stuck in Syncing (no EnterCaughtUp)

The HAA is satisfied (`PreSyncingToSyncing` seen) but sync does not complete.
The stall is downstream of peer selection: in header delivery, the density rule, or block fetch.
Work through the cases below in order; each names the trace event that confirms or rules it out.

- **LoE not advancing.** Chain selection is pinned at the LoE anchor. Watch `Consensus.GDD.TraceGDDEvent` (its payload `kind` field is `TraceGDDDebugInfo` for the density comparison and the disconnect decision otherwise). GDD advances the anchor only after it disconnects the peers offering lower-density chains. If the ChainDB tip stalls while headers keep arriving, GDD has not been able to move the anchor — usually because the competing chains have not yet diverged enough in density (small peer set, or too few headers past the intersection), not because GDD is malfunctioning.
- **CSJ dynamo wedged.** No `Consensus.CSJ.SentJumpInstruction` for a long time, and jumpers stuck at `Consensus.CSJ.BlockedOnJump`.
- **BlockFetch starvation.** Frequent `BlockFetch.Decision.PeerStarvedUs` (the Genesis bulk-sync path). The node rotates peers but none keeps up.
- **LoP disconnections.** ChainSync clients drop slow peers via the Limit on Patience. Slow peers, not a Genesis defect.

### Bouncing between PreSyncing and Syncing

The HAA is flapping.
The count of active big ledger peers keeps crossing `MinBigLedgerPeersForTrustedState`, so the trusted-state signal toggles.
You see `Consensus.GSM.PreSyncingToSyncing` and `Consensus.GSM.SyncingToPreSyncing` alternating.
The toggling is a symptom, not the cause: something keeps disconnecting the big ledger peers.
For each `SyncingToPreSyncing`, find why a peer dropped just before it.
The node may have disconnected it — GDD found its chain not dense enough, or the Limit on Patience timed it out for being too slow, both surfacing as `ChainSync.Client.Exception` (severity Warning, visible by default).
The connection may instead have failed at the network layer, shown by `Net.ConnectionManager.Remote.ConnectError` with the reason.

### Healthy signature

`Consensus.GSM.InitializedInPreSyncing` → `PreSyncingToSyncing` → eventually `EnterCaughtUp`.
`Consensus.CSJ.InitializedAsDynamo` once a peer registers, then steady `Consensus.CSJ.SentJumpInstruction`.
Periodic `Consensus.GDD.TraceGDDEvent`.
`BlockFetch.Decision.PeersFetch` with one peer fetching.
The ChainDB tip extending.

## Enabling the tracers

The events above come from cardano-node's tracing system.
Several are below the default severity and only appear once their namespace is raised to `Debug`.
The node-side configuration for that lives on the developer portal: see the [new tracing system quick start](https://developers.cardano.org/docs/operators/monitoring/new-tracing-system/new-tracing-system).
