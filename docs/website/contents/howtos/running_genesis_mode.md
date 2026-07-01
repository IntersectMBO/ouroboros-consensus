# Running Genesis Mode

## When to use Genesis mode

Genesis mode lets a syncing node defend against long-range attacks without trusting recent ledger state for peer evaluation.
It adds defensive components — LoE, GDD, CSJ, LoP, and Devoted BlockFetch (DBF) — on top of existing ChainSync/BlockFetch, coordinated by the Genesis State Machine (GSM).
The GSM itself runs in both modes: it tracks whether the node is caught up and disables the Genesis-only components once it is.
See the [Genesis design doc](../references/miscellaneous/genesis_design.md) for the full component description.

Use Genesis mode on **relay nodes and syncing nodes** that connect to the public network.

Use Praos mode on **block-producing nodes** that connect only to trusted local relays.
Setting `useLedgerAfterSlot: -1` disables ledger peers, so the node uses only its configured roots (`-1` turns ledger peers off; `0` would mean "always use the ledger").
A forger with `GenesisMode` + trusted `localRoots` + ledger peers disabled will behave like Praos mode in practice — the peer-selection target basis differs only in the `(GenesisMode, TooOld)` state — but there is no benefit over Praos mode in that configuration.

## Required configuration

Genesis mode is turned on by one `config.json` field:

```json
"ConsensusMode": "GenesisMode"
```

`ConsensusMode` activates the Genesis defenses.
The GSM runs in both modes, so a Praos-mode node still emits GSM events.
CSJ and GDD are Genesis-only defenses: a Praos-mode node never writes to those tracers.

Two other `config.json` fields only take effect in Genesis mode: `MinBigLedgerPeersForTrustedState` (see [Small testnets](#small-testnets)) and `LowLevelGenesisOptions`.

The topology may also list a big-ledger-peer snapshot via the optional `peerSnapshotFile` field (a file path).
It seeds ledger peer selection with an initial set of big ledger peers.
It is not required: in Genesis mode with ledger peers enabled, a node without one only logs a recommendation to supply it.

## Host networking

`--host-addr` and `--host-ipv6-addr` are CLI options, not `config.json` fields:

```sh
cardano-node run --host-addr 0.0.0.0 --host-ipv6-addr ::
```

They set the local addresses the node binds its listening sockets to.
`--host-ipv6-addr` also selects the DNS lookup family: without it the node resolves only A records, so peers given as domain names never yield IPv6 addresses.

Neither option gates outbound connections to literal IPv6 addresses.
If the peer snapshot lists a peer by its IPv6 address, the node dials it whether or not `--host-ipv6-addr` is set.
On a host with no IPv6 route, that dial fails with `Network is unreachable`.

**Diagnostic**: if logs show repeated `Net.ConnectionManager.Remote.ConnectError` against IPv6 addresses (e.g. `[2a05:...]:3001`) while all `HandshakeSuccess` entries are IPv4, the host has no IPv6 route to those peers.
To stop the failures, give the host an IPv6 route or remove the IPv6 peers from the snapshot.

## Small testnets

Genesis-mode peer selection defaults are mainnet-scale.
On a small testnet (e.g. 3 relays), these defaults are unsatisfiable and the node will stall in `PreSyncing` forever.

The critical parameter is `MinBigLedgerPeersForTrustedState` (default: **5**).
This is the minimum number of active big ledger peers required for the diffusion layer to signal `TrustedStateWithExternalPeers` to consensus, which is the gate that allows the GSM to transition from `PreSyncing` to `Syncing`.
If the network has fewer than 5 reachable big ledger peers, this field must be lowered.

Lowering `SyncTargetNumberOfActiveBigLedgerPeers` alone is **not sufficient** — the `MinBigLedgerPeersForTrustedState` gate must also be lowered.

Minimal config for a 3-relay testnet (top-level fields, not inside `TraceOptions`):

```json
"MinBigLedgerPeersForTrustedState":            1,
"SyncTargetNumberOfRootPeers":                 0,
"SyncTargetNumberOfKnownPeers":                1,
"SyncTargetNumberOfEstablishedPeers":           1,
"SyncTargetNumberOfActivePeers":                1,
"SyncTargetNumberOfKnownBigLedgerPeers":        1,
"SyncTargetNumberOfEstablishedBigLedgerPeers":  1,
"SyncTargetNumberOfActiveBigLedgerPeers":       1
```

The peer selection targets must satisfy `active <= established <= known` for both regular and big ledger peers.
For production small testnets, raise these to match the actual peer pool size.

| Parameter | Default | Config key |
|-----------|---------|------------|
| Min big ledger peers for HAA | 5 | `MinBigLedgerPeersForTrustedState` |
| Active big ledger peers | 30 | `SyncTargetNumberOfActiveBigLedgerPeers` |
| Established big ledger peers | 40 | `SyncTargetNumberOfEstablishedBigLedgerPeers` |
| Known big ledger peers | 100 | `SyncTargetNumberOfKnownBigLedgerPeers` |
| Active peers | 5 | `SyncTargetNumberOfActivePeers` |
| Established peers | 10 | `SyncTargetNumberOfEstablishedPeers` |
| Known peers | 150 | `SyncTargetNumberOfKnownPeers` |
| Root peers | 0 | `SyncTargetNumberOfRootPeers` |

## Debugging a sync stall

A Genesis sync stall is always somewhere in the feedback loop:

```
ChainSync (per peer) → LoP → CSJ → GDD → LoE → ChainSel → DBF → ChainDB → GSM
```

This is a linearization: LoP and CSJ run inside ChainSync, and the GSM is not a terminal sink — its state feeds back into ChainSync's LoP bucket, closing the loop.

The GSM has three states: `PreSyncing`, `Syncing`, `CaughtUp`.

### Stuck in PreSyncing

The HAA is never satisfied.
This is almost always a peer-selection or networking issue, not a Genesis bug.

**Signature**: exactly one `Consensus.GSM.InitializedInPreSyncing` at startup, no further GSM events.
Repeated `Net.Peers.Ledger.NotEnoughBigLedgerPeers`.
Repeated `PromoteColdBigLedgerPeerFailed` and `ConnectError`.

**Causes**: IPv6 unreachable (see [Host networking](#host-networking)), target/pool mismatch on small testnets (see [Small testnets](#small-testnets)), or peer snapshot exhaustion.

### Stuck in Syncing (no EnterCaughtUp)

The HAA is satisfied but sync doesn't complete.
Look downstream:

- **LoE pinned**: `Consensus.GDD.TraceGDDEvent` shows the LoE candidate not advancing.
  ChainDB tip stops growing despite headers flowing.
- **GDD impotent**: `Consensus.GDD.TraceGDDEvent` with `"kind": "TraceGDDDebugInfo"` shows multiple peers at equal density.
  GDD is correctly refusing to act.
  Cause is upstream: small peer set, era/window math, checkpoint config.
- **CSJ dynamo wedged**: no `Consensus.CSJ.SentJumpInstruction` for a long time.
  Jumpers logging `BlockedOnJump`.
- **BlockFetch starvation**: frequent `BlockFetch.Decision.PeerStarvedUs`.
  Rotating peers but none keeps up.
- **LoP disconnections**: ChainSync drops with LoP-related `Exception`.
  Slow peers, not a Genesis defect.

### Bouncing Syncing ↔ PreSyncing

HAA flapping.
Diffusion gaining/losing trusted peers.
Debug from first principles — this case is not analyzed in the spec.

### Healthy log signature

`Consensus.GSM.InitializedInPreSyncing` → `PreSyncingToSyncing` → eventually `EnterCaughtUp`.
Steady `Consensus.CSJ.SentJumpInstruction`.
Periodic `Consensus.GDD.TraceGDDEvent`.
`BlockFetch.Decision.PeersFetch` with one peer fetching.
ChainDB tip extending.

## Tracer configuration

The overrides below use cardano-node's new tracing system, which is always on — no separate flag enables it.
(Older nodes gated it behind `UseTraceDispatcher`/`TurnOnLogging`, both generic and defaulting to on; current cardano-node has removed that switch.)

The trace namespaces below match mainline cardano-node; leaf names can change between node versions, so confirm them against the version you run.

At the default root severity (Notice), GSM events and ChainSync.Client `Exception` (severity Warning) are visible.
`BlockFetch.Decision.PeerStarvedUs` is severity Info and is silenced by the default config, so it only appears once you raise `BlockFetch.Decision` to Info or Debug (below).
To debug a stall, add the following to the `TraceOptions` object in `config.json`:

```json
"Consensus.GSM":               { "severity": "Debug" },
"Consensus.CSJ":               { "severity": "Debug" },
"Consensus.GDD":               { "severity": "Debug" },
"Consensus.DevotedBlockFetch": { "severity": "Debug" },
"BlockFetch.Decision":         { "severity": "Debug", "detail": "DMaximum" },
"ChainSync.Client":            { "severity": "Debug" },
"Net.PeerSelection":           { "severity": "Debug" },
"Net.ConnectionManager":       { "severity": "Debug" }
```

`detail: DMaximum` on `BlockFetch.Decision` gives the most verbose per-peer output.
Per-peer decline reasons in `PeersFetch` already appear at the default detail level (any level above `DMinimal`), so `DMaximum` is not strictly required.

### Volume control

Debug-level Genesis tracers are noisy.
Add rate limits and silence high-volume low-signal namespaces:

```json
"BlockFetch.Decision.PeersFetch":                        { "maxFrequency": 1.0 },
"ChainSync.Client.GaveLoPToken":                         { "maxFrequency": 5.0 },
"ChainSync.Client.DownloadedHeader":                     { "maxFrequency": 1.0 },
"ChainSync.Client.ValidatedHeader":                      { "maxFrequency": 1.0 },
"ChainDB.AddBlockEvent.TrySwitchToAFork":                { "severity": "Silence" },
"StateQueryServer":                                      { "severity": "Silence" },
"Net.Mux.Local":                                         { "severity": "Silence" },
"Net.ConnectionManager.Remote.ConnectionManagerCounters": { "severity": "Silence" },
"ChainSync.Client.JumpingWaitingForNextInstruction":      { "severity": "Silence" },
"ChainSync.Client.JumpingInstructionIs":                  { "severity": "Silence" }
```

### Verification

After restart, confirm the overrides took effect:

```sh
grep -E 'Consensus\.(GSM|CSJ|GDD|DevotedBlockFetch)|BlockFetch\.Decision\.(PeersFetch|PeerStarvedUs)' <logfile>
```

Within seconds of startup, expect `Consensus.GSM.InitializedInPreSyncing`.
Once a peer registers, expect `Consensus.CSJ.InitializedAsDynamo` — CSJ runs only in Genesis mode, and this event is severity Debug, so it needs the `Consensus.CSJ` Debug override above.
