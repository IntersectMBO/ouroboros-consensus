# Running Genesis Mode

## When to use Genesis mode

Genesis mode lets a syncing node defend against long-range attacks without trusting recent ledger state for peer evaluation.
It adds defensive components (GSM, LoE, GDD, CSJ, LoP, DBF) on top of existing ChainSync/BlockFetch.
See the [Genesis design doc](../references/miscellaneous/genesis_design.md) for the full component description.

Use Genesis mode on **relay nodes and syncing nodes** that connect to the public network.

Use Praos mode on **block-producing nodes** that connect only to trusted local relays with `useLedgerAfterSlot: -1`.
A forger with `GenesisMode` + trusted `localRoots` + `useLedgerAfterSlot: -1` will behave like Praos mode in practice (the peer selection targets only differ when `(GenesisMode, TooOld)`), but there is no benefit over Praos mode in that configuration.

## Required configuration

The following `config.json` fields are prerequisites for Genesis mode:

```json
"ConsensusMode": "GenesisMode",
"UseTraceDispatcher": true,
"TurnOnLogging": true
```

`ConsensusMode` activates the Genesis defenses.
Praos-mode nodes do not emit GSM, CSJ, or GDD events at all — the tracers exist but are never written to.

The node also requires a peer snapshot file (`peer-snapshot.json`) listed in the topology, which provides the initial set of peers for ledger peer selection.

## Host networking

The node binds listening sockets and creates outbound connections based on `HostIPv4Addr` and `HostIPv6Addr` in `config.json`:

```json
"HostIPv4Addr": "0.0.0.0",
"HostIPv6Addr": "::"
```

If the peer snapshot contains IPv6 addresses but `HostIPv6Addr` is not set, the node will fail to connect to those peers with `Network is unreachable`.
On a host without IPv6 routing, omit `HostIPv6Addr` entirely — the node will skip IPv6 peers.

**Diagnostic**: if logs show repeated `Net.ConnectionManager.Remote.ConnectError` against IPv6 addresses (e.g. `[2a05:...]:3001`) while all `HandshakeSuccess` entries are IPv4, the host has no IPv6 route.
Either enable IPv6 on the host and set `HostIPv6Addr`, or remove it from the config.

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
- **GDD impotent**: `TraceGDDDebug` shows multiple peers at equal density.
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

At default severity (Notice), only GSM events, `PeerStarvedUs`, and ChainSync `Exception` are visible.
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

`detail: DMaximum` on `BlockFetch.Decision` is required for per-peer decline reasons in `PeersFetch` events.

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

Within seconds of startup, expect `Consensus.GSM.InitializedInPreSyncing` and `Consensus.CSJ.InitializedAsDynamo`.
