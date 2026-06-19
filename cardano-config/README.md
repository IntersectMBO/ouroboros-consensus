# `cardano-config`

This package exposes a library that defines:

- A parser for CLI options based on `optparse-applicative` (see `parseCliArgs`).

- Instances for parsing the configuration files from JSON/YAML (see `parseConfigurationFiles`).

- A function (`resolveConfiguration`) for combining the two above into a
  datatype representing the configuration of the `cardano-node` (`NodeConfiguration`).

The goal of this library is to offer a single entry-point for applications that
need access to the configuration file of the node, such as [`cardano-cli`](https://github.com/IntersectMBO/cardano-cli),
[`dmq-node`](https://github.com/IntersectMBO/dmq-node/), [the `ouroboros-consensus` tools](https://github.com/IntersectMBO/ouroboros-consensus/tree/main/ouroboros-consensus-cardano#consensus-db-tools), ...

## What this library parses

The configuration is a single JSON/YAML object. The parsers are derived from
[`autodocodec`](https://hackage.haskell.org/package/autodocodec) codecs, and the
**authoritative, always-up-to-date key listing** (including nested fields,
defaults and validation) is the JSON Schema derived from those very codecs. Dump
it with the bundled executable:

```console
$ cardano-config-schema          # the whole configuration
$ cardano-config-schema --list   # the available components
$ cardano-config-schema Storage  # one component
```

Keys that none of the parsers below recognise are **ignored** (parsing neither
fails nor preserves them).

The recognised keys are grouped into the following components. Unless noted
otherwise, every key is optional and, when omitted, the node falls back to its
own default.

| Component | Top-level keys | Sub-file? |
| --- | --- | --- |
| **Storage** | `DatabasePath`, `LedgerDB` (`Snapshots`, `QueryBatchSize`, `Backend` = `V2InMemory`/`V2LSM`, `LSMDatabasePath`) | yes |
| **Consensus** | `ConsensusMode` (`PraosMode`/`GenesisMode`), `LowLevelGenesisOptions` (`EnableCSJ`, `EnableLoEAndGDD`, `EnableLoP`, `BlockFetchGracePeriod`, `BucketCapacity`, `BucketRate`, `CSJJumpSize`, `GDDRateLimit`) — Genesis mode only | yes |
| **Protocol** | `ByronGenesisFile`/`ByronGenesisHash`, `RequiresNetworkMagic`, `PBftSignatureThreshold`, `LastKnownBlockVersion-Major`/`-Minor`/`-Alt`, `ShelleyGenesisFile`/`Hash`, `AlonzoGenesisFile`/`Hash`, `ConwayGenesisFile`/`Hash`, `StartAsNonProducingNode`, `CheckpointsFile`/`CheckpointsFileHash` | yes |
| **Network** | `DiffusionMode`, `MaxConcurrencyBulkSync`, `MaxConcurrencyDeadline`, `ProtocolIdleTimeout`, `TimeWaitTimeout`, `EgressPollInterval`, `ChainSyncIdleTimeout`, `AcceptedConnectionsLimit`, the `TargetNumberOf*`/`SyncTargetNumberOf*` peer targets, `MinBigLedgerPeersForTrustedState`, `PeerSharing`, `ResponderCoreAffinityPolicy`, `ExperimentalProtocolsEnabled`, `TxSubmissionLogicVersion`, `TxSubmissionInitDelay` | yes |
| **LocalConnections** | `SocketPath`, `EnableRpc`, `RpcSocketPath` | no |
| **Mempool** | `MempoolCapacityBytesOverride`, `MempoolTimeoutSoft`, `MempoolTimeoutHard`, `MempoolTimeoutCapacity` | no |
| **Testing** | `ExperimentalHardForksEnabled`, the `Test<Era>HardForkAtEpoch`/`Test<Era>HardForkAtVersion` knobs (Shelley … Dijkstra), `DijkstraGenesisFile`/`DijkstraGenesisHash` | no |

The genesis files for the established eras — `ByronGenesisFile`,
`ShelleyGenesisFile`, `AlonzoGenesisFile`, `ConwayGenesisFile` — and the
`LastKnownBlockVersion-Major` / `-Minor` keys are required; everything else
(including the `*Hash` keys, the experimental `DijkstraGenesisFile` and
`CheckpointsFile`) is optional.

### Tracing is *not* parsed

Tracing is owned by the node's tracing system (hermod / `trace-dispatcher`), not
by this library. It is given under a single `HermodTracing` key, whose value is
either an inline object or a path (a string) to a separate file holding it. The
key is recognised and captured **opaquely**: it appears in the schema so that
users can see it exists, but its contents are neither interpreted nor validated
here. The authoritative schema for them lives in `trace-dispatcher`.

## Single-file and split forms

In the **single-file form**, all of the keys above live directly at the top level
of one object:

```console
$ cat config.json
{
    "ConsensusMode": "PraosMode",
    "ByronGenesisFile": "byron-genesis.json",
    "LastKnownBlockVersion-Major": 3,
    "LastKnownBlockVersion-Minor": 0,
    "ShelleyGenesisFile": "shelley-genesis.json",
    "LedgerDB": {
        "Backend": "V2InMemory",
        "NumOfDiskSnapshots": 2,
        "QueryBatchSize": 100000,
        "SnapshotInterval": 4320
    }
}
```

Alternatively, the `Storage`, `Consensus`, `Protocol` and `Network` components
may each be **split into a sub-file**: give the component key a string path
(relative to the main config file) instead of an inline object. The remaining
components (`LocalConnections`, `Mempool`, `Testing`) and the tracing keys are
always read from the main file's top level.

```console
$ cat config.json
{
    "Protocol": "protocol.json",
    "Storage": "storage.json"
}
$ cat storage.json
{
    "LedgerDB": {
        "Backend": "V2InMemory",
        "NumOfDiskSnapshots": 2,
        "QueryBatchSize": 100000,
        "SnapshotInterval": 4320
    }
}
```
