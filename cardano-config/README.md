# `cardano-config`

This package exposes a library that defines:

- A parser for CLI options based on `optparse-applicative` (see `parseCLIArgs`).

- Instances for parsing the configuration files from JSON/YAML (see `parseConfigurationFiles`).

- A function (`resolveConfiguration`) for combining the two above into a
  datatype representing the configuration of the `cardano-node` (`NodeConfiguration`).

The goal of this library is to offer a single entry-point for applications that
need access to the configuration file of the node, such as [`cardano-cli`](https://github.com/IntersectMBO/cardano-cli),
[`dmq-node`](https://github.com/IntersectMBO/dmq-node/), [the `ouroboros-consensus` tools](https://github.com/IntersectMBO/ouroboros-consensus/tree/main/ouroboros-consensus-cardano#consensus-db-tools), ...

In particular it offers the possibility of splitting the configuration file in multiple sub-files per component as in:

```console
$ cat config.json
{
    "Protocol": "protocol.json",
    "Storage": "storage.json",
    "Tracing": "tracing.json"
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
