# Migrating to a UTxO-HD node as a user

This is a checklist of modifications needed to be performed by node users when
migrating to UTxO-HD. It aims to ease the transition for node 10.4.

## Populate the new configuration options

The shape of the configuration file was slightly altered to put together all the
options related to the LedgerDB into a JSON object `"LedgerDB"`. The shape of
this object is as follows:

```json
{
  ...,
  "LedgerDB": {
    "SnapshotInterval":   INT, //in seconds
    "NumOfDiskSnapshots": INT,
    "QueryBatchSize":     INT,
    "Backend":            <backend>
  }
}
```

Notice that `"SnapshotInterval"` and `"NumOfDiskSnapshots"` have moved from the
top-level to the `"LedgerDB"` object. The option `"DoDiskSnapshotChecksum"` has
been deleted as snapshots now include the metadata unconditionally. The value of
`<backend>` depends on your choice of backend for UTxO-HD. It can be one of
these:

```json
// InMemory
{
  "Backend": "V2InMemory"
}

// OnDisk
{
  "Backend":        "V1LMDB",
  "FlushFrequency": INT,
  "MapSize":        INT,  // number of GBs
  "LiveTablesPath": PATH
}
```

The explanation for the new options follows:

- `"QueryBatchSize"`: when querying the store for a big range of UTxOs (such as
  with `QueryUTxOByAddress`), the store will be read in batches of this size.
- `"FlushFrequency"`: the number of immutable blocks that need to exists before
  we flush the sequence of differences to the disk. Smaller implies more
  frequent disk writing, bigger implies more retained memory and slower
  consulting of values.
- `"MapSize"`: LMDB needs to be given the desired mapsize with which the
  database is open. See the [LMDB
  docs](http://www.lmdb.tech/doc/group__mdb.html#gaa2506ec8dab3d969b0e609cd82e619e5).
  By default we set this value to 16, which is fine on Linux as the file
  progressively grows. On Windows this will write a 16GB file directly on the
  disk. This is something that LMDB does, out of our control.
- `"LiveTablesPath"`: LMDB will use a live database on the file system to store
  the live UTxO set. Taking snapshots will "copy" this database to the snapshot
  path. Therefore this "live" database is not needed for starting a node (as the
  one from the snapshot will be used instead), but it will be used by the
  running node process. It is recommended that this path is **on SSD
  storage**. The default is `mainnet/db/lmdb`.

## Convert the existing Ledger snapshots with `snapshot-converter`

:::warning

Backup your ledger state snapshots before converting them to be safe in the
unlikely case that something fails during conversion.

:::

The format of the Ledger snapshots in the ChainDB has changed with UTxO-HD. The
simplest way to use the new format is deleting your snapshots and replaying the
chain from Genesis, however this can take some hours.

We provide the `snapshot-converter` tool which can load a snapshot in the Legacy
format and write it either in the in-memory or on-disk formats for
UTxO-HD. Supposing you have a legacy snapshot at `<dbDir>/ledger/<slotno>` you
can run the following command:

```console
$ cd <dbDir>
$ snapshot-converter Legacy ./ledger/<slotno> Mem ./ledger/<slotno>_mem cardano --config <path-to-config.json>

# or

$ cd <dbDir>
$ snapshot-converter Legacy ./ledger/<slotno> LMDB ./ledger/<slotno>_lmdb cardano --config <path-to-config.json>
```

This will create a snapshot with the proper format for UTxO-HD. See [the UTxO-HD
in depth document](utxo-hd-in-depth.md#storing-snapshots) for more
information on the contents of the different files in the snapshot.

If the snapshot does not have the expected structure or the backend for which it
was created is different from the backend used by the node, the node will issue
a warning mentioning the `snapshot-converter` and will ignore such snapshot.
