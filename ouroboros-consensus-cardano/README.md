# Cardano instantiation of the Consensus Layer

The [Consensus core package](../ouroboros-consensus) is abstract over the
specific choice of ledger, dictated by the type of block used. This package
defines the Block (and therefore the Ledger) for the Cardano blockchain, in
particular for the Byron, Shelley and Cardano instantiations.

There are also test-suites for each of the block definitions.

# Consensus DB tools

This package also contains a few executables:

* `app/db-analyser.hs`: performs different analyses on a ChainDB, for
  performance measurements or ensuring validity.

* `app/db-synthesizer.hs`: builds a chain, to be used with benchmarking purposes.

* `app/db-truncater.hs`: truncate an immutable DB to a specific block/slot number.

* `app/immdb-server.hs`: serve an immutable DB via ChainSync and BlockFetch.

### Assertions

Our top level `cabal.project` enables assertions in both our local packages
and the ones we depend on. If you build these tools from this repository in
order to do performance-sensitive measurements, it is recommended that you
override this locally by means of a `cabal.project.local`.

You can also build the tools without assertions via Nix. For the default GHC version (see `compiler-nix-name` in `nix/haskell.nix`), you can run
```sh
nix build .#db-analyser
```
For more GHC versions, use one of
```sh
nix build .#hydraJobs.x86_64-linux.native.haskell810.exesNoAsserts.ouroboros-consensus-cardano.db-analyser
nix build .#hydraJobs.x86_64-linux.native.haskell92.exesNoAsserts.ouroboros-consensus-cardano.db-analyser
nix build .#hydraJobs.x86_64-linux.native.haskell96.exesNoAsserts.ouroboros-consensus-cardano.db-analyser
```

## db-analyser

### About
This tool was initially developed to help Consensus debugging some issues, while the team was still working on Shelley. Later it was recognized that db-analyser might be potentially used by other teams when benchmarking / profiling some part of the code base.

### Running the tool

When you run db-analyser without any arguments, it will print out a help
message. The tool requires a few command line options. Some of these options
determine which analysis to perform on the ChainDB. In the following sections,
we explain each command line option.

#### --db PATH

The tool works on a cardano-node's ChainDB. Thus the user must provide an obligatory `--db PATH` argument pointing to the particular DB.

#### --verbose

db-analyser will get quite noisy

#### --only-immutable-db

By default db-analyser will process all blocks from the chain database
(`ChainDB`), from the genesis block up to the chain tip. In order to do this it
must first properly initialize the whole database. That means that before it
even starts processing blocks it will:

1. look for the latest snapshot stored in DB_PATH/ledger. The latest snapshot is determined by looking at the highest snapshot number. The storage layer of consensus expects the snapshot file names to be of the form `<SNAPSHOT NUMBER_SUFFIX>`, this is, a snapshot number, optionally followed by the `_` character and an arbitrary suffix. For instance, given snapshots files with these names `101_foo`, `100_db-analyser`, `99`, the file with the highest snapshot number is `101_foo`.
2. load that snapshot into memory
3. start replaying blocks
   * starting from that ledger state
   * while updating the ledger state in the process for each replayed block
   * keeping the intermediate results (ledger states) in memory while replaying blocks that live in the volatile DB (less than k blocks from the tip)

This may heavily impact any profiling that the user might be interested in doing.

To counter that problem `--only-immutable-db` flag was introduced.

```
[--only-immutable-db [--analyse-from SLOT_NUMBER]]
```

When enabled, db-analyser will work only with blocks from immutableDB, thus initialization described above will not happen.

This flag comes with an additional `--analyse-from` flag. It allows to start processing blocks from the requested slot number. A snapshot at that slot number must exist in `DB_PATH/ledger/SLOT_NUMBER_db-analyser` - where `SLOT_NUMBER` is the value provided by the user with the `--analyse-from` flag.
The user can use snapshots created by the node or they can create their own snapshots via db-analyser - see the `--store-ledger` command

#### COMMAND

There are three options: `byron`, `shelley`, `cardano`. When in doubt which one to use, use `cardano`.

* `byron`

User should run this if they are dealing with Byron only chain. When the command is `byron` then user must provide `--configByron PATH` pointing to a byron configuration file.

* `shelley`

User should run this if they are dealing with Shelley only chain (neither Byron nor Allegra or any other era that comes after). When the command is `shelley` then user must provide `--configShelley PATH` pointing to a shelley configuration file. They may also provide `--genesisHash HASH` and `--threshold THRESHOLD`

* `cardano`
User should run this if they are dealing with a `cardano` chain.

#### --num-blocks-to-process

```
[--num-blocks-to-process INT]
```

The user can limit the maximum number of blocks that db-analyser will process.

### Database validation

The tool provides two database validation policies:

- `validate-all-blocks`, which will cause the tool to validate all chunks on the
  immutable and volatile databases.
- `minimum-block-validation`, which will cause the tool to validate only the
  most recent chunk in the immutable database.

Note that these flags do not refer to Ledger validation.

#### Analysis

Lastly the user can provide the analysis that should be run on the chain:

* `--show-slot-block-no` prints the slot and block number of each block it
  process.

* `--count-tx-outputs` prints the block and slot number, tx out output for given
  block and the cumulative tx out output for all the blocks seen so far.

* `--show-block-header-size` shows block header size for each block and also the
  maximum head size it has seen in the whole chain it processed.

* `--show-block-txs-size` prints number of transactions and transactions size
  per each block.

* `--show-ebbs` prints all EBB blocks including their hash, previous block hash
  and a boolean value whether it is a known EBB (list of known EBBs stored in
  module `Ouroboros.Consensus.Byron.EBBs`).

* `--store-ledger SLOT_NUMBER` stores a snapshot of a ledger state under
  `DB_PATH/ledger/SLOT_NUMBER_db-analyser`. If there is no block under requested
  slot number, it will create one on the next available slot number (and issue a
  warning about this fact).

* `--count-blocks` prints out the number of blocks it saw on the chain

* `--benchmark-ledger-ops` applies the main ledger calculations to each block in
  the chain database, and collects different metrics (see below).
  The ledger operations this
  command benchmarks are: forecasting, ticking and applying the header, and
  ticking and applying a block. The benchmarking results are stored in the file
  specified by the `out-file` option. In the [`scripts`](./scripts) directory we
  provide a `plot-ledger-ops-cost.gp` script that can be used to plot the
  benchmarking results. See this file for usage information.
  The metrics collected by each block application are:
  - Block slot.
  - Gap since slot of previous block.
  - Total time spent applying the block, in microseconds (using [`RTSStats.elapsed_ns`](https://hackage.haskell.org/package/base-4.18.1.0/docs/GHC-Stats.html#t:RTSStats)).
  - Total time spent in the mutator when applying the block, in microseconds (using [`RTSStats.mutator_elapsed_ns`](https://hackage.haskell.org/package/base-4.18.1.0/docs/GHC-Stats.html#t:RTSStats)).
  - Total time spent in garbage collection when applying the block, in microseconds (using [`RTSStats.gc_elapsed_ns`](https://hackage.haskell.org/package/base-4.18.1.0/docs/GHC-Stats.html#t:RTSStats)).
  - Total number of major (oldest generation) garbage collections that took place when applying the block (using [`RTSStats.major_gcs`](https://hackage.haskell.org/package/base-4.18.1.0/docs/GHC-Stats.html#t:RTSStats)).
  - Total time spent in the mutator, in microseconds, when:
      - Forecasting.
      - Ticking the [chain dependent state](https://github.com/IntersectMBO/ouroboros-consensus/blob/51da3876c01edc2eec250fdc998f6cb33cdc4367/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Protocol/Abstract.hs#L55).
      - Applying a header.
      - Ticking the [ledger state](https://github.com/IntersectMBO/ouroboros-consensus/blob/51da3876c01edc2eec250fdc998f6cb33cdc4367/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Basics.hs#L174).
      - Applying a block.

* `--repro-mempool-and-forge NUM` populates the mempool with the transactions
  from NUM blocks every time and then runs the forging loop. Useful to inspect
  regressions in the forging loop or in the mempool adding/snapshotting logic.
  The output shows the time spent on ticking and snapshotting the mempool broken
  down into:
  - real monotonic measured time
  - time spent in the mutator in microseconds
  - time spent in GC in microseconds

* `--get-block-application-metrics NUM` computes different block application metrics every `NUM` blocks.
It currently outputs block number, slot number, UTxO size in MB, and UTxO map size.
In the [`scripts`](./scripts) directory we provide a `plot_utxo-growth.py` script that can be used to plot the these results.
See this file for usage information.

If no analysis flag is provided, then the ChainDB will be opened, all the chunks
in the immutable and volatile databases will be validated (see
[validation](#database-validation)), and the tool will exit.

### Examples

The use of this tool requires a chain database together with the configuration
files that were used (by `cadano-node`) to populate it (by means of chain
syncing). Assuming you ran `cardano-node` on your machine, it is quite handy to
save the location to the working directory from which it was run. Eg:

```sh
export NODE_HOME=/path/to/local/copy/of/cardano-node/working/dir/
```

#### Saving a snapshot

Suppose we have a local chain database in reachable from `$NODE_HOME`, and we
want to take a snapshot of the ledger state for slot `100`. Then we can run:

```sh
cabal run exe:db-analyser -- cardano \
    --config $NODE_HOME/configuration/cardano/mainnet-config.json \
    --db $NODE_HOME/mainnet/db \
    --only-immutable-db --store-ledger 100
```

If we had a previous snapshot of the ledger state, say corresponding to slot
`50`, it is possible to tell `db-analyser` to start from this snapshot:

```sh
cabal run exe:db-analyser -- cardano \
    --config $NODE_HOME/configuration/cardano/mainnet-config.json \
    --db $NODE_HOME/mainnet/db \
    --analyse-from 50 \
    --only-immutable-db --store-ledger 100
```

#### Running the ledger operations benchmark

To benchmark the ledger operations, using the setup mentioned in the foregoing
examples, one could run the tool as follows:

```sh
cabal run exe:db-analyser -- cardano
    --config $NODE_HOME/configuration/cardano/mainnet-config.json \
    --db $NODE_HOME/mainnet/db \
    --analyse-from 100 \
    --benchmark-ledger-ops \
    --out-file ledger-ops-cost.csv \
    --only-immutable-db
```

The benchmarking command can be combined with `--num-blocks-to-process` to
specify the application of how many blocks we want to process. Eg:

```sh
cabal run exe:db-analyser -- cardano
    --config $NODE_HOME/configuration/cardano/mainnet-config.json \
    --db $NODE_HOME/mainnet/db \
    --analyse-from 100 \
    --benchmark-ledger-ops \
    --out-file ledger-ops-cost.csv \
    --num-blocks-to-process 200
    --only-immutable-db
```

##### Plotting the benchmark results

Assuming you are at the top level of `ouroboros-network`, and the benchmark data
has been written to a file named `ledger-ops-cost.csv`, a plot can be generated
by running:

```sh
gnuplot -e "bench_data='ledger-ops-cost.csv'" \
        -e "out_file='results.png'" \
           ouroboros-consensus-cardano-tools/scripts/plot-ledger-ops-cost.gp
```

The plot will be written to a file named `results.png`. See the script file for
more usage options.

## db-immutaliser

Copy a specific chain from a volatile DB to an immutable DB, such that other
tools that expect an immutable DB can process the corresponding blocks.

Currently, it will copy the longest chain that extends the immutable tip, but it
will not perform any validation (and therefore, it does not require a ledger
snapshot).

Basic usage:
```sh
cabal run db-immutaliser -- \
  --immutable-db /path/to/db1/immutable \
  --volatile-db /path/to/db2/volatile \
  --config /path/to/config.json
```

The `config.json` should be in the same format (Node configuration) as for eg
db-analyser.

## db-synthesizer

### About
This tool synthesizes a valid ChainDB, replicating cardano-node's UX. The blocks
forged to synthesize the ChainDB won't contain any transactions.

A minimal test case is provided which incorporates a staked genesis and credentials for 2 forgers (cf. `test/config`).

### Running the tool
When you run db-synthesizer without any arguments, it will print out usage information.

* Providing a `cardano-node` configuration file is mandatory. However, only very few values regarding geneses and protocol are actually required. Using a configuration stub would be possible; for expected key-value pairs see the `NodeConfigStub` type as well as its deserialization in module `Cardano.Tools.DBSynthesizer.Orphans`.
* Specifying a path on the local file system where the ChainDB should be written to is mandatory.
* Credentials for block forging are mandatory and may be provided as seperate files or in bulk as a single file.

### Options

The options for configuration and credentials are identical to those of `cardano-node`. The other options have the following meaning:

#### forcing (over)write

The tool expects the given ChainDB path (`--db` option) to *not* be present. Should a directory by that name already exist, you can tell the tool to clear and reuse it with the flag `-f`. For safety deliberations, it will do so if and only if the directory is either empty, or contains a ChainDB.

#### limiting synthesis

A limit must be specified up to which the tool synthesizes a ChainDB. Possible limits are either the number of slots processed (`-s`), the number of epochs processed (`-e`) or the absolute number of blocks in the resulting ChainDB (`-b`).

## ImmDB Server

A standalone tool that serves a Cardano ImmutableDB via ChainSync and BlockFetch.

This can be useful to locally test syncing without having to run a full node just for serving the chain.

```sh
cabal run immdb-server -- \
  --db /path/to/db/immutable/ \
  --config /path/to/cardano/config.json
```
It also accepts a `--port` argument; if unspecified, it will serve on port 3001.

Currently, the ImmDB server has very sparse log output, i.e. it will only trace handshakes and exceptions.

The ChainSync miniprotocol will terminate with an exception when it receives a `MsgRequestNext` after the immutable tip.

To point a node to a running ImmDB server, use a topology file like
```json
{
  "Producers": [
    {
      "addr": "127.0.0.1",
      "port": 3001,
      "valency": 1
    }
  ]
}
```
