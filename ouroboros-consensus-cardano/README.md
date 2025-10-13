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

* `app/snapshot-converter.hs`: converts snapshots among different storage formats.

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
nix build .#hydraJobs.x86_64-linux.native.haskell96.exesNoAsserts.db-analyser
nix build .#hydraJobs.x86_64-linux.native.haskell910.exesNoAsserts.db-analyser
nix build .#hydraJobs.x86_64-linux.native.haskell912.exesNoAsserts.db-analyser
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

The tool works on a cardano-node's ChainDB (in fact, only on the ImmutableDB and the ledger snapshots). Thus the user must provide an obligatory `--db PATH` argument pointing to the particular DB.

If you want to analyse blocks from the VolatileDB, consider copying them to the ImmutableDB via db-immutaliser.

#### --analyse-from

```
[--analyse-from SLOT_NUMBER]
```

This flag allows to start processing blocks from the requested slot number.
A block with the corresponding slot number must exist in the ImmutableDB.

For certain analyses, a snapshot at that slot number must exist in `DB_PATH/ledger/SLOT_NUMBER_db-analyser` - where `SLOT_NUMBER` is the value provided by the user with the `--analyse-from` flag.
The user can use snapshots created by the node or they can create their own snapshots via db-analyser - see the `--store-ledger` command

#### --num-blocks-to-process

```
[--num-blocks-to-process INT]
```

The user can limit the maximum number of blocks that db-analyser will process.

### Database validation, via --db-validation

The tool provides two database validation policies:

- `validate-all-blocks`, which will cause the tool to validate all chunks on the
  immutable database.
- `minimum-block-validation`, which will cause the tool to validate only the
  most recent chunk in the immutable database.

Note that these flags do not refer to Ledger validation.

#### Analysis

Lastly the user can provide the analysis that should be run on the chain:

* `--show-slot-block-no` prints the slot and block number and hash of each block
  it process.

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

  By default, for better performance, blocks are only *re*applied, skipping eg
  validation of signatures and Plutus scripts. If desired (eg when investigating
  a Ledger bug), one can use `--full-ledger-validation` to also perform these
  checks. If there is an error on block application, the previous ledger state
  is stored.

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

    When the `--reapply` flag is specified, we measure header/block
    *re*application instead of full application.

  - Size of the entire block in bytes.
  - Era-specific stats:
     - Byron:
        - Number of txs.
        - Total size of all txs.
     - Shelley-based:
        - Number of txs.
        - Total size of all txs.
        - Since Alonzo: Total script execution steps.

* `--repro-mempool-and-forge NUM` populates the mempool with the transactions
  from NUM blocks every time and then runs the forging loop. Useful to inspect
  regressions in the forging loop or in the mempool adding/snapshotting logic.
  The output shows the time spent on ticking and snapshotting the mempool broken
  down into:
  - real monotonic measured time
  - time spent in the mutator in microseconds
  - time spent in GC in microseconds

  Currently, only NUM=1 or NUM=2 are supported. Note that with NUM=2, even for a
  fully valid chain (like mainnet), you might get an error like this:

  ```
  Mempool rejected some of the on-chain txs: ... (OutsideValidityIntervalUTxO ...) ...
  ```

  Therefore, it is recommended to start with NUM=1, and only use NUM=2 when you
  want to test the performance impact of a more filled mempool.

* `--get-block-application-metrics NUM` computes different block application metrics every `NUM` blocks.
It currently outputs block number, slot number, UTxO size in MB, and UTxO map size.
In the [`scripts`](./scripts) directory we provide a `plot_utxo-growth.py` script that can be used to plot the these results.
See this file for usage information.

If no analysis flag is provided, then the ChainDB will be opened, all the chunks
in the immutable and volatile databases will be validated (see
[validation](#database-validation-via---db-validation)), and the tool will exit.

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
cabal run exe:db-analyser -- \
    --config $NODE_HOME/configuration/cardano/mainnet-config.json \
    --db $NODE_HOME/mainnet/db \
    --store-ledger 100
```

If we had a previous snapshot of the ledger state, say corresponding to slot
`50`, it is possible to tell `db-analyser` to start from this snapshot:

```sh
cabal run exe:db-analyser -- \
    --config $NODE_HOME/configuration/cardano/mainnet-config.json \
    --db $NODE_HOME/mainnet/db \
    --analyse-from 50 \
    --store-ledger 100
```

#### Running the ledger operations benchmark

To benchmark the ledger operations, using the setup mentioned in the foregoing
examples, one could run the tool as follows:

```sh
cabal run exe:db-analyser -- \
    --config $NODE_HOME/configuration/cardano/mainnet-config.json \
    --db $NODE_HOME/mainnet/db \
    --analyse-from 100 \
    --benchmark-ledger-ops \
    --out-file ledger-ops-cost.csv
```

The benchmarking command can be combined with `--num-blocks-to-process` to
specify the application of how many blocks we want to process. Eg:

```sh
cabal run exe:db-analyser -- \
    --config $NODE_HOME/configuration/cardano/mainnet-config.json \
    --db $NODE_HOME/mainnet/db \
    --analyse-from 100 \
    --benchmark-ledger-ops \
    --out-file ledger-ops-cost.csv \
    --num-blocks-to-process 200
```

##### Plotting the benchmark results

Assuming you are at the top level of `ouroboros-consensus`, and the benchmark data
has been written to a file named `ledger-ops-cost.csv`, a plot can be generated
by running:

```sh
gnuplot -e "bench_data='ledger-ops-cost.csv'" \
        -e "out_file='results.png'" \
           ouroboros-consensus-cardano/scripts/plot-ledger-ops-cost.gp
```

The plot will be written to a file named `results.png`. See the script file for
more usage options.

#### Finding the intersection between two ChainDBs

Suppose that you have two ChainDBs containing different forks, and you want to
find their intersection. This can be accomplished with db-analyser via the
`--show-slot-block-no` analysis:

First, run the following command for both of your ChainDBs:

```
db-analyser --analyse-from 1234 --db /path/to/dbX --show-slot-block-no \
  --config /path/to/config.json | cut -d ' ' -f 2- > dbX.log
```

Note that specificying `--analyse-from` is optional; it means that you are
certain that both ChainDBs still were on the same fork in slot `1234`, in order
to decrease the run time of the command.

Then, you can `diff` the resulting files to find the last common block, or the
`comm` tool like this:

 - Get the last few blocks in common (last one is the intersection):
   ```console
   comm -1 -2 db0.log db1.log | tail
   ```
 - Get the first few blocks after the intersection:
   ```console
   comm -3 db0.log db1.log | head
   ```

> It is possible to do this in logarithmic instead of linear time via a binary
> search; however, this hasn't been necessary so far.

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

Additional flags:

 - `--verbose`: Print additional information on the blocks reachable from the
   immutable tip, as well as all possible volatile candidates.

 - `--dot-out /path/to/volatile.dot`: Write the block tree rooted at the
   immutable tip in the graphviz DOT format to a file. The `dot` CLI tool can
   then be used to graphically render this tree.

 - `--dry-run`: Do not actually append anything to the ImmutableDB.

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
  "bootstrapPeers": [],
  "localRoots": [
    {
      "accessPoints": [
        {
          "address": "127.0.0.1",
          "port": 3001
        }
      ],
      "advertise": false,
      "trustable": true,
      "valency": 1
    }
  ],
  "publicRoots": []
}
```

See https://developers.cardano.org/docs/operate-a-stake-pool/node-operations/topology/ for more details.

## snapshot-converter

## About

This tool converts snapshots among the different backends supported by the node.

## Running the tool

Invoking the tool follows the same simple pattern always:

```sh
cabal run snapshot-converter -- <IN> <OUT> --config /path/to/cardano/config.json
```

The `<IN>` and `<OUT>` parameters depend on the input and output format, receiving options:
- `--mem-in PATH`/`--mem-out PATH` for InMemory
- `--lmdb-in PATH`/`--lmdb-out PATH` for LMDB
- `--lsm-database-in DB_PATH --lsm-snapshot-in PATH`/`--lsm-database-out DB_PATH --lsm-snapshot-out PATH` for LSM-trees.
