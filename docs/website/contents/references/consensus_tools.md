# Consensus tools

This repository ships a set of command-line tools built on top of the Consensus
layer. They all operate on the same artifacts a `cardano-node` produces and
consumes — the ChainDB (ImmutableDB + VolatileDB + ledger snapshots) and the
node configuration files — so they are useful to node operators, benchmarking
teams, and developers debugging Consensus or Ledger issues.

The sources live in
[`ouroboros-consensus-cardano/app`](https://github.com/IntersectMBO/ouroboros-consensus/tree/main/ouroboros-consensus-cardano/app).

For the configuration knobs and on-disk formats these tools operate on, see
[Consensus configuration values](./consensus_configuration).

## Which tool do I need?

| If you want to… | Use |
|---|---|
| Inspect, validate, or benchmark an existing chain database; create ledger snapshots | [`db-analyser`](#db-analyser) |
| Move blocks from a VolatileDB into an ImmutableDB so other tools can see them | [`db-immutaliser`](#db-immutaliser) |
| Create a synthetic chain (e.g. for benchmarks) without running a network | [`db-synthesizer`](#db-synthesizer) |
| Cut an ImmutableDB back to an earlier slot or block number | [`db-truncater`](#db-truncater) |
| Serve an ImmutableDB over the network so a node can sync from it | [`immdb-server`](#immdb-server) |
| Convert ledger snapshots between the in-memory and LSM formats | [`snapshot-converter`](#snapshot-converter) |
| Generate or check Praos headers for conformance testing | [`gen-header`](#gen-header) |

## Building the tools

All executables are components of the `ouroboros-consensus` package, so from a
checkout of this repository:

```sh
cabal build db-analyser     # or any other tool name
cabal run db-analyser -- --help
```

**A note on assertions:** the top-level `cabal.project` enables assertions both
in our local packages and in some dependencies. If you build these tools for
performance-sensitive measurements, override this locally via a
`cabal.project.local`, or build them without assertions via Nix. For the
default GHC version (see `compiler-nix-name` in `nix/haskell.nix`):

```sh
nix build .#db-analyser
```

For other GHC versions, use one of:

```sh
nix build .#hydraJobs.x86_64-linux.native.haskell96.exesNoAsserts.db-analyser
nix build .#hydraJobs.x86_64-linux.native.haskell910.exesNoAsserts.db-analyser
nix build .#hydraJobs.x86_64-linux.native.haskell912.exesNoAsserts.db-analyser
```

Most tools need the **node configuration file** that was used to populate the
database they read (the `--config` flag): the file usually called
`config.json`/`mainnet-config.json`, which points at the genesis files of each
era. The examples below assume:

```sh
export NODE_HOME=/path/to/cardano-node/working/dir
```

---

## db-analyser

`db-analyser` streams the blocks of an existing chain database and runs one
analysis over them. It was initially developed to help debug Consensus issues,
and has grown into the main tool for benchmarking and profiling the
block-processing parts of the code base: it can time ledger operations, measure
UTxO growth, reproduce the mempool/forging loop, validate blocks, and create
ledger snapshots at arbitrary slots.

### When to use it

- You want to know *what is on a chain* (block numbers, hashes, sizes, EBBs,
  transaction counts).
- You want to *benchmark* how expensive blocks are to apply, or how the UTxO
  set grows over time.
- You want a *ledger snapshot* at a specific slot — e.g. to bisect a bug, to
  speed up later analyses, or to seed another tool.
- You suspect database corruption and want to *validate* the on-disk files.

### General operation

The tool works on a `cardano-node` ChainDB — in fact only on the
**ImmutableDB** and the **ledger snapshots** (`<db>/immutable` and
`<db>/ledger`). Blocks that only exist in the VolatileDB are invisible to it;
copy them into the ImmutableDB first with [`db-immutaliser`](#db-immutaliser).

```sh
cabal run db-analyser -- \
  --config $NODE_HOME/configuration/cardano/mainnet-config.json \
  --db $NODE_HOME/mainnet/db \
  --in-mem \
  <analysis flag> [options]
```

### Common options

| Flag | Meaning |
|---|---|
| `--db PATH` | Path to the ChainDB (required). |
| `--config PATH` | Node configuration file (required). |
| `--in-mem` / `--lsm` | LedgerDB backend to use for analyses that maintain a ledger state (one of the two is required; there is no default). See below. |
| `--analyse-from SLOT_NUMBER` | Start the analysis at the block in slot `SLOT_NUMBER` (a block must exist exactly at that slot). See below. |
| `--num-blocks-to-process INT` | Cap on the number of blocks to process. |
| `--db-validation POLICY` | Extent of the on-disk file validation when opening the database: `validate-all-blocks` or `minimum-block-validation` (only the most recent chunk). This is unrelated to validation of the ledger rules. |
| `--verbose` | Enable verbose logging (ChainDB traces, timestamped, on stderr). |
| `--threshold T` | Override the PBft signature threshold (rarely needed). |

If no analysis flag is given, the tool only opens the database, validating all
chunks of the ImmutableDB, and exits — useful as a standalone integrity check.

#### Choosing a ledger backend: `--in-mem` vs `--lsm`

Analyses that maintain a ledger state need a LedgerDB, and you must pick its
backend (see
[Consensus configuration values §1](./consensus_configuration#1-ledgerdb-and-storage-backends)):

- `--in-mem`: the whole UTxO set lives in memory. Fastest, but needs as much
  RAM as a pre-UTxO-HD node.
- `--lsm`: the UTxO set lives in an LSM tree on disk. The tool creates its
  working LSM database under `<db>/lsm`, with a fresh random bloom-filter salt
  on every run. If the node configuration sets `LedgerDB.LSMExportPath`,
  snapshots taken with `--store-ledger` are additionally *exported* into that
  directory as standalone LSM snapshots, which
  [`snapshot-converter`](#snapshot-converter) can then convert or import (the
  random salt is irrelevant here, as exported snapshots record their own
  salt).

#### Starting from a slot: `--analyse-from`

`--analyse-from SLOT_NUMBER` starts processing at the block with that slot
number, which **must exist** in the ImmutableDB.

For analyses that need a ledger state, the tool initialises the LedgerDB from
the **newest snapshot in `<db>/ledger` that is not newer than that slot** and
replays blocks until the ledger state is *exactly* at the requested slot, no
matter which snapshots happen to exist on disk. If no snapshot sits exactly at
the requested slot, it prints a warning — the replay from an older snapshot can
take considerably longer. Both snapshots written by a node and snapshots
created with `--store-ledger` work, as long as they were created with the same
backend that the analysis uses.

Without `--analyse-from`, ledger-state analyses start from the genesis ledger
state and (re)apply every block, which on mainnet takes days — creating
intermediate snapshots with `--store-ledger` first is strongly recommended.

### Analyses

Exactly one analysis can be selected per run.

* `--show-slot-block-no` — prints slot number, block number and hash of every
  block.

* `--count-tx-outputs` — for each block, prints the number of transaction
  outputs created in it together with the running total so far.

* `--show-block-header-size` — prints the header size of every block, and at
  the end the maximum header size seen.

* `--show-block-txs-size` — prints the number of transactions and total
  transaction size per block.

* `--show-ebbs` — prints every Epoch Boundary Block (Byron-era), with its
  hash, its predecessor's hash, and whether it is one of the known EBBs
  (hard-coded in `Ouroboros.Consensus.Byron.EBBs`).

* `--count-blocks` — counts the number of blocks processed.

* `--store-ledger SLOT_NUMBER [--full-ledger-validation]` — replays blocks and
  stores a ledger snapshot once the requested slot is reached. The snapshot is
  written to `<db>/ledger/<SLOT>_db-analyser`, follows the regular on-disk
  snapshot format of the chosen backend (see
  [Consensus configuration values §6](./consensus_configuration#6-on-disk-snapshot-layout)),
  and — because it has a suffix — will never be garbage-collected by a node's
  snapshot retention policy. If no block exists exactly at the requested slot,
  the snapshot is taken at the next block after it (with a warning).

  By default blocks are only *re*applied, skipping e.g. signature and Plutus
  script validation. Pass `--full-ledger-validation` to run the full ledger
  rules instead (much slower) — useful when hunting a Ledger bug. If a block
  fails to apply, the last valid ledger state is stored before exiting.

* `--checkThunks BLOCK_COUNT` — applies blocks and checks the ledger state for
  unexpected thunks (memory leaks) every `BLOCK_COUNT` blocks, failing on the
  first one found.

* `--trace-ledger` — maintains a ledger state and emits markers for the ledger
  phases and 'significant' events (e.g. epoch transitions) to the GHC event
  log. Run with the `-l` RTS option and inspect the resulting eventlog with
  e.g. `ghc-events` or Eventlog2html.

* `--benchmark-ledger-ops [--out-file FILE] [--reapply]` — benchmarks the main
  ledger operations for every block: forecasting, ticking and applying the
  header, and ticking and applying the block. One line of statistics per block
  is written to `FILE` (default stdout):

  - slot number and the gap (in slots) to the previous block;
  - total elapsed time, mutator time and GC time spent applying the block, in
    microseconds (from [`GHC.Stats.RTSStats`](https://hackage.haskell.org/package/base/docs/GHC-Stats.html#t:RTSStats));
  - number of major and minor garbage collections, and allocated bytes;
  - mutator time spent in each phase: forecast, header tick, header apply,
    block tick, block apply;
  - block size in bytes;
  - era-specific stats: number and total size of the transactions, and (for
    Alonzo and later) the total script execution steps.

  With `--reapply`, headers and blocks are *re*applied rather than fully
  validated, which isolates the cost of the cheaper code path a caught-up node
  pays when adopting its own chain again or replaying.

  The [`ouroboros-consensus-cardano/scripts/plot-ledger-ops-cost.gp`](https://github.com/IntersectMBO/ouroboros-consensus/blob/main/ouroboros-consensus-cardano/scripts/plot-ledger-ops-cost.gp)
  gnuplot script can plot the output; see the script header for usage:

  ```sh
  gnuplot -e "bench_data='ledger-ops-cost.csv'" \
          -e "out_file='results.png'" \
          ouroboros-consensus-cardano/scripts/plot-ledger-ops-cost.gp
  ```

* `--get-block-application-metrics NUM [--out-file FILE]` — computes block
  application metrics every `NUM` blocks: block and slot number, UTxO size in
  MB and number of UTxO map entries. The
  [`ouroboros-consensus-cardano/scripts/plot_utxo_growth.py`](https://github.com/IntersectMBO/ouroboros-consensus/blob/main/ouroboros-consensus-cardano/scripts/plot_utxo_growth.py)
  script plots the results.

* `--repro-mempool-and-forge INT` — populates the mempool with the
  transactions of `INT` blocks at a time and runs the forging loop, printing
  the time spent ticking and snapshotting the mempool (monotonic time, mutator
  time and GC time). Useful to investigate regressions in the forging loop or
  in the mempool adding/snapshotting logic.

  Only `1` or `2` are supported. Note that with `2`, even on a fully valid
  chain like mainnet, you might get an error like
  `Mempool rejected some of the on-chain txs: ... (OutsideValidityIntervalUTxO ...)`:
  transactions of the second block may be invalid in the mempool's ledger
  state. Start with `1`, and use `2` only to test the performance impact of a
  more filled mempool.

### Examples

#### Storing a ledger snapshot

Take a snapshot of the ledger state at slot 100:

```sh
cabal run db-analyser -- \
  --config $NODE_HOME/configuration/cardano/mainnet-config.json \
  --db $NODE_HOME/mainnet/db \
  --in-mem \
  --store-ledger 100
```

If a snapshot at (or before) slot 50 already exists, start from it instead of
from genesis:

```sh
cabal run db-analyser -- \
  --config $NODE_HOME/configuration/cardano/mainnet-config.json \
  --db $NODE_HOME/mainnet/db \
  --in-mem \
  --analyse-from 50 \
  --store-ledger 100
```

#### Benchmarking the ledger operations

```sh
cabal run db-analyser -- \
  --config $NODE_HOME/configuration/cardano/mainnet-config.json \
  --db $NODE_HOME/mainnet/db \
  --in-mem \
  --analyse-from 100 \
  --benchmark-ledger-ops \
  --out-file ledger-ops-cost.csv \
  --num-blocks-to-process 200
```

#### Finding the intersection of two chain databases

Suppose you have two ChainDBs containing different forks and want to find
their intersection point. Run for both databases:

```sh
db-analyser --db /path/to/dbX --show-slot-block-no --in-mem \
  --config /path/to/config.json | cut -d ' ' -f 2- > dbX.log
```

(Adding `--analyse-from SLOT` is optional, if you know both chains were still
on the same fork at that slot, to cut down the runtime.)

Then compare the files with `comm`:

```sh
comm -1 -2 db0.log db1.log | tail   # last common blocks; the last one is the intersection
comm -3 db0.log db1.log | head      # first blocks after the intersection
```

---

## db-immutaliser

`db-immutaliser` copies a chain from a **VolatileDB** into an **ImmutableDB**,
so that tools which only read the ImmutableDB (like `db-analyser` or
`immdb-server`) can process those blocks. This is useful e.g. for analysing the
most recent blocks of a node's database, which still sit in the VolatileDB.

It selects the **best** candidate chain in the VolatileDB that extends the
current immutable tip — "best" by the same header-level chain order used by
chain selection (`SelectView`) — and appends its blocks to the ImmutableDB. It
does **not** perform any validation (and therefore needs no ledger snapshot):
only use it on blocks you trust, e.g. blocks produced by your own node.

```sh
cabal run db-immutaliser -- \
  --immutable-db /path/to/db/immutable \
  --volatile-db /path/to/db/volatile \
  --config /path/to/config.json
```

Note that the `--immutable-db` and `--volatile-db` flags point directly at the
respective subdirectories (not at the ChainDB directory), so blocks can be
copied between unrelated databases.

Additional flags:

- `--verbose` — print information on every volatile block reachable from the
  immutable tip, and on all candidate chains considered.
- `--dot-out PATH` — write the tree of volatile blocks rooted at the immutable
  tip to a file in graphviz DOT format; render it with the `dot` CLI tool.
- `--dry-run` — select a candidate and report it, but do not append anything
  to the ImmutableDB.

---

## db-synthesizer

`db-synthesizer` *forges* a valid ChainDB without running a node or a network,
using the same code path that a real block producer uses. The forged blocks
contain no transactions. Its primary use is creating chains of arbitrary
length cheaply, e.g. as input for benchmarks.

### When to use it

- You need a syntactically and cryptographically valid chain of a given length
  for benchmarking or testing other tools.
- You do not care about transaction content.

### Requirements

- `--config FILE` — a node configuration file. Only a few values regarding
  geneses and protocol are actually required, so a configuration stub is
  possible; for the expected key-value pairs see the `NodeConfigStub` type and
  its deserialization in `Cardano.Tools.DBSynthesizer.Orphans`.
- `--db PATH` — where to write the ChainDB.
- Block forging credentials, either as separate files
  (`--shelley-operational-certificate`, `--shelley-vrf-key`,
  `--shelley-kes-key`, all in JSON TextEnvelope format) or in bulk
  (`--bulk-credentials-file`, a JSON array of `[opcert, VRF key, KES key]`
  triples). The genesis must give the corresponding pools enough stake to be
  elected.

A minimal working setup — a staked genesis with bulk credentials for two
forgers — is provided in
[`ouroboros-consensus-cardano/test/tools-test/disk/config`](https://github.com/IntersectMBO/ouroboros-consensus/tree/main/ouroboros-consensus-cardano/test/tools-test/disk/config):

```sh
cabal run db-synthesizer -- \
  --config ouroboros-consensus-cardano/test/tools-test/disk/config/config.json \
  --db /tmp/synthesized-db \
  --bulk-credentials-file ouroboros-consensus-cardano/test/tools-test/disk/config/bulk-creds-k2.json \
  -s 10000
```

### Limiting synthesis

Exactly one limit must be given, up to which the tool forges:

- `-s NUMBER` / `--slots NUMBER` — number of slots to process;
- `-b NUMBER` / `--blocks NUMBER` — number of blocks to forge;
- `-e NUMBER` / `--epochs NUMBER` — number of epochs to process.

### Opening modes

By default the tool expects the `--db` directory to *not* exist yet, and fails
otherwise. Two flags change this:

- `-f` — force overwrite: wipe and recreate the directory. As a safety
  measure, it only does so if the directory looks like a ChainDB (its
  subdirectories are a subset of `immutable`/`ledger`/`volatile`/`gsm`).
- `-a` — append: open the existing ChainDB and continue forging from its tip
  (again only if the directory looks like a ChainDB).

---

## db-truncater

`db-truncater` truncates an **ImmutableDB** to an earlier state, deleting all
blocks after a given slot or block number. Typical uses: re-creating an
old-enough database state to test syncing behaviour, or cutting a database
back to just before a problematic block so the problem can be reproduced by
replaying it.

```sh
cabal run db-truncater -- \
  --db /path/to/db \
  --config /path/to/config.json \
  --truncate-after-slot 1000000
```

`--db` points at the ChainDB directory; the tool operates on its `immutable`
subdirectory.

Exactly one truncation target must be given:

- `--truncate-after-slot SLOT_NUMBER` — the new tip is the last block with a
  slot number ≤ `SLOT_NUMBER`; all later blocks are removed.
- `--truncate-after-block BLOCK_NUMBER` — the new tip is the last block with
  block number ≤ `BLOCK_NUMBER`. (This currently performs a linear search
  through the ImmutableDB, so it can take a while on a large database.)

If the requested truncation point is at or after the current tip, the tool
does nothing and reports that there is nothing to truncate.

Note that the VolatileDB and the ledger snapshots are left untouched. In
particular, ledger snapshots for slots after the truncation point will no
longer match the chain; consider deleting them.

---

## immdb-server

`immdb-server` is a standalone server that serves a Cardano **ImmutableDB**
via the ChainSync and BlockFetch (node-to-node) miniprotocols. It lets you
test syncing locally — against a stable, immutable chain — without having to
run a full node just for serving blocks.

```sh
cabal run immdb-server -- \
  --db /path/to/db/immutable/ \
  --config /path/to/cardano/config.json \
  --port 3001
```

- `--db` points directly at the `immutable` directory.
- `--port` defaults to `3001`. The server listens on `127.0.0.1` only.

Log output is sparse: it traces handshakes, mux events and exceptions only.

The ChainSync miniprotocol terminates with a `ReachedImmutableTip` exception
when a client sends `MsgRequestNext` after reaching the tip of the served
ImmutableDB; this is expected, as an ImmutableDB never grows while being
served.

To point a syncing node at a running `immdb-server`, use a topology file like:

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

See the [topology documentation](https://developers.cardano.org/docs/operate-a-stake-pool/node-operations/topology/)
for more details.

---

## snapshot-converter

`snapshot-converter` manages and converts the ledger state snapshots used by
`cardano-node`. It understands two snapshot formats — **Mem** (the InMemory
backend format, fully contained in one directory) and **LSM** — and offers
four subcommands in two groups:

- **Conversions** (`convert`, `daemon`) — convert between Mem snapshots and
  *exported* LSM snapshots. They never touch a live LSM database; the LSM
  ledger tables must first be exported into a standalone directory (see
  below). These commands decode ledger states and therefore need `--config`
  (and accept `--threshold`, like `db-analyser`).
- **LSM database operations** (`lsm export`, `lsm import`) — move snapshots
  out of, or into, an *offline* LSM database. No ledger decoding is involved,
  so no `--config` is needed.

The same functionality is also exposed as a `cardano-node` subcommand. The
snapshot formats themselves (the `state`/`meta`/tables layout) are described
in
[Consensus configuration values §6](./consensus_configuration#6-on-disk-snapshot-layout).

### When to use it

- You are switching a node between the InMemory and LSM LedgerDB backends and
  want to keep your synced state instead of replaying from genesis.
- You received a Mem snapshot (e.g. via Mithril) and want to start an
  LSM-backed node from it, or vice versa.
- You want to continuously mirror an LSM node's snapshots as portable Mem
  snapshots (daemon mode).

### Exported LSM snapshots

Inside a running node, an LSM snapshot is split between the snapshots
directory (the `state`/`meta` files) and the LSM database itself (the ledger
tables). Since the database cannot be shared, the tables of a snapshot must be
*exported* into a standalone directory before they can be moved around or
converted. An exported LSM snapshot is thus a pair of: a snapshot directory
with the `state`/`meta` files, plus a directory with the exported tables.

Exported snapshots are produced either by `snapshot-converter lsm export`
(from an offline database), by the node itself as it takes snapshots (when
`LedgerDB.LSMExportPath` is set in its configuration), or by a `convert` run
with `--lsm-export-to`.

No bloom-filter salt ever needs to be provided: `lsm export` salts the
exported snapshot with the database's own on-disk salt, and `lsm import`
recovers the salt from the exported snapshot.

### Snapshot naming convention

Snapshots are named after the **slot number** of the contained ledger state,
optionally with a suffix: a snapshot for slot 100 lives in a directory named
`100` or `100_some-suffix`. This is the same convention `cardano-node` uses.
The conversion commands check the name against the slot of the actual ledger
state and fail on a mismatch.

### `convert`: one-shot conversion

```sh
# mem to exported lsm
snapshot-converter convert \
  --snapshot-in  /path/to/mem-snapshots/100 \
  --snapshot-out /path/to/out-snapshots/100 --lsm-export-to DIR \
  --config PATH

# exported lsm to mem
snapshot-converter convert \
  --snapshot-in  /path/to/snapshots/100 --lsm-import-from DIR \
  --snapshot-out /path/to/mem-snapshots/100 \
  --config PATH
```

- `--snapshot-in PATH` / `--snapshot-out PATH` — the input/output snapshot
  directories (named after the slot, holding at least `state`/`meta`).
- `--lsm-import-from DIR` — if set, the input is an exported LSM snapshot
  whose tables live at `DIR/<input snapshot name>`; otherwise the input is a
  Mem snapshot.
- `--lsm-export-to DIR` — if set, the output is an exported LSM snapshot whose
  tables are written to `DIR/<output snapshot name>`; otherwise the output is
  a Mem snapshot.

The tool copies the ledger state file, then streams the UTxO tables from the
input format to the output format (with a progress indicator), checking the
result against the checksum recorded in the input snapshot's metadata — if the
metadata file is missing it warns that correctness cannot be guaranteed — and
finally writes a fresh metadata file for the output snapshot.

**Caution:** if the output paths already exist, the tool *wipes* them (after
asking for confirmation).

Common failure modes, all reported with explicit errors: a snapshot directory
not named after the contained slot; an input snapshot written by an
incompatible node version (deserialisation failure); a metadata/backend
mismatch (e.g. passing an exported LSM snapshot without `--lsm-import-from`);
and a checksum mismatch (corrupted input).

### `daemon`: continuous conversion

In daemon mode, the tool watches the ledger snapshots directory of a running
node and converts every exported **LSM** snapshot the node completes into a
**Mem** snapshot, as it is written. It is only meaningful for a node that
produces LSM snapshots *and* exports them (i.e. with `LedgerDB.LSMExportPath`
set in the node configuration).

```sh
snapshot-converter daemon \
  --monitor-snapshots-in PATH \
  --lsm-exported-path PATH \
  --output-snapshots-in PATH \
  --config PATH
```

- `--monitor-snapshots-in` — the node's ledger snapshots directory, watched
  for finished snapshots (it reacts when a snapshot's `meta` file is closed
  after writing).
- `--lsm-exported-path` — the directory into which the node exports its LSM
  snapshots; the exported tables for a snapshot named `N` are expected at
  `<this>/N`.
- `--output-snapshots-in` — where to write the converted Mem snapshots, named
  after the original snapshot.

The daemon runs until interrupted, logging one line per conversion. Unlike
`convert` it never asks for confirmation, so existing output snapshots with
the same name are overwritten.

### `lsm export` / `lsm import`: offline LSM database operations

These commands operate directly on an LSM database directory, which must not
be in use by a node, and don't need `--config`:

```sh
snapshot-converter lsm export \
  --lsm-database DIR \
  --lsm-export-to DIR \
  --snapshot NAME

snapshot-converter lsm import \
  --lsm-database DIR \
  --lsm-import-from DIR \
  --snapshot NAME
```

- `lsm export` exports the snapshot named `NAME` (e.g. `163470034` or
  `163470034_my-suffix`) out of the database at `--lsm-database` into
  `<--lsm-export-to>/NAME`, which must not exist yet. Note that this exports
  only the ledger tables: to obtain a complete exported LSM snapshot, pair it
  with the snapshot's `state`/`meta` directory from the node's snapshots
  directory.
- `lsm import` creates a **new** LSM database at `--lsm-database` (the
  directory is created if missing, and must be empty otherwise) containing the
  snapshot found at `<--lsm-import-from>/NAME`. The database and the source
  directory must live on the same volume.

---

## gen-header

`gen-header` generates valid and invalid **Praos headers**, and validates
them. Its purpose is testing header validation logic — in particular,
providing test vectors for conformance testing of alternative implementations
of the Cardano protocols.

Two subcommands:

- `gen-header generate --count N` — generates `N` samples and writes them as
  JSON to stdout. Each sample contains a *generator context* (keys, nonce,
  protocol parameters, stake distribution) together with a header which is
  either valid or *mutated* in a way that must make validation fail in a
  specific manner (e.g. an invalid KES signature or a wrong VRF proof).
- `gen-header validate` — reads such a JSON sample from stdin, runs the actual
  Praos KES/VRF header validation on each header within its context, and
  prints for each one whether the outcome matched the expectation, e.g.
  `Valid NoMutation` or `Invalid MutateKESKey "..."`.

```sh
cabal run gen-header -- generate -c 5 > sample.json
cabal run gen-header -- validate < sample.json
```
