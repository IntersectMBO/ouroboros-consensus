# How to run the Leios Octoboer demo

See https://github.com/IntersectMBO/ouroboros-consensus/issues/1701 for context.

## Prepare the shell environment

- If your environment can successfully execute `cabal build exe:cardano-node` from this commit, then it can build this demo's exes.
    ````
    $ git log -1 10.5.1
    commit ca1ec278070baf4481564a6ba7b4a5b9e3d9f366 (tag: 10.5.1, origin/release/10.5.1, nfrisby/leiosdemo2025-anchor)
    Author: Jordan Millar <jordan.millar@iohk.io>
    Date:   Wed Jul 2 08:24:11 2025 -0400
    
        Bump node version to 10.5.1
    ```
- The Python script needs `pandas` and `matplotlib`.
- The bash script needs `ps` (which on a `nix-shell` might require the `procps` package for matching CLIB, eg), and `sqlite`, and so on.
- Set `CONSENSUS_BUILD_DIR` to the absolute path of a directory in which `cabal build exe:immdb-server` will succeed.
- Set `NODE_BUILD_DIR` to the absolute path of a directory in which `cabal build exe:cardano-node` will succeed.
- Set `CONSENSUS_REPO_DIR` to the absolute path of the `ouroboros-consensus` repo.

- Checkout a patched version of the `cardano-node` repository, something like the following, eg.

```
6119c5cff0 - (HEAD -> nfrisby/leiosdemo2025, origin/nfrisby/leiosdemo2025) WIP add Leios demo Consensus s-r-p (25 hours ago) <Nicolas Frisby>
```

- If you're using a `source-repository-package` stanza for the `cabal build exe:cardano-node` command in the `NODE_BUILD_DIR`, confirm that it identifies the `ouroboros-consensus` commit you want to use (eg the one you're reading this file in).

## Build the exes

```
$ (cd $CONSENSUS_BUILD_DIR; cabal build exe:immdb-server exe:leiosdemo202510)
$ IMMDB_SERVER="$(cd $CONSENSUS_BUILD_DIR; cabal list-bin exe:immdb-server)"
$ DEMO_TOOL="$(cd $CONSENSUS_BUILD_DIR; cabal list-bin exe:leiosdemo202510)"
$ (cd $CONSENSUS_BUILD_DIR; cabal build exe:cardano-node)
$ CARDANO_NODE="$(cd $CONSENSUS_BUILD_DIR; cabal list-bin exe:cardano-node)"
```

## Prepare the input data files

```
$ (cd $CONSENSUS_BUILD_DIR; $DEMO_TOOL generate demoUpstream.db "${CONSENSUS_REPO_DIR}/demoManifest.json" demoBaseSchedule.json)
$ cp demoBaseSchedule.json demoSchedule.json
$ # You must now edit demoSchedule.json so that the first number in each array is 182.9
$ echo '[]' >emptySchedule.json
$ # create the following symlinks
$ (cd $CONSENSUS_REPO_DIR; ls -l $(find nix/ -name genesis-*.json))
lrwxrwxrwx 1 nfrisby nifr 30 Oct 24 16:27 nix/leios-mvd/immdb-node/genesis-alonzo.json -> ../genesis/genesis.alonzo.json
lrwxrwxrwx 1 nfrisby nifr 29 Oct 24 16:27 nix/leios-mvd/immdb-node/genesis-byron.json -> ../genesis/genesis.byron.json
lrwxrwxrwx 1 nfrisby nifr 30 Oct 24 16:27 nix/leios-mvd/immdb-node/genesis-conway.json -> ../genesis/genesis.conway.json
lrwxrwxrwx 1 nfrisby nifr 31 Oct 24 16:27 nix/leios-mvd/immdb-node/genesis-shelley.json -> ../genesis/genesis.shelley.json
lrwxrwxrwx 1 nfrisby nifr 30 Oct 24 16:27 nix/leios-mvd/leios-node/genesis-alonzo.json -> ../genesis/genesis.alonzo.json
lrwxrwxrwx 1 nfrisby nifr 29 Oct 24 16:27 nix/leios-mvd/leios-node/genesis-byron.json -> ../genesis/genesis.byron.json
lrwxrwxrwx 1 nfrisby nifr 30 Oct 24 16:27 nix/leios-mvd/leios-node/genesis-conway.json -> ../genesis/genesis.conway.json
lrwxrwxrwx 1 nfrisby nifr 31 Oct 24 16:27 nix/leios-mvd/leios-node/genesis-shelley.json -> ../genesis/genesis.shelley.json
```

## Run the scenario

Run the scenario with `emptySchedule.json`, ie no Leios traffic.

```
$ LEIOS_UPSTREAM_DB_PATH="$(pwd)/demoUpstream.db" LEIOS_SCHEDULE="$(pwd)/emptySchedule.json" SECONDS_UNTIL_REF_SLOT=5 REF_SLOT=182 CLUSTER_RUN_DATA="${CONSENSUS_REPO_DIR}/nix/leios-mvd" CARDANO_NODE=$CARDANO_NODE IMMDB_SERVER=$IMMDB_SERVER ${CONSENSUS_REPO_DIR}/scripts/leios-demo/leios-october-demo.sh
$ # wait about ~20 seconds before stopping the execution by pressing any key
```

Run the scenario with `demoSchedule.json`.

```
$ LEIOS_UPSTREAM_DB_PATH="$(pwd)/demoUpstream.db" LEIOS_SCHEDULE="$(pwd)/demoSchedule.json" SECONDS_UNTIL_REF_SLOT=5 REF_SLOT=182 CLUSTER_RUN_DATA="${CONSENSUS_REPO_DIR}/nix/leios-mvd" CARDANO_NODE=$CARDANO_NODE IMMDB_SERVER=$IMMDB_SERVER ${CONSENSUS_REPO_DIR}/scripts/leios-demo/leios-october-demo.sh
$ # wait about ~20 seconds before stopping the execution by pressing any key
```

## Analysis

Compare and contrast the cell that is in the column for `latency_ms` and the row for the Praos block in slot 183.

**WARNING**.
Each execution consumes about 0.5 gigabytes of disk.
The script announces where (eg `Temporary data stored at: /run/user/1000/leios-october-demo.c5Wmxc`), so you can delete each run's data when necessary.

**INFO**.
If you don't see any data in the 'Extracted and Merged Data Summary' table, then check the log files in the run's temporary directory.
This is where you might see messages about, eg, the missing `genesis-*.json` files, bad syntax in the `demoSchedule.json` file, etc.
