In these notes we describe how to benchmark and profile genesis with a
`cardano-node` downloading blocks from mainnet. We wrote three scripts to
help with this task:

* provision.sh
    * installs nix, clones repositories, and configures a cabal project to
      use these repositories.
    * Downloads a db-mainnet snapshot to save setup time.
    * Installs system dependencies via nix and builds the cardano-node and
      the immdb-server.
* run-server-node.sh:
  Runs the immdb-server and launches toxiproxy so the server is reachable
  through multiple addresses that can be supplied as different peers for the
  syncing node to connect to. The amount of addresses is taken as an input
  parameter to the script.
* run-syncing-node.sh
    * Resets the state of the chain db so synchronization starts at a
      designated slot in the past.
    * Runs the syncing node until the tip of the volatile db reaches or is
      newer than a designated slot.
    * At the end prints how long it took to run the syncing node and the memory
      usage report from the GHC runtime produced with +RTS -s

provision.sh must run first to install system dependencies and tooling. Then
the server and its proxies can be started with run-server-node.sh. And finally
run-syncing-node.hs can be started in another terminal.

All three scripts build Haskell libraries and binaries in the normal way, but
with a profiling-enabled GHC. This makes it easy to reuse the setup to build
with profiling enabled.

We have used the scripts successfully in a t3.2xlarge machine in aws with 400
GB of storage and ubuntu installed.

### How to start the server

The following command runs the server and configures 30 proxy addresses to
connect to it.
```
$ ./run-server-node.sh 30
```

### How to sync with genesis disabled

The following command runs the syncing node which will connect to a single peer
and synchronize using Praos.
```
$ ./run-syncing-node.sh 1
```

The following command will connect to 30 peers and will synchronize using Praos.
```
$ ./run-syncing-node.sh 30
```

### How to sync with genesis enabled

The following command runs the syncing node which will connect to two peers and
synchronize using Genesis. Genesis can also synchronize with only 1 peer, but
it is slower because the implementation is not optimized for that case.
```
$ ENABLE_GENESIS=1 ./run-syncing-node.sh 2
```

Using 30 peers should produce little overhead when compared to only 2 peers.
```
$ ENABLE_GENESIS=1 ./run-syncing-node.sh 30
```

### How to tweak the range of slots to sync

By default, the script will synchronize 50000 slots starting at slot 100007913.
To synchronize 155000 slots instead, the environment variable `NUM_SLOTS` can
be used.
```
$ NUM_SLOTS=155000 ENABLE_GENESIS=1 ./run-syncing-node.sh 30
```

The initial slot of the range is harder to change because the startup of the
node is optimized with a snapshot of the ledger at slot 100007913. Generating a
snapshot for a different slot would make it possible to start the range from
it. But at the price of deleting the current chaindb, one can start syncing
from slot 0 with,

```
# WARNING: deletes the chaindb from the local storage
$ SYNC_FROM_0=1 NUM_SLOTS=155000 ./run-syncing-node.sh 1
```

### How to collect the eventlog of the syncing node

The following command collects the eventlog.
```
$ CARDANO_NODE_RTS_FLAGS=-ls ENABLE_GENESIS=1 ./run-syncing-node.sh 30
```

This produces a file `cardano-node/cardano-node.eventlog` that can be fed to
`ghc-events-analyze` to observe the CPU consumption per thread.

### How to produce tracing with full detail

The following command enables tracing with full detail.
```
$ ENABLE_FULL_TRACING=1 ENABLE_GENESIS=1 ./run-syncing-node.sh 30
```
