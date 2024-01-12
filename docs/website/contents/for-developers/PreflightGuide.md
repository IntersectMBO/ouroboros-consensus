# Preflight guide

> "What I Wish I Knew When Working In Consensus"

This document lists possible steps in getting familiar with the core of the Cardano universe with a focus on topics that are important for the daily life of a Consensus developer.

Every step has a clear **goal** as well as optional ideas for other activities.

This document contains various footnotes with additional information that can be safely skipped on a first pass.

If you have ideas for further steps, first check [this epic][preflight epic] to compare it with existing ideas, and then create a PR!

## Setting up a local node

The [cardano-node][] (usually just called "node") is the most important consumer[^other-consensus-consumers] of the Consensus and Storage layer maintained by the Consensus team.
It is *the* way to get a full copy of "the" blockchain for any given network[^cardano-node-purpose], in particular and most importantly, for **mainnet**. This process is called "syncing".

Having a local copy of the node is not absolutely essential for your daily work, but it can be useful in several cases:

 - Assessing a change for correctness and performance problems by using the entire multi-year history of Cardano as a test case.
 - Quickly gathering empirical statistics of quantities of interest, like minimum/average/maximum size/validation time/frequency of blocks.
 - Directly accessing any on-chain data, like the current protocol version, stake distribution or UTxO size.

### Disclaimer

 - The node is currently relatively resource-intensive[^resource-relative], see the minimum system requirements on the [node release page][].
    - It uses a lot of memory, and the memory requirements are growing as the chain evolves[^utxo-hd].
    - It inherently requires a lot of disk space (as of January 2024, 150GB).
    - During syncing, it is quite CPU-intensive (albeit mostly only on a single core).
      Once syncing is done, it is very light on CPU usage.
 - You do not need to run the node at all times.
   The stored blockchain can also be consumed by other developer tools that are independent of the node.
 - Syncing can take multiple days (full sync in December 2023 on a decently powerful server: 2 days).
   Unless your network connection is very slow compared to your CPU, the bottleneck is likely your CPU.
 - It is safe to cleanly interrupt syncing, eg via <kbd>Ctrl</kbd> + <kbd>C</kbd>.
   The node is able to gracefully resume on restart.

### Running the node

First, get a [cardano-node][] binary, see the README, eg via Nix, binary from the releases or compiling locally, depending on preferences and context.

A basic invocation looks like this:

```shell
./result/bin/cardano-node \
    run \
    --config /path/to/mainnet-config.yaml \
    --database-path /path/to/db-mainnet \
    --topology /path/to/mainnet-topology.json \
    --host-addr 0.0.0.0 --port 3001 \
    --socket-path /path/to/node.socket
```

 - `--config`: Get the default Cardano mainnet configuration file [here][cardano mainnet conf].
    - The logging system supports human-readable or JSON output (look for `ScText`/`ScJson`).
      `ScJson` sometimes contains more information, and it can be handy for automatic processing via eg `jq`.
    - Note that Genesis files are still mutable as long as their corresponding era is still in development (like Conway as of early 2024), so your configuration file might need changes once you update your node.
 - `--topology`: This file defines how/when the node will connect to other nodes.
   Get the default [here][cardano mainnet topo].

   While syncing, it makes sense to set the valency, ie the number of nodes sampled from the given DNS name to connect to, to `1` for better performance.[^genesis-syncing]

Make sure to store the log output somewhere.

### Syncing to mainnet

Now you are ready to start syncing to mainnet!

During this long process, you can stare at the log output, and eg look for lines containing `Chain extended` or `AddedToCurrentChain`, which (at least) mention the slot number of the most recently added block.
Bonus points for a plot of your sync progress over time!

### Syncing faster 🚀

As seen above, syncing is relatively slow, and you might be in a situation where you haven't caught up to mainnet in maybe a couple of months, but don't want to wait several hours.
Here are two options:

 - If do not mind a little extra clutter, it's possible to sync to mainnet in about two hours by using the [Mithril client][].
   See [here][Mithril instructions] for instructions.
 - Maybe someone you know has a very recent local copy of the mainnet ChainDB.
   If you are sure you *really* trust them, ask them to send you their ChainDB (or at least the missing parts) via eg [Magic Wormhole][].

## Understanding ledger snapshots and using db-analyser

You need a local ChainDB for this step, see the previous step.

[db-analyser][] is a tool maintained by the Consensus team to gather information and investigate problems (especially performance-related) by analyzing a local ChainDB, providing a variety of analysis passes.

### Ledger snapshots

A *ledger snapshot* is the CBOR serialization of the (extended[^extended-ledger-state]) *ledger state*, which represents the summary of all blocks up to a specific point[^glossary], the tip point or point of the last applied block, that is necessary to judge the validity of any block extending the last applied block.
Here, "validity" refers most prominently to the ledger rules ("Does this person actually have enough money to make that payment?") of the [Cardano ledger].
On disk, the file name of a ledger snapshot usually contains the slot number of its tip point.

### Running db-analyser passes

Most [db-analyser][] passes need a ledger snapshot for the specified starting slot.
Read the [db-analyser][] documentation up to the [the corresponding example][db-analyser snapshot] to create a ledger snapshot for slot `4492800`[^first-shelley-slot].

Starting from there (using `--analyse-from 4492800`), try the following two db-analyser passes:

 - `--show-slot-block-no --num-blocks-to-process 100`:
   You will see that many slots do not contain a block; our Consensus protocols involve a _leader schedule_[^glossary] that determines which slots are allowed to contain blocks.
   On average, there will be a block every $1/f$ slots, where $f$ is the _active slot coefficient_[^glossary].
   On mainnet, we have $f=1/20$.
 - `--benchmark-ledger-ops --num-blocks-to-process 1000`:
   Look for the `mut_headerApply` and `mut_blockApply` columns, which are the bulk of the time that is spent when validating headers and blocks, respectively; in order to get a rough sense for these durations, as they are relevant in lots of situations.

Feel free to run other passes that sound interesting to you.

### Reusing ledger snapshots

The node regularly creates ledger snapshots while running.
You can reuse these snapshots in db-analyser, and vice versa.

> ‼️ Note that the format of ledger snapshots can change with every new ledger release, so in order for this to work, make sure that your db-analyser and your node use the same ledger version.
> Some ways to achieve this:
>  - Click on the [release page][node release page] corresponding to your node, expand the "Individual packages' changelogs" section, and then check out the corresponding `ouroboros-consensus-cardano` release tag in the Consensus repo.
>  - If you used Nix to get the `cardano-node` binary, you can use `nix build .#db-analyser` to get a compatible db-analyser, assuming you are in the root of the `cardano-node` repo.
>
> You might also run into this when you update the node to a newer version.
> The node will automatically detect this situation, and create a new ledger state starting from Genesis.[^apply-vs-reapply]

Use a ledger snapshot of your local node (make sure to add the `_db-analyser` suffix[^snapshot-suffix]) to run the two db-analyser passes again, but now starting at the slot of your ledger snapshot.

 - The pattern of filled/unfilled slots will look very different to what you saw at the beginning of Shelley above.
   This is due to the usage of _transitional Praos_ (TPraos) at the beginning of Shelley, where the leader schedule gradually transitioned from being fully centralized to fully decentralized.
   It has been fully decentralized for several years now.
 - Comparing the benchmarking results for block validation times, you should see that validating a typical block takes much longer today.
   This is mainly due to larger allowed block sizes and this size being actually used, as well as more complex ledger rules (most prominently, Plutus smart contracts).

<!-- Footnotes -->

[^other-consensus-consumers]: Other important consumers include:

    - [cardano-cli][], a CLI tool allowing to query various information about the current state of a running node, as well as submitting transactions.
    - Indexers like [cardano-db-sync][] or [kupo][] store certain historical information (like old transactions) in an easily queryable format (in contrast to the node, which, by design, does not store information in ways that are unnecessary for its core responsibilities).
    - Other services or tools that need to communicate with nodes, like [hydra][] or [ogmios][].

[^cardano-node-purpose]: Note however that this is not its only purpose; it is also required to actively participate in the ongoing evolution of any Cardano-based blockchain.

[^utxo-hd]: The main reason for this is that the ledger state, ie the aggregated information necessary to validate blocks, is currently fully stored in memory.
The Consensus team is currently working on *UTxO HD*, a solution to move the ledger state to disk.

[^resource-relative]: Cardano is not a huge outlier in either direction, there are many examples for blockchains that are either much less resource-intensive (due to very low activity or new age, or due to very fancy cryptography, like Mina) or much more resource-intensive (due to very old age and large accumulated history, like Bitcoin, or a hyperfocus on performance, like [Solana](https://docs.solana.com/running-validator/validator-reqs#hardware-recommendations)).

[^genesis-syncing]: As of early 2024, syncing is a fully trusted process; if any node you are syncing from is adversarial, you might end up on an adversarial chain.
There is an ongoing effort to implement *Ouroboros Genesis* in order to reduce this strong trust assumption; in particular, it will involve reaching out to lots of nodes while syncing.

[^glossary]: See the [Glossary][].

[^extended-ledger-state]: A ledger snapshot also contains the _header state_ in addition to the ledger state.
As a rough idea, this is the state maintained by Consensus protocol (in our case: Praos), and much more lightweight than the ledger state both in size and in time it takes to update.
An "extended" ledger state is a ledger state plus the header state, but people sometimes/often drop the "extended" prefix, especially for ledger snapshots, as there are no "unextended" ledger snapshots.

[^first-shelley-slot]: This is the first "Shelley" slot, see [here](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0059/feature-table.md).
All previous slots were "Byron" slots, which is a legacy era that is largely irrelevant for our work, except in the very annoying case where it isn't.

[^apply-vs-reapply]: It will be much faster than syncing from scratch, as the blocks are only _reapplied_ instead of _applied_.
This means that certain checks (in particular, evaluating smart contracts) are skipped, which is sound as we know that the blocks were already applied/validated fully previously.

[^snapshot-suffix]: db-analyser only ever reads/writes snapshots with a `_db-analyser` suffix in the file name, eg `4492800_db-analyser`.
The node itself will create snapshots without a suffix (eg `4492800`), but can also read snapshots with any suffix.
Crucially, the node won't ever *delete* snapshots with a suffix, as opposed to unsuffixed ones, which are periodically garbage-collected, as well as deleted if the node can't decode them.

[cardano-node]: https://github.com/IntersectMBO/cardano-node
[cardano-cli]: https://github.com/IntersectMBO/cardano-cli
[cardano-db-sync]: https://github.com/IntersectMBO/cardano-db-sync
[kupo]: https://github.com/cardanosolutions/kupo
[hydra]: https://github.com/input-output-hk/hydra
[ogmios]: https://github.com/CardanoSolutions/ogmios
[node release page]: https://github.com/IntersectMBO/cardano-node/releases
[cardano mainnet conf]: https://github.com/IntersectMBO/cardano-node/blob/master/configuration/cardano/mainnet-config.yaml
[cardano mainnet topo]: https://github.com/IntersectMBO/cardano-node/blob/master/configuration/cardano/mainnet-topology.json
[Mithril client]: https://mithril.network/doc
[Mithril instructions]: https://mithril.network/doc/manual/getting-started/bootstrap-cardano-node
[Magic Wormhole]: https://github.com/magic-wormhole/magic-wormhole
[db-analyser]: https://github.com/IntersectMBO/ouroboros-consensus/blob/main/ouroboros-consensus-cardano/README.md#db-analyser
[Cardano ledger]: https://github.com/IntersectMBO/cardano-ledger
[Glossary]: https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/Glossary
[db-analyser snapshot]: https://github.com/IntersectMBO/ouroboros-consensus/blob/main/ouroboros-consensus-cardano/README.md#saving-a-snapshot
[preflight epic]: https://github.com/IntersectMBO/ouroboros-consensus/issues/887
