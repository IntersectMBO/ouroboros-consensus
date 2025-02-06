In this document, we describe the necessary components comprising the Consensus layer of a Cardano blockchain node. The main goal of this report is to provide guidance to software engineers that intend to implement the Consensus layer of a Cardano node from scratch.

This document is a work in progress. We strive to provide a set of requirements for and responsibilities of the Consensus layer that is agnostic of a particular implementation. However, we still very much are informed by the Haskell implementation in [ouroboros-consensus](https://github.com/IntersectMBO/ouroboros-consensus) as it is the only one.

# Introduction

# What does Consensus and Storage need to do: responsibilities and requirements

## Responsibilities of the Consensus Layer

```mermaid
flowchart TD
    A("Consensus") --> B("Mint Blocks")
    A("Consensus") --> C("Select From Candidate Chains")
    A("Consensus") --> D("Accept transactions into Mempool")
    A("Consensus") --> E("Store and Serve Historical Chain")


    B("Mint Blocks") --> B1("Ask Ledger to Validate Transations")

    C("Select From Candidate Chains") --> A1("Fetch Chains from Peers")
```

The Consensus layer is responsible to choose among the different chains that might
co-exist on the network. The candidate chains may arise both from honest and adversarial participation.
The Consensus layer must implement the consensus protocol or the current Cardano era to replicate the blockchain across all Cardano nodes.

Also for the new blocks to include meaningful data, Consensus has to receive and maintain a list of submitted transactions that are not in the chain yet.

Blocks can be theoretically forged every slot (e.g. every second on Cardano mainnet), so the Consensus layer must be
extremely fast in producing the blocks once it knows it has to do so.

In terms of security, Consensus tries to not expose meaningful advantages for adversaries that could trigger a worst-case situation in which the amount of computation to be performed would block the node.
This is generally achieved by trying to respect the following principle:

- The cost of the worst case should be no greater than the cost of the average
  case.

| Component                          | Responsibility                                                                     |   |   |
|:-----------------------------------|:-----------------------------------------------------------------------------------|:--|:--|
| Forging Loop                       | Extend the Cardano chain by minting new blocks                                     |   |   |
| Forging Loop                       | Block forging must be as fast as possible, to not delay later diffusion  block.    |   |   |
| Mempool                            | Maintain a list of valid transactions to include in new blocks                     |   |   |
| ChainSync, BlockFetch              | Establish means of gathering chains from peers                                     |   |   |
| ChaineSel                          | Among all forks that can be fetched from peers, must identify the best chain.      |   |   |
| N2N TxSubmission, N2C TxSubmission | Accept transactions from peers and store them into Mempool  for further processing |   |   |
|                                    |                                                                                    |   |   |
|                                    |                                                                                    |   |   |
|                                    |                                                                                    |   |   |

TODO(georgy): ChainSync, BlockFetch, N2N TxSubmission, N2C TxSubmission --- aren't those part of network? Do we need to describe them here? How do we put the emphasis right?

## Responsibilities of the Storage Layer

In order to switch to an alternative chain, Consensus needs to evaluate the
validity of such a chain. In order to evaluate the validity of a block, we need to have a Ledger state at the
predecessor block. As applying blocks is not an inversible operation, this is usually solved by maintaining the last `k` ledger states in memory.


| Component   | Responsibility                                                                                            |   |   |
|:------------|:----------------------------------------------------------------------------------------------------------|:--|:--|
| ImmutableDB | Store definitive blocks on disk and provide iterator access to them for new syncing peers.                |   |   |
| VolatileDB  | Store non-definitive blocks on disk, provide them to peers (random access?),                              |   |   |
|             | efficiently switch to a different suffix of the chain if needed                                           |   |   |
| LedgerDB    | Maintaining the last `k` (2160 on Cardano mainnet) ledger states in memory to facilitate chain selection. |   |   |
|             |                                                                                                           |   |   |

## Requirements imposed onto the Networking/Diffusion layer

To maximize the probability of the block being included in the definitive chain,
the Consensus layer has to strive to mint new blocks on top of the best block
that exists in the network. Therefore it necessitates of a fast diffusion layer
for blocks to arrive on time to the next block minters.

Transmit chains as fast as possible so that blocks arrive to the next forger in time.

In Cardano, transactions are distributed twice: once as pending transactions that exist outside of a block and again when the transaction is directly included within some minted block.

A block is distributed once among the caught-up nodes when it's minted, and then potentially any number of times later to nodes that are trying to join/catch back up to the network after this block was minted.

In today's Cardano network, moreover, block headers diffuse before their blocks, so that nodes only download blocks they prefer in that moment.

# Single-era Consensus Layer

## Outline of the Consensus components

### ChainSync client

Each ChainSync client maintains an upstream peer's candidate chain.

The protocol state is also independently maintained alongside each candidate chain by ChainSync.

#### Details

It disconnects if the peer violates the mini protocol, if the peer sends an invalid header, or if switching to their chain would require rolling back more than kcp blocks of the local selection.

*TODO*(georgy): what is kcp here?

It's able to their validate headers past the intersection with the local selection because the parts of the ledger state necessary to validate a header were completely determined some positive number of slots ago on that header's chain; see _forecasting_ within the ledger rules and _snapshots_ within the ledger state.

ChainSync blocks while the candidate chain is past the forecast range of the local selection.
The forecast range must be great enough for the peer to rescue the local node from the worse-case scenario.
The Praos paper bounds this to needing at most kcp+1 headers from the peer, which that paper also bounds to requiring at most scg slots of forecast range.
The Genesis paper is also satisfied by a forecast range of scg.
(The ChainSync server is much simpler than the client; see _followers_ below.)

### ChainSync server

ChainSync server provides an iterator into the ChainDB for downstream peers to be able to download headers and blocks.

#### Details

Moreover — because the node must serve the whole chain and not only the historical chain — each ChainSync server actually uses a _follower_ abstraction, which is implemented via iterators and additionally supports the fact that ChainSel might have to rollback a follower if it's already within kcp of the local selection's tip.
(Even the pipelining signaling from ChainSel to ChainSync clients is implemented via a follower, one that follows the so-called _tentative chain_ instead of just the actual local selection.)

### BlockFetch client and client coordinator

The client-side of the BlockFetch mini-protocol comprises the client itself and the centralised logic that coordinates multiple clients.
Based on the set of all candidate chains and the local selection, the centralized BlockFetch logic (one instance, no matter how many peers) decides which of the candidate chains to fetch and which particular blocks to fetch from which peers.
It instructs the corresponding BlockFetch clients (one per upstream peer) to fetch those blocks and add them to the ChainDB.
The client disconnects if the peer violates the mini protocol or if it sends a block that doesn't match (eg different hash) the requested portion of the snapshot of its candidate chain that lead to that request.

### BlockFetch server

The BlockFetch server uses a mere iterator instead of a follower because each fetch request is so specific; their only corner case involves garbage collection discarding a block while a corresponding fetch request is being served.

### ChainSel

The ChainDB's ChainSel logic persists each fetched block to the ChainDB and then uses that block to improve the local selection if possible.
(No other component persists blocks or mutates the selection, only ChainSel.)
Improving the selection requires validation of the fetched block (and maybe more, if blocks arrived out of order).
If the fetched block is invalid, ChainSel disconnects from the upstream peer who sent it, unless that block may have been pipelined; see the specific pipelining rules.
In turn, if a fetched block should be pipelined, ChainSel signals the ChainSync servers to send that header just before it starts validating that block.
If it turns out to be invalid, ChainSel promptly signals the ChainSync servers to send the corresponding MsgRollBack.

The combined ledger and protocol state is maintained alongside the local selection by ChainSel, so that blocks can be validated.

### ChainDB

A Praos node must not introduce unnecessary delays between receiving a block and forwarding it along.
It is therefore an important observation that the ChainDB does not require the Durability property of ACID: upstream peers will always be available to replace any blocks a node loses.

### LedgerDB

In both ChainSel and ChainSync, rollbacks require maintenance of/access to the past kcp+1 states, not only the tip's state — access to any such state must be fast enough to avoid disrupting the semantics of the worst-case delay parameter Delta assumed in the Praos paper's security argument.

In addition to validation in ChainSel and ChainSync, these ledger states are how the node handles a fixed set of queries used by wallets, CLI tools, etc via the LocalStateQuery mini protocol.

### Mempool & TxSubmission

The Mempool maintains a sequence of transactions that could inhabit a hypothetical block that is valid and extends the current local selection.
The Mempool is bounded via a multi-dimensional notion of size such that it never contains more transactions than could fit in N blocks.
Peers synchronize their Mempools via the TxSubmission protocol.
This mini protocol leverages the fact that the Mempool is a sequence as opposed to an unordered set; a simple integer suffices as the iterator state for the TxSub client.
(Recall that transactions flow from client to server, since the orientation is determined by the flow of blocks and pending transactions naturally flow opposite of blocks.)

### The Mint aka Block Forge

Whenever the wall clock enters a new slot, the node checks whether its configured minting credentials (if any) were elected by the protocol state and forecasted ledger state to lead this slot.
If so, it mints a block that extends its local selection (or its immediate predecessor if the tip somehow already occupies this slot or greater) and contains the longest prefix of the Mempool's transactions that can fit.
That block is then sent to ChainSel, as if it had been fetched.

When (if) the node selects that block, the Mempool will react as it does for any change in the local selection: it discards any transactions that are no longer valid in the updated hypothetical block the Mempool continuously represents.
Because every Ouroboros transaction _consumes_ at least one UTxO, the transactions in a newly minted and selected block will definitely be discarded.

## Interaction with the Networking layer

Here we need to talk about what parts of the networking layer Consensus relies on. What is are the interfaces between the Consensus and Network? The answer is, as I understand, typed mini protocols. How to describe these interfaces in a language-agnostic way?

The implementations of the mini-protocols are here: `ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/MiniProtocol`

## Storage Subsystem

This section should describe the concepts necessary to implement the storage subsystem of a Cardano node. The description should focus on things that are necessary to keep track of to implement i.e. Praos, but do not go into the details about how these things are stored. No need to discuss in-memory vs on-dist storage and mutable vs persistent data structures, as we have separate documents for this.

## Some Important Optimizations

Both both blocks and transactions can be applied much more quickly if they are known to have been previously validated.
For blocks, the outcome will be exact same, since they can only validly extend a single chain.
A transaction, though, might be validated against a different chain/ledger state than the first time, so many checks still need to happen.
But many "static" checks don't need to be repeated, since they'd fail regardless of the ledger state.

Despite block reapplication being much faster than initial validation, the node should not need to reapply their entire historical chain whenever it is restarted.
The node instead occasionally persists its oldest ledger state (ie the kcp+1st).
On startup, the node only needs to deserialize that snapshotted ledger state and then replay the best chain amongs the persisted blocks it has that extends this ledger state in order to re-establish its kcp+1 ledger states.

# Multi-era Considerations

Ledger rules and consensus protocols evolve over time.
In Cardano, the on-chain governance ultimately decides when to adopt backwards-incompatible changes, by incrementing the major component of the protocol version.
The first such _era_ transition switched to the Praos protocol.

| Responsibility           | Timing                                             | Description                                                                                          |   |   |
|:-------------------------|:---------------------------------------------------|:-----------------------------------------------------------------------------------------------------|:--|:--|
| Process historical chain | slot number of the era boundaries known statically | switch and translate between the statically known historical sequence of revisions of block formats, |   |   |
|                          |                                                    | ledger rules and protocols                                                                           |   |   |
| Transition to a new era  | slot number of the era boundary unknown            | during the era transition:                                                                           |   |   |
|                          |                                                    | * switch the consensus protocol, block format, ledger rules                                          |   |   |
|                          |                                                    | * translate transactions received from prevoios-era peers                                            |   |   |
|                          |                                                    | into the format of the current era for them included in a new block                                  |   |   |

## Approaches to handle protocol changes

With the blokchain network evolving, the block format and ledger rules are bound to change. In Cardano, every significant change starts a new "era". There are several ways to deal with multiple eras in the node software, associated here with some of the DnD alignments:

* Chaotic Evil: the node software only ever implements one era. When the protocol needs to be updated, all participants must update the software or risk being ejected from the network. Most importantly, the decision to transition to the new era needs to happen off-chain.
Pros: the simplest possible node software.
Cons: on-chain governance of the hard-fork is impossible, as the software has no way of knowing where the era boundary is and does not even have such a concept. Additionally, special care is needed to process history blocks: chain replay is likely to be handled by special code, separate from the active era's logic.

* Chaotic Good: the node software supports the current era and the next era. Once the next era is adopted, a grace period is allowed for the participants to upgrade. The decision to upgrade may happen on chain.
Pros: allows for on-chain governance of the hard fork.
Cons: supporting two eras is more difficult than one: higher chances of bugs that will cause the network to fork in an unintended way. Like in the previous case, special care is needed to process historic blocks.

* True Neutral: software is structured in such a way that is supports all eras.
Pros: enables massive code reuse and forces the code to be structured in the way that allows for abstract manipulation of blocks of different eras. The on-chain governance of hard-forks is reflected in the code, and ideally in the types as well, making it more likely that unintended scenarios are either impossible or easily discover able through type checking and testing.

Cons: abstraction is a double-edged sword and may be difficult to encode in some programming languages. Engineers require extended onboarding to be productive.

## Replaying history of multiple eras

Cardano has the peculiarity of being a multi-era network, in which at given
points in the chain, new backwards-incompatible features were added to the
ledger. Consensus, as it needs to be able to replay the whole chain, needs to
implement some mechanism to switch the logic used for each of the eras, in
particular the Ledger layer exposes different implementations for each one of
the ledger eras.

## The Hard Fork Combinator: a uniform way to support multiple eras}

*Hard Fork Combinator (HFC)* is mechanism that handles era transitions, including changes to ledger rules, block formats and even consensus protocols. It automates translations between different eras and provides a minimal interface for defining specific translations that obey rigorous laws. The HFC was primarily introduced to handle the fundamental complexity that arises when the translation between wall clocks and slot onsets depends on the ledger state (since on-chain governance determines when the era transition happens).
It also handles the comparatively simple bookkeeping of the protocol, ledger, codecs, and so on changing on the era boundary — ie a variety of blocks, transactions, etc co-existing on the same chain in a pre-specified sequence but not according to a pre-specified schedule.
Lastly, it handles the fact that ticking and forecasting can cross era boundaries, which requires translation of ledger states, protocol states, and pending transactions from one era to the next.
The HFC cannot automatically infer the implementation of these necessities, but it automates as much as possible against a minimal interface that requires the definition of the specific translations etc.

| Component           | Responsibility           |                                                    | Description                                                                                          |   |   |
|:--------------------|:-------------------------|:---------------------------------------------------|:-----------------------------------------------------------------------------------------------------|:--|:--|
| HFC-historical-eras | Process historical chain | slot number of the era boundaries known statically | switch and translate between the statically known historical sequence of revisions of block formats, |   |   |
|                     |                          |                                                    | ledger rules and protocols                                                                           |   |   |
| HFC-new-era,HFC-tx  | Transition to a new era  | slot number of the era boundary unknown            | during the era transition:                                                                           |   |   |
|                     |                          |                                                    | * switch the consensus protocol, block format, ledger rules                                          |   |   |
|                     |                          |                                                    | * translate transactions received from prevoios-era peers                                            |   |   |
|                     |                          |                                                    | into the format of the current era for them included in a new block                                  |   |   |

# Glossary
