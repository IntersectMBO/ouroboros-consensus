---
sidebar_label: 'Overview'
sidebar_position: 1
---

# What is Ouroboros Consensus?

_Ouroboros_ is the name of a family of _Proof-of-Stake_ consensus protocols which provides foundation for Cardano and other blockchains. _Distributed Consensus_ is the mechanism through which a single, linear, eventually consistent, _chain of blocks_ is established among all participants of the network.  This
website aims to provide a comprehensive and detailed technical
documentation about the implementation of Ouroboros which is at the
heart of Cardano.


:::info

This documentation is a work-in-progress, please feel free to raise
issues and contribute on
[GitHub](https://github.com/input-output-hk/ouroboros-consensus)
repository should you find missing or inaccurate information.

:::

## Very High-Level Motivation

At a very high level, a net of Cardano nodes should exhibit the following
concrete behaviors.

  * The net should diffuse transactions throughout the net as rapidly as
    possible.

  * The net should diffuse blocks throughout the net as rapidly as possible.

  * Whenever the Ouroboros protocol specifies that (a stake pool operating) a
    particular node should lead, that node should extend the best chain it has
    seen so far by minting a new block, which should contain as many valid
    transactions as possible.

  * The net should be very difficult to disrupt. For example, a successful
    denial of service attack should be prohibitively expensive to enact.

The primary data are _blocks_ and _transactions_. The possible contents of both
of those are primarily constrained by the ledger rules. The Consensus Layer and
[Network Layer](https://github.com/input-output-hk/ouroboros-network) together
implement a _filtering forwarding network_. Specifically, the Consensus Layer
determines which blocks propagate between neighboring nodes: those on the _best_
chain. The IOG researchers have established that the Ouroboros protocol can be
used to ensure that -- unless an adversary controls more than half of the net's
stake -- the honest nodes will all continually reach _consensus_ regarding the
selection of a single best chain and that that chain grows over time.

The Consensus Layer defines the core Consensus components and logic, notably the
Ouroboros protocol. See [References](References).

## The Neighbors of Consensus

The Consensus Layer integrates the Consensus core with the Network Layer and the
[Ledger Layer](https://github.com/input-output-hk/cardano-ledger). We therefore work closely with the Network
and Ledger teams.

The [Network Layer](https://github.com/input-output-hk/ouroboros-network) manages the nodes' connections to its
neighbors. So it ultimately provides communication channels to the Consensus
Layer, while the Consensus Layer reports back to it if a neighbor has misbehaved
etc. The Network Layer also provides the library used to define the Consensus
Layer's _client_ and _server_ state machines that let each connected pair of
nodes exchange messages according to the various _mini protocols_ (cf
[typed-protocols](https://github.com/input-output-hk/typed-protocols) package).

The Consensus Layer uses the ledger rules to validate blocks and transactions
and apply them in order to maintain _the ledger state_. The Consensus Layer in
turn needs the ledger state in order to determine when a node is allowed to mint
a block, ie the _leader schedule_.

The primary use of the Consensus Layer is the [Cardano node](https://github.com/input-output-hk/cardano-node). This
is what IOG actually deploys on the live Cardano mainnet. Such a node runs the
full Ouroboros protocol and mints new blocks. Secondary uses include the
_Cardano wallet_ and [Cardano DB sync](https://github.com/input-output-hk/cardano-db-sync), which
connect to a proper node and only follow and _trust_ its chain selection. For
example, these uses involve problem-specific queries of the latest ledger state.
