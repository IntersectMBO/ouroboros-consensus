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
[GitHub](https://github.com/IntersectMBO/ouroboros-consensus)
repository should you find missing or inaccurate information.

:::

## Very High-Level Motivation

The Consensus component allows a network of Cardano node to agree on a single (block) chain.
To this end, this component implements the Ouroboros family of consensus protocols.

Another functional requirement is that the network should diffuse blocks and transactions as rapidly as
possible. Also the network should be very difficult to disrupt. For example, a successful denial of service attack should be prohibitively expensive to execute.
The Consensus layer must contribute to these goals.

Whenever the Ouroboros protocol specifies that (a stake pool operating) a particular node should lead, that node should extend the best chain it has seen so far by minting a new block.
A newly forged block should contain as many valid transactions as possible.

The primary data handled by this component are _blocks_ and _transactions_, the validity of which is determined by the ledger rules.
The Consensus layer in combination with the [Network layer][ouroboros-network] implement a _filtering forwarding network_.
Specifically, Consensus determines which blocks propagate between neighboring nodes (those on the _best_ chain).

The Ouroboros research papers that formalize the different protocols (such as Praos) contain proofs that:
- the honest nodes will all continually and eventually agree on what the best chain is (unless an adversary controls more than half of the network's stake).
- the best chain grows over time.

<!-- xrefcheck: ignore link -->
The Consensus Layer defines the core Consensus components and logic, notably the
Ouroboros protocol. See [additional material](../additional_material).

## The Neighbors of Consensus

The Consensus Layer integrates the Consensus core with the [Network Layer][ouroboros-network] and the
[Ledger Layer](https://github.com/IntersectMBO/cardano-ledger). We therefore work closely with the Network
and Ledger teams.

The Network Layer manages the nodes' connections to its
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

The primary use of the Consensus Layer is the [Cardano node](https://github.com/IntersectMBO/cardano-node). This
is what IOG actually deploys on the live Cardano mainnet. Such a node runs the
full Ouroboros protocol and mints new blocks. Secondary uses include the
_Cardano wallet_ and [Cardano DB sync](https://github.com/IntersectMBO/cardano-db-sync), which
connect to a proper node and only follow and _trust_ its chain selection. For
example, these uses involve problem-specific queries of the latest ledger state.

[ouroboros-network]: https://github.com/IntersectMBO/ouroboros-network
