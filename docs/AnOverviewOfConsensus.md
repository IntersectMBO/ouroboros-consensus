# An Overview of consensus

## Very High-Level Motivation

At a very high level, a net of Cardano nodes should exhibit the following
concrete behaviors.

  * The net should diffuse transactions throughout the net as rapidly as
    possible.

  * The net should diffuse blocks throughout the net as rapidly as possible.

  * Whenever the Ouroboros protocol specifies that (a stake pool operating) a
    particular node should lead, that node should extend the best chain it has
    seen so far by minting a new block, which should contain as many
    transactions as possible.

  * The net should be very difficult to disrupt. For example, a successful
    denial of service attack should be prohibitively expensive to enact.

The primary data are _blocks_ and _transactions_. The possible contents of both
of those are primarily constrained by the ledger rules. The Consensus Layer and
[Network Layer][ouroboros-network] together
implement a _filtering forwarding network_. Specifically, the Consensus Layer
determines which blocks propagate between neighboring nodes: those on the _best_
chain. The IOG researchers have established that the Ouroboros protocol can be
used to ensure that -- unless an adversary controls more than half of the net's
stake -- the honest nodes will all continually reach _consensus_ regarding the
selection of a single best chain and that that chain grows over time.

The Consensus Layer defines the core Consensus components and logic, notably the
Ouroboros protocol.

## The Neighbors of Consensus

The Consensus Layer integrates the Consensus core with the Network Layer and the
[Ledger Layer][cardano-ledger]. We therefore work closely with the Network
and Ledger teams.

The [Network Layer][ouroboros-network] manages the nodes' connections to its
neighbors. So it ultimately provides communication channels to the Consensus
Layer, while the Consensus Layer reports back to it if a neighbor has misbehaved
etc. The Network Layer also provides the library used to define the Consensus
Layer's _client_ and _server_ state machines that let each connected pair of
nodes exchange messages according to the various _mini protocols_ (cf
[typed-protocols][typed-protocols] package).

The Consensus Layer uses the ledger rules to validate blocks and transactions
and apply them in order to maintain _the ledger state_. The Consensus Layer in
turn needs the ledger state in order to determine when a node is allowed to mint
a block, ie the _leader schedule_.

The primary use of the Consensus Layer is the [Cardano node][cardano-node]. This
is what IOG actually deploys on the live Cardano mainnet. Such a node runs the
full Ouroboros protocol and mints new blocks. Secondary uses include the
[Cardano wallet][cardano-wallet] and [Cardano DB sync][cardano-db-sync], which
connect to a proper node and only follow and _trust_ its chain selection. For
example, these uses involve problem-specific queries of the latest ledger state.

[ouroboros-network]: https://github.com/input-output-hk/ouroboros-network
[cardano-ledger]: https://github.com/input-output-hk/cardano-ledger
[typed-protocols]: https://github.com/input-output-hk/typed-protocols
[cardano-node]: https://github.com/input-output-hk/cardano-node
[cardano-db-sync]: https://github.com/input-output-hk/cardano-db-sync
