# Introduction

The Cardano protocol will change over time.
The community innovates applications and updates feature priorities, researchers refine Ouroboros, and developers improve the code.

This document describes how Cardano accomodates these changes as a community, as a software engineering project, and as a distributed system.

## The Protocol Version

Each change to the Cardano protocol advances through the following steps before influencing the network's chain.

- Someone (you!) specifies a change that is backwards-incompatible but worthwhile.
  For example, it could be a [Cardano Improvement Proposal](https://cips.cardano.org/) (CIP).
- The Technical Steering Committee and Ouroboros researchers address the worth, safety, and future consequences of the proposed solution.
- Cardano developers prepare a software release that includes the changes but only enables them for chains on which the protocol version has been incremented.
- The community uses on-chain governance to increment the protocol version, thereby putting the change into effect.
  (Stake pool operators must not vote to increment the protocol version before updating their software!)

Cardano philosophy mandates that the community has the final say regarding if and when to adopt some protocol change.
They accordingly control the final step above: developers determine the exact meaning of each protocol version, but only the community increments the protocol version on mainnet.

It is crucial that the community does not increment the protocol version before enough nodes have upgraded their software.
Each protocol version increment corresponds to a _hard fork_, because the changes are backwards-incompatible: nodes running the old code will not be able to adopt blocks minted by the new code.
Even if some block content happened to be valid in both the old and the new protocol, each software release explicitly refuses any block that (extends a ledger state that) has a protocol version greater than that release was intended to implement — see the MaxMajorPV constant.

Even software released well after mainnet increments the protocol version must implement the protocol both with and without that increment's changes.
The Cardano node must correctly process blocks with older protocol versions, eg when catching up to mainnet for the first time or after being offline for a long time.
So every protocol change must remain conditional within all future releases, instead of only within the particular release that mainnet used for that increment of the protocol version.
(Exceptions are possible if the community decides to somehow truncate/compress a prefix of the historical chain.)

## When to Increment the Protocol Version?

Fundamentally, the protocol version must be incremented for some change to the protocol when an old node and a new node that received the same messages might disagree on which block is the best valid block, aka maintain consensus.
A more precise and actionable answer will be given after introducing more context.

### The Cardano Abstraction Ladder

For the sake of this discussion, Cardano fixes a ladder of abstractions between the following two extrema.

- *Opacity* (bottom rung).
  The Cardano node exchanges bytestrings with peers in the network and with local users (eg wallets).
  At any given time, the node can determine whether such a bytestring is valid, and that predicate depends on previously exchanged bytestrings and the passage of time.

- *Transparency* (top rung).
  Users interact with Cardano nodes to interpret the current state of the network and to evolve it according to their goals.

An analogous spectrum applies to any computer system that humans interact with, and there are longstanding traditions of how to organize the rungs between these two.
The Cardano node is internally organized as the following ladder.
The overall intent is for every Cardano use case to be satisfied by exchanging messages with nodes, so messages are the focus of some foundational rungs.

- Opaque Rung (bottom).
- CBOR Rung.
    - The _generic data model_ of [RFC 8949 Concise Binary Object Representation](https://cbor.io/).
- Message Data Rung.
    - A data model for the content of Cardano messages that is sufficiently abstract for Cardano developers to easily interpret them in a consistent and useful way (eg algebraic data types).
    - Note that this content includes transaction IDs, transactions, block headers, blocks, queries, query replies, etc, but not the node's internal state [^state-data-model].
    - Some notable fields within this data model are hashes computed in terms of the CBOR Rung.
- Message Validity Rung.
    - The key difference from the Message Data Rung is that this rung excludes messages with invalid content.
      Users usually only consider valid messages, and the honest node should certainly never send invalid messages.
    - In practice, this rung requires the honest node to maintain as state a summary of all relevant messages that is sufficient to determine which messages to send [^theory-coalgebra].
    - Query replies are an awkward fit for this rung, since the node only ever sends them without ever receiving them.
      But the honest node must only send valid messages, which in this case means a correct reply.
    - Another important category of messages the honest node must send are messages that indicate which chain it has selected.
      This rung also therefore must resolve Praos's inherent short forks.
- There are other rungs, such as the rung that abstracts from the set of nodes to the network as a whole, but they are beyond the scope of this document.
- Transparent Rung (top).

### The Cardano Message Data Onion

The Message Data Rung is organized as the layers of an onion.
So are some other rungs, but their structure is beyond the scope of this document.

- [Ledger Layer](https://github.com/IntersectMBO/cardano-ledger/).
    - The notion of _era_ is an organizing concept within the Ledger Layer --- more on this below.
    - For each era, the content of its blocks, transactions, queries, and query responses.
- [Consensus Layer](https://github.com/IntersectMBO/ouroboros-consensus/).
    - The content of the few queries and their responses that either do not depend on the ledger era or else regard when prior era transitions happened, which the Ledger Layer does not record.
    - An additional envelope wrapping block headers, blocks, transactions, era-specific queries, and era-specific query responses.
      The envelope indicates a particular era via an _era tag_ [^era-tag-parse], which affects the validity of the object but not its identifying hash.
- [Network Layer](https://github.com/IntersectMBO/ouroboros-network) (outermost) [^mux-layer].
    - ChainSync, BlockFetch, TxSubmission, LocalStateQuery, etc; see "Chapter 3 Mini Protocols" within the [Ouroboros Network Specification](https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf) [^network-pdf-link].
    - The content of each mini protocol message, excluding the payload.

### Precise Answer

The protocol version must be incremented if an old node and a new node would otherwise disagree on any of the following.

- *Chain Validity Predicates*, the validity predicates within the Message Validity Rung for block headers and blocks.
- *Chain Order*, the [partial order](https://en.wikipedia.org/wiki/Partially_ordered_set) within the Message Validity Rung that determines one chain is better than another.
  Tiebreakers are a gray area, since Praos does not inherently rely on them.
- *Chain Data Model*, the parts of the Message Data Rung used to define the Chain Validity Predicates and the Chain Order.
    - A change to the data model of the node's maintained state --- as opposed to its messages --- would not inherently trigger this.
    - However, if such a change were meaningful (ie not just a refactor/optimization/etc), then some Chain Validity Predicates would necessarily also change.
- *Chain Codecs*, the parts of the mapping from the CBOR Rung to the Chain Data Model that affect the hashes therein.
  Two nodes won't even be able to exchange transactions, block headers, and/or blocks if they disagree on their identifying hashes.

The most common change in Cardano's protocol so far has been the addition of a new kind/variant of transaction, which triggers all of these except the Chain Order.
The Chain Order has only changed from Praos's length to Ouroboros Genesis's length-soon-after-the-intersection, which is slight.
On the other hand, the tiebreaker has changed, and out of caution those changes were bundled up with unrelated changes that independently required a protocol version increment.

It's also worth noting that the Consensus Layer and Network Layer of the Message Data Rung can change without incrementing the protocol version.
The most common example is that queries can be changed without triggering any of the above, since queries do not contain any of the Chain Data Model.
Another example is that the structure of the Consensus envelope could change.
Or the mini protocols --- excluding their payload --- could change.
All such changes are conditional on the per-connection version negotiated by the Handshake mini protocol instead of the protocol version.

## Ledger Eras

Any Cardano node implemented in a strongly-typed language is likely going to declare distinct types for Chain Data Models that differ between protocol versions; more precise types yield stronger assurances from the typechecker.
For the sake of such implementations, the ledger introduces a new era whenever the Chain Data Model changes.
The [sequence of eras](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0059/feature-table.md) at the time of writing are: Byron [^byron-halves], Shelley, Allegra, Mary, Alonzo, Babbage, and Conway.

A Cardano node implementation or other tooling implemented in a language prone to dynamic typing might find it convenient to ignore the era tags for the most part.
However, the era tags must still be validated according to the intended function from Cardano protocol versions to Ledger eras, since a strongly-typed node would fail to accept a block with the wrong era tag.

Protocol changes that do not also introduce a new era are known as _intra-era hard forks_.
These are certainly sound if they only change the Chain Validity Predicate, but they generally could do more.
But so far, Cardano has forbidden intra-era hard forks from changing the Chain Codecs.
This could be allowed in either of two ways.

- *NoEnvelope*.
  The Consensus envelope could be entirely eliminated if an object's slot could be always be determined even without knowing the protocol version.
  The slot could then be used to determine the appropriate protocol version, based on the node's state.
  The downside here is significant: programs that do not maintain the node's state (since that is costly to do) would not necessarily be able to parse block headers, blocks, and transactions, since they wouldn't necessarily know the mapping from slot to protocol version (since it's determined by on-chain governance).
- *HideEras*.
  The Consensus envelope could store the protocol version instead of the era tag.
  At that point, the era tag would be an internal implementation detail of strongly-typed node implementations, and could even freely differ between them.
  The only downside is that the Consensus envelope's byte size might grow slightly, but it'll still be dwarfed by the Ledger Layer portion of the wrapped block header/block/transaction.
    - Notably, the Ledger already maintains an invariant that the CBOR of UTxO are forward-compatible, so the storage of each UTxO does not need to include an era tag.
      That's the only object in the system that might be of comparable size as the era tag and numerous enough to incur significant costs.
    - The code would also need to store the mapping from protocol version to era tags, but that is known at compile-time --- there is the slight complication that the mapping might vary between chains (eg testnets), but they already tend to have different configuration files.

It might be plausible for the NoEnvelope option to restrict stateless programs to assume they're downstream of a node that had annotated the blocks and/or transactions with the correct protocol version.
At that point, though, this option is basically the same as HideEras, except that nodes would exclude the Consensus envelope when exchanging with other nodes.

[^state-data-model]: *Remark*.
  The state maintained by the Message Validity Rung also has a data model, but, in contrast to messages, it is an internal implementation detail of the node.
  Different node implementations could have different state data models, but still implement the same protocol, ie send/receive/accept/reject the exact same messages.

[^theory-coalgebra]: *Remark*.
  In theory, on the other hand, that summary state could merely be the sequence of valid received and sent messages along with their timestamps.

[^era-tag-parse]: *Remark*.
  In particular, it's the only part of the parsed object for which the codec does not depend on the era.

[^mux-layer]: *Disclaimer*.
  This Network Layer of the Cardano Message Data Onion actually contains an additional layer, the Mux Layer, but that is transparent to the rest of system and beyond the scope of this doucment.

[^network-pdf-link]: *Note*.
  If that link breaks, the document can usually be obtained via the "The Shelley Networking Protocol" hyperlink in the `README` file of the [`ouroboros-network` repository](https://github.com/IntersectMBO/ouroboros-network).

[^byron-halves]: *Historical Note*.
  Byron originally had two halves (Ouroboros Classic with Epoch Boundary Blocks versus round-robin with no EBBs), but today's node does not differentiate the two.
  It uses the same Chain Validity Predicate for both: the Permissive BFT protocol, which essentially ignores any EBBs.
