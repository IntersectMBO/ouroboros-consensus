# Hard Forks in Cardano

## Introduction

This document details how the Cardano node handles a [hard fork](https://en.wikipedia.org/wiki/Fork_(blockchain)#Hard_fork), and why it handles hard forks that way.

Cardano mainnet is intended to hard fork when and only when the major component of the [`protocol version`](https://github.com/IntersectMBO/cardano-ledger/blob/2ff5fa4e8c6b773748799981ef44ef6a62cd5c92/libs/cardano-ledger-core/src/Cardano/Ledger/Core/PParams.hs#L350) changes.
The protocol version can only change due to on-chain governance [^updating-protver], and in practice the major protocol version must only ever increase by one [^PVP-style].

- The requirement that a major protocol version increment implies a hard fork is merely the traditional semantics of the major component of a version, ie _backwards-incompatibility_ (aka _breaking_ change).

- The requirement of on-chain governance for each hard fork means that --- as of the arrival of decentralized governance to the Cardano mainnet chain --- it's the community that decides when to transition to the next iteration of the protocol rules and/or ledger rules, ie when to abandon any nodes that have not yet updated their software in preparation for the hard fork.

There are other Cardano networks besides mainnet, notably the testnets.
By their essential nature, though, testnets should also use mainnet's mapping between each major protocol version number and the corresponding pair of protocol rules and ledger rules.
On the other hand, these other networks must be able to hard fork at different times than mainnet does --- the testnets should have hard forked successfully before mainnet hard forks!
Since the testnet governance and mainnet governance are independent, the determination of the hard fork timing via on-chain governance immediately enables it to vary on different networks' chains.

Beyond those fundamentals, the community of Cardano developers must also decide how to implement the changes that underly a hard fork, how to organize them within the nodes' specifications and implementations [^multiple-nodes].

## Concrete Dynamics

A hard fork in Cardano proceeds in phases.

- *Implementation*.
  Cardano developers introduce some backwards-incompatible changes to the protocol and/or ledger rules in the node software [^change-decisions].
- *Release*.
  As a decentralized network, it is impractical to rely on all Cardano node operators simultaneously updating their software.
  Therefore, a new release of the software implements the new rules _and also_ the old rules.
  That release continues to use the old rules --- even for fresh blocks --- until the community ultimately chooses to switch to the new rules by increasing the major protocol version parameter via on-chain governance [^frame-rule].
- *Adoption*.
  It is crucial that the community does not increment the major protocol version until a vast majority of (honest) stake has upgraded their nodes to the new software.
  Stake pool operators should therefore not vote to increment before upgrading their nodes to the new software.

Because the protocol version is determined on-chain, it's perfectly plausible for divergent chains to increment the major version at different times.
The Cardano node ought to handle that scenario gracefully, if only to bound degradation during atypical scenarios (eg low participation within the limits of Praos's _dynamic availability_ feature).
However, branching realities are disruptive even within blockchain communities, so the current design additionally ensures that even temporarily divergent chains (aka _short forks_) will agree about when to increment the major version as long as mainnet enjoys high participation from its honest majority of stake.

## Eras

When Cardano developers design and implement the changes for a hard fork, they determine whether the changes are easy to maintain as variations within the existing code.
If so, the hard fork is ultimately implemented by some conditionals within the code that branch on the major protocol version.
If the changes are not so simple, the Cardano developers instead introduce a new _era_ within the code [^roadmap-disambiguation].
(See "Which changes require a new era?" below --- this determination is done within the context of the existing implementation's architecture, not a vacuum.)

In the intended usage, each era defines some new types and rules that are independent within the code from the existing eras.
For example, the [Conway era introduces](https://docs.cardano.org/about-cardano/evolution/upgrades/chang) many new parts of the ledger state, transaction types, corresponding rules, etc that the preceding era does not have, and so the architects decided to explicitly separate the Conway code from the old code as a new era.
The Ledger and Consensus infrastructure code is defined so that each era can flexibly reuse whichever of the older eras' features it does not supersede.

Several eras coexist sequentially on the Cardano chain; a hard fork that takes one step within that sequence is called an _era transition_.
Every intentional hard fork is either an era transition or an _intra-era hard fork_ [^accidentals].
Intra-era hard forks only introduce changes with a small effect on the code, eg a few easy-to-maintain conditionals.
Some have been bugfixes that were small by chance, and others have been changes that were only small because they had been anticipated and intentionally deferred when the era was originally designed.

Eras are a straight-forward mechanism that enables node developers to separate the concerns arising from features that are added to or removed from Cardano over time.
The separation amongst eras inherently simplifies the development of the new features, and it also helps ensure that the Cardano node can sync the entire historical chain [^mithril] --- as well as refuse invalid alternative histories.

### Which changes require a new era?

It's ultimately the judgment of the node developers whether some changes should be achieved with an era transition rather than an intra-era hard fork.
In other words, eras are not the only to organize non-trivial backwards-incompatible changes in Cardano.
However, only a single node implementation has existed for several years now, and aspects of its existing design make some backwards-incompatible changes prohibitively costly (ie redesigns) unless they are delineated by an era transition.

The Cardano developers have so far used very expressive static typing in order to provide the safety guarantees that Cardano is known for.
It's plausible that an implementation that instead used a language more suited to dynamic types could more frequently use intra-era hard forks rather than era transitions or avoid the concept of eras altogether and rely solely on logic that branches on the major protocol version.
However, we strongly believe that overall development and assurance would significantly degrade without the benefits of such precise types.

The first era transition (from Byron to Shelley) is a notable example.
It was the first era transition we experienced, and no subsequent transition changes nearly so much.
The two code bases are drastically different, so it's hard to argue this should have been an intra-era hard fork.
In particular, this is the only era transition that changed the duration of slots and the number of slots in an epoch.
In the current codebase, any hard fork that changes those per-era constants must be an era transition, since the Consensus infrastructure was explicitly designed with that restriction as a simplifying assumption.
A different design might relax that constraint, but it has not been restrictive so far: a second change to these constants has been discussed only rarely and never actually proposed, and deferring them until whatever justifies the next era transition wouldn't obviously be prohibitively disruptive.

### How wide-reaching are eras?

Eras are ultimately an implementation detail of the node, a consequence of architectural trade-offs.
The node's behaviors currently reflects the eras in only two ways.

Specifically, the node must be able to forecast the data necessary to validate a useful prefix of some header chain from the intersection of that header chain and the node's current selection, including whether each header arrived too early.

- The familiar and intuitive notion of era is useful for organization in general, and so the eras' names might appear within parameter names in configuration files, specification documents, announcements, etc.

  However, the era transitions themselves should generally be considered as "just a big hard fork".
  The foundational data is instead the protocol version, especially because not all hard forks are era transitions.
  (See [this GitHub comment](https://github.com/IntersectMBO/ouroboros-consensus/issues/416#issuecomment-2669347315) for the details of how this lesson was learned.)

- Some codecs reflect Cardano's specific era structure.
  That's not strictly necessary, but a tagged union approach has made it trivial to combine the era-specific codecs into a full Cardano codec.
  Specifically, top-level codecs fundamentally branch on a simple number located near the beginning of the bytestring and dispatch accordingly to the corresponding era's codecs.

  *Remark*.
  These eras tags are not present in any codecs for bytes that determine hashes used on chain.
  For example, suppose the only difference between some transaction's serialization in two different eras is the numeric value of the era tag.
  In that case --- unless the eras use different hashing functions --- both serializations would be assigned the same transaction ID, since the function that assigns transaction IDs is applied to the bytes inside of the envelope that carries the era tag.

## Forecasting, forewarning

Hard forks can affect the validity of block headers, and so must be sufficiently forecastable.
For this reason, hard forks must not happen less than 36 hr (on mainnet) after the voting deadline, ie hard forks must have at least 36 hrs of forewarning --- just like other protocol parameter updates that affect headers (eg maximum block size).

TODO how to argue that hard forks or era transitions needs more than that?

[^updating-protver]: *Remark*.
  Prior to Conway, the protocol version was just another protocol parameter; see Section 5 "Protocol Parameters" of the "A Formal Specification of the Cardano Ledger, Deliverable SL-D5" document for more details --- its latest revision can be accessed via the hyperlink in the "Formal Specification" column of the "Shelley" row of the documentation table at the top of the [README](https://github.com/IntersectMBO/cardano-ledger/blob/master/README.md) of the [`cardano-ledger` repository](https://github.com/IntersectMBO/cardano-ledger).
  As of Conway's [CIP-1694](https://github.com/cardano-foundation/CIPs/tree/master/CIP-1694) implementation, the protocol version has special treatment compared to other protocol parameters, since it so clearly requires a preceding software upgrade.
  The logic is the same at a high-level, but the details are tuned to ensure a hard fork could only ever surprise a very small amount of stake.

[^PVP-style]: *Remark*.
  The Cardano protocol version has two components, so-called major and minor.
  The lesser component has always been zero on mainnet.
  We might eventually see a use for it, but at the moment the only plausible idea we have is for both components to track backwards-incompatible changes, as in the [Haskell Package Versioning Policy](https://pvp.haskell.org/), eg.

[^multiple-nodes]: *Remark*.
  There has only been one Cardano node so far, but others are now in development.
  The different nodes _must_ be able to exchange messages, but it is not yet clear how the multiple teams will cooperate on decisions such as how to specify and implement some backwards-incompatible change.
  Cardano Improvement Proposals (aka [CIPs](https://cips.cardano.org/)) will certainly be involved in such alignment efforts but are not necessarily sufficient.

[^change-decisions]: *Remark*.
  At time of writing, these are almost always going to be (possibly indirectly) related to a Cardano Improvement Proposal (aka [CIP](https://cips.cardano.org/)).

[^frame-rule]: *Clarification*.
  There's no other technical reason for the community to increment the major protocol version.

[^roadmap-disambiguation]: *Disambiguation*.
  The [Cardano Roadmap](https://roadmap.cardano.org/en/) also defines some very broad "eras": Byron, Shelley, Goguen, Basho, and Voltaire.
  These are more general than the eras that divide the actual evolution of the protocol rules and ledger rules within the node.
  [CIP-0059](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0059/feature-table.md) maintains a table showing the correspondence; it lists roadmap eras in the "Phase" column.

[^accidentals]: *Clarification*.
  Accidental hard forks (eg due to a bug) would necessarily arise within a single era, but they are not _intra-era hard forks_ because they weren't gated by a protocol version change.

[^mithril]: *Clarification*.
  Mechanisms such as [Mithril](https://github.com/input-output-hk/mithril) would provide alternatives to the node, but the node itself should always be able to revalidate (or merely reapply) all the blocks if needed/desired.
