In this technical report, we describe the necessary components comprising the consensus layer of a Cardano blockchain node. The main goal of this report is to provide guidance to software engineers that intend to implement the Consensus layer of a Cardano node from scratch.

# Introduction

# Single-era Consensus Layer

This section describes the components of the Consensus layer of the Cardano Node, as if Cardano only ever had one era. While this assumption greatly simplifies the implementation of the Consensus layer, one must keep that if such an assumption is made, in may not be straightforward or even possible to implement a node that is capable of syncing with Cardano mainnet. However, we still argue that this section is useful as an educational material.

We need to take both advantage of the rigorous structure we have in the code base, but at the same time take care not to expose the non-Haskell target audience to the full power of abstraction it provides. We can achieve it by instantiating the abstractions we have at concrete types. We may also want to monomorphise, i.e. remove the typeclass constraints as much as possible after instantiating them. We may then proceed to obfuscate the Haskell syntax as something else.

The key typeclass we need to instantiate is ConsensusProtocol.

## Outline of the Consensus components

The section 5.2 of the `network-design` document can be imported into the whitepaper almost wholesale. It gives a good outline of the tasks that the consensus layer is supposed to be able to perform.

## Interaction with the Networking layer

Here we need to talk about what parts of the networking layer Consensus relies on. What is are the interfaces between the Consensus and Network? The answer is, as I understand, typed mini protocols. How to describe these interfaces in a language-agnostic way?

The implementations of the mini-protocols are here: `ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/MiniProtocol`

## Storage Subsystem

This section should describe the concepts necessary to implement the storage subsystem of a Cardano node. The description should focus on things that are necessary to keep track of to implement i.e. Praos, but do not go into the details about how these things are stored. No need to discuss in-memory vs on-dist storage and mutable vs persistent data structures, as we have separate documents for this.

# Multi-era Considerations

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

When replaying the chain in `ouroboros-consensus`, as I understand, we use the same code of the HardFork combinator that is used when actually producing/validating these blocks. This is nice because we do not have to have separate code path for the historic chain and caught-up protocol participation.

But obviously we don not have to use the HFC. What would be a reasonable way to enable a non-HFC node to catch up with the chain? Would this hypothetical node be able to enjoy code reuse for the historic and non-historic blocks? In the worst case, we should be able to hard-code the historical eras.

## The Hard Fork Combinator: a uniform way to support multiple eras}

Ideally, this would be a short section that should outline the core ideas behind the HFC without a single line of Haskell. The purpose should be to demonstrate the benefits of having an abstract interface for describing mixed-era blocks. The interested reader would be then referred to an extended, Haskell-enabled document that described the HFC in its full glory.
