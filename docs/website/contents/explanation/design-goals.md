# Design Goals

The components that make up `ouroboros-consensus` were designed with the following goals in mind.

## Multiple Consensus Protocols

The design must support different consensus algorithms, requiring abstraction over the specific choice of consensus protocol.
From the project's inception, it was evident that multiple protocols would be required.
The Byron era of Cardano utilizes the PBFT protocol, while the Shelley era transitioned to TPraos, and Praos has been used since the Babbage era.
The consensus component must support not only the current era but also past eras, requiring the ability to compose protocols.
Additionally, we must ensure support for integrating new consensus protocols.

## Support for Multiple Ledgers

Similar to the need for multiple consensus protocols, our implementation must support multiple ledger implementations.
This is crucial for accommodating ledger changes as improvements are made.
Abstracting over the ledger implementation enables the consensus layer to work with various ledgers.

## Decoupling Consensus Protocol from Ledger

The consensus protocol is designed to be independent of any specific ledger implementation.
Since multiple ledgers (such as Shelley and its Shelley-based successors) can utilize the same consensus protocol (TPraos or Praos), the consensus protocol is defined based on what it expects or requires from the ledger rather than being tightly coupled to a specific one.
This approach makes the consensus protocol abstract and reusable across different ledgers.

## Testability

Ensuring the thorough testability of the consensus layer is a critical design goal.
As a core component of the Cardano Node, which manages the cryptocurrency, the consensus layer must adhere to strict correctness standards.
Currently, we extensively employ property-based testing.
Whenever possible, we should abstract over IO, enabling simulations of various failures (such as disk or network errors) to verify system recovery capabilities.
Additionally, to leverage the property-based methodology, tests must be relatively inexpensive.
The design should also support testing rare but expected scenarios (such as multiple slot leaders in Praos) by allowing overrides in protocol or ledger behavior at specific points.
Furthermore, the system should facilitate the isolation testing of individual components.

## Adaptability and Maintainability

The consensus layer was developed as a replacement for a [previous implementation](https://github.com/input-output-hk/cardano-sl), with the immediate goal of transitioning from Byron/BFT to Shelley/Praos while supporting future ledger and protocol changes.
This called for a flexible and adaptable design.
Abstracting both the consensus algorithm and the ledger plays a crucial role in achieving this.
Working with abstract interfaces prevents developers from making assumptions that may hold for one ledger but not others, avoiding costly fixes later.
This abstraction also allows the consensus layer to be reused in other blockchain projects.
Most importantly, an abstract design enables extensive testing with simpler mock ledgers, which are easier to set up and reason about compared to the complex real ledger.
Abstraction is considered good engineering practice, enhancing clarity, reducing dependencies, and making the system easier to understand and maintain.

## Composability

Given the complexity and scale of the consensus layer codebase, it is essential to divide it into smaller, manageable components that can be understood and modified independently.
Composability is a key technique employed to achieve this.
A prime example is the Hard Fork Combinator (HFC), which enables the combination of different consensus protocols (such as BFT and Praos) and ledgers (such as Byron and Shelley) into a unified composite protocol or ledger for the hybrid chain.

## Predictable Performance

This goal ensures that node operators can configure nodes for "normal circumstances" without the network failing during infrequent but expected events.
It aims to make node performance predictable, ensuring that the average-case scenario aligns with the worst-case scenario in terms of resource requirements—not only for security but also to maintain network stability with honest nodes.

## Protection Against DoS Attacks

The consensus layer must help safeguard the network against disruptions, making denial-of-service (DoS) attacks prohibitively expensive for adversaries.
This involves design decisions that prevent attackers from easily causing a node to perform extensive, wasteful computations.
For example, validating headers before downloading block bodies prevents attackers from forcing nodes to process potentially invalid blocks.
The design often seeks a balance between the cost for an attacker to induce work and the cost for a node to defend against it.
