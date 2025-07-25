# Managing Updates

The Cardano blockchain was designed to evolve, allowing for the incorporation of new functionality, research advancements, and enhancements to security and performance over time.

Updating a blockchain system presents significantly greater challenges than a centralized one:
- Upgrades cannot be centrally enforced. All participants in a blockchain must eventually agree on the same set of rules and state, requiring meticulous coordination to prevent network partitions or 'forks'.
- Updates must take place without downtime or disruption.
- The update protocol must be engineered against adversarial behavior.
- Upgrades cannot invalidate past data (block and transactions).

Some updates focus solely on improving the internal working of the node without changing its external behavior. These changes
do not change to external interfaces, data encoding or consensus rules. They focus on performance, efficiency, or bug fixes. These updates do not require coordination among the nodes of the network, and can be applied independently by operators upgrading their node.

Other updates involve changes to specific configurable parameters of
the protocol that are agreed upon and enacted through on-chain
governance mechanisms. These are automatically activated in a coordinated manner, without requiring software updates. Examples include changes to the maximum block size or minimum transaction fees.

A third type of updates, described in [this page](#queries), involve backward-incompatible changes to the communication protocols or the encoding of data types exchanged between nodes, or between a node and a client.
Updates requiring a new node-to-node (N2N) or node-to-client (N2C) version do not necessarily require that all other nodes update their software or coordinate simultaneous upgrades for basic communication.
Instead, nodes running older versions can still communicate with those running newer versions through a process version negotiation.

The fourth kind of updates are those that introduce fundamental changes to the blockchain's core rules, leading to a hard fork.
These are often associated with the transition to a new ledger era (e.g., from Shelley to Alonzo or Babbage).
These updates modify the very consensus protocol or ledger rules that define the blockchain's state and block validity.
Unlike mere parameter changes, they can alter how blocks are produced, validated, or how the chain progresses.
Each of these major updates marks a hard fork and the transition to a new Cardano era.

The Hard Fork Combinator[^1] (HFC) is a core component within Cardano's consensus layer that enables the seamless composition and management of these different eras into a single, logical blockchain.

## The Hard Fork Combinator

The Hard Fork Combinator allows different blockchain eras to be combined into a single, unified blockchain type, facilitating smooth transitions between these eras.
This design choice is fundamental to Cardano, which has evolved through several distinct eras, each with potentially different rules and parameters.

The HFC essentially enables the sequential composition of various consensus protocols and ledgers, such as Byron (using Permissive BFT) and Conway (using Praos), into a cohesive "hybrid" chain.

To ensure seamless era transitions, the HFC must orchestrate the complex process of moving from one blockchain era to the next, ensuring the network can continue operating despite significant changes in rules and parameters. To this end, the HFC must address several key problems.

- Era Transitions:
The HFC is must determine when a blockchain should transition from one era to the next.

- Ledger State Evolution:
As a system like Cardano transitions into a new era, the ledger state must be translated to comply with the ledger state format and rules of the subsequent era.
This translation is needed because each era can introduce significant alterations to the blockchain's rules and the underlying structure of its ledger state.
For instance, when transitioning from Babbage to the Conway era, several parts of the ledger state changed to enable the new on-chain governance features.

- Translation of Data Among Eras:
Beyond the ledger state itself, the HFC must facilitate the translation and compatibility of various data types across eras. The UTxO set of the ledger state may contain transaction outputs (txouts) from later eras, which to avoid a  prohibitively costly full UTxO set translations on era boundaries, must be translated on-demand.
Queries and serialization also vary era by era.

- Perform time translations:
Translating between UTC time, slot numbers, and epoch numbers is not trivial because different eras can have different slot duration.
For example, the Byron era had 20-second slots, while all subsequent eras use one-second slots.
Consequently, time/slot conversions are dependent on the ledger state and cannot be predicted arbitrarily far into the future if the era transition is not yet known.

The following sections describe how the HFC addresses these challenges.

### Era Transitions

### Ledger State Evolution

### Data Translation

### Time Translations

[^1]: The recordings of Edsko de Vries' presentations at the Weekly IOG Seminar([1](https://drive.google.com/file/d/1m_jKQM_gxBm0ctLqIq9NGj5_nXPI66Su/view),[2](https://drive.google.com/file/d/1QIJ-VBlj-txB6K6E7DIEnY5TzaD89qQm/view)) provide a good introduction to the topic.
