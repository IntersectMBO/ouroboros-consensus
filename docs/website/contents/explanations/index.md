# Introduction

Welcome to the documentation for the [`ouroboros-consensus`](https://github.com/IntersectMBO/ouroboros-consensus) repository.
This repository houses the Haskell implementation of three crucial components utilized by the [`cardano-node`](https://github.com/IntersectMBO/cardano-node): Consensus, Storage, and Mempool.
- The [Consensus component](https://cardano-scaling.github.io/cardano-blueprint/consensus/index.html) implements the [Ouroboros](https://www.iog.io/papers/ouroboros-a-provably-secure-proof-of-stake-blockchain-protocol) family of Proof-of-Stake protocols.
- The [Storage component](https://cardano-scaling.github.io/cardano-blueprint/storage/index.html) is responsible for providing efficient access to the blockchain data, as well as maintaining the current and recent past ledger states, and storing ledger state snapshots.
- The [Mempool component](https://cardano-scaling.github.io/cardano-blueprint/mempool/index.html) serves as a buffer for valid transactions that are waiting to be included in a block. It is used by the Consensus component when forging a block and by the Network layer's transaction submission mini-protocol to diffuse transactions among nodes.

A core design principle in the implementation of these components is the abstraction from specific ledger and protocol implementations.
The aim is to decouple the consensus protocol from the ledger and to support multiple consensus algorithms and ledgers for improved adaptability and maintainability.
This design allows different ledgers (like the Byron or Shelley ledgers) and different Ouroboros protocol instances (like Praos or TPraos) to be integrated into the abstract consensus framework.
To reflect this design, the repository is structured into different sub-repositories.
- The polymorphic implementations and abstract classes, which define the core consensus logic independently of specific ledger or protocol details, can be found in the [`ouroboros-consensus`](https://github.com/IntersectMBO/ouroboros-consensus/tree/main/ouroboros-consensus) sub-directory.
- The Cardano specific instantiations, which provide the concrete implementations, reside in the [`ouroboros-consensus-cardano`](https://github.com/IntersectMBO/ouroboros-consensus/tree/main/ouroboros-consensus-cardano) sub-directory.
