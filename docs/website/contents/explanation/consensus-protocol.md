# Consensus Protocol

The [Consensus protocol](https://cardano-scaling.github.io/cardano-blueprint/consensus/index.html) is a foundational element for Cardano and other blockchains. It provides the mechanism through which a single, linear, and eventually consistent chain of blocks is established among all network participants. The consensus layer acts as an orchestrator, mediating between the network and ledger layers.

Two core responsibilities of a consensus protocol are:

* **Chain Selection**: The process of choosing between multiple competing chains. The Chain Database (ChainDB) component within a node is responsible for performing the final stages of chain selection, by either extending the current chain or adopting a new one.

* **Block Production**: The consensus protocol determines when a node is allowed to mint a block, via leader schedule. When the Ouroboros protocol designates a particular node (operating a stake pool) as the slot leader, that node is expected to extend the best chain it has seen by minting a new block.
The consensus layer decides when to contribute to the chain by producing blocks.

The Consensus layer of `ouroboros-consensus` implements all past and current protocols in the Cardano chain. When the consensus layer was re-implemented, it initially supported Permissive BFT (Byzantine Fault Tolerance) during the Byron era, and then transitioned to Ouroboros TPraos for Shelley. Starting with the Babbage era, the Consensus layer supports the Praos protocol. The [`ouroboros-consensus-protocol`](https://github.com/IntersectMBO/ouroboros-consensus/tree/main/ouroboros-consensus-protocol) package defines the Praos and TPraos (Transitional Praos) instantiations. The [`ouroboros-consensus-cardano`](https://github.com/IntersectMBO/ouroboros-consensus/tree/main/ouroboros-consensus-cardano) package integrates these protocols into Cardano.

## Classes

The general implementation of a consensus protocol is abstracted into the following classes.

### `ConsensusProtocol`

The [**`ConsensusProtocol`**](https://github.com/intersectmbo/ouroboros-consensus/blob/d014aae802159286bdc09bc4730966094d2d95dd/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Protocol/Abstract.hs#L66) class is a central abstraction within the Ouroboros consensus layer, generalizing across specific consensus algorithms.

Among its responsibilities, the `ConsensusProtocol` class defines the protocol state required to run the protocol. This "protocol state" is represented by the `ChainDepState` type family. This state is updated when new block headers arrive, and is subject to rollback. In particular, in the Praos consensus protocol, the `ChainDepState` includes "nonces", which are random numbers derived from the chain itself, used as seeds for pseudo-random number generators in leader selection. This state is also ["ticked"](./ledger-interaction#ticking) to apply time-related changes, such as nonce rotation at certain slot numbers, even before a new block is processed.

`CanBeLeader` and `IsLeader` are type families that define the requirements and proofs, respectively, related to a node's ability to produce a block:

- `CanBeLeader` represents the configuration data and keys a node needs to potentially be elected as leader. For instance, [in Praos](https://github.com/intersectmbo/ouroboros-consensus/blob/d014aae802159286bdc09bc4730966094d2d95dd/ouroboros-consensus-protocol/src/ouroboros-consensus-protocol/Ouroboros/Consensus/Protocol/Praos/Common.hs#L247), `PraosCanBeLeader` includes the operational certificate (OpCert), the cold verification key, and the VRF signing key.

- `IsLeader` represents cryptographic evidence or proof that a node is the leader for a specific slot. [For Praos](https://github.com/intersectmbo/ouroboros-consensus/blob/d014aae802159286bdc09bc4730966094d2d95dd/ouroboros-consensus-protocol/src/ouroboros-consensus-protocol/Ouroboros/Consensus/Protocol/Praos.hs#L242), this type would typically its VRF output, which constitutes the cryptographic proof demonstrating the node's right to lead in that slot.

Function `checkIsLeader` takes among its arguments a `CanBeLeader` value and the current slot (`SlotNo`). If the node is the leader for that slot, this function returns a `Just (IsLeader p)`. Otherwise it returns `Nothing`.

The `SelectView` type represents a summary of the header at the tip of a chain. Values of this type allow the consensus protocol to choose between multiple competing chains by comparing these views.
In Praos, the `SelectView` is instantiated as `PraosChainSelectView`, containing the necessary information for the chain selection process, namely:
- The length of the chain.
- The slot number of the block.
- The verification key of the block's issuer
- A counter for blocks issued by a specific issuer within a KES period.
- A Verifiable Random Function (VRF) output, used for tie-breaking.

The `LedgerView` type is a projection or summary of the ledger state,
which the Consensus protocol requires for tasks such as leadership
checks or transaction size limits for blocks. This information, like
the stake distribution, must be computable for slots in the near
future. The `LedgerView` is used when [ticking](./ledger-interaction#ticking) the `ChainDepState` (`tickChainDepState`) to apply time-related changes. It can also be [forecast](./ledger-interaction.md#forecasting-and-the-forecast-range) for future slots, meaning we can predict that the `LedgerView` will be, irrespective of which blocks are applied in the given forecast range.

[In Praos](https://github.com/intersectmbo/ouroboros-consensus/blob/d014aae802159286bdc09bc4730966094d2d95dd/ouroboros-consensus-protocol/src/ouroboros-consensus-protocol/Ouroboros/Consensus/Protocol/Praos/Views.hs#L41), the `LedgerView` contains:
- The stake pool distribution, which is used by `checkIsLeader`.
- Maximum allowed size for a block header and body.
- The current ledger protocol version.

The `ValidateView` type represents a projection of a block header that is used for validating said header.
This view is used when `updateChainDepState` and `reupdateChainDepState` functions are called to advance the protocol's state based on a new header.

[In Praos](https://github.com/intersectmbo/ouroboros-consensus/blob/d014aae802159286bdc09bc4730966094d2d95dd/ouroboros-consensus-protocol/src/ouroboros-consensus-protocol/Ouroboros/Consensus/Protocol/Praos/Views.hs#L22), the `ValidateView` of a header contains:
- The verification key of the block's issuer (stake pool cold key).
- The VRF verification key, which is checked against the registered VRF key for the stake pool.
- The VRF output, which serves as a cryptographic proof that the issues is eligible to produce a block for that slot, and contributes to the evolving nonce.
- The operational certificate, which delegates rights from the stake pool's cold key, to the online KES key.
- The block's slot number.
- The hash of the previous block.
- The KES signature data, which is used to verify the issuer's identity.

Function `protocolSecurityParam` extracts the [security parameter](TODO!) `k` from the consensus protocol's static's configuration.

### `LedgerSupportsProtocol`

The [`LedgerSupportsProtocol`](https://github.com/intersectmbo/ouroboros-consensus/blob/a70eb17ef28831cd2e140b33ded49ce791028d88/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/SupportsProtocol.hs#L25) type class links the consensus protocol to the ledger. It defines what information the consensus layer requires from the ledger state to perform its operations, particularly for leader selection and forecasting future ledger states.

- [Byron instance](https://github.com/intersectmbo/ouroboros-consensus/blob/a70eb17ef28831cd2e140b33ded49ce791028d88/ouroboros-consensus-cardano/src/byron/Ouroboros/Consensus/Byron/Ledger/Ledger.hs#L282)
- [Shelley instance](https://github.com/intersectmbo/ouroboros-consensus/blob/a70eb17ef28831cd2e140b33ded49ce791028d88/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/SupportsProtocol.hs#L51).
- [`HardForkBlock` instance](https://github.com/intersectmbo/ouroboros-consensus/blob/a70eb17ef28831cd2e140b33ded49ce791028d88/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/HardFork/Combinator/Ledger.hs#L399).

### `BlockSupportsProtocol`

The [`BlockSupportsProtocol`](https://github.com/intersectmbo/ouroboros-consensus/blob/a70eb17ef28831cd2e140b33ded49ce791028d88/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Block/SupportsProtocol.hs#L26) type class links the specific block type to its corresponding consensus protocol. It defines the capabilities a block must provide for the consensus protocol to operate correctly.

- [Byron instance](https://github.com/intersectmbo/ouroboros-consensus/blob/a70eb17ef28831cd2e140b33ded49ce791028d88/ouroboros-consensus-cardano/src/byron/Ouroboros/Consensus/Byron/Ledger/PBFT.hs#L42).
- [Shelley instance](https://github.com/intersectmbo/ouroboros-consensus/blob/a70eb17ef28831cd2e140b33ded49ce791028d88/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Protocol.hs#L32).
- [`HardForkBlock` instance](https://github.com/intersectmbo/ouroboros-consensus/blob/a70eb17ef28831cd2e140b33ded49ce791028d88/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/HardFork/Combinator/Protocol.hs#L141).
