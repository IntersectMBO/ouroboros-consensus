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

The `ConsensusProtocol` type class is indexed over the protocol type. The type family [`BlockProtocol`](https://github.com/intersectmbo/ouroboros-consensus/blob/c573f0639584623bd143f39e722340e412859aa1/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Block/Abstract.hs#L121) allows us to get the protocol associated to a given block. Note that, according to this definition, two blocks can be associated with the same protocol.

`CanBeLeader` and `IsLeader` are type families that define the requirements and proofs, respectively, related to a node's ability to produce a block:

- `CanBeLeader` represents the configuration data and keys a node needs to potentially be elected as leader. For instance, [in Praos](https://github.com/intersectmbo/ouroboros-consensus/blob/d014aae802159286bdc09bc4730966094d2d95dd/ouroboros-consensus-protocol/src/ouroboros-consensus-protocol/Ouroboros/Consensus/Protocol/Praos/Common.hs#L247), `PraosCanBeLeader` includes the operational certificate (OpCert), the cold verification key, and the VRF signing key.

- `IsLeader` represents cryptographic evidence or proof that a node is the leader for a specific slot. [For Praos](https://github.com/intersectmbo/ouroboros-consensus/blob/d014aae802159286bdc09bc4730966094d2d95dd/ouroboros-consensus-protocol/src/ouroboros-consensus-protocol/Ouroboros/Consensus/Protocol/Praos.hs#L242), this type would typically its VRF output, which constitutes the cryptographic proof demonstrating the node's right to lead in that slot.

Function `checkIsLeader` takes among its arguments a `CanBeLeader` value and the current slot (`SlotNo`). If the node is the leader for that slot, this function returns a `Just (IsLeader p)`. Otherwise it returns `Nothing`.

The `SelectView` type represents a summary of the header at the tip of a chain. It contains the necessary information for chain comparison. Values of this type allow the consensus protocol to choose between multiple competing chains by comparing these views.
In Praos, the `SelectView` is instantiated as [`PraosChainSelectView`](https://github.com/intersectmbo/ouroboros-consensus/blob/010b374c54f4d2f485ab114f702db6ec3b7a8f95/ouroboros-consensus-protocol/src/ouroboros-consensus-protocol/Ouroboros/Consensus/Protocol/Praos/Common.hs#L65), containing the necessary information for the chain selection process, namely:
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

### `ChainOrdering`

To agree on a single, linear, and eventually consistent chain of blocks we need to have a mechanism for ordering chains.

The abstract Consensus layer mainly relies on a `SelectView` taken from the tip of each header fragment.
[`ChainOrder`](https://github.com/intersectmbo/ouroboros-consensus/blob/010b374c54f4d2f485ab114f702db6ec3b7a8f95/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Protocol/Abstract.hs#L219) is a type class that defines how to compare these `SelectView`s.

Its main function is [`preferCandidate`](https://github.com/intersectmbo/ouroboros-consensus/blob/010b374c54f4d2f485ab114f702db6ec3b7a8f95/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Protocol/Abstract.hs#L262), which checks if a candidate chain is strictly better than the node’s current chain. This function is used during [initial chain selection](https://github.com/intersectmbo/ouroboros-consensus/blob/010b374c54f4d2f485ab114f702db6ec3b7a8f95/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Storage/ChainDB/Impl/ChainSel.hs#L180) and in [chain selection](https://github.com/intersectmbo/ouroboros-consensus/blob/010b374c54f4d2f485ab114f702db6ec3b7a8f95/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Storage/ChainDB/Impl/ChainSel.hs#L692).

`ChainOrder` also requires a total order on the `SelectView` type. This allows candidate chains to be [sorted](https://github.com/intersectmbo/ouroboros-consensus/blob/010b374c54f4d2f485ab114f702db6ec3b7a8f95/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Util/AnchoredFragment.hs#L126) for prioritization.

The `ConsensusProtocol` class provides a [`SimpleChainOrder`](https://github.com/intersectmbo/ouroboros-consensus/blob/010b374c54f4d2f485ab114f702db6ec3b7a8f95/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Protocol/Abstract.hs#L272) deriving helper, which implements `preferCandidate` using the standard `Ord` instance of `SelectView`. However, some protocols like Praos use more complex tiebreaking rules. [`ChainOrderConfig`](https://github.com/intersectmbo/ouroboros-consensus/blob/010b374c54f4d2f485ab114f702db6ec3b7a8f95/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Protocol/Abstract.hs#L227) allows different tiebreaking strategies to be used within the same protocol.

For a detailed discussion on chain ordering in Ouroboros, see [this section](#Chain-Ordering-in-Ouroboros).

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

### `ValidateEnvelope`

Envelope validation is a specific, initial phase of [header validation](https://github.com/intersectmbo/ouroboros-consensus/blob/4091b92226a7d5b0fd6531876722df32ea6b7f16/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/HeaderValidation.hs#L509) handled by the [`validateEnvelope`](https://github.com/intersectmbo/ouroboros-consensus/blob/4091b92226a7d5b0fd6531876722df32ea6b7f16/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/HeaderValidation.hs#L356) function.
It focuses on ledger-independent, basic structural checks and protocol compatibility of the block header.
The `validateHeader` function first executes the `validateEnvelope` checks.
If these pass, it then proceeds to update the chain-dependent state (`updateChainDepState`), where the more complex consensus-specific checks like VRF and KES validation occur.
This sequential approach allows for early rejection of malformed or fundamentally incompatible headers before more computationally intensive cryptographic validations are performed.

The `validateEnvelope` function performs the following checks:
• **Block Number Consistency**: Ensures that the actual block number of the new header matches the expected next block number based on the previous chain tip.
• **Slot Number Consistency**: Verifies that the actual slot number of the new header is greater than or equal to the minimum expected next slot number.
• **Previous Hash Matching**: Checks that the `headerPrevHash` of the new header correctly references the hash of the previous block in the chain.
• **Checkpoint Mismatches**: Validates against any configured [checkpoint](TODO: link to genesis) hashes for specific block numbers.
• **Additional Envelope Checks**: This is an extensible part that allows for block-type or protocol-specific.

For [Byron](https://github.com/intersectmbo/ouroboros-consensus/blob/4091b92226a7d5b0fd6531876722df32ea6b7f16/ouroboros-consensus-cardano/src/byron/Ouroboros/Consensus/Byron/Ledger/HeaderValidation.hs#L55), the additional envelope checks verify that [EBB](TODO: ref to EBBs)s occur only in allowed slots.

For [Praos](https://github.com/intersectmbo/ouroboros-consensus/blob/4091b92226a7d5b0fd6531876722df32ea6b7f16/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Protocol/Praos.hs#L111), additional envelope checks specifically verify:
- **Protocol Version Compatibility**: Ensuring that the major protocol version of the block does not exceed the maximum version supported by the node's configuration. If it does, it throws an `ObsoleteNode` error, preventing the node from processing blocks from a future, unsupported protocol version.
- **Header Size Limits**: Verifying that the block header's size is within a configured maximum limit.
- **Block Body Size Declaration**: Checking that the declared size of the block body (as indicated in the header) does not exceed the maximum allowed block body size.

The `HardForkBlock` also has an [instance](https://github.com/intersectmbo/ouroboros-consensus/blob/4091b92226a7d5b0fd6531876722df32ea6b7f16/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/HardFork/Combinator/Ledger.hs#L343) for `ValidateEnvelope`. Its `OtherHeaderEnvelopeError` is `HardForkEnvelopeErr`, which can encapsulate either a `HardForkEnvelopeErrFromEra` (an error from one of the constituent eras) or `HardForkEnvelopeErrWrongEra` (indicating a block from an unexpected era).
The `additionalEnvelopeChecks` for the `HardForkBlock` ensures that the block's era matches the expected era at the current tip, and then it delegates the check to the `additionalEnvelopeChecks` of the specific era's block type.

Envelope validation is largely ledger-independent, though the additional checks for Praos reference configured maximum sizes and protocol versions derived from the ledger configuration.

Envelope validation relies on [ValidateEnvelope](https://github.com/intersectmbo/ouroboros-consensus/blob/4091b92226a7d5b0fd6531876722df32ea6b7f16/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/HeaderValidation.hs#L340) instances. This class extends [`BasicEnvelopeValidation`](https://github.com/intersectmbo/ouroboros-consensus/blob/4091b92226a7d5b0fd6531876722df32ea6b7f16/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/HeaderValidation.hs#L297).

## The Extended Ledger State

The **extended ledger state** ([`ExtLedgerState`](https://github.com/intersectmbo/ouroboros-consensus/blob/c573f0639584623bd143f39e722340e412859aa1/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Extended.hs#L71) is a combination of two primary components:

- **Ledger State**
- **Header State**, which includes the [protocol state](#consensusprotocol)

Bundling these two states is not merely a matter of convenience—though it does help maintain consistency between them. This combination is essential because, to determine whether a block can extend the chain, we must [validate](#block-validity) both:

- The block itself, using the ledger rules
- The block header, using the protocol rules

Therefore, both the ledger state and the protocol state are required. The [block application function](https://github.com/intersectmbo/ouroboros-consensus/blob/c573f0639584623bd143f39e722340e412859aa1/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Extended.hs#L191) takes a (ticked) extended ledger state as an argument.

The `LedgerDB` is responsible for maintaining the `ExtLedgerState` at the chain tip and for the past `k` blocks. This is necessary to validate new blocks and handle potential forks. If the ledger and header states were stored separately, ensuring their consistency—especially during rollbacks or chain replays—would be significantly more complex.

The extended ledger state is also used in queries, meaning that the validity, interpretation, and results of those queries may depend on the consensus-specific header state.

The [`HeaderState`](https://github.com/intersectmbo/ouroboros-consensus/blob/c573f0639584623bd143f39e722340e412859aa1/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/HeaderValidation.hs#L185) is defined as a data structure containing:

- The **chain tip**, which includes:
  - Slot number
  - Block number
  - Additional `TipInfo` specific to the block type (e.g., whether it's an Epoch Boundary Block in Byron)
- The **chain-dependent state** of the consensus protocol ([`ChainDepState`](#consensusprotocol)), which:
  - Is protocol-specific
  - Is updated with new block headers
  - It can be rolled back

## Chain Validity

Checking for [chain validity](https://cardano-scaling.github.io/cardano-blueprint/consensus/chainvalid.html) in Cardano encompasses several stages, including time-based validity, header validity, and full block validity.

### Time-based Validity

The system must reject blocks from the **far future**, ie those whose slot [onset](#TODO-ref) is ahead of the local wall clock by more than the admissible clock skew. Such blocks are assumed not to have been minted by honest nodes.

However, blocks from the **near future**, ie blocks whose slot onset is ahead of the wall clock but within the admissible skew, are not immediately rejected. These blocks are assumed to potentially have been minted by honest nodes.
An artificial delay is introduced until their slot time is reached, preventing a node from processing a block before its actual slot onset.

This time-based check is primarily performed when headers are [received by the chain client](https://github.com/intersectmbo/ouroboros-consensus/blob/5785878d4db2500e137276569a63e5d57f80df50/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/MiniProtocol/ChainSync/Client.hs#L1509), as shown in [this section of the code](https://github.com/intersectmbo/ouroboros-consensus/blob/5785878d4db2500e137276569a63e5d57f80df50/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/MiniProtocol/ChainSync/Client.hs#L1728).

### Header Validity

Header validity is primarily implemented in the abstract consensus layer.
It is [performed by the `ChainSync` client](https://github.com/intersectmbo/ouroboros-consensus/blob/5785878d4db2500e137276569a63e5d57f80df50/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/MiniProtocol/ChainSync/Client.hs#L1937) when downloading headers from upstream peers.
The goal of this early validation is to prevent Denial-of-Service (DoS) attacks by quickly discarding invalid headers.

This process involves two main components:

- **Header envelope validation**: defined by the [`ValidateEnvelope`](#validateenvelope) class.
- **Consensus protocol validation**: implemented indirectly via the `updateChainDepState` function of the [`ConsensusProtocol`](#consensusprotocol) class.
The validity of a header is determined by examining its `ValidateView`.

### Block Validity

Full block validity involves both:

- Applying the block to the ledger state.
- Applying the header to the protocol state.

This validation is a critical part of the [chain selection](#chain-selection) process.

Although headers are already validated by the `ChainSync` client, they are re-validated during block application.
This re-validation is necessary to update the `ChainDepState` within the extended ledger state.
This step is expected to succeed, given that the header was previously validated by the `ChainSync` client.

## Chain Ordering in Ouroboros

The core rule in Ouroboros protocols (including Praos) is to prefer longer chains over shorter ones. This assumes that the honest majority of stake will produce denser chains.

- A chain that extends the current one is always preferred.
- If two chains are equally preferable, the node sticks with its current chain.

In Praos and Shelley-based eras, if chains have the same length, tiebreaking [rules](https://github.com/intersectmbo/ouroboros-consensus/blob/010b374c54f4d2f485ab114f702db6ec3b7a8f95/ouroboros-consensus-protocol/src/ouroboros-consensus-protocol/Ouroboros/Consensus/Protocol/Praos/Common.hs#L108) are applied in this order:

- 1. **Operational Certificate Issue Number**:
If both chain tips are from the same issuer and slot, the one with the higher `opcert` issue number is preferred.
This allows a stake pool operator to replace a compromised hot key with a new one and still have their blocks take precedence.

- 2. **VRF Tiebreaker**:
If the `opcert` check is inconclusive (which is common when two pools are elected in the same slot), the chain with the lower VRF value at its tip is preferred.
This avoids always picking the first block to arrive, which could encourage centralization to reduce latency (see ["The Frankfurt Problem"](https://github.com/IntersectMBO/ouroboros-consensus/blob/40d77fdd74a9b2b2a1d112a0b836b5cb8026c88c/ouroboros-consensus-protocol/src/ouroboros-consensus-protocol/Ouroboros/Consensus/Protocol/Praos/Common.hs#L227)). The VRF value used for tiebreaking (non-range extended) is uncorrelated to the leader VRF value and typically results in a uniformly random decision. Depending on the `ChainOrderConfig` for Praos, there are two [flavors](https://github.com/intersectmbo/ouroboros-consensus/blob/010b374c54f4d2f485ab114f702db6ec3b7a8f95/ouroboros-consensus-protocol/src/ouroboros-consensus-protocol/Ouroboros/Consensus/Protocol/Praos/Common.hs#L75) that determine when this tie-breaker comparison takes place.
    - `UnrestrictedVRFTiebreaker`: With this flavor, VRF tiebreakers are always compared. This has been the standard behavior for all eras before Conway.
    - `RestrictedVRFTiebreaker`: This flavor restricts the VRF tiebreaker comparison to situations where the slot numbers of the chain tips differ by at most a specified maximum distance (`maxDist`).
The primary motivation for this restriction is to favor blocks that were diffused earlier (in earlier slots) over those diffused later, even if the later block has a "better" VRF tiebreaker value. This aims to mitigate [issues](https://github.com/IntersectMBO/ouroboros-network/issues/2913) caused by poorly configured or resource-constrained pools that might diffuse blocks later.

### On the Transitivity of Praos Chain Ordering

Praos chain ordering is **not transitive**, regardless of the VRF tiebreaker flavor.

Consider the following select views where chains `A`, `B`, and `C` have the same length:

|                 | A | B | C |
|-----------------|---|---|---|
| Issuer          | x | y | x |
| Slot            | 0 | i | 0 |
| `opcert` number | 2 | j | 1 |
| VRF             | 3 | 2 | 1 |

Lower-case letters stand for arbitrary values (two letters designate the same value if and only if they are same letter).

In this example we have:

- `B` is preferred over `A`, since `B` has lower VRF than `A`.
- `C` is preferred over `B`, since `C` has lower VRF than `B`.
- However `C` is **not** preferred over `A`, since they have the same issuer and slot, and therefore we prefer the chain with the highest `opcert` number (2), therefore `A` is preferred over `C`.

Also, the `RestrictedVRFTiebreaker` flavour breaks the transitivity of chain ordering. To see this consider the following example where chains `D`, `E`, and `F` have the same length and different issuers, and assume `maxDist = 5` slots:

|              | D | E | F |
| ------------ | - | - | - |
| Slot         | 0 | 3 | 6 |
| VRF          | 3 | 2 | 1 |


We have that:
- `E` is preferred over `D`, since `E` has lower VRF than `D` and `|0 - 3| < 5`.
- `F` is preferred over `E`, since `F` has lower VRF than `E` and `|3 - 6| < 5`
- However, `D` is **not** preferred over `F`, but instead `D` and `F` are equally preferred since `|0 - 6| > 5` which implies that the VRF values of `D` and `F` are not used.

Despite the non-transitivity, the fundamental Consensus properties, such as [Common Prefix](TODO-ref!), are not affected. This is because the primary factor for chain selection for a [caught-up](TODO-ref!) node remains the chain length, with longer chains always being preferred. However, a non-transitive chain ordering brings the following complications:

- **Implementation**: The use of `sortBy` from base in `chain` selection, which is typically expected to work with transitive relations, could become a concern. While preliminary property tests suggest it works for the current non-transitive order, there's a theoretical risk that future GHC implementations might interact non-trivially.
- **Reasoning**: The non-transitive order can be very confusing for reasoning about anything related to chain order, as transitivity is an implicitly assumed property of orders.
This, in turn, leads to "obvious" properties failing to hold. For instance, the expectation that observing a node's selection over time yields a strictly improving sequence may not hold, as different observers could disagree on whether each selection is "strictly better" than the previous one. This non-objectivity can have practical effects, particularly for diffusion pipelining, which relies on a clear, consistent chain order.
- **Potential for Cycles**: The non-transitivity can conceptually give rise to "cycles" in preference, such as `A < B < C = A`. However, in practice, the node's implementation guarantees that it will never end up changing its selection in such a cycle because blocks already in the VolatileDB are not added again

See [this issue comment](https://github.com/IntersectMBO/ouroboros-consensus/issues/1075#issuecomment-3035911537) for potential approaches to restoring chain transitivity.

### On The History Of Chain Ordering In Cardano

The relevant changes to the chain order occurred in these PRs in chronological order:

 - [ouroboros-network#2108](https://github.com/IntersectMBO/ouroboros-network/pull/2108) added the opcert number comparison step as the first tiebreaker (before that, only chain length was used).
 - [ouroboros-network#2195](https://github.com/IntersectMBO/ouroboros-network/pull/2195) added the VRF tiebreaker, but lexicographically *before* the opcert number tiebreaker, in contrast to the status quo.
   This means that the order still was total at this point, also see below.
 - [ouroboros-network#2348](https://github.com/IntersectMBO/ouroboros-network/pull/2348) mostly did a change unrelated to the purpose of this document, but crucially, it swapped the order in which the VRF and opcert number comparison are done, introducing the current non-transitivity. Currently we do not prioritize the adoption of the node's own blocks. See [this comment](https://github.com/IntersectMBO/ouroboros-network/issues/1286#issuecomment-777614715), which mentions that this is not crucial for correctness, however the implications for incentives are still unclear.


## Chain Selection
