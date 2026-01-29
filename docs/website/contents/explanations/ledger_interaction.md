# Interaction with the Ledger Layer

The `ouroboros-consensus` implementation is intimately related to the
`cardano-ledger` implementation. Consensus uses the ledger implementation as a
pure function for validating transactions and blocks.

The `cardano-ledger` implementation makes a clear distinction between Byron and
the rest of the eras, much like the Consensus layer does. All the other eras are
derivatives of the Shelley era, and as such, we will refer to them as _Shelley
eras_. The Byron implementation is encapsulated in `cardano-ledger-byron`.



Some of the basic types used in all the ledger implementations for Shelley eras
are also encapsulated in `cardano-ledger-core`.

## Small-steps transition system

The Ledger implementation is based on the
[`small-steps`](https://cardano-ledger.cardano.intersectmbo.org/small-steps/)
library, which exposes the function `applySTS` that uses the class `STS` to
define the state, signal, environment, logged events and failures that a rule
has. It follows the [operational
semantics](https://en.wikipedia.org/wiki/Operational_semantics) described in the
different specification documents listed in their README.  Evaluating a ledger
rule is essentially running an `STS` transition that produces a new state or
throws failures.

The relation between rules can be seen in section 14 of [the Shelley
spec](https://github.com/intersectmbo/cardano-ledger/releases/latest/download/shelley-ledger.pdf). Later
eras extend internally some of those rules, but from the Consensus perspective,
understanding the Shelley rules is enough.

## Types defined by the Ledger layer

:::note

`cardano-ledger` exposes very basic types from `cardano-slotting` (such as
`SlotNo`, `EpochNo`, ...) and `cardano-crypto-*` packages (such as `HASH`,
`VRF`, ...), which are part of the `cardano-base` repository, but we will omit
those in this discussion.

:::

This table describes how the types defined in the Ledger code are wrapped in
Consensus, and how those types are then aggregated in different type families
used all over the Consensus codebase:

| Ledger era(s) | Ledger type                                         | In Consensus                                         | Aggregating type family                        |
|---------------|-----------------------------------------------------|------------------------------------------------------|------------------------------------------------|
| Byron         | [`ABlockOrBoundary`][ledger-ABlockOrBoundary]       | [`ByronBlock`][consensus-ByronBlock]                 |                                                |
| Byron         | [`ABlockOrBoundaryHdr`][ledger-ABlockOrBoundaryHdr] | [`ByronHeader`][consensus-ByronHeader]               | [`Header blk`][consensus-Header]               |
| Shelley eras  | [`Block era`][ledger-Block]                         | [`ShelleyBlock proto era`][consensus-ShelleyBlock]   |                                                |
| TPraos eras   | [`BHeader c`][ledger-BHeader]                       | [`ShelleyHeader`][consensus-ShelleyHeader]           | [`Header blk`][consensus-Header]               |
| Byron         | [`Tx`][ledger-byron-Tx]                             | [`GenTx ByronBlock`][consensus-GenTx-Byron]          | [`GenTx blk`][consensus-GenTx]                 |
| Shelley eras  | [`Tx l era`][ledger-Tx]                             | [`ShelleyTx`][consensus-ShelleyTx]                   | [`GenTx blk`][consensus-GenTx]                 |
| Shelley eras  | [`Validated tx`][ledger-Validated]                  | [`ShelleyValidatedTx`][consensus-ShelleyValidatedTx] | [`Validated (GenTx blk)`][consensus-Validated] |
| Shelley eras  | [`NewEpochState era`][ledger-NewEpochState]         | [`ShelleyLedgerState`][consensus-ShelleyLedgerState] | [`LedgerState blk mk`][consensus-LedgerState]  |

Note that the header for TPraos eras (Shelley, Allegra, Mary, Alonzo) is defined
in `cardano-ledger` (`cardano-protocol-tpraos`) but the header for Praos eras
(Babbage, Conway, ...) is in `ouroboros-consensus-protocol`, which is why it is
not reflected in this table.

It is clearly visible that the type families are indexed (or transitively
indexed) by the block types. The block types are combined into the
`CardanoBlock` which is a `HardForkBlock`, providing a dispatcher for each of
the eras depending on the component of the n-ary sum on the value. The
`CardanoBlock` is another block, and as such, it also has instances for the type
families which will be n-ary sums or telescopes over the types for the
particular blocks defined in the ledger.

## Ledger state ticking and forecasting

Before a block can be applied to a Ledger state, the state has to be transported
through time up to the slot in which the block was forged. There are mutations
in the Ledger state that are enacted just by the passing of time, such as epoch
transitions (which imply rewards), or pulsing calculations that are spread
over time.

For Shelley eras, ticking a Ledger State is implemented as an evaluation of the
`TICK` transition rule. Ledger exposes the function
[`applyTick`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-shelley/Cardano-Ledger-Shelley-API-Validation.html#v:applyTick)
to run this rule.

When evaluating the validity of a header in Consensus (which is checked before
attempting to validate the body of the block), parts of the Ledger state (the
[`LedgerView`](https://cardano-ledger.cardano.intersectmbo.org/cardano-protocol-tpraos/Cardano-Protocol-TPraos-API.html#t:LedgerView))
need to be transported to the slot of the header, very much like a restricted
version of ticking the Ledger state. This is done by means of the
[`futureLedgerView`](https://cardano-ledger.cardano.intersectmbo.org/cardano-protocol-tpraos/Cardano-Protocol-TPraos-API.html#v:futureLedgerView)
function which in the end is an evaluation of the `TICKF` transition rule.

## Transaction validation

We will only talk about Shelley eras' transactions as it is expected there are
no more Byron transactions floating in the network. Byron transactions implement
the same instances in Consensus, but call different functions on the Ledger.

Applying a transaction to a ledger state means running the `MEMPOOL` transition
rule.

A transaction is said to be valid on top of a ledger state if applying the
transaction on said ledger state succeeds. Validity of a transaction is not
permanent, as transactions can be applied to different ledger states, and
succeed only on some.

Consensus checks the validity of transactions when adding them to the
mempool. Application of transactions is abstracted in the class
[`LedgerSupportsMempool`](https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus/Ouroboros-Consensus-Ledger-SupportsMempool.html#t:LedgerSupportsMempool).

For the Shelley eras, `cardano-ledger-shelley` exposes a function
[`applyTx`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-shelley/Cardano-Ledger-Shelley-API-Mempool.html#v:applyTx)
which returns either an error (`ApplyTxError era`) if the transaction is invalid
on the given ledger state, or a new state (`LedgerState era`) and a thin newtype
wrapper that tags the transaction as valid (`Validated (Tx era)`).

Note that even if a transaction is applied to a different ledger state, there
are some checks that cannot change their outcome, such as checking cryptographic
hashes. For this purpose, Ledger exposes the function
[`reapplyTx`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-shelley/Cardano-Ledger-Shelley-API-Mempool.html#v:reapplyTx)
which has a type very similar to the one of `applyTx`.

Both of these functions are wrapped by homonym functions in Consensus, which
operate on Consensus types and prepare the environment and context to call the
Ledger functions with the appropriate arguments and Ledger types.

## Block validation


A block `B` is said to be valid if applying the block on top of its predecessor
`B'` is successful. In particular if some ancestor block (or even `B'`) fails to
apply, then the ledger state at the predecessor block (at `B'`) cannot exist and
it would be absurd to ask whether `B` is valid or not.

In Shelley, applying a block to a ledger state means running the `BBODY` transition rule.

Consensus checks the validity of blocks when selecting chains in ChainSelection.

For Byron, Consensus calls either `validateBlock` or `validateBoundary` from
`cardano-ledger-byron` depending on whether a regular block or an [EBB](./ebbs)
is being validated.

For the Shelley eras, `cardano-ledger-shelley` exposes a function
[`applyBlock`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-shelley/Cardano-Ledger-Shelley-API-Validation.html#v:applyBlock)
which returns a new state (`NewEpochState era`), a list of failures (empty if
the block is valid) and a list of generated events.

Note that in contrast to transactions, blocks can only be applied on top of
their parent block, which cannot ever change, so a valid block is valid
always. This introduces a duality between `applyBlock` and `reapplyBlock`, in
which `reapplyBlock` skips as many checks as possible and cannot fail.

We use `reapplyBlock` to apply the blocks from Genesis (or the snapshot we
start from) to the immutable tip when initializing consensus, as we know those
blocks have been previously validated by us.

:::note

When a volatile block has been validated, it would be correct to reapply it if
we ever want to adopt that block again. Consensus at the moment does not do this
optimization, and applies it as if it was a new block.

:::

## Storing the UTxOs on disk

The UTxO set is by far the largest structure in the Ledger State. It contains
the millions of associations of `TxIn` to `TxOut`. Consensus, being the first
layer that uses impure code, is the one responsible for storing and managing
such set so that it can be stored in persistent storage, lowering the memory
requirements of a running node.

The ledger states that Consensus carries around have had their UTxO sets
stripped out. When a ledger rule is invoked, Consensus asks the ledger for the
needed entries of the UTxO set (with
[`neededTxInsForBlock`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-core/Cardano-Ledger-Block.html#v:neededTxInsForBlock)
for Shelley eras' blocks, and with
[`allInputsTxBodyF`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-api/Cardano-Ledger-Api-Tx-Body.html#v:allInputsTxBodyF)
for transactions) and injects only those into the Ledger state before invoking
the ledger rules.

Once the rules return a new ledger state, Consensus extracts the new UTxO set,
calculates the difference with the input set and pushes those differences to the
UTxO store. In particular, if the node is using the InMemory backend (which
consists of a UTxO set in a `TVar`), it will clone the UTxO set from the parent
ledger state and apply those differences to the cloned UTxO set. If the node is
using the LSM-trees backend, it will
[duplicate](https://intersectmbo.github.io/lsm-tree/lsm-tree-1.0.0.1-inplace/Database-LSMTree.html#v:duplicate)
the handle for the parent ledger state and
[push](https://intersectmbo.github.io/lsm-tree/lsm-tree-1.0.0.1-inplace/Database-LSMTree.html#v:updates)
the differences to the cloned handle.

## Codecs

`cardano-ledger` provides instances for serializing all the relevant types it
exposes. It uses the classes
[`EncCBOR`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-binary/Cardano-Ledger-Binary-Encoding.html#t:EncCBOR)
and
[`DecCBOR`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-binary/Cardano-Ledger-Binary-Decoding.html#t:DecCBOR)
to produce
[`Encoding`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-binary/Cardano-Ledger-Binary-Encoding.html#t:Encoding)
and
[`Decoder`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-binary/Cardano-Ledger-Binary-Decoding.html#t:Decoder)
values which depend on a
[`Version`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-binary/Cardano-Ledger-Binary-Version.html#t:Version)
to decide on the particular codec format. This allows different eras to encode values in different ways.

Most of the ledger types are wrapped in a Hard Fork Combinator construct in
Consensus, where a tag for the era is prepended thus allowing Consensus to
express/infer the era of the underlying data.

From the Consensus side, the particular era codec is invoked from within the
[`ToCBOR`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-binary/Cardano-Ledger-Binary.html#t:ToCBOR)
and
[`FromCBOR`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-binary/Cardano-Ledger-Binary.html#t:FromCBOR)
instances for a particular value, where the era is known, by using
[`toEraCBOR`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-core/Cardano-Ledger-Core.html#v:toEraCBOR)/[`toPlainEncoding`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-binary/Cardano-Ledger-Binary-Encoding.html#v:toPlainEncoding)
or
[`eraDecoder`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-core/Cardano-Ledger-Core.html#v:eraDecoder)/[`toPlainDecoder`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-binary/Cardano-Ledger-Binary-Decoding.html#v:toPlainDecoder).

As such, the serialized form of a block is just the `toEraCBOR @era` of the
block, wrapped in a tag that indicates the era of the block. The serialized form
of a transaction is `toEraCBOR @era` of the transaction, wrapped in a tag that
indicates the era of the transaction.

## Translation among eras

The Ledger layer is structured in separate eras. Each era is its own package,
implementing all the interfaces required for such an era, and linking it to the
previous era ([`PreviousEra
era`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-core/Cardano-Ledger-Core.html#t:PreviousEra)).

Some of the Ledger types will need to be transported from one era to the next
one, in particular the most relevant one is the Ledger State. When Consensus
ticks through an era boundary, the state has to be translated to the new era in
order to apply the block after the era boundary (which will belong to the new
era too). For this purpose, `cardano-ledger` defines the
[`TranslateEra`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-core/Cardano-Ledger-Core.html#t:TranslateEra)
class, which is capable of translating types from the previous era to the
current one, using a
[`TranslationContext`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-core/Cardano-Ledger-Core.html#t:TranslationContext).
Transactions (`Tx era`) sometimes need to also be translated to a later era,
when the transaction received from the network is in an older era than the
current selection.

Some types do not require a translation context and can always be converted to a
later era. Instead of "translate", this is called "upgrade". A notable example
is `TxOut`s which are upgraded through
[`upgradeTxOut`](https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-core/Cardano-Ledger-Core.html#v:upgradeTxOut).

## Queries

`cardano-ledger` also exposes an API to perform queries into the Ledger State,
to offer information to local clients such as wallets. Queries are discussed extensively in [the next section](./queries).

[ledger-ABlockOrBoundary]: https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-byron/Cardano-Chain-Block.html#t:ABlockOrBoundary
[consensus-ByronBlock]: https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus-cardano/Ouroboros-Consensus-Byron-Ledger-Block.html#t:ByronBlock
[ledger-ABlockOrBoundaryHdr]: https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-byron/Cardano-Chain-Block.html#t:ABlockOrBoundaryHdr
[consensus-ByronHeader]: https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus-cardano/Ouroboros-Consensus-Byron-Ledger-Block.html#t:Header
[consensus-Header]: https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus/Ouroboros-Consensus-Block-Abstract.html#t:Header
[ledger-BHeader]: https://cardano-ledger.cardano.intersectmbo.org/cardano-protocol-tpraos/Cardano-Protocol-TPraos-BHeader.html#t:BHeader
[consensus-ShelleyHeader]: https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus-cardano/Ouroboros-Consensus-Shelley-Ledger-Block.html#t:Header
[ledger-byron-Tx]: https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-byron/Cardano-Chain-UTxO.html#t:Tx
[consensus-GenTx-Byron]: https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus-cardano/Ouroboros-Consensus-Byron-Ledger-Mempool.html#t:GenTx
[ledger-Block]: https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-core/Cardano-Ledger-Block.html#t:Block
[consensus-ShelleyBlock]: https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus-cardano/Ouroboros-Consensus-Shelley-Ledger-Block.html#t:ShelleyBlock
[ledger-BlockBody]: https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-core/Cardano-Ledger-Core.html#t:BlockBody
[ledger-Tx]: https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-core/Cardano-Ledger-Core.html#t:Tx
[consensus-ShelleyTx]: https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus-cardano/Ouroboros-Consensus-Shelley-Ledger-Mempool.html#t:GenTx
[consensus-GenTx]: https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus/Ouroboros-Consensus-Ledger-SupportsMempool.html#t:GenTx
[ledger-NewEpochState]: https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-shelley/Cardano-Ledger-Shelley-LedgerState.html#t:NewEpochState
[consensus-ShelleyLedgerState]: https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus-cardano/Ouroboros-Consensus-Shelley-Ledger-Ledger.html
[consensus-LedgerState]: https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus/Ouroboros-Consensus-Ledger-Basics.html#t:LedgerState
[ledger-Validated]: https://cardano-ledger.cardano.intersectmbo.org/cardano-ledger-shelley/Cardano-Ledger-Shelley-API-Mempool.html#t:Validated
[consensus-ShelleyValidatedTx]: https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus-cardano/Ouroboros-Consensus-Shelley-Ledger-Mempool.html#t:GenTx
[consensus-Validated]: https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus/Ouroboros-Consensus-Ledger-SupportsMempool.html#t:Validated
