# Interaction with the Ledger Layer

The [`ChainDB`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Storage/ChainDB/API.hs#L113) component, which combines storage and some consensus logic, uses the [`LedgerDB`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Storage/LedgerDB/API.hs#L23) and [interfaces](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Abstract.hs#L10) provided by the Ledger layer, such as [`ApplyBlock`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Abstract.hs#L93), to validate blocks as part of chain selection and initialization.

The [`Mempool`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Mempool/API.hs#L50) uses the Ledger layer to validate individual transactions against the current mempool's ledger state.

The Ledger layer itself defines how the ledger state transitions upon transaction or block application, or via time-based updates (ticking).
It also provides interfaces, like [`ledgerViewForecastAt`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/SupportsProtocol.hs#L71) within the `LedgerSupportsProtocol` class, to offer limited lookahead (*forecastin*g) of parts of the future ledger state ([`LedgerView`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Protocol/Abstract.hs#L94)), which is needed for validating block headers without the full block bodies.

Below, we list the  key classes and types governing the interaction between Consensus and Ledger.

The [`IsLedger`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Basics.hs#L161) class defines a fundamental abstract interface that a ledger must provide to the Consensus layer, independent of a specific block type or consensus protocol. Its primary responsibility is to specify how the ledger state changes purely due to the passage of time (see [Ticking](#ticking)).

The [`ApplyBlock`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Abstract.hs#L93) class is a core abstract interface in the Consensus layer that defines how the ledger state is updated by applying a block. It specifies the functions needed for this state transition, operating on a ledger state type (`l`) and a block type (`blk`). This class is fundamental to how the Consensus layer validates blocks and incorporates them into the chain.

The [`LedgerSupportsMempool`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/SupportsMempool.hs#L116) typeclass defines the essential interface that the Ledger layer provides to the Mempool. Its primary function is to enable the Mempool to validate individual transactions.

The [`LedgerCfg`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Basics.hs#L127) type family defines the static environment or configuration required by a specific ledger implementation. It holds the fixed parameters and rules that govern a ledger's behavior. This configuration is passed to various ledger-related functions used throughout `ouroboros-consensus`. Different [ledger eras](TODO: link to some table that explains and summarizes them) require different static configurations. For instance, the duration of slots and the size (in slots) of epochs[^1], initial delegations or assignment of funds to addresses.

The [`GetTip`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Basics.hs#L62) class requires that any ledger can report its tip as a `Point`. A `Point l` is either genesis or a pair of a hash and slot number, and it is parametric over the ledger type `l` to accommodate different hash types. This class is also required for the [ticked](#ticking) ledger state.

The [`LedgerState`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Basics.hs#L244) data family defines the state of the ledger at a particular point in the chain and is associated with a specific block type (`blk`). The `LedgerState` encapsulates the accumulated effect of all applied blocks and time-based updates, essentially representing the summary needed to validate subsequent blocks and transactions.

The `LedgerState` also provides a necessary projection of the state that the consensus protocol uses for tasks like leadership checks and header validation: the [`LedgerView`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Protocol/Abstract.hs#L94).

## Cardano Instances

To instantiate the classes and types that define the interaction between `ouroboros-consensus` and the Cardano ledger, we rely on the [`cardano-ledger`](https://github.com/input-output-hk/cardano-ledger) packages. The sub-folder [`ouroboros-consensus-cardano`](https://github.com/IntersectMBO/ouroboros-consensus/tree/main/ouroboros-consensus-cardano) contains all the Cardano specific instantiations of the aforementioned classes and types.

The Cardano chain contains block segments from [different eras](https://cardano-scaling.github.io/cardano-blueprint/consensus/index.html#the-consensus-protocol-in-cardano).
The `cardano-ledger` packages offer an implementation of the ledger functionality for each of these eras.
Each era implements the full Ouroboros Consensus ledger interface.
The [Hard Fork Combinator](TODO: link to HFC/chain updates) (HFC) allows us to dispatch the era-specific implementations, depending on the era we need to use.

Currently, we support two kinds of blocks in Cardano: Byron and Shelley-based.

Shelley-based blocks, which include eras like Mary, Allegra, Alonzo, and Babbage, share part of their implementation. This shared aspect is facilitated by common APIs and classes defined within `cardano-ledger`, such as the `ShelleyBasedEra` class.

In addition, each of these Shelley-based eras also implements details specific to their era. For instance, transactions (`GenTx`) and the logic for applying them (`applyTx`) or measuring their size (`TxMeasure`) can differ between eras.
[Forecasting](#forecasting-and-the-forecast-range) ledger views might also have era-specific logic or parameters.

## Ticking

"Tick" is a common term in discrete systems for time passing.
Within Ouroboros, time is divided into slots, and to _tick_ a state `X` means to advance `X` from one slot to a later slot.
In the Cardano system, we tick a ledger state, a protocol state, or both at once (see `ExtLedgerState`).

The fundamental use case for ticking is to advance the ledger state between one block and the next on that same chain.
In Ouroboros Praos, one block and the next will be separated by zero or more slots; usually several, since Praos relies on time passing between blocks arising to limit short forks.
However, applying blocks is not the only reason the Consensus Layer ticks.
If it were, then ticking wouldn't need to be a distinguished step: it'd merely be the first of however many "hidden" steps are involved in applying a block.
Instead, ticking is used in a few places, some of which don't involve immediately subsequent header/block application.

- Ticking advances protocol state between headers.
The [`ConsensusProtocol`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Protocol/Abstract.hs#L66) class defines [`tickChainDepState`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Protocol/Abstract.hs#L155), which is used to tick the protocol-specific state. It takes the `LedgerView` as an argument
- Ticking advances ledger states between blocks (via `ExtLedgerState`, this also ticks the protocol state). The `IsLedger` class defines the [`applyChainTick`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Basics.hs#L214) function (via `applyChainTickLedgerResult`). This function is used to advance the ledger state purely due to the passage of time (slots). It handles state transitions that occur solely because time has passed, such as scheduled updates or epoch boundary logic.
- The leadership check ticks the protocol state from the selected chain's tip to the wall clock's current slot when checking whether this stake pool should mint a block.
- The mempool ticks the selected chain's ledger state ahead by exactly one slot, since the hypothetical next block to extend the selection has to be strictly younger than the selection. (Only Byron's Epoch Boundary Blocks can share their predecessor's slot, but EBBs also don't contain transactions.)
- Today's mint also ticks the ledger state up to the current slot, to determine the transaction limits (`size`, `ExUnits`, etc) for the new block.

The ledger rules for ticking involve some crucial concerns: deadlines come and go (despite being enforced in block applications), epochs begin and end, protocol parameters change, and even the ledger rules themselves might change.

## Forecasting and the Forecast Range

_Forecasting_ is an alternative to ticking that also represents the passage of time.
There are two key differences between ticking and forecasting.

- Ticking is only sound if the intervening slots do not contain a block; it is erroneous to tick _past_ a block on the chain.
- Forecasting does not produce a complete ledger state.
  It specifically only yields the parts of the future ledger state that are already fully determined _regardless of blocks you're forecasting past_.

In Cardano, the only thing we ever need to forecast is the part of the ledger state that is necessary to validate a header (see `LedgerView`).
This can be fully determined regardless of intervening blocks because the ledger rules ensure the relevant parts of the ledger state do not change without a minimum duration of forewarning (eg by retaining occasional snapshots of this data).
That minimum duration of stability is therefore _the forecast range_, beyond which the code cannot necessarily do correct forecasting.

Thus forecasting from an (unticked) ledger state in slot `X` to slot `Y` (within the forecast range) must be equivalent to ticking from `X` to `Y` and then "forecasting" from `Y` to `Y`.

Note that ticking from `X` to `Y` is usually only sound if there are no blocks in that slot interval. However, since the part of the ledger that we forecast is stable, it cannot be affected by those blocks, and so the ticking is sound in the specific context of characterizing forecasting.

The primary part of the ledger state needed for forecasting is the `LedgerView`, defined within the `ConsensusProtocol p` class.
The primary function used for forecasting (a `LedgerView`) within the Ouroboros consensus layer is [`ledgerViewForecastAt`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/SupportsProtocol.hs#L71).

## `Ticked` data family

We introduced the [`Ticked`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ticked.hs#L45) data family to ensure that the Consensus Layer ticks exactly once between blocks.
The abstract interface maps an unticked ledger state to a `Ticked` ledger state, and applying a header/block is the _only_ way to map a `Ticked` ledger state to an unticked ledger state.

By encoding this alternation between ticking and application in the types of the abstract interface, we prevent ourselves from accidentally ticking too few or too many times.

## Partiality of Ticking

In our code base, ticking is a **total** function.
Forecasting, on the other hand, is partial, since it is able to reject requests that violate the forecast range.
(It's important to note that such a request is not doomed: the node might just need to process another block or two before the slot of interest will be within the forecast range of its selected chain's tip.)

However, the Consensus Layer itself will never need to tick farther than the forecast range.
The ledger rules might actually be able to do so, since it's mostly just a matter of the state _not_ changing.
It could plausibly cause problems within the ledger rules if an entire epoch has no blocks, for example, so there's definitely no guarantee that the resulting ledger state would actually be useful.
But again: the Consensus Layer will never request such a tick.

The inductive argument is that all the ticking the Consensus Layer does is ultimately guarded by some forecast, unless someone has altered the node's files between executions (or if there's a bad bug somewhere else).

- The mempool only ticks by a single slot, so any non-degenerate forecast range supports that.
- The leadership check immediately aborts if it cannot forecast the `LedgerView`.
  The mint only proceeds if the leadership succeeded.
- The `ChainSync` client forecasts the `LedgerView` before it applies any header.
- The `ChainSel` logic only applies blocks whose headers arrived via a `ChainSync` client or were minted by this node.
  The node also processes blocks from its on-disk storage when initializing, but those blocks were either minted by this node, acquired by this node via `ChainSync` (and `BlockFetch`), written by another trusted node (eg via Mithril) between executions, or written by some other means that is not explicitly unsupported.

Thus, it's reasonable for the ticking function to have a total type because none of its calls within the node can fail; it's always guarded by a forecast.

## Bounding Ticking and Forecasting Computations

On a single valid chain, the Consensus Layer will never tick across the same slots multiple times.
However, Ouroboros involves short forks and potentially invalid blocks.
On the other hand, even for a single chain, the Consensus Layer would forecast across the same slots multiple times: it does it each time the wall clock enters a new slot as part of the leadership check, and it does so each time any peer sends it a header.

Ideally all ticks would be inexpensive, but the occasional spike is managable, since short forks are short lived.
An expensive forecast, on the other hand, can be disastrous.
If the leadership check's forecast takes too long, then the node might be late to produce its block.
If that forecast takes longer than a slot, then the node might not even check every slot, which is unacceptable.

For these reasons, the ledger rules contain certain optimizations that either prevent forecasts from ever paying certain costs or else ensure that forecasting across some particular slot is only expensive the first time it happens and subsequent forecasts will then avoid those costs via memoization.

[^1]: Byron era had a 20-second slot duration, distinct from the 1-second duration in subsequent eras.
