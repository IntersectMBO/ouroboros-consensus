# Interaction with the Ledger Layer

The [`ChainDB`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Storage/ChainDB/API.hs#L113) component, which combines storage and some consensus logic, uses the [`LedgerDB`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Storage/LedgerDB/API.hs#L23) and [interfaces](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Abstract.hs#L10) provided by the Ledger layer, such as [`ApplyBlock`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Abstract.hs#L93), to validate blocks as part of chain selection and initialization.

The [`Mempool`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Mempool/API.hs#L50) uses the Ledger layer to validate individual transactions against the current mempool's ledger state.

The Ledger layer itself defines how the ledger state transitions upon transaction or block application, or via time-based updates (ticking).
It also provides interfaces, like [`ledgerViewForecastAt`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/SupportsProtocol.hs#L71) within the `LedgerSupportsProtocol` class, to offer limited lookahead (*forecastin*g) of parts of the future ledger state ([`LedgerView`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Protocol/Abstract.hs#L94)), which is needed for validating block headers without the full block bodies.

Below, we list the  key classes and types governing the interaction between Consensus and Ledger.

The [`IsLedger`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Basics.hs#L161) class defines a fundamental abstract interface that a ledger must provide to the Consensus layer, independent of a specific block type or consensus protocol. Its primary responsibility is to specify how the ledger state changes purely due to the passage of time (see [Ticking](#ticking)).

The [`ApplyBlock`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Abstract.hs#L93) class is a core abstract interface in the Consensus layer that defines how the ledger state is updated by applying a block. It specifies the functions needed for this state transition, operating on a ledger state type (`l`) and a block type (`blk`). This class is fundamental to how the Consensus layer validates blocks and incorporates them into the chain.

The [`LedgerSupportsMempool`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/SupportsMempool.hs#L116) typeclass defines the essential interface that the Ledger layer provides to the Mempool. Its primary function is to enable the Mempool to validate individual transactions.

The [`LedgerCfg`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Basics.hs#L127) type family defines the static environment or configuration required by a specific ledger implementation. It holds the fixed parameters and rules that govern a ledger's behavior. This configuration is passed to various ledger-related functions used throughout `ouroboros-consensus`. Different ledger eras require different static configurations. For instance, the duration of slots and the size (in slots) of epochs[^1], initial delegations or assignment of funds to addresses.

The [`GetTip`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Basics.hs#L62) class requires that any ledger can report its tip as a `Point`. A `Point l` is either genesis or a pair of a hash and slot number, and it is parametric over the ledger type `l` to accommodate different hash types. This class is also required for the [ticked](#ticking) ledger state.

The [`LedgerState`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Basics.hs#L244) data family defines the state of the ledger at a particular point in the chain and is associated with a specific block type (`blk`). The `LedgerState` encapsulates the accumulated effect of all applied blocks and time-based updates, essentially representing the summary needed to validate subsequent blocks and transactions.

The `LedgerState` also provides a necessary projection of the state that the consensus protocol uses for tasks like leadership checks and header validation: the [`LedgerView`](https://github.com/intersectmbo/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Protocol/Abstract.hs#L94).

## Ticking


[^1]: Byron era had a 20-second slot duration, distinct from the 1-second duration in subsequent eras.
