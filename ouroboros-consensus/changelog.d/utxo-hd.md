### Breaking

- Implement the UTxO-HD feature. See the documentation in [the
  webpage](https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/utxo-hd/Overview).
  - The `LedgerState` type family changed its kind to `Type -> (Type -> Type) ->
    Type` to account for `MapKind`s.
  - `LedgerTables` store a `MapKind` of `TxIn l` to `TxOut l`.
  - The `HardFork` block's `TxIn` is `CanonicalTxIn` which is stable across
    eras.
  - Applying blocks and ticking consume `ValuesMK` and produce `DiffMK`.
  - The LedgerDB has three implementations: `V1InMemory`, `V1LMDB` and
    `V2InMemory`. The first one is not intended to be used in production.
  - The LedgerDB keeps track of differences from blocks and flushes them to the
    backend.
  - Snapshots have changed in format, now there are two files in each snapshot
    instead of one.
  - To evaluate forks, the LedgerDB exposes the `Forker` and the
    `ReadOnlyForker` interfaces.
  - `BlockQuery` is now parametrized by a `QueryFootprint` type.
  - `HardFork` `BlockQuery` are now processed slightly different than single era
    `BlockQuery`, see `BlockSupportsHFLedgerQuery`.
  - The mempool state is now held in a `TMVar` instead of a `TVar` to be able to
    acquire it, read values from the backend and update it.
