### Breaking

- Implement the UTxO-HD feature. See the documentation in [the
  webpage](https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/utxo-hd/Overview).
  - `hStateQueryServer` now needs a `ResourceRegistry` to allocate `Forker`s.
  - `DiskPolicyArgs` was transformed into `SnapshotPolicyArgs`.
  - `StdRunNodeArgs` got two new fields:
    - `srnQueryBatchSize` to specify how many entries read on each batch when
      answering queries.
    - `srnLdbFlavorArgs` to select the LedgerDB backend.
  - The forging loop uses a `ReadOnlyForker` to get the ledger state of the
    block on top of which it should forge and the mempool snapshot.
