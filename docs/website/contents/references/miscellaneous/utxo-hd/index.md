# High Level Overview of UTxO-HD

UTxO-HD is an internal rework of the Consensus layer that features a hybrid
database for Ledger State data. UTxOs are stored in a separate database that
can be backed by an on-disk database or with an in-memory implementation.

Each of those backends have specific behaviors and implications, so we will
refer to them individually by `InMemory` and `OnDisk`.

End-users of the `InMemory` backend (the default one) should not appreciate any
major difference in behavior and performance with respects to a pre-UTxO-HD
node.

End-users of the `OnDisk` backend will observe a regression in performance. For
now the `OnDisk` backend is implemented via LMDB and not optimal in terms of
performance, but we plan on making use of the LSM trees library that Well-Typed
is developing for a much better performance. In particular operations that need
UTxOs (applying blocks/transactions) will have the overhead of a trip to the
disk storage plus some calculations to bring the disk values up to date to the
tip of the chain.

In exchange for that performance regression, a Cardano node using the `OnDisk`
backend can run with much more modest memory requirements than a pre-UTxO-HD
node.

In terms of functionality, both backends are fully functional.

For a more extensive description of UTxO-HD, see [the full documentation](utxo-hd_in_depth.md).
