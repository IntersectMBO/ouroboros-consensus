# Cardano instantiation of the Consensus Layer

The [Consensus core package](../ouroboros-consensus) is abstract over the
specific choice of ledger, dictated by the type of block used. This package
defines the Block (and therefore the Ledger) for the Cardano blockchain, in
particular for the Byron, Shelley and Cardano instantiations.

There are also test-suites for each of the block definitions.

This package also contains two executables:

* `app/db-analyser.hs`: performs different analyses on a ChainDB, for
  performance measurements or ensuring validity.
  
* `app/db-synthesizer.hs`: builds a chain, to be used with benchmarking purposes.

* `app/ledger-db-backends-checker.hs`: simple tool (for the UTxO-HD feature) to
  compare the contents of an in-memory backing store and an LMDB backing store
  (i.e., it checks if they have the same slot number and values).
