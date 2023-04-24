# Cardano instantiation of the Consensus Layer

The [Consensus core package](../ouroboros-consensus) is abstract over the
specific choice of ledger, dictated by the type of block used. This package
defines the Block (and therefore the Ledger) for the Cardano blockchain, in
particular for the Byron, Shelley and Cardano instantiations.

There are also test-suites for each of the block definitions.

This package also contains two executables:

* `app/db-analyser.hs`: performs different analyses on a ChainDB, for
  performance measurements or ensuring validity.
  
* `app/db-synthesizer`: builds a chain, to be used with benchmarking purposes.
