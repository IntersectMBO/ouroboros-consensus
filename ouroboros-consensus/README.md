# Core Consensus package

This package constitutes the cornerstone of the Consensus Layer of Cardano. It
implements the Ouroboros family of blockchain protocols. The best way to
understand this repository is by reading our
[documentation](https://ouroboros-consensus.cardano.intersectmbo.org/).

This package contains:

* `src`: the implementation of the Ouroboros consensus protocols and required
  components, e.g., the storage layer, mempool, protocol clients and servers,
  etc. This library abstracts over the choice of specific ledger and protocol.

* `consensus-testlib`: testing utilities for the test-suites.

* `tutorials`: a collection of tutorials that are helpful for understanding the
  abstract structure of the consensus protocol. The structure of these is
  explained [here](../docs/website/contents/tutorials/instantiating_consensus.md).

* `mock-block`: definition of a simple mock ledger used for testing.

It also contains some test-suites and benchmarks:

* `consensus-test`: tests for the Consensus core library.

* `storage-test`: tests the ChainDB implementation

* `mempool-bench`: a benchmark of the time it takes to add transactions to a mempool
