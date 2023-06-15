# Consensus benchmarks

We are in the process of adding component level microbenchmarks for Consensus.
We started with microbenchmarks for adding transactions to the mempool. The
mempool benchmarks can be run using the following command.

```sh
cabal new-run ouroboros-consensus:mempool-bench
```

We check for regressions in performance on CI. We might publish benchmark results in this site shortly.
