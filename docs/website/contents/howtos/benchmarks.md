# Consensus benchmarks

We are in the process of adding component level microbenchmarks for Consensus.

We check for regressions in performance on CI.

## Mempool Benchmark

We started with microbenchmarks for adding transactions to the mempool. The
mempool benchmarks can be run using the following command.

```sh
cabal new-run ouroboros-consensus:mempool-bench
```

## ChainSync Client Benchmark

To aid the refactoring of the ChainSync client, we added a benchmark for it in [PR#823](https://github.com/IntersectMBO/ouroboros-consensus/pull/823). The benchmark could be invoked as follows:

```sh
cabal new-run ouroboros-consensus:ChainSync-client-bench -- 10 10
```
