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

## PerasCertDB Benchmark

We have a microbenchmark for the boosted chain fragment weight calculation, which could be run as follows:

```sh
cabal run ouroboros-consensus:PerasCertDB-bench -- +RTS -T -A32m -RTS
```

We request GHC runtime system statistics with `-T` to get a memory usage estimate, and also request a large nursery with `-A32m` to minimise garbage collection. See `tasty-bench` [documentation](https://github.com/Bodigrim/tasty-bench?tab=readme-ov-file#troubleshooting) for more tips.
