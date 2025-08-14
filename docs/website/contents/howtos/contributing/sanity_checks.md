# Sanity Checks

This document is intended for anyone testing or benchmarking the Consensus component  outside this repository, for instance `cardano-node` or `cardano-node-tests`. We describe a series of sanity check that can be performed to rule out some known problems.


## Enable assertions

Even if the Consensus code is being benchmarked, it might be a good idea to *temporarily* enable assertions to perform additional checks on consistency.
The following snippet can be added to the `cabal.project` file to enable assertions in our two main components:

```cabal
package ouroboros-consensus
  flags: +asserts

package ouroboros-consensus-cardano
  flags: +asserts
```

## Double check configuration sanity

- The `k` value has to be the same for all eras.
- The `SnapshotInterval` has to be manually set or it will be computed as `2*k` slots, which will have a *huge* negative impact on performance if `k` is small.
- The mempool capacity is set by default to 2 times the maximum transaction capaciy in a block. Check if this default makes sense in the given context.
