### Breaking

- Use new mlocked KES API to represent KES sign keys internally. This ensures
  that KES keys are securely erased when replaced with a newer evolution or a
  fresh key, and that they will not spill to disk or swap. See
  https://github.com/IntersectMBO/cardano-base/pull/255.
- Add `finalize` method to `BlockForging`, and use it where necessary to clean
  up when a block forging thread terminates (see `forkLinkedWatcherFinalize`)
