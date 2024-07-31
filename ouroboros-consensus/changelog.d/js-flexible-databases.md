### Breaking

- `completeChainDbArgs` now requires two file-systems. This allows to place the
  immutable data (which doesn't need to be stored in a very performant device)
  somewhere else than the volatile data.
