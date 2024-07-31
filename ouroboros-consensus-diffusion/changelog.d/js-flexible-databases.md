### Breaking

- The `StdRunNodeArgs(srnDatabasePath)` argument becomes of type `NodeDatabasePaths`
  which will allow storing the immutable db (which doesn't need to be in a very
  performant device) somewhere different than the volatile data.
