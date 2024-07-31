### Non-Breaking

- Emit `ChainDB.TraceEvent(TraceLastShutdownUnclean)` when the `clean` marker is
  missing and the node will therefore revalidate all the data.
