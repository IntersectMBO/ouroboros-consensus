### Breaking

- New `ChainDB.TraceEvent(TraceLastShutdownUnclean)` trace message to be emitted
  when the `clean` marker is missing and the node will therefore revalidate all
  the data.
