### Breaking

- Added `DiffusionPipeliningSupport` to Config module. Since o-network does not track
  any longer whether pipelining support is enabled for a particular `NodeToNodeVersion`
  this capability was moved into the consensus layer to preserve generality.
  - mkBlockFetchConsensusInterface and bracketChainSyncClient signatures were adapted
    to leverage the new type.
