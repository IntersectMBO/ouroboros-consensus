<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- `inlineCertifiedEbsFollower` takes three more arguments: ways to read a
  block's header and its raw bytes, and a way to find a block's parent.
- `chainSyncBlocksServer` needs `GetPrevHash blk`, in place of
  `HasHeader (Header blk)`.

### Non-Breaking

- New `findParentPoint`, `headerOnChain`, `parentOnChain`,
  `inlineCertifiedEbsFollowerOnChainDB` and `RbParentNotFound`,
  exported from `Ouroboros.Consensus.MiniProtocol.ChainSync.Server`.

### Patch

- Node-to-client ChainSync: when a rollback lands on a block RB that announced
  an EB, the client is rolled back one block deeper, to RB's parent, and then
  gets RB or RB⁺ again. This covers a chain switch that keeps RB but changes
  whether its EB is certified, and a client that reconnects at RB. In both, the
  server cannot tell whether the client has RB or RB⁺.
- RB's parent comes from the node's current chain, with no lookup, unless RB
  is at or below the immutable tip. Then it is found in the ImmutableDB.
- If the server cannot find RB's parent, it throws `RbParentNotFound` and ends
  the client's connection, rather than leave the client with the wrong version
  of RB.
- A rollback point that has left the node's selected chain is sent as it is,
  because the server rolls the client back past it next.
