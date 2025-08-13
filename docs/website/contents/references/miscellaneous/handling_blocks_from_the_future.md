#  Blocks from the future

A node can receive a block whose slot is ahead of the current slot. We call such **blocks from the future**.

The Praos protocol ignores chains with blocks from the future during chain selection.
It assumes nodes have perfectly synchronized clocks, which is not realistic due to imperfections in protocols like NTP and leap seconds.
In practice, a clock skew of up to 2 seconds is considered acceptable.
Our implementation differentiates between blocks from the near future and those from the far future:

- A block is from the near future if the onset of its slot is ahead of the wall clock, but only by at most the admissible clock skew. Despite being from the future, these blocks are assumed to potentially have been minted by honest nodes.
- A block is from the far future if the onset of its slot is ahead of the wall clock by more than the admissible clock skew.  By assumption, these blocks cannot have been minted by an honest node.

# Handling blocks from the future

As of [#525](https://github.com/IntersectMBO/ouroboros-consensus/pull/525):

- When receiving a header from the **near** future in `ChainSync`, an artificial delay is introduced until the header is no longer from the future.
Only then it is validated and the corresponding block body is downloaded and added to the `ChainDB` for chain selection, where it is not considered to be from the future due to the previous artificial delay.
- When receiving a header from the far future, we immediately disconnect from the corresponding peer.

In addition, we never forge atop a block from the future (which was the case even before [#525](https://github.com/IntersectMBO/ouroboros-consensus/pull/525).

### During initialization

Since we now delay the headers until they are no longer from the **near future**, a caught up node will never contain blocks from the future in the `VolatileDB`, according to its own clock.
However, there are two caveats:
- Clock rewinds can violate this property. In particular the node [will error](https://github.com/IntersectMBO/ouroboros-consensus/blob/4488656439e78c572c3dce0f7ed2cf98f61c65bb/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/BlockchainTime/WallClock/HardFork.hs#L138-L146) when we rewind the clock by more than [20 seconds](https://github.com/IntersectMBO/ouroboros-consensus/blob/4488656439e78c572c3dce0f7ed2cf98f61c65bb/ouroboros-consensus-diffusion/src/ouroboros-consensus-diffusion/Ouroboros/Consensus/Node.hs#L485).
- The node clock might be set in the future relative to the rest of the nodes in the network.
Thus, it is possible that after restarting a node with a clock set in the future, and setting the clock back so that the clock is now synchronized with the rest of the network, the blocks in the `VolatileDB` are regarded as blocks from the future.

When initializing the `ChainDB` we do not check if blocks in the `VolatileDB` are from the future. This presents two inconveniences:

- When the node diffuses these blocks from the **far** future, it will be disconnected from other peers.
- The node [will not forge](https://github.com/IntersectMBO/ouroboros-consensus/blob/16fa8754be24f26eddef006c03ba945ea00e3566/ouroboros-consensus-diffusion/src/ouroboros-consensus-diffusion/Ouroboros/Consensus/NodeKernel.hs#L708) a block on top of a block from the future, thus missing its chance to lead in the slot.

These problems can be solved by wiping out the `VolatileDB` in this situation.
However, note this is an extremely rare situation: the clock of the node would have to have been set quite far in the future, as shutting down a node and restarting it already takes a significant amount of time.

In the future we might delete blocks from the future from the `VolatileDB` to improve the user experience and robustness of the initialization logic. For now it does not seem worthwhile to handle that rare case. (Downstream/bidirectional peers will disconnect from such a node, but only until enough time has passed that its `VolatileDB` does not contain blocks from the future anymore.)

# References

- [Original issue that prompted the fix](https://github.com/IntersectMBO/ouroboros-network/issues/4251)
- [Blocks from the future (Incident report)](https://updates.cardano.intersectmbo.org/2024-09-07-incident/)
