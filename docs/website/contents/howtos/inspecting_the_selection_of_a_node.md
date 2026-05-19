# HOWTO Inspect the Selection of a Node

A running node will always be able to answer queries about its selection.
This is a fundamental assumption of the Ouroboros protocols.

This HOWTO document describes how to reuse the existing mechanisms to manually explore/inspect a node's selection.
The intended audience is a developer who is trying to inspect/troubleshoot the differences between the selections of two different nodes.

# Status Quo

The following mechanisms are currently available for inspecting a node's selected chain.

- Node-to-Node ChainSync iterates through headers, starting with the one after the initially given point.
- Node-to-Node BlockFetch fetches the requested blocks.
- Node-to-Client ChainSync iterates through blocks, starting with the one after the initially given point.
- Node-to-Client LocalStateQuery (LSQ) answers a broad range of queries about a particular point, which enable a suite of related `cardano-cli` subcommands.

There will be more details on each below.
But first, some caveats.

# The Node Must Be Up

If the node is down or otherwise unavailable, none of these mechanisms can work.
In that case, the only way to inspect the node's selection is with lower-level "power user" tooling that inspect the node's _private_ on-disk files.
These tools include `db-analyser` and `db-immutaliser`, but are considered beyond the scope of this HOWTO document.
They should only be used to inspect a node's files while the node is down; the running node does not bother to maintain the files in a way that ensures they're coherent for other concurrent processes to access, since that's not necessary for the Ouroboros protocol.

# You Always Need a Known Slot-Hash Pair (Known Point)
 
A _point_ is a pair of a slot and a hash (more precisely: a header hash).
For the Ouroboros protocols, hashes are always accompanied by a slot, either directly or implicitly.

For this reason, the node requires you to provide a known starting point when inspecting its selection.
If you have only a slot or only a hash, then your first step _must_ be to pick a point that satsifies the following criteria.

- You're sure the point is equally old or older than the slot/hash you're interested in.
- You're sure the point is on the selection of the node you're interested in.

For example, the GenesisPoint always satisfies these criteria, but you would always save a good chunk of your time as well as some of your node's resources by trying to find a younger known point.
You could pick a point from your node's logs from last week.
You could use the public web interface of some Cardano index service in order to find the point of the first block from last month.
Etc.

Once you have a point that satisfies those criteria, simply use ChainSync to iterate forward from that known point until you find a header with your hash or with a slot that's >= your slot.
By this bootstrap step, you now have the point you actually need in order to use the available mechanisms.

There are three notable examples where a hash might be available but not the corresponding slot.
All of them could be handled as described above.

- The `prevhash` field of a header lists only the hash of the parent header but not its slot.
  This is because headers never exist in isolation in the node.
  The predecessor is always already available from context.
  The `prevhash` field is merely intended to disambiguate; it's not meant to enable navigation/reverse-iteration.
- The `checkpoints.json` file in a node configuration lists only the block number and hash of a checkpoint but not its slot.
  This is the minimal data necessary for the node to enforce checkpoints; the slot is irrelevant.
- Your colleague sent a hash but not the slot.
  Remind your colleague to always send the slot too.

# The Known Point Must Be on the Node's Selection

None of these mechanisms support inspecting orphaned blocks.
Not even the power-user tools (`db-analyser` et al) support that.
Right now, the only option would be to rely on that fact that each file in the `volatile` folder is just the concatenation of the CBOR of some number of blocks, so you could search through them that way---no promises on the ordering.
The Consensus Team doesn't guarantee to maintain that fact, but it's unlikely to change anytime soon (for the actual Praos blocks, at least; Leios and its "super Praos blocks" is whole different story).

# Node-to-Node ChainSync (NTN ChainSync)

This protocol begins by sending the known point as the FindIntersect P message.
Then every MsgRollForward message iterates **forward** along the node's selection, returning each **next** header.
There will be many more MsgRollForwards than MsgRollBackward Q, but it just means to "discard" headers previously sent until you get back to a header with point Q.

The meaning of P argument in the FindIntersect message is a point that the sender _already has_.
That initialization step allows the mini protocol to investigate what **subsequent** points the remote node has, which makes sense, the sender claimed to already have P.

One heads up is due.
As a quirk, the first message after finding the intersection will be a seemingly degenerate MsgRollBackward to the intersection point.
That message is sent for the implementation's convenience, but it unfortunately confuses some people inspecting the traffic for the first time.

# Node-to-Node BlockFetch (NTN BlockFetch)

This protocol sends a request that specifies two points, the start and end of a closed interval (aka both given points are included in the interval).
The node either replies with "At least one of those points is not on my selection" or it sends multiple replies, where each is the next block in that interval on its selection.

If you just want one specific block, request the interval [P, P] where P is the point of that block.

# Node-to-Client ChainSync (NTC ChainSync)

This is the exact same as NTN ChainSync, except that it sends the next block instead of the next header.

# LocalStateQuery/`cardano-cli` (LSQ/`cardano-cli`)

This mechanism is different than the other two in some crucial ways.

- LSQ only works for points on a fixed-length suffix of the node's current selection.
  The suffix is exactly k+1 blocks long (unless the entire selection is shorter than that), where k is 2160 on mainnet.
- You can use LSQ with some special points implicitly, even if you don't know their actual slot and hash.
    - SpecificPoint.
      This argument uses your known point, if you already have it.
    - VolatileTip.
      This argument implicitly uses the youngest point on the suffix (ie the tip of the node's selection).
    - ImmutableTip.
      This argument implicitly uses the oldest point on the suffix.

Notably, one of the supported queries returns the acquired point, so you can use it to determine the node's volatile or immutable tip.
Once you have that point, you can then use it with the other mechanisms.

# Summary

Always start from some known point that is old enough but not excessively old.

Most of the time, a block's slot is already listed alongside its hash, so you already have a point.
To clarify, however: that point is only a _known_ point if it's on the node's current selection.

Then:

- To find the immutable or volatile tips of a node's selection, use LSQ.
- To iterate forward through the points on the node's selection, use NTN ChainSync.
- To iterate forward through the headers on the node's selection, use NTN ChainSync.
- To iterate forward through the blocks on the node's selection, use NTC ChainSync.
- To fetch specific intervals of blocks from the node's selection, use NTN BlockFetch.
- To learn some information about a specific point that the node is offering (eg useful functions of the ledger state), use LSQ.
