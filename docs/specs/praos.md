# Praos consensus

A model of the Ouroboros Praos consensus protocol as implemented in ouroboros-consensus.
The model captures how a network of nodes agree on a single chain despite forks and adversarial behavior.

**References:**
- Paper: [Ouroboros Praos][praos-paper] (Bernardo et al., 2018)
- Implementation: [`Ouroboros.Consensus.Protocol.Praos`][impl-praos]
- Tech report: [`docs/tech-reports/report/`][tech-report], chapters on consensus and chain selection

**Scope and simplifications.**
This is the first level of a multi-level specification.
It models the protocol at network level: nodes produce blocks, exchange chains, and select the best chain.
Several details are abstracted away and marked for later refinement:

- Leader election is nondeterministic (any node can lead any slot).
  A later refinement will add VRF-based stake-weighted election.
- Chain exchange is atomic (a node receives a full candidate chain in one step).
  A later refinement will decompose this into ChainSync and BlockFetch.
- Header validation is a predicate on block sequences.
  A later refinement will add the per-field checks (KES, VRF, nonce).

## Types

There is no genesis block.
A chain is a list of actual blocks; the empty list represents genesis.
The first block's `prevHash` is `GenesisHash`.
This matches the implementation, where `ChainHash` (defined in `Ouroboros.Network.Block`, re-exported by `Ouroboros.Consensus.Block`) distinguishes between genesis and a real block hash.

```quint praos.qnt +=
module Praos {

  type NodeId = int
  type Slot = int
  type BlockNo = int
  type Hash = int

  // Ouroboros.Network.Block:ChainHash
  type ChainHash = GenesisHash | BlockHash(Hash)

  type Block = {
    slot: Slot,
    blockNo: BlockNo,
    hash: Hash,
    prevHash: ChainHash,
    creator: NodeId
  }

  // A chain is a list of blocks from oldest to newest.
  // The empty list is genesis.
  type Chain = List[Block]

  // Cardano.Slotting.Slot:WithOrigin
  type WithOrigin = Origin | At({ slot: Slot, blockNo: BlockNo, hash: Hash })
```

## Constants

```quint praos.qnt +=
  // The security parameter.
  // A node never rolls back more than k blocks.
  const k: int
```

## Chain selection

A node selects a chain by comparing candidates to its current chain.
The rule: prefer the candidate if it is strictly longer and forks at most k blocks back.
On equal length, the current chain wins (the node does not switch).

This is a simplification.
In the implementation, equal-length chains are compared by a protocol-specific tiebreaker ([`TiebreakerView`][impl-tiebreaker]): for Praos, this is the VRF output of the tip block.
A later refinement will add this.

**References:**
- Implementation: [`Ouroboros.Consensus.Protocol.Abstract:preferCandidate`][impl-prefer]
- Implementation: [`Ouroboros.Consensus.Util.AnchoredFragment:preferAnchoredCandidate`][impl-prefer-anchored]

```quint praos.qnt +=
  // The tip (most recent block) of a chain.
  pure def tip(chain: Chain): WithOrigin =
    if (chain.length() == 0)
      Origin
    else
      val b = chain[chain.length() - 1]
      At({ slot: b.slot, blockNo: b.blockNo, hash: b.hash })

  // The block number of a tip, or -1 for genesis.
  pure def tipBlockNo(t: WithOrigin): BlockNo =
    match t {
      | Origin => -1
      | At(b) => b.blockNo
    }

  // The length of the common prefix of two chains.
  pure def commonPrefixLength(a: Chain, b: Chain): int =
    val minLen = if (a.length() < b.length()) a.length() else b.length()
    0.to(minLen - 1).filter(i => a[i].hash == b[i].hash).size()

  // How many blocks from the tip of `current` we must roll back
  // to reach the intersection with `candidate`.
  pure def forkDepth(current: Chain, candidate: Chain): int =
    current.length() - commonPrefixLength(current, candidate)

  // Whether a candidate chain is preferred over the current chain.
  // Prefer strictly longer valid chains that fork at most k blocks back.
  pure def preferred(current: Chain, candidate: Chain): bool = and {
    tipBlockNo(tip(candidate)) > tipBlockNo(tip(current)),
    forkDepth(current, candidate) <= k,
  }
```

## State

Each node maintains a local chain.
A message buffer models chains in transit between nodes.
A global clock tracks the current slot.

```quint praos.qnt +=
  const totalSlots: int
  const nodes: Set[NodeId]

  var chains: NodeId -> Chain
  var slot: Slot

  type Message = { sender: NodeId, recipient: NodeId, chain: Chain }
  var messages: Set[Message]

  // Counter for generating unique block hashes.
  var hashCounter: int
```

## Initialization

All nodes start with the genesis chain (empty).
The clock starts at slot 1.

```quint praos.qnt +=
  action init = all {
    chains' = nodes.mapBy(_ => List()),
    slot' = 1,
    messages' = Set(),
    hashCounter' = 0,
  }
```

## Block production

In each slot, zero or more nodes may produce a block.
Leader election is nondeterministic: any node may be elected in any slot.
In the real protocol, election is determined by a VRF with a stake-weighted threshold.
This refinement is deferred.

```quint praos.qnt +=
  action produceBlock(node: NodeId): bool = all {
    // The node must not have already produced a block in this slot.
    match tip(chains.get(node)) {
      | Origin => true
      | At(t) => t.slot < slot
    },
    val chain = chains.get(node)
    val prevHash = match tip(chain) {
      | Origin => GenesisHash
      | At(t) => BlockHash(t.hash)
    }
    val newBlock: Block = {
      slot: slot,
      blockNo: chain.length() + 1,
      hash: hashCounter,
      prevHash: prevHash,
      creator: node,
    }
    val newChain = chain.append(newBlock)
    all {
      chains' = chains.set(node, newChain),
      hashCounter' = hashCounter + 1,
      // Broadcast the new chain to every other node: one message per recipient.
      messages' = messages.union(
        nodes.exclude(Set(node)).map(recipient =>
          { sender: node, recipient: recipient, chain: newChain }
        )
      ),
      slot' = slot,
    }
  }
```

## Chain delivery

A node receives a candidate chain from the message buffer and applies chain selection.
If the candidate is preferred, the node adopts it.
The message is consumed either way.

```quint praos.qnt +=
  action deliverChain(node: NodeId): bool = all {
    messages.filter(m => m.recipient == node) != Set(),
    nondet msg = messages.filter(m => m.recipient == node).oneOf()
    val current = chains.get(node)
    all {
      chains' = if (preferred(current, msg.chain))
                  chains.set(node, msg.chain)
                else
                  chains,
      messages' = messages.exclude(Set(msg)),
      slot' = slot,
      hashCounter' = hashCounter,
    }
  }
```

## Slot progression

The clock advances by one slot.

```quint praos.qnt +=
  action advanceSlot: bool = all {
    slot < totalSlots,
    slot' = slot + 1,
    chains' = chains,
    messages' = messages,
    hashCounter' = hashCounter,
  }
```

## Step

The system step is a nondeterministic choice among all possible actions.

```quint praos.qnt +=
  action step = any {
    nondet node = nodes.oneOf()
    produceBlock(node),
    nondet node = nodes.oneOf()
    deliverChain(node),
    advanceSlot,
  }
```

## Properties

### All chains valid

Every node's chain has strictly increasing slots, consecutive block numbers, and matching hash pointers.
This is a sanity check on the model: if our actions produce invalid chains, the spec has a bug.

```quint praos.qnt +=
  pure def validChain(chain: Chain): bool =
    chain.length() == 0 or
    and {
      chain[0].prevHash == GenesisHash,
      chain[0].blockNo == 1,
      chain.indices().forall(i =>
        if (i == 0) true
        else and {
          chain[i].slot > chain[i - 1].slot,
          chain[i].blockNo == chain[i - 1].blockNo + 1,
          chain[i].prevHash == BlockHash(chain[i - 1].hash),
        }
      ),
    }

  val allChainsValid: bool =
    nodes.forall(n => validChain(chains.get(n)))
```

### Common prefix

For any two nodes, after dropping k blocks from each chain, one is a prefix of the other.
This is the central safety property of Praos.

**This property does not hold in the current model.**
The Praos paper assumes a network delay bound Δ: every message is delivered within Δ slots.
Our model has no such constraint — messages can sit in the buffer indefinitely while slots advance.
Two nodes can independently build parallel chains of length > k without ever exchanging messages.
To fix this, we need to add a Δ constant and constrain the step action to deliver messages within Δ slots.

```quint praos.qnt +=
  pure def dropLast(chain: Chain, n: int): Chain =
    if (n >= chain.length()) List()
    else chain.slice(0, chain.length() - n)

  pure def isPrefix(a: Chain, b: Chain): bool =
    a.length() <= b.length() and
    a.indices().forall(i => a[i].hash == b[i].hash)

  val commonPrefix: bool =
    nodes.forall(n1 =>
      nodes.forall(n2 =>
        val c1 = dropLast(chains.get(n1), k)
        val c2 = dropLast(chains.get(n2), k)
        isPrefix(c1, c2) or isPrefix(c2, c1)
      )
    )
```

```quint praos.qnt +=
} // module Praos
```

## Run configuration

A concrete instantiation for simulation: 3 nodes, k=2, 20 slots.

```quint praos.qnt +=
module PraosRun {
  import Praos(k = 2, totalSlots = 20, nodes = Set(0, 1, 2)).*
}
```

[praos-paper]: https://eprint.iacr.org/2017/573
[impl-praos]: https://github.com/IntersectMBO/ouroboros-consensus/tree/main/ouroboros-consensus-protocol/src/ouroboros-consensus-protocol/Ouroboros/Consensus/Protocol/Praos
[tech-report]: https://github.com/IntersectMBO/ouroboros-consensus/tree/main/docs/tech-reports/report
[impl-tiebreaker]: https://github.com/IntersectMBO/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Protocol/Abstract.hs#L104
[impl-prefer]: https://github.com/IntersectMBO/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Protocol/Abstract.hs#L265
[impl-prefer-anchored]: https://github.com/IntersectMBO/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Util/AnchoredFragment.hs#L168
