# Glossary

Notes on the use and maintenance of this glossary:

- Definitions are preceded by a semicolon (`;`), eg `;Slot` to make it easy to find the using `Ctrl-F`.
- When adding a new definition, add a semicolon in front of it. If the definition has synonyms, separate them by commas (prefixing a semicolon for each synonym).

## ;Active Slot Coefficient, ;f

The active slot coefficient represents the chance a given slot elects one of the registered stake pools.
In other words, the probability that each slot has any leaders.
It is usually denoted as $f$.

On the Cardano mainnet we have:

$$f = 1/20$$

So this means a 5% chance of electing one of the registered stakepools. Thus, under ideal circumstances, each slot has a 19/20 probability of having no leaders whatsoever.

In the implementation we assume $f$ won't change. Dealing with changing $f$ is a hard problem which requires proper research.

## ;Adversarial node

A node that exhibits any behavior beyond the honest behaviors. EG it doesn't mint when it's elected. EG it mints multiple blocks when elected. EG it mints its block "too" late. EG it mints its block "too" soon. EG it mints invalid blocks. EG it sends headers but withholds the block, etc, etc, etc.

## ;Adversary

Security analyses typically assume the worst-case, ie the behavior of all adversarial nodes as well as any environmental interference is concerted (eg the adversasy "controls" more than just nodes, eg natural arising network delays).

## ;Anchor

A chain fragment always has an anchor point, which is the predecessor of the oldest header/block on the fragment.

## ;Babbage era

The Babbage era is a major phase of the Cardano blockchain, identified as a [Shelley-based era](#shelley-based-eras), that succeeded the Alonzo era.
On mainnet, it corresponds to least major protocol version 7 and ends when the version increments past 8.
It uses the Ouroboros [Praos](#ouroboros-praos) consensus protocol.

## ;Block

In some slots, the protocol allows a block-producing node to contribute to the chain; it does so by minting a new block.
A block at least identifies its issuer, its own real point, and the hash of the preceding block.

## ;BlockFetch

Download block bodies corresponding to the candidate fragments, prioritizing whichever fragments the BlockFetch heuristics consider most important (chain length is a key factor).

## ;Block-producing node versus relay node versus client

A node is a party that can participate in the network.

- It can be a *block-producing node* if it has the necessary setup to mint blocks.

- If not, but if it is still intended to be connected to from other nodes, it is a *relay node*.

- If it is just used to get a local view of the state of the network, it is a *client node*.

## ;Block validity and ledger rules, ;ledger rules

A valid block satisfies the ledger rules in the context of the ledger state that resulted from the preceding block.

## ;Bootstrap peers

Trusted honest caught-up peers that are used for syncing before a Genesis-capable node is released. By default, these are public root peers.

## ;Byron era

The Byron era refers to the first implementation of the Cardano blockchain, which was introduced in September 2017.
On the [first implementation](https://github.com/input-output-hk/cardano-sl) it used the Ouroboros Classic consensus protocol. In the current Haskell implementation, the Byron Era uses the Permissive Ouroboros BFT consensus protocol, which is backwards compatible with the Ouroboros Classic protocol, relying on seven federated nodes for block production.

## ;Chain

A sequence of blocks.

## ;ChainDB

Wraps several other components and provides *chain selection* (ChainSel) functionality on top:

   - ImmutableDB - stores "the" honest immutable chain, i.e. all blocks that are known to no longer be subject to rollback.

   - VolatileDB - stores all blocks that could become part of an extension of the immutable chain

   - ChainSel - processes (stores, validates) incoming blocks and maintains the node's *selection*, which is anchored at the tip of the ImmutableDB, and will be of length `k` (unless near Genesis or due to VolatileDB corruption).

   - LedgerDB - stores a ledger state for all points on the node's selection.

## ;Chain growth property

This property states that there are at least $k$ blocks in $3k/f$ slots.

Here $f$ refers to the [active slot coefficient](#active-slot-coefficient-f).

The main Praos theorem also establishes a similarly-shaped bound on the probability of the immutable chain having less than `k` blocks in any contiguous run of `s` slots. The IOG researchers chose `s=3k/f` for Cardano. We started calling it `scg`, since `s` is a pretty common identifier.

## ;ChainSync

Chain sync is a mini protocol to exchange chains of headers.

   - maintains a *candidate fragment* of headers for each peer with the following properties (might not be satisfied transiently):

      - validated (detail: currently, it is *not* checked whether the headers come from the future, but that [might change soon](https://github.com/IntersectMBO/ouroboros-network/issues/4251)). Note that this places a bound on its length as header validation is limited by how far we can forecast.

      - selectable (ie forks off at most `k` from the node's selection).

  - The server can *claim to have no additional headers* when asked for updates. This is signaled explicitly via `MsgAwaitReply` or implicitly by the server's selection tip point that is sent with every message (though the latter is currently not used on the client for any logic).

## ;ChainSync Jumping

In the context of [Genesis](#genesis-consensus-protocol-paper-version), a node should execute [ChainSync](#chainsync) with all of its peers, thus often re-downloading and re-validating the same header from multiple peers. This would unnecessarily increase the network load on honest peers, which by definition will serve the same headers on the historical part of the chain.

To mitigate this load, we use ChainSync Jumping, an extended version of ChainSync in which a node only downloads headers from *one* of its peers, and periodically asks all the other peers if they agree with it. If there is a disagreement, ChainSync starts downloading headers from another peer until one of them gets disconnected.

### ;Dynamo

The dynamo is the only peer from which a syncing node gets all headers, and run the normal ChainSync protocol. Every once in a while, the syncing node sends a message to all of its jumpers to check if the tip of the dynamo's chain is also on their chain.

### ;Jumper

The jumpers are all the peers of a syncing node from which headers aren't downloaded with normal ChainSync protocol. The syncing node will periodically send the tip of its chain to all jumpers. If they confirm this tip is on their chain as well, the node assumes they have the exact same chain as the dynamo, up to their current tip. If they disagree with the tip of the dynamo, they might be promoted to objector.

### ;Objector

An objector is a peer which disagrees with the dynamo and from which headers are downloaded. When a jumper doesn't have the tip of the dynamo's chain, it might be promoted to an objector, and exchange headers with the syncing node as per ChainSync protocol. This lasts until either the objector or the dynamo gets disconnected. If the dynamo gets disconnected, a new dynamo is selected by the syncing node.

### ;Disengaged Peer

A disengaged peer is a peer from which headers are downloaded as per ChainSync protocol. It cannot be selected as a dynamo, and never becomes a jumper nor an objector.

A dynamo, an objector, or a jumper can be disengaged if they rollback their selection or if they claim to have sent all headers.

## ;Checkpointing

Solve dynamic availability by providing syncing nodes with (trusted) information: the points on the (immutable) honest chain every few (i.e. `≤ k`) blocks.

## ;Common Prefix

In particular, the main Praos theorem establishes an upper bound of `a*exp(-k*b+c)` for the probability that honest nodes in an Ouroboros net will disagree on more but the latest k blocks, despite an adversary with less than half of the stake and the ability to delay any and all blocks by up to `Δ` slots. On Cardano `mainnet`, `k=2160`.

## ;Delta, ;Δ

`Δ`, aka maximum delay - The Praos theorems assume that messages (aka "chains"!) arrive within `Δ` slots. IE every honest block minted in slot `S` and also its predecessors will have arrived at every node before the onset of slot `S+1+Δ`. `Δ=0` implies no message is delayed beyond the next slot, aka "synchronous". Remarkably, the node is completely unaware of `Δ`; it is only used in the security analysis.

Note that this implies in particular that (Praos has to assume that) there are no long-lived network partitions.

## ;Density

The density of a chain in a specific interval of slots is the number of blocks in that slots.

## ;Density interval

Due to the incremental nature of ChainSync, we often only know a *prefix* of the eventual chain inside of a Genesis window. Density intervals allow us to bound the density that extensions of the prefix could have eventually.

  - Inclusive lower bound `lo`: The number of blocks in the Genesis window (i.e. the density in the relevant Genesis window of the upstream peer's current candidate fragment).

  - Inclusive upper bound `hi`: `lo`, plus the number of slots in the Genesis window beyond the tip of the peer's current candidate fragment if the ChainSync server claims to have more headers in the Genesis window.

## ;Density rule

A rule to compare two chains `C` and `D`: prefer the chain that has the higher density in the interval after `C ∩ D` of length `sgen`.

## ;Eclipse attack

An attack on a blockchain (or any other peer-to-peer network) system in which malicious actors attempt to isolate one or several hones participants from the rest of the network. See [this paper](https://www.usenix.org/conference/usenixsecurity15/technical-sessions/presentation/heilman) for an example attack on Bitcoin.


## ;Election proof

A cryptographic proof that the [Ouroboros](#ouroboros) protocol did indeed assign the [block](#block)'s issuer to lead the block's [slot](#slot).

## ;Epoch

The sequence of slots is partitioned into a sequence of epochs, each of which is a contiguous run of a fixed number of slots.
In the latest Cardano era, each epoch lasts for 432000 slots (= 5 days).

## ;Epoch structure

The ledger rules take snapshots of the nonce and stake distribution at different points of time in the course of an epoch. A snapshot may only be _used_ when it has stabilized, which means that their block has become immutable (being older than `k` blocks). Currently, Cardano `mainnet` uses an epoch length of `10k/f` slots, divided into three parts:

Note that nothing in the implementation happens on the transition from Part 1 to Part 2 (in contrast to "from Part 2 to Part 3"), so there exist no concrete values for the individual length of these two phases. The length of Part 3 is however explicitly recorded in the implementation, so the length of Part 1 and Part 2 combined is `10k/f` minus the length of Part 3.

- Part 1, at least length `3k/f` - At the beginning of this part, which forms the boundary with the previous epoch, the stake distribution snapshot is taken.
  At the end of this part, the stake distribution has stabilized.

- Part 2, at least length `k/f` - At the end of this part, the nonce snapshot is taken.

  The nonce is snapshotted after the stake distribution to prevent _identity grinding_.
  However, this does not prevent _nonce grinding_.

  It must contain at least one honest block (created by an honest party as defined above), so that the nonce cannot be solely influenced by the adversary – otherwise they could identity grind before Part 1, knowing what the nonce would be due to owning all the blocks up to its snapshot.
  This criterion is a minimum requirement and wildly unrealistic not to be satisfied with `mainnet` parameters.

  The nonce snapshot could likely be taken earlier without sacrificing security, like already after `3k/f` after the start of the epoch.
  Waiting all the way until the start of Part 3 is playing it extra safe.

- Part 3, length `4k/f` since the Conway era, and `3k/f` for all prior Shelley-based eras - At the end of this part, the nonce snapshot has stabilized and can be used for the leader schedule of the next epoch.

  Most importantly, Part 3 has to be at least `3k/f` slots (one stability window) long for the nonce to stabilize before the start of the next epoch (such that all pools agree on the leader schedule).

  As advised by the IOG researchers, the nonce should be snapshotted even a bit earlier for intricate reasons related to Ouroboros Genesis. See erratum 17.3 in the [Shelley ledger specs](https://github.com/IntersectMBO/cardano-ledger/blob/master/README.md) for context.

## ;Eventual consensus

When Ouroboros runs as intended, all short forks are short-lived.

## ;Forecasting

Forecasting is the ability to validate headers that are ahead of a node's current selection.
Because of [Common Prefix](#common-prefix) and [Chain Growth](#chain-growth-property), the latest `k+1` ledger states along the node's selection always provide sufficient information for the node to validate its peers' headers that are no more than `3k/f` after the peer's first header.
Since the node hasn't selected that header's block, it has to use forecasting in order to validate its descendant headers.

### ;Forecast horizon

The forecast horizon is the number of slots ahead of its current selection in which a node can validate headers. With current Genesis parameters, it is `3k/f`, which is the [stability window](#stability-window) for [Shelley-based eras](#shelley-based-eras).

## ;Genesis block

The hypothetical [block](#block) that precedes the first real block, ie the empty [chain](#chain).
It's the beginning of _the_ chain: all [nodes](#node) in a net must agree on at least the genesis block; otherwise, they have no common ground.

## ;Genesis chain selection rule

The Genesis paper describes a new chain selection rule (`maxvalid-bg`) to use instead of the longest chain rule from Praos:

    - Let `C` be the currently selected chain and `D` be a candidate chain.
    - If `D` forks off from `C` at most `k` blocks deep: Compare `C` and `D` using the longest chain rule.
    - Otherwise: Compare them using the density rule.

## ;Genesis consensus protocol, paper version™

Praos, but using the [Genesis chain selection rule](#genesis-chain-selection-rule).
Note that this protocol requires rollbacks by more than `k` blocks!
We do not want to change that invariant at the moment.

## ;Genesis Density Disconnection Rule (GDD)

Disconnect from nodes whose fragments certainly lose to other fragments according to the density rule.

Motivation: allows the intersection of candidate fragments to progress

## ;Genesis State Machine (GSM)

Mechanism describing when a node can conclude that it is caught-up. This is used to avoid connecting to lots of ledger peers (for the HAA) when unnecessary and to disarm certain timeouts.

Key transitions:

  - Syncing -> CaughtUp: All peers claim to have no more headers (via ChainSync), and we selected one of the candidate fragments.

  - CaughtUp -> Syncing: Tip of our selection becomes older than X number of slots

## ;Genesis window

A number of slots `sgen`. Consider the honest chain `H`, and any other valid (but potentially adverserial) chain `A` such that their intersection is more than `k` blocks behind the tip of `H`.
Then the Genesis paper guarantees that `H` has a higher density than `A` in the `sgen`-many slots beginning immediately after their intersection.

The Genesis design uses `sgen = scg = 3k/f = 129600`.

## ;Grinding

A difficult to quantify behavior of the adversary whereby they leverage a lot of compute power to make choices so that they lead more slots in the corresponding later epoch than the `phi_f(α)` random variable would theoretically allow them.

## ;Hard forks

Moments in the life of a chain where old nodes cannot validate new blocks.
There is one at each era change but they can also happen within the same era; they are then called “intra-era” hard forks.
Intra-era hard forks are mostly ledger related, for instance to fix a bug of (de)serialisation of transactions, or to add a new smart contract feature.
Recently, hard forks have been given names: Vasil is the hard fork from Alonzo to Babbage; Valentine is an intra-era hard fork within Babbage.

## ;Hard-fork combinator

The Hard-fork Combinator (HFC) is a core architectural component of the Consensus layer designed to enable sequential composition of multiple blockchain eras (eg Byron, Shelley, Allegra, Babbage, etc.) so they can be managed as a single, unified chain type.
It is responsible for handling the complex logistics of transitions, including providing the necessary context for the underlying ledgers.
It facilitates the necessary translations between successive eras, such as translating the ledger state and chain dependency state during an era boundary

## ;Header-body split

The [stability window](#stability-window) enables the engineering design of nodes exchanging chains of headers before exchanging chains of the corresponding blocks.

## ;Header and body

The two key parts of a block. The consensus part of Ouroboros directly involves the header but it also indirectly depends on the body.

## ;Header validity

Boring part: parseable, strictly ascending slot number, issuer's crypto signature, etc.
Interesting part: the election proof. TODO also interesting: not from the future.

## ;Honest Availability Assumption

The assumption that any syncing node is always connected to at least one honest caught-up node.

Motivation: ensure that one of the candidate fragments is (a prefix of) the honest chain

The Networking teams plans to essentially satify this assumption by always maintaining a minimum number of [ledger peers](#ledger-peers) from a recent stake distribution.
The foundational Praos assumption of an honest majority then quickly implies the probability of no ledger peers is extremely low.

## ;Honest majority

Ouroboros assumes the adversary always controls strictly less than 50% stake.
Typically, the researchers bound it even lower than that. But Praos is 50% - ε, as long as eg `k` is large enough.

## ;Honest caught-up parties

All honest block-producing nodes that are fully synchronized to the state of the system; i.e. they act indistinguishably from nodes that have been there since the beginning of the system.

 - active parties - honest caught-up parties plus the adversary.

   [Genesis](miscellaneous/genesis_design.md) has two natural requirements which must be met throughout the system's lifetime:

    - The ratio `α` of honest caught-up parties over active parties is above `1/2`.

      Phrased differently: The honest caught-up parties have more stake than the adversary (historical honest majority assumption).

    - The ratio `β` of active over all parties is bounded from `0` by some constant.

## ;Honest node and selection

A node that follows the Praos specification.
Assuming infinite compute resources, this is abstractly summarized as: at the onset of every slot, select the longest valid chain you have received breaking ties arbitrarily, if you lead this slot mint a new block extending your selection, select it, and propagate it to the world.
On Cardano `mainnet`, the tiebreakers are carefully chosen and universal, but the Praos protocol doesn't technically constrain them.
There are many many more rules for an honest node (eg "put as many transactions as you can in the block you mint"), but that summary is the only crucial rule for Praos.

## ;Identity grinding

This is an unofficial term. If an adversary knew the nonce snapshot before the stake distribution was snapped, they could use infinite compute resources to find a private key that optimizes the leader schedule, and transfer their stake before the snapshot.

This cannot occur on Cardano thanks to the epoch structure: the nonce snapshot is taken after the stake distribution snapshot.

## ;Immutable chain

The immutable chain of whichever well-connected and caught-up honest node has the worst current selection.

## ;Immutability/stable/settled/etc

Because of Common Prefix, no honest Praos node will ever need to discard more than k blocks (when switching to a longer chain). Thus, any block with at least `k` descendants on the node's selection is immutable. So is that block's contents (eg its transactions).

## ;In-future check

As nodes can not be expected to have perfectly synchronized clocks (ball park: <=100ms globally), blocks that are early but within the *maximum permissible clock skew* (currently: 5s) are not immediately considered invalid during ChainSel, but are instead stored and reprocessed immediately before the next time ChainSel is triggered by a newly added block.

## ;Independent aggregation

`ϕ` was chosen in part because `ϕ(x+y) = 1 - (1 - ϕ(x))*(1 - ϕ(y)) = ϕ(x) + ϕ(y) - ϕ(x)*ϕ(y)`. Therefore reapportioning stake amongst the stake pools you control doesn't alter your chance of leading a slot.

## ;Issuer

Who can make blocks. Every issuer runs a node, but not every node is an issuer. We distinguish _block-producing nodes_.

## ;Issuer's leader schedule

Which slots a specific issuer leads is private information, until the issuer shares an election proof. (This privacy itself is an important feature.)

## ;Leader schedule

Which issuers the protocol assigns to lead each slot in an epoch; there can be zero, one, or many leaders in each slot.
This omniscient view of the schedule only exists theoretically, because of crypto.

## ;Ledger eras

The sequence of ledger rule sets used by today's node to validate the respective segments of the historical Cardano chain.
Byron, Shelley, Allegra, Mary, Alonzo, Babbage (current one as of August 2023), Conway.
Conway is currently (August 2023) under development.

## ;Ledger peers

Relays selected to be peers, sampled according to a sufficiently-recent stake distribution.

## ;Ledger state

The accumulated result (of a [prefix](#prefixtipetc-and-fragment-chain-fragment)) of a [chain](#chain) (which necessarily started from the [genesis block](#genesis-block)).
The genesis block is not actually a block; it's the first ledger state.

## ;Ledger view

The small part of the ledger state actually required for header validation.
It's the same part whether or not the validation is done via forecasting.

## ;Limit on Eagerness (LoE)

Do not select more than `k` blocks past the intersection of all candidate fragments.

Motivation: ensure that we can still switch to any candidate chain while respecting the maximum rollback limit.

### ;LoE Fragment

The common prefix of all candidate fragments, anchored at the tip of the current chain selection

### ;LoE Tip

The tip of the LoE fragment

### ;Long-range Attack

An attack on Proof-of-Stake blockchain systems that refers to the ability of a minority set of stakeholders to execute the blockchain protocol starting from the genesis block (or any sufficiently old state) and produce a valid alternative history of the system. See [this blog post](https://blog.ethereum.org/2014/05/15/long-range-attacks-the-serious-problem-with-adaptive-proof-of-work) by Vitalik Buterin and [this paper](https://www.iog.io/papers/stake-bleeding-attacks-on-proof-of-stake-blockchains) for details.

## ;Limit on Patience (LoP)

Disconnect peers that advertise a better header than their current one, but haven't provided it quickly enough.

Motivation: because of the [LoE](#limit-on-eagerness-loe), we don't select headers past `k` blocks beyond the common intersection of all candidate fragments. Therefore, an attacker could prevent a syncing node from making progress by stalling indefinitely at an intersection, promising headers to extend their chain but never delivering them. The LoP mitigates that attack.

In the current version of Genesis, the limit on patience is implemented by a "leaky token bucket" algorithm.

### ;Token Capacity (TCAP)

The LoP bucket has an initial and maximal token capacity of `TCAP`. A token is gained when the associated peer sends a header with a higher block number than its previous one. When the bucket is already at maximum capacity, extra tokens are lost.

### ;Token Drip Rate (TDRIP)

When the peer has advertised a header with a higher block number than its previous one, but hasn't served yet, the LoP bucket loses tokens at a rate of `TDRIP` tokens per second. When the bucket is empty, the associated peer has exhausted its limit on patience, and gets disconnected.

## ;Local root peers

Locally configured peers that the node should know and try to maintain a connection with. These are usually block-producing nodes or relay nodes that SPOs setup in order to shield their block-producing nodes from direct contact.

## ;Long-range attack

An adversary presents to a syncing node a chain that forks from the honest chain far in the past, in order to prevent the node from ever selecting the honest chain.

  - Superficial variant: An adversary, even with very low stake, can *eventually* produce very long (i.e. longer than `k`) forks. If a syncing node is served this chain before the honest chain, the "maximum rollback" engineering decision implies that the node can never switch away from it.

  - Fundamental variant: After some time (multiple epochs), an adversary will be able to create blocks on its fork much faster (due to accumulated block rewards/governance) than the honest chain grows. Because it's actually the longest chain in the system, the theoretical Praos node---and also the real node, if patched to allow unlimited rollback---would select this adversarial chain.

## ;Maximum rollback

The node relies on the exact value of [`k`](#security-parameter), at least for engineering purposes.
By appeal to [Common Prefix](#common-prefix), the node design is incapable of discarding its selection's immutable prefix (eg this bounds the honest node's RAM requirements).
If a peer tries to send blocks that diverges deeper than that, the node disconnects from them.

## ;Mini protocol

Nodes communicate with other nodes via a set of two-party protocols in which one node is the *client* and the other is the *server*.

   - The dynamics are specified by a state machine, which is enforced by the [typed-protocols](https://github.com/input-output-hk/typed-protocols) library. [List of all mini protocol state machines](https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf#chapter.3).

   - The client can *pipeline* its messages.

   - All node-to-node mini protocols are pull-based.

   - Each directed edge in the node topology is a bundle of (multiplexed) mini-protocol instances.

## ;Node

## ;Node's immutable chain

The prefix of the node's selection that excludes the youngest `k` blocks.

## ;Nonce

The [ledger state](#ledger-state) maintains a nonce, updated by each block's header, independent of the block body. The ledger takes a snapshot of this nonce once per epoch. The snapshot taken during one epoch is used by VRFs in the next epoch.

## ;Nonce grinding

When the nonce is the only undetermined leader schedule input left, an adversary leading the last slots before the snapshot could compute the nonce for all permutations of blocks and pick the one that optimizes the leader schedule.

This is possible on Cardano, though suspected to be uncommon.

## ;Net

A network of nodes running [Ouroboros](#ouroboros) to continually grow a single agreed upon chain.

## ;Opposite of a short fork

We don't really have a word for this; it's when "something went wrong", and a [short fork](#short-forks) is so long it's problematic.

## ;Ouroboros

The family of consensus protocols that underly Cardano.

## ;Ouroboros Classic

(and/or (?) Ouroboros BFT) The protocols the older Cardano nodes actually used during Byron.

## ;Ouroboros Genesis

A slight refinement of [Praos](#ouroboros-praos).

## ;Ouroboros Praos

The protocol underlying the latest Cardano era.
It was the first proof-of-stake protocol designed and formally proven to provide security against fully-adaptive corruption in a setting that tolerates adversarial message delays (up to Δ slots).
Praos implements slot leader election using stake distribution and Verifiable Random Functions (VRF), and honest parties converge to a unique chain view primarily by following the longest chain rule.
The name "Praos" means "mellow" or "gentle," derived from the protocol's use of deliberately inserted empty slots to facilitate synchronization in the semi-synchronous environment.

## ;Peer kinds

These kinds are maintained by the Networking layer:

- [Bootstrap peers](#bootstrap-peers).
- [Ledger peers](#ledger-peers).
- [Local root peers](#local-root-peers).
- [Public root peers](#public-root-peers).
- [Shared peers](#shared-peers).

## ;Peras ;weight ;boost

Peras is an extension of Praos enabling faster settlement under optimistic conditions.
To this end, Peras can result in a block `B` receiving a *boost*, which means that any chain containing `B` gets additional weight when being compared to other chains.

Consider a chain fragment `F`:

- Its ;*weight boost* is the sum of all boosts received by points on this fragment (excluding the anchor). Note that the same point can be boosted multiple times.

- Its ;*total weight* is its tip block number plus its weight boost.

Note that these notions are always relative to a particular anchor, so different chain fragments must have the same anchor when their total weight is to be compared.

## ;Permissive BFT (PBFT)

Ouroboros Permissive BFT (PBFT) is a simple, deterministic, Byzantine Fault Tolerant (BFT) consensus protocol derived from Ouroboros-BFT, typically designed to ensure consistency and liveness against `t < n/3` Byzantine faults.
The protocol is deemed "permissive" because it relaxes the strict requirement that blocks must be signed according to a predetermined round-robin schedule.
Instead, blocks are merely required to be signed by any of the known core nodes.
However, this permissiveness is still bounded: the protocol limits the number of signatures a given node can contribute within a window of blocks.


## ;Phases

Byron, Shelley, Goguen (current one as of August 2023), Basho, Voltaire.
Part of [the Cardano roadmap](https://roadmap.cardano.org/en/), also regularly called “eras”.
Voltaire might happen before Basho.

## ;Point

`Point` is essentially `Maybe RealPoint` where the `Nothing` case identifies the [genesis block](#genesis-block).

## ;Prefix/tip/etc and fragment, ;chain fragment

Any prefix of a [chain](#chain) is also a chain; suffixes and infixes are instead called _chain fragments_.
Because (valid) blocks uniquely identify their predecessors, the prefixs and blocks of a chain are one-to-one.

## ;Protocol eras
The sequence of protocols used by today's node to validate the respective segments of the historical Cardano chain.
Permissive BFT, Transitional Praos (aka. TPraos), Praos.

## ;Public root peers

Peers coming from trusted public information (IOG relays, Cardano foundation also hosts similar nodes).

## ;Real point

The slot number and hash of a block.

## ;Relative stake and phi

Relative stake and `ϕ`, aka phi: If some stake pool contains `U` ada in the stake distribution from epoch number `E`, and that whole distribution sums to `V` ada, then that pool leads each slot in epoch `E+2` with an independent probability of `ϕ_f(x) = 1 - (1 - f)^(x)` for `x = U/V`, the pool's relative stake

## ;Security parameter

`k`, aka "_the_ security parameter": A parameter of the Common Prefix theorem that bounds how many blocks would exist on a well-connected honest node's selections after its intersection with another well-connected honest node.

## ;Swept into the adversarial budget

Obviously, some messages on a real network will exceed `Δ` or even be actually lost. In that case, the security analysis considers the node to be adversarial, "sweeping" it under the rug, consuming a little bit of the analysis's budget. Other things get swept there too, but we try not too.

## ;Shared peers

Peers that were discovered via the Peer Sharing protocol. These can be any kind of peer, e.g. ledger peers or client/wallet peers.

## ;Shelley-based eras

All eras apart from Byron share most of their code.
They are sometimes just referred as “Shelley” eras.
[For instance, the `ShelleyBlock` type has two arguments: protocol and actual era, giving eg. `ShelleyBlock (TPraos c) (AllegraEra c)`](https://github.com/IntersectMBO/ouroboros-consensus/blob/4c0a421d187478f9f740d27521b682988d843596/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/Block.hs#L97-L106).

_For a table on phases, eras, protocols, etc., see [the Cardano features table in CIP 59](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0059/feature-table.md)_.

## ;Short forks

If a [slot](#slot) has multiple leaders or if the leader of a slot hadn't received the latest block, then they will issue multiple blocks that all claim to have the same predecessor.
These competing chains are called short forks, unless they grow too long.

## ;Slot

Ouroboros discretizes time into a sequence of slots. In the latest Cardano era, each slot lasts one second.

## ;Stability window

The node relies on the exact value of `scg`. Specifically, the ledger rules prevent any block from influencing the leader schedule until at least `scg` slots later. Note that a block thus cannot influence the leader schedule until it's immutable.

## ;Stake and delegation

In Praos, stake pools are the block issuers. Any party that controls ada can _delegate_ it to a _stake pool_.

## ;Stake distribution

The ledger state maintains the amount of stake controlled by every issuer (every issuer is registered). The ledger takes a snapshot of this distribution at the boundary between two epoches. The snapshot taken at the boundary of epochs `e` and `e+1` is used by VRFs in epoch `e+2`.

## ;Stake pool operator (SPO)

Some entity that maintains/controls one or multiple stake pools.

## ;Syncing

The process of becoming synchronized with the system, either from scratch or due to a temporary restart/crash/local network outage.

## ;TPraos

TPraos (Transitional Praos) is a Proof-of-Stake consensus protocol used in the early decentralized eras of Cardano (specifically Shelley, Allegra, and Mary).
It was designed to manage the transition from the initial federated system by using a decentralization parameter (`d`) which controls the ratio of slots produced by legacy bootstrap keys (OBFT slots) versus those produced by stake pools selected via Ouroboros Praos, allowing for a smooth shift toward full decentralization.

## ;Transactions

If a slot has multiple leaders or if the leader of a slot hadn't received the latest block, then they will issue multiple blocks that all claim to have the same predecessor.A [block body](#header-and-body) is just a sequence of transactions.
-Each one modifies the [ledger state](#ledger-state) in a way determined by the [ledger rules](#block-validity-and-ledger-rules-ledger-rules).

## ;Valency

The number of peers the node maintains a connection to.

## ;Verifiable Random Functions, ;VRF

Verifiable Random Functions are used to ensure that even the proofs don't leak the issuers' private credentials. The VRF's output is determined by the issuer's private credentials, the slot number, and the nonce and stake distribution of the epoch that contains that slot.
