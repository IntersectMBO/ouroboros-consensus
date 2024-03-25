# UTxO-HD for the general public

This document aims to provide a comprehensible explanation of what UTxO-HD is
and what it means for the end users of the Cardano blockchain.

UTxO-HD addresses the problem of the Cardano node consuming a large amount of
memory. It does not add any user-facing functionality, therefore there will not
be many technical details in this document, instead feel invited to read the
[documentation for developers](./for-developers.md) for more information.

# Why UTxO-HD

Over time, `cardano-node` memory usage has been steadily increasing.
Synchronizing with mainnet from scratch requires around 16GBs of memory at its
peak. Not every user of the Cardano blockchain has a computer with that much
memory and therefore it becomes necessary to reduce the memory usage of the
running node.

This poses two main reasons for this project:

1. To promote decentralization of the network, more users should be able to
   participate actively. Nodes running on a Raspberry Pi or shared instances
   should be capable of keeping up with the network. Disk storage is cheaper
   than memory and as long as the overall performance of the network is not
   impacted, these machines should be perfectly acceptable to run a node.

2. The memory consumption of a node will be in principle forever increasing as
   more data gets into the blockchain. The UTxO set can grow and so can other
   data structures such as delegations or rewards. UTxO-HD opens the door for
   moving part of this data to the disk, which in the long run will aid with the
   sustainability of Cardano.

The first step towards reducing the memory required by a Cardano node is to move
the UTxO set into a disk-backed storage. This effectively lowers the memory by
5GB (tentative numbers shown by ad-hoc benchmarks, should not be considered
exact). However, this comes at a cost both in speed performance and in code
complexity.

We have implemented two different backends for this feature:

- `InMemory`: enabled by default. Instead of using the disk, it uses an
  in-memory storage to keep the UTxO set. It is roughly as fast as the
  pre-UTxO-HD node, but it doesn't contribute to lowering the memory, as the
  data retained is roughly the same. Intended to be used by users with powerful
  machines.
- `LMDB`: enabled by the `--lmdb-ledger-db-backend` flag for the node. Uses a
  [LMDB](http://www.lmdb.tech/doc/) backend which stores data on disk thereby
  lowering the node's memory usage of the node. It is slower as it has to
  perform I/O to get the data, but it greatly reduces the memory consumption of
  the node.

# Impact of the solution

We have measured the performance of the node after implementing UTxO-HD and we
have observed 4 different types of impact depending on the operation performed:

## Syncing impact

The syncing operation of the node takes place when a node joins the network
after being offline for some time or when it joins for the first time. The node
has to download the chain from other peers and validate each block and apply it
to its local copy of the ledger state.

In this front, the speed performance is not heavily impacted as most of the time
is consumed either in block cryptographic validation or in the network transfer
of data.

## Replaying impact

The replay operation happens when a node is shutdown ungracefully, the data in
the disk is corrupted, or the serialization of some data changes (like changes
in the ledger state serialization format). In this case the blocks are already
available in the local storage and the application of blocks skips some costly
validation checks because the node can safely assume that the blocks come from a
previous sync and therefore knows that they were already validated previously.

In this case, some replay speed is lost because of the I/O operations to provide
the Ledger rules with the necessary inputs and because of the accounting of the
changes in the UTxO set. However, replaying is something that should happen only
on rare occasions.

## Impact on a passive running node

When running a non block producing node that is caught up with the network, the
current throughtput of blocks is roughly 1 every 20 seconds, so the node has
plenty of time to perform I/O to fetch inputs and apply the blocks. In this
case, a pre-UTxO-HD node doesn't run any faster than the UTxO-HD node as both
nodes can easily process one block per twenty seconds, as opposed to syncing and
replaying which are operations whose aim is to process a big pool of data.

## Impact on a block producer node

There is a slight difference when the node is a block producer, as those also
manage a mempool to validate transactions. These transactions also have to go
through the same workflow as block application and therefore need to perform I/O
to fetch arbitrary inputs from disk. There is in principle some slowdown in this
case. However, we put effort into reducing this impact and by maintaining cached
read views of the disk data. This ensures that the node can function properly. A
small impact is expected, but in general (given that blocks are produced every
20 seconds) there is enough time for the node to fetch data from disk such that
the impact should not be very noticeable.

# Future protocols of Cardano

With the implementation of [Ouroboros
Leios](https://iohk.io/en/research/library/papers/ouroboros-leios-design-goals-and-concepts/),
blocks will be produced faster than they are currently, so there arises a need
to use more advanced solutions to saturate the throughtput of SSDs and maximize
parallelism when operating on the changes to the UTxO set (or other data
structures).

For this goal, there is ongoing development to produce an [LSM
tree](https://en.wikipedia.org/wiki/Log-structured_merge-tree) backend
implementation that will provide us with faster access to the data. This is
mainly achieved by tailoring the implementation to the needs of the node. There
is also the prospect of using pipelining of blocks and fetch of inputs to speed
up the operations.

# Querying the node

*Note: this section is still under discussion, so take it with a grain of salt,
this is just my own wording (@jasagredo).*

There is no need for the node to provide fast access to the ledger state data,
at least not for its own goals, but it is true that it comes in handy.at least
not for performing its core function, but nevertheless it comes in handy when
providing additional functionality. As much as possible, the node aims to
provide this access, but due to the way the data is processed, there is a
particular query that has become much more expensive than before:
`GetUTxOByAddress`.

Before UTxO-HD, this query needed to traverse the whole UTxO set, filtering for
the UTxOs owned by a particular addres. Now this has to be done by querying the
disk, which makes this quite costly.

This is an undesirable effect of UTxO-HD that will result in a need of an
external indexer service that keeps a table of UTxOs indexed by addresses. This
service still doesn't exist (see
[#4678](https://github.com/input-output-hk/cardano-node/issues/4678)). We
acknowledge that SPOs use this query, and this will probably impact them.

When moving some other data to the disk (in future versions of UTxO-HD), it
becomes evident that this could happen with other queries. We will work closely
with the community and SPOs to mitigate the impact this might cause to them.

# What does UTxO-HD entail for end users

Summing up what was said above, as there are several different end users of the
Cardano blockchain the degree of impact for each one of them differs:

- Full nodes will have a slight impact when starting the node and joining the
  network, which should clear up once the node is caught up.

- SPOs and wallets will see an impact in the `GetUTxOByAddress` query as
  discussed above.

- SPOs with powerful machines will observe a slight impact on the performance of
  the node, which should not degrade their activity on the network.

- SPOs with more modest machines will be able to run the node, improving the
  decentralization of the network.

- Passive nodes not producing blocks should not see a big impact on the
  adoptions of blocks that flow through the network.

- Wallets should not be impacted by this solution (more than the impact that the
  node it is connected to might see, by the points above).

