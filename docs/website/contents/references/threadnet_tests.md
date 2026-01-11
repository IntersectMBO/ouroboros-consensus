# ThreadNet tests

The ThreadNet tests spin up a network of nodes, but they are all within a single process --- a network of threads.

The purpose of the ThreadNet tests is to run a simple testnet within a Haskell process, and leverage the QuickCheck infrastructure to vary the parameters of the testnet. The following basic ideas apply:
- all the nodes are honest --- can a few honest nodes build a chain together?
- The test have an ability to partition the network, in order to check that the node can recover from the network partition. The test only supports an "easy" case of partition, half-in-half.
- The leader schedule is not prescribed, but rather evolved naturally as it would in the real system.

## Enumeration and description of the ThreadNet tests

### Shelley era crossing tests

These exercise an era transitions between two consecutive Cardano eras.

We currently the following Shelley-based era crossing tests:

- [Test.ThreadNet.ShelleyAllegra](./../../../../ouroboros-consensus-cardano/test/cardano-test/Test/ThreadNet/ShelleyAllegra.hs)
- [Test.ThreadNet.AllegraMary](./../../../../ouroboros-consensus-cardano/test/cardano-test/Test/ThreadNet/AllegraMary.hs)
- [Test.ThreadNet.MaryAlonzo](./../../../../ouroboros-consensus-cardano/test/cardano-test/Test/ThreadNet/MaryAlonzo.hs)

The test scenario is roughly as follows:

- generate credentials of several core nodes.
- Craft a protocol version update transaction that is signed by all core nodes. The meaning of the transaction is "every node proposes to increment the protocol version".
- Spin up a network of core nodes.
- Repeatedly submit the update transaction to the mempool of every core node, so that it end ups in the first minted block.
- Wait for several slots
- Stop the nodes, examine their final chains, and make sure the hard fork has happened.

### Shelley test that updates the decentralisation parameter

- [Test.ThreadNet.Shelley](./../../../../ouroboros-consensus-cardano/test/shelley-test/Test/ThreadNet/Shelley.hs)

This test has a structure similar to the Shelley era crossing tests, but actually does not cross between eras, but rather updates the decentralisation parameter.

### Test that crosses from Byron to Shelley, the "Cardano" test

- [Test.ThreadNet.Cardano](./../../../../ouroboros-consensus-cardano/test/cardano-test/Test/ThreadNet/Cardano.hs)

The `Test.ThreadNet.Cardano` module contains the test that crosses from Byron to Shelley. Notably, it uses the Cardano block type, rather than the more specialised two-era `ShelleyBasedHardForkBlock`. Otherwise the flow of the test is very similar to the Shelley-based era crossing tests described above.

### Byron tests

There are two more ThreadNet tests that test the Byron-era consensus code:

- the [Test.ThreadNet.Byron](./../../../../ouroboros-consensus-cardano/test/byron-test/Test/ThreadNet/Byron.hs) tests an ability of the nodes to build a chain using the old BFT protocol and also a bunch of Byron-specific chain properties related to epoch boundary blocks (EBBs).
- The [Test.ThreadNet.DualByron](./../../../../ouroboros-consensus-cardano/test/byron-test/Test/ThreadNet/DualByron.hs) test runs the Byron ledger and the Byron specification in lockstep, verifying that they agree at every point.

These are very old tests that are mostly irrelevant today.

### Mock block tests

There are four ThreadNet tests that use a mock block, rather than a real Shelley block:

- [Test.ThreadNet.BFT](./../../../../ouroboros-consensus-diffusion/test/mock-test/Test/ThreadNet/BFT.hs) --- tests convergence of the Byron-era BFT consensus.
- [Test.ThreadNet.PBFT](./../../../../ouroboros-consensus-diffusion/test/mock-test/Test/ThreadNet/PBFT.hs) --- tests convergence of the Shelley-era Permissive BFT (PBFT) consensus.
- [Test.ThreadNet.Praos](./../../../../ouroboros-consensus-diffusion/test/mock-test/Test/ThreadNet/Praos.hs) --- tests convergence of Praos.
- [Test.ThreadNet.LeaderSchedule](./../../../../ouroboros-consensus-diffusion/test/mock-test/Test/ThreadNet/LeaderSchedule.hs) --- looks very similar to the Praos test, but I don't know what exactly it tests.
