# Era Transition Goverance

This document explains how on-chain governance ends each era within Cardano, with a particular emphasis on testnets.

Every era ends when on-chain governance increments the major protocol version past the greatest within that era.
According to [the table maintained alongside CIP-59](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0059/feature-table.md), those versions are as follows.

| Era | Least major version | Greatest major version |
| - | - | - |
| Byron | 0 | 1 |
| Shelley | 2 | 2 |
| Allegra | 3 | 3 |
| Mary | 4 | 4 |
| Alonzo | 5 | 6 |
| Babbage | 7 | 8 |
| Conway | 9 | >= 10 (Conway is the latest era at time of writing) |

All eras forbid proposals to increase the major protocol version by more than one at a time.
Thus the governance process within those eras (Byron, Alonzo, Babbage, and Conway, so far) that contain multiple major protocol versions must take place multiple times.
In turn, for example, at least the first two epochs of any Cardano chain will be in the Byron era -- see the "Initializing the Chain with a Later Protocol Version" section below for the one exception.

## Byron Era

The relevant Byron update proposal increments [the major component of the protocol version](https://github.com/IntersectMBO/cardano-ledger/blob/f5b35692b4d92d664187781a98b7af3fab445bad/eras/byron/ledger/impl/src/Cardano/Chain/Update/Proposal.hs#L219).
As with all Byron governance, it's a six stage process on-chain.

- Event 1.
  Some genesis key delegate registers the update proposal at least `4k` slots before the first slot intended to use the incremented protocol version.
  (On mainnet, `k=2160`, but that value can be different on testnets.)
- Event 2.
  Enough of the genesis key delegates vote for the proposal.
  The exact quorum threshold is a protocol parameter; testnets can ignore the particular value by simply having all keys vote for the proposal.
- Event 3.
  At least `2k` slots pass after Event 2.
- Event 4.
  Enough genesis delegates then extend the chain with a block that endorses the proposal, by setting their [`headerProtocolVersion` field](https://github.com/IntersectMBO/cardano-ledger/blob/f5b35692b4d92d664187781a98b7af3fab445bad/eras/byron/ledger/impl/src/Cardano/Chain/Block/Header.hs#L158-L159) to the proposed version.
  The validation for this field is lax, so testnets can simply set it for all Byron blocks to the Shelley protocol version (ie the one in the proposal that ends Byron).
- Event 5.
  At least `2k` slots pass after Event 4.
- Event 6.
  The next epoch transition will adopt the proposal, thereby incrementing the major protocol version.

The proposal for an increment and each vote for that proposal must each be separate transactions, due to the limited expressiveness of a Byron transaction.
Testnets when possible should introduce those transactions in the first slot of whichever epoch is intended to be the last with that protocol version, to maximize the probability that they enter the mempools and are included in a block soon enough, for example.

## Shelley, Allegra, Mary, Alonzo, and Babbage Eras

These eras all use [an update proposal that increments the major component of the protocol version](https://github.com/IntersectMBO/cardano-ledger/blob/f5b35692b4d92d664187781a98b7af3fab445bad/libs/cardano-ledger-core/src/Cardano/Ledger/Core/PParams.hs#L355).
The stages on-chain are simpler than for Byron.

- Enough of the genesis key delegates make the same proposal at least `6k/f` slots before the first slot intended to use the incremented protocol version.
  (On mainnet, `k=2160` and `f=1/20`, but those values can be different on testnets.)
  The exact quorum threshold is a protocol parameter; testnets can ignore the particular value by simply having all keys propose the increment.
  (Voting is no longer distinct from proposing.)
- The next epoch transition will adopt the proposal, thereby incrementing the major protocol version.

Unlike Byron, all of the proposals for one increment can be introduced in a single transaction.
Testnets when possible should introduce that transaction in the first slot of whichever epoch is intended to be the last with that protocol version, to maximize the probability that it enters the mempools and is included in a block soon enough, for example.

*Remark*.
The function that constructs the initial Shelley ledger state from the final Byron ledger state actually ignores the incoming state's protocol version.
This function has other inputs, principally the given Shelley Genesis file, which includes a protocol version.
Note that the Byron Genesis file does _not_ include a protocol version (no one has ever needed Byron to start from a protocol version other than 0).

## Conway Era

The Conway era uses a dedicated [`Hard Fork Initiation` governance action](https://github.com/IntersectMBO/cardano-ledger/blob/f5b35692b4d92d664187781a98b7af3fab445bad/eras/conway/impl/src/Cardano/Ledger/Conway/Governance/Procedures.hs#L805-L810) to increment the major protocol version.

See [CIP-1694](https://cips.cardano.org/cip/CIP-1694) and/or its [formal specification](https://github.com/IntersectMBO/formal-ledger-specifications/) for the stages of the governance process, specifically the dynamics (eg dedicated quorum threshold) of "Hard Fork Initiations".

## Examples of On-Chain Governance in TestNets

A few teams within IOG already have test suites that involve on-chain governance.

- `cardano-node-tests`.
  The SDET Team maintains a set of tests that involve a local cluster of nodes building a Cardano chain that has some blocks in every Cardano era.
  [Their script]( https://github.com/IntersectMBO/cardano-node-tests/blob/05d0c4989d3d1bbebb7af894b4c5f9ba9fd32a25/cardano_node_tests/cluster_scripts/conway/start-cluster) demonstrates how to use `cardano-cli` commands to construct, sign, and submit the necessary transactions for every era except Byron.

  The Byron era there is currently termninated after one epoch via a `TestShelleyHardForkAt: 1` configuration setting, but the Consensus Team is currently working to [remove that functionality](https://github.com/IntersectMBO/ouroboros-consensus/issues/416).
  Byron on-chain governance as described in this document will replace it, although it will require two epochs, one each to reach protocol versions 1 and 2.

- `cardano-db-sync`.
  The DB Sync Team creates a "mock" chain to use when testing their tool.
  [This test](https://github.com/IntersectMBO/cardano-db-sync/blob/0f1d93f9b868caaf14c6cd0e77991e63c07c067f/cardano-chain-gen/test/Test/Cardano/Db/Mock/Unit/Conway/Governance.hs#L451) raises the protocol version within Conway.
  And [this test](https://github.com/IntersectMBO/cardano-db-sync/blob/0f1d93f9b868caaf14c6cd0e77991e63c07c067f/cardano-chain-gen/test/Test/Cardano/Db/Mock/Unit/Conway/Other.hs#L447) raises the protocol version within Babbage.
  The linked definitions use more combinators defined elsewhere in that codebase, but it's only a few indirections until the definitions start using the `cardano-ledger` interface directly.
  Do note that these transactions have no witnesses, which can be sufficient for the testing of a tool that trusts its upstream node.

- `ouroboros-consensus`.
  The Consensus Team wrote tests shortly before the end of the Byron era that tested the mechanism that would transition to Shelley.
  In particular, [the `mkProtocolByronAndHardForkTxs` function](https://github.com/IntersectMBO/ouroboros-consensus/blob/a9a5f3aaf3ddd45b3dd58a132d65b657bbf285e5/ouroboros-consensus-cardano/src/unstable-byron-testlib/Test/ThreadNet/Infra/Byron/TrackUpdates.hs#L318-L363) and the `mkHardForkProposal` function defined just below it use the `cardano-ledger` interface for Byron in order to create the transactions for the proposal and votes --- each with the necessary witnesses/signatures --- in order to increment the protocol version via Byron on-chain governance.
  Those functions are used within the Consensus Layer's "ThreadNet" tests, which submit the transactions listed in the `tniCrucialTxs` field at every possible opportunity.
  That degenerate persistence and eagerness reliably ensures that the chain increments the protocol version as soon as possible.

  A similar ThreadNet test transitions from Shelley to Allegra; its `tniCrucialTxs` field is created by [this function](https://github.com/IntersectMBO/ouroboros-consensus/blob/a9a5f3aaf3ddd45b3dd58a132d65b657bbf285e5/ouroboros-consensus-cardano/src/unstable-shelley-testlib/Test/ThreadNet/Infra/Shelley.hs#L431), which leverages the fact that a single Shelley transaction can demonstrate the necesary quorum for the protocol version increment.

The key features of these three suites with regard to this document are summarized in the following table.

| Repository | Byron | Shelley, ..., and/or Babbage | Conway |
| - | - | - | - |
| `cardano-db-sync` | No | Haskell, unsigned | Haskell, unsigned |
| `ouroboros-consensus` | Haskell, signed | Haskell, signed | No (eg see [Issue 1065](https://github.com/IntersectMBO/ouroboros-consensus/issues/1065)) |
| `cardano-node-tests` | No | `cardano-cli`, signed | `cardano-cli`, signed |

## Initializing the Chain with a Later Protocol Version

It is often convenient for testnet chains to entirely skip some initial protocol versions.
The node currently supports that for testing purposes via the following steps.

- Choose X, the desired protocol version for the testnet's chain to start with.
  As a running example, suppose it is 6.0.
- Declare the `protocolVersion` field to be X (eg `{major: 6, minor: 0}`) within the `protocolParams` field within top-level JSON object of the Shelley Genesis file.
- In the node configuration file, include a `Test<Era>HardForkAtEpoch: 0` declaration for each Cardano era up to and including the era that contains X.
  In the running example, protocol version X=6.0 is in Alonzo (refer to the table in the Introduction above), so the following declarations need to be present within the node configuration file.

```
  ...
  TestShelleyHardForkAtEpoch: 0,
  TestAllegraHardForkAtEpoch: 0,
  TestMaryHardForkAtEpoch: 0,
  TestAlonzoHardForkAtEpoch: 0,
  ...
```

*Warning*.
Values other than `0` should not be used with these settings.
Instead, use the necessary governance transactions to increment the protocol version as explained in the sections above.

With settings like those, today's node still starts from a Byron ledger state corresponding to the given Byron Genesis file (and with a protocol version of 0), but [it immediately](https://github.com/IntersectMBO/ouroboros-consensus/blob/a9a5f3aaf3ddd45b3dd58a132d65b657bbf285e5/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/HardFork/Combinator/Embed/Nary.hs#L268-L276) applies the ledger's translation functions to reach the appropriate era for X by the onset of slot 0.

- The translation from Byron to Shelley uses the protocol version specified in the Shelley Genesis file, regardless of the protocol version of the final Byron ledger state.
  Some pointers into the code:
    - The Consensus Layer translates the last Byron ledger state to the first Shelley ledger state with [this function](https://github.com/IntersectMBO/ouroboros-consensus/blob/a9a5f3aaf3ddd45b3dd58a132d65b657bbf285e5/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/CanHardFork.hs#L265-L269).
    - That function mostly delegates to [this function](https://github.com/IntersectMBO/cardano-ledger/blob/f5b35692b4d92d664187781a98b7af3fab445bad/eras/shelley/impl/src/Cardano/Ledger/Shelley/API/ByronTranslation.hs#L92-L100) in the Ledger Layer.
    - That function sets the first Shelley ledger state's protocol parameters according to the translation context, which is built from the Shelley Genesis file by [this function](https://github.com/IntersectMBO/cardano-ledger/blob/f5b35692b4d92d664187781a98b7af3fab445bad/eras/shelley/impl/src/Cardano/Ledger/Shelley/Translation.hs#L77-L85).
- No subsequent translation (from each "Shelley-based era" to the next) alters the protocol version.

This interface is the result of historical choices.
It's not ideal, but it seems sufficient for the testnet use case.
In particular, the whole thing could be replaced by simply determining which initial eras to skip based on the protocol version given in the Shelley Genesis file and a flag indicating whether to skip Byron.
