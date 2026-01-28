# Queries

Ouroboros-consensus implements a querying interface to ask information about
the ledger state or about the shape of the blockchain. This interface is exposed
via the `LocalStateQuery` mini-protocol (Section 3.13 in [the Ouroboros Network
Specification](https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf)).

Queries can be categorized in 3 classes:

- [Top level
  queries](https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus/Ouroboros-Consensus-Ledger-Query.html#t:Query):
  - `GetSystemStart` returns the start time of the system,
  - `GetChainBlockNo` returns the current tip block number,
  - `GetChainPoint` returns the current tip point (slot number and hash),
  - `DebugLedgerConfig` returns a debug representation of the ledger
    configuration in use,
  - `BlockQuery` which depends on the particular block type, concretized in
    `BlockQuery (HardForkBlock xs)` for the Hard Fork Block and in particular
    the Cardano block, and `BlockQuery (ShelleyBlock proto era)` for the shelley
    based eras.

- Hard Fork era independent queries ([`BlockQuery (HardForkBlock
  xs)`](https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus/Ouroboros-Consensus-HardFork-Combinator-Ledger-Query.html#t:BlockQuery)):
  can be answered at any point in the chain.
  - [`QueryAnytime
    GetEraStart`](https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus/Ouroboros-Consensus-HardFork-Combinator-Ledger-Query.html#t:QueryAnytime)
    which together with an era index returns information about the era start in
    terms of time, slot, epoch number and Peras round.
  - [`QueryHardFork
    GetInterpreter`](https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus/Ouroboros-Consensus-HardFork-Combinator-Ledger-Query.html#t:QueryHardFork)
    returns an
    [`Interpreter`](https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus/Ouroboros-Consensus-HardFork-History-Qry.html#t:Interpreter)
    that can be used to inspect the start and end of eras as well as the
    parameters of each era (slot duration, epoch size, ...)
  - [`QueryHardFork GetCurrentEra`](https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus/Ouroboros-Consensus-HardFork-Combinator-Ledger-Query.html#t:QueryHardFork) returns the index of the era at the current
    tip of the chain.

- Single-era block queries, note these can only be interpreted when the tip of
  the chain is on the particular era, wrapped in [`QueryIfCurrent`](https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus/Ouroboros-Consensus-HardFork-Combinator-Ledger-Query.html#t:QueryIfCurrent):
  - Byron queries ([`BlockQuery
    ByronBlock`](https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus-cardano/Ouroboros-Consensus-Byron-Ledger-Ledger.html#t:BlockQuery)):
    which consists only of `GetUpdateInterfaceState` which returns the state of
    the blockchain. This query can only be answered while the node's tip is
    still in the Byron era.
  - Shelley queries ([`BlockQuery (ShelleyBlock proto
    era)`](https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus-cardano/Ouroboros-Consensus-Shelley-Ledger-Query.html#t:BlockQuery)):
    these queries give access to information on the Ledger state. They are
    discussed below as there are many of them.

## Versioning

Versioning of queries is done in multiple layers, some as global versions, some
as block-dependent versions. When a client connects to a `cardano-node` they
negotiate the highest
[`NodeToClientVersion`](https://ouroboros-network.cardano.intersectmbo.org/cardano-diffusion/api/Cardano-Network-NodeToClient-Version.html#t:NodeToClientVersion)
they both know about and use that one for deciding which queries are available
and how to interpret the results.

The top-level available queries depend on the `NodeToClientVersion`.

| `NodeToClientVersion` | `QueryVersion`  | Newly enabled top-level queries                                    |
|-----------------------|-----------------|--------------------------------------------------------------------|
| `NodeToClientV_16`    | `QueryVersion2` | `BlockQuery`, `GetSystemStart`, `GetChainBlockNo`, `GetChainPoint` |
| `NodeToClientV_17`    | `QueryVersion2` |                                                                    |
| `NodeToClientV_18`    | `QueryVersion2` |                                                                    |
| `NodeToClientV_19`    | `QueryVersion2` |                                                                    |
| `NodeToClientV_20`    | `QueryVersion3` | `DebugLedgerConfig`                                                |
| `NodeToClientV_21`    | `QueryVersion3` |                                                                    |
| `NodeToClientV_22`    | `QueryVersion3` |                                                                    |
| `NodeToClientV_23`    | `QueryVersion3` |                                                                    |

Particular block-query versions are of type `BlockNodeToNodeVersion blk`, which
is associated with the global `NodeToClientVersion` in
`supportedNodeToClientVersions`. There exist associations for the Byron and
Shelley blocks alone but those are in principle uninteresting for mainnet, and
instead we focus on the Cardano version. All the current versions imply also
`HardForkSpecificNodeToClientVersion3` and `ByronNodeToClientVersion1`:

| `NodeToClientVersion` | `BlockNodeToNodeVersion blk`   | `ShelleyNodeToClientVersion`   |
|-----------------------|--------------------------------|--------------------------------|
| `NodeToClientV_16`    | `CardanoNodeToClientVersion12` | `ShelleyNodeToClientVersion8`  |
| `NodeToClientV_17`    | `CardanoNodeToClientVersion13` | `ShelleyNodeToClientVersion9`  |
| `NodeToClientV_18`    | `CardanoNodeToClientVersion14` | `ShelleyNodeToClientVersion10` |
| `NodeToClientV_19`    | `CardanoNodeToClientVersion15` | `ShelleyNodeToClientVersion11` |
| `NodeToClientV_20`    | `CardanoNodeToClientVersion16` | `ShelleyNodeToClientVersion12` |
| `NodeToClientV_21`    | `CardanoNodeToClientVersion17` | `ShelleyNodeToClientVersion13` |
| `NodeToClientV_22`    | `CardanoNodeToClientVersion18` | `ShelleyNodeToClientVersion14` |
| `NodeToClientV_23`    | `CardanoNodeToClientVersion19` | `ShelleyNodeToClientVersion15` |

## Codecs

Queries are sent over the LocalStateQuery mini-protocol (section 3.13 in [the
Network
documentation](https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf)). First
the client has to request the acquisition of a point to run the queries on, and
after confirmation from the server, the client can send `msgQuery` messages,
finishing with a `msgRelease` signal.

Queries are encoded with
[`queryEncodeNodeToClient`](https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus/Ouroboros-Consensus-Ledger-Query.html#v:queryEncodeNodeToClient)
and decoded with
[`queryDecodeNodeToClient`](https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus/Ouroboros-Consensus-Ledger-Query.html#v:queryDecodeNodeToClient). Query
results are encoded via the [`SerialiseResult`](https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus/Ouroboros-Consensus-Node-Serialisation.html#t:SerialiseResult) class.

Query binary format is a CBOR encoding of tagged constructors, followed by the
arguments for the query. Note the description of the binary format is
representing almost CBOR diagnostics notation, so containers without underscores
mean definite-length containers. The top level queries are encoded as:

| Query              | Binary format         |
|--------------------|-----------------------|
| `BlockQuery query` | `[0, <encode query>]` |
| `GetSystemStart`   | `[1]`                 |
| `GetChainBlockNo`  | `[2]`                 |
| `GetChainPoint`    | `[3]`                 |

The hard-fork era independent queries are encoded as:

| Query                          | Binary format              |
|--------------------------------|----------------------------|
| `QueryIfCurrent blockquery`    | `[0, <encode blockquery>]` |
| `QueryAnytime GetEraStart era` | `[1, [0], <encode era>]`   |
| `QueryHardFork GetInterpreter` | `[2, [0]]`                 |
| `QueryHardFork GetCurrentEra`  | `[2, [1]]`                 |

The block query is an N-ary sum-like construct, so queries for each era are
encoded in a tagged construct (note that we concretize the Byron query as there
is only one):

| Query in era | Encoding       | Constructor                  |
|--------------|----------------|------------------------------|
| Byron        | `[0, 0]`       | `QZ GetUpdateInterfaceState` |
| Shelley      | `[1, <query>]` | `QS (QZ query)`              |
| Allegra      | `[2, <query>]` | `QS (QS (QZ query))`         |
| ...          | ...            | ...                          |

We will discuss the codecs of the Shelley eras' queries in the section below.

The encoding of results is a CBOR encoding of the value, done without any
tagging or prefixing. As the client knows which query it sent, it can infer how
to decode the result. Sometimes the encoding of the result changes depending on
the particular `NodeToClientVersion` negotiated.

## Shelley queries

The different parts of the Ledger State can be accessed via these Shelley
queries. The interface is shared among all Shelley-based blocks, but some are
only accessible when the tip of the chain is in Conway or a later era. The
encoding of the queries consists of a (definite-length) list containing a tag
and the serialization of the arguments.

| Tag | Query                                     | Only active on Shelley NTC versions | Arguments                                                                                     | Result                                                       |
|-----|-------------------------------------------|-------------------------------------|-----------------------------------------------------------------------------------------------|--------------------------------------------------------------|
| 0   | `GetLedgerTip`                            |                                     |                                                                                               | `Point (ShelleyBlock proto era)`                             |
| 1   | `GetEpochNo`                              |                                     |                                                                                               | `EpochNo`                                                    |
| 2   | `GetNonMyopicMemberRewards`               |                                     | `Set (Either Coin (Credential Staking))`                                                      | `NonMyopicMemberRewards`                                     |
| 3   | `GetCurrentPParams`                       |                                     |                                                                                               | `PParams era`                                                |
| 4   | `GetProposedPParamsUpdates`               | < v12                               |                                                                                               | `ProposedPPUpdates era` ‡                                    |
| 5   | `GetStakeDistribution`                    | < v13 *                             |                                                                                               | `PoolDistr (ProtoCrypto proto)`                              |
| 6   | `GetUTxOByAddress`                        |                                     | `Set Addr`                                                                                    | `UTxO era`                                                   |
| 7   | `GetUTxOWhole`                            |                                     |                                                                                               | `UTxO era`                                                   |
| 8   | `DebugEpochState`                         |                                     |                                                                                               | `EpochState era`                                             |
| 9   | `GetCBOR`                                 | the version of the internal query   | `BlockQuery (ShelleyBlock proto era) fp result`                                               | `BlockQuery (ShelleyBlock proto era) fp (Serialised result)` |
| 10  | `GetFilteredDelegationsAndRewardAccounts` |                                     | `Set (Credential Staking)`                                                                    | `(Delegations, Map (Credential Staking) Coin)`               |
| 11  | `GetGenesisConfig`                        |                                     |                                                                                               | `CompactGenesis` ‡                                           |
| 12  | `DebugNewEpochState`                      |                                     |                                                                                               | `NewEpochState era`                                          |
| 13  | `DebugChainDepState`                      |                                     |                                                                                               | `ChainDepState proto`                                        |
| 14  | `GetRewardProvenance`                     |                                     |                                                                                               | `RewardProvenance`                                           |
| 15  | `GetUTxOByTxIn`                           |                                     | `Set TxIn`                                                                                    | `UTxO era`                                                   |
| 16  | `GetStakePools`                           |                                     |                                                                                               | `Set (KeyHash StakePool)`                                    |
| 17  | `GetStakePoolParams`                      |                                     | `Set (KeyHash StakePool)`                                                                     | `Map (KeyHash StakePool) PoolParams`                         |
| 18  | `GetRewardInfoPools`                      |                                     |                                                                                               | `(RewardParams, Map (KeyHash StakePool) RewardInfoPool)`     |
| 19  | `GetPoolState`                            |                                     | `Maybe (Set (KeyHash StakePool))`                                                             | `QueryPoolStateResult`                                       |
| 20  | `GetStakeSnapshots`                       |                                     | `Maybe (Set (KeyHash StakePool))`                                                             | `StakeSnapshots`                                             |
| 21  | `GetPoolDistr`                            | < v13 *                             | `Maybe (Set (KeyHash StakePool))`                                                             | `PoolDistr (ProtoCrypto proto)`                              |
| 22  | `GetStakeDelegDeposits`                   |                                     | `Set StakeCredential`                                                                         | `Map StakeCredential Coin`                                   |
| 23  | `GetConstitution`                         | †                                   |                                                                                               | `Constitution era`                                           |
| 24  | `GetGovState`                             |                                     |                                                                                               | `GovState era`                                               |
| 25  | `GetDRepState`                            | †                                   | `Set (Credential DRepRole)`                                                                   | `Map (Credential DRepRole) DRepState`                        |
| 26  | `GetDRepStakeDistr`                       | †                                   | `Set DRep`                                                                                    | `Map DRep Coin`                                              |
| 27  | `GetCommitteeMembersState`                | †                                   | `Set (Credential ColdCommitteeRole)`, `Set (Credential HotCommitteeRole)`, `Set MemberStatus` | `CommitteeMembersState`                                      |
| 28  | `GetFilteredVoteDelegatees`               | †                                   | `Set (Credential Staking)`                                                                    | `VoteDelegatees`                                             |
| 29  | `GetAccountState`                         | †                                   |                                                                                               | `ChainAccountState`                                          |
| 30  | `GetSPOStakeDistr`                        | †                                   | `Set (KeyHash StakePool)`                                                                     | `Map (KeyHash StakePool) Coin`                               |
| 31  | `GetProposals`                            | >= v9 †                             | `Set GovActionId`                                                                             | `Seq (GovActionState era)`                                   |
| 32  | `GetRatifyState`                          | >= v9 †                             |                                                                                               | `RatifyState era`                                            |
| 33  | `GetFuturePParams`                        | >= v10                              |                                                                                               | `Maybe (PParams era)`                                        |
| 34  | `GetBigLedgerPeerSnapshot`                | >= v11                              |                                                                                               | `LedgerPeerSnapshot` ‡                                       |
| 35  | `GetStakePoolDefaultVote`                 | >= v12 †                            | `KeyHash StakePool`                                                                           | `DefaultVote`                                                |
| 36  | `GetPoolDistr2`                           | >= v13 *                            | `Maybe (Set (KeyHash StakePool))`                                                             | `PoolDistr`                                                  |
| 37  | `GetStakeDistribution2`                   | >= v13 *                            |                                                                                               | `PoolDistr`                                                  |
| 38  | `GetMaxMajorProtocolVersion`              | >= v13                              |                                                                                               | `MaxMajorProtVer`                                            |
| 39  | `GetDRepDelegations`                      | >= v14                              | `Set DRep`                                                                                    | `(Map DRep (Set (Credential Staking)))`                      |

*: The queries enabled only before version 13 used old types from the Ledger
that were removed in recent versions. The queries enabled at version 13 use the
new corresponding ledger types.

†: Even if an appropriate version is enabled, these queries can only be answered
when the corresponding era is Conway or later, as they relate to governance
concepts only present starting on Conway.

‡: The format of the result of these queries depend on the particular
`NodeToClientVersion` negotiated. For now, do check
[`encodeShelleyResult`](https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/ouroboros-consensus-cardano/Ouroboros-Consensus-Shelley-Ledger-Query.html#v:encodeShelleyResult)
to see what exactly changes.
