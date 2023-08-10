# Query versioning

This document explains why and how we version local state queries.

If you want to add a new query, [jump here](#how-to-version-a-new-query).

## Context

Local state queries allow clients (like `cardano-cli`/`ogmios`/...) to fetch information from a node about its state for a particular point on the chain (like the tip of the node's current selection). Examples are the current block or slot number, the current epoch as well as various information of the ledger state, like the protocol parameters or the stake distribution. As the ledger gets more features and new use cases are explored, teams will add new queries that allow to get the necessary data. To track this, every query has an associated version number which indicates since when it is available.

In general, we can't assume that every node operator is running on the same/latest version, so a query might be available in one node, but not in another.
Thus, when a client sends a query to a node, it is important that the node is aware of whether it supports a given query.

At the beginning of every connection from a client to a node, the Network layer will negotiate a [`NodeToClientVersion`][n2c]. Before the client sends a query, it can now check that the negotiated version is not older than the associated version number of the query, and act accordingly (i.e. display an indicative error message, or use a different/fallback mechanism to do its job).

Custom implementation of the Cardano node client can choose to bypass this check before submitting the query. This does not constitute a problem for the node integrity, but is, instead, an inconvenience for the user as she will be presented with an obscure CBOR decoding error when an old node tries to answer a query it does not support.

## Implementation

### Version types

Our code does not use the negotiated [`NodeToClientVersion`][n2c] directly, but translates them first to a [`CardanoNodeToClientVersion`][cardano-n2c] and then to [`ShelleyNodeToClientVersion`][shelley-n2c].

 - A [`ShelleyNodeToClientVersion`][shelley-n2c] is what is associated with every query in [`querySupportedVersion`][query-supported-version].
 - A [`CardanoNodeToClientVersion`][cardano-n2c] specifies the [`ShelleyNodeToClientVersion`][shelley-n2c] for each era, or indicates that a specific [era][feature-table] is not supported. As an example, consider
   ```haskell
   pattern CardanoNodeToClientVersion10 :: BlockNodeToClientVersion (CardanoBlock c)
   pattern CardanoNodeToClientVersion10 =
       HardForkNodeToClientEnabled
         HardForkSpecificNodeToClientVersion2
         (  EraNodeToClientEnabled ByronNodeToClientVersion1
         :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
         :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
         :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
         :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
         :* EraNodeToClientEnabled ShelleyNodeToClientVersion6
         :* EraNodeToClientDisabled
         :* Nil
         )
   ```
   This tells us that in Shelley, Allegra, Mary, Alonzo and Babbage, we use `ShelleyNodeToClientVersion6`, and Conway is disabled. This means that no queries that were introduced in `ShelleyNodeToClientVersion7` can be used, and no queries in the Conway era are possible at all.

   The mapping from `NodeToClientVersion`s to `CardanoNodeToClientVersion`s is [`supportedNodeToClientVersions`][supportedNodeToClientVersions]

Additionally, all versions larger than a certain `NodeToClientVersion` (see [`latestReleasedNodeVersion`][latestReleasedNodeVersion]) are considered experimental, and are only offered in the initial version negotiation when a flag (currently, `ExperimentalProtocolsEnabled`) is set. Also see [`limitToLatestReleasedVersion`][limitToLatestReleasedVersion] and its call/usage sites.

#### Shelly node-to-client version

Each `ShelleyNodeToClientVersion` has a set of queries it supports. Assume the maximum version is $X$, and that it has queries $Q_0$, ... $Q_{n-1}$ associated to it. If no node was released that supports version $X$, ie `ShelleyNodeToClientVersionX`, we have a reasonable degree of certainty that no client will send any $Q_i$, $x \in [0, n - 1]$ to older nodes (since no such node was yet released). Therefore, if we add a new query $Q_n$ we can associate it to the unreleased version $X$ (`ShelleyNodeToClientVersionX`).

On the other hand, the node that supports version `X` has been released, then we
need to increase the maximum Shelley node-to-client version, by adding one more constructor to `ShelleyNodeToClientVersion`, which is defined in module [Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion](https://github.com/input-output-hk/ouroboros-consensus/blob/main/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/NetworkProtocolVersion.hs). By adding this new version the node is able to detect if other Cardano clients that respect this versioning mechanism support said query.

Henceforth, we call an unreleased version "experimental" (ie only used for demo purposes/specific to an unreleased era).

### Checks

The supported query versions are only enforced in the [Shelley query encoder][shelley-encodeNodeToClient], ie in code run by clients. The server will answer queries even if the negotiated version is older than expected (which might be the case with a custom client implementation), following the robustness principle.

### On newly added golden files

When adding a new `ShelleyNodeToClientVersion` or a new `CardanoNodeToClientVersions` new golden files will be generated. Because serialization is [version dependent](https://github.com/input-output-hk/ouroboros-consensus/pull/95), a new `ShelleyNodeToClientVersion` could also introduce a different serialization. See how function [`decodeShelleyResult`](https://github.com/input-output-hk/ouroboros-consensus/pull/95/files#diff-0bff1ce7a3c11a5b7371e38d073430fc4e49042adad2ce81577793d0e5d28020R726) uses `ShelleyNodeToClientVersion`. Therefore we have to test the serialization for the new versions.

The golden tests only generate golden files for queries that have examples. So if a newly added query does not have an example, no golden files will be generated.

## How to version a new query

 1. Determine whether the query is supposed to be experimental.

 2. Check whether you need a new [`NodeToClientVersion`][n2c].

     - If the query is experimental, you only need one if there does not already exist a [`NodeToClientVersion`][n2c] beyond the [`latestReleasedNodeVersion`][latestReleasedNodeVersion] (usually, it should already exist).

     - If the query is not experimental, you need one if the current [`latestReleasedNodeVersion`][latestReleasedNodeVersion] is already used in a released version of the node. For this, check the version of `ouroboros-consensus-cardano` in the latest node release, and navigate to the corresponding [`latestReleasedNodeVersion`][latestReleasedNodeVersion].

    If positive, create a corresponding PR in the [Network repository][network-repo], and wait for a new release to CHaP.

 3. Depending on the previous step:

     - if necessary, add a new [`CardanoNodeToClientVersion`][cardano-n2c] and adapt [`supportedNodeToClientVersions`][supportedNodeToClientVersions] (as well as Shelley's [`supportedNodeToClientVersions`][shelley-supportedNodeToClientVersions], which is only used in tests).
     We use this mapping because `SupportedNetworkProtocolVersion` is not a composable instance.

     - if necessary, add a new [`ShelleyNodeToClientVersion`][shelley-n2c] and adapt [`querySupportedVersion`][query-supported-version]

4. In case the network node-to-client versions (eg `NodeToClient_V16`) is not linked to to the `CardanoNodeToClientVersion` mentioned above, we need to do this by adding an extra entry to `supportedNodeToClientVersions`, in [Ouroboros.Consensus.Cardano.Node](https://github.com/input-output-hk/ouroboros-consensus/blob/main/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/Node.hs).

 5. In many cases, these changes will make our golden tests fail (due to missing files); to fix CI, run the tests locally and check in the newly generated files.

 6. Follow the compiler warnings.


## Sample pull requests

Old pull-requests that added new queries serve as good reference material when adding new queries. For instance see [#191](https://github.com/input-output-hk/ouroboros-consensus/pull/191). Be aware that these PRs they can get out of date. If you detect this please delete old links and add those corresponding to newer pull requests.

[n2c]: https://input-output-hk.github.io/ouroboros-network/ouroboros-network/Ouroboros-Network-NodeToClient.html#t:NodeToClientVersion
[network-spec]: https://input-output-hk.github.io/ouroboros-network/pdfs/network-spec/network-spec.pdf
[shelley-n2c]: https://github.com/input-output-hk/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/NetworkProtocolVersion.hs#L17
[cardano-n2c]: https://github.com/input-output-hk/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/Node.hs#L341-L527
[query-supported-version]: https://github.com/input-output-hk/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs#L537-L574
[feature-table]: https://github.com/cardano-foundation/CIPs/blob/master/CIP-0059/feature-table.md
[supportedNodeToClientVersions]: https://github.com/input-output-hk/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/Node.hs#L540
[latestReleasedNodeVersion]: https://github.com/input-output-hk/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/Node.hs#L551
[limitToLatestReleasedVersion]: https://github.com/input-output-hk/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-diffusion/src/ouroboros-consensus-diffusion/Ouroboros/Consensus/Node.hs#L896-L897
[shelley-encodeNodeToClient]: https://github.com/input-output-hk/ouroboros-consensus/blob/3d55ae3ca7a9e1c63a19266d35ef5512bbef13ab/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Node/Serialisation.hs#L180-L185
[network-repo]: https://github.com/input-output-hk/ouroboros-network
[shelley-supportedNodeToClientVersions]: https://github.com/input-output-hk/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/NetworkProtocolVersion.hs#L56-L65
