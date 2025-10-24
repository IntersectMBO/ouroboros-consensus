# Query Versioning

:::warning
This document should contain only the "How to version a new query section"
:::

This document explains why and how we version local state queries.

If you want to add a new query, [jump here](#how-to-version-a-new-query).

## Context

Local state queries allow clients (like `cardano-cli`, `ogmios`, etc) to fetch information from a node about its state for a particular point on the chain (like the tip of the node's current selection). Examples are the current block or slot number, the current epoch as well as various information of the ledger state, like the protocol parameters or the stake distribution. As the ledger gets more features and new use cases are explored, teams will add new queries that allow to get the necessary data. To track this, every query has an associated version number which indicates since when it is available.

In general, we can't assume that every node operator is running on the same/latest version, so a query might be available in one node, but not in another.
Thus, when a client sends a query to a node, it is important that the node is aware of whether it supports a given query.
Morever, beyond mere availability of the query, the exact details of the on-the-wire codec used for each query and its arguments and its response may change over time. The negotiated version is therefore sometimes also necessary to simply select the correct codec to use when communicating with the node.

At the beginning of every connection from a client to a node, the Network layer will negotiate a [`NodeToClientVersion`][n2c]. Before the client sends a query, it can now check that the negotiated version is not older than the associated version number of the query, and act accordingly (i.e. display an indicative error message, or use a different/fallback mechanism to do its job).

Custom implementations of the Cardano node client are free to bypass this check before submitting the query. This does not constitute a problem for the node integrity, but is, instead, an inconvenience for the user. When querying an older node, such an inconsiderate client will simply be disconnected from without explanation. If the user has access to the node's logs, she'll find there an obscure CBOR decoding error.

## Implementation

### Version types

Our code does not use the negotiated [`NodeToClientVersion`][n2c] directly, but translates them first to a [`CardanoNodeToClientVersion`][cardano-n2c] and then to [`ShelleyNodeToClientVersion`][shelley-n2c].

 - The [`querySupportedVersion`][query-supported-version] function assigns a [`ShelleyNodeToClientVersion`][shelley-n2c] to each Shelley-based query.
 - Each [`CardanoNodeToClientVersionX`][cardano-n2c] specifies the [`ShelleyNodeToClientVersion`][shelley-n2c] for each era, or indicates that a specific [era][feature-table] is not supported. As an example, consider
   ```haskell
   pattern CardanoNodeToClientVersion10 :: BlockNodeToClientVersion (CardanoBlock c)
   pattern CardanoNodeToClientVersion10 =
       HardForkNodeToClientEnabled
         HardForkSpecificNodeToClientVersion2
         (  EraNodeToClientEnabled ByronNodeToClientVersion1   -- Byron
         :* EraNodeToClientEnabled ShelleyNodeToClientVersion6 -- Shelley
         :* EraNodeToClientEnabled ShelleyNodeToClientVersion6 -- Allegra
         :* EraNodeToClientEnabled ShelleyNodeToClientVersion6 -- Mary
         :* EraNodeToClientEnabled ShelleyNodeToClientVersion6 -- Alonzo
         :* EraNodeToClientEnabled ShelleyNodeToClientVersion6 -- Babbage
         :* EraNodeToClientDisabled                            -- Conway
         :* Nil
         )
   ```
   This tells us that in Shelley, Allegra, Mary, Alonzo and Babbage, we use `ShelleyNodeToClientVersion6`, and Conway is disabled. This means that no queries that were introduced in `ShelleyNodeToClientVersion7` can be used, and no queries in the Conway era are possible at all.

   In order to reduce the number of possible version combinations, we currently follow the convention that all `ShelleyNodeToClientVersion`s in one `CardanoNodeToClientVersionX` are equal. This means that the developers of clients (like `cardano-api`, etc) can rely on the fact that once a `NodeToClient` version has been negotiated, all enabled Shelley-based eras support exactly the same queries.[^conway-queries] We might weaken this guarantee in the future, see [#864](https://github.com/IntersectMBO/ouroboros-consensus/issues/864).

The mapping from `NodeToClientVersion`s to `CardanoNodeToClientVersion`s is [`supportedNodeToClientVersions`][supportedNodeToClientVersions]. Additionally, all versions larger than a certain `NodeToClientVersion` (see [`latestReleasedNodeVersion`][latestReleasedNodeVersion]) are considered experimental, which means that queries newly enabled by them can be added and changed at will, without compatibility guarantees. They are only offered in the version negotiation when a flag (currently, `ExperimentalProtocolsEnabled`) is set; also see [`limitToLatestReleasedVersion`][limitToLatestReleasedVersion] and its call/usage sites.

#### Why have a separate version type per block?

At the moment, all genuine chains using the abstract Ouroboros code (ie the Diffusion and Consensus Layers) are maintained by the same people that maintain the Ouroboros code, and moreover those chains are all instances of `CardanoBlock`. Thus, it has so far been a convenient simplification to merely increment `NodeToClientVersion` whenever `BlockNodeToClientVersion (CardanoBlock c)` needs a new increment (in addition to when the mini protocols' code changes require such an increment). This approach would be untenable if there were multiple genuine chains with different block types and their own queries evolving independently, sharing the common Ouroboros code as a dependency. That is especially true if some of those chains were maintained by someone other than the Cardano team maintaining the Ouroboros code. In that case, the Diffusion Layer would either need to negotiate the two versions separately or else negotiate the block-specific version `CardanoNodeToClientVersionX` and derive the `NodeToClientVersion` from that (via a callback passed by the owner of the block-specific code) instead of the opposite, which is what the current code does.

That same fundamental hypothetical of genuine chains with different block types motivates defining the `BlockNodeToClientVersion` abstractly, instead of only for `CardanoBlock`. In particular, it's technically possible that some chain could exist that only uses `ByronBlock`, or the `ShelleyBlock`s without Byron, or some other "incomplete" subset of the `ShelleyBlocks`, etc. The block-specific version for such a chain should not necessarily advance in lock-step with the Cardano chain's, since some Cardano changes might not apply to that hypothetical chain [^non-Cardano-block-types]. Via the `BlockNodeToClientVersion` type family, the Consensus Layer is already positioned to properly support multiple block types, once the Diffusion Layer eventually negotiates the versions in a way that allows non-Cardano chains to properly evolve. But for now, the only genuine chain uses all of the `ShelleyBlock` eras, and so we version as a single bundle, pretending that they are inseparable.

#### Shelly node-to-client version

Each `ShelleyNodeToClientVersion` has a set of queries it supports. Assume the maximum version is $X$, and that it has queries $Q_0, \dots, Q_{n-1}$ associated to it. If no node was released that supports version $X$, ie `ShelleyNodeToClientVersionX`, we have a reasonable degree of certainty that no client will send any $Q_i$, $x \in [0, n - 1]$ to older nodes (since no such node was yet released). Therefore, if we add a new query $Q_n$ we can associate it to the unreleased version $X$ (`ShelleyNodeToClientVersionX`).

On the other hand, the node that supports version `X` has been released, then we
need to increase the maximum Shelley node-to-client version, by adding one more constructor to `ShelleyNodeToClientVersion`, which is defined in module [Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion](https://github.com/IntersectMBO/ouroboros-consensus/blob/main/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/NetworkProtocolVersion.hs). By adding this new version the node is able to detect if other Cardano clients that respect this versioning mechanism support said query.

Henceforth, we call an unreleased version "experimental" (ie only used for demo purposes/specific to an unreleased era).

### Checks

The supported query versions are only enforced in the [Shelley query encoder][shelley-encodeNodeToClient], ie in code run by clients. The server will currently answer queries even if the negotiated version is not in the supported version range for that query (which might be the case with a custom client implementation, or when one forgot to enable experimental protocols), following the robustness principle (ie "be conservative in what you send, be liberal in what you accept").

As an example, consider a query $Q$ that is enabled after version $x$, and consider a connection between a client and a node that negotiated version $y$. If $y < x$, then the client will throw an exception before sending $Q$ as the negotiated version is too old, so the server probably won't understand the query. But if the server does actually understand the query, and the client uses a custom implementation that does not perform the check on $x$ and $y$, then the server will reply as normal despite $y < x$.

### On newly added golden files

When adding a new `ShelleyNodeToClientVersion` or a new `CardanoNodeToClientVersions` new golden files will be generated. Because serialization is [version dependent](https://github.com/IntersectMBO/ouroboros-consensus/pull/95), a new `ShelleyNodeToClientVersion` could also introduce a different serialization. See how function [`decodeShelleyResult`][decodeShelleyResult] uses `ShelleyNodeToClientVersion`. Therefore we have to test the serialization for the new versions.

The golden tests only generate golden files for queries that have examples. So if a newly added query does not have an example, no golden files will be generated for it.

## How to version a new query

 1. Determine whether the query is supposed to be experimental.

 2. Check whether you need a new [`NodeToClientVersion`][n2c].

     - If the query is experimental, you only need one if there is no [`NodeToClientVersion`][n2c] beyond the [`latestReleasedNodeVersion`][latestReleasedNodeVersion] (usually, it should already exist).

     - If the query is not experimental, you need one if the current [`latestReleasedNodeVersion`][latestReleasedNodeVersion] is already used in a released version of the node. For this, check the version of `ouroboros-consensus-cardano` in the latest node release, and navigate to the corresponding [`latestReleasedNodeVersion`][latestReleasedNodeVersion].

    If you determine that you need a new [`NodeToClientVersion`][n2c], create a corresponding PR in the [Network repository][network-repo], and wait for a new release to CHaP.

 3. Depending on the previous step:

     - if necessary, add a new [`CardanoNodeToClientVersion`][cardano-n2c] and adapt [`supportedNodeToClientVersions`][supportedNodeToClientVersions] (as well as Shelley's [`supportedNodeToClientVersions`][shelley-supportedNodeToClientVersions], which is only used in tests).
     We use this mapping because `SupportedNetworkProtocolVersion` is not a composable instance.

     - if necessary (ie there is no existing one you can use), add a new [`ShelleyNodeToClientVersion`][shelley-n2c] and adapt [`querySupportedVersion`][query-supported-version]

 4. In case the network node-to-client versions (eg `NodeToClient_V16`) is not linked to to the `CardanoNodeToClientVersion` mentioned above, we need to do this by adding an extra entry to [`supportedNodeToClientVersions`][supportedNodeToClientVersions].

 5. In many cases, these changes will make our golden tests fail (due to missing files); to fix CI, run the tests locally and check in the newly generated files.

 6. Follow the compiler warnings.


## Sample pull requests

Old pull-requests that added new queries serve as good reference material when adding new queries. For instance see [#191](https://github.com/IntersectMBO/ouroboros-consensus/pull/191). Be aware that these PRs they can get out of date. If you detect this please delete old links and add those corresponding to newer pull requests.

[n2c]: https://ouroboros-network.cardano.intersectmbo.org/cardano-diffusion/api/Cardano-Network-NodeToClient-Version.html#t:NodeToClientVersion
[network-spec]: https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf
[shelley-n2c]: https://github.com/IntersectMBO/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/NetworkProtocolVersion.hs#L17
[cardano-n2c]: https://github.com/IntersectMBO/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/Node.hs#L341-L527
[query-supported-version]: https://github.com/IntersectMBO/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs#L537-L574
[feature-table]: https://github.com/cardano-foundation/CIPs/blob/master/CIP-0059/feature-table.md
[supportedNodeToClientVersions]: https://github.com/IntersectMBO/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/Node.hs#L540
[latestReleasedNodeVersion]: https://github.com/IntersectMBO/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/Node.hs#L551
[limitToLatestReleasedVersion]: https://github.com/IntersectMBO/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-diffusion/src/ouroboros-consensus-diffusion/Ouroboros/Consensus/Node.hs#L896-L897
[shelley-encodeNodeToClient]: https://github.com/IntersectMBO/ouroboros-consensus/blob/3d55ae3ca7a9e1c63a19266d35ef5512bbef13ab/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Node/Serialisation.hs#L180-L185
[network-repo]: https://github.com/IntersectMBO/ouroboros-network
[shelley-supportedNodeToClientVersions]: https://github.com/IntersectMBO/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/NetworkProtocolVersion.hs#L56-L65
[decodeShelleyResult]: https://github.com/IntersectMBO/ouroboros-consensus/blob/3d55ae3ca7a9e1c63a19266d35ef5512bbef13ab/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs#L733

[^conway-queries]: There are already queries that morally are Conway-specific, but still work in older eras, returning something along the lines of `mempty` in that case.

[^non-Cardano-block-types]: The opposite is only true if that chain has some block types that Cardano doesn't.
