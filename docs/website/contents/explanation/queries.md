# Queries

In the Cardano consensus layer, queries provide a mechanism for clients, such as `cardano-cli`, Ogmios, Lace, and Cardano `db-sync`, to fetch information about the state of the node and its ledger. This includes retrieving details like the current block or slot number, the current epoch, protocol parameters, stake distribution, and UTxO size.

The query mechanism is abstracted within the code using a primary [`Query`](https://github.com/intersectmbo/ouroboros-consensus/blob/master/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Query.hs#L225) data type, which is parameterized by the block type and the expected result type. This allows the consensus layer to define a generic interface for querying, independent of the specific block type or era.

In the following sections we explain the different types of queries, how they are versioned, and serialized.

## Query types

The `Query blk result` top-level `Query` type is parameterized by the block type (`blk`) and the expected result. This Query type has different constructors or categories that access different kinds of information:

### BlockQuery Queries

These are queries defined using the `BlockQuery` constructor. This is a data family instantiated for specific block types, such as `ShelleyBlock` or `HardForkBlock`. These queries are typically the ones that directly access or compute information from the ledger state or the ledger configuration specific to a particular era.

The `BlockQuery` for the [HardForkBlock](#TODO-reference-the-HFC-section) itself includes specialized forms for handling multi-era chains:
- `QueryIfCurrent` answers a query about a specific era only if the queried point is in that era.
- `QueryAnytime` answers a query about an era from any supported era. For instance,  `GetEraStart`.
- `QueryHardFork` answers queries specific to the hard-fork combinator itself, like the current era index.


Examples of this sort of queries include:

- `GetStakeDistribution` (Shelley-based): Get the total stake distribution.
- `GetUTxOByAddress` (Shelley-based): Get UTxO filtered by address.
- `GetProposals` (Available since Conway): Get governance proposals by ID.


### General Queries

These are other constructors defined directly within the top-level `Query` type, not nested under `BlockQuery`. These queries typically fetch more general node-level or chain-level information that isn't tied to the specific rules or state of a single ledger era. They are answered by accessing configuration data or the `HeaderState` component of the extended ledger state, rather than the main ledger state itself.

Examples of this sort of queries include:

- `GetSystemStart`: Get the fixed SystemStart time.
- `GetChainBlockNo`: Get the block number of the chain at the queried point.
- `GetChainPoint` Get the Point (slot and hash) of the chain at the queried point.

## Query versioning

Queries are versioned to ensure compatibility between nodes and clients running different software versions.
As the ledger gets more features and new use cases are explored, teams will add new queries that allow to get the necessary data.
To track this, each query has an associated version number indicating when it became available.

In general, we can't assume that every node operator is running on the same/latest version, so a query might be available in one node, but not in another.
Thus, when a client sends a query to a node, it is important that the node is aware of whether it supports a given query.
Morever, beyond mere availability of the query, the exact details of the on-the-wire codec used for each query and its arguments and its response may change over time. The negotiated version is therefore sometimes also necessary to simply select the correct codec to use when communicating with the node.

At the beginning of every connection from a client to a node, the Network layer will negotiate a [`NodeToClientVersion`][n2c]. Before the client sends a query, it can now check that the negotiated version is not older than the associated version number of the query, and act accordingly (i.e. display an indicative error message, or use a different/fallback mechanism to do its job).

Custom implementations of the Cardano node client are free to bypass this check before submitting the query. This does not constitute a problem for the node integrity, but is, instead, an inconvenience for the user. When querying an older node, such an inconsiderate client will simply be disconnected from without explanation. If the user has access to the node's logs, they'll find there an obscure CBOR decoding error.

### Version types

Our code does not use the negotiated [`NodeToClientVersion`][n2c] directly, but translates them first to a [`CardanoNodeToClientVersion`][cardano-n2c] and then to [`ShelleyNodeToClientVersion`][shelley-n2c].

- The [`querySupportedVersions`][query-supported-versions] returns the [NodeToClientVersion][n2c]s that support the given query.
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

To reduce the number of possible version combinations, we currently follow the convention that all `ShelleyNodeToClientVersion`s in one `CardanoNodeToClientVersionX` are equal. This means that the developers of clients (like `cardano-api`, etc) can rely on the fact that once a `NodeToClient` version has been negotiated, all enabled Shelley-based eras support exactly the same queries.[^conway-queries] We might weaken this guarantee in the future, see [#864](https://github.com/IntersectMBO/ouroboros-consensus/issues/864).

The mapping from `NodeToClientVersion`s to `CardanoNodeToClientVersion`s is [`supportedNodeToClientVersions`][supportedNodeToClientVersions]. Additionally, all versions larger than a certain `NodeToClientVersion` (see [`latestReleasedNodeVersion`][latestReleasedNodeVersion]) are considered experimental, which means that queries newly enabled by them can be added and changed at will, without compatibility guarantees. They are only offered in the version negotiation when a flag (currently, `ExperimentalProtocolsEnabled`) is set; also see [`limitToLatestReleasedVersion`][limitToLatestReleasedVersion] and its call/usage sites.

### Why have a separate version type per block?

At the moment, all genuine chains using the abstract Ouroboros code (ie the Diffusion and Consensus Layers) are maintained by the same people that maintain the Ouroboros code, and moreover those chains are all instances of `CardanoBlock`. Thus, it has so far been a convenient simplification to merely increment `NodeToClientVersion` whenever `BlockNodeToClientVersion (CardanoBlock c)` needs a new increment (in addition to when the mini protocols' code changes require such an increment). This approach would be untenable if there were multiple genuine chains with different block types and their own queries evolving independently, sharing the common Ouroboros code as a dependency. That is especially true if some of those chains were maintained by someone other than the Cardano team maintaining the Ouroboros code. In that case, the Diffusion Layer would either need to negotiate the two versions separately or else negotiate the block-specific version `CardanoNodeToClientVersionX` and derive the `NodeToClientVersion` from that (via a callback passed by the owner of the block-specific code) instead of the opposite, which is what the current code does.

That same fundamental hypothetical of genuine chains with different block types motivates defining the `BlockNodeToClientVersion` abstractly, instead of only for `CardanoBlock`. In particular, it's technically possible that some chain could exist that only uses `ByronBlock`, or the `ShelleyBlock`s without Byron, or some other "incomplete" subset of the `ShelleyBlocks`, etc. The block-specific version for such a chain should not necessarily advance in lock-step with the Cardano chain's, since some Cardano changes might not apply to that hypothetical chain [^non-Cardano-block-types]. Via the `BlockNodeToClientVersion` type family, the Consensus Layer is already positioned to properly support multiple block types, once the Diffusion Layer eventually negotiates the versions in a way that allows non-Cardano chains to properly evolve. But for now, the only genuine chain uses all of the `ShelleyBlock` eras, and so we version as a single bundle, pretending that they are inseparable.

### Shelly node-to-client version

Each `ShelleyNodeToClientVersion` has a set of queries it supports. Assume the maximum version is $X$, and that it has queries $Q_0, \dots, Q_{n-1}$ associated to it. If no node was released that supports version $X$, ie `ShelleyNodeToClientVersionX`, we have a reasonable degree of certainty that no client will send any $Q_i$, $x \in [0, n - 1]$ to older nodes (since no such node was yet released). Therefore, if we add a new query $Q_n$ we can associate it to the unreleased version $X$ (`ShelleyNodeToClientVersionX`).

On the other hand, once the node that supports version `X` has been released, we need to increase the maximum Shelley node-to-client version, by adding one more constructor to `ShelleyNodeToClientVersion`, say `ShelleyNodeToClientVersionY`, which is defined in module [Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion](https://github.com/IntersectMBO/ouroboros-consensus/blob/main/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/NetworkProtocolVersion.hs).
By adding this new version the node is able to detect if other Cardano clients that respect this versioning mechanism support said query.

Henceforth, we call an unreleased version "experimental" (ie only used for demo purposes/specific to an unreleased era).

### Checks

The supported query versions are only enforced in the [Shelley query encoder][shelley-encodeNodeToClient], ie in code run by clients. The server will currently answer queries even if the negotiated version is not in the supported version range for that query (which might be the case with a custom client implementation, or when one forgot to enable experimental protocols), following the robustness principle (ie "be conservative in what you send, be liberal in what you accept").

As an example, consider a query $Q$ that is enabled after version $x$, and consider a connection between a client and a node that negotiated version $y$. If $y < x$, then the client will throw an exception before sending $Q$ as the negotiated version is too old, so the server probably won't understand the query. But if the server does actually understand the query, and the client uses a custom implementation that does not perform the check on $x$ and $y$, then the server will reply as normal despite $y < x$.

### On newly added golden files

When adding a new `ShelleyNodeToClientVersion` or a new `CardanoNodeToClientVersions` new golden files will be generated. Because serialization is [version dependent](https://github.com/IntersectMBO/ouroboros-consensus/pull/95), a new `ShelleyNodeToClientVersion` could also introduce a different serialization. See how function [`decodeShelleyResult`][decodeShelleyResult] uses `ShelleyNodeToClientVersion`. Therefore we have to test the serialization for the new versions.

The golden tests only generate golden files for queries that have examples. So if a newly added query does not have an example, no golden files will be generated for it.

[n2c]: https://ouroboros-network.cardano.intersectmbo.org/ouroboros-network/Ouroboros-Network-NodeToClient.html#t:NodeToClientVersion
[network-spec]: https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf
[shelley-n2c]: https://github.com/IntersectMBO/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/NetworkProtocolVersion.hs#L17
[cardano-n2c]: https://github.com/IntersectMBO/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/Node.hs#L341-L527
[query-supported-versions]:
https://github.com/IntersectMBO/ouroboros-consensus/blob/main/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/Query.hs#L343
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
