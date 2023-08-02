# Query versioning

This document explains why and how we version local state queries.

If you want to add a new query, [jump here](#how-to-version-a-new-query).

## Context

Local state queries allow clients (like `cardano-cli`/`ogmios`/...) to fetch information from a node about its state for a particular point on the chain (like the tip of the node's current selection). Examples are the current block or slot number, the current epoch as well as various information of the ledger state, like the protocol parameters or the stake distribution. As the ledger gets more features and new use cases are explored, teams will add new queries that allow to get the necessary data. To track this, every query has an associated version number which indicates since when it is available.

In general, we can't assume that every node operator is running on the same/latest version, so a query might be available in one node, but not in another. At the beginning of every connection from a client to a node, the Network layer will negotiate a [`NodeToClientVersion`][n2c]. Before the client sends a query, it can now check that the negotiated version is not older than the associated version number of the query, and act accordingly (i.e. display an indicative error message, or use a different/fallback mechanism to do its job).

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

### Checks

The supported query versions are only enforced in the [Shelley query encoder][shelley-encodeNodeToClient], ie in code run by clients. The server will answer queries even if the negotiated version is older than expected (which might be the case with a custom client implementation), following the robustness principle.

## How to version a new query

 1. Determine whether the query is supposed to be experimental, ie only used for demo purposes/specific to an unreleased era.

 2. Check whether you need a new [`NodeToClientVersion`][n2c].

     - If the query is experimental, you only need one if there does not already exist a [`NodeToClientVersion`][n2c] beyond the [`latestReleasedNodeVersion`][latestReleasedNodeVersion] (usually, it should already exist).

     - If the query is not experimental, you need one if the current [`latestReleasedNodeVersion`][latestReleasedNodeVersion] is already used in a released version of the node. For this, check the version of `ouroboros-consensus-cardano` in the latest node release, and navigate to the corresponding [`latestReleasedNodeVersion`][latestReleasedNodeVersion].

    If positive, create a corresponding PR in the [Network repository][network-repo], and wait for a new release to CHaP.

 3. Depending on the previous step:

     - if necessary, add a new [`CardanoNodeToClientVersion`][cardano-n2c] and adapt [`supportedNodeToClientVersions`][supportedNodeToClientVersions] (as well as Shelley's [`supportedNodeToClientVersions`][shelley-supportedNodeToClientVersions], which is only used in tests)

     - if necessary, add a new [`ShelleyNodeToClientVersion`][shelley-n2c] and adapt [`querySupportedVersion`][query-supported-version]

 4. In many cases, these changes will make our golden tests fail (due to missing files); to fix CI, run the tests locally and check in the newly generated files.

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
