# How to version a new query

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

[n2c]: https://ouroboros-network.cardano.intersectmbo.org/ouroboros-network/Ouroboros-Network-NodeToClient.html#t:NodeToClientVersion
[latestReleasedNodeVersion]: https://github.com/IntersectMBO/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/Node.hs#L551
[network-repo]: https://github.com/IntersectMBO/ouroboros-network
[cardano-n2c]: https://github.com/IntersectMBO/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/Node.hs#L341-L527
[supportedNodeToClientVersions]: https://github.com/IntersectMBO/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/Node.hs#L540
[shelley-n2c]: https://github.com/IntersectMBO/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/NetworkProtocolVersion.hs#L17
[query-supported-version]: https://github.com/IntersectMBO/ouroboros-consensus/blob/35e444f1440cef34e0989519f025231241397674/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Ledger/Query.hs#L537-L574
