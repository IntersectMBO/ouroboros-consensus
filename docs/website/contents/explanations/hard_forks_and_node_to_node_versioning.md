# Hard forks and node-to-node versioning

Part of: [System Overview](index.md)

Does a hard-fork release require bumping the node-to-node version?
**Short answer: no.**

A [hard fork](../references/glossary.md#hard-forks) is triggered by the on-chain ledger protocol major version, which governance sets.
It is not triggered by the network handshake version.
Adding the new era to the node can reuse the existing [node-to-node](../references/glossary.md#node-to-node-protocol) version.

## Five different "versions"

The question is easy to get wrong, because several separate things all get called a "version". Three of them drive the argument below:

1. **[On-chain protocol version](../references/miscellaneous/era_transition_governance.md)** (ledger `major.minor`, e.g. major 9 = Conway).
   Bumping the major version *is* the hard fork.
   Governance controls it.
2. **Negotiated `NodeToNodeVersion`** (eg the integer 14/15/16 sent in the handshake).
   Controls the wire codecs and which mini-protocols are available.
3. **`BlockNodeToNodeVersion` / `CardanoNodeToNodeVersion`** (the [HFC](../references/glossary.md#hard-fork-combinator) block-level version).
   Controls how a Cardano block is serialised across eras.

Two more carry the "version" name but do not drive this argument:

4. **Header protocol version** (the `ProtVer major.minor` carried in each block header).
   The producer sets it to the highest protocol version its node understands (its `cardanoProtocolVersion`), as an upgrade-readiness signal; it is not the version the block was built under.
5. **Negotiated `NodeToClientVersion`** (the handshake version for the node-to-*client* protocols used by wallets, the CLI, and db-sync).
   The node-to-client counterpart of (2).

The consensus code states outright that (2) and (3) are independent of (1).
See [`NetworkProtocolVersion.hs#L25-L33`](https://github.com/IntersectMBO/ouroboros-consensus/blob/bd119d907b7701aac5f7825e6645614a7473e9bf/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Node/NetworkProtocolVersion.hs#L25-L33):

```haskell
-- | Protocol versioning
--
-- IMPORTANT Note that this is entirely independent of the
-- 'Ouroboros.Consensus.Shelley.Node.TPraos.shelleyProtVer' field et al.
--
-- Its primary purpose is to control the details of on-the-wire codecs.
```

## Where eras live: the block codec version

Why care about the block codec?
A hard fork puts a new kind of block on the wire.
Nodes send blocks, headers, and transactions to each other over the node-to-node mini-protocols, and the block codec version decides how each one is encoded.
It is the version argument to the encode and decode functions.
See [`Serialisation.hs#L68-L72`](https://github.com/IntersectMBO/ouroboros-consensus/blob/bd119d907b7701aac5f7825e6645614a7473e9bf/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Node/Serialisation.hs#L68-L72):

```haskell
-- | Serialise a type @a@ so that it can be sent across network via a
-- node-to-node protocol.
class SerialiseNodeToNode blk a where
  encodeNodeToNode :: CodecConfig blk -> BlockNodeToNodeVersion blk -> a -> Encoding
  decodeNodeToNode :: CodecConfig blk -> BlockNodeToNodeVersion blk -> forall s. Decoder s a
```

Adding an era changes how that era's blocks, headers, and transactions serialise.
On the node-to-node side, that serialisation is controlled by `BlockNodeToNodeVersion blk`.
The rest of this section shows the value is one codec per era, not one codec for all.

`BlockNodeToNodeVersion (CardanoBlock c)` reduces to `HardForkNodeToNodeVersion (CardanoEras c)`.
[`HardForkNodeToNodeVersion`](https://github.com/IntersectMBO/ouroboros-consensus/blob/bd119d907b7701aac5f7825e6645614a7473e9bf/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/HardFork/Combinator/NetworkVersion.hs#L47-L63) has two constructors: `HardForkNodeToNodeDisabled` (the HFC off, used only before Shelley) and `HardForkNodeToNodeEnabled` ([`NetworkVersion.hs#L60-L63`](https://github.com/IntersectMBO/ouroboros-consensus/blob/bd119d907b7701aac5f7825e6645614a7473e9bf/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/HardFork/Combinator/NetworkVersion.hs#L60-L63)).
For the Cardano block, `HardForkNodeToNodeDisabled` appears only in `CardanoNodeToNodeVersion1`, which the node no longer advertises (the supported-versions map below lists only `CardanoNodeToNodeVersion2`), so it is unused on the Cardano side (though the single-era `ByronHFC` and `ShelleyHFC` instances still construct it).
In particular, in [`Node.hs#L280-L293`](https://github.com/IntersectMBO/ouroboros-consensus/blob/bd119d907b7701aac5f7825e6645614a7473e9bf/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/Node.hs#L280-L293) we have:

```haskell
pattern CardanoNodeToNodeVersion2 :: BlockNodeToNodeVersion (CardanoBlock c)
pattern CardanoNodeToNodeVersion2 =
  HardForkNodeToNodeEnabled
    HardForkSpecificNodeToNodeVersion1                      -- era-tag version
    ( WrapNodeToNodeVersion ByronNodeToNodeVersion2         -- ByronBlock
        :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1  -- ShelleyBlock (TPraos c) ShelleyEra
        :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1  -- ShelleyBlock (TPraos c) AllegraEra
        :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1  -- ShelleyBlock (TPraos c) MaryEra
        :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1  -- ShelleyBlock (TPraos c) AlonzoEra
        :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1  -- ShelleyBlock (Praos c) BabbageEra
        :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1  -- ShelleyBlock (Praos c) ConwayEra
        :* WrapNodeToNodeVersion ShelleyNodeToNodeVersion1  -- ShelleyBlock (Praos c) DijkstraEra
        :* Nil )
```

One entry per era, so a hard fork appends an entry and leaves the existing ones unchanged.
It creates no new `CardanoNodeToNodeVersion`, and every existing era still serialises exactly as before.

## The supported-versions map

The node declares which node-to-node versions it speaks in `supportedNodeToNodeVersions`.
Its type already keeps two things apart: the negotiated handshake version and the block codec version.
See [`NetworkProtocolVersion.hs#L50-L57`](https://github.com/IntersectMBO/ouroboros-consensus/blob/bd119d907b7701aac5f7825e6645614a7473e9bf/ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Node/NetworkProtocolVersion.hs#L50-L57):

```haskell
class HasNetworkProtocolVersion blk => SupportedNetworkProtocolVersion blk where
  -- | Enumerate all supported node-to-node versions
  supportedNodeToNodeVersions ::
    Proxy blk -> Map NodeToNodeVersion (BlockNodeToNodeVersion blk)
```

The `Map` type already separates the two.
Its key is `NodeToNodeVersion`, the integer negotiated in the handshake; its value is `BlockNodeToNodeVersion blk`, the block codec version.
So the handshake version and the block codec are, by construction, two separate things.

The Cardano instance fills the map like this.
See [`Node.hs#L437-L442`](https://github.com/IntersectMBO/ouroboros-consensus/blob/bd119d907b7701aac5f7825e6645614a7473e9bf/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/Node.hs#L437-L442):

```haskell
supportedNodeToNodeVersions _ =
  Map.fromList $
    [ (NodeToNodeV_14, CardanoNodeToNodeVersion2)
    , (NodeToNodeV_15, CardanoNodeToNodeVersion2)
    , (NodeToNodeV_16, CardanoNodeToNodeVersion2)
    ]
```

We can see that bumping the handshake integer from 14 to 15 to 16 did not change the block codec.

## The answer, and the mainnet caveat

Does a hard fork *require* a node-to-node bump? That depends on what "require" means.
One is about the wire format: does the new era need a node-to-node bump to serialise and deserialise?
The other is operational: does a release bump the version anyway, to force upgrades around the fork?
The two have different answers.

**For the new era's serialisation: no, as long as the change only appends.**
Since [`ed49cd11b`](https://github.com/IntersectMBO/ouroboros-consensus/commit/ed49cd11b2318a201bba6ef3fe0ce514461da015), the block codec version cannot disable an era, and every node-to-node payload self-identifies its era.
Adding an era appends one always-on entry, as Dijkstra did to `CardanoNodeToNodeVersion2` above, and leaves every existing era's per-era codec untouched.
A not-yet-active era is held back by the max-major protocol-version check, not by the node-to-node version.
That check is in the consensus protocol's envelope validation.
See [`Praos.hs#L111-L122`](https://github.com/IntersectMBO/ouroboros-consensus/blob/bd119d907b7701aac5f7825e6645614a7473e9bf/ouroboros-consensus-cardano/src/shelley/Ouroboros/Consensus/Shelley/Protocol/Praos.hs#L111-L122):

```haskell
envelopeChecks cfg lv hdr = do
  unless (m <= maxpv) $ throwError (ObsoleteNode m maxpv)
  ...
 where
  (MaxMajorProtVer maxpv) = praosMaxMajorPV pp
  (ProtVer m _) = lvProtocolVersion lv
```

`m` is the protocol major version currently in force on-chain, read from the ledger view (`lvProtocolVersion lv`); `maxpv` is the highest major version the node supports.
If that on-chain version exceeds `maxpv`, the header is rejected as `ObsoleteNode`.
That maximum comes from the protocol version the node's software and config declare, not from the handshake.
See [`Node.hs#L640-L641`](https://github.com/IntersectMBO/ouroboros-consensus/blob/bd119d907b7701aac5f7825e6645614a7473e9bf/ouroboros-consensus-cardano/src/ouroboros-consensus-cardano/Ouroboros/Consensus/Cardano/Node.hs#L640-L641):

```haskell
maxMajorProtVer :: MaxMajorProtVer
maxMajorProtVer = MaxMajorProtVer $ pvMajor cardanoProtocolVersion
```

So no new node-to-node version is needed to carry the new era.

The exception is changing how an *existing* era serialises on the wire.
Each era is pinned to a per-era codec version, threaded into the `encodeNodeToNode` / `decodeNodeToNode` methods that serialise that era's block, header, transaction, and transaction id.
The Shelley-based eras all use `ShelleyNodeToNodeVersion1`, but a per-era version can grow: Byron already has two, `ByronNodeToNodeVersion1` and `ByronNodeToNodeVersion2` (headers without vs with a size hint).
Byron's header codec branches on the version to pick the layout.
See [`Serialisation.hs#L89-L105`](https://github.com/IntersectMBO/ouroboros-consensus/blob/bd119d907b7701aac5f7825e6645614a7473e9bf/ouroboros-consensus-cardano/src/byron/Ouroboros/Consensus/Byron/Node/Serialisation.hs#L89-L105):

```haskell
instance SerialiseNodeToNode ByronBlock (Header ByronBlock) where
  encodeNodeToNode ccfg = \case
    ByronNodeToNodeVersion1 ->
      wrapCBORinCBOR $
        encodeUnsizedHeader . fst . splitSizeHint
    ByronNodeToNodeVersion2 ->
      encodeDisk ccfg . unnest
  ...
```

Bumping an existing era's codec while still supporting peers that speak the old one would need a second `CardanoNodeToNodeVersion` holding the new per-era version, mapped to a new negotiated `NodeToNodeVersion`, so peers can negotiate which codec to use.
That is a node-to-node bump; adding an era does not do it.

**As an upgrade gate around a hard fork: sometimes, by choice.**
Conway's most recent hard fork, Plomin, made `NodeToNodeV_14` mandatory on 2025-01-29, per the comment on that constructor in the [`NodeToNodeVersion` enum](https://github.com/IntersectMBO/ouroboros-network/blob/e8d59d8a219563760fc21ba5bc86fab77d886742/cardano-diffusion/api/lib/Cardano/Network/NodeToNode/Version.hs#L71-L72) (in `ouroboros-network`).
That bump forces nodes to upgrade before the fork.
It is a coordination step, not an era-codec requirement: V_14 itself lists no wire-format change; the changes shipped around then were in V_13, and were all PeerSharing-related.

So a hard-fork release does not need a node-to-node bump for the new era's sake.
A release may still bump the version and make it mandatory as a coordination step, which is a separate operational decision.

## Further reading

- [Era transition governance](../references/miscellaneous/era_transition_governance.md): how on-chain governance ends an era by incrementing the protocol major version, with the era-to-version table.
- [Adding an era](../howtos/adding_an_era.md): the checklist for adding a new era to the node.
- [Key type families and classes](../references/key_type_families_and_classes.md): `HasNetworkProtocolVersion` and the block-version type families used here.
