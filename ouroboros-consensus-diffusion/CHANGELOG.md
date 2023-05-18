# Ouroboros-consensus-diffusion Changelog

# Changelog entries

<a id='changelog-0.5.1.0'></a>
## 0.5.1.0 — 2023-04-28

### Non-Breaking

- Update `io-sim` dependency to 1.1.0.0.

- Update `ouroboros-network` dependency.

<a id='changelog-0.5.0.0'></a>
## 0.5.0.0 - 2023-04-24

### Breaking

- Apply new organization of Consensus packages. Absorb the ThreadNet tests and
  the ledger-agnostic test-suites that use them.

<a id='changelog-0.4.0.0'></a>
## 0.4.0.0 - 2023-04-21

### Breaking

- Peer Sharing Integration:
  - Monomorphized `Handlers` client field types to `ConnectionId addr`;
    and added PeerSharing handlers.
  - Changed `mkHandlers` function to receive a function to compute peer sharing addresses;
  - Changed `Codecs` type and propagated changes to relevant functions (e.g. `defaultCodecs`);
  - Changed `Apps` type and propagated changes to relevant functions (e.g. `mkApps`);
  - `initiatorAndResponder` receives PeerSharing value;
  - Added PeerSharing field to `RunNodeArgs`;
  - Changed `runWith` to receive necessary parameters;
  - `NodeKernel` changes to incorporate PeerSharing miniprotocol (adds `PeerSharingRegistry`);

- Extract `ouroboros-consensus-diffusion` from the bundle of packages.

### Non-Breaking

- Renamed address type variables to more consistent naming

- Update chainsync timeout: Increase the minimum timeout from 90s to 135s and
  switch from picking from an array of 5 values to a range of timeouts. This
  change reduces the risk of synchronisation among nodes in the network.

### Patch

- `ouroboros-consensus-diffusion`: `ouroboros-network` packages version bumps.

## Before 0.4.0.0

Before this version, `ouroboros-consensus-diffusion` lived in a bundle of
packages with `ouroboros-consensus`, thus the changelog was the same.
