<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
-->

<!--
### Patch

- A bullet item for the Patch category.

-->

### Non-Breaking

- Add a `ProtocolTransitionParams` data family, and provide instances for
  transitions from Byron to Shelley and Shelley-based eras to Shelley-based
  eras.
- Add a data instance of `ProtocolParams` for the Cardano block.
- Provide a `CardanoProtocolParams` type synonym and associated pattern synonym
  (with record syntax).
- Export all `ProtocolParams` and `ProtocolTransitionParams` instances from
  `Ouroboros.Consensus.Cardano` and `Ouroboros.Consensus.Cardano.Node`.

### Breaking

- Refactor `ProtocolParamsByron` to a data instance of `ProtocolParams`.
- Refactor protocol parameters for Shelley eras (e.g, `ProtocolParamsAlonzo` and `ProtocolParamsBabbage`) to data instances of `ProtocolParams`.
- Export all Shelley `ProtocolParams` data instances from `Ouroboros.Consensus.Shelley.Node`.
- Remove the `ProtocolTransitionParamsShelleyBased` datatype in favour of
  `ProtocolTransitionParams`.
- Make `protocolInfoCardano` require a `CardanoProtocolParams` type as its only
  argument, instead of a long list of arguments.