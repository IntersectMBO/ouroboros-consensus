# Ouroboros-consensus Cardano Changelog

# Changelog entries

<a id='changelog-0.23.0.0'></a>
## 0.23.0.0 -- 2025-04-03

### Breaking

- Define `blockQueryIsSupportedOnVersion` for Byron and Shelley.
  - For Shelley, this is just a relocation of the now gone `querySupportedVersion` function.
- Update now that `ImmutableEraParams` was split out

<a id='changelog-0.22.0.0'></a>
## 0.22.0.0 -- 2025-03-25

### Breaking

 - Removed legacy `{Shelley,Cardano}NodeToClientVersion`s and related code.

- Adapt to the change in block application and ticking interface in
  Byron and Shelley. Block application and ticking now can choose
  validation policy and enable or disable ledger events.

- Move `Ouroboros.Consensus.Cardano.ByronHFC` to `Ouroboros.Consensus.Byron.ByronHFC`
- Move Byron-only code from `Ouroboros.Consensus.Cardano.CanHardFork` to `Ouroboros.Consensus.Byron.ByronHFC`

- Adapt to Ledger's Crypto monomorphization. Many types and fields have lost their `c/crypto` type variable as now `StandardCrypto` is used by the Ledger everywhere.
- Deprecate the `StandardX` type aliases.

- The serialization of `ApplyTxErr` for eras in between Shelley and
  Babbage (both included) has changed from using an indefinite-length
  list to using a definite-length list. This is still conformant to
  the CDDL spec (as CDDL does not explicit whether lists are encoded
  with definite or indefinite lengths), but it might require a change
  in client codecs to account for this change.

- Use new `NodeToClientV_20`.
- Expose new query `QueryStakePoolDefaultVote` in new `ShelleyNodeToClientVersion12`.

- Query `GetProposedPParamsUpdates` has been deprecated and is not
  supported in `ShelleyNodeToClientV12`, which maps to
  `NodeToClientV_20`. This query became redundant once the chain
  transitioned to the Conway era. Moreover, the query constructor is
  kept with a dummy response for previous `NodeToClientV_X` versions,
  meaning that performing such a query in a node that is still not in
  Conway will return dummy values.

### Non-Breaking

- Add instances for `TxMeasureMetrics` to Cardano block types

- Bump upper bound on `base` dependency.

<a id='changelog-0.21.0.1'></a>
## 0.21.0.1 -- 2025-02-10

### Patch

- Bump to ouroboros-network-framework 0.16.

<a id='changelog-0.21.0.0'></a>
## 0.21.0.0 -- 2025-01-08

### Patch

- Adapted to removed `PraosParams` fields.

- fixup NodeToClientV_19 mapping to ShelleyNodeToClientVersion11

- Fixed a bug where a valid tx with less than `4` bytes less than the max tx
  size would be incorrectly rejected by the mempool.

* Use [`resource-registry`](https://hackage.haskell.org/package/resource-registry).

### Non-Breaking

- Remove references to `Ouroboros.Consensus.Fragment.InFuture`.
- Adapt the code to account for the removed `cdbFutureBlocks` and related fields.

- Updated to `ouroboros-network-0.14`, and `typed-protocols-0.3.0.0` as a consequence.
- Updated to `ouroboros-network-api-0.11`, which introduced `NodeToClientV_19`.

- Removed now-redundant hack in Babbage→Conway translation.

- Depend on `network-mux` and use its types.

### Breaking

- Remove `CheckInFuture m blk` from `openChainDB`.

- Changed `CardanoHardTriggers` to contain `CardanoHardForkTrigger`s which are a
  simpler version of the previous `TriggerHardForkAt`. In particular, this will
  affect call sites of `protocolInfoCardano`.

  Migration notes:

    - Change `TriggerHardForkAtEpoch` to `CardanoTriggerHardForkAtEpoch`.
    - Change `TriggerHardForkAtVersion` to `CardanoTriggerHardForkAtDefaultVersion`.

      This constructor does not take a version argument, but rather defaults to
      the corresponding first ledger protocol version. We are not aware of any
      use case that requires a different value, but if there is, it is still
      possible to manually modify the returned `LedgerConfig`s of
      `protocolInfoCardano` directly.

- Added ShelleyNodeToClientVersion11 for GetBigLedgerPeerSnapshot query

- Removed legacy (pre-Conway) diffusion pipelining scheme.

- Changed `Ouroboros.Consensus.Cardano.CanHardFork` to expose
  `crossEraForecastAcrossShelley`, in particular for testing.

- Change the type of the `mkKeyHashVrf` function to use the new `VRFVerKeyHash` ledger type.

- Remove versions before `NodeToClientV_16` from the `supportedNodeToClientVersions`.

<a id='changelog-0.20.0.0'></a>
## 0.20.0.0 -- 2024-10-14

### Patch

- Updates for the `TxLimits` mempool consolidation.

### Non-Breaking

- Do not check transaction sizes in the forging functions; simply include all
  given transactions.

- Remove the hotfix Babbage mempool checks.

- Dropped `NodeToNodeV_12` and below from `supportedNodeToNodeVersion` as they were
  removed from o-n-api.
- Added `NodeToNodeV_14` to `supportedNodeToNodeVersions`

### Breaking

- Added a `blockBytes` column to the output of --benchmark-ledger-ops and --show-header-size.

- Add `GetFuturePParams` query.

<a id='changelog-0.19.0.0'></a>
## 0.18.0.0 -- 2024-08-26

### Breaking

- Add `GetProposals` and `GetRatifyState` queries.
- Propagate cardano-ledger's updated `TranslationContext` `NoGenesis`
- Add `NodeToClientV_17` to `supportedNodeToClientVersions`
- Add `CardanoNodeToClientVersion13`
- Replace deprecated `TxSizeInBytes` with `SizeInBytes`
- Remove the capacity override from forging functions.
- ProtocolParams (CardanoBlock c) was monomorphized to `CardanoProtocolParams`
- Remove `cardanoProtocolParamsPerEra` from `CardanoProtocolParams` in favour of a single `cardanoProtocolVersion`.
  This patch intentionally removes the flexibility the n-ary product of versions per-era gave us, in favour of a simpler interface, as the current one has caused a lot of confusion and led to several mistakes in the past.

### Non-Breaking

- Also enabled total ref script size limit in the mempool in Conway (it
  continues to be enabled in Babbage).

<a id='changelog-0.18.0.0'></a>
## 0.18.0.0 -- 2024-07-02

### Non-Breaking

- Update ledger packages:
  - `cardano-ledger-alonzo`: `^>=1.10`
  - `cardano-ledger-babbage`: `^>=1.8.2`
  - `cardano-ledger-conway`: `^>=1.16.0`
  - `cardano-ledger-core`: `^>=1.13.2`

### Breaking

- Added `GetSPOStakeDistr` query

- Added `TickedLedgerState` argument to `txMeasure`.

- Introduced `ConwayMeasure`, a Conway-specific `TxMeasure` adding the total
  reference scripts size as a new dimension on top of `AlonzoMeasure`.

- Update N2C part of `latestReleasedNodeVersion` to `NodeToClientV_16`.

<a id='changelog-0.17.1.0'></a>
## 0.17.1.0 -- 2024-06-26

### Non-Breaking

- Enforce Conway minfees for ref scripts in the mempool, even in Babbage

<a id='changelog-0.17.0.0'></a>
## 0.17.0.0 -- 2024-06-19

### Breaking

- Changed `GetStakeDistribution` and `GetPoolDistr` queries to return a
  Consensus-vendored version of `PoolDistr` (instead of using the one from
  Ledger, as it changed in a backwards-incompatible manner).

<a id='changelog-0.16.0.0'></a>
## 0.16.0.0 -- 2024-05-13

### Patch

- Update `SupportsTwoPhaseValidation` instance for `Conway`:
  `ConwayUtxowFailure` was changed upstream

- Adapt to `Ouroboros.Consensus.Protocol.Praos.Translate` removal.

### Non-Breaking

- Added instances for `BlockSupportsDiffusionPipelining`.

- Use new issuer-based diffusion pipelining criterion starting in Conway.

- Set Genesis window for Byron and Shelley.

- Bump to `HardForkSpecificNodeToClientVersion3` for
  `CardanoNodeToClientVersion12` to account for serialisation change of
  `EraParams`.

- Update ledger packages:
  - `cardano-ledger-alonzo`: `^>=1.8`
  - `cardano-ledger-babbage`: `^>=1.8`
  - `cardano-ledger-conway`: `^>=1.14`
  - `cardano-ledger-core`: `^>=1.12`
  - `cardano-ledger-mary`: `^>=1.6`
  - `cardano-ledger-shelley`: `^>=1.11`

- Adapt `QueryLedger` instances to the renaming to `BlockSupportsLedgerQuery`.

### Breaking

- Restricted the VRF tiebreaker based on slot distance starting in Conway.

- Add `GetAccountState` query

- Remove `getProposedProtocolVersion` and `defaultGetProposedProtocolVersion`
- Remove `ProtocolUpdate`, `UpdateProposal`, `UpdateState` and `protocolUpdates`
- Add `pparamsUpdate`
- Change `ShelleyLedgerUpdate` by replacing `ShelleyUpdatedProtocolUpdates` with `ShelleyUpdatedPParams`

<a id='changelog-0.15.0.0'></a>
## 0.15.0.0 -- 2024-04-03

### Patch

- Bugfix: Add an extra case to `isIncorrectClaimedFlag` for `Babbage` and `Conway` (https://github.com/IntersectMBO/ouroboros-consensus/issues/973).

### Non-Breaking

- Change the randomness stabilization window for Conway (and future eras) to
  `4k/f` instead of `3k/f` (one stability window) that was used for Babbage and
  TPraos-based eras. See erratum 17.3 in the Shelley ledger specs for context.

  Note that this is a backwards-incompatible change for all existing chains
  containing (at least one full epoch worth of) Conway blocks.

- Use the version of cardano-git-rev in the cardano-base repo.

- Bump `NodeToNodeVersion` part of `latestReleasedNodeVersion` to
  `NodeToNodeV_13` from `NodeToNodeV_11`.

- Add `ConvertRawTxId` instances for Shelley and Byron.

- Update ledger packages
  - `cardano-ledger-allegra`: `^>=1.4`
  - `cardano-ledger-alonzo`: `^>=1.7`
  - `cardano-ledger-api`: `^>=1.9`
  - `cardano-ledger-babbage`: `^>=1.7`
  - `cardano-ledger-conway`: `^>=1.13`
  - `cardano-ledger-core`: `^>=1.11`
  - `cardano-ledger-shelley`: `^>=1.10`

- Update `plutus-tx-plugin`: `>= 1.23`

- Update `cardano-slotting`: `>=0.2`

### Breaking

- Implement lightweight checkpointing [#449](https://github.com/IntersectMBO/ouroboros-consensus/issues/449).
  A validation to help nodes follow the historical chain. A new field `cardanoCheckpoints`
  has been added to the record `ProtocolParams (CardanoBlock c)` record, with a new type `CheckpointsMap`.

- Remove `shelleyBasedEraName` from `ShelleyBasedEra`, use `eraName` from
  `Cardano.Ledger.Api.Era` instead.

<a id='changelog-0.14.0.0'></a>
## 0.14.0.0 -- 2024-02-23

### Non-Breaking

- Remove redundant `DiskPolicy` argument from call to `ChainDB.defaultArgs`

- Integrate with network-packages and io-sim 1.4.1 packages
- Bump dependencies version bounds

<a id='changelog-0.13.0.0'></a>
## 0.13.0.0 -- 2024-01-29

### Patch

- Fix imports and type mismatches caused by `ouroboros-consensus` bumping
  `strict-checked-vars` to `^>= 0.2`.

- Improved Haddock comments that explain how to control which eras are enabled
  when constructing a `ProtocolInfo`.

- Updated to GHC 9.8.1.

### Non-Breaking

- Re-enable completeness warnings in Ouroboros.Consensus.Cardano.Node only on GHC>=9.6

- `CardanoNodeToClientVersion12` now implies `ShelleyNodeToClientVersion8` in
  all Shelley-based eras instead of just in `Conway`. This means that downstream
  logic determining query availablity (eg in `cardano-api`) can (continue to)
  simply branch on the negotiated `NodeToClientVersion`.

- Update dependency on `ouroboros-network-api` to `^>=0.6.2`.

- Update ledger packages
  - `cardano-ledger-allegra`: `^>=1.3`
  - `cardano-ledger-alonzo`: `^>=1.6`
  - `cardano-ledger-api`: `^>=1.8`
  - `cardano-ledger-babbage`: `^>=1.6`
  - `cardano-ledger-binary`: `^>=1.3`
  - `cardano-ledger-conway`: `^>=1.12`
  - `cardano-ledger-core`: `^>=1.10`
  - `cardano-ledger-mary`: `^>=1.5`
  - `cardano-ledger-shelley`: `^>=1.9`
  - `cardano-protocol-tpraos`: `^>=1.1`

### Breaking

- Removed `EraNodeToNodeVersion`, replacing it with `WrapNodeToNodeVersion`.
- Removed `CardanoNodeToNodeVersion3` and above, since they're all equivalent
  to `CardanoNodeToNodeVersion2` now.

- The type system now prohibits Conway-specific queries for Shelley-based eras
  before Conway, by adding the `ConwayEraGov era` constraint to those query
  constructors.
- The new `getConwayEraGovDict` enables the decoder to find the necessary
  dictionaries and issue a specific error message if there isn't one.

<a id='changelog-0.12.1.0'></a>
## 0.12.1.0 -- 2023-11-30

### Non-Breaking

- Enable `NodeToNodeV_13` protocol.

- New tests to check that all network versions are supported by the Shelley and
  Cardano blocks.

<a id='changelog-0.12.0.0'></a>
## 0.12.0.0 -- 2023-11-14

### Non-Breaking

- Updates dependency on `ouroboros-network-api` to `0.6.0`.

- Update ledger packages
  - `cardano-ledger-allegra`: `^>=1.2.5`
  - `cardano-ledger-alonzo`: `^>=1.5.0 -> ^>=1.5.1`
  - `cardano-ledger-babbage`: `^>=1.5.0 -> ^>=1.5.1`
  - `cardano-ledger-binary`: `^>=1.2.1`
  - `cardano-ledger-byron`: `^>=1.0`
  - `cardano-ledger-conway`: `^>=1.10.0 -> ^>=1.11`
  - `cardano-ledger-core`: `^>=1.8.0 -> ^>=1.9`
  - `cardano-ledger-mary`: `^>=1.4`
  - `cardano-ledger-shelley`: `^>=1.7.0 -> ^>=1.8`
  - `cardano-protocol-tpraos`: `^>=1.0.3.6 -> ^>=1.0.3.7`

### Breaking

- Add a query for vote delegatees: GetFilteredVoteDelegatees

<a id='changelog-0.11.0.0'></a>
## 0.11.0.0 -- 2023-10-26

### Patch

- `TriggerHardForkNever` was renamed to `TriggerHardForkNotDuringThisExecution` upstream.

### Non-Breaking

- Updated to `typed-protocols-0.1.1`
- Add `getProposedProtocolVersion` to `ShelleyBasedEra` class

### Breaking

- Remove `Ticked (LedgerView X)` data family instances.
- Remove `toTickedPBftLedgerView`.

 - Updated to newer `cardano-ledger-*` packages:
    * `cardano-ledger-api-1.7`
    * `cardano-ledger-core-1.8`
    * `cardano-ledger-shelley-1.7`
    * `cardano-ledger-babbage-1.5`
    * `cardano-ledger-conway-1.10`
    * `cardano-protocol-tpraos-1.0.3.6`
 - Replaced `GetCommitteeState` query with `GetCommitteeMembersState`

<a id='changelog-0.10.0.0'></a>
## 0.10.0.0 -- 2023-09-27

### Non-Breaking

 - Updated to newer `cardano-ledger-*` packages:
    * `cardano-ledger-api-1.6`
    * `cardano-ledger-core-1.7`
    * `cardano-ledger-conway-1.9`

 - Protocol parameters (in particular the version number) in the ledger state
   are now updated properly on the Babbage→Conway era transition.

### Breaking

 - `CardanoProtocolParams` now contains a `TransitionConfig` (new Ledger
   concept) and the `CardanoHardForkTriggers` instead of the previous per-era
   `ProtocolTransitionParams`.

 - Removed `ShelleyGenesis` from `ProtocolParamsShelleyBased` as a
   `TransitionConfig` already contains a `ShelleyGenesis`.

 - Removed `registerInitialFunds` and `registerGenesisStaking`, these now live
   in Ledger as part of the `EraTransition` class.

 - Removed now-unused `Ouroboros.Consensus.Cardano.ShelleyBased` module.

<a id='changelog-0.9.0.0'></a>
## 0.9.0.0 -- 2023-09-06

### Non-Breaking

- Updated to newer `cardano-ledger-*` packages:
    * `cardano-ledger-api-1.5`,
    * `cardano-ledger-alonzo-1.4.1`
    * `cardano-ledger-shelley-1.6`
    * `cardano-ledger-babbage-1.4.4`
    * `cardano-ledger-conway-1.8`
    * `cardano-ledger-tpraos-1.0.3.5`

### Breaking

- Use `ouroboros-network-framework-0.8`. Types of
  `Cardano.Tools.ImmDBServer.MiniProtocols.immDBServer` and
  `Cardano.Tools.ImmDBServer.Diffusion.serve` have changed.

<a id='changelog-0.8.0.1'></a>
## 0.8.0.1 -- 2023-08-21

### Patch

- Removed the `expose-sublibs` cabal flag, since Cabal/Nix handled it poorly.
- Instead, added a `unstable-` prefix to the name of each sublibrary, to
  strongly indicate that we ignore them when evolving the package's version.

<a id='changelog-0.8.0.0'></a>
## 0.8.0.0 -- 2023-08-18

### Patch

- Update `fs-api` dependency to `^>=0.2`

### Non-Breaking

- Add new `ProtocolInfo` module to `cardano-testlib`, containing utilities for
  creating Cardano `ProtocolInfo`s for testing purposes.

- Expose the latest Conway ledger queries.
    - `GetCommitteeState`
    - `GetConstitution`
    - `GetDRepStakeDistr setOfDReps`
    - `GetDRepState setOfDRepCredentials`
    - `GetGovState`

- Add a `ProtocolTransitionParams` data family, and provide instances for
  transitions from Byron to Shelley and Shelley-based eras to Shelley-based
  eras.
- Add a data instance of `ProtocolParams` for the Cardano block.
- Provide a `CardanoProtocolParams` type synonym and associated pattern synonym
  (with record syntax).
- Export all `ProtocolParams` and `ProtocolTransitionParams` instances from
  `Ouroboros.Consensus.Cardano` and `Ouroboros.Consensus.Cardano.Node`.

### Breaking

- Update ledger dependencies to pull in the latest Conway changes.
    - `cardano-ledger-conway` from `^>=1.6` to `^>=1.7`
    - `cardano-ledger-alonzo` from `^>=1.3` to `^>=1.4`
    - `cardano-ledger-api` from `^>=1.3` to `^>=1.4`
    - `cardano-ledger-core` from `^>=1.4` to `^>=1.5`
    - `cardano-ledger-shelley` from `^>=1.4.1` to `^>=1.5`
- Remove the `degenerateAlonzoGenesis` declaration.
- Delete the (as of yet unreleased) `GetConstitutionHash`.
    - Use `anchorDataHash . constitutionAnchor <$> GetConstitution` instead.

- Refactor `ProtocolParamsByron` to a data instance of `ProtocolParams`.
- Refactor protocol parameters for Shelley eras (e.g, `ProtocolParamsAlonzo` and `ProtocolParamsBabbage`) to data instances of `ProtocolParams`.
- Export all Shelley `ProtocolParams` data instances from `Ouroboros.Consensus.Shelley.Node`.
- Remove the `ProtocolTransitionParamsShelleyBased` datatype in favour of
  `ProtocolTransitionParams`.
- Make `protocolInfoCardano` require a `CardanoProtocolParams` type as its only
  argument, instead of a long list of arguments.

<a id='changelog-0.7.0.0'></a>
## 0.7.0.0 -- 2023-07-06

### Non-Breaking

- Refactor code because block forging credentials got extracted out of
  `ProtocolInfo` type.

### Breaking

- Change the return type of numerous functions to include block forging credentials since
  they got extracted out of `ProtocolInfo` type.
  - Refactor the type signatures to accommodate the fact that `ProtocolInfo` does not
  need monad type variable.

- Add `GetConstitutionHash` ledger query

<a id='changelog-0.6.1.0'></a>
## 0.6.1.0 -- 2023-06-23

### Patch

- Rename `StrictMVar` to `StrictSVar`

- Add support for new `cardano-ledger` package versions

### Non-Breaking

- Call `cryptoInit` before running test suites

- Make sure `defaultMainWithTestEnv` is used everywhere

- Call `cryptoInit` in `defaultMainWithTestEnv`

- Call `cryptoInit` in our utility tools

<a id='changelog-0.6.0.0'></a>
## 0.6.0.0 -- 2023-05-19

### Patch

- Optimise `GetStakeSnapshots` query to not traverse all delegations
  per stake pool, but instead compute the total stake per pool in a
  map and do a lookup
- Update CHaPs dependencies
- Fix performance regression of `GetFilteredDelegationsAndRewardAccounts` query
- Remove deprecated pattern synonyms from `Ouroboros.Consensus.Shelley.Ledger`:
  `GetUTxO` and `GetFilteredUTxO`.

### Breaking

- Bumped latest released node versions to `NodeToNodeV_11` and `NodeToClientV_15`.
- `NodeToClientV_15` enables the deposits query.
- The `GetCurrentPParams` query now uses the legacy en-/decoding for its result again when the `NodeToClientVersion` is `<15`, restoring compatibility with older clients.

### Non-Breaking

- Bump `cardano-ledger-{alonzo,babbage}` to 1.2.1.0, which changes the corresponding `PParams` serialisation. This affects the ledger snapshots, and the `GetCurrentPParams` query for `NodeToClientVersion >= 15`.

<a id='changelog-0.5.0.1'></a>
## 0.5.0.1 -- 2023-04-28

### Patch

- Update `ouroboros-network` dependency.

<a id='changelog-0.5.0.0'></a>
## 0.5.0.0 - 2023-04-24

### Breaking

- Apply new organization of Consensus packages. Absorb `byron`, `shelley`,
  `cardano-tools` and all the testing packages for those.

- Add the new `ShelleyNodeToClientVersion7` to the `ShelleyNodeToClientVersion`

### Non-Breaking

- Add a new ledger query: `GetStakeDelegDeposits`

<a id='changelog-0.4.0.1'></a>
## 0.4.0.1 -- 2023-04-10

### Patch

- `ouroboros-consensus-cardano`: Since the filesystem API that lives in
  `ouroboros-consensus` will live in the `fs-api` package for now on, start
  depending on `fs-api`, and change imports accordingly.

- Collapse all imports into one group in every file.
- Adapt to relocation of SOP-related `Util` modules.

<a id='changelog-0.4.0.0'></a>
## 0.4.0.0 -- 2023-03-07

### Non-Breaking

- Fix the version bounds for the bundle and version sets the bounds for the
  `ouroboros-consensus` bundle to `^>=0.3`.

### Breaking

- Return stake snapshots for stake pools that have the `Set` or `Go` ledger
  snapshots. When querying `GetStakeSnapshots Nothing`, which means to query for
  all stake pools, previously only stake snapshots for stake pools that have the
  `Mark` ledger snapshot were returned.

<a id='changelog-0.3.0.0'></a>
## 0.3.0.0 -- 2023-02-09

### Patch

- Remove redundant proxy argument for `ledgerDbTip`.

### Non-Breaking

- Adapted to the new reorganization of Mempool modules in `ouroboros-consensus`.

### Breaking

####  Added:
- `Conway` to `CardanoEras`
- `NodeToNodeV_11` and `NodeToClientV_15`, both of which enable Conway.
- Conway-related type and pattern synonyms. Eg `StandardConway`, `HeaderConway`,
  `GentTxConway`, etc.

#### Changed

- The `protocolInfoTPraosShelleyBased` and `protocolInfoPraosShelleyBased`
  functions now expect a tuple of the `AdditionalGenesisConfig` and the
  `TranslationContext` instead of just the `TranslationContext`. For all
  Shelley-based eras before Conway, those had been equal types.

---

### Archaeological remark

Before following a more structured release process, we tracked most significant
changes affecting downstream users in the
[interface-CHANGELOG.md](https://github.com/IntersectMBO/ouroboros-consensus/blob/8d8329e4dd41404439b7cd30629fcce427679212/docs/website/docs/interface-CHANGELOG.md).
