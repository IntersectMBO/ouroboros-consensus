# Ouroboros-consensus-protocol Changelog

# Changelog entries

<a id='changelog-0.9.0.0'></a>
## 0.9.0.0 — 2024-05-13

### Non-Breaking

- Adapted to introduction of new `ChainOrder` type class.

- Changed the Praos chain order such that for two blocks `A` and `B` by the same
  issuer with the same block number, `A` is now preferred over `B` only if `A`
  has a higher issue number *and* (new) `A` and `B` are in the same slot.

  This is in line with the motiviation for the issue number tiebreaker, and
  fixes the transitivity of the `Ord PraosChainSelectView` instance in a special
  case.

### Breaking

- Allowed to configure Praos chain order to restrict the VRF tiebreaker based on
  slot distance.

- Delete `Ouroboros.Consensus.Protocol.Praos.Translate`, moving the orphan
  instance to `Ouroboros.Consensus.Protocol.Praos`.
- Delete `Ouroboros.Consensus.Protocol.Translate`, moving `TranslateProto`
  to `Ouroboros.Consensus.Protocol.Abstract`.

<a id='changelog-0.8.0.0'></a>
## 0.8.0.0 — 2024-04-03

### Breaking

- Remove unused `translateConsensusConfig` from `TranslateProto`.

- Add `praosRandomnessStabilisationWindow` to `PraosParams`, allowing to
  configure in which slot the epoch nonce is snapshotted (which can now vary
  between different eras).

<a id='changelog-0.7.0.0'></a>
## 0.7.0.0 — 2024-01-29

### Patch

- Fix imports and type mismatches caused by `ouroboros-consensus` bumping
  `strict-checked-vars` to `^>= 0.2`.

### Breaking

- Praos' `LedgerView.lvMaxHeaderSize` type changed from `Natural` to `Word16`.
- Praos' `LedgerView.lvMaxBoxySize` type changed from `Natural` to `Word32`.

<a id='changelog-0.6.0.0'></a>
## 0.6.0.0 — 2023-10-26

### Breaking

- Replace all occurrences of `Ticked (LedgerView X)` with `LedgerView X`.
- Remove `Ticked (LedgerView X)` data family instances.
- Rename `translateTickedLedgerView` to `translateLedgerView`.

<a id='changelog-0.5.0.7'></a>
## 0.5.0.7 — 2023-09-27

### Patch

- Update upper bound on `ouroboros-consensus`

<a id='changelog-0.5.0.6'></a>
## 0.5.0.6 — 2023-08-21

### Patch

- Removed the `expose-sublibs` cabal flag, since Cabal/Nix handled it poorly.
- Instead, added a `unstable-` prefix to the name of each sublibrary, to
  strongly indicate that we ignore them when evolving the package's version.

<a id='changelog-0.5.0.5></a>
## 0.5.0.5 — 2023-08-18

### Non-Breaking

- Relax upper bound on `ouroboros-consensus`

<a id='changelog-0.5.0.4'></a>
## 0.5.0.4 — 2023-07-06

### Non-Breaking

- Relax upper bound on `ouroboros-consensus`

<a id='changelog-0.5.0.3'></a>
## 0.5.0.3 — 2023-06-23

### Patch

- Rename `StrictMVar` to `StrictSVar`

<a id='changelog-0.5.0.2'></a>
## 0.5.0.2 — 2023-05-19

### Patch

- Relax bounds on `ouroboros-consensus`

<a id='changelog-0.5.0.1'></a>
## 0.5.0.1 — 2023-04-28

### Patch

- Relax bounds on `ouroboros-consensus`

<a id='changelog-0.5.0.0'></a>
## 0.5.0.0 - 2023-04-24

### Breaking

- Apply new organization of Consensus packages.

## Before 0.4.0.0

Before this version, `ouroboros-consensus-protocol` lived in a bundle of
packages with `ouroboros-consensus`, thus the changelog was the same.

---

### Archaeological remark

Before following a more structured release process, we tracked most significant
changes affecting downstream users in the
[interface-CHANGELOG.md](https://github.com/IntersectMBO/ouroboros-consensus/blob/8d8329e4dd41404439b7cd30629fcce427679212/docs/website/docs/interface-CHANGELOG.md).
