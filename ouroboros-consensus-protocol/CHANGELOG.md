# Ouroboros-consensus-protocol Changelog

# Changelog entries

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
[interface-CHANGELOG.md](https://github.com/intersectmbo/ouroboros-consensus/blob/8d8329e4dd41404439b7cd30629fcce427679212/docs/website/docs/interface-CHANGELOG.md).
