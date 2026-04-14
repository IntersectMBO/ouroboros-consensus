# Ouroboros Consensus

[![consensus](https://img.shields.io/badge/ouroboros--consensus-3.0.1.0-blue)](https://chap.intersectmbo.org/package/ouroboros-consensus-3.0.1.0/) [![docs](https://img.shields.io/badge/Documentation-yellow)][webpage]

Implementation of the Ouroboros family of consensus algorithms.

## Libraries and executables

This repository provides six sublibraries, with the following dependencies among
them:

``` mermaid
flowchart TD
    A["ouroboros-consensus:cardano"] --> B["ouroboros-consensus:protocol"]
    A --> C["ouroboros-consensus:ouroboros-consensus"]
    B --> C
    D["ouroboros-consensus:diffusion"] --> C
    E["ouroboros-consensus:lsm"] --> C
    F["ouroboros-consensus:lmdb"] --> C
```

The package contains many test-suites that complicate the dependency graph as
they create new dependency arcs.

This repository also provides some executables:

| Name                 | Purpose                                                                                               |
|----------------------|-------------------------------------------------------------------------------------------------------|
| `db-analyser`        | Analyzing ChainDBs for testing and validation, or producing ledger snapshots                          |
| `db-synthesizer`     | Quickly generate new synthetic chains for benchmarking                                                |
| `db-truncater`       | Truncating an Immutable DB                                                                            |
| `db-immutaliser`     | Converting the volatile suffix of the chain into immutable, so that `db-analyser` can also analyse it |
| `immdb-server`       | Serves immutable blocks from a ChainDB in a Node-to-node connection                                   |
| `gen-header`         | Generate and validate Praos headers                                                                   |
| `snapshot-converter` | Converts snapshots among the UTxO-HD formats of the database (InMemory, LMDB, LSM)                    |

Some documentation on how to use the tools can be found in the
[`ouroboros-consensus:cardano` README](./ouroboros-consensus-cardano/README.md).

To list all the available Cabal components, one can use the following script
because unfortunately, `cabal` doesn't have a command to list the [available
targets](https://github.com/haskell/cabal/issues/4070):

``` bash
grep -E "^(library|test-suite|executable|benchmark)" ouroboros-consensus.cabal --color=never | column -t | sort | sed 's/^/\t/'
```

## Building the project

We use `cabal` to build our project, potentially inside a Nix shell (`nix
develop` or `nix-shell`). It should suffice with:

``` bash
> cabal build all
```

Specific executables can be executed through cabal once built:

``` bash
> cabal run db-analyser
```

## Testing the project

The project is tested with numerous Haskell test suites. To run every test
suite, one can use:

``` bash
> cabal test all
```

For running specific test-suites (such as `consensus-test`), we recommend one of
the following commands:

``` bash
> cabal run ouroboros-consensus:test:consensus-test -- <args>
> cabal test ouroboros-consensus:test:consensus-test --test-show-details=direct
```

Note the second one cannot be used when we want to provide CLI arguments to the
test-suite.

## Using Consensus as a dependency

We make releases to the [Cardano Haskell
Package](https://chap.intersectmbo.org/all-packages/)
repository from where you should pull new releases.

To use CHaP, follow their Readme, but in short:

1. Add this at the top of your `cabal.project` file:

  ```
  repository cardano-haskell-packages
    url: https://chap.intersectmbo.org
    secure: True
    root-keys:
      3e0cce471cf09815f930210f7827266fd09045445d65923e6d0238a6cd15126f
      443abb7fb497a134c343faf52f0b659bd7999bc06b7f63fa76dc99d631f9bea1
      a86a1f6ce86c449c46666bda44268677abf29b5b2d2eb5ec7af903ec2f117a82
      bcec67e8e99cabfa7764d75ad9b158d72bfacf70ca1d0ec8bc6b4406d1bf8413
      c00aae8461a256275598500ea0e187588c35a5d5d7454fb57eac18d9edb86a56
      d4a35cd3121aa00d18544bb0ac01c3e1691d618f462c46129271bccf39f7e8ee
  ```

2. Run `cabal update` to pull in the latest index.
3. Specify which version of the index you want for both Hackage and CHaP. Note
   that it has to be higher or equal to the highest timestamp of the released
   versions of the packages that you want to use as dependencies:

   ```
   index-state:
    , hackage.haskell.org      2023-04-12T00:00:00Z
    , cardano-haskell-packages 2023-04-23T00:00:00Z
   ```

At this point, you should be able to declare our libraries as dependencies in
your `build-depends` list on your cabal files.

If you use Nix, see the [CHaP
website](https://chap.intersectmbo.org/) on how to
configure CHaP for haskell.nix.

The Consensus sublibraries---which are used for our internal testing---are
visible and buildable by default only because the appropriate Cabal and/or Nix
infrastructure to guard them behind an explicit opt-in is currently immature
and fragile. **WARNING** breaking changes to the these libraries are **not**
reflected in the package versions. That is why they all have the `unstable-`
prefix in their name; use at your own risk, and please reach out to us if this
policy of ours is an excessive burden on your use case.

## How to contribute to the project

Your help is greatly appreciated. Please see [our CONTRIBUTING
document](CONTRIBUTING.md).

## How to submit an issue

Issues can be filled in our [GitHub issue
tracker](https://github.com/IntersectMBO/ouroboros-consensus/issues). Please
use the provided issue templates.

## Documentation

We have several sources of documentation:

- [Haddocks](https://ouroboros-consensus.cardano.intersectmbo.org/haddocks/):
  our code is full of haddock annotations and comments that try to clarify
  expected behaviors and subtleties. Reading through the code should provide
  most of the information on how is Consensus implemented.

- [Website](https://ouroboros-consensus.cardano.intersectmbo.org/): this
  website provides access to the markdown documentation to which step by step we
  want to move the bulk of the "higher level documentation" as well as
  achitectural documentation.

- [Report](./docs/tech-reports/report/): this in-depth technical report describes many of the
  deep choices made in the implementation of the Consensus layer, as well as
  non-trivial lemmas or properties of the Consensus algorithms that have
  perspired to the implementation. Although incomplete in some sections, it is a
  mandatory reading for anyone looking to understand why Consensus does what it
  does.
  ([rendered](https://ouroboros-consensus.cardano.intersectmbo.org/pdfs/report.pdf))

- [Formal specification](./docs/agda-spec/): this directory contains the formal (Agda)
  specification of the Consensus layer.

## Other packages in the IOG/IntersectMBO namespaces that this project benefits from

- [`sop-extras`](https://github.com/input-output-hk/sop-extras)
- [`strict-sop-core`](https://github.com/input-output-hk/sop-extras)
- [`rawlock`](https://github.com/IntersectMBO/io-classes-extra)
- [`resource-registry`](https://github.com/IntersectMBO/io-classes-extra)
- [`strict-checked-vars`](https://github.com/IntersectMBO/io-classes-extra)
- [`fs-api`](https://github.com/input-output-hk/fs-sim)
- [`fs-sim`](https://github.com/input-output-hk/fs-sim)
- [`io-classes`](https://github.com/input-output-hk/io-sim)
- [`io-sim`](https://github.com/input-output-hk/io-sim)
- [`cardano-lmdb`](https://github.com/input-output-hk/haskell-lmdb)
- [`cardano-lmdb-simple`](https://github.com/input-output-hk/lmdb-simple)
- [`diff-containers`](https://github.com/input-output-hk/anti-diffs)
- [`fingertree-rm`](https://github.com/input-output-hk/anti-diffs)
- [`blockio`](https://github.com/IntersectMBO/lsm-tree)
- [`lsm-tree`](https://github.com/IntersectMBO/lsm-tree)

[webpage]: https://ouroboros-consensus.cardano.intersectmbo.org/
