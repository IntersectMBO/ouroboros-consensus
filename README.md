# [Ouroboros Consensus](https://input-output-hk.github.io/ouroboros-consensus/)

[![consensus](https://img.shields.io/badge/ouroboros--consensus-0.7.0.0-blue)](https://input-output-hk.github.io/cardano-haskell-packages/package/ouroboros-consensus-0.7.0.0/)
[![diffusion](https://img.shields.io/badge/ouroboros--consensus--diffusion-0.6.0.0-blue)](https://input-output-hk.github.io/cardano-haskell-packages/package/ouroboros-consensus-diffusion-0.6.0.0/)
[![protocol](https://img.shields.io/badge/ouroboros--consensus--protocol-0.5.0.2-blue)](https://input-output-hk.github.io/cardano-haskell-packages/package/ouroboros-consensus-protocol-0.5.0.2/)
[![cardano](https://img.shields.io/badge/ouroboros--consensus--cardano-0.6.0.0-blue)](https://input-output-hk.github.io/cardano-haskell-packages/package/ouroboros-consensus-cardano-0.6.0.0/)

[![docs](https://img.shields.io/badge/Documentation-yellow)](https://input-output-hk.github.io/ouroboros-consensus/)

Implementation of the [Ouroboros-family](docs/website/docs/References.md) of consensus
algorithms.

## Libraries and executables

This repository provides four packages, with the following dependencies among
them:

``` mermaid
flowchart TD
    D[ouroboros-consensus-diffusion] --> C
    A[ouroboros-consensus-cardano] --> B[ouroboros-consensus-protocol]
    A --> C
    B --> C[ouroboros-consensus]
```

The packages contain many test-suites that complicate the dependency graph as
they create new depencency arcs.

This repository also provides two executables:

- `ouroboros-consensus-cardano/app/db-analyser.hs`: for analyzing ChainDBs as
  the ones created by the node. This helps identifying performance hotspots and
  testing that the validating logic remains correct.

- `ouroboros-consensus-cardano/app/db-synthesizer.hs`: for quickly generating
  chains to be used in benchmarking.

To list all the available Cabal components, one can use the following script
because unfortunately, `cabal` doesn't have a command to list the [available
targets](https://github.com/haskell/cabal/issues/4070):

``` bash
for f in $(find ouroboros-consensus* -type f -name "*.cabal"); do
    printf "Components of package %s:\n" $f;
    cat $f | grep -E "^(library|test-suite|executable|benchmark)" \
      | sed 's/library /🤫📦 /g' \
      | sed 's/test-suite /🧪   /g' \
      | sed 's/benchmark /🏁   /g' \
      | sed 's/executable /⚙️    /g' \
      | sed "s/library/📦   $(echo $f | cut -d\/ -f2 | cut -d\. -f1)/g"
    printf "\n\n"
done
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
Package](https://input-output-hk.github.io/cardano-haskell-packages/all-packages/)
repository from where you should pull new releases.

To use CHaP, follow their Readme, but in short:

1. Add this at the top of your `cabal.project` file:

  ```
  repository cardano-haskell-packages
    url: https://input-output-hk.github.io/cardano-haskell-packages
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
   that it has to be higher or equal to the highest timestamp of the release
   versions of the packages that you want to use as dependencies:

   ```
   index-state:
    , hackage.haskell.org      2023-04-12T00:00:00Z
    , cardano-haskell-packages 2023-04-23T00:00:00Z
   ```

At this point, you should be able to declare our libraries as dependencies in
your `build-depends` list on your cabal files.

If you use Nix, see the [CHaP
website](https://input-output-hk.github.io/cardano-haskell-packages/) on how to
configure CHaP for haskell.nix.

## How to contribute to the project

Your help is greatly appreciated. Please see [our CONTRIBUTING
document](CONTRIBUTING.md).

## How to submit an issue

Issues can be filled in our [GitHub issue
tracker](https://github.com/input-output-hk/ouroboros-consensus/issues). Please
use the provided issue templates.

## Documentation

We have several sources of documentation:

- [Haddocks](https://input-output-hk.github.io/ouroboros-consensus/haddocks/):
  our code is full of haddock annotations and comments that try to clarify
  expected behaviors and subtleties. Reading through the code should provide
  most of the information on how is Consensus implemented.

- [Website](https://input-output-hk.github.io/ouroboros-consensus/): this website
  provides access to the markdown documentation to which step by step we want to
  move the bulk of the "higher level documentation" as well as achitectural
  documentation.

- [Report](./docs/report/): this in-depth technical report describes many of the
  deep choices made in the implementation of the Consensus layer, as well as
  non-trivial lemmas or properties of the Consensus algorithms that have
  perspired to the implementation. Although incomplete in some sections, it is a
  mandatory reading for anyone looking to understand why Consensus does what it
  does.

