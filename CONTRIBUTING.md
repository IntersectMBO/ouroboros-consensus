Thank you for taking the time to contribute 🙌

The following is a set of guidelines for contributing to the Consensus component
of Cardano. If you have suggestions on how to improve this document, please feel
free to [propose changes](#contributing-to-the-code) to it in a pull request.
Bear in mind that the document should remain simple.

# Documentation

Documentation should be a first-class citizen of the Consensus code base. We are
in the process of improving the Consensus documentation, and your improvement
proposals are welcome.

We have two types of documentation:

- Markdown files, which can be found in the [docs](docs/website/docs) directory.
  They contain information that is not strictly related to the code itself, such
  as getting started guides, references, tutorials, etc.
- [Haddock][haddock-site] comments. They contain more low level information
  about the code.

When adding or improving documentation about the implementation, it is
preferable to add haddock comments since they are closer to the code. However
not all documentation can be placed inside haddock comments, and in such cases
the contributor can update the markdown files in [docs](docs/website/docs).

This repository also contains a [technical report](docs/report) that describes
the implementation of the Consensus layer. We will not update this report. We
keep it here as a historical reference, and we will systematically convert the
relevant parts of the report into the two types of documentation mentioned
above.

When somebody asks a question about the code, we should try to refer people to
the documentation. If no relevant entry exists, we should create it and submit a
pull request.

For the time being, all markdown files that contain the Consensus documentation
live in this repository.

# Setting up the build tools

## Using Nix

Consensus can be built using [Nix](https://nixos.org/download.html). The
installation and configuration instructions are taken from
[cardano-node](https://github.com/input-output-hk/cardano-node/blob/master/doc/getting-started/building-the-node-using-nix.md),
and detailed below. To install `nix` run:

```sh
curl -L https://nixos.org/nix/install > install-nix.sh
chmod +x install-nix.sh
./install-nix.sh
```

Then enable the following features:

```sh
sudo mkdir -p /etc/nix
cat <<EOF | sudo tee /etc/nix/nix.conf
experimental-features = nix-command flakes
allow-import-from-derivation = true
EOF
```

As an optional step, to improve build speed (highly recommended), you can set up a binary
cache maintained by IOG:

```sh
sudo mkdir -p /etc/nix
cat <<EOF | sudo tee -a /etc/nix/nix.conf
substituters = https://cache.nixos.org https://cache.iog.io
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
EOF
```

## Using `cabal`

An alternative to using `nix` is to set up the development
environment yourself. Follow [these
instructions](https://github.com/input-output-hk/cardano-node/blob/master/doc/getting-started/install.md)
to properly configure your system.

# Building the project

To build all the packages in this repository run:

```sh
cabal build all
```

in the command line, either inside `nix-shell` if you use `nix`, or in a
system with `cabal` installed.

Specific executables can be executed through cabal once built:

``` sh
cabal run db-analyser
```

# Testing

To test all the packages in this repository run:

```sh
cabal build all
```

in the command line, either inside `nix-shell` if you use `nix`, or in a
system with `cabal` installed.

For running specific test-suites (such as `consensus-test`), we recommend one of
the following commands:

```sh
cabal run ouroboros-consensus:test:consensus-test -- <args>
cabal test ouroboros-consensus:test:consensus-test --test-show-details=direct
```

Note the second one cannot be used when we want to provide CLI arguments to the
test-suite.

# Contributing to the code

The following sections contain some guidelines that should be followed when
contributing to the Consensus code base. Please take some time to go through
them. We do not expect newcomers to adhere to these guidelines perfectly, and we
will guide you through the process when reviewing your pull request.

## Following our git process

Our [git process](docs/website/docs/GitProcess.md) describes the `git` practices we
encourage when working with the code in this repository.

## Updating the documentation

When submitting a pull request, please look update the relevant parts of the
documentation (see [this section](#documentation)).

## Following the style guide

We have a [Haskell style guide](docs/website/docs/StyleGuide.md) that should be followed when
writing code in Consensus. Our style guide is not set in stone, and improvements
are always welcome.

## Formatting the code

We use `stylish-haskell` >= 0.14.4.0 for Haskell code formatting. There is a [CI
script](./scripts/ci/check-stylish.sh) that checks that the code is properly
formatted.

Either enable editor integration or call the script used by CI itself:

```bash
./scripts/ci/check-stylish.sh
```

When using Nix, you can use the following command, which will build and use
the right version of `stylish-haskell`.

```bash
nix-shell --run ./scripts/ci/check-stylish.sh
```

## Making and reviewing changes

If you are working on changing a **substantial** part of Consensus, it is
**important** that you contact the core developers first to discuss design
considerations (See section [Contacting the
developers](#contacting-the-developers)). This will help detecting any potential
problems with the change in the design phase, preventing misunderstandings and
frustrations later on in the process.

We maintain a changelog. If your pull request requires a changelog entry, please
follow [these
instructions](docs/website/docs/ReleaseProcess.md#adding-a-changelog-fragment).
Even if your change doesn't require a changelog fragment, create an empty one as
CI will reject your change otherwise. We made this choice to ensure authors of
PRs would always take a moment to consider whether a changelog fragment should
be added for their PR. For more information see [our release
process](docs/website/docs/ReleaseProcess.md).

When creating a pull-request (PR), it is **crucial** that the PR:

- has a clear description,

- targets the `main` branch (unless there is a good reason not to),

- is as small as possible, and

- is organized in a cohesive sequence of commits, each representing a
  meaningful, logical and reviewable step.

# Reporting a bug or requesting a feature

If you happen to encounter what you think is a bug or you wish there was some
feature that was added to Consensus, please
[open](https://github.com/input-output-hk/ouroboros-consensus/issues/new/choose) an
issue in our [issue
tracker](https://github.com/input-output-hk/ouroboros-consensus/issues/).

# Submitting pull requests

We monitorize the repository constantly so we should be aware if you open a
pull-request, however if some reasonable time passes and we miss your PR, feel
free to ping someone from the team.

# Contacting the developers

The core contributors to consensus codebase are:

-   [Nicolas Frisby](https://github.com/nfrisby)

-   [Javier Sagredo](https://github.com/Jasagredo)

-   [Alexander Esgen](https://github.com/amesgen)

-   [Joris Dral](https://github.com/jorisdral)

-   [Bart Frenk](https://github.com/bartfrenk)

-   [Arnaud Bailly](https://github.com/abailly-iohk)

-   [Fraser Murray](https://github.com/fraser-iohk)

-   [Damian Nadales](https://github.com/dnadales)

# Code of conduct

See [Cardano engineering
handbook](https://github.com/input-output-hk/cardano-engineering-handbook/blob/main/CODE-OF-CONDUCT.md)'s
code of conduct.

[haddock-site]: https://haskell-haddock.readthedocs.io/en/latest/
