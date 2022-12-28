Thank you for taking the time to contribute 🙌

The following is a set of guidelines for contributing to the Consensus component
of Cardano. If you have suggestions on how to improve this document, please feel
free to [propose changes](#_contributing_to_the_code) to it in a pull request.
Bear in mind that the document should remain simple.

# Resources

To read about the theoretical foundations of consensus, as well as other
architectural considerations, please see TODO!.

TODO: provide links to the consensus report (it’s momentary broken due to Hydra
being replaced by Cicero).

We are in the process of improving the consensus documentation, and your
improvement proposals are welcome.

# Setting up the build tools

TODO: here we should mention who to contact if the user has problem installing
the tools.

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

As an optional step, to improve build speed, you can set up a binary
cache maintained by IOG:

```sh
sudo mkdir -p /etc/nix
cat <<EOF | sudo tee -a /etc/nix/nix.conf
substituters = https://cache.nixos.org https://cache.iog.io
trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
EOF
```

## Using `cabal`

A more involved alternative to using `nix` is to set up the developmen
environment yourself. Follow [these
instructions](https://github.com/input-output-hk/cardano-node/blob/master/doc/getting-started/install.md/)
to properly configure your system.

# Building the project

To build all the packages in this repository run:

```sh
cabal build all
```

in the command line, either inside `nix-shell` if you use `nix`, or in a
system with `cabal` installed.

# Setting up your developer environment

TODO: we do not support a single editor, however can we give some
generic instructions here? Alternatively we could propose **a** way to
setup each of the editors we use (Emacs, VSCode, Vim), just to give the
reader something to hold on to if they’re clueless. Otherwise we could
skip this version.

# Testing

To build all the packages in this repository run:

```sh
cabal build all
```

in the command line, either inside `nix-shell` if you use `nix`, or in a
system with `cabal` installed.

# Contributing to the code

The following sections contain some guidelines that should be followed when
contributing to the Consensus code base. Please take some time to go through
them. We do not expect newcomers to adhere to these guidelines perfectly, and we
will guide you through the process when reviewing your pull request.

## Following our git process

Our [git process](docs/GitProcess.xml) describes the `git` practices we
encourage when working with the code in this repository.

## Following the style guide

We have a [Haskell style guide](STYLEGUIDE.xml) that should be followed
when writing code in Consensus. Our style guide is not set in stone, and
improvements are always welcome.

## Formatting the code

We use `stylish-haskell` for Haskell code formatting. There is a CI
script (TODO: link to it) that checks that the code is properly
formatted.

TODO: describe how to fix `stylish-haskell` with a script and optionally
setting up editors to run `stylish-haskell` on save.

## Making and reviewing changes

If you are working on changing a substantial part of Consensus, it is
**important** that you contact the core developers first to discuss design
considerations (See section [Contacting the
developers](#contacting-the-developers)). This will help detecting any potential
problems with the change in the design phase, preventing misunderstandings and
frustrations later on in the process.

We maintain a [changelog](docs/interface-CHANGELOG.md). See this file for
instructions on how to maintain it and process it when looking for information
about changes. Not all pull request need changelog entries. It is up to the
judgment of the contributor to determine this. The heuristic here is to update
the changelog if a downstream consumer would find it useful.

When creating a pull-request (PR), it is **crucial** that the PR:

- has a clear description,

- targets the `master` branch (unless there is a good reason not to),

- is as small as possible, and

- is organized in a cohesive sequence of commits, each representing a
  meaningful, logical and reviewable step.

# Reporting an issue

TODO: how to submit a bug?

TODO: how to submit a feature request?

# Submitting pull requests

# Contacting the developers

The core contributors to consensus code base are:

-   [Nicolas Frisby](https://github.com/nfrisby)

-   [Javier Sagredo](https://github.com/Jasagredo)

-   [Alexander Esgen](https://github.com/amesgen)

-   [Yogesh Sajanikar](https://github.com/yogeshsajanikar)

-   [Joris Dral](https://github.com/jorisdral)

-   [Bart Frenk](https://github.com/bartfrenk)

-   [Damian Nadales](https://github.com/dnadales)

# Code of conduct

See [Cardano engineering
handbook](https://github.com/input-output-hk/cardano-engineering-handbook/blob/main/CODE-OF-CONDUCT.md)'s
code of conduct.
