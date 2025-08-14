Thank you for taking the time to contribute 🙌

The following is a set of guidelines for contributing to the Consensus component
of Cardano. If you have suggestions on how to improve this document, please feel
free to [propose changes](#contributing-to-the-code) to it in a pull request.
Bear in mind that the document should remain simple.
See [the quick reference section](#quick-reference) if you are in a hurry.

# Documentation

Documentation should be a first-class citizen of the Consensus code base. We are
in the process of improving the Consensus documentation, and your improvement
proposals are welcome.

We have two types of documentation:

- Markdown files, which can be found in the [docs](docs/website/contents)
  directory. They contain information that is not strictly related to the code
  itself, such as getting started guides, references, tutorials, etc.

  They are rendered at the [Consensus
  website](https://ouroboros-consensus.cardano.intersectmbo.org).
- [Haddock][haddock-site] comments. They contain more low level information
  about the code and should be used as a reference, although we aim to provide
  some context navigation (cross referencing modules) on the module
  descriptions.

When starting to work on Consensus, we recommend to take a look at the following
resources:

 - [Preflight guide](https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/PreflightGuide/)
 - [Glossary](https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/Glossary/)

When adding or improving documentation about the implementation, it is
preferable to add haddock comments since they are closer to the code. However
not all documentation can be placed inside haddock comments, and in such cases
the contributor can update the markdown files in [docs](docs/website/contents).

This repository also contains a [technical report](docs/tech-reports/report)
that describes the implementation of the Consensus layer. We will not update
this report. We keep it here as a historical reference, and we will
systematically convert the relevant parts of the report into the two types of
documentation mentioned above.

When somebody asks a question about the code, we should try to refer people to
the documentation. If no relevant entry exists, we should create it and submit a
pull request.

For the time being, all markdown files that contain the Consensus documentation
live in this repository.

# Setting up the build tools

## Using Nix

Consensus can be built using [Nix](https://nixos.org/download/). The
installation and configuration instructions are taken from
[cardano-node](https://github.com/input-output-hk/cardano-node-wiki/blob/main/docs/getting-started/building-the-node-using-nix.md),
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

As an optional step, to improve build speed (highly recommended), you can set up
a binary cache maintained by IOG. If you use flakes, you will be asked to accept
using it automatically; alternatively, you can add it globally:

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
instructions](https://developers.cardano.org/docs/get-started/cardano-node/installing-cardano-node/)
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
cabal test all
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

# Generating documentation and setting up hoogle

The documentation contains some [tikz](https://tikz.net) figures that require
some preprocessing for them to be displayed. To do this, use the documentation
script:

```bash
./scripts/docs/haddocks.sh
```

If not already in your `PATH` (eg when in a Nix shell), this will install
[`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec)
from a binary, and then build the haddocks for the project.

Often times, it is useful to have a
[`hoogle`](https://github.com/ndmitchell/hoogle) server at hand, with the
packages and its dependencies. Our suggestion is to install
[`cabal-hoogle`](https://github.com/kokobd/cabal-hoogle) from github:

```bash
git clone git@github.com:kokobd/cabal-hoogle
cd cabal-hoogle
cabal install exe:cabal-hoogle
```

and then run `cabal-hoogle`:

```bash
cabal-hoogle generate
cabal-hoogle run -- server --local
```

This will fire a `hoogle` server at https://localhost:8080/ with the local
packages and their dependencies.

# Contributing to the code

The following sections contain some guidelines that should be followed when
contributing to the Consensus code base. Please take some time to go through
them. We do not expect newcomers to adhere to these guidelines perfectly, and we
will guide you through the process when reviewing your pull request.

## Quick reference

This section contain guidelines on what to check when making a pull request.

- When bumping version bounds on the dependencies *it is not necessary* to
  increase the package version number. See [this
  section](#updating-the-dependencies-bounds).
- When you want to create a changelog entry, follow [this and the following
  section](docs/website/contents/for-developers/ReleaseProcess.md#installing-scriv).

## Following our git process

Our [git process](docs/website/contents/for-developers/GitProcess.md) describes
the `git` practices we encourage when working with the code in this repository.

## Updating the documentation

When submitting a pull request, please look update the relevant parts of the
documentation (see [this section](#documentation)).

## Following the style guide

We have a [Haskell style
guide](docs/website/contents/for-developers/StyleGuide.md) that should be
followed when writing code in Consensus. Our style guide is not set in stone,
and improvements are always welcome.

## Formatting the code

We use `fourmolu` for Haskell code formatting. See [tools.nix](./nix/tools.nix) for the `fourmolu` version used in CI.

Either enable editor integration or call the script used by CI itself:

```bash
./scripts/ci/run-fourmolu.sh
```

When using Nix, you can use the following command, which will build and use
the right version of `fourmolu`.

```bash
nix develop -c ./scripts/ci/run-fourmolu.sh
```

# Generating documentation and setting up hoogle

To generate the documentation, use the documentation script:

```bash
./scripts/docs/haddocks.sh
```

Often times, it is useful to have a
[`hoogle`](https://github.com/ndmitchell/hoogle) server at hand, with the
packages and its dependencies. Our suggestion is to use
[`cabal-hoogle`](https://github.com/kokobd/cabal-hoogle) which is included in
the `nix` shell:

```bash
nix develop
$ cabal-hoogle generate
$ cabal-hoogle run -- server --local
```

This will fire a `hoogle` server at https://localhost:8080/ with the local
packages and their dependencies.

## Making and reviewing changes

If you are working on changing a **substantial** part of Consensus, it is
**important** that you contact the core developers first to discuss design
considerations (See section [Contacting the
developers](#contacting-the-developers)). This will help detecting any potential
problems with the change in the design phase, preventing misunderstandings and
frustrations later on in the process.

We maintain a changelog. If your pull request requires a changelog entry, please
follow [these
instructions](docs/website/contents/for-developers/ReleaseProcess.md#adding-a-changelog-fragment).
Even if your change doesn't require a changelog fragment, create an empty one as
CI will reject your change otherwise. We made this choice to ensure authors of
PRs would always take a moment to consider whether a changelog fragment should
be added for their PR. For more information see [our release
process](docs/website/contents/for-developers/ReleaseProcess.md).

When creating a pull-request (PR), it is **crucial** that the PR:

- has a clear description,

- targets the `main` branch (unless there is a good reason not to),

- is as small as possible, and

- is organized in a cohesive sequence of commits, each representing a
  meaningful, logical and reviewable step.

## Updating dependencies (`index-state`)

Our Haskell packages come from two package repositories:
- Hackage
- [CHaP][chap] (which is essentially another Hackage)

The `index-state` of each repository is pinned to a particular time in
`cabal.project`. This tells Cabal to treat the repository as if it was the
specified time, ensuring reproducibility. If you want to use a package version
from repository X which was added after the pinned index state time, you need to
bump the index state for X. This is not a big deal, since all it does is change
what packages `cabal` considers to be available when doing solving, but it will
change what package versions cabal picks for the plan, and so will likely result
in significant recompilation, and potentially some breakage. That typically just
means that we need to fix the breakage (increasing the lower-bound on the
problematic package if fix is not backward compatible), or delay that work and
instead decrease the upper-bound on the problematic package for now.

Note that `cabal`'s own persistent state includes which index states it is
aware of, so when you bump the pinned index state you may need to
call `cabal update` in order for `cabal` to be happy.

The Nix code which builds our packages also needs some information relating to
the index-state. This information needs to be new enough to include the
index-state specified in `cabal.project`. The information is represented by Nix
flake inputs. You can update these by running:
- `nix flake update hackageNix` for Hackage
- `nix flake update CHaP` for CHaP

If you fail to do this you may get an error like this from Nix:
```
error: Unknown index-state 2021-08-08T00:00:00Z, the latest index-state I know about is 2021-08-06T00:00:00Z. You may need to update to a newer hackage.nix.
```

The `index-state` of the tools is pinned in `./nix/tools.nix` to ensure that an
incompatible change in the set of packages available in Hackage doesn't break
the shell. From time to time, this `index-state` should be updated manually.

## Updating the dependencies bounds

Sometimes, when creating pull requests to [CHaP][chap], it is desirable to
loose/tighten certain dependencies bounds via a revision.

 - If you do so for a Consensus package, please first open a PR to Consensus
   mirroring the change in CHaP; but do not increment the version number of the
   Consensus package.

 - If your revision is about allowing a new version of package, please update
   the version bound in a way that keeps the previously allowed versions, unless
   this is undesirable due to eg a bug in an earlier version.

   For example, if we have `cardano-ledger-core ^>= 1.1` and you want to also
   allow `1.2`, use `^>= 1.1 || ^>= 1.2`.

### Use of `source-repository-package`s

We *can* use Cabal's `source-repository-package` mechanism to pull in
un-released package versions. This can be useful when debugging/developing
across different repositories. However, we should not release our packages
to CHaP while we depend on a `source-repository-package` since downstream
consumers would not be able to build such package.

If we are stuck in a situation where we need a long-running fork of a
package, we should release it to CHaP instead (see the
[CHaP README](https://github.com/IntersectMBO/cardano-haskell-packages)
for more).

In general, we strive to avoid having `source-repository-package`s on our `main` branch. However, there are situations where we want to prevent pull requests from piling up while awaiting the release of upstream components[^1]. 
In these cases, we allow merging pull requests that contain `source-repository-package`s, provided the referenced commit is on the `main` branch of the upstream package.

If you do add a temporary `source-repository-package` stanza, you need to
provide a `--sha256` comment in `cabal.project` so that Nix knows the hash
of the content. There are two relatively straightforward ways to do this:

1. The TOFU approach: put in the wrong hash and then Nix will tell you the
   correct one, which you can copy in.
2. Calculate the hash with `nix-shell -p nix-prefetch-git --run
   'nix-prefetch-git <URL> <COMMIT_HASH>'`

## Inspecting dependencies as used by Nix

When debugging an issue or when not using the shipped Nix shell, it is often
desirable to know the exact version/revision of a specific tool or library as
used by Nix.

To get the exact `libsodium-vrf` used by Nix:
```console
 $ nix eval --json .#libsodium-vrf.src.rev
"dbb48cce5429cb6585c9034f002568964f1ce567"
```

To get the `cabal-gild` version used by Nix:
```console
 $ nix eval --json .#cabal-gild.version
"1.3.0.1"
```

In more complex cases, you can start a Nix REPL and go on to explore
interactively:

```console
 $ nix repl
nix-repl> :lf .
nix-repl> pkgs = legacyPackages.${__currentSystem}
nix-repl> pkgs.hsPkgs.ouroboros-consensus-cardano.components.exes.db-
pkgs.hsPkgs.ouroboros-consensus-cardano.components.exes.db-analyser     pkgs.hsPkgs.ouroboros-consensus-cardano.components.exes.db-synthesizer
```

# Reporting a bug or requesting a feature

If you happen to encounter what you think is a bug or you wish there was some
feature that was added to Consensus, please
[open](https://github.com/IntersectMBO/ouroboros-consensus/issues/new/choose) an
issue in our [issue
tracker](https://github.com/IntersectMBO/ouroboros-consensus/issues/).

# Submitting pull requests

We monitorize the repository constantly so we should be aware if you open a
pull-request, however if some reasonable time passes and we miss your PR, feel
free to ping someone from the team.

# Contacting the developers

The core contributors to consensus codebase are:

-   [Nicolas Frisby](https://github.com/nfrisby)

-   [Javier Sagredo](https://github.com/jasagredo)

-   [Alexander Esgen](https://github.com/amesgen)

-   [Joris Dral](https://github.com/jorisdral)

-   [Fraser Murray](https://github.com/fraser-iohk)

-   [Damian Nadales](https://github.com/dnadales)

-   [Georgy Lukyanov](https://github.com/geo2a)

# Code of conduct

See [Cardano engineering
handbook](https://github.com/input-output-hk/cardano-engineering-handbook/blob/main/CODE-OF-CONDUCT.md)'s
code of conduct.

[haddock-site]: https://haskell-haddock.readthedocs.io/latest/
[chap]: https://github.com/IntersectMBO/cardano-haskell-packages

[^1]: [#1376](https://github.com/IntersectMBO/ouroboros-consensus/pull/1376) provides an example of an integration pull request that incorporated changes from several others that were waiting on the release of upstream packages. The resulting pull request was extremely tedious to review, making the process more error-prone than reviewing a shorter one due to its sheer size.
