# How Consensus Makes Releases

This document explains how the Consensus team uses Pull Requests (PRs), version numbers, and branches to prepare releases of our packages.

Let us assume our repository contains two packages FOO and BAR.
Two is enough to explain the complexities.
The resulting rules can be generalized to any number of packages, as explained at the end of the document.

*Remark*.
If you disagree with the decisions in this document, please consider reviewing our brainstorming notes and discussions, available at [this document's PR](https://github.com/IntersectMBO/ouroboros-network/pull/4207).
Even if you choose not to do so, please share your concern with us.

## Notation

In this document, we will assume each version number is a MAJOR.MINOR.PATCH triple.
This is easy to generalize to other conventional notations, such as [the PVP](https://pvp.haskell.org/)'s MAJOR.MAJOR.MINOR.PATCH quadruples.

We refer throughout this document to _the main branch_ (aka `master`, aka `main`).

If you search for "RULE:" in this document, you'll find the key statements.

## Rules for Branches

The Consensus team will sometimes be maintaining multiple MAJOR.MINOR versions of the same package simultaneously.
EG If we were currently maintaining a 2.3 version and a 2.4 version simultaneously, then we might release FOO-2.4.1 on Monday, release FOO-2.3.7 on Wednesday, and release FOO-2.4.2 on Friday, etc.

- Whenever we're maintaining only the one greatest-ever MAJOR.MINOR version of a package, then all work on that package will happen on the main branch.
- Whenever we're also maintaining some lesser MAJOR.MINOR versions of a package, then some work for that package will also be happening on the respective _release branches_.

A _release branch_ is a branch dedicated to the maintenance of some older release of one or multiple packages.

RULE: A release branch should branch off the main branch just at a released commit.
Usually, our backports are targeted at a particular Cardano Node version; we use the naming pattern `cardano-node-X.Y-backports` in that case.
For example, suppose we released FOO-3.0.0 and BAR-4.0.0 for Node X.Y.0, and we want to release new (potentially major) versions of FOO and BAR for Node X.Y.1, but cannot do that from the main branch as it already contains changes not suited for Node 3.4.x.
Then, we must create the release branch `cardano-node-X.Y-backports` just at the release commit(s) of FOO-3.0.0 and BAR-4.0.0.
On this branch, we can then release new (even major, see the EXCEPTION in the "Rules for releases" below) versions of FOO and BAR.
See the "Release Branch Example" below for a more thorough example.

There are two kinds of work on release branches.
Most of the time, that work is immitating some work that was done on the main branch.
EG we fixed a bug on the main branch and we're backporting that fix onto a release branch (hopefully it's a trivial cherry-pick).
Rarely, though, there will be fresh work done on a release branch.
EG Consider the following possible timeline.

- We release FOO-2.4.1 on Monday, and FOO-2.4.1 completely reworked some functionality.
- We then realize on Tuesday that there was a severe bug in the old functionality.
- We fix the bug by merging a PR that targets the release branch containing FOO-2.3.x (since the bug no longer exists on the main branch!)
- We release FOO-2.3.7 on Wednesday.

*Remark*.
Not every first-parent commit in the history of the release branch was announced as a release of a package.
We will be merging multiple PRs into the release branch in order to prepare the next release from it, so some commits will just be the intermediates between two releases.

## Rules for PRs

We classify each PR as either a _fresh_ PR or as a _backport_ PR.

- A fresh PR is doing work that has never been done before (new feature, new bugfix, etc).
  Except in the rare circumstances discussed in the previous section, every fresh PR will target the main branch.
- The primary objective of a backport PR is merely to immitate some fresh PR.

The rules are then as follows.

- RULE: A fresh PR MUST target the main branch, if possible.
- RULE: A PR MUST NOT be merged into a branch unless it should be included in the next release from that branch.
- We maintain a changelog for each package (one for FOO and one for BAR).
- RULE: A PR MUST add a pending changelog entry for each package that it alters.
- RULE: Each pending changelog entry MUST at the very least classify the alteration to that package as a _patch_ level change (eg a bugfix), a _minor_ change (eg a non-breaking interface change), or a _major_ change (eg a breaking interface change).

*Remark*.
Notice that we do not allow merging a PR that should not be included the subsequent release.
This means some PR may be "done" but still should not be merged.
When work has been done "prematurely" in this way, it risks requiring duplication of some future work (eg introduces merge conflicts with other simultaneously active PRs).
Our choice here doesn't create any more duplication, it merely confines that duplication to having to rebase that premature PR.
Our choice also preserves the intuitive monotonic relationship between releases and the main and releases branches.

We also maintain our changelog in a specific way.

- RULE: We maintain pending changelog entries in the same way that [`scriv`](https://github.com/nedbat/scriv) does, so that they do not generate conflicts and are not lost when rebasing/cherry-picking/etc PRs.
- RULE: The pending entries MUST NOT assume any specific version number of the previous release or the next release, because the entry may be backported etc.

*Remark*.
Backporting a PR wouldn't generate any conflicts in the changelog entries (by `scriv`'s design), so the author won't necessarily be prompted to update explicit version numbers.
Hence we forbid them, via the review process.
To explicitly clarify: it's fine to refer to historical explicit version numbers, but not in such a way that assumes anything about how many or which versions are between that explicit version and the changelog entry that mentions it.

## Rules for Releases

Infinitely often, the Consensus team will decide that FOO and/or BAR are ready for a next release (either from the main branch or from a release branch).
IE We will eventually decide that the code of that package on the tip commit of a branch should be the code of our next release of that package from that branch.
We prepare that release as follows.

- For each package we are including in the release, we review its pending changelog entries (eg by issuing `scriv collect --version DUMMY` to automatically collate the pending changelog entries).
- RULE: We update the declared version (ie in its `.cabal` file) of each package we are including in the release based on the content of those pending changelog entries.
    - (Note that there's at least one package, since otherwise we wouldn't be mkaing a release.)
    - (Note that there must be some alterations to each package we are including in the release, since otherwise we wouldn't be including it in the release---but mind the _bundles_ mentioned below.)
    - Let X.Y.Z be the version of the package currently declared on this branch.
    - RULE: If any alteration was major-level, then we bump to (X+1).0.0.
    - RULE: Else if any alteration was minor-level, then we bump to X.(Y+1).0.
    - RULE: Otherwise all alterations were patch-level, so we bump to X.Y.(Z+1).
    - EXCEPTION: If we released the package from a release branch with version larger than X.Y.Z, then we should always increase the major version X, potentially by more than one if the other release was a major one.
      This way, we avoid multiple versions with the same major component being released from different branches.
      This process is inelegant, but it seems acceptable for now as backport branches are relatively rare and only active for a limited amount of time.
- RULE: We merge one final PR, which MUST do exactly the following and nothing more.
    - RULE: It updates the versions of the packages being released as described above.
    - RULE: It flushes the pending changelog entries of each package being released into the `CHANGELOG.md` file for that package.
- RULE: We tag that resulting merge commit as release-PKG-A.B.C; one tag per package PKG that is being released (ie FOO and/or BAR).
- RULE: Finally, we announce this commit hash as the new release of these packages.
  EG We insert these package's new versions into Hackage, [CHaP][chap], etc.

*Remark*.
To explicitly clarify: after a release of a package, there will be zero pending changelog entries for that package.
When making the release, those entries were first incorporated into the `CHANGELOG.md` file and then the individual files containing those pending entires (see `scriv`'s mechanism) were removed (a la `git rm`).
But there may be pending changelog entries of other packages, those that were not included in this release.

*Remark*.
This scheme allows for multiple commits to declare their package has the same version number as some released version, even if those commits have made alterations to the package.
This means the mapping from commit hash <-> released version number is unfortunately not one-to-one.
There is obviously some possibility for confusion there.
However, we think the probability is low: if users only retrieve our code from the package repositories that we insert our release into, then the mapping will be one-to-one (as enforced by those repositories).
Despite it being relatively easy to preclude this risk (add an extra .0 dimension to the just-released versions immediately after announcing them, remove it when preparing the next release, but immediately add it back, etc -- thus between-release commits are always versions MAJOR.MINOR.PATCH.0), we're choosing not to.
The mitigation is quite simple, but that extra version dimension is unfamiliar to the broader community and so is likely to cause confusion of its own.
We are, though, ready to try this mitigation if the lack of one-to-one mapping does end up causing enough confusion.

## Generalizing to more packages

In actuality, the Consensus team maintains N-many packages instead of just FOO and BAR.
We could apply the above rules exactly as is: one changelog per package, independent version numbers per package, one release branch per MAJOR.MINOR.0 release of each package, etc.
We see some downsides to that.

- That's a lot of bookkeeping; many PRs would have to create multiple changelog entries.
- There's no easy way to recall which versions of our various packages are compatible with one another.
- There are multiple changelogs that it might not be easy for downstream users to navigate (ie to know which one to check for the information they're wondering about).

We therefore partition our N-many packages into B-many _bundles_ where B < N.

- RULE: The packages within a bundle always share a single changelog and always declare the exact same version.
- RULE: So we apply the rules from the above sections to each package bundle instead of to each package independently.

The only refinement to the wording in the above sections is that we update the `.cabal` file's of each package in a bundle being released based on the bundle's changelog entries, _even if that package had no alterations since the previous release of that bundle from this branch_.
This means some packages will occasionally get vacuous versions bumps.
That's obnoxious to downstream users and would be easy to interpret as evidence the Consensus team has made a mistake in during that release.
However, we either mitigate this by choosing our bundle partitioning to make it unlikely (put coupled packages into the same bundle) or else accept the oddity of the vacuous version bumps as an acceptable cost for the bundling's other advantages.

## Some Concrete Examples

Some of the rules above are much easier to internalize via concrete examples.

### Release Branch Example

Consider just one package, FOO.
Suppose we have released the following versions: 1.0.0 and 2.0.0.
The commit history might look like the following linear chain, with the tags and branch pointers annotated.

```
D - the release of FOO-2.0.0 (tag: release-FOO-2.0.0, branch: main)
C - replace that confusing feature with much nicer one
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

#### Releasing a new patch version on the main branch

If we subsequently merge a PR that fixes a minor bug present in 2.0.0, it'd then look like this.

```
E - fix that off-by-one error (branch: main)
D - the release of FOO-2.0.0 (tag: release-FOO-2.0.0)
C - replace that confusing feature with much nicer one
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

Once we release version 2.0.1, we'd have the following.

```
F - the release of FOO-2.0.1 (tag: release-FOO-2.0.1, branch: main)
E - fix that off-by-one error
D - the release of FOO-2.0.0 (tag: release-FOO-2.0.0)
C - replace that confusing feature with much nicer one
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

#### Releasing a fix exclusive to a previous release

Suppose we then find a bad bug in feature that commit C had replaced.
We might want to fix it because our users aren't yet ready to integrate the new feature from 2.0 version into their code.
Thus, we'd create a release branch, say `release-FOO-1.x.x`, and merge a bugfix PR directly into it.
Note that we should merge bugfixes into the main branch and then backport them onto a release branch when that is possible, but in this case the buggy feature no longer exists on the main branch.

```
G - fix confusion in the old logic (branch: release-FOO-1.x.x)
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

Once that patch passes validation, we could then release 1.0.1.

```
H - the release of FOO-1.0.1 (tag: release-FOO-1.0.1, branch: release-FOO-1.x.x)
G - fix confusion in the old logic
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

#### Backporting a fix

Now suppose we belatedly realized we can easily backport E onto 1.x.x as well.
If we're making another release of the 1.0 version, our users would likely appreciate us including as many bugfixes as we can.

```
I - cherry-pick of E (branch: release-FOO-1.x.x)
H - the release of FOO-1.0.1 (tag: release-FOO-1.0.1)
G - fix confusion in the old logic
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

And shortly the following.

```
J - the release of FOO-1.0.2 (tag: release-FOO-1.0.2, branch: release-FOO-1.x.x)
I - cherry-pick of E
H - the release of FOO-1.0.1 (tag: release-FOO-1.0.1)
G - fix confusion in the old logic
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

#### Backporting an unreleased minor fix

Return to the original example, and suppose that we didn't yet release 2.0.0:

```
C - replace that confusing feature with much nicer one (branch: main)
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

Suppose that we already now notice an off-by-one error, which we promptly fix:

```
K - fix that off-by-one error (branch: main)
C - replace that confusing feature with much nicer one
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

We also want to release FOO-1.0.1 with the fix (but without C), which we do by creating a corresponding release branch and cherry-picking K:

```
M - the release of FOO-1.0.1 (tag: release-FOO-1.0.1, branch: release-FOO-1.x.x)
L - cherry-pick of K
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

Note that when now releasing from the main branch later, we have to be careful to not *also* release FOO-1.0.1 there (in case C would allow for that).
Rather, following the rules above, we will have to release FOO-2.0.0.

```
N - the release of FOO-2.0.0 (tag: release-FOO-2.0.0, branch: main)
K - fix that off-by-one error
C - replace that confusing feature with much nicer one
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

#### Backporting an unreleased major fix

Consider a variant of the previous case where instead of K, the fix actually requires a major version bump:

```
O - fix API soundness issue (branch: main)
C - replace that confusing feature with much nicer one
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

Again, we want to release a new version of FOO with O, but without C. As O requires a major version bump, we have to release FOO-2.0.0:

```
Q - the release of FOO-2.0.0 (tag: release-FOO-2.0.0, branch: release-FOO-2.0.0)
P - cherry-pick of O
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

When we now want to release from the main branch later, we now have to release FOO-3.0.0, even though release-FOO-2.0.0 is not an ancestor of `main`:

```
R - the release of FOO-3.0.0 (tag: release-FOO-3.0.0, branch: main)
O - fix API soundness issue
C - replace that confusing feature with much nicer one
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

## Installing `scriv`

To manage the workflow described above, we will use the `scriv` tool slightly
modified to support cabal files. If you use `nix` then you will find `scriv` in
the Nix shell. Otherwise, the way to install it from source is:

```
pip install scriv
```

If you encounter an error mentioning:
`pkg_resources.extern.packaging.version.InvalidVersion: Invalid version: ...` we
found that downgrading `setuptools` to a version `< 66` seems to solve this
problem.

If you don't want to use virtual environments for python packages and
installation complains with the error `error: externally-managed-environment`,
pass in the flag `--break-system-packages`:

```
pip install --break-system-packages scriv
```

## Adding a changelog fragment

When a given branch contains a change that deserves a changelog entry, you
should add a changelog
[fragment](https://scriv.readthedocs.io/en/stable/concepts.html#fragments). When
we [cut a release](#cutting-a-release) the changelog fragments will be merged
into a changelog update. To add a changelog fragment, use `scriv`. See [this
section](#installing-scriv) for instructions on how to install it.

To create a changelog fragment you can follow these steps:

1. Run `scriv create` on the directory of one of the packages in the bundle you
   want to create an entry for (the symlinks described above will make sure that
   the fragment gets created in the right directory). This will need to be
   repeated twice if you want to create changelog entries for both bundles. This will
   create a new file inside the `changelog.d` sub-directory of the bundle.
2. Edit the newly created file(s) by uncommenting and filling in the appropriate
   section (Breaking, etc). Write the entry in the imperative, as per [these
   guidelines][git-contributing-to-a-project].
3. Add the file(s) to the branch.

## Cutting a release

First, make sure `scriv` is [installed](#installing-scriv), along with the
following Python3 packages (unless you use `scriv` in a `nix` shell):

- `bs4`
- `html5lib`

If they these packages are not installed, then run:

```sh
pip install bs4 html5lib
```

To cut a release we rely on a script. Simply run:

```sh
./scripts/release/create-release.hs
```

After the script is run, check the diff for coherence and make sure that the
packages build locally.

As a sanity check, run:

```sh
./scripts/release/cabal-plan-diff
```

Now open a pull request and ask for review. It is
recommended to already open a draft PR to [CHaP][chap] and make sure that CI
passes to avoid later surprises (eg due to missing bounds). Once the Consensus
PR is approved, add it to the merge queue, and once it hits `main`, create the
release tags as follows:

```sh
git checkout <rev-merge>
git pull
./scripts/release/tag-release.sh
```

Where `<rev-merge>` is the respective merge commit on `master`.

Finally, create a release in [CHaP][chap], for which one can invoke the
following script:

```sh
git checkout <rev-merge>
git pull
./scripts/release/release-to-chap.sh
```

[contributing-to-a-project]: https://git-scm.com/book/en/v2/Distributed-Git-Contributing-to-a-Project#Commit-Guidelines
[chap]: https://github.com/IntersectMBO/cardano-haskell-packages

## The first time we had to break this release process

On the first half of April 2023, we were asked to get a new Consensus release
from current `master` in order for it to be integrated into the node. However
the version on `master` could not be released because it was dependent on some
changes from network that were not yet released.

In order to make this clear, I am adding a `+` at the end of the version number
to indicate that it contains unreleased changes on top of the version mentioned
(which would be in CHaP). Note that in the cabal version, the `+` is not present
so for cabal `0.3.1.0+` and `0.3.1.0` are both `0.3.1.0`.

The release engineer that was integrating the node at that time, had pinned a
specific commit in a `source-repository-packages` clause in his integration
branch but by luck, several circumstances took place:
- the commit was in `0.3.1.0+`
- only `ouroboros-consensus-diffusion` depended on the network changes
- `ouroboros-consensus` with his changes was, as mentioned above, at `0.3.1.0+`
- `ouroboros-consensus-diffusion 0.3.1.0+` would not have been buildable at the
  current CHap versions
- but `ouroboros-consensus-diffusion 0.3.1.0` (the one from CHaP) declared a
  dependency on `ouroboros-consensus 0.3.1.0`
- and by chance, the changes in `ouroboros-consensus 0.3.1.0+` (which actually
  were "Breaking changes") didn't break the compilation of
  `ouroboros-consensus-diffusion 0.3.1.0`
- so he could pull `ouroboros-consensus 0.3.1.0+` from the
  `source-repository-packages` and `ouroboros-consensus-diffusion 0.3.1.0` from
  CHaP

So in that situation we were breaking the bundle in a way that our previous
rules couldn't handle.

> Problem 1: We were unable to make a release from `master`, so we had to make
> the release from some previous commit as a side branch.

> Problem 2: Our bundles were too rigid for the development that happens.

> Problem 2.1: Sharing a repository with Network means some changes from network
> will transpire to our packages, and network might not want to make a release
> just yet, but we might need a release from consensus.

> Problem 2.2: The strict inter-dependency of `consensus` and
> `consensus-diffusion` is actually wrong, as `consensus-diffusion` is what puts
> together `consensus` and `network` thus it might not be releasable always.

> Problem 3: our bundles are rigid but one can import only specific packages
> therefore not following the bundles schema.

> Problem 4: The version number of a released package and a development version
> is the same in the eyes of Cabal. Therefore the development `consensus` could
> co-exist with the released `consensus-diffusion`.

This case was just a series of coincidences that happened at the same time but
is a clear example of why our previous choices were not correct/good enough.

We [solved][solution] the situation by breaking the bundles momentarily and releasing only
the needed packages at the needed commit, therefore messing up the Changelogs,
and publishing a release for `ouroboros-consensus-diffusion` to allow for a
`0.4.0.0` `ouroboros-consensus` package. But we used this opportunity to propose
a new organization, and also migrate to a new repository.

[solution]: https://github.com/IntersectMBO/cardano-haskell-packages/pull/207

## The new organization

The consensus packages are now split as follows:

- `ouroboros-consensus`: contains the whole implementation of consensus, with an
  abstract `blk` type variable (and `BlockProtocol blk` type family) all over
  the place.
- `ouroboros-consensus-diffusion`: glues together `ouroboros-consensus` and
  `ouroboros-network` code.
- `ouroboros-consensus-protocol`: defines the `Praos` and `TPraos`
  instantiations of the protocols.
- `ouroboros-consensus-cardano`: contains the instantiations of `blk` for the
  Cardano case, which also entails all the typeclass instances required to
  support `ouroboros-consensus` and the association with the protocols in
  `ouroboros-consensus-protocol`. In particular it contains 3 subdirectories
  with the `byron`, `shelley` and `cardano` (`HardForkBlock`) instantitations.

  It also contains the code for the `cardano-tools` like `db-analyzer` and
  `db-synthesizer`.

``` mermaid
flowchart TD
    D[ouroboros-consensus-diffusion] --> C
    A[ouroboros-consensus-cardano] --> B[ouroboros-consensus-protocol]
    A --> C
    B --> C[ouroboros-consensus]
```
