#!/bin/bash

echo "=== cabal-plan-diff.sh ===

In our CI and in most development setups, we build our projects with
--enable-tests and --enable-benchmarks, but this can cause problems when
uploading a package to CHaP (or Hackage)

When a package is built from CHaP, only the dependencies on public libs are
resolved. With --enable-tests and --enable-benchmarks, the dependencies for
test-suites and benchmark suites are checked as well.

So, enabling tests might lead to a build plan that compiles successfully.
However, omitting these deps might lead to a different build plan that does not
compile, for example because newer versions of packages are used.

This script show the diff between two build plans for our published packages: (i)
the local dev setup, and (ii) a fresh configuration that disables test suites and
benchmark suitesthe, and uses the most recent index-states for CHaP and Hackage.

After the script is run, check the diff to see if the build plan changes little.
In particular, it is preferable to have no changes in major version for
packages. It is fine if dependencies disappear.

... running cabal-plan-diff.sh ...
"

function main() {
  if [[ -d "./tmp" ]]; then
    rm -r tmp
  fi
  mkdir tmp

  cabal update

  cabal build all --dry-run --minimize-conflict-set
  cp ./dist-newstyle/cache/plan.json ./tmp/plan1.json

  touch ./tmp/cabal.project
  echo -e "
repository cardano-haskell-packages
  url: https://chap.intersectmbo.org/
  secure: True
  root-keys:
    3e0cce471cf09815f930210f7827266fd09045445d65923e6d0238a6cd15126f
    443abb7fb497a134c343faf52f0b659bd7999bc06b7f63fa76dc99d631f9bea1
    a86a1f6ce86c449c46666bda44268677abf29b5b2d2eb5ec7af903ec2f117a82
    bcec67e8e99cabfa7764d75ad9b158d72bfacf70ca1d0ec8bc6b4406d1bf8413
    c00aae8461a256275598500ea0e187588c35a5d5d7454fb57eac18d9edb86a56
    d4a35cd3121aa00d18544bb0ac01c3e1691d618f462c46129271bccf39f7e8ee

index-state:
  , hackage.haskell.org HEAD
  , cardano-haskell-packages HEAD

with-compiler: ghc-9.2.8

tests: False
benchmarks: False

packages:
  ouroboros-consensus
  ouroboros-consensus-cardano
  ouroboros-consensus-protocol
  ouroboros-consensus-diffusion
  sop-extras
  strict-sop-core
" > ./tmp/cabal.project
  cabal build all --dry-run --minimize-conflict-set --project-file='./tmp/cabal.project'
  cp ./dist-newstyle/cache/plan.json ./tmp/plan2.json
}

main > /dev/null

cabal-plan diff --plan-json='tmp/plan1.json' --plan-json='tmp/plan2.json' | awk "/Package versions/{f=1} /Dependency graph/{f=0} f"

rm -r ./tmp