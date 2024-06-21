#!/bin/bash

echo "=== cabal-plan-diff.sh ===

In our CI and in most development setups, we build our projects with
--enable-tests and --enable-benchmarks, but this can cause problems when
uploading a package to CHaP (or Hackage).

When a package is built from CHaP, only the dependencies on public libs are
resolved. With --enable-tests and --enable-benchmarks, the dependencies for
test-suites and benchmark suites are checked as well.

So, enabling tests might lead to a build plan that compiles successfully.
However, omitting these deps might lead to a different build plan that does not
compile, for example because newer versions of packages are used.

This script shows the diff between two build plans for our published packages: (i)
the local dev setup, and (ii) a fresh configuration that disables test suites and
benchmark suites, and uses the most recent index-states for CHaP and Hackage.

The script will show a summary of the version bumps and the dependencies that
have disappeared in either build plan. Using \`--show-diff\` shows the raw
\`cabal-plan\` diff. After the script is run, check the diff to see if the build
plan changes little. In particular, it is preferable to have no changes in major
version for packages. It is fine if dependencies disappear.

... running cabal-plan-diff.sh ...
"


if [[ -d "./tmp" ]]; then
  rm -r tmp
fi
mkdir tmp
trap 'rm -r ./tmp' EXIT

cabal update &>/dev/null

cabal build all --dry-run --minimize-conflict-set
cp ./dist-newstyle/cache/plan.json ./tmp/plan-full.json

GHC_BIN=$(readlink $(which ghc) | rev | cut -d'/' -f1 | rev)

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

with-compiler: $GHC_BIN

tests: False
benchmarks: False

packages:
  nf-vars
  resource-registry
  ouroboros-consensus
  ouroboros-consensus-cardano
  ouroboros-consensus-protocol
  ouroboros-consensus-diffusion
  sop-extras
  strict-sop-core
" > ./tmp/cabal.project

cabal build all --dry-run --minimize-conflict-set --project-file='./tmp/cabal.project'

retVal=$?
if [ $retVal -ne 0 ]; then
  echo "warning: Your project will fail to build in CHaP/Hackage"
  if [[ -n $GH ]]; then
    echo "::warning title=Unbuildable project::Your project will fail to build in CHaP/Hackage"
  fi
else
  cp ./dist-newstyle/cache/plan.json ./tmp/plan-libs.json

  cabal-plan diff --plan-json='tmp/plan-full.json' --plan-json='tmp/plan-libs.json' | awk "/Package versions/{f=1} /Dependency graph/{f=0} f" > ./tmp/the-diff-1

  for i in "$@" ; do [[ $i == "--show-diff" ]] && cat ./tmp/the-diff-1 && break ; done

  awk '/^$/{f=!f} f' ./tmp/the-diff-1 > ./tmp/the-diff

  grep "^\-" ./tmp/the-diff | cut -d'-' -f2- | cut -d' ' -f1 | sort -u > ./tmp/only-in-full
  grep "^\+" ./tmp/the-diff | cut -d'+' -f2- | cut -d' ' -f1 | sort -u > ./tmp/only-in-libs

  echo -e "\n=== TEST ONLY DEPENDENCIES"

  comm -2 -3 <(rev ./tmp/only-in-full | cut -d'-' -f2- | rev | sort -u) <(rev ./tmp/only-in-libs | cut -d'-' -f2- | rev | sort -u)

  echo -e "\n=== FULL-BUILD ONLY DEPENDENCIES"

  comm -1 -3 <(rev ./tmp/only-in-full | cut -d'-' -f2- | rev | sort -u) <(rev ./tmp/only-in-libs | cut -d'-' -f2- | rev | sort -u)

  echo -e "\n=== VERSION BUMPS FROM TESTS ONLY TO FULL-BUILD"

  common=$(comm -1 -2 <(rev ./tmp/only-in-full | cut -d'-' -f2- | rev | sort -u) <(rev ./tmp/only-in-libs | cut -d'-' -f2- | rev | sort -u))

  major=()
  minor=()
  patch=()

  for dep in $common; do
    full=$(grep -E "^$dep-([0-9]+\.)*[0-9]" ./tmp/only-in-full | rev | cut -d'-' -f1 | rev)
    if [[ $(echo $full | awk -F. '{print NF-1}') -lt 3 ]]; then
      n=$(((3 - $(echo $full | awk -F. '{print NF-1}'))*2))
      sff=".0.0.0.0"
      full="$full${sff:0:$n}"
    fi
    libs=$(grep -E "^$dep-([0-9]+\.)*[0-9]" ./tmp/only-in-libs | rev | cut -d'-' -f1 | rev)
    if [[ $(echo $libs | awk -F. '{print NF-1}') -lt 3 ]]; then
      n=$(((3 - $(echo $libs | awk -F. '{print NF-1}'))*2))
      sff=".0.0.0.0"
      libs="$libs${sff:0:$n}"
    fi
    verDiff=$(printf "%s\n" "$full" "$libs" | sed -e 'N;s/^\(.*\).*\n\1.*$/\1/' | awk -F. '{print NF-1}')
    case $verDiff in
      0)
        major+=("$dep $full -> $libs")
        ;;
      1)
        major+=("$dep $full -> $libs")
        ;;
      2)
        minor+=("$dep $full -> $libs")
        ;;
      *)
        patch+=("$dep $full -> $libs")
        ;;
    esac
  done

  echo -e "- MAJOR BUMPS"
  printf '%s\n' "${major[@]}" | sed 's/^\(.\+\)/   - \1/g'

  echo -e "- MINOR BUMPS"
  printf '%s\n' "${minor[@]}" | sed 's/^\(.\+\)/   - \1/g'

  echo -e "- PATCH BUMPS"
  printf '%s\n' "${patch[@]}" | sed 's/^\(.\+\)/   - \1/g'

  if [[ -n $GH && ${#major[@]} ]]; then
    str=$(printf '%s\n' "${major[@]}")
    echo "::warning title=Major bumps detected::$str"
  fi
fi
