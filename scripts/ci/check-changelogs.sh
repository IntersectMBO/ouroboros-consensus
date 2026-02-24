#!/usr/bin/env bash

# Expected env vars:
# - NO_CHANGELOG_LABEL: disables the check for changelog fragments additions
# - BASE_REF: what to compare this branch against

ok=1
IFS=','


packages=("ouroboros-consensus,." "sop-extras,sop-extras" "strict-sop-core,strict-sop-core")

# libraries :: [(package directory, relative src directory, cabal-file name)]
libraries=(".,ouroboros-consensus/src/ouroboros-consensus"
           ".,ouroboros-consensus/src/ouroboros-consensus-lmdb"
           ".,ouroboros-consensus/src/ouroboros-consensus-lsm"
           ".,ouroboros-consensus-diffusion/src/ouroboros-consensus-diffusion"
           ".,ouroboros-consensus-protocol/src/ouroboros-consensus-protocol"
           ".,ouroboros-consensus-cardano/src/ouroboros-consensus-cardano"
           ".,ouroboros-consensus-cardano/src/byron"
           ".,ouroboros-consensus-cardano/src/shelley"
           "sop-extras,src"
           "strict-sop-core,src"
          )

echo "####### Checking for Haskell changes"

if [ "${NO_CHANGELOG_LABEL}" = "true" ] || [ "${RELEASE_LABEL}" = "true" ]; then
    echo "Label set: No new changelog fragments expected"
else
    for p in "${libraries[@]}"; do
        set -- $p
        printf "\t- %s\n" "$1/$2"
        # if some haskell file changed
        if ! git diff --quiet --name-only "origin/${BASE_REF}" -- "$1/$2/***.hs"; then
            # if something was added, modified or removed in the changelog folder
            if ! git diff --quiet --name-only --diff-filter=AMR "origin/${BASE_REF}" -- "$1/changelog.d"; then
                printf "\t\tNew fragments found. OK.\n"
                git diff --name-only --diff-filter=AMR "origin/${BASE_REF}" -- "$1/changelog.d" | sed 's/^/\t\t- /g'
            else
                printf "\t\tNo new fragments found, but code changed. Please push a fragment or add the \"no changelog\" label to the PR. The diff follows:\n"
                git --no-pager -c color.diff=always diff "origin/${BASE_REF}" -- "$1/$2/***.hs" | sed 's/^/diff> /g'
                ok=0
            fi
        else
            printf "\t\tNo haskell code changes\n"
        fi
    done
fi

echo "####### Checking for Cabal changes"

if [ "${NO_CHANGELOG_LABEL}" = "true" ] || [ "${RELEASE_LABEL}" = "true" ]; then
    echo "Label set: No new changelog fragments expected"
else
    for p in "${packages[@]}"; do
        set -- $p
        printf "\t- %s\n" "$1"
        if ! git diff --quiet --name-only "origin/${BASE_REF}" -- "$2/$1.cabal"; then
            if ! git diff --quiet --name-only --diff-filter=AMR "origin/${BASE_REF}" -- "$2/changelog.d/*.md" ; then
                printf "\t\tNew fragments found. OK.\n"
                git diff --name-only --diff-filter=AMR "origin/${BASE_REF}" -- "$2/changelog.d/*.md" | sed 's/^/\t\t- /g'
            else
                printf "\t\tNo new fragments found, but code changed. Please push a fragment or add the \"no changelog\" label to the PR. The diff follows:\n"
                git --no-pager -c color.diff=always diff "origin/${BASE_REF}" -- "$2/$1.cabal" | sed 's/^/diff> /g'
                ok=0
            fi
        else
            printf "\t\tNo cabal code changes\n"
        fi
    done
fi

[ $ok = 0 ] && exit 1

echo "####### If this has the release label, changelogs must be empty"

if [ "${RELEASE_LABEL}" = "true" ]; then
  for p in "${packages[@]}"; do
      set -- $p
      printf "\t- %s\n" "$1"
      if [ 1 = $(ls -1 $2/changelog.d | wc -l) ]; then
          printf "\t- %s OK\n" "$p"
      else
          printf "\t- %s ERROR: There are fragments remaining in changelog.d:\n" "$p"
          ls -1 $2/changelog.d
          ok=0
      fi
  done
fi

[ $ok = 0 ] && exit 1

echo "####### Checking that changelog and .cabal versions match:"
for p in "${packages[@]}"; do
    set -- $p
    if [[ $(grep -E "^<a id='changelog-" "$2/CHANGELOG.md" | head -n1) =~ $(grep -E "^version:" $2/$1.cabal | rev | cut -d' ' -f1 | rev) ]]; then
        printf "\t- $1 OK\n"
    else
        printf "\t- $2 FAIL\n"
        ok=0
    fi
done

[ $ok = 0 ] && exit 1


echo "####### Checking the cabal version matches the version in the badge in the README"

for p in "${packages[@]}"; do
    set -- $p
    pkg=$(grep "^name: " $2/$1.cabal | rev | cut -d' ' -f1 | rev)
    ver=$(grep "^version: " $2/$1.cabal | rev | cut -d' ' -f1 | rev)
    printf "\t- Checking badge for %s " "$pkg-$ver"
    if ! grep -q "$(echo $pkg | sed 's/-/--/g')-$ver" README.md; then
        echo "FAIL"
        ok=1
    else
        echo "OK"
    fi
done

exit $ok
