#!/usr/bin/env bash

# Expected env vars:
# - NO_CHANGELOG_LABEL: disables the check for changelog fragments additions
# - RELEASE_LABEL:
# - BASE_REF: what to compare this branch against (default: "main")

BASE_REF="${BASE_REF:-main}"

ok=1

# libraries :: [relative src directory]
libraries=("ouroboros-consensus/src/ouroboros-consensus"
           "ouroboros-consensus/src/ouroboros-consensus-lmdb"
           "ouroboros-consensus/src/ouroboros-consensus-lsm"
           "ouroboros-consensus-diffusion/src/ouroboros-consensus-diffusion"
           "ouroboros-consensus-protocol/src/ouroboros-consensus-protocol"
           "ouroboros-consensus-cardano/src/ouroboros-consensus-cardano"
           "ouroboros-consensus-cardano/src/byron"
           "ouroboros-consensus-cardano/src/shelley"
          )

echo "####### Checking for Haskell changes"

if [ "${NO_CHANGELOG_LABEL}" = "true" ] || [ "${RELEASE_LABEL}" = "true" ]; then
    echo "Label set: No new changelog fragments expected"
else
    for p in "${libraries[@]}"; do
        printf "\t- %s\n" "$p"
        # if some haskell file changed
        if ! git diff --quiet --name-only "origin/${BASE_REF}" -- "$p/***.hs"; then
            # if something was added, modified or removed in the changelog folder
            if ! git diff --quiet --name-only --diff-filter=AMR "origin/${BASE_REF}" -- "changelog.d"; then
                printf "\t\tNew fragments found. OK.\n"
                git diff --name-only --diff-filter=AMR "origin/${BASE_REF}" -- "changelog.d" | sed 's/^/\t\t- /g'
            else
                printf "\t\tNo new fragments found, but code changed. Please push a fragment or add the \"no changelog\" label to the PR. The diff follows:\n"
                git --no-pager -c color.diff=always diff "origin/${BASE_REF}" -- "$p/***.hs" | sed 's/^/diff> /g'
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
    if ! git diff --quiet --name-only "origin/${BASE_REF}" -- "ouroboros-consensus.cabal"; then
        if ! git diff --quiet --name-only --diff-filter=AMR "origin/${BASE_REF}" -- "changelog.d/*.md" ; then
            printf "\t\tNew fragments found. OK.\n"
            git diff --name-only --diff-filter=AMR "origin/${BASE_REF}" -- "changelog.d/*.md" | sed 's/^/\t\t- /g'
        else
            printf "\t\tNo new fragments found, but code changed. Please push a fragment or add the \"no changelog\" label to the PR. The diff follows:\n"
            git --no-pager -c color.diff=always diff "origin/${BASE_REF}" -- "ouroboros-consensus.cabal" | sed 's/^/diff> /g'
            ok=0
        fi
    else
        printf "\t\tNo cabal code changes\n"
    fi
fi

[ $ok = 0 ] && exit 1

echo "####### If this has the release label, changelogs must be empty"

if [ "${RELEASE_LABEL}" = "true" ]; then
  if [ 1 = $(ls -1 changelog.d | wc -l) ]; then
      echo "OK"
  else
      echo "ERROR: There are fragments remaining in changelog.d:"
      ls -1 changelog.d
      ok=0
  fi
fi

[ $ok = 0 ] && exit 1

echo "####### Checking that changelog and .cabal versions match:"
if [[ $(grep -E "^<a id='changelog-" "CHANGELOG.md" | head -n1) =~ $(grep -E "^version:" ouroboros-consensus.cabal | rev | cut -d' ' -f1 | rev) ]]; then
    echo "OK"
else
    echo "FAIL"
    ok=0
fi

[ $ok = 0 ] && exit 1

echo "####### Checking the cabal version matches the version in the badge in the README"

ver=$(grep "^version: " ouroboros-consensus.cabal | rev | cut -d' ' -f1 | rev)
if ! grep -q "ouroboros--consensus-$ver" README.md; then
    echo "FAIL"
    ok=0
else
    echo "OK"
fi

exit $((! $ok))
