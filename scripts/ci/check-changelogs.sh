#!/usr/bin/env bash

# Expected env vars:
# - NO_CHANGELOG_LABEL: disables the check for changelog fragments additions
# - BASE_REF: what to compare this branch against

packages=(ouroboros-consensus ouroboros-consensus-diffusion ouroboros-consensus-protocol ouroboros-consensus-cardano sop-extras strict-sop-core)
tracked_libs=(ouroboros-consensus/src/ouroboros-consensus
              ouroboros-consensus-diffusion/src/ouroboros-consensus-diffusion
              ouroboros-consensus-protocol/src/ouroboros-consensus-protocol
              ouroboros-consensus-cardano/src/shelley
              ouroboros-consensus-cardano/src/byron
              ouroboros-consensus-cardano/src/ouroboros-consensus-cardano
              sop-extras
              strict-sop-core)

ok=1

echo "Checking that changelog and .cabal versions match:"
for p in "${packages[@]}"; do
    if [[ $(grep -E "^<a id='changelog-" "$p/CHANGELOG.md" | head -n1) =~ $(grep -E "^version:" "$p/$p.cabal" | rev | cut -d' ' -f1 | rev) ]]; then
        printf "\t- %s OK\n" "$p"
    else
        printf "\t- %s FAIL\n" "$p"
        ok=0
    fi
done

[ $ok = 0 ] && exit 1

if [ "${NO_CHANGELOG_LABEL}" = "true" ] || [ "${RELEASE_LABEL}" = "true" ]; then
    echo "Label set: No new changelog fragments expected"
else
    echo "Checking for new changelog fragments:"
    for p in "${tracked_libs[@]}"; do
        printf "\t- %s\n" "$p"
        if ! git diff --quiet --name-only "origin/${BASE_REF}" -- "$p/***.hs"; then
            if ! git diff --quiet --name-only --diff-filter=A "origin/${BASE_REF}" -- "$(echo $p | cut -d/ -f1)/changelog.d" ; then
                printf "\t\tNew fragments found. OK.\n"
                git diff --name-only --diff-filter=A "origin/${BASE_REF}" -- "$p/changelog.d" | sed 's/^/\t\t- /g'
            else
                printf "\t\tNo new fragments found, but code changed. Please push a fragment or add the \"no changelog\" label to the PR. The diff follows:\n"
                git --no-pager -c color.diff=always diff "origin/${BASE_REF}" -- "$p/***.hs" | sed 's/^/diff> /g'
                ok=0
            fi
        else
            printf "\t\tNo haskell code changes\n"
        fi
    done
    for p in "${packages[@]}"; do
        printf "\t- %s\n" "$p"
        if ! git diff --quiet --name-only "origin/${BASE_REF}" -- "$p/***.cabal"; then
            if ! git diff --quiet --name-only --diff-filter=A "origin/${BASE_REF}" -- "$(echo $p | cut -d/ -f1)/changelog.d" ; then
                printf "\t\tNew fragments found. OK.\n"
                git diff --name-only --diff-filter=A "origin/${BASE_REF}" -- "$p/changelog.d" | sed 's/^/\t\t- /g'
            else
                printf "\t\tNo new fragments found, but code changed. Please push a fragment or add the \"no changelog\" label to the PR. The diff follows:\n"
                git --no-pager -c color.diff=always diff "origin/${BASE_REF}" -- "$p/***.cabal" | sed 's/^/diff> /g'
                ok=0
            fi
        else
            printf "\t\tNo cabal code changes\n"
        fi
    done
fi

[ $ok = 0 ] && exit 1

if [ "${RELEASE_LABEL}" = "true" ]; then
  echo "This PR is said to be a release. Checking that changelog.d directories are in fact empty"
  for p in "${packages[@]}"; do
    if [ 1 = $(ls -1 $p/changelog.d | wc -l) ]; then
      printf "\t- %s OK" "$p"
    else
      printf "\t- %s ERROR: There are fragments remaining in changelog.d!"
      exit 1
    fi
  done
fi
