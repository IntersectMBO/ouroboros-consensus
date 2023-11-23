#!/usr/bin/env bash

# Expected env vars:
# - NO_CHANGELOG_LABEL: disables the check for changelog fragments additions
# - BASE_REF: what to compare this branch against

packages=(ouroboros-consensus ouroboros-consensus-diffusion ouroboros-consensus-protocol ouroboros-consensus-cardano sop-extras strict-sop-core)

echo "Checking that changelog and .cabal versions match:"
for p in "${packages[@]}"; do
    if [[ $(grep -E "^<a id='changelog-" "$p/CHANGELOG.md" | head -n1) =~ $(grep -E "^version:" "$p/$p.cabal" | rev | cut -d' ' -f1 | rev) ]]; then
        printf "\t- %s OK\n" "$p"
    else
        printf "\t- %s FAIL\n" "$p"
        exit 1
    fi
done

if [ "${NO_CHANGELOG_LABEL}" = "true" ]; then
    exit 0
else
    echo "Checking for new changelog fragments:"
    for p in "${packages[@]}"; do
        printf "\t- %s\n" "$p"
        if ! git diff --quiet --name-only "origin/${BASE_REF}" -- "$p/***.hs"|| ! git diff --quiet --name-only "origin/${BASE_REF}" -- "$p/***.cabal"; then
            if ! git diff --quiet --name-only --diff-filter=A "origin/${BASE_REF}" -- "$p/changelog.d" ; then
                printf "\t\tNew fragments found. OK.\n"
                git diff --name-only --diff-filter=A "origin/${BASE_REF}" -- "$p/changelog.d" | sed 's/^/\t\t- /g'
            else
                printf "\t\tNo new fragments found, but code changed. Please push a fragment or add the \"no changelog\" label to the PR. The diff follows:\n"
                git --no-pager -c color.diff=always diff "origin/${BASE_REF}" -- "$p/***.hs" | sed 's/^/diff> /g'
                git --no-pager -c color.diff=always diff "origin/${BASE_REF}" -- "$p/***.cabal" | sed 's/^/diff> /g'
                exit 1
            fi
        else
            printf "\t\tNo haskell/cabal code changes\n"
        fi
    done
fi
