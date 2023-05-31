#!/usr/bin/env bash

# set -x
# For each one of the two bundles, check that:
# (1) the version in the changelog is the same as the package's cabal file
# (2) if the version was changed from the last one on the changelog then there
#     must be no remaining changelog files
# otherwise exits with exit code 1

# Offline use:
#    ./scripts/ci/check-changelogs.sh main

# CI uses:
#    CI=1 ./this-pr/scripts/ci/check-changelogs.sh main this-pr

TARGET=${1:-"main"}
BASE=${2:-"."}

function get_last_version {
    cd $BASE
    if [[ -n $CI ]];
    then grep "<a id=" $1/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1
    else grep "<a id=" $1/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1
    fi
}

function cabal_files_version {
    cd $BASE
    if [[ -n $CI ]];
    then cat $1/$1.cabal | grep "^version" | rev | cut -d' ' -f1 | rev
    else cat $1/$1.cabal | grep "^version" | rev | cut -d' ' -f1 | rev
    fi
}

function this_merge_updates_version {
    cd $BASE
    if [[ -n $CI ]];
    then diff -w ../$TARGET/$1/$1.cabal $1/$1.cabal | grep "^> version"
    else git diff -b $TARGET HEAD -- $1/$1.cabal | grep "^+version"
    fi
}

function this_merge_adds_fragment {
    if [[ -n $CI ]];
    then echo "$(($(ls -A1 $BASE/$1/changelog.d | wc -l) - $(ls -A1 $TARGET/$1/changelog.d | wc -l)))"
    else git diff $TARGET HEAD --name-only --diff-filter=AR | grep $1/changelog.d | wc -l
    fi
}

function files-changed {
    if [[ -n $CI ]]
    then diff -x .git -x README.md -x changelog.d -x scripts -r $TARGET/$1 $BASE/$1
    else git diff $TARGET HEAD --name-only | grep -v README | grep -v changelog.d | grep "^$1/" | wc -l
    fi
}

function check {
    pkg=$1

    echo "Checking consistency of $pkg"

    if [[ $(files-changed $pkg) -gt 0 ]]; then

        version=$(cabal_files_version $pkg)
        last_version=$(get_last_version $pkg)
        updated_versions=$(this_merge_updates_version $pkg)
        adds_fragment=$(this_merge_adds_fragment $pkg)

        if [[ $version != $last_version ]]; then
            echo "ERROR: In $pkg, last version in the changelog ($last_version) is not the same as in the package's cabal file ($version)"
            exit 1
        elif [[ -n "$updated_versions" && $(ls -A1 $BASE/$pkg/changelog.d | wc -l) != 1 ]]; then
            echo "ERROR: In $pkg, last commit updated the version but there are remaining changelog fragments"
            exit 1
        elif [[ -z "$updated_versions" && $adds_fragment -lt 1 ]]; then
            echo "ERROR: In $pkg, there are no new changelog fragments comparing to pkg. This is enforced. Push an empty fragment if there is nothing to mention."
            echo "   new fragments - old fragments = $adds_fragment"
            exit 1
        else
            printf "version:       %s\nnew version:   %s\nnew fragments: $adds_fragment\nOK\n\n" $version $updated_versions $last_version
        fi
    else
        printf "No source files changed in %s\n\n" $pkg
    fi
}

check ouroboros-consensus
check ouroboros-consensus-protocol
check ouroboros-consensus-cardano
check ouroboros-consensus-diffusion
