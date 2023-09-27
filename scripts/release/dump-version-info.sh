#!/usr/bin/env bash

set -eu

# Emit a JSON object in which each property is a package name and its value is
# an object whose properties are the version (as a string) from its cabal file,
# the latest version (as a string) from its `CHANGELOG.md' file, and the names
# of the files other than `scriv.ini' in its `changelog.d' directory.
#
# There are two assumptions made. First, every `scriv.ini' file is at depth 3
# in the git repository. Second, each `${GITROOT}/X/changelog.d/scriv.ini' file
# has matching `${GITROOT}/X/X.cabal' and `${GITROOT}/X/CHANGELOG.md' files,
# such that the cabal package name is also `X'.

# See https://stackoverflow.com/a/3352015
function trim {
    local var="$*"
    # remove leading whitespace characters
    var="${var#"${var%%[![:space:]]*}"}"
    # remove trailing whitespace characters
    var="${var%"${var##*[![:space:]]}"}"
    printf '%s' "$var"
}

function project_cabal_file_version {
    trim "$(grep -E -e '^version:' | head -n1 | cut -d':' -f2-)"
}

function project_latest_changelog_version {
    trim "$(grep -E -e '^## ' | head -n1 | awk '{print $2}')"
}

function normalize_changelog.d_filenames {
    grep -vE -e '^scriv.ini$' | sort
}

function WORKINGTREE_package {
    pkg="$(basename "$1")"
    echo "\"$pkg\": {"

    echo -n '"version": "'
    cat "$1/$pkg.cabal" | project_cabal_file_version
    echo '",'

    echo -n '"changelog-version": "'
    cat "$1/CHANGELOG.md" | project_latest_changelog_version
    echo '",'

    echo -n '"changelog.d-files": '
    echo -n '['
    local comma=0
    (cd "$1/changelog.d/"; ls -A1) | normalize_changelog.d_filenames | while read -r line; do
        [[ $((comma++)) -gt 0 ]] && echo -n ','
        echo -n "\"$line\""
    done
    echo ']'

    echo '}'
}

function GITREV_package {
    echo "\"$2\": {"

    echo -n '"version": "'
    git show "$1:$2/$2.cabal" | project_cabal_file_version
    echo '",'

    echo -n '"changelog-version": "'
    git show "$1:$2/CHANGELOG.md" | project_latest_changelog_version
    echo '",'

    echo -n '"changelog.d-files": '
    echo -n '['
    local comma=0
    basename -a $(git ls-tree --name-only "$1" --full-tree -- "$2/changelog.d/") | normalize_changelog.d_filenames | while read -r line; do
        [[ $((comma++)) -gt 0 ]] && echo -n ','
        echo -n "\"$line\""
    done
    echo ']'

    echo '}'
}

function WORKINGTREE_case {
    local comma=0
    find "$(git rev-parse --show-toplevel)" -maxdepth 3 -type f -name 'scriv.ini' | sort | while read -r path; do
        [[ $((comma++)) -gt 0 ]] && echo -n ','
        WORKINGTREE_package "$(realpath "$(dirname "$path")/..")"
    done
}

function GITREV_case {
    local comma=0
    # `sort' behaves differently if the trailing slash is present or not, and
    # it will be present in the `WORKINGTREE_case', so synthetically add it
    # here.
    git ls-tree --name-only "$1" --full-tree | sed -r 's|$|/|' | sort | while read -r dir; do
        dir="$(basename "$dir")"   # remove the trailing slash
        for i in $(git ls-tree --name-only "$1" --full-tree -- "$dir/changelog.d"); do
            [[ $((comma++)) -gt 0 ]] && echo -n ','
            GITREV_package "$1" "$dir"
        done
    done
}

function main {
    echo '{'
    if [ $# -eq 0 ]; then
        WORKINGTREE_case
    else
        GITREV_case "$1"
    fi
    echo '}'
}

main "$@"
