#!/usr/bin/env bash

# Fails if the PR branch contains merge commits not already present on the base
# branch. Merge commits in the history of main (e.g. from past releases) are
# allowed; only new ones introduced by the PR are rejected.
#
# Expected env vars:
# - BASE_REF: what to compare this branch against (default: "main")

set -euo pipefail

BASE_REF="${BASE_REF:-main}"

echo "####### Checking for merge commits not on origin/${BASE_REF}"

merge_commits=$(git log --merges --oneline "origin/${BASE_REF}..HEAD")

if [ -z "$merge_commits" ]; then
    echo "OK: no merge commits found."
    exit 0
fi

echo "ERROR: the following merge commits were found in this branch but not in origin/${BASE_REF}:"
echo "$merge_commits" | sed 's/^/  - /g'
echo ""
echo "Please rebase your branch onto origin/${BASE_REF} instead of merging it."
exit 1
