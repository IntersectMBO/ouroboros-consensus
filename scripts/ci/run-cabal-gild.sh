#!/usr/bin/env bash

set -euo pipefail

# First, try to find the 'fd' command
fdcmd="fd"
if ! command -v "$fdcmd" &> /dev/null; then
    # In Ubuntu systems the fd command is called fdfind.
    # If 'fd' is not found, try 'fdfind'
    fdcmd="fdfind"
    if ! command -v "$fdcmd" &> /dev/null; then
        echo "Error: Neither 'fd' nor 'fdfind' command found." >&2
        exit 1
    fi
fi

$fdcmd --full-path "$(pwd)/(ouroboros-consensus|sop-extras|strict-sop-core)" -e cabal -x cabal-gild -i {} -o {}
