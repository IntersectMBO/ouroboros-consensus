#!/usr/bin/env bash

set -euo pipefail

fdprogram=fd
if ! command -v fd &> /dev/null
then
    fdprogram=fdfind
fi

$fdprogram --full-path "$(pwd)/(ouroboros-consensus|sop-extras|strict-sop-core)" -e cabal -x cabal-fmt -i
