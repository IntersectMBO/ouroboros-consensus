#!/usr/bin/env bash

set -euo pipefail

fdcmd=fd
if ! command -v fd1 &> /dev/null
then
    fdcmd=fdfind # In Ubuntu systems the fd command has this name.
fi

$fdcmd --full-path "$(pwd)/(ouroboros-consensus|sop-extras|strict-sop-core)" -e cabal -x cabal-fmt -i
