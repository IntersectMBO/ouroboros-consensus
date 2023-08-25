#!/usr/bin/env bash

set -euo pipefail

fd --full-path "$(pwd)/(ouroboros-consensus|sop-extras|strict-sop-core)" -e cabal -x cabal-fmt -i
