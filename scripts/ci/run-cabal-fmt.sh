#!/usr/bin/env bash

set -euo pipefail

fd -p $(pwd)/ouroboros-consensus -e cabal -x cabal-fmt -i

fd -p $(pwd)/strict-mvar-checked -e cabal -x cabal-fmt -i
