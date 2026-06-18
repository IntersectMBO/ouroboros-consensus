#!/usr/bin/env bash

set -euo pipefail

cabal-gild ouroboros-consensus.cabal
cabal-gild cardano-config/cardano-config.cabal

case "$(uname -s)" in
    MINGW*) git ls-files --eol | grep "w/crlf" | awk '{print $4}' | xargs dos2unix;;
    *) ;;
esac || true
