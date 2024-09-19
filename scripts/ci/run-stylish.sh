#!/usr/bin/env bash

set -e

echo "The custom options for formatting this repo are:"
stylish-haskell --defaults | diff - ./.stylish-haskell.yaml | grep -E "^>.*[[:alnum:]]" | grep -v "#"
printf "\nFormatting haskell files...\n"

export LC_ALL=C.UTF-8
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

case "$(uname -s)" in
    MINGW*)     path="$(pwd -W | sed 's_/_\\\\_g')\\\\(ouroboros-consensus|sop-extras|strict-sop-core)";;
    *)          path="$(pwd)/(ouroboros-consensus|sop-extras|strict-sop-core)";;
esac

$fdcmd --full-path "$path" \
       --extension hs \
       --exclude Setup.hs \
       --exclude ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/SupportsMempool.hs \
       --exclude ouroboros-consensus-cardano/src/unstable-cardano-tools/Cardano/Tools/DBAnalyser/Analysis.hs \
       --exclude ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Storage/LedgerDB/V2/Init.hs \
       --exclude ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Storage/LedgerDB/V1/Init.hs \
       --exclude ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Mempool/Query.hs \
       --exclude ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Mempool/Impl/Common.hs \
       --exclude ouroboros-consensus-cardano/app/DBAnalyser/Parsers.hs \
       --exec-batch stylish-haskell -c .stylish-haskell.yaml -i

# We don't want these pragmas to be removed accidentally
f () {
    grep "#if __GLASGOW_HASKELL__.*
import" $1 >/dev/null 2>&1
}
f ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Ledger/SupportsMempool.hs
f ouroboros-consensus-cardano/src/unstable-cardano-tools/Cardano/Tools/DBAnalyser/Analysis.hs
f ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Storage/LedgerDB/V2/Init.hs
f ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Storage/LedgerDB/V1/Init.hs
f ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Mempool/Query.hs
f ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/Mempool/Impl/Common.hs
f ouroboros-consensus-cardano/app/DBAnalyser/Parsers.hs

case "$(uname -s)" in
    MINGW*) git ls-files --eol | grep "w/crlf" | awk '{print $4}' | xargs dos2unix;;
    *) ;;
esac
