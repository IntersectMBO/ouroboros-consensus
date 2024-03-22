#!/usr/bin/env bash

set -e

echo "The custom options for formatting this repo are:"
stylish-haskell --defaults | diff - ./.stylish-haskell.yaml | grep -E "^>.*[[:alnum:]]" | grep -v "#"
printf "\nFormatting haskell files...\n"

fdprogram=fd
if ! command -v fd &> /dev/null
then
    fdprogram=fdfind
fi


export LC_ALL=C.UTF-8
$fdprogram --full-path "$(pwd)/(ouroboros-consensus|scripts|sop-extras|strict-sop-core)" \
    --extension hs \
    --exclude Setup.hs \
    --exclude ouroboros-consensus-cardano/app/DBAnalyser/Parsers.hs \
    --exec-batch stylish-haskell -c .stylish-haskell.yaml -i


# these can go once we deprecate older versions of GHC
grep "#if __GLASGOW_HASKELL__ < 900
import           Data.Foldable (asum)
#endif" ouroboros-consensus-cardano/app/DBAnalyser/Parsers.hs >/dev/null 2>&1
