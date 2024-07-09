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
    MINGW*)     path="$(pwd -W | sed 's_/_\\\\_g')\\\\(ouroboros-consensus|sop-extras|strict-sop-core|resource-registry|nf-vars)";;
    *)          path="$(pwd)/(ouroboros-consensus|sop-extras|strict-sop-core|resource-registry|nf-vars)";;
esac

$fdcmd --full-path "$path" \
       --extension hs \
       --exclude ouroboros-consensus-cardano/app/DBAnalyser/Parsers.hs \
       --exclude resource-registry/test/Main.hs \
       --exec-batch stylish-haskell -c .stylish-haskell.yaml -i

# We don't want these to be removed accidentally
grep "#if __GLASGOW_HASKELL__ < 900
import           Data.Foldable (asum)
#endif" ouroboros-consensus-cardano/app/DBAnalyser/Parsers.hs >/dev/null 2>&1
grep "#if __GLASGOW_HASKELL__ >= 900
import           Control.Monad.IO.Class
#endif" resource-registry/test/Main.hs                        >/dev/null 2>&1

case "$(uname -s)" in
    MINGW*) git ls-files --eol | grep "w/crlf" | awk '{print $4}' | xargs dos2unix;;
    *) ;;
esac
