#!/usr/bin/env bash

set -e

echo "The custom options for formatting this repo are:"
stylish-haskell --defaults | diff - ./.stylish-haskell.yaml | grep -E "^>.*[[:alnum:]]" | grep -v "#"
printf "\nFormatting haskell files...\n"

export LC_ALL=C.UTF-8
fd -p $(pwd)/ouroboros-consensus \
    -e hs \
    -E Setup.hs \
    -E ouroboros-consensus-cardano/app/DBAnalyser/Parsers.hs \
    -E ouroboros-consensus/src/ouroboros-consensus/Control/Concurrent/Class/MonadMVar/Strict/NoThunks.hs \
    -X stylish-haskell \
    -c .stylish-haskell.yaml -i


# We don't want these deprecation warnings to be removed accidentally
grep "#if __GLASGOW_HASKELL__ < 900
import           Data.Foldable (asum)
#endif" ouroboros-consensus-cardano/app/DBAnalyser/Parsers.hs                           >/dev/null 2>&1
grep "#if CHECK_MVAR_NOTHUNKS
import qualified Control.Concurrent.Class.MonadMVar.Strict.Checked as StrictMVar
import           Control.Concurrent.Class.MonadMVar.Strict.Checked as StrictMVar hiding (newMVar, newMVarWithInvariant, newEmptyMVarWithInvariant, newEmptyMVar)
#else
import qualified Control.Concurrent.Class.MonadMVar.Strict as StrictMVar
import           Control.Concurrent.Class.MonadMVar.Strict as StrictMVar hiding (newMVar, newMVarWithInvariant, newEmptyMVarWithInvariant, newEmptyMVar)
#endif" ouroboros-consensus/src/ouroboros-consensus/Control/Concurrent/Class/MonadMVar/Strict/NoThunks.hs >/dev/null 2>&1
