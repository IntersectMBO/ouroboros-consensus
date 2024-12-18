#!/usr/bin/env bash

set -Eeuo pipefail

for x in $(find . -name '*.cabal' | grep -vE 'dist-newstyle|asserts\.cabal' | cut -c 3-); do
  (
    d=$(dirname $x)
    echo "== $d =="
    cd $d
    cabal check
  )
done
