#!/usr/bin/env bash

set -e

cabal update
cabal freeze

for pkg in $(grep asserts cabal.project.freeze | awk '{$1=$1};1' | cut -d' ' -f1); do
    echo "
package $pkg
  flags: +asserts" >> asserts.cabal.new
done

rm cabal.project.freeze
