#!/usr/bin/env bash

set -e

common=$(sed '/package /q' cabal.project | sed '$ d')

echo "$common" > cabal.project

cabal update
cabal freeze

for pkg in $(grep asserts cabal.project.freeze | awk '{$1=$1};1' | cut -d' ' -f1); do
    echo "
package $pkg
  flags: +asserts" >> cabal.project
done
