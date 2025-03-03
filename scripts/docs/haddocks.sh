#!/usr/bin/env bash
# Build haddock documentation and an index for all projects in
# `ouroboros-consensus` repository.

set -euo pipefail

# pre-requisites
if ! command -v cabal-docspec &> /dev/null
then
    # cabal-docspec. Download binary
    curl -sL https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20240703/cabal-docspec-0.0.0.20240703-x86_64-linux.xz > cabal-docspec.xz
    # this doesn't seem to exist in GH runners?
    mkdir -p "$HOME"/.local/bin
    xz -d < cabal-docspec.xz > "$HOME"/.local/bin/cabal-docspec
    rm -f cabal-docspec.xz
    chmod a+x "$HOME"/.local/bin/cabal-docspec
    export PATH=$PATH:"$HOME"/.local/bin/
fi

echo "Building documentation of the packages"
# build documentation of all modules
grey=$(printf "\\033[38;5;245m")
white=$(printf "\\033[0m")

cabal build all 2>&1 | sed "s/^/${white}cabal> ${grey}/"
cabal-docspec 2>&1 | sed "s/^/${white}cabal-docspec> ${grey}/"
cabal haddock-project --prologue=./scripts/docs/prologue.haddock --hackage all 2>&1 | sed "s/^/${white}haddock> ${grey}/"

echo -e -n "${white}"

echo "Copying images"
# Copy modules map to output directory

# Generating the graph, note you need graphmod and a recent enough `dot` (ubuntu
# ships 2.42.2 but latest versions in 10.0.0)
#
# > fd --full-path ouroboros-consensus/src/ouroboros-consensus -e 'hs' | xargs graphmod -p -d 20,20 -q | dot -Tsvg -o"${SCRIPTS_DIR}/modules-consensus.svg"
cp ./scripts/docs/modules-consensus.svg ./haddocks
cp ./scripts/docs/packages-consensus.svg ./haddocks

# The Consensus.svg file is built using plantuml with C4 extensions
#
# > plantuml -tsvg c4-component.puml
cp ./scripts/docs/Consensus.svg ./haddocks

# HACK: Replace <img> tag with <object> tag for embedded svg
sed -i -e 's/\(.*\)<img src=".\/Consensus.svg" title="Ouroboros Consensus Components" \/>\(.*\)/\1<object data="Consensus.svg" type="image\/svg+xml"><\/object>\2/' ./haddocks/index.html

mv haddocks docs/website/static/haddocks

echo "Generated documentation in docs/website/static/haddocks"
