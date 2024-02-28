#!/usr/bin/env bash
# Build haddock documentation and an index for all projects in
# `ouroboros-consensus` repository.
#
# usage:
# ./scripts/docs/haddocks.sh [output-dir] [build-dir]
#
# $1 - where to put the generated pages, this directory contents will be wiped
#      out (so don't pass `/` or `./` - the latter will delete your 'dist-newstyle')
#      (the default is './haddocks')
#
# $2 - cabal build directory
#      (the default is "dist-newstyle")

set -euo pipefail

OUTPUT_DIR=${1:-"./docs/website/static/haddocks"}
BUILD_DIR=${2:-"dist-newstyle"}

# the directory containing this script
SCRIPTS_DIR=$(realpath "$(dirname "$(realpath "$0")")")
GHC_VERSION=$(ghc --numeric-version)


# pre-requisites
if ! command -v cabal-docspec &> /dev/null
then
  # cabal-docspec. Download binary
  curl -sL https://github.com/phadej/cabal-extras/releases/download/cabal-docspec-0.0.0.20230517/cabal-docspec-0.0.0.20230517-x86_64-linux.xz > cabal-docspec.xz
  xz -d < cabal-docspec.xz > "$HOME"/.local/bin/cabal-docspec
  rm -f cabal-docspec.xz
  chmod a+x "$HOME"/.local/bin/cabal-docspec
fi

# we don't include `--use-index` option, because then quickjump data is not
# generated.  This is not ideal, but there is no way to generate only top level
# `doc-index.html` file.  With this approach we get:
#
# * `doc-index.json` and `doc-index.html` per package
#
# * we can generate top level `doc-index.json` (which will only work at the top
#   level).
#
# * we could ammend package level `doc-index.json` files, but it's enough ...
#   this should be fixed upstream.
HADDOCK_OPTS=(
    --builddir "${BUILD_DIR}"
    --disable-optimization
    --haddock-all
    --haddock-internal
    --haddock-html
    --haddock-quickjump
    --haddock-hyperlink-source
    --haddock-option "--show-all"
    --haddock-option "--use-unicode"
    --haddock-option "--use-contents=../index.html"
  )

echo "Building documentation of the packages"
# build documentation of all modules
grey=$(printf "\\033[38;5;245m")
white=$(printf "\\033[0m")

cabal build all | sed "s/^/${white}cabal> ${grey}/"

OS_ARCH="$(jq -r '.arch + "-" + .os' dist-newstyle/cache/plan.json | head -n 1 | xargs)"

cabal-docspec --extra-package latex-svg-image --extra-package directory | sed "s/^/${white}cabal-docspec> ${grey}/"

cabal haddock "${HADDOCK_OPTS[@]}" all 2>&1 | sed "s/^/${white}haddock> ${grey}/"
echo -e -n "\\033[0m"

if [[ ! ( -d ${OUTPUT_DIR} ) ]]; then
  mkdir -p "${OUTPUT_DIR}"
else
  rm -rf "${OUTPUT_DIR:?}"/*
fi

# make all files user writable
chmod -R u+w "${OUTPUT_DIR}"

copy_components () {
  package=$1
  dir=$2
  suffix=$3
  has_subdir=$4
  if [[ -d "${dir}/${suffix}" ]]; then
    # component_dir ~ dist-newstyle/build/x86_64-linux/ghc-9.6.3/ouroboros-consensus-0.9.0.0/t/consensus-test
    for component_dir in "${dir}/${suffix}"/*; do
      # component ~ consensus-test
      component="$(echo "$component_dir" | rev | cut -d'/' -f1 | rev)"
      if [[ "$has_subdir" == "true" ]]; then
        component_doc_dir="${component_dir}/noopt/doc/html/${package}/${component}"
      else
        component_doc_dir="${component_dir}/noopt/doc/html/${package}"
      fi
      if [[ -d "$component_doc_dir" ]]; then
        cp -r "$component_doc_dir" "${OUTPUT_DIR}/${package}-${component}"
        printf "\t\t- %s:%s:%s\n" "$package" "$suffix" "$component"
      fi
    done
  fi
}

echo "Copying generated documentation"
# copy the new docs
# dir ~ dist-newstyle/build/x86_64-linux/ghc-9.6.3/ouroboros-consensus-0.9.0.0
for dir in "${BUILD_DIR}/build/${OS_ARCH}/ghc-${GHC_VERSION}"/*; do
  # package ~  ouroboros-consensus
  package=$(echo "${dir//-[0-9]\+\(\.[0-9]\+\)*/}" | rev | cut -d'-' -f2- | cut -d'/' -f1 | rev)
  printf "\t- %s\n" "$package"
  cp -r "${dir}/noopt/doc/html/${package}" "${OUTPUT_DIR}/${package}"
  printf "\t\t- library\n"
  copy_components "$package" "$dir" "l" "false"
  copy_components "$package" "$dir" "t" "true"
  copy_components "$package" "$dir" "b" "true"
  copy_components "$package" "$dir" "x" "true"
done

# --read-interface options
declare -a interface_options=()
for package_path in "${OUTPUT_DIR}"/*/; do
  if [[ -d "${package_path}" ]]; then
    package=$(echo "${package_path}" | rev | cut -d'/' -f2 | rev)
    # take the first haddock file found.
    # there should be only one but the filename is the name of the main package
    # and can differ from the name of the enclosing directory
    haddock_file=$(find "${package_path}" -maxdepth 1 -type f -name "*.haddock" -print | cut -d/ -f2- | head -1)
    interface_options+=("--read-interface=${package},${haddock_file}")
  fi
done

echo "Generating top level page"
# Generate top level index using interface files
#
haddock \
  -o "${OUTPUT_DIR}" \
  --title "ouroboros-consensus" \
  --package-name "Ouroboros Consensus" \
  --gen-index \
  --gen-contents \
  --quickjump \
  --prolog "${SCRIPTS_DIR}/prologue.haddock" \
  "${interface_options[@]}"

echo "Assembling doc-index.json"
# Assemble a top-level `doc-index.json` from package level ones.
#
echo "[]" > "${OUTPUT_DIR}/doc-index.json"
for file in "$OUTPUT_DIR"/*/doc-index.json; do
  project=$(basename "$(dirname "$file")");
  jq -s \
    ".[0] + [.[1][] | (. + {link: (\"${project}/\" + .link)}) ]" \
    "${OUTPUT_DIR}/doc-index.json" \
    "${file}" \
    > /tmp/doc-index.json
  mv /tmp/doc-index.json "${OUTPUT_DIR}/doc-index.json"
done

echo "Copying images"
# Copy modules map to output directory

# Generating the graph, note you need graphmod and a recent enough `dot` (ubuntu
# ships 2.42.2 but latest versions in 10.0.0)
#
# > fd --full-path ouroboros-consensus/src/ouroboros-consensus -e 'hs' | xargs graphmod -p -d 20,20 -q | dot -Tsvg -o"${SCRIPTS_DIR}/modules-consensus.svg"
cp "${SCRIPTS_DIR}/modules-consensus.svg" "${OUTPUT_DIR}"
cp "${SCRIPTS_DIR}/packages-consensus.svg" "${OUTPUT_DIR}"

# The Consensus.svg file is built using plantuml with C4 extensions
#
# > plantuml -tsvg c4-component.puml
cp "${SCRIPTS_DIR}/Consensus.svg" "${OUTPUT_DIR}"

# HACK: Replace <img> tag with <object> tag for embedded svg
sed -i -e 's/\(.*\)<img src=".\/Consensus.svg" title="Ouroboros Consensus Components" \/>\(.*\)/\1<object data="Consensus.svg" type="image\/svg+xml"><\/object>\2/' "${OUTPUT_DIR}/index.html"

echo "Generated documentation in ${OUTPUT_DIR}"
