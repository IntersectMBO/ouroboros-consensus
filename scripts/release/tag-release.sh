#!/usr/bin/env bash

# Creates the right tags for the release
#
# To be run on `main` just after a release was done

if [[ ! $(git rev-parse --abbrev-ref HEAD) == "main" ]]; then
    echo "This must be run on main"
    exit 1
fi

git pull

consensus_last_version=$(grep "<a id=" ouroboros-consensus/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)
protocol_last_version=$(grep "<a id=" ouroboros-consensus-protocol/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)
cardano_last_version=$(grep "<a id=" ouroboros-consensus-cardano/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)
diffusion_last_version=$(grep "<a id=" ouroboros-consensus-diffusion/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1)

if [[ -z $consensus_last_version || -z $protocol_last_version || -z $cardano_last_version || -z $diffusion_last_version ]]; then
    echo "Some package has no version at all in the changelog. This is a critical error!"
fi

./scripts/ci/check-changelogs.sh || (echo "Inconsistent versioning!" && exit 1)

tags=()

if [[ ! $(git tag -l "release-consensus-$consensus_last_version") ]]; then
    t="release-consensus-$consensus_last_version"
    git tag $t HEAD
    tags+=($t)
fi

if [[ ! $(git tag -l "release-protocol-$protocol_last_version") ]]; then
    t="release-protocol-$protocol_last_version"
    git tag $t HEAD
    tags+=($t)
fi

if [[ ! $(git tag -l "release-cardano-$cardano_last_version") ]]; then
    t="release-cardano-$cardano_last_version"
    git tag $t HEAD
    tags+=($t)
fi

if [[ ! $(git tag -l "release-diffusion-$diffusion_last_version") ]]; then
    t="release-diffusion-$diffusion_last_version"
    git tag $t HEAD
    tags+=($t)
fi

printf "Tagged the release. Please push the following tags to origin:\n%s" "${tags[*]}"
