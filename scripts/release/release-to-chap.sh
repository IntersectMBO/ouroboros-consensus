#!/usr/bin/env bash

# Create a release to CHaP.
#
# Tags must exist in `ouroboros-consensus` repository. It will release the
# latest version mentioned in the changelogs, pointing at the tag for that
# version.
git pull

function last_version {
  grep "<a id=" $1/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1
}

consensus_last_version=$(last_version ouroboros-consensus)
protocol_last_version=$(last_version ouroboros-consensus-protocol)
cardano_last_version=$(last_version ouroboros-consensus-cardano)
diffusion_last_version=$(last_version ouroboros-consensus-diffusion)

if [[ ! $(git tag -l "release-consensus-$consensus_last_version") || ! $(git tag -l "release-protocol-$protocol_last_version") || ! $(git tag -l "release-cardano-$cardano_last_version") || ! $(git tag -l "release-diffusion-$diffusion_last_version") ]]; then
    echo "There are no tags pushed for the versions! Checkout the commit after bors merged the release PR and run ./scripts/release/tag-release.sh"
    exit 1
fi

if [[ ! -d chap ]]; then
  git clone git@github.com:input-output-hk/cardano-haskell-packages chap
else
  ( cd chap
    git checkout main
    git pull
  )
fi

( cd chap
  # delete branch if it existed
  if [[ $(git branch -v | grep release-consensus) ]]; then
    git branch --delete release-consensus
  fi
  if [[ $(git branch --remote -v | grep release-consensus) ]]; then
    git push origin --delete release-consensus
  fi
  git checkout -b release-consensus

  if [[ ! -d _sources/ouroboros-consensus/$consensus_last_version ]]; then
      ./scripts/add-from-github.sh https://github.com/input-output-hk/ouroboros-consensus $(cd ..; git rev-list -n 1 release-consensus-$consensus_last_version) ouroboros-consensus
  fi

  if [[ ! -d _sources/ouroboros-consensus-protocol/$protocol_last_version ]]; then
      ./scripts/add-from-github.sh https://github.com/input-output-hk/ouroboros-consensus $(cd ..; git rev-list -n 1 release-protocol-$protocol_last_version) ouroboros-consensus-protocol
  fi

  if [[ ! -d _sources/ouroboros-consensus-cardano/$cardano_last_version ]]; then
      ./scripts/add-from-github.sh https://github.com/input-output-hk/ouroboros-consensus $(cd ..; git rev-list -n 1 release-cardano-$cardano_last_version) ouroboros-consensus-cardano
  fi

  if [[ ! -d _sources/ouroboros-consensus-cardano-test/$cardano_test_last_version ]]; then
      ./scripts/add-from-github.sh https://github.com/input-output-hk/ouroboros-consensus $(cd ..; git rev-list -n 1 release-diffusion-$diffusion_last_version) ouroboros-consensus-diffusion
  fi

  echo "I have created a branch named \"release-consensus\" here: $(pwd)"
  echo "Push it to the remote and open a PR."
  echo ""
  echo "  git push --set-upstream origin release-consensus"
)
