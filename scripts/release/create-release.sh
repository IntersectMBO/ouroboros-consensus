#!/usr/bin/env bash

# Updates the version for ouroboros-consensus packages depending on the entries
# in the changelog directory

function increment_version {
  local delimiter=.
  local array=($(echo "$1" | tr $delimiter '\n'))
  array[$2]=$((array[$2]+1))
  for i in $(seq $(($2 + 1)) 3); do
    array[$i]=0
  done
  echo $(local IFS=$delimiter ; echo "${array[*]}")
}

function last_version {
  grep "<a id=" $1/CHANGELOG.md  | cut -d\' -f2 | cut -d- -f2 | head -n1
}

function compute_new_version {
  ch=$(cat ./$1/changelog.d/*.md | python3 ./scripts/release/strip-md-comments.py)
  if [[ $(echo "$ch" | grep "### Breaking") ]]; then
      increment_version $2  1
  elif [[ $(echo "$ch" | grep "### Non-Breaking") ]]; then
      increment_version $2  2
  elif [[ $(echo "$ch" | grep "### Patch") ]]; then
      increment_version $2  3
  fi
}

if [[ $(ls ouroboros-consensus/changelog.d ouroboros-consensus-cardano/changelog.d ouroboros-consensus-protocol/changelog.d ouroboros-consensus-diffusion/changelog.d | wc -l) == 11 ]]; then
    echo "No changelog fragments. No need to do a new release"
    exit 1
fi

consensus_last_version=$(last_version ouroboros-consensus)
protocol_last_version=$(last_version ouroboros-consensus-protocol)
cardano_last_version=$(last_version ouroboros-consensus-cardano)
diffusion_last_version=$(last_version ouroboros-consensus-diffusion)

echo "Preparing a release for:"
if [[ $(ls ouroboros-consensus/changelog.d | wc -l) != 1 ]]; then
    consensus_new_version=$(compute_new_version ouroboros-consensus $consensus_last_version)
    echo "- consensus $consensus_last_version -> $consensus_new_version"
fi

if [[ $(ls ouroboros-consensus-protocol/changelog.d | wc -l) != 1 ]]; then
    protocol_new_version=$(compute_new_version ouroboros-consensus-protocol $protocol_last_version)
    echo "- protocol  $protocol_last_version -> $protocol_new_version"
fi

if [[ $(ls ouroboros-consensus-cardano/changelog.d | wc -l) != 1 ]]; then
    cardano_new_version=$(compute_new_version ouroboros-consensus-cardano $cardano_last_version)
    echo "- cardano   $cardano_last_version -> $cardano_new_version"
fi

if [[ $(ls ouroboros-consensus-diffusion/changelog.d | wc -l) != 1 ]]; then
    diffusion_new_version=$(compute_new_version ouroboros-consensus-diffusion $diffusion_last_version)
    echo "- diffusion $diffusion_last_version -> $diffusion_new_version"
fi

function release_num {
  if [[ -n $2 ]]; then
      echo "$1-$2/"
  else
      echo ""
  fi
}

printf "\n"

################################################################################
## Create git branch
################################################################################

if [[ -z $cardano_new_version && -z $consensus_new_version && -z $protocol_new_version && -z $diffusion_new_version ]]; then
    echo "Nothing to update"
    exit 1
fi

branch="rel/"
branch+="$(release_num "co" $consensus_new_version)"
branch+="$(release_num "p" $protocol_new_version)"
branch+="$(release_num "ca" $cardano_new_version)"
branch+="$(release_num "d" $diffusion_new_version)"
printf "Creating branch %s\n\n" ${branch%?}
# remove last slash
git checkout -b ${branch%?} >/dev/null 2>&1

################################################################################
## Update cabal files, update changelogs and create commits
################################################################################

if [[ -n $consensus_new_version ]]; then
    echo "Updating ouroboros-consensus package"

    sed -E -i "/^version:/ s/$consensus_last_version/$consensus_new_version/g" ouroboros-consensus/ouroboros-consensus.cabal

    echo "- Updating changelog"

    ( cd ouroboros-consensus
      scriv collect >/dev/null 2>&1
    )

    echo "- Committing changes"
    git add -A
    git commit -m "Release consensus-$consensus_new_version" >/dev/null 2>&1
fi

function replace_caret_up_to {
  packages=$1
  old_ver=$2
  new_ver=$3
  up_to=$4
  cabal_file=$5
  if [[ -n $new_ver ]]; then
      # update caret dep bounds for packages in ouroboros-consensus
      regex=$(echo "$packages" | tr '\n' '|')
      sed -E -i "/${regex%?}/ s/$(echo $old_ver | cut -d'.' -f1-$up_to)/$(echo $new_ver | cut -d'.' -f1-$up_to)/g" $cabal_file
  fi
}

if [[ -n $protocol_new_version ]]; then
    echo "Updating ouroboros-consensus-protocol package"

    sed -E -i "/^version:/ s/$protocol_last_version/$protocol_new_version/g" ouroboros-consensus-protocol/ouroboros-consensus-protocol.cabal

    replace_caret_up_to \
      ouroboros-consensus  \
      "$consensus_last_version" \
      "$consensus_new_version" \
      2                      \
      ouroboros-consensus-protocol/ouroboros-consensus-protocol.cabal

    echo "- Updating changelog"

    ( cd ouroboros-consensus-protocol
      scriv collect >/dev/null 2>&1
    )

    echo "- Committing changes"
    git add -A
    git commit -m "Release protocol-$protocol_new_version" >/dev/null 2>&1
fi

if [[ -n $cardano_new_version ]]; then
    echo "Updating ouroboros-consensus-cardano package"
    sed -E -i "/^version:/ s/$cardano_last_version/$cardano_new_version/g" ouroboros-consensus-cardano/ouroboros-consensus-cardano.cabal

    replace_caret_up_to \
        ouroboros-consensus  \
        "$consensus_last_version" \
        "$consensus_new_version" \
        2                      \
        ouroboros-consensus-cardano/ouroboros-consensus-cardano.cabal

    replace_caret_up_to \
        ouroboros-consensus-protocol  \
        "$protocol_last_version" \
        "$protocol_new_version" \
        2                           \
        ouroboros-consensus-cardano/ouroboros-consensus-cardano.cabal

    echo "- Updating changelog"
    ( cd ouroboros-consensus-cardano
      scriv collect >/dev/null 2>&1
    )

    echo "- Committing changes"
    git add -A
    git commit -m "Release cardano-$cardano_new_version" >/dev/null 2>&1
fi

if [[ -n $diffusion_new_version ]]; then
    echo "Updating ouroboros-consensus-diffusion package"

    sed -E -i "/^version:/ s/$diffusion_last_version/$diffusion_new_version/g" ouroboros-consensus-diffusion/ouroboros-consensus-diffusion.cabal

    replace_caret_up_to \
      ouroboros-consensus  \
      "$consensus_last_version" \
      "$consensus_new_version" \
      2                    \
      ouroboros-consensus-diffusion/ouroboros-consensus-diffusion.cabal

    echo "- Updating changelog"
    ( cd ouroboros-consensus-diffusion
      scriv collect >/dev/null 2>&1
    )

    echo "- Committing changes"
    git add -A
    git commit -m "Release diffusion-$diffusion_new_version" >/dev/null 2>&1
fi

echo "Checking that the versions are consistent with the contents of the changelog directories"
./scripts/ci/check-changelogs.sh

echo "Succesfully created release. You should now inspect the current branch (${branch%?}) and if everything looks right, push it to GitHub, open a PR and get it merged!"
