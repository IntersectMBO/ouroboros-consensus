#!/usr/bin/env bash
set -eux

# Downloads db-mainnet
# Truncates db-mainnet
# Builds a snapshot of the ledged (takes near 12 hours)
# Builds immdb-server and cardano-node from amesgen/csj branch
# Dowloads toxiproxy

echo colorscheme desert > .vimrc

sh <(curl -L https://nixos.org/nix/install) --daemon --yes

source /etc/bashrc

mkdir -p .config/nix
echo experimental-features = nix-command flakes > .config/nix/nix.conf
mkdir -p .local/share/nix
echo '{"extra-substituters":{"https://cache.iog.io":true},"extra-trusted-public-keys":{"hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=":true}}' > .local/share/nix/trusted-settings.json

sudo bash -c "echo extra-trusted-users = $USER >> /etc/nix/nix.conf"
sudo systemctl restart nix-daemon.service

git clone https://github.com/IntersectMBO/cardano-node -b genesis/benchmarks-rebased

git clone https://github.com/IntersectMBO/ouroboros-consensus -b genesis/benchmarks-rebased

git clone https://github.com/IntersectMBO/ouroboros-network -b blockfetch/milestone-1-rebased

cat << EOF > cardano-node/cabal.project.local
packages:
  ../ouroboros-network/ouroboros-network
  ../ouroboros-network/ouroboros-network-api
  ../ouroboros-network/ouroboros-network-protocols
  ../ouroboros-consensus/ouroboros-consensus
  ../ouroboros-consensus/ouroboros-consensus-cardano
  ../ouroboros-consensus/ouroboros-consensus-diffusion
  ../ouroboros-consensus/ouroboros-consensus-protocol
  ../ouroboros-consensus/sop-extras
  ../ouroboros-consensus/strict-sop-core

program-options
  ghc-options: -Wwarn
EOF

(cd cardano-node; nix develop .#project.x86_64-linux.projectVariants.ghc96.profiled.shell -c bash -c "cabal update; cabal build cardano-node:exe:cardano-node ouroboros-consensus-cardano:exe:immdb-server ouroboros-consensus-cardano:exe:db-analyser")

wget -c https://update-cardano-mainnet.iohk.io/cardano-node-state/db-mainnet.tar.gz -O - | tar -xz

# wget -c http://dl.amesgen.de/tmp/100007913_db-analyser.zst -O - | unzstd > 100007913_db-analyser

# Make a copy of the chain db for the syncing node

NODE_DB=db-mainnet-truncated
cp -r db-mainnet $NODE_DB

# build a snapshot of the ledger at a recent slot

build_ledger_snapshot() {
  local NODE_DIR=$PWD/server-node
  local CONFIG_YAML=$NODE_DIR/mainnet-config.yaml
  local CONFIG_JSON=$NODE_DIR/config.json
  mkdir -p $NODE_DIR
  cp cardano-node/configuration/cardano/mainnet-config.yaml $CONFIG_YAML
  # sed -i 's/EnableP2P: true/EnableP2P: false/g' $CONFIG_YAML
  cp cardano-node/configuration/cardano/*.json $NODE_DIR
  nix-shell -p yaml2json --run "yaml2json < $CONFIG_YAML > $CONFIG_JSON"

  (cd cardano-node; nix develop .#project.x86_64-linux.projectVariants.ghc96.profiled.shell -c bash -c "cabal exec db-analyser -- --store-ledger 100007913 --db ../db-mainnet cardano --config $CONFIG_JSON")
  mv db-mainnet/ledger/100007913_db-analyser 100007913_db-analyser
}

# call build_ledger_snapshot or alternatively download it from friends :)
wget -c https://ramify.amesgen.de/100007913_db-analyser.zst -O - | unzstd > 100007913_db-analyser

# downloading toxiproxy
wget https://github.com/Shopify/toxiproxy/releases/download/v2.9.0/toxiproxy-server-linux-amd64
wget https://github.com/Shopify/toxiproxy/releases/download/v2.9.0/toxiproxy-cli-linux-amd64
chmod +x toxiproxy-cli-linux-amd64 toxiproxy-server-linux-amd64
