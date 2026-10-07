#!/usr/bin/env bash
set -eux

# run-server-node.sh <N>
#
# Runs the server node and <N> toxiproxies on ports 23000, 23001, etc
#

N=$1
NODE_DIR=$PWD/server-node
NODE_DB=$PWD/db-mainnet

CONFIG_YAML=$NODE_DIR/mainnet-config.yaml
CONFIG_JSON=$NODE_DIR/config.json

mkdir -p $NODE_DIR
cp cardano-node/configuration/cardano/*.json $NODE_DIR
cp cardano-node/configuration/cardano/mainnet-config.yaml $CONFIG_YAML
echo ConwayGenesisFile: shelley_qa-conway-genesis.json >> $CONFIG_YAML
nix-shell -p yaml2json --run "yaml2json < $CONFIG_YAML > $CONFIG_JSON"

# Running the server node

generate_toxiproxy_config_json() {
  local OUTPUT=$1
  local CONFIG_ARR=("{\"name\": \"genesis_proxy 0\",\"upstream\":\"127.0.0.1:3001\",\"listen\":\"127.0.0.1:23000\",\"enabled\":true}")
  for((i=1;i<$N;i++))
  do
    CONFIG_ARR+=(",{\"name\": \"genesis_proxy $i\",\"upstream\":\"127.0.0.1:3001\",\"listen\":\"127.0.0.1:$((i+23000))\",\"enabled\":true}")
  done
  echo '[' ${CONFIG_ARR[*]} ']' > $OUTPUT
}

TOXIPROXY_CONFIG=$NODE_DIR/toxiproxy_conf.json
generate_toxiproxy_config_json $TOXIPROXY_CONFIG
./toxiproxy-server-linux-amd64 -config $TOXIPROXY_CONFIG &

trap 'kill $(jobs -p)' EXIT

CABAL_FLAGS=${CABAL_FLAGS:-}

(cd cardano-node; nix develop .#project.x86_64-linux.projectVariants.ghc96.profiled.shell -c bash -c "cabal build $CABAL_FLAGS ouroboros-consensus-cardano:exe:immdb-server && echo running immdb-server && cabal run $CABAL_FLAGS ouroboros-consensus-cardano:exe:immdb-server -- \
  --db $NODE_DB/immutable/ \
  --config $CONFIG_JSON \
  --port 3001" \
)
