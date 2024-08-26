#!/usr/bin/env bash
set -eux

# NUM_SLOTS=1000000 run-syncing-node.sh <N>
#
# Runs the syncing node with a topology with <N> peers on ports 23000, 23001, etc
#
# NUM_SLOTS tells how many slots to sync up. The default value is 50000.
#

N=$1
NODE_DIR=$PWD/syncing-node
NODE_DB=$PWD/db-mainnet-truncated
mkdir -p $NODE_DIR/logs

CONFIG_YAML=$NODE_DIR/mainnet-config.yaml
cp cardano-node/configuration/cardano/mainnet-config.yaml $CONFIG_YAML
cp cardano-node/configuration/cardano/*.json $NODE_DIR
echo TraceOptionPeerFrequency: 5000 >> $CONFIG_YAML
echo ConwayGenesisFile: shelley_qa-conway-genesis.json >> $CONFIG_YAML
sed -i 's/PeerSharing: .*//g' $CONFIG_YAML
if [ -v ENABLE_GENESIS ]
then
   echo EnableGenesis: true >> $CONFIG_YAML
fi
if [ -v ENABLE_FULL_TRACING ]
then
cat <<END >> $CONFIG_YAML
UseTraceDispatcher: True

TraceOptions:
  "":
    severity: Debug
    detail: DNormal
    backends:
      - Stdout MachineFormat

TraceOptionPeerFrequency: 5000
END
fi
TOPOLOGY_JSON=$NODE_DIR/topology.json

truncate_chaindb() {
  echo truncating chain db ...
  rm -rf $NODE_DB/volatile

  mkdir -p $NODE_DB/ledger
  rm -rf $NODE_DB/ledger/*
  cp 100007913_db-analyser $NODE_DB/ledger/100007913

  set +x
  for i in $(seq -f "%05g" 4630 99999); do
      if [ ! -f $NODE_DB/immutable/"$i.chunk" ]; then
          break
      fi
      for t in primary secondary chunk; do
          rm -f $NODE_DB/immutable/"$i.$t"
      done
  done
  set -x
}

generate_topology_json_p2p() {
  local OUTPUT=$1
  local TOPOLOGY_ARR=("{\"address\": \"127.0.0.1\", \"port\": 23000}")
  for((i=1;i<$N;i++))
  do
    TOPOLOGY_ARR+=(", {\"address\": \"127.0.0.1\", \"port\": $((i+23000))}")
  done
  echo '{"publicRoots": [], "localRoots": [ { "accessPoints": [' ${TOPOLOGY_ARR[*]}'], "advertise": false, "hotValency": ' $N', "trustable": true}]}' > $OUTPUT
}

generate_topology_json_legacy() {
  local OUTPUT=$1
  local TOPOLOGY_ARR=("{\"addr\":\"127.0.0.1\",\"port\":23000,\"valency\":1}")
  for((i=1;i<$N;i++))
  do
    TOPOLOGY_ARR+=(",{\"addr\":\"127.0.0.1\",\"port\":$((i+23000)),\"valency\":1}")
  done
  echo '{"localRoots": [], "publicRoots": [], "Producers": [' ${TOPOLOGY_ARR[*]} ']}' > $OUTPUT
}

START_SLOT=100007913
if [ -v SYNC_FROM_0 ]
then
    START_SLOT=0
    rm -rf $NODE_DB
else
    truncate_chaindb
fi
generate_topology_json_p2p $TOPOLOGY_JSON

CABAL_FLAGS=${CABAL_FLAGS:-}

(cd cardano-node; nix develop .#project.x86_64-linux.projectVariants.ghc96.profiled.shell -c bash -c "cabal build $CABAL_FLAGS cardano-node:exe:cardano-node && time cabal run $CABAL_FLAGS cardano-node:exe:cardano-node -- \
    run \
    --config $CONFIG_YAML \
    --database-path $NODE_DB \
    --topology $TOPOLOGY_JSON \
    --host-addr 0.0.0.0 --port 3002 \
    --socket-path $NODE_DIR/node.socket \
    --shutdown-on-slot-synced $((${START_SLOT} + ${NUM_SLOTS:-50000})) \
    +RTS -s ${CARDANO_NODE_RTS_FLAGS:-} \
   | tee $NODE_DIR/logs/sync-$(date -Iseconds).json" \
)
