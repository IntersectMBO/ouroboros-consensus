#!/bin/bash

if [[ ! "$SECONDS_UNTIL_REF_SLOT" =~ ^[0-9]*$ ]] || [[ "$SECONDS_UNTIL_REF_SLOT" -le 0 ]]; then
     echo "Error: \${SECONDS_UNTIL_REF_SLOT} must be a positive integer of seconds, which will be added to the execution time of this script." >&2
     exit 1
fi

if [ ! -d "${CLUSTER_RUN_DATA%/}" ]; then
    CLUSTER_RUN_DATA="${CLUSTER_RUN_DATA%/}"
    echo "Error: CLUSTER_RUN_DATA directory '$CLUSTER_RUN_DATA' not found or is not a directory." >&2
    exit 1
fi

if [[ -z "${CARDANO_NODE}" ]]; then
    echo "Error: \${CARDANO_NODE} must be the path to the cardano-node exe." >&2
    exit 1
fi

if [[ -z "${IMMDB_SERVER}" ]]; then
    echo "Error: \${IMMDB_SERVER} must be the path to the immdb-server exe." >&2
    exit 1
fi

if [[ -z "${LEIOS_SCHEDULE}" ]]; then
    echo "Error: \${LEIOS_SCHEDULE} must be the path to the JSON file that lists the schedule of Leios offers." >&2
    exit 1
fi

if [[ -z "${LEIOS_UPSTREAM_DB_PATH}" ]]; then
    echo "Error: \${LEIOS_UPSTREAM_DB_PATH} must be the path to the source Leios database." >&2
    exit 1
fi

if [[ -z "${REF_SLOT}" ]] || [[ ! "$REF_SLOT" =~ ^[0-9]*$ ]] || [[ "$REF_SLOT" -lt 0 ]]; then
     echo "Error: \${REF_SLOT} must be a non-negative integer, a slot number" >&2
     exit 1
fi

now=$(date +%s)
ONSET_OF_REF_SLOT=$(( $now + ${SECONDS_UNTIL_REF_SLOT} ))
echo "REF_SLOT=$REF_SLOT"
echo "ONSET_OF_REF_SLOT=$ONSET_OF_REF_SLOT"
echo "$REF_SLOT" >ref_slot
echo "$ONSET_OF_REF_SLOT" >onset_of_ref_slot

# arbitrary choices

PORT1=3001
PORT2=3002
PORT3=3003

echo "Ports: ${PORT1} ${PORT2} ${PORT3}"

TMP_DIR=$(mktemp -d ${TMPDIR:-/tmp}/leios-october-demo.XXXXXX)
echo "Using temporary directory for DB and logs: $TMP_DIR"

rm -f ./leios-run-tmp-dir
ln -s "$TMP_DIR" ./leios-run-tmp-dir

pushd "$CARDANO_NODE_PATH" > /dev/null

##
## Run cardano-node (node-0)
##

echo "Creating topology-node-0.json in $(pwd)"
cat << EOF > topology-node-0.json
{
  "bootstrapPeers": [],
  "localRoots": [
    {
      "accessPoints": [
        {
          "address": "127.0.0.1",
          "port": ${PORT1}
        }
      ],
      "advertise": false,
      "trustable": true,
      "valency": 1
    }
  ],
  "publicRoots": []
}
EOF

mkdir -p "$TMP_DIR/node-0/db"

cp "$LEIOS_UPSTREAM_DB_PATH" "$TMP_DIR/node-0/leios.db"
sqlite3 "$TMP_DIR/node-0/leios.db" 'DELETE FROM ebTxs; DELETE FROM txCache; DELETE FROM ebPoints; VACUUM'

CARDANO_NODE_CMD="env LEIOS_DB_PATH=$TMP_DIR/node-0/leios.db \
    ${CARDANO_NODE} run \
    --config $CLUSTER_RUN_DATA/leios-node/config.json \
    --topology topology-node-0.json \
    --database-path $TMP_DIR/node-0/db \
    --socket-path node-0.socket \
    --host-addr 127.0.0.1 --port ${PORT2}"

echo "Command (Node 0): $CARDANO_NODE_CMD &> $TMP_DIR/cardano-node-0.log &"

$CARDANO_NODE_CMD &> "$TMP_DIR/cardano-node-0.log" &

CARDANO_NODE_0_PID=$!

echo "Cardano node 0 started with PID: $CARDANO_NODE_0_PID"

##
## Run a second Cardano-node (To be eventually replaced by a mocked downstream node)
##

cat << EOF > topology-node-1.json
{
  "bootstrapPeers": [],
  "localRoots": [
    {
      "accessPoints": [
        {
          "address": "127.0.0.1",
          "port": ${PORT2}
        }
      ],
      "advertise": false,
      "trustable": true,
      "valency": 1
    }
  ],
  "publicRoots": []
}
EOF

mkdir -p "$TMP_DIR/node-1/db"

cp "$LEIOS_UPSTREAM_DB_PATH" "$TMP_DIR/node-1/leios.db"
sqlite3 "$TMP_DIR/node-1/leios.db" 'DELETE FROM ebTxs; DELETE FROM txCache; DELETE FROM ebPoints; VACUUM;'

MOCKED_PEER_CMD="env LEIOS_DB_PATH=$TMP_DIR/node-1/leios.db \
    ${CARDANO_NODE} run \
    --config $CLUSTER_RUN_DATA/leios-node/config.json \
    --topology topology-node-1.json \
    --database-path $TMP_DIR/node-1/db \
    --socket-path node-1.socket \
    --host-addr 127.0.0.1 --port ${PORT3}"

echo "Command (Node 1): $MOCKED_PEER_CMD &> $TMP_DIR/cardano-node-1.log &"

$MOCKED_PEER_CMD &> "$TMP_DIR/cardano-node-1.log" &

MOCKED_PEER_PID=$!

echo "Cardano node 1 started with PID: $MOCKED_PEER_PID"

# Return to the original directory
popd > /dev/null

##
## Run immdb-server
##

IMMDB_CMD_CORE="${IMMDB_SERVER} \
    --db $CLUSTER_RUN_DATA/immdb-node/immutable/ \
    --config $CLUSTER_RUN_DATA/immdb-node/config.json \
    --initial-slot $REF_SLOT \
    --initial-time $ONSET_OF_REF_SLOT
    --leios-schedule $LEIOS_SCHEDULE
    --leios-db $LEIOS_UPSTREAM_DB_PATH
    --port ${PORT1}"

echo "Command: $IMMDB_CMD_CORE &> $TMP_DIR/immdb-server.log &"

$IMMDB_CMD_CORE &> "$TMP_DIR/immdb-server.log" &

IMMDB_SERVER_PID=$!

echo "ImmDB server started with PID: $IMMDB_SERVER_PID"

read -n 1 -s -r -p "Press any key to stop the spawned processes..."
echo

echo "Killing processes $IMMDB_SERVER_PID (immdb-server), $CARDANO_NODE_0_PID (node-0), and $MOCKED_PEER_PID (node-1)..."

kill "$IMMDB_SERVER_PID" 2>/dev/null || true

# Use negative PID to target the process group ID and SIGKILL for cardano-node processes.
kill "$CARDANO_NODE_0_PID" 2>/dev/null || true

kill "$MOCKED_PEER_PID" 2>/dev/null || true

echo "Temporary data stored at: $TMP_DIR"

# Log analysis

cat $TMP_DIR/cardano-node-0.log | grep -v -i -e leios >logA
cat $TMP_DIR/cardano-node-1.log | grep -v -i -e leios >logB

python3 ouroboros-consensus/scripts/leios-demo/log_parser.py \
        $REF_SLOT $ONSET_OF_REF_SLOT \
        logA logB \
        "scatter_plot.png"

# Status

echo
echo Any processes still running:
ps -aux | grep -e '[c]ardano-node' -e '[i]mmdb' | cut -c-180

exit 0
