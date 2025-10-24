#!/bin/bash

# The only parameter should be the path to the folder
# where the data of a benchmarking cluster run is stored
# (CLUSTER_RUN_DATA directory).

# P&T cluster run data
# Safely remove trailing slash if present
CLUSTER_RUN_DATA="${1%/}"

if [ "$#" -ne 1 ]; then
    echo "Error: Please provide one parameter, the path to the cluster run data directory." >&2
    exit 1
fi

if [ ! -d "$CARDANO_NODE_PATH" ]; then
    echo "Error: Cardano node path '$CARDANO_NODE_PATH' not found or is not a directory." >&2
    exit 1
fi

if [ ! -d "$CLUSTER_RUN_DATA" ]; then
    echo "Error: CLUSTER_RUN_DATA directory '$CLUSTER_RUN_DATA' not found or is not a directory." >&2
    exit 1
fi

if [[ -z "${cardano_node}" ]]; then
    echo "Error: Please set \${cardano_node} to the path to the cardano-node exe." >&2
    exit 1
fi

if [[ -z "${immdb_server}" ]]; then
    echo "Error: Please set \${immdb_server} to the path to the immdb-server exe." >&2
    exit 1
fi

if [[ -z "${REF_SLOT}" ]]; then
    echo "Error: Please set \${REF_SLOT} to the reference slot." >&2
    exit 1
fi

if [[ -z "$LEIOS_SCHEDULE_PATH" ]]; then
    echo "Error: Please set \${LEIOS_SCHEDULE_PATH} to path to the Leios schedule file." >&2
    exit 1
fi

TMP_DIR=$(mktemp -d)
echo "Using temporary directory for DB and logs: $TMP_DIR"

pushd "$CARDANO_NODE_PATH" > /dev/null

##
## Run cardano-node (node-0)
##

echo "Creating topology-node-0.json in $(TMP_DIR)"
cat << EOF > "${TMP_DIR}/topology-node-0.json"
{
  "bootstrapPeers": [],
  "localRoots": [
    {
      "accessPoints": [
        {
          "address": "127.0.0.1",
          "port": 3001
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

CARDANO_NODE_CMD="${cardano_node} run \
    --config $CLUSTER_RUN_DATA/node-0/config.json \
    --topology ${TMP_DIR}/topology-node-0.json \
    --database-path $TMP_DIR/node-0/db \
    --socket-path $TMP_DIR/node-0.socket \
    --host-addr 0.0.0.0 --port 3002"

echo "Command: $CARDANO_NODE_CMD &> $TMP_DIR/cardano-node-0.log &"

$CARDANO_NODE_CMD &> "$TMP_DIR/cardano-node-0.log" &

CARDANO_NODE_0_PID=$!

echo "Cardano node 0 started with PID: $CARDANO_NODE_0_PID"

##
## Run a second Cardano-node (To be eventually replaced by a mocked downstream node)
##

cat << EOF > "${TMP_DIR}/topology-node-1.json"
{
  "bootstrapPeers": [],
  "localRoots": [
    {
      "accessPoints": [
        {
          "address": "127.0.0.1",
          "port": 3002
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

MOCKED_PEER_CMD="${cardano-node} run \
    --config $CLUSTER_RUN_DATA/node-0/config.json \
    --topology ${TMP_DIR}/topology-node-1.json \
    --database-path $TMP_DIR/node-1/db \
    --socket-path $TMP_DIR/node-1.socket \
    --host-addr 0.0.0.0 --port 3003"

echo "Command (Node 1): $MOCKED_PEER_CMD &> $TMP_DIR/cardano-node-1.log &"

$MOCKED_PEER_CMD &> "$TMP_DIR/cardano-node-1.log" &

MOCKED_PEER_PID=$!

echo "Cardano node 1 started with PID: $MOCKED_PEER_PID"

# Return to the original directory
popd > /dev/null

##
## Run immdb-server
##

## TODO: we should find a better way to wait for the nodes to be started
# Calculate the POSIX time 5 seconds from now.
REF_TIME_FOR_SLOT=$(( $(date +%s) + 5 ))

IMMDB_CMD_CORE="${immdb_server} \
    -- --db $CLUSTER_RUN_DATA/node-0/db/immutable/ \
    --config $CLUSTER_RUN_DATA/node-0/config.json \
    --initial-slot $REF_SLOT \
    --initial-time $REF_TIME_FOR_SLOT \
    --leios-schedule $LEIOS_SCHEDULE_PATH \
"

echo "Command: $IMMDB_CMD_CORE &> $TMP_DIR/immdb-server.log &"

$IMMDB_CMD_CORE &> "$TMP_DIR/immdb-server.log" &

IMMDB_SERVER_PID=$!

echo "ImmDB server started with PID: $IMMDB_SERVER_PID"


# TODO: we should change the condition on which we terminate the demo.
echo "Sleeping..."
sleep 120

echo "Killing processes $IMMDB_SERVER_PID (immdb-server), $CARDANO_NODE_0_PID (node-0), and $MOCKED_PEER_PID (node-1)..."

kill "$IMMDB_SERVER_PID" 2>/dev/null || true

# Use negative PID to target the process group ID and SIGKILL for cardano-node processes.
kill -9 -"$CARDANO_NODE_0_PID" 2>/dev/null || true
kill -9 -"$MOCKED_PEER_PID" 2>/dev/null || true

echo "Temporary data stored at: $TMP_DIR"

# # Log analysis

# VENV_PATH="./scripts/leios-demo/venv"

# # 1. Activate the Python Virtual Environment
# if [ -f "$VENV_PATH/bin/activate" ]; then
#     echo "Activating virtual environment..."
#     # 'source' must be used for activation to modify the current shell environment
#     source "$VENV_PATH/bin/activate"
# else
#     echo "Error: Virtual environment activation script not found at $VENV_PATH/bin/activate." >&2
# fi

# python3 scripts/leios-demo/log_parser.py $TMP_DIR/cardano-node-0.log $TMP_DIR/cardano-node-1.log

# # 2. Deactivate the Python Virtual Environment before exiting
# deactivate 2>/dev/null || true

# exit 0
