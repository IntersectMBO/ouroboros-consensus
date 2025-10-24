#!/bin/bash

# The first parameter should be the path to the local checkout of
# cardano-node.
#
# The second parameter should be the path to the folder
# where the data of a benchmarking cluster run is stored
# (CLUSTER_RUN_DATA directory).

# Local checkout path of the cardano-node repository
# Safely remove trailing slash if present
CARDANO_NODE_PATH="${1%/}"

# P&T cluster run data
CLUSTER_RUN_DATA="${2%/}"

if [ "$#" -ne 2 ]; then
    echo "Error: Please provide two parameters: <cardano-node path> and <CLUSTER_RUN_DATA directory path>." >&2
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

TMP_DIR=$(mktemp -d)
echo "Using temporary directory for DB and logs: $TMP_DIR"

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

CARDANO_NODE_CMD="cabal run -- cardano-node run \
    --config $CLUSTER_RUN_DATA/node-0/config.json \
    --topology topology-node-0.json \
    --database-path $TMP_DIR/node-0/db \
    --socket-path node-0.socket \
    --host-addr 0.0.0.0 --port 3002"

echo "Command: $CARDANO_NODE_CMD &> $TMP_DIR/cardano-node-0.log &"

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

MOCKED_PEER_CMD="cabal run -- cardano-node run \
    --config $CLUSTER_RUN_DATA/node-0/config.json \
    --topology topology-node-1.json \
    --database-path $TMP_DIR/node-1/db \
    --socket-path node-1.socket \
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
# Calculate the POSIX time 60 seconds from now.
REF_TIME_FOR_SLOT=$(( $(date +%s) + 60 ))
INITIAL_SLOT=80

IMMDB_CMD_CORE="cabal run immdb-server \
    -- --db $CLUSTER_RUN_DATA/node-0/db/immutable/ \
    --config $CLUSTER_RUN_DATA/node-0/config.json \
    --initial-slot $INITIAL_SLOT \
    --initial-time $REF_TIME_FOR_SLOT"

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

# Log analysis

VENV_PATH="./scripts/leios-demo/venv"

# 1. Activate the Python Virtual Environment
if [ -f "$VENV_PATH/bin/activate" ]; then
    echo "Activating virtual environment..."
    # 'source' must be used for activation to modify the current shell environment
    source "$VENV_PATH/bin/activate"
else
    echo "Error: Virtual environment activation script not found at $VENV_PATH/bin/activate." >&2
fi

python3 scripts/leios-demo/log_parser.py \
        $INITIAL_SLOT $REF_TIME_FOR_SLOT \
        $TMP_DIR/cardano-node-0.log $TMP_DIR/cardano-node-1.log

# 2. Deactivate the Python Virtual Environment before exiting
deactivate 2>/dev/null || true

exit 0
