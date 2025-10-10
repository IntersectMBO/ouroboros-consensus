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

##
## Run immdb-server
##
IMMDB_CMD_CORE="cabal run immdb-server \
    -- --db $CLUSTER_RUN_DATA/node-0/db/immutable/ \
    --config $CLUSTER_RUN_DATA/node-0/config.json"

echo "Command: $IMMDB_CMD_CORE &> $TMP_DIR/immdb-server.log &"

$IMMDB_CMD_CORE &> "$TMP_DIR/immdb-server.log" &

IMMDB_SERVER_PID=$!

echo "ImmDB server started with PID: $IMMDB_SERVER_PID"

##
## Run cardano-node
##
pushd "$CARDANO_NODE_PATH" > /dev/null

echo "Creating topology.json in $(pwd)"
cat << EOF > topology.json
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

CARDANO_NODE_CMD_CORE="cabal run -- cardano-node run \
    --config $CLUSTER_RUN_DATA/node-0/config.json \
    --topology topology.json \
    --database-path $TMP_DIR/db \
    --socket-path node.socket \
    --host-addr 0.0.0.0 --port 3002"

echo "Command: $CARDANO_NODE_CMD_CORE &> $TMP_DIR/cardano-node.log &"

$CARDANO_NODE_CMD_CORE &> "$TMP_DIR/cardano-node.log" &

CARDANO_NODE_PID=$!

echo "Cardano node started with PID: $CARDANO_NODE_PID"

# Return to the original directory
popd > /dev/null

# TODO: we should change the condition on which we terminate the demo.
echo "Sleeping for 30 seconds"
sleep 30

echo "Killing processes $IMMDB_SERVER_PID (immdb-server) and $CARDANO_NODE_PID (cardano-node)..."

kill "$IMMDB_SERVER_PID" 2>/dev/null || true

# Use negative PID to target the process group ID and SIGKILL.
kill -9 -"$CARDANO_NODE_PID" 2>/dev/null || true

echo "Temporary data stored at: $TMP_DIR"

exit 0
