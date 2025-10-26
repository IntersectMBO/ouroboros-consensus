#!/bin/bash

now=$(date +%s)

if [[ -z "${NETSTAT_OUTPUT}" ]]; then
    echo "Error: \${NETSTAT_OUTPUT} must be the path to the stdout of a recent call to netstat -lntp." >&2
    exit 1
fi

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

if [[ -z "${LEIOS_DB}" ]]; then
    echo "Error: \${LEIOS_DB} must be the path to an exist Leios database." >&2
    exit 1
fi

if [[ -z "${REF_SLOT}" ]] || [[ ! "$REF_SLOT" =~ ^[0-9]*$ ]] || [[ "$REF_SLOT" -lt 0 ]]; then
     echo "Error: \${REF_SLOT} must be a non-negative integer, a slot number" >&2
     exit 1
fi

find_random_unused_port() {
  local port
  local min_port=1024  # Start checking from non-privileged ports
  local max_port=65535 # Maximum possible port number

  while true; do
    # Generate a random port within the specified range
    port=$(( RANDOM % (max_port - min_port + 1) + min_port ))

    # Check if the port is in use using netstat
    # -l: listening sockets, -t: TCP, -n: numeric addresses, -p: show PID/program name
    # grep -q: quiet mode, exits with 0 if match found, 1 otherwise
    if ! cat ${NETSTAT_OUTPUT} | grep -q ":$port "; then
      echo "$port"
      return 0 # Port found, exit function
    fi
  done
}

PORT1=$(find_random_unused_port)
PORT2=$(find_random_unused_port)
PORT3=$(find_random_unused_port)

echo "Ports: ${PORT1} ${PORT2} ${PORT3}"

TMP_DIR=$(mktemp -d ${TMPDIR:-/tmp}/leios-october-demo.XXXXXX)
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

CARDANO_NODE_CMD="${CARDANO_NODE} run \
    --config $CLUSTER_RUN_DATA/leios-node/config.json \
    --topology topology-node-0.json \
    --database-path $TMP_DIR/node-0/db \
    --socket-path node-0.socket \
    --host-addr 0.0.0.0 --port ${PORT2}"

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

MOCKED_PEER_CMD="cabal run -- cardano-node run \
    --config $CLUSTER_RUN_DATA/leios-node/config.json \
    --topology topology-node-1.json \
    --database-path $TMP_DIR/node-1/db \
    --socket-path node-1.socket \
    --host-addr 0.0.0.0 --port ${PORT3}"

echo "Command (Node 1): $MOCKED_PEER_CMD &> $TMP_DIR/cardano-node-1.log &"

$MOCKED_PEER_CMD &> "$TMP_DIR/cardano-node-1.log" &

MOCKED_PEER_PID=$!

echo "Cardano node 1 started with PID: $MOCKED_PEER_PID"

# Return to the original directory
popd > /dev/null

##
## Run immdb-server
##

ONSET_OF_REF_SLOT=$(( $now + ${SECONDS_UNTIL_REF_SLOT} ))

IMMDB_CMD_CORE="${IMMDB_SERVER} \
    --db $CLUSTER_RUN_DATA/immdb-node/immutable/ \
    --config $CLUSTER_RUN_DATA/immdb-node/config.json \
    --initial-slot $REF_SLOT \
    --initial-time $ONSET_OF_REF_SLOT
    --leios-schedule $LEIOS_SCHEDULE
    --leios-db $LEIOS_DB
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

# python3 scripts/leios-demo/log_parser.py \
#         $REF_SLOT $ONSET_OF_REF_SLOT \
#         $TMP_DIR/cardano-node-0.log $TMP_DIR/cardano-node-1.log \
#         "scatter_plot.png"

# # 2. Deactivate the Python Virtual Environment before exiting
# deactivate 2>/dev/null || true

# exit 0
