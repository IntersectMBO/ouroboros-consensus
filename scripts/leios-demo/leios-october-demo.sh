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

# Shape traffic

IF=lo
RATE_MBIT=20          # Target bandwidth (Mbit/s)
DELAY_MS=100          # Target one-way delay (ms)
OVERHEAD_FACTOR=1.2   # Safety margin (20% extra for bursts/jitter)
MSS=1460              # Typical TCP MSS (1500 - 40 IP+TCP headers)

RATE_HI_MBIT=$(echo "1.05 * $RATE_MBIT" | bc)
RATE_HI_MBIT=${RATE_HI_MBIT%.*}
RTT_SEC=$(echo "2 * $DELAY_MS / 1000" | bc -l)
RATE_BPS=$(echo "$RATE_MBIT * 1000000" | bc)
BDP_BYTES=$(echo "$RATE_BPS * $RTT_SEC / 8 * $OVERHEAD_FACTOR" | bc -l)
BDP_BYTES=${BDP_BYTES%.*}  # truncate

MAX_BDP=$((4 * 1024 * 1024))
[[ $BDP_BYTES -gt $MAX_BDP ]] && BDP_BYTES=$MAX_BDP

SOCK_BUF_DEFAULT=$BDP_BYTES
SOCK_BUF_MAX=$((BDP_BYTES * 2))

PAGE_SIZE=4096
SOCK_BUF_DEFAULT=$(((SOCK_BUF_DEFAULT + PAGE_SIZE - 1) / PAGE_SIZE * PAGE_SIZE))
SOCK_BUF_MAX=$(((SOCK_BUF_MAX + PAGE_SIZE - 1) / PAGE_SIZE * PAGE_SIZE))

BDP_PKTS=$(echo "$BDP_BYTES / $MSS" | bc)
CODEL_LIMIT=$BDP_PKTS
CODEL_LIMIT=$((CODEL_LIMIT < 50 ? 50 : CODEL_LIMIT))
CODEL_LIMIT=$((CODEL_LIMIT > 1000 ? 1000 : CODEL_LIMIT))

cat <<EOF
Target ceil: ${RATE_HI_MBIT}mbit/s
Target rate: ${RATE_MBIT}mbit/s
RTT: ${RTT_SEC}s | BDP: ${BDP_BYTES} bytes (${BDP_PKTS} packets)
Socket buffers: default=${SOCK_BUF_DEFAULT}B, max=${SOCK_BUF_MAX}B
codel limit: ${CODEL_LIMIT} packets
EOF

# test w/o first
# apply_buffers() {
#     echo "Applying sysctl buffer settings..."
#     sysctl -q \
#         net.core.rmem_default=$SOCK_BUF_DEFAULT \
#         net.core.rmem_max=$SOCK_BUF_MAX \
#         net.core.wmem_default=$SOCK_BUF_DEFAULT \
#         net.core.wmem_max=$SOCK_BUF_MAX \
#         net.ipv4.tcp_rmem="4096 $SOCK_BUF_DEFAULT $SOCK_BUF_MAX" \
#         net.ipv4.tcp_wmem="4096 $SOCK_BUF_DEFAULT $SOCK_BUF_MAX" \
#         net.ipv4.tcp_no_metrics_save=1
# }

sudo tc qdisc del dev $IF root 2>/dev/null || true
sudo tc qdisc del dev $IF ingress 2>/dev/null || true
sudo ip link del ifb0 2>/dev/null || true

sudo ip link set lo mtu 1500

sudo ip link add ifb0 type ifb
sudo ip link set ifb0 up

sudo tc qdisc add dev $IF root handle 1: htb default 1
sudo tc class add dev $IF parent 1: classid 1:1 \
     htb rate ${RATE_MBIT}mbit ceil ${RATE_HI_MBIT}mbit quantum 10000 burst $BDP_BYTES
sudo tc qdisc add dev $IF parent 1:1 fq_codel limit $CODEL_LIMIT \
     quantum 10000 \
     target 5ms \
     interval 100ms
sudo tc qdisc add dev $IF handle ffff: ingress
sudo tc filter add dev $IF parent ffff: \
    protocol ip \
    flower ip_proto tcp dst_port 5200-5202 \
    action mirred egress redirect dev ifb0
sudo tc filter add dev $IF parent ffff: \
    protocol ip \
    flower ip_proto tcp src_port 5200-5202 \
    action mirred egress redirect dev ifb0
sudo tc qdisc add dev ifb0 root netem delay ${DELAY_MS}ms limit 10000

cleanup_network() {
    sudo ip link del ifb0 2>/dev/null
    sudo tc qdisc del dev $IF ingress 2>/dev/null
    sudo tc qdisc del dev $IF root 2>/dev/null
    sudo ip link set lo mtu 65536
}
trap cleanup_network EXIT INT TERM

TMP_DIR=$(mktemp -d ${TMPDIR:-/tmp}/leios-october-demo.XXXXXX)
echo "Using temporary directory for DB and logs: $TMP_DIR"

rm -f ./leios-run-tmp-dir
ln -s "$TMP_DIR" ./leios-run-tmp-dir

pushd $(dirname "$CARDANO_NODE") > /dev/null

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
          "port": 5200
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

CARDANO_NODE_CMD="env LEIOS_DB_PATH=$TMP_DIR/node-0/leios.db
    ${CARDANO_NODE} run
    --config $CLUSTER_RUN_DATA/leios-node/config.json
    --topology topology-node-0.json
    --database-path $TMP_DIR/node-0/db
    --socket-path node-0.socket
    --port 5201"

echo "node-0: $CARDANO_NODE_CMD"

$CARDANO_NODE_CMD 1> "$TMP_DIR/cardano-node-0.log" 2>"$TMP_DIR/cardano-node-0.stderr" &

CARDANO_NODE_0_PID=$!

cleanup_node_0() {
    sudo kill $CARDANO_NODE_0_PID
    cleanup_network
}

trap cleanup_node_0 EXIT INT TERM

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
          "port": 5201
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

DOWNSTREAM_PEER_CMD="env LEIOS_DB_PATH=$TMP_DIR/node-1/leios.db
    ${CARDANO_NODE} run
    --config $CLUSTER_RUN_DATA/leios-node/config.json
    --topology topology-node-1.json
    --database-path $TMP_DIR/node-1/db
    --socket-path node-1.socket
    --port 5202"

echo "downstream: $DOWNSTREAM_PEER_CMD"

$DOWNSTREAM_PEER_CMD 1>"$TMP_DIR/cardano-node-1.log" 2>"$TMP_DIR/cardano-node-1.stderr" &

DOWNSTREAM_PEER_PID=$!

cleanup_node_1() {
    sudo kill $DOWNSTREAM_PEER_PID
    cleanup_node_0
}

trap cleanup_node_1 EXIT INT TERM

echo "Cardano node 1 started with PID: $DOWNSTREAM_PEER_PID"

# Return to the original directory
popd > /dev/null

##
## Run immdb-server
##

UPSTREAM_PEER_CMD="${IMMDB_SERVER}
    --db $CLUSTER_RUN_DATA/immdb-node/immutable/
    --config $CLUSTER_RUN_DATA/immdb-node/config.json
    --initial-slot $REF_SLOT
    --initial-time $ONSET_OF_REF_SLOT
    --leios-schedule $LEIOS_SCHEDULE
    --leios-db $LEIOS_UPSTREAM_DB_PATH
    --port 5200"

echo "upstream: $UPSTREAM_PEER_CMD"

$UPSTREAM_PEER_CMD &> "$TMP_DIR/immdb-server.log" &

UPSTREAM_PEER_PID=$!

cleanup_immdb() {
    sudo kill $UPSTREAM_PEER_PID
    cleanup_node_1
}

trap cleanup_immdb EXIT INT TERM

echo "ImmDB server started with PID: $UPSTREAM_PEER_PID"

TIMEOUT=${TIMEOUT:-25}
read -t "$(($TIMEOUT + $SECONDS_UNTIL_REF_SLOT))" -n 1 -s -r -p "Press any key to stop the spawned processes, or just wait ~$SECONDS_UNTIL_REF_SLOT + $TIMEOUT seconds..."
echo

echo "Temporary data stored at: $TMP_DIR"

echo "$(date) Tearing down processes $UPSTREAM_PEER_PID (upstream peer, aka immdb-server), $CARDANO_NODE_0_PID (node-0), and $DOWNSTREAM_PEER_PID (downstream peer, aka node-1)..."
cleanup_immdb

# Log analysis

cat $TMP_DIR/cardano-node-0.log >logA
cat $TMP_DIR/cardano-node-1.log >logB

python3 scripts/leios-demo/log_parser.py $REF_SLOT $ONSET_OF_REF_SLOT logA logB "scatter_plot.png"

# Status

sleep 1s

echo
echo Any processes still running:
ps -aux | grep -e '[S]TART' -e '[c]ardano-node' -e '[i]mmdb' | cut -c-180

echo "(Hopefully there were none!)"
echo

exit 0
