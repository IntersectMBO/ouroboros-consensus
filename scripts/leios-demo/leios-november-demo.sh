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

IP1="10.0.0.1" # immdb server / upstream
IP2="10.0.0.2" # node-0
IP3="10.0.0.3" # node-1 downstream
PORT=5200

# scrub
for i in 1 2 3; do { sudo ip netns del ns$i; } 2>/dev/null; done

# create namespaces
for i in 1 2 3; do set -x; sudo ip netns add ns$i; { set +x; } 2>/dev/null; done

# create veth pairs
for i in 1 2 3; do
    IP=IP$i
    set -x
    sudo ip link add veth${i}p netns ns$i type veth peer name veth${i} netns ns2
    sudo ip -n ns$i link set veth${i}p up
    sudo ip -n ns2 link set veth$i up
    sudo ip -n ns$i addr add ${!IP}/24 dev veth${i}p
    { set +x; } 2>/dev/null
done

set -x

# bridge
sudo ip -n ns2 link add name br type bridge
sudo ip -n ns2 link set dev br up

# ifb for modeling buffer bloat
sudo ip -n ns2 link add ifb2 type ifb
sudo ip -n ns2 link set ifb2 up

{ set +x; } 2>/dev/null

# attach to bridge
for i in 1 2 3; do set -x; sudo ip -n ns2 link set veth${i} master br; { set +x; } 2>/dev/null; done

# Shape traffic

RATE_MBIT=20           # Target bandwidth (Mbit/s) per peer
DELAY_MS=100          # Target one-way delay (ms)
OVERHEAD_FACTOR=1.2   # Safety margin (20% extra for bursts/jitter)
MSS=1448              # Typical TCP MSS (1500 - 40 IP+TCP+FLAGS headers)
TOKEN_RATE=250        # TBF replenishment rate (kernel parameter)

RTT_MS=$((2 * $DELAY_MS))
RTT_SEC=$(echo "2 * $DELAY_MS / 1000" | bc -l)
BURST=$(echo "$RATE_MBIT * $OVERHEAD_FACTOR * 1000000 / $TOKEN_RATE / 8" | bc)
RATE_BPS=$(echo "$RATE_MBIT * 1000000" | bc)
BDP_BYTES=$(echo "$RATE_BPS * $RTT_SEC * $OVERHEAD_FACTOR / 8" | bc)

MAX_BDP=$((4 * 1024 * 1024))
[[ $BDP_BYTES -gt $MAX_BDP ]] && BDP_BYTES=$MAX_BDP

SOCK_BUF_DEFAULT=$BDP_BYTES
SOCK_BUF_MAX=$((BDP_BYTES * 2))

PAGE_SIZE=4096
SOCK_BUF_DEFAULT=$(((SOCK_BUF_DEFAULT + PAGE_SIZE - 1) / PAGE_SIZE * PAGE_SIZE))
SOCK_BUF_MAX=$(((SOCK_BUF_MAX + PAGE_SIZE - 1) / PAGE_SIZE * PAGE_SIZE))

BDP_PKTS=$(echo "$BDP_BYTES / $MSS" | bc)

cat <<EOF
Target rate: ${RATE_MBIT}mbit/s
RTT: ${RTT_SEC}s | BDP: ${BDP_BYTES} bytes (${BDP_PKTS} packets)
TBF burst: $BURST bytes
Socket buffers: default=${SOCK_BUF_DEFAULT}B, max=${SOCK_BUF_MAX}B
EOF

# shape egress
for i in 1 2 3; do
    set -x
    sudo tc -n ns$i qdisc add dev veth${i}p root handle 1: tbf \
         rate ${RATE_MBIT}mbit burst $BURST latency 1ms
    sudo tc -n ns$i qdisc add dev veth${i}p parent 1: fq_codel \
         quantum 2000 target 5ms interval 10ms
    # shape netem at bridge port
    # limit sets buffer size
    sudo tc -n ns2 qdisc add dev veth${i} root netem delay ${DELAY_MS}ms
    { set +x; } 2>/dev/null
done

set -x

# model buffer bloat
sudo tc -n ns2 qdisc add dev veth2p handle ffff: ingress
sudo tc -n ns2 filter add dev veth2p parent ffff: \
     protocol ip u32 match u32 0 0 action mirred egress redirect dev ifb2
# sudo tc -n ns2 qdisc add dev ifb2 root netem delay 500ms

# benchmark harness
# run iperf servers
# sudo ip netns exec ns2 iperf3 -s -1 -p 3001 -B $IP2 1>/dev/null &
# sudo ip netns exec ns2 iperf3 -s -1 -p 3003 -B $IP2 1>/dev/null &
# sudo ip netns exec ns1 iperf3 -c $IP2 -p 3001 -t 24 -i 1 --get-server-output > $TMP_DIR/iperf_upstream.log &
# sudo ip netns exec ns3 iperf3 -c $IP2 -p 3003 -t 24 -i 1 --get-server-output > $TMP_DIR/iperf_downstream.log &

{ set +x; } 2>/dev/null

cleanup_netns() {
    for i in 1 2 3; do
        echo "pids in ns${i}:"
        for pid in $(sudo ip netns pids ns$i); do
            echo -n "$pid "
            ps -aux | grep -e $pid
        done
    done
    for i in 1 2 3; do sudo ip netns del ns$i; done

    # no need to cleanup links, since deleting the namespace deletes those
    # links

    # the script itself invokes this handler directly to stop the processes, so
    # reset the SIGNAL handlers
    trap - EXIT INT TERM
}
trap cleanup_netns EXIT INT TERM

TMP_DIR=$(mktemp -d ${TMPDIR:-/tmp}/leios-november-demo.XXXXXX)
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
          "address": "$IP1",
          "port": $PORT
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

CARDANO_NODE_CMD="sudo ip netns exec ns2 env LEIOS_DB_PATH=$TMP_DIR/node-0/leios.db
    ${CARDANO_NODE} run
    --config $CLUSTER_RUN_DATA/leios-node/config.json
    --topology topology-node-0.json
    --database-path $TMP_DIR/node-0/db
    --socket-path node-0.socket
    --host-addr $IP2 --port $PORT"

echo "node-0: $CARDANO_NODE_CMD"

# capture tcp at node-0, it will have streams for both sides
sudo ip netns exec ns2 tcpdump \
  -i veth2p \
  -w ${TMP_DIR}/node_0.pcap \
  -s 0 \
  -n \
  "(host 10.0.0.2 and port 5200) or (icmp and icmp[0] == 3 and icmp[1] == 4)" &

dump_pid=$!

sudo ip netns exec ns2 bash -c "{ while true; do date; ss -i -m -t sport = :5200; sleep 0.1s; done; } 1>${TMP_DIR}/catch-ns2-ss.log 2>&1" &

ss_pid2=$!

$CARDANO_NODE_CMD 1> "$TMP_DIR/cardano-node-0.log" 2>"$TMP_DIR/cardano-node-0.stderr" &

CARDANO_NODE_0_PID=$!

cleanup_node_0() {
    sudo ip netns exec ns2 kill $CARDANO_NODE_0_PID
    sudo ip netns exec ns2 kill $dump_pid
    sudo ip netns exec ns2 kill $ss_pid2
    cleanup_netns
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
          "address": "$IP2",
          "port": $PORT
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

DOWNSTREAM_PEER_CMD="sudo ip netns exec ns3 env LEIOS_DB_PATH=$TMP_DIR/node-1/leios.db
    ${CARDANO_NODE} run
    --config $CLUSTER_RUN_DATA/leios-node/config.json
    --topology topology-node-1.json
    --database-path $TMP_DIR/node-1/db
    --socket-path node-1.socket
    --host-addr $IP3 --port $PORT"

echo "downstream: $DOWNSTREAM_PEER_CMD"

sudo ip netns exec ns3 bash -c "{ while true; do date; ss -i -m -t sport = :5200; sleep 0.1s; done; } 1>${TMP_DIR}/catch-ns3-ss.log 2>&1" &

ss_pid3=$!

$DOWNSTREAM_PEER_CMD 1>"$TMP_DIR/cardano-node-1.log" 2>"$TMP_DIR/cardano-node-1.stderr" &

DOWNSTREAM_PEER_PID=$!

cleanup_node_1() {
    sudo ip netns exec ns2 kill $DOWNSTREAM_PEER_PID
    sudo ip netns exec ns3 kill $ss_pid3
    cleanup_node_0
}

trap cleanup_node_1 EXIT INT TERM

echo "Cardano node 1 started with PID: $DOWNSTREAM_PEER_PID"

# Return to the original directory
popd > /dev/null

##
## Run immdb-server
##

sudo ip netns exec ns1 bash -c "{ while true; do date; ss -i -m -t sport = :5200; sleep 0.1s; done; } 1>${TMP_DIR}/catch-ns1-ss.log 2>&1" &

ss_pid=$!

UPSTREAM_PEER_CMD="sudo ip netns exec ns1 ${IMMDB_SERVER}
    --db $CLUSTER_RUN_DATA/immdb-node/immutable/
    --config $CLUSTER_RUN_DATA/immdb-node/config.json
    --initial-slot $REF_SLOT
    --initial-time $ONSET_OF_REF_SLOT
    --leios-schedule $LEIOS_SCHEDULE
    --leios-db $LEIOS_UPSTREAM_DB_PATH
    --address $IP1
    --port $PORT"

echo "upstream: $UPSTREAM_PEER_CMD"

$UPSTREAM_PEER_CMD &> "$TMP_DIR/immdb-server.log" &

UPSTREAM_PEER_PID=$!

cleanup_immdb() {
    sudo ip netns exec ns1 kill $UPSTREAM_PEER_PID
    sudo ip netns exec ns1 kill $ss_pid
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

echo -e "Send MsgLeiosBlockTxsRequest\tRecv MsgLeiosBlockTxsRequest\tSend MsgLeiosBlockTxs    \tRecv MsgLeiosBlock         \tTxsBytesSize" \
    >$TMP_DIR/LF-req-rsp.txt
paste \
    <(cat $TMP_DIR/cardano-node-0.log | grep -e Send.BlockTxsRequest | jq -r .at) \
    <(cat $TMP_DIR/immdb-server.log | grep -e 'Recv MsgLeiosBlockTxsRequest' | cut -d' ' -f1) \
    <(cat $TMP_DIR/immdb-server.log | grep -e 'Send MsgLeiosBlockTxs' | cut -d' ' -f1) \
    <(cat $TMP_DIR/cardano-node-0.log | grep -e 'Receive.BlockTxs"' | jq -r .at) \
    <(cat $TMP_DIR/cardano-node-0.log | grep -e 'Receive.BlockTxs"' | jq -r .data.msg.txsBytesSize) \
    >>$TMP_DIR/LF-req-rsp.txt

ANALYSIS_CMD="python3 scripts/leios-demo/log_parser.py
        $REF_SLOT $ONSET_OF_REF_SLOT
        ${LEIOS_SCHEDULE}
        $TMP_DIR/immdb-server.log $TMP_DIR/cardano-node-0.log $TMP_DIR/cardano-node-1.log
        scatter_plot.png"
echo "analysis: $ANALYSIS_CMD"
$ANALYSIS_CMD

# Status

sleep 1s

echo
echo Any processes still running:
ps -aux | grep -e '[S]TART' -e '[c]ardano-node' -e '[i]mmdb' | cut -c-180

echo "(Hopefully there were none!)"
echo

exit 0
