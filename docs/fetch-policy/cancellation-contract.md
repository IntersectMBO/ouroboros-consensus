# LeiosFetch cancellation and checkpoint contract

Status: agreed fetch-policy contract. This specifies the cancellation,
checkpoint and resource semantics implemented by the source commits that follow
this document. Initial resource limits remain subject to validation and tuning.
The high-level [fine-MVP and atomic policy reference](mvp-fetch-policy.md)
describes the simulator policy; this contract defines production wire ordering,
request ownership and progress handling.

Agreed design direction: retain one typed transaction-batch reply and expose
complete transaction elements incrementally through codec/driver progress hooks.
Use opt-in callback-enabled codec/driver plumbing while preserving other
protocols' behavior. Concrete interfaces live in their implementing modules.

Agreed decisions:

- One transaction wire batch is one checkpoint; pipeline multiple batches.
- Add opt-in progress-enabled codec/driver plumbing in network. Consensus supplies
  request-specific handlers. Aim to retain the existing typed-protocols core
  peer interpreter and `DecodeStep`, and preserve other protocols' driver behavior.
- Accept independently verified whole transactions before the rest of their
  response arrives. A checkpoint's membership and deadline remain fixed; prefix
  delivery does not renew its timer. Body acceptance remains all-or-nothing.
- Reserve bounded storage handoff capacity before issuing a request. Keep
  progress handoff independent of writer-queue backpressure; storage pressure
  defers new admissions. Accepted data remains node-owned across disconnects.
- Extend transaction-write results with confirmed-present offsets and existing
  completed-closure results. Confirmation includes already present transactions,
  not only newly inserted ones, and settlement keeps pruning/lifetime guards.
- Start each upstream's storage handoff at a separate 1 MiB payload budget and
  4,096 transaction-entry slots, counting reservations and unsettled data.
  Coalesce already available verified transactions for an EB into roughly one
  checkpoint's bytes per write, without a batching timer or waiting for the
  network response to finish. Audit and tune these initial bounds.
- Reserve the applicable trusted body-size upper bound for each body request,
  independently of its untrusted announced size. Preserve acceptance of a valid
  body before handling a size misstatement. With a 600,000 B bound and 1 MiB per
  peer, only one such body reservation fits; TX batches may use the remainder.
- Keep an outstanding batch while any of its transactions remain needed,
  subject to normal stall and handoff rules. Cancel it when wholly redundant;
  on abandonment, construct replacements only for transactions still missing.
- Cancellation covers both body requests and transaction batch requests.
- Both request kinds use an explicit `MsgCancelled` result, not an empty payload.
- The original request retains exactly one response slot. The cancellation
  command has its own independent `MsgAckTryCancel` response slot.
- Every reply-bearing client request carries an explicit unsigned 64-bit
  sequence number. Replies use their FIFO position and do not echo the number.
- Number requests consecutively from zero within a LeiosFetch session. Assign
  numbers at admission to the ordered outbound stream; never reuse or wrap them.
- Key local attempts by their session/lifetime and sequence number so that old
  callbacks cannot settle requests in a replacement session.
- Bound each session to `D` data-request records across all EBs. Each record
  reserves capacity for its original exchange and at most one cancellation.
  Retire it after its original reply and, if issued, cancel acknowledgment have
  both arrived. This bounds unresolved exchanges by `W = 2D`.
- Negotiate `D = min(local_advertised_maximum, remote_advertised_maximum)` in
  the existing node-to-node handshake, initially advertising 64. Fix the agreed
  ceiling for the connection lifetime; local admission may use fewer records.
- Keep the current LeiosFetch behavior for version-16 connections. Select the
  new atomic policy only when the new cancellation capability is negotiated.
  Legacy and new-version replies still satisfy shared acquisition obligations.
- Keep retry/backoff state and logical hedge slots per EB. Share per-peer
  admission budgets across all EBs on that connection and preserve cross-EB
  priorities; there is no single node-wide hedge slot.
- Tune byte budgets, pipeline limits and request counts for the intended MVP
  closure sizes. Preserving the old 5 MB/60 MB budgets or 150-request pipeline
  is not a requirement.
- Start with a 1 MiB outstanding-response-byte budget per peer, including
  bodies, `D = 64` data-request records and `W = 128` unresolved exchanges/history
  entries. Ordinary and big-ledger peers receive the same budget. These are
  agreed initial values to validate, not measured final production settings.
- Anchor each deadline at `max(t_head, t_sent + R)`, then add its service
  allowance. Use receiver-observable dispatch and FIFO wire-response completion
  events, not inferred remote queue or congestion-window state.
- Retain a head data reply's fixed deadline until wire completion, even after
  its data is acquired elsewhere. If it blocks the FIFO past that deadline,
  stop appending new work and allow still-needed queued work to be rescued
  within existing limits. Canceling one batch leaves other batches intact;
  cancellation itself does not cause the upstream to stop serving.
- Give a cancel acknowledgment a soft deadline of `t_head + R`, starting only
  when it is the next FIFO reply. Expiry blocks new assignments to that upstream
  and permits bounded rescue of queued missing data, while the wire exchange
  remains open. It neither repeats the cancellation nor increases payload hedge
  backoff or cooldown; the fatal driver guard remains separate.
- Initially obtain RTT information from KeepAlive, sampled at a nominal
  10-second cadence. Use cached information without waiting for a new probe.
  The future mux quantile reader is not an initial production dependency.
- The initial network-to-consensus RTT input is the latest successful KeepAlive
  exchange's RTT and monotonic measurement timestamp, or explicit absence of a
  real measurement. Scope observations to the connection lifetime.
- Start with `R = min(500 ms, 1.5 * measured_RTT)` for a positive KeepAlive
  observation no older than 30 seconds. Use `R = 500 ms` for missing or stale
  input. Snapshot it per attempt; the measured RTT and transport timers remain
  unchanged. These are agreed initial heuristic settings to validate.
- Calibrate minimum service from expected chain-following traffic with a 2x
  safety factor. About 0.3 Mb/s is a provisional pilot calibration for the
  roughly 260 kB MVP profile, not an established sufficient rate. Keep the
  benchmark tunable and derive it from trusted sizing, not upstream claims.
- Retain the general startup calculation independently of that calibration:
  fresh/drained pipelines use `max(T, min(cold_cap, K * R))`, while continuous
  pipelines use `max(T, R)`. Initially use a 14,600 B reference window and a
  500 ms cold cap. Raising the service benchmark must not require restoring
  startup machinery or changing the algorithm.
- Preserve the unscaled allowance when applying hedge backoff. Use
  `min(base * F, max(base, configured_ceiling))`, initially with a two-second
  configured ceiling. The ceiling limits backoff growth without shortening
  the base allowance; snapshot the multiplier for each attempt.

The fetch policy belongs in consensus. The protocol supplies ordered requests,
replies, best-effort cancellation and observable completion events. Network
supplies connection and transport information. A policy deadline expiring is a
local decision; it does not by itself revoke a wire request.

## 1. Make each transaction batch a checkpoint

Agreed design direction: **one transaction wire batch is one checkpoint**.
Refactor job construction and request packing to implement this boundary, rather
than adapting the policy around the current larger wire batches.

A transaction checkpoint has:

- One immutable, nonempty set of transactions from one EB, in the order specified
  by its body.
- One actual byte total, calculated from that body's transaction sizes.
- One request attempt, one progress deadline, one reply and one cancellation
  target at a particular upstream.

The batch is the deadline and cancellation unit, not an all-or-nothing unit of
transaction acceptance. Complete transactions can be decoded, independently
verified and submitted for storage while the rest of its response is pending.
An incomplete transaction supplies no reusable prefix, and partial bytes from
different attempts never combine.

Build a checkpoint from eligible missing whole transactions until it reaches or
crosses the configured service target. The current candidate target is 20,000 B;
it is independent of the publication chunk size. A final or temporarily
availability-limited batch can be smaller. Do not split a transaction. Use the
actual batch bytes for patience and resource accounting, including overhang.

Publication chunks determine what a peer offers, not where requests must end.
A batch can contain transactions from several already offered chunks. It cannot
include transactions whose offers have not arrived. Atomic publication makes
all remaining transactions eligible together after the full-closure offer.

Pipeline multiple checkpoint requests to hide request latency. Do not combine
several checkpoints into one LeiosFetch reply. TCP and mux can coalesce bytes
without changing these application message boundaries. Appending another batch
does not enlarge an existing checkpoint or renew its deadline.

This changes the current implementation: a wire transaction request currently
can combine several internal jobs. Construction, validation descriptors and
packing should be refactored so that the checkpoint is the semantic request
unit. Internal indexing structures need not become wire fields.

A rescue batch requests the still-missing eligible subset of a stalled
checkpoint. It is a separate attempt with its own fixed membership and deadline,
linked to the original obligations. Completing those obligations from another
source does not imply that the original wire reply has retired.

## 2. Separate policy obligations from wire attempts

The receiver distinguishes:

1. **Data obligations:** which body or transactions are still needed globally.
2. **Wanted attempts:** which requests the policy currently relies on.
3. **Unresolved wire exchanges:** which issued requests still require replies.
4. **Storage completion:** which validated results are durably held.

An attempt can stop being wanted while its wire exchange remains unresolved.
A complete late transaction can still satisfy its obligation even if the rest
of the response remains incomplete. Arrival, individual verification, wire
retirement and durable acquisition are separate events.

Canceling a whole batch does not erase transactions already acquired through
other completed replies. A canceled body has no reusable prefix. Partial bytes
from separate attempts never combine.

## 3. Request identity

Agreed numbering scheme:

- Use a monotonically increasing unsigned 64-bit ordinal within one LeiosFetch
  client/server session.
- Number every reply-bearing client message: body requests, transaction batch
  requests and cancellation commands. Start at zero. `MsgDone` has no ordinal.
- Assign the ordinal when a message is irrevocably admitted to the ordered
  outbound stream. A locally prepared request withdrawn before that point has
  no wire ordinal and needs no cancellation.
- Every reply-bearing client message carries its own explicit ordinal. The
  server checks that it is the next consecutive ordinal in the session.
  `MsgTryCancel` additionally carries the ordinal of its target data request.
- Replies, including `MsgCancelled` and `MsgAckTryCancel`, do not echo an ordinal.
  Their FIFO position identifies the corresponding request and its descriptor.
- Never wrap or reuse an ordinal in a session. End the session before exhaustion.

Local bookkeeping uses a session/lifetime identifier as well as the ordinal.
A reconnect starts a new identity domain. A late storage callback from an old
session cannot settle a request in a replacement session. Existing consensus
job identities need not be replaced by these wire ordinals.

For example, a session can issue `BodyRequest(id=0, ...)`,
`TxsRequest(id=1, ...)`, then `TryCancel(id=2, target=1)`. The final command has
its own response slot; its target remains the original transaction request.

The dispatch integration must establish exactly where numbering occurs and
where it is serialized with outbound admission. Preparing work, withdrawing
unsent work and concurrent reply handling must not create ordinal gaps or
duplicate assignments. These are implementation obligations, not a reason to
reuse a canceled request's identity.

## 4. Messages and terminal results

Agreed: keep the normal request/reply transitions, add `MsgCancelled` as an
alternative result for both data request kinds, and add a separate busy state
for cancellation acknowledgments:

| Client message | State transition | Corresponding server reply |
|---|---|---|
| Body request `(id, ...)` | Idle -> BusyBody | Complete body, or `MsgCancelled` |
| Transaction batch request `(id, ...)` | Idle -> BusyTxs | Complete batch, or `MsgCancelled` |
| `MsgTryCancel(id, target)` | Idle -> BusyCancel | `MsgAckTryCancel`, returning to Idle |

These are logical state transitions. Pipelining lets the client send later
requests while earlier exchanges remain unresolved; server lookahead lets the
server receive them before sending every earlier reply.

While the session remains connected, each accepted data request has exactly one
terminal reply: either its complete normal response or its canceled response.
The cancel command has exactly one acknowledgment of its own. Acknowledgment
does not replace the original reply.

`MsgCancelled` is legal only as the original response to a body or transaction
batch request for which the client issued cancellation. It is not a possible
response to a cancellation command. It supplies no body, transaction data or
evidence that the receiver holds the requested content.

Empty transaction requests and empty transaction payload replies are invalid.
A normal response must satisfy the original request in full. The server cannot
return an arbitrary subset or an empty payload as a cancellation result.

`MsgAckTryCancel` has no payload and does not report success or failure. The
original response tells the receiver whether payload was suppressed.
Cancellation commands cannot themselves be cancellation targets.

## 5. Cancellation races and the response commit point

For each data request, the server tracks whether a response has been committed
and whether a cancellation has been processed.

- If cancellation is processed before response commitment, the request must
  produce its canceled result instead of the normal payload.
- If the normal response has already been committed, it remains the request's
  sole reply. The cancellation still receives its acknowledgment.
- Choosing a reply and processing cancellation must have a well-defined atomic
  ordering. Neither race can generate two replies or omit the original slot.

**Response commitment** is the sender's irrevocable choice of the complete
message immediately before handing it to the ordered output path. Preparing
data, reading storage or constructing a response does not by itself commit it.
Do not make a separate cancellation check followed by an unprotected commit.

Once committed, the response may already be encoded or queued below the
protocol. This contract does not retract it or interrupt a partially emitted
message. A cancellation can save work and egress before that point; its arrival
does not guarantee a particular saving.

Request intake must remain able to record cancellation while payload preparation
or output is blocked. A cancellable preparation wait must be woken or abandoned
when cancellation wins; merely recording a flag that is never checked until a
stalled operation finishes is insufficient. A result from abandoned preparation
must not later send a second reply.

## 6. Preserve FIFO response order

All server replies, including cancel acknowledgments, remain in client request
order. They cannot overtake an earlier response. The client retains the original
response handler after cancellation and installs another for its acknowledgment.

For example, request IDs are explicit wire fields. Reply IDs below are
annotations of their FIFO positions and are not wire fields:

```text
client -> server:  TxsRequest(id=40, A)
client -> server:  TxsRequest(id=41, B)
client -> server:  TryCancel(id=42, target=40)

server -> client:  reply #40: MsgCancelled, if cancellation won before commitment
server -> client:  reply #41: B
server -> client:  reply #42: AckTryCancel
```

If A was already committed, reply #40 contains all of A instead. There is no
additional normal or canceled response for #40.

The intake side processes #42 immediately when it is read; it must not defer
marking #40 canceled until the acknowledgment reaches the head of the output
queue. Conversely, the acknowledgment may wait behind #41. It is not an
out-of-order control channel or a low-latency notification.

The policy can request a replacement from another upstream before this
acknowledgment, subject to resource admission.

## 7. Define "too old" by a bounded request window

Elapsed time or the fact that a response was sent is insufficient grounds to
reject a cancellation. That response may still be in flight to the client.

Agreed admission scheme, initially with `D = 64` and `W = 128`:

- Admit at most `D` data-request records per LeiosFetch session, across body
  requests, transaction batches and all EBs being fetched from that peer.
- Each record reserves capacity for its original reply and one optional cancel
  exchange. Ordinary requests cannot consume this reserved control capacity.
- Retire a record when its original reply has arrived and, if a cancel was
  issued, its acknowledgment has arrived too. A canceled result arriving before
  the cancel acknowledgment does not yet make that record reusable.
- Thus every admitted data request still eligible for its first cancellation
  has reserved control capacity, even when all data-request records are occupied.
- Let `W = 2D`. There can be at most `W` unresolved request/reply exchanges.
  The unused cancel reservation is not itself a wire exchange or a sequence ID.

Proposed recognition rule using this bound:

- An exchange remains unresolved until its complete reply is received at the
  client. Logical withdrawal does not free its slot.
- For cancellation command ordinal `c` and target `r`, require `0 <= r < c` and
  `c - r < W`. Require that `r` names a data request, not a cancellation.
- Retain the kind, cancellation flag and response state of the previous `W`
  request ordinals at the server. Do not retain their payloads merely to
  recognize a late cancellation.
- Reject a target outside the window, a future target, a control-message target
  or a second cancellation for the same data request.

Why an honest late cancel fits: if request `r` was unresolved when the client
issued `c`, FIFO replies imply that every exchange from `r` through `c` was
also unresolved at that issue point. Their count is `c - r + 1`, which cannot
exceed `W`. A cancel already issued remains valid even if the original reply
arrives before that cancel reaches the server.

Issuing a cancellation and receiving the original result must be serialized
against the local request ledger. Do not newly issue a cancellation for a
request whose reply has already been consumed. A cancellation already admitted
to the outbound stream is not retracted when that reply subsequently arrives.

This pairs cancellation capacity with a particular data-request record; it is
not an independent control pool whose previous users can occupy every slot
while new data requests continue to be admitted. Cancellation cannot cause
unbounded admission by releasing records before their wire obligations retire.

The shared limit is negotiated as specified below. A server cannot silently use
a shorter history than the client's permitted window. Byte and message-size bounds remain
separate limits; the request-count window does not replace them.

### Agreed negotiation of the request-count bound

Agreed: negotiate `D` in the existing node-to-node handshake's version data
for the new cancellation-capable protocol version:

```text
D = min(local_advertised_maximum, remote_advertised_maximum)
W = 2 * D
```

Each endpoint advertises a validated positive limit, initially 64. Both derive
the same result. A peer advertising a larger number never raises our configured
maximum. For example, 64 and 32 select `D = 32, W = 64`; 64 and 256 select
`D = 64, W = 128`. Exact field encoding and permitted numerical range remain
implementation design work.

Use the agreed value throughout the connection lifetime, including later
LeiosFetch sessions on that connection. Local admission may operate below that
ceiling without notifying the peer. Raising the shared ceiling requires a new
handshake/connection; do not resize the request window or discard recognition
history underneath unresolved exchanges.

This adds no LeiosFetch initialization round trip. Version negotiation establishes
the limit before fetching begins. The network context must carry it to the
consensus client and server, which use it for paired-record admission, response
queue bounds and recent-request history. Do not negotiate the 1 MiB local byte
budget, checkpoint target, service benchmark or timeout parameters merely because
`D` is shared.

In the inspected local network checkout, `Cardano.Network.NodeToNode.Version`
already has version-specific version-data codecs and symmetric `Acceptable`
combination rules, but no LeiosFetch request-window field. Check the exact
network dependency of the chosen consensus base before wiring changes there.
Gate the new field and cancellation messages together by protocol capability.
Agreed: version-16 connections retain the current LeiosFetch behavior; the new
atomic policy is selected only on the new capability. Legacy connections cannot
be treated as if they supported cancellation or this negotiated window.

The implementation plan needs the version-data field and bounds checks, symmetric
combination, context propagation and asymmetric-limit/legacy-compatibility tests.

### Agreed initial sizing and validation

Size the new limits for our roughly 260 kB MVP closure and 20 kB checkpoint
profile, including its body, concurrent EB demand and expected service. The old
implementation's capacities are not throughput goals for the new policy. A
big-ledger peer need not receive a larger request budget merely because it had
one in the old implementation.

At the scouted consensus revision, the ordinary per-peer byte budget is
5,000,000 B and the big-ledger budget supplied by NodeKernel is 60,000,000 B.
The maximum transaction request size is 500,000 B, and the existing pipeline
depth is 150 requests, intended to cover several large closures with those
larger requests. These values are evidence about the current implementation,
not a decision to preserve its fetch policy unchanged.

With exactly 20,000 B checkpoints, the two byte budgets correspond to 250 and
3,000 data requests. Overhang, short batches, body requests and their mix change
the actual counts. A smaller count cap may deliberately bind before the byte
cap. Choosing a count cap from the nominal checkpoint alone does not guarantee
that every byte budget is reachable for arbitrarily small batches.

Validate the chosen `D` against concurrent EB demand, metadata per request/history
entry, maximum simultaneous sessions, and server lookahead bounds. Keep this
safety ceiling separate from the policy's desired pipeline size. The current
value 150 cannot simply retain its old meaning when wire batches become much
smaller checkpoints.

Agreed initial values, subject to resource audit and pilot measurements:

| Limit | Initial value |
|---|---:|
| Expected outstanding response bytes per peer, including bodies | 1 MiB |
| Data-request records per session, `D` | 64 |
| Maximum unresolved exchanges and recognition history, `W = 2D` | 128 |
| Extra budget just for big-ledger status | None |

1 MiB is four 262,144 B closure payloads' worth; bodies also consume the budget,
so this is not a promise to admit four complete closures plus their bodies.
At exactly 20,000 B per batch the byte cap admits about 52 such batches, while
the count ceiling leaves some room for smaller batches. Both caps apply.

An indivisible valid object must remain admissible. In particular, the scouted
protocol's body ceiling is 600,000 B. The 1 MiB budget accommodates that body
without an oversized-request exception; a shared 512 KiB cap would not. The
normal bodies of our small-closure runs are expected to be much smaller, but
the configured admission rules still need to handle protocol-valid sizes.

Agreed body reservation rule: before the body arrives its announced size is
untrusted. Reserve its applicable trusted body-size upper bound, including
encoded framing for wire accounting, in the relevant wire and storage budgets.
Do not reserve only the advertised bytes and then assume a larger valid body
can always obtain extra handoff capacity on receipt. This preserves the current
rule that a valid, admissible body is retained even if its peer misstated its
size, before that misstatement is handled as a peer fault. At a 600,000 B body
bound, one such reservation fits in the initial 1 MiB per-peer budget, while
transaction requests may consume the remaining capacity. Release unused space
at the appropriate wire/storage lifecycle boundary. Reservation size is distinct
from the policy's service/trigger allowance; it does not make every small body
inherit a maximum-body transfer timer.

Validate these initial values through a resource audit and later pilot measurements
of request-count pressure, byte pressure, memory, refill gaps and concurrent
EBs. A safety ceiling is not a target to fill; none of these values requires
the scheduler to request redundant or unnecessary data.

## 8. Checkpoint deadlines and policy reactions

Batch construction, peer selection, deadline formulas, primary persistence,
hedge rotation and backoff are consensus policy decisions. The cancellation
mechanism does not choose them.

For the current policy direction:

- Snapshot a batch's members, actual byte total and allowance. Do not renew its
  deadline on partial payload progress, queue appends or repeated offers.
- A queued batch's active-checkpoint timer does not begin merely because it was
  appended behind earlier work. Define activation from locally observable
  issuance and response-order events, not inferred remote TCP state. Logical
  withdrawal of an earlier batch does not establish that its wire slot has
  retired or that the next batch is now the serving head.
- Primary expiry can enable a rescue while retaining the primary. It does not
  require `MsgTryCancel`.
- When an attempt is abandoned, release its logical assignment as appropriate
  and send cancellation if it was issued and remains unresolved. Start another
  attempt without waiting for the old acknowledgment, if budgets admit it.
- A timeout or a cancellation is never evidence that a transaction is held.

There is no aggregate queue timeout in this proposal. A pipeline contains
several independently timed checkpoint batches.

### Agreed coordinator wakeups

For the new policy, wait atomically for the first of a coalesced readiness
signal, the nearest active policy deadline/cooldown, or a 500 ms fallback.
The fallback is not a minimum interval between planning passes: new state and
deadlines can wake the coordinator earlier.

Use cancellable timer registrations and arm the protocol timer for the remaining
time until its fixed absolute expiry. Readiness and periodic wakes do not renew
an attempt's allowance. Consume readiness before the planning snapshot, cancel
obsolete registrations when replacing/exiting a wait, and retire handled expiry
triggers to avoid stale notifications, lost wakeups and busy loops.

On every wake, process current state and all deadlines now due, applying
already-observed completions before expiry decisions. The selected wake branch
does not exclude other due work; frequent readiness cannot postpone an expired
deadline. Keep the existing version-16 behavior when integrating shared state.
The implementation plan maps this to the Churn-style STM wait and the readiness
signal conversion; the main planning machinery need not be replaced wholesale.

### Receiver-observable deadline activation

Agreed activation rule:

```text
anchor   = max(t_head, t_sent + R)
deadline = anchor + service_allowance
```

- `t_sent` is observed immediately after ordinary `Channel.send` returns. It
  excludes planner preparation/local work queues and includes waiting inside the
  channel operation. It is a local send-completion observation, not physical
  socket transmission. Tracer ordering is not part of this timing contract.
- `t_head` uses the receipt observation attached to the decoded terminal of all
  preceding FIFO response slots, including preceding cancel acknowledgments.
  Retain input-span timestamps across trailing bytes. With no preceding
  unresolved response, use `t_sent`.
- `R` is a conservative RTT allowance snapshotted for the attempt. Its estimator,
  freshness policy and fallback follow the agreed KeepAlive policy below.
- The service allowance distinguishes fresh/drained pipelines from continuously
  fed pipelines. Only the former get the bounded startup allowance. Final
  production constants and their hooks still require review.

The anchor is an accounting point at the receiver, not a claim about when the
server starts sending. If the request was sent sufficiently early during the
preceding response, its transit grace is already absorbed. A late append gets
only its remaining transit grace, rather than another full RTT at head activation.

For example, with `R = 100 ms` and a preceding response ending at `200 ms`, a
request sent at `180 ms` has anchor `280 ms`; one sent at `50 ms` has anchor
`200 ms`. Neither receives its service allowance while it is still behind the
preceding response. A fresh request still has a fixed deadline even if no
response bytes ever arrive.

Partial transaction receipt, repeated offers, logical reassignment and later
RTT updates do not renew an active deadline. Logical cancellation or global
completion of a predecessor's data obligations does not establish wire
retirement. Timestamp newly read input immediately after ordinary `Channel.recv`
returns, then retain those times while decoding/validation/storage proceeds.
Resolve element and terminal offsets to their last required input span; reusing
trailing bytes does not create a fresh arrival. Distinguish local backpressure
from upstream service. Reuse annotated codecs/drivers; no mux observer API or
special runner is required, and optional tracing may precede these observations.

A fast reply may be observed while the sender has not yet returned from its
send operation. Register immutable request identity before sending and reconcile
these events by identity/lifetime; do not require send-time notification to
precede receipt, or revive a completed attempt when that notification arrives.

Complete transaction delivery alone cannot free a FIFO slot that remains
unfinished. The data-reply and control-reply liveness rules below are agreed.
Neither introduces a whole-queue aggregate service timer.

### Agreed liveness rule for an unfinished data reply

Agreed rule: retain a head data reply's existing, fixed deadline until
its terminal wire result, even if its transactions have already been acquired
elsewhere or its logical attempt has been withdrawn. Do not renew that deadline
on progress, cancellation or fulfillment through another peer.

For example, upstream X's reply A may be committed and incomplete when another
source supplies all of A's transactions. Canceling A does not let X interrupt a
committed message. A distinct batch B already queued behind A is still needed,
but B cannot activate its own deadline until A finishes. If X then stalls on A,
dropping A's timer on fulfillment would leave B dependent on the much longer fatal driver
timeout. The stall is a separate upstream or transport failure, not an effect
of cancellation. A responsive upstream finishes A's committed reply, or sends
`MsgCancelled` if cancellation wins before commitment, then serves B normally.
Canceling A never cancels B.

On A's deadline, treat that upstream as blocked for scheduling: stop appending
new distinct work, and make still-needed queued work eligible for rescue through
another upstream. Keep the existing rescue-slot and admission limits, including
reservations for the old wire exchanges. Do not start B's own service clock
early, re-request transactions already acquired, or close the connection solely
because the soft deadline expired. Original replies may continue to drain and
provide useful late transactions. Resume ordinary scheduling when the blocking
wire reply retires, subject to any other pending cancellation or lifecycle rules.

Retaining the scheduling deadline does not create another failure event. An
already failed attempt must not be counted twice; a wholly redundant or withdrawn
attempt must not acquire a new hedge-failure/backoff/cooldown penalty merely
because its wire reply remains open. A still-wanted primary or hedge retains its
ordinary timeout behavior.

This rule covers data-response slots. A cancel acknowledgment can also block
later replies, but it has no payload-service allowance. Its agreed soft guard
below does not start a timer while the acknowledgment is still waiting behind
earlier replies.

### Agreed soft guard for a cancel acknowledgment at the FIFO head

Agreed rule: use one conservative RTT allowance `R` as a soft scheduling
guard once a cancel acknowledgment becomes the next FIFO reply. Snapshot `R`
for the cancellation command using the agreed KeepAlive policy. Define its head
time from the same terminal wire events as data replies; if it has no unresolved
predecessor, use its own dispatch time. Its soft deadline is:

```text
ack_deadline = t_head + R
```

This timer excludes all time spent behind earlier data replies. It does not
assign a payload-transfer allowance, cold-start budget or adaptive hedge backoff
to a small control acknowledgment, and it introduces no new tuning parameter.

If the guard expires, treat the upstream as blocked for new assignments and make
still-needed queued data eligible for the existing bounded rescue policy. Do not
resend `MsgTryCancel`, release unresolved exchange reservations, close the
connection on this soft expiry, or count it as a payload hedge failure that
increases backoff or cooldown. Continue receiving. A late acknowledgment retires
its own exchange exactly once; reevaluate scheduling against the new FIFO head
and any other lifecycle constraints. The fatal driver/session timeout remains
the final guard for a connection that never completes the exchange.

For example, an acknowledgment dispatched at 50 ms may wait behind data until
2 seconds. With `R = 150 ms`, its soft deadline is 2.150 seconds, not
200 ms. A useful batch queued behind it can therefore become eligible for rescue
if the acknowledgment stalls, while slow preceding data is judged using that data
reply's own allowance.

### Initial RTT source: KeepAlive

Agreed: use the existing sparse KeepAlive measurements for the first integration.
The production assumption is a nominal 10-second sampling cadence, not a new RTT
measurement per fetch request or per transaction batch. Read cached information;
do not postpone a fetch until another KeepAlive probe completes.

The inspected KeepAlive client sends its first probe immediately when it starts,
then waits the configured interval after a successful reply. Thus subsequent
completed samples can be separated by the interval plus the exchange duration.
A fresh connection need not wait ten seconds for its first sample, although
fetching still needs a fallback until a real sample is available.

The inspected registry's `PeerGSV` stores a smoothed model, currently with a
1000-second EWMA time constant, and a `sampleTime` based on the probe's send time.
Its distribution implementation is degenerate; it does not provide an empirical
p90 merely because a percentile accessor exists. The dummy initial value is not
a successful measurement. These are current-code facts, not agreed estimator
settings to carry into this policy unchanged.

Agreed semantic network-to-consensus input:

- The RTT of the latest successfully completed KeepAlive exchange.
- Its monotonic measurement timestamp, taken when that exchange completes.
- Explicit absence until a real measurement is available. Do not pass the
  registry's dummy initial model off as an observation.
- Connection-lifetime scope: a new connection does not inherit an old one's
  measurement. A new batch, EB or fetch session on the same live connection
  need not reset that connection's RTT input.

The producer should publish the RTT and timestamp consistently as one observation
in functional state; consumers must not reconstruct it from an optional tracer.
The exact reader type and dispatch-context wiring are implementation work.
The existing `PeerGSV.sampleTime` records probe send time, so it is not directly
the agreed completed-measurement timestamp. A smoothed estimate may be exposed
later but is not required by this initial interface.

The initial headroom, freshness threshold, fallback and cap are agreed below.
The simulator's static jitter-p90 formula depends on its configured distribution
and is not a production RTT estimator.
Network should expose the measured information; consensus applies fetch-policy
allowances. Refreshing a cached estimate affects future attempts, not deadlines
already snapshotted.

Agreed initial policy parameters, subject to later pilot validation:

| Setting | Initial value |
|---|---:|
| Headroom over the latest usable KeepAlive RTT | 1.5x |
| Maximum age of a usable observation | 30 s |
| RTT allowance cap | 500 ms |
| RTT allowance without a usable observation | 500 ms |

For a positive RTT sample `r` no older than 30 seconds, the agreed allowance
is `R = min(500 ms, 1.5 * r)`. Otherwise use `R = 500 ms` without waiting for a
probe. These are bounded heuristic settings, not an empirical p90 or a guarantee
that all request transit completes within `R`. They require later pilot
validation. The RTT headroom factor is independent of hedge-failure backoff.

Keep the underlying measurement unchanged. Applying a cap to fetch patience
does not change the peer's physical RTT or transport recovery timers. A newer
observation affects future request snapshots; it does not renew earlier ones.

The local network branch contains a mux RTT quantile reader, but it is future
development. Do not require it or describe its distribution as the initial
production source. A later producer can be integrated without changing the
checkpoint and cancellation semantics established here.

### Service benchmark and general startup allowance

The RTT policy, activation rule and chain-following service calibration are
agreed starting choices. Retaining the general cold-window calculation is also
agreed. The numerical service floor is an educated starting estimate to test,
not a reason to specialize away startup handling.

Let `B` be the original requested payload byte total: the whole body for a body
request, or all transactions in a checkpoint batch. Use actual sizes, including
transaction overhang, and do not reduce `B` when another source makes part of
the batch redundant.

Initial service calibration:

- Receiver service benchmark: derive from expected full chain-following traffic
  with a safety multiplier, initially 2x. For the roughly 260 kB MVP profile,
  the illustrative result is about 0.287 Mb/s; the rounded initial pilot value
  is 0.3 Mb/s. Whether that rate is sufficient remains to be tested; preserve
  an explicit benchmark override and a tunable safety factor.
- Calibrate from trusted deployment sizing: expected Praos block bytes, closure
  bytes, body allowance and mean block interval. Do not let an upstream's claims
  change the benchmark. Keep it independent of peer stake, role and RTT.

```text
minimum_service_bits_per_second =
    safety_factor * 8 * (praos_bytes + closure_bytes + body_bytes) / block_interval
```

For the user's approximate 90,000 B Praos block, a 262,144 B closure ceiling,
6,624 B illustrative body and 20-second interval, baseline demand is
17,938.4 B/s. Doubling gives 35,876.8 B/s, or 287,014.4 bits/s. The body value
comes from the sampled mainnet-TX seed-1 fixture, not a universal production body
size: `smol-world/benchmark-logs/capped750-degree6-calibration/README.md` in the
network repository records 184 transactions and a 6,624 B body.

Agreed general startup calculation, initially using a fixed reference window
`I = 14,600 B` and a 500 ms startup cap. Keep the reference window, startup cap
and service benchmark independently configurable. The reference window is a
receiver-policy yardstick, not an observation of the sender's TCP state:

```text
T = 8 * B / minimum_service_bits_per_second
K = smallest positive integer satisfying I * (2^K - 1) >= B
cold_allowance = min(500 ms, K * R)

fresh/drained pipeline: service_allowance = max(T, cold_allowance)
continuous pipeline:   service_allowance = max(T, R)

deadline = max(t_head, t_sent + R) + service_allowance
```

Classify startup eligibility from the locally observed data pipeline when the
request is dispatched. A fresh or locally drained pipeline receives startup
patience; a continuously fed pipeline does not receive it again for every batch.
Do not infer the server's current congestion window or reset TCP state. The
exact classification across preceding control replies and local backpressure
must be covered by the integration audit.

For a 20,000 B batch, the rounded 0.3 Mb/s starting value gives `T` about 533 ms,
versus 107 ms at the previously proposed 1.5 Mb/s. With `R = 150 ms`, `K = 2`
and a 300 ms cold allowance, transfer time dominates: a fresh request's base
deadline is about 683 ms after dispatch; an early-pipelined successor gets
about 533 ms of service allowance from its FIFO head time, without an extra
full transit RTT. Membership, byte totals and deadlines remain fixed as before.
At this provisional floor transfer time dominates, but retain the full formula.
For comparison, at a 1.5 Mb/s benchmark the same batch has `T` about 107 ms:
the fresh service allowance is then 300 ms, versus 150 ms for a continuously
fed pipeline. Changing the calibrated rate therefore changes which existing
term binds, without requiring an algorithm change.

This calibrates the average chain-following service benchmark. It does not by
itself establish the 7-second certification or 14-second recovery objective:
at 0.3 Mb/s, a 262,144 B closure takes about 6.99 seconds of payload transfer
from one source before request latency or multi-hop publication delays. Multiple
upstreams can contribute concurrently, and actual honest service can exceed
the benchmark. The lower floor also delays rescue triggers. Assess the resulting
tradeoff with hold-time and amplification measurements before treating the
rounded value as a final production setting.

The benchmark is not a strict sustained-rate guarantee: RTT patience can
dominate for small batches. The 500 ms RTT/startup caps do not cap total service
time. A large body's unscaled deadline can exceed two seconds; the agreed hedge
ceiling below therefore cannot shorten its base allowance. Apply this rule when
porting the policy's hedge timers rather than copying the simulator's two-second
hedge setting blindly.

### Agreed hedge-ceiling rule

Agreed rule: preserve the base allowance when applying adaptive hedge backoff,
rather than rejecting a whole configuration or shortening a large request to a
fixed two-second ceiling.

Let `base` be the unscaled allowance from the response's FIFO head time, including
any remaining transit grace:

```text
base = max(0, t_sent + R - t_head) + service_allowance
effective_ceiling = max(base, configured_hedge_ceiling)
hedge_allowance = min(base * F, effective_ceiling)
deadline = t_head + hedge_allowance
```

`F >= 1` is the applicable hedge-backoff multiplier, snapshotted for that attempt.
With `F = 1`, this matches the agreed ordinary activation formula. The agreed
initial configured ceiling is two seconds, following the simulator's normal
profile, but it may not shorten `base`.

For example, `base = 0.7 s`, `F = 4` and a two-second configured ceiling gives
two seconds. If `base = 3 s`, it gives three seconds, without further backoff
growth unless the configured ceiling is raised. A 600,000 B body at 0.3 Mb/s
already has 16 seconds of transfer allowance, and can have a 16.5-second fresh
base with `R = 500 ms`; treating two seconds as an unconditional cap would be
inconsistent with the service calculation.

This does not cancel a persistent primary on its trigger deadline or renew an
active deadline on progress. Retain the separation from the fatal driver guard,
and check configurations so that valid base allowances are not pre-empted by
that guard. Compute clipped backoff without allowing multiplier overflow.

### When only part of a batch becomes redundant

The first contract cancels whole batches, not selected members. This is an
intentional request boundary, not a promise of per-transaction wire cancellation.

Agreed rule: if a batch still contains needed transactions, let
it continue unless the policy explicitly abandons that attempt. If none of
its transactions remain needed, cancel the whole batch if its wire response
remains unresolved and no cancellation has already been issued. Withdraw work
that has not entered the outbound stream locally, without a cancellation
message. Do not newly cancel an exchange whose reply has already been consumed.
On explicit abandonment, construct replacement batches from transactions still
missing at that time; do not re-request already held transactions.

Becoming partly redundant does not shrink the original batch's byte total,
renew its deadline or exempt it from its normal stall and handoff rules.

This bounds cancellation granularity by the checkpoint size. It differs from
the simulator's ability to withdraw individual transaction attempts. Record and
measure that difference rather than claiming identical wire behavior. Selective
batch editing remains a separate extension; accepting independently verified
transactions during a response is agreed here.

## 9. Late replies, validation and acquisition

A normal reply arriving after cancellation is legal and must still be correlated
with its original request. Validate it against that request's immutable
descriptor, even when a replacement is in progress or the data is now duplicate.

### Accept individual transactions during a response

Agreed: a fully received transaction can be accepted before its containing
response finishes, provided it independently matches its expected position,
identifier/hash and declared size in the verified EB body. Here verification
means the fetch request's authenticity and matching checks; this does not claim
that ledger execution or every later validation stage has completed.

Retain newly useful verified transactions from late responses too. If the peer
then disconnects, sends a wrong later transaction or supplies malformed remaining
framing, that does not invalidate an earlier independently verified transaction.
Preserve such data subject to the EB's relevance and storage lifecycle, reject
the invalid remainder, and apply the existing peer-failure rules. A partially
received transaction is discarded and cannot combine with another attempt.

For example, if a response owes transactions 1--10 and valid transactions 1--8
arrive before it stalls part-way through 9, the receiver can retain 1--8.
Recovery requests only the transactions still missing; it does not repeat those
eight merely because their original batch never finished.

The response still has to obey its original membership, order, count and framing.
Reject an unexpected or oversized element as soon as it can be identified. An
invalid remainder cannot supply extra credit or silently shift the expected
position of later elements.

### Validation descriptors and progress events

Each outstanding batch needs access to the individual expected transaction
hashes and sizes, in request order. Retain those bounded descriptors or provide
a reliable storage lookup while the attempt remains unresolved. An aggregate
hash commitment alone cannot authenticate each transaction prefix independently.
Pruning and reconnects must not make an old descriptor refer to new work.

The decoder/driver integration must report complete transaction boundaries
without waiting for the entire vector. For each element, correlate its session,
request ID and position, validate it, then emit a transaction event. Bound any
progress queue and storage backlog. Select the concrete API in the integration
plan; the existing complete-message interface does not provide these events.

Distinguish transaction-byte receipt, successful request validation, durable
storage, and the terminal completion of the original response. Partial delivery
does not alter batch membership or renew its fixed deadline. It also does not
advance the FIFO response slot or establish that a later pipelined request is
now the serving head.

### Deduplication and durable acquisition

Do not count duplicate transactions as new service or settle a different wire
attempt merely because its transaction set overlaps. Reconcile the global
missing set and pending storage state so that competing copies earn useful
delivery credit at most once. Withdraw wholly redundant batches or apply the
agreed partly redundant and explicit abandonment rules above.

Verified data awaiting storage is not yet durable acquisition. If duplicate
requests are provisionally suppressed during a write, a failed or discarded
write must leave still-relevant missing transactions fetchable. Do not restore
pruned EB state or mark data durably held merely because a decoder reported it.
Advertise body, chunk or closure possession only at its appropriate durable
milestone. The EB body itself remains an indivisible verified object.

Receiving `MsgCancelled` is not useful delivery and earns no
service credit. Only the associated wire exchange retires. A cancel
acknowledgment likewise supplies no data or service evidence.

Individual transaction events do not retire the response slot. Keep the batch's
full response-byte reservation until its terminal wire result, as in the current
conservative admission model; storage and useful-delivery credit are separate.
Wire retirement releases the completed exchange's accounting and corresponding
in-flight wire byte reservation exactly once. The paired data-request record
remains occupied until both the original reply and any required cancellation
acknowledgment have arrived. Sending cancellation alone refunds neither.
Separate storage work may outlive wire retirement: consensus must preserve its
durable-write, pruning and peer-lifetime safeguards, and advertise acquisition
only at the appropriate durable milestone.

## 10. Admission, shutdown and compatibility

Bound data requests, control requests, total reserved bytes and server lookahead
work. Canceled but unresolved exchanges still consume capacity. Replacement
admission must account for that debt, so immediate logical reassignment is not
an unlimited source of new traffic.

Cancel intake must not sit behind slow payload preparation or an output operation
in the same thread. The sender remains the single owner of response order.
An implementation must bound the lookahead queue as well as its wire budgets.

Do not retry an unacknowledged cancel with another `MsgTryCancel` for the same
target. Its acknowledgment is ordered behind earlier replies, so a one-RTT
deadline measured from cancellation dispatch would be inappropriate. The agreed
soft guard above begins only at the acknowledgment's FIFO head; it does not
replace the existing session-failure policy or permit duplicate controls.

Graceful termination drains accepted replies, including cancel results and
acknowledgments. Abrupt connection loss retires that session's wire state;
unfinished obligations can be reassigned. Later storage callbacks remain tied
to their original lifetime and cannot recreate a disconnected peer.

Introduce this as a versioned protocol capability. Do not send the new messages
to an older peer; `MsgCancelled` is not a valid result in the legacy protocol.
The version-16 fallback is agreed: retain its current LeiosFetch behavior and
select the new atomic policy only when cancellation capability is negotiated.
Exact new version/tag allocation and handshake encoding remain implementation
design work. Mixed-version connections must share acquisition/deduplication
without settling each other's wire exchanges or counting useful service twice.

## 11. Integration boundaries and evidence

The initial reconnaissance used consensus
`nfrisby/leios-main-fetch-logic` at
`75b62a611dd6088c9c98cf7f06b914aaa44f5d97`. The notes below describe that earlier
baseline. This publication stack instead starts from
`nfrisby/leios-main-fetch-logic-stack-chunk-5` at `ff7c9760d`, then introduces the
cancellation-capable implementation in the following source commits.

At the target revision:

- `MiniProtocol/LeiosFetch/Type.hs` has body and transaction busy states, a
  transaction-vector reply and no cancellation messages.
- `MiniProtocol/LeiosFetch/Server.hs` receives a request, runs its handler and
  yields its response before receiving the next request.
- `MiniProtocol/LeiosFetch/Client/Replies.hs` combines jobs' offsets into one
  wire request and validates replies against their requested jobs.
- Its transaction codec builds a complete vector, and the typed-protocols
  `Driver.recvMessage` interface returns a complete message. Independent
  transaction-prefix events require an integration change, not merely an
  earlier call to the current batch reply handler.

Refactor those responsibilities around the checkpoint batch. Update decision
state, client dispatch, server handling, validation, codecs, limits and tests as
one integration effort; do not assume a change to request packing alone suffices.

The agreed direction is to extend incremental decoding/progress reporting
while retaining one typed transaction-batch reply. A typed response stream is an
alternative if implementation review exposes a reason to revisit the design.
The opt-in callback API direction is agreed; exact signatures still need design.
The batch stays one checkpoint and unresolved
exchange, with the agreed FIFO and terminal-result rules. Accepting prefixes
alone does not authorize interrupting an already committed normal response;
any change to that cancellation commit rule needs an explicit further decision.

The local typed-protocols checkout at
`8fa4ed2f5bfece84b323f6d80e0b61f4153b6975` already includes `Lookahead`,
`AwaitLookahead`, ordered background senders and supporting drivers/proofs.
Its changelog lists them in version 1.2.2.0. They are a candidate framework
mechanism, not cancellation semantics by themselves. Check dependency versions,
network driver support, cancellation wakeups and queue bounds before concluding
whether additional typed-protocols changes are required.

The [simulator policy reference](mvp-fetch-policy.md) was copied from
`ouroboros-network/smol-world/mvp-fetch-policy.md`,
particularly its persistent body/checkpoint sections. Its wire-attempt and
per-transaction completion assumptions require the explicit adjustments above.

### Driver.Limits reconnaissance

Read-only reconnaissance, not a choice of wire representation:

- `Ouroboros.Network.Driver.Limits` exports `driverWithLimits` and
  `annotatedDriverWithLimits`. The ordinary and pipelined runner functions are
  wrappers around those factories and the typed-protocols peer interpreters.
  Reuse their size checks, framing/trailing-byte handling and timeout machinery.
- Internally, `mkDriverWithLimits` accepts a decoder-running function; it is not
  exported. A progress-aware factory added in that module could reuse this
  structure. It would still need a decoder/progress API and request-context
  contract; the public factories do not currently accept transaction-yield hooks.
- The target consensus branch already supplies a small
  `runLookaheadFixedSenderPeerWithLimits` wrapper for LeiosNotify, using the
  public factory; its comment says to upstream it to ouroboros-network. A
  variable-sender lookahead runner, including the required annotated form, is
  a plausible generic network addition for LeiosFetch's differing response kinds.
  It does not require a separate implementation of network I/O or limit checks.
- Lookahead is still needed for prompt server cancellation intake whichever
  payload representation is selected. A decoder progress callback alone does
  not make a sequential server receive while it is preparing/sending a reply.
- The driver factory selects limits from the protocol state and wraps the whole
  incremental message decoder in its timeout. Expiry throws
  `ExceededTimeLimit`; it does not notify consensus and keep the primary alive.
  Our soft checkpoint timers must remain distinct from that fatal wire guard.
- The scouted LeiosFetch body and transaction states use `longWait`; the inspected
  network implementation defines it as 60 seconds. This is generous relative to
  our ordinary checkpoint deadlines. Keep its role as a coarse connection-failure
  guard; its existing value does not itself justify a custom driver or a timeout
  redesign. The earlier distinction is about expiry semantics, not a claim that
  this guard is too tight for normal transaction handling.
- Per-message size/time limits do not enforce the negotiated `D`. The server's
  lookahead/admission logic must bound queued work while continuing to read first
  cancellation commands when all data records are occupied. Stopping all reads
  at `D` data requests would defeat the reserved cancellation capacity.
- `Driver.recvMessage` returns a complete typed message. `DecodeStep` exposes
  partial input, completion and failure, but no first-class transaction-yield
  event. Incremental byte input is not itself incremental transaction delivery.

The preferred path and its alternative are:

1. **Single vector message with incremental transaction events (preferred):**
   keep the checkpoint as one typed wire reply and refactor its decoder to
   expose complete elements before returning the whole message. This needs a
   first-class progress codec/driver interface or carefully scoped monadic
   decoder callbacks plus active-request context. A new driver name wrapping
   the unchanged vector codec is insufficient. It can reuse the current peer
   interpreter and limit logic, but progress publication, request correlation
   and annotation handling need explicit design and tests.
2. **Typed transaction stream (alternative):** one checkpoint request has several server
   transaction messages and one terminal outcome. The pipelined `Receiver`
   loops through messages, validates each transaction and returns `ReceiverDone`
   only at batch end. Existing BlockFetch already does the analogous per-block
   processing in `receiveBlocks`. The standard limited driver can be reused.
   Per-message limits must be supplemented by immutable whole-batch count/byte
   checks and the non-renewing consensus checkpoint deadline. Progress messages
   do not consume new request IDs or `D` records. Frame/terminal details remain
   to be settled; this replaces the current single-vector reply format.

In the preferred path, the standard CBOR adapter currently returns only when its
entire decoder succeeds. A callback cannot simply be attached to a partially
filled vector that the adapter never exposes. The generic size-limit runner
also permits a final input chunk to exceed the message limit, then subtracts
trailing bytes after decoding; progress callbacks need their own element/count
and cumulative-consumption checks before publishing results. Independently valid
earlier transactions can still be retained when later framing is rejected, as
agreed, without allowing unbounded callback work on oversized input.

Decoder callbacks execute inside the fatal wire timeout if placed in that
incremental decoder. The existing 60-second guard leaves substantial room for
bounded decoding and quick progress publication; callback placement is not by
itself an architectural blocker. Keep potentially unbounded database waits and
policy work outside it, using bounded handoff and explicit local-backpressure
handling. The much shorter consensus checkpoint deadline still needs to
distinguish local delays. A typed stream puts
per-transaction peer effects between message receives, although the separate
checkpoint timer still needs to distinguish local delays. Do not infer that
either representation automatically solves callback/backpressure accounting.

The single-message option avoids per-transaction wire envelopes and driver/trace
calls; the typed stream reuses established peer machinery and makes incremental
delivery explicit in the protocol. No performance comparison has been run.

LeiosFetch's wire types currently live in consensus. A driver in network that
pattern-matches those constructors would require moving the protocol definitions
into network or introducing a dependency cycle. Generic driver/progress hooks can
remain in network while consensus supplies the codec and request-specific actions;
a thin Leios-specific runner can also live in consensus. Keep policy there.

Functional dispatch/receive timestamp hooks may be useful with either path.
Existing send/receive traces are not a substitute for a required event interface,
and receipt events should be captured before storage work delays processing.

Changing to a typed stream would also permit a later decision to honor cancellation
between transactions. That is separate from selecting a representation: the
current agreed commit rule does not interrupt a normal response once committed.
A new driver alone cannot retract part of an already emitted CBOR message.

### Agreed single-message receive sequence

Agreed API direction, not functionality already supplied by the current driver:

1. Before decoding a reply, pin the exact FIFO request's receive context: its
   connection/session lifetime, request ID, EB and immutable ordered transaction
   descriptors. Retain access to that context until the reply retires or the
   connection closes; logical reassignment must not replace it underneath an
   active decoder or delayed callback.
2. Parse and check batch framing and declared count. Incrementally decode each
   complete transaction, preserving input bounds, consumed-byte accounting and
   trailing input for the rest of this or the next reply.
3. Report each complete element with its position and receiver-observation time
   to the pinned context. Network supplies generic decoding/progress hooks;
   the protocol codec and consensus callback supply transaction-specific checks.
   Network must not import consensus fetch policy or the current consensus-owned
   LeiosFetch message definitions.
4. Consensus checks the element's expected
   hash, size and position using a descriptor prepared before dispatch. Keep
   this validation bounded and free of database lookups. Then hand verified data
   to bounded storage processing without waiting for durable completion. Exact
   queue/backpressure behavior remains to be designed.
5. The ordinary full-message result still terminates the original response slot.
   Its final handler performs remaining framing/accounting work, not a second
   ingestion or useful-byte credit for elements delivered through progress.
   Keeping the final vector until this point is acceptable initially; eliminating
   that retention is a separate optimization, not a reason to weaken message
   invariants or return a synthetic empty transaction reply.
6. On later decode failure, invalid data or disconnect, preserve previously
   independently verified elements subject to storage/pruning rules. Dispose of
   the incomplete element and clean up the original receive context. Outstanding
   storage callbacks remain lifetime-scoped and cannot settle replacement work.

The codec must actually expose element boundaries; a different driver wrapper
cannot recover them from an unchanged decoder that returns only a full vector.
Prefer reusable progress facilities in or alongside `Driver.Limits`, using its
existing limit machinery, over a separate Leios-specific network I/O stack.
The exact types and whether a new public factory is needed remain open.

The [progress-event contract](#validation-descriptors-and-progress-events)
and [durable-acquisition contract](#deduplication-and-durable-acquisition)
require reserve-before-issue storage capacity and a transaction-write receipt.
The initial 1 MiB/4,096-entry per-upstream handoff bounds and ready-data
coalescing are agreed. The opt-in callback codec/driver direction is agreed;
concrete signatures and hooks still need implementation design and validation.
The current SQLite writer's submission can block on its 27-job queue even
though it returns asynchronous durability promises; do not assume calling it
directly makes a decoder callback nonblocking.

This preference retains the current cancellation commit rule. A batch can be
suppressed before normal-response commitment. Once committed, the original
message must drain normally or the session must close, even if the policy starts
recovery elsewhere. Prefix acceptance saves verified transactions from being
re-requested; it does not create selective cancellation within a CBOR message.

## 12. Implementation design and validation still required

The following requirements connect policy, codecs, driver events, storage and
capability negotiation. Version-16 fallback and per-EB retry/backoff/hedge scope
with shared peer budgets are agreed. Validate the initial resource settings
against these requirements; documenting a contract alone does not establish its
correctness or operational suitability.

1. Identify exact client/driver event hooks for activation and fresh/drained
   classification, and distinguish local backpressure from upstream service.
   The general startup rule is retained independently of the provisional
   chain-derived floor. Validate the floor and other starting parameters in
   pilots. The unfinished-data-reply rule, head-only soft guard for cancel
   acknowledgments and base-preserving hedge ceiling are agreed. Validate their
   event ordering and timeout races together. The initial configured hedge
   ceiling is two seconds.
2. Wire the agreed handshake negotiation `D = min(local_max, remote_max)` into
   both endpoints. Review the bounded-history argument against final admission
   rules, and validate resource use before implementation defaults are final.
3. Develop and review the preferred single-message incremental decoder/progress
   interface, pinned receive context, callback validation and bounded delivery/
   storage queues, following the [progress-event contract](#validation-descriptors-and-progress-events). Specify and
   validate the agreed opt-in callback adapter. Buffer sizes, coalescing,
   reserve-before-issue handoff and the per-transaction durable receipt are
   agreed initial choices; implement and validate their lifecycle and resource
   guards.
   Retain a typed stream as an alternative if review exposes a
   reason to revisit the preference. Preserve the agreed transaction, response
   and cancellation semantics across the API choice.
4. Specify protocol-version/capability gating and implement the agreed
   version-16 fallback with mixed-version acquisition tests.
   Assign wire tags and handshake encoding only after reviewing the target
   branch's version conventions. Reconcile per-message limits, mux ingress
   limits, negotiated request counts and local wire/storage budgets; they bound
   different resources and none substitutes for the others.

### Post-implementation follow-ups

- **Consolidate into experimental V16:** remove V17 and roll its changes into
  V16, which is still experimental. Move cancellation-capable LeiosFetch,
  request-count and optional publication-chunk-size negotiation, codecs and CDDL
  together. Update both repositories' version maps, capability selection,
  constructors/generators, schema and compatibility tests, and current policy
  documentation in the same migration. Retire the split between legacy
  LeiosFetch in V16 and cancellation-capable LeiosFetch in V17; compatibility
  with older V16 prototype peers is not a requirement of this consolidation.
  The V15/V16 capability boundary remains: V15 retains its existing protocols
  without the Leios mini-protocols, while V16 enables the updated Leios protocols.
  This is not an old/new LeiosFetch fallback at V15. Preserve the agreed
  checkpoint/cancellation semantics and resource bounds, and keep Cardano's
  released-version cutoff at V15. This is a
  post-implementation change: the current implementation retains V17 until that
  migration is performed and validated.

- **Cooldown jitter:** consider bounded random jitter on closure retry cooldowns
  to reduce synchronized retry bursts across nodes. Choose the jitter range and
  its interaction with the 16-second cap after the deterministic implementation
  is validated. Sample once when entering a cooldown and retain that absolute
  expiry, following [the coordinator wakeup rules](#agreed-coordinator-wakeups);
  new offers, progress and readiness wakes must not resample or renew it. Keep
  random streams reproducible for tests and verify that separate EBs remain
  independent. This is future policy tuning: the initial cooldowns remain
  deterministic at 1, 2, 4, 8 and 16 seconds, and no jitter is implemented here.

## 13. Scenarios the eventual implementation must cover

- Cancel before preparation, during preparation, immediately before commitment,
  during output, and after a full response was sent but is still in flight.
- Receive a full result concurrently with issuing its cancellation; retire and
  refund once, with no ordinal gap or lost reply handler.
- Exercise early and late pipeline appends, fresh/drained queues, no response
  bytes, stale or absent KeepAlive samples, dummy initial data and a new RTT
  sample arriving after a deadline was snapshotted. Test local processing
  backpressure and incomplete terminal framing after every transaction verifies.
- Reject a missing, skipped, duplicate or out-of-sequence request ID. Cover
  unsent withdrawals and mixed body, transaction and cancellation commands.
- Cancel an early batch with later batches already pipelined; process the cancel
  promptly while preserving the order of every reply and acknowledgment.
- Cancel A while B remains queued behind it: a responsive server continues to B.
  Separately, stall A's committed reply after another source fulfills A; retain
  A's original deadline and allow still-needed B to be rescued within the
  existing limits, without starting B's service clock early or penalizing A's
  already fulfilled data obligation again.
- Queue a cancel acknowledgment behind slow data without starting its soft
  guard early; then stall it at the FIFO head while later data is still needed.
  Cover guard expiry, bounded rescue, late acknowledgment and normal scheduling
  recovery without duplicate cancellation, premature refunds or hedge penalties.
  Include coalesced input where the preceding reply, acknowledgment and next
  data reply arrive together, preserving their observed completion timestamps.
- Reject an unsolicited `MsgCancelled`, empty transaction payload, duplicate
  cancel, future target, control-message target and an actually out-of-window
  target.
- Exercise the oldest legal target, mixed body/transaction/control exchanges,
  full data-request records and reserved cancellation capacity. Demonstrate
  bounded history and hold a canceled record until its acknowledgment arrives.
- Exercise asymmetric maxima, invalid advertised
  values, older versions, reconnects with changed limits and a local admission
  target below the negotiated ceiling. Both ends must derive the same `D`.
- Complete an obligation through another peer while a batch is partly useful,
  wholly redundant, queued, committed or already canceling.
- Accept independently verified transaction prefixes, then stall, disconnect,
  receive a wrong later element or encounter malformed remaining framing.
  Preserve valid data without accepting partial or unverified transactions.
- Cover duplicates arriving during validation and storage, failed/discarded
  writes, stale descriptors, queue backpressure and exactly-once useful credit.
- Receive a valid late batch after replacement, pruning or EB refocus; finish
  processing an already received batch after disconnection. Preserve useful
  durable data without settling a different attempt.
- Shut down with pending normal replies and cancel acknowledgments; reconnect
  while callbacks from the previous lifetime remain active.
- Verify whole-transaction overhang, a transaction larger than the service
  target, short offered batches, and multiple checkpoint requests sharing one
  TCP connection without being merged into one application reply.

These validation obligations apply to the source implementation and its tests.
