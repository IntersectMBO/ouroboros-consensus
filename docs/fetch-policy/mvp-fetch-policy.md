# Fine-MVP and atomic EB fetching

This is the simulator policy reference copied from `ouroboros-network/smol-world`.
The [cancellation and checkpoint contract](cancellation-contract.md) specifies
production wire behavior. Simulator source/document paths below refer to that
repository and are not paths in ouroboros-consensus.

Both policies fetch one verified EB body, identify the transactions still
missing locally, and request those transactions through ordered per-peer
pipelines. Each missing transaction has one logical owner. Different peers can
supply different transactions concurrently. By default, closure fetching uses
single ownership without duplicate races. The optional persistent-checkpoint
policy adds one receiver-wide rescue batch. Neither procedure uses a fixed
worker set. Body fetching can independently hedge one extra complete response.

The DES implements this scheduling policy with simplified wire mechanics.
[Production wire-order liveness and simulator limits](#production-wire-order-liveness-and-simulator-limits)
records the additional requirements identified while implementing the node.

The difference is when closure data becomes eligible to request and forward.
**Atomic refers to the whole-closure publication boundary; its downloader still
requests individual transactions and checks progress in small batches.**

## Policies and offers

A **chunk** groups consecutive whole transactions. We use a nominal target of
**16,384 B (about 16 kB)**. This is a byte target, rather than a fixed transaction
count: transaction sizes vary naturally. Add whole transactions until the target
is reached or crossed, then start the next chunk. The closing transaction may
make a chunk larger than the target; the final chunk may be shorter. For example,
6,000 B, 5,000 B and 7,000 B transactions form an 18,000 B chunk.

A chunk is advertised only once all its constituent transactions have been
verified. Transactions are never split to fit a chunk, and the nominal target
is not a hard maximum on its payload.

| Policy | When a serving node offers closure data | What a receiver waits for |
|---|---|---|
| `mvp-fine` | Each complete verified chunk | An arrived offer covering the requested transaction |
| `atomic` | Its complete verified closure | Arrived full-closure coverage from that upstream |

An atomic receiver attached to a fine-MVP supplier waits until that supplier's
propagated chunk offers cover the entire closure. A fine-MVP receiver attached
to an atomic supplier receives no closure offer until that supplier holds the
whole closure. Core and client policies can therefore be selected independently.
Clients only receive; they do not advertise or serve data.

Every offer pays announcement propagation, modeled as RTT/2. A chunk offer
permits requests for any subset of its transactions; it does not require
fetching the chunk as one payload. Offers received before the body is verified
can be remembered, but do not authorize closure requests yet. Offers remain
claims until the requested data verifies.

The EB body has an independent publication rule in both policies: a serving
node may offer it once the **complete body** verifies, even if it holds no
closure transactions. A body-only offer does not imply closure availability.

## Transaction-aware MVP

The following procedure is shared by `mvp-fine` and ordinary `atomic`. Only the
closure-offer eligibility rule in the table above differs.

The receiver keeps the verified body, its missing transaction set, eligible
per-peer offers and their arrival times, ordered request queues, transaction
ownership, active progress checkpoints, and a cumulative failure count per peer.
Queue ownership includes requests still propagating and requests waiting behind
the serving head.

### Body fetch and activation

By default, keep at most one logical body request active. Start with the first arrived
body offer. On retries, choose among eligible offerers by fewest cumulative
peer failures, then earliest body-offer arrival, then stable peer ID. The
simulator resolves exactly tied initial offers in upstream-list order.

The body is one indivisible verified object. Each request starts at byte zero;
a canceled body's received prefix is discarded. Continuing an open response
would preserve its progress, but the current policy cancels it on expiry even
when no replacement supplier is available. It does not retain that response
as a fallback or increase patience after repeated failures.

Once the body verifies, identify which referenced transactions are already
held locally and which need fetching. An entirely local closure still requires
the body before the receiver can declare complete EB possession.

```text
on a body offer, while the body is missing and no body request is active:
    request the body from the best available offerer
    start its deadline at request issuance

on expiry of the body request:
    count one failure for its peer and cancel the request
    discard its incomplete body prefix
    request a complete body from the best eligible offerer, if any
    otherwise wait until an offerer is eligible

on receipt of the complete valid body:
    mark the body verified and offer it downstream, if this node serves data
    establish the missing transactions using local possession
    consider the remembered closure offers under this receiver's policy
```

#### Light body hedging

With `--mvp-body-hedge 1`, ordinary atomic and fine-MVP use a persistent
**primary** body request plus at most one logically active **hedge**. The
option defaults to off and applies by receiver policy, including clients.
It does not change capped or atomic-all, nor closure fetching in any policy.

Issue the primary to the first arrived body offerer, breaking equal arrival
times by peer ID. Start its timer when issued, using the fresh-body allowance
defined in [Deadlines](#deadlines). Expiry enables hedging; the primary stays
open regardless of partial progress or whether another supplier exists.
A sole supplier can therefore finish its original response without restarting.

Keep the other body offerers in arrival order, breaking equal arrivals by peer
ID. Newly observed peers append once; repeated offers do not add entries.
When hedging is enabled and its slot is free, request from the first eligible
peer in this round-robin queue. A new offer does not preempt an unexpired hedge.

Each hedge uses its own peer's freshly calculated base allowance, multiplied
by `1.5^k`. Here `k` counts hedge timeouts across all peers for this receiver
and EB; the first hedge uses zero. Scale the whole allowance, including its
RTT terms. Cap the result at `--mvp-body-hedge-max-ms`, which defaults to
**2,000 ms per hedge attempt**. Reject a ceiling below any enabled peer's
unscaled base allowance. The ceiling is not a limit on total body-fetch age.

On hedge expiry, count one peer failure, cancel that hedge, increase `k`,
and return its peer to the queue's tail. Another eligible peer can be requested
immediately while the old response drains. Skip temporarily ineligible peers;
do not reissue to the same peer until its cancellation takes effect and its
old transport payload drains. A retry requests the complete body from byte zero.
Primary expiry, duplicate offers and cancellation after a winner do not
increase `k` or the peer's failure count.

```text
on the first body offer:
    issue the persistent primary and start its trigger timer

when the primary timer expires:
    enable hedging, leaving the primary open

while the body is missing and hedging is enabled:
    if no hedge is active and a queued peer is eligible:
        issue one whole-body hedge with that peer's bounded backoff allowance
    when the hedge expires:
        count its failure, cancel it, and return its peer to the queue tail

on a complete valid body response:
    accept it once and cancel the other active body requests
    retain their physical drain and activate ordinary closure fetching
```

A complete valid body from a retiring response can also win. Keep that
response's identity; never combine partial bodies or credit an old response
to a replacement request. An already recorded hedge failure remains recorded.
At a policy boundary, observable completions precede timeout decisions. Resolve
ties by completion time, issue time, then peer ID. This procedure needs only
whole-body completion notifications, not partial-byte progress measurements.

### Assignment and pipelining

For each unowned missing transaction, rank eligible offers by cumulative peer
failures, then that offer's arrival time, then stable peer ID. With equal failure
counts, the first eligible offer wins. An earlier offer of a different chunk
does not backdate this transaction's offer. Once assigned, a transaction keeps
its owner until completion or explicit withdrawal/reassignment; a later offer
cannot preempt it merely by appearing. A failed default checkpoint triggers
reassignment. Persistent checkpoints retain their original ownership on expiry.

```text
on a newly eligible offer, or when missing work loses its owner:
    consider eligible peer/transaction offers in ranking order
    for each transaction still missing and unowned:
        assign its owner immediately
        append its request to that peer's ordered pipeline
```

Append requests immediately, including when less than one initial congestion
window of work is available. There is no startup minimum, measured-rate placement,
worker role, probing, watermark or separate queue-depth cap. The missing set
bounds the total logically reserved work.

An early append can hide request propagation behind the sender's existing work.
Already queued successors need no new request handshake when the serving head
finishes. If the queue drains before the next request arrives, the resulting
idle gap still costs network time. Request and cancellation controls do not
change arriving payload instantaneously.

Atomic allows concurrent sourcing under the same ownership rule. However, the
first full-closure offer can claim every missing transaction at once. Later full
holders cannot take those reservations merely by appearing; concurrency arises
when distinct transactions are unowned or reassigned after failure.

### Progress checkpoints

Each peer has one active immutable checkpoint: a prefix of its requested
transactions selected against an independent **service byte target**. Publication
chunks determine when transactions can be offered; checkpoints determine how
much requested service must finish before a progress deadline.

Use `--fetch-checkpoint-kb 20` for the **20,000 B service target**, while
keeping `--chunk-kb 16.384` for publication. The checkpoint option defaults to
`0`, which inherits the nominal chunk target. Both fine-MVP and ordinary atomic
use this same checkpoint setting.

Take the shortest whole-transaction prefix reaching the checkpoint target, or
all available requested work if underfilled. Start immediately without waiting
for more work to accumulate. There is no fixed transaction count. The last
transaction may overhang the target, so actual checkpoint sizes vary too.

A checkpoint measures requested service, not completion of a particular
publication chunk. Its members may span chunks, and locally held transactions
are omitted from requests. If only a small transaction from a chunk is missing,
request that transaction alone. Keep the configured service target when selecting
the checkpoint; a publication chunk's overhang does not enlarge it. The deadline
uses the actual total bytes in the selected checkpoint, including its own closing
transaction's overhang.

Checkpoint size does not cap pipeline depth. Record the original members,
full transaction byte total, start time and deadline when the checkpoint becomes
active. Completions, partial progress and appends do not renew it. In the default
procedure, keep it while any member remains unfinished and requested; once all
members are complete or withdrawn, snapshot the next queued work. With persistent
checkpoints enabled, withdrawal does not satisfy an obligation: every original
member must verify. Future queued checkpoints do not start timers when their
requests are first appended.

For example, if only 8,000 B is queued when a checkpoint starts, that checkpoint
covers those 8,000 B. Appending another 32,000 B later does not enlarge it or
restart its clock. Once it is satisfied, the next checkpoint can cover the
remaining queued work.

### Persistent checkpoints and rotating hedges

Enable this procedure with `--mvp-checkpoint-hedge 1`. It applies to ordinary
atomic and fine-MVP receivers, including clients, independently of body hedging.
It is off by default; capped, atomic-all and legacy modes keep their own rules.

A checkpoint remains an immutable set of whole-transaction obligations. Its
original request is the **primary**. On its deadline, record one peer stall
and enable rescue while leaving that request and its queued work open. Partial
progress is retained in that same response. Without an eligible alternative,
the original download continues. Primary expiry does not increase hedge backoff.

Keep at most **one additional logical hedge batch per receiver**, alongside
ordinary concurrent primary pipelines. Choose stalled checkpoints in FIFO order,
skipping those without an eligible alternative. After each rescue batch ends,
an unfinished checkpoint goes to the back so others get a turn.

Each checkpoint has a round-robin queue of alternative offerers, initialized
by qualifying offer arrival and peer ID. Newly useful peers append once.
Request only the offered subset of that checkpoint's still-missing transactions.
A batch's membership and deadline stay fixed; later offers do not enlarge it.

The first implementation admits a hedge only to an **idle peer pipeline**.
An upstream already serving unrelated primary work keeps that work. Its rescue
opportunity waits until it becomes idle. Once a hedge starts, ordinary distinct
work may append behind it; the queued primary checkpoint starts its clock only
when it becomes the logical head. There is no hidden insertion ahead of a busy
peer's active checkpoint or queued tail.

Hedge timeout cancels only that named batch, counts one failure, grows backoff
and rotates its peer. When no retry cooldown is active, another eligible peer
may be requested immediately while old payload drains. The retiring peer waits for its old batch to retire before
being reused. A missing transaction has one primary and at most one wanted
hedge; older canceled payload can overlap physically and still consumes bandwidth.

Any complete valid transaction, including a late response, satisfies its global
obligation and withdraws redundant named copies. A subset batch can finish while
the original checkpoint remains incomplete. Cancellation alone satisfies neither.
Partial bytes from different attempts never combine.

If an expired checkpoint is completed with useful hedge delivery, the productive
hedge peer can take its primary's remaining tail transactions which it currently
offers. Prefer the largest unique contribution to that checkpoint, then completion
time and peer ID. Cancel the old named tail requests and issue fresh replacement
requests in the same decision pass, preserving unrelated work and destination
queue order. The old sender may already have transmitted some tail bytes; retain
their drain and accept useful complete late transactions. This handoff uses no
assumption about which bytes the sender has not yet sent.

```text
on primary checkpoint expiry:
    record its stall once; keep its original requests alive
    make that checkpoint eligible for rescue

when the extra slot and a useful idle peer are available, outside cooldown:
    request that peer's offered, still-missing checkpoint transactions
    snapshot its own allowance, including current backoff

on hedge expiry:
    count one batch failure and cancel only its outstanding attempts
    grow backoff, rotate the peer and release the logical slot
    after each group of five hedge failures, start the next retry cooldown

on a complete verified transaction:
    satisfy it globally once and withdraw its redundant requests
    credit its useful bytes toward backoff recovery

on checkpoint completion:
    consider productive handoff of its failed primary's remaining tail
    activate successor checkpoints only when they become the logical head
```

#### Cooldown after repeated hedge failures

Within persistent closure fetching, retry cooldown is enabled by default through
`--mvp-checkpoint-cooldown 1`. Count actual timed-out hedge batches per receiver
and EB. After each group of five, delay all new closure hedges as follows:

| Cumulative hedge timeouts | Cooldown before the next hedge |
|---:|---:|
| 5 | 1 s |
| 10 | 2 s |
| 15 | 4 s |
| 20 | 8 s |
| 25, 30, ... | 16 s |

Start the wait when the fifth failure is observed, before considering a
replacement in that decision pass. Fresh peers and recycled peers obey the same
gate. A multi-transaction batch counts once. Primary checkpoint stalls, body
events, successful batches and cancellation after fulfillment do not count.

Primary downloads and ordinary distinct assignments continue during cooldown,
as do verification, late replies, cancellation drain and productive tail handoff.
Only new extra hedge admission pauses. The deadline is absolute: time without
an eligible alternative still consumes the wait, and a peer arriving after
expiry does not earn another full delay or RTT allowance. Completing the EB
does not require waiting out an unused cooldown.

Successful service, new chunks, new checkpoints and peer changes do not reset
the group count or the exponentially increasing stage. Reset them for the next
EB. Useful bytes continue reducing the separate timeout multiplier below;
they do not forgive the cooldown history. The first three waits total 7 s and
the first four total 15 s, so repeated failures can sacrifice the 14 s recovery
objective to limit additional rescue traffic.

Use `--mvp-checkpoint-cooldown 0` to retain immediate eligible retries while
keeping persistent primaries and their other hedge rules. This switch is inert
when persistent checkpoint fetching is off and for capped, atomic-all, legacy
and body-only paths. No peer score, request deadline or sender TCP state is
changed by the cooldown.

#### Adaptive hedge patience

Maintain a receiver/EB-wide multiplier `F`, starting at 1. A hedge batch timeout
sets `F = min(FMAX, 1.5 * F)`. Each observation of `b` newly verified transaction
bytes sets `F = max(1, F * 0.7^(b / C))`, where `C` is the **nominal checkpoint
target**. The default `FMAX` is **16**, controlled by
`--mvp-checkpoint-backoff-max`. Peer failure counts remain separate and do not decay.

Count each useful transaction's actual bytes once, from primary, hedge, WAN,
local-fluid or valid late delivery. Initial/mempool possession, duplicate copies,
partial bytes and body traffic earn nothing. Count useful queued-tail transactions
even if one transport grant carries them beyond the current checkpoint. A small
underfilled success earns a proportional decrease; success while `F=1` banks no
credit. A timeout followed by a checkpoint target's worth of useful service leaves
`F` approximately 1.05 if neither cap intervenes.

New hedge patience is `min(hedge ceiling, own-peer base allowance * F)`, using
its actual requested bytes and the deadline rules below. The ceiling defaults
to **2,000 ms** via `--mvp-checkpoint-hedge-max-ms`; configurations whose ceiling
could shorten an unscaled base are rejected before simulation. Pressure changes
affect future requests only. Observable completions precede timeout decisions;
a late completion does not erase a failure already recorded at an earlier boundary.

Use `--mvp-checkpoint-hedge-slots 0` with the feature enabled to test primary
persistence alone. The default is `1`; this is an extra-batch limit, not a cap
on ordinary concurrent sourcing or queued primary work.

### Deadlines

Use `--min-peer-rate-mbps` as the receiver's service benchmark. A positive value
sets the expectation; zero uses **1.5 Mb/s**. It reserves no sender bandwidth.
For the body, `bytes` is the whole body length. For closure progress, it is the
active checkpoint's original transaction byte total, rather than the closure's
whole size or the number of chunks touched.

Let `T = 8 * bytes / minimum_rate_bits_per_second` and
`R = min(conservative_RTT_estimate, RTT_policy_cap)`. The RTT policy cap defaults to 500 ms.
For the simulator's configured uniform WAN jitter `j`, use its static p90:
`conservative_RTT_estimate = base_RTT * (1 + 0.4 * j)`. Thus `--jitter 0.2`
gives a 1.08 multiplier, while no jitter gives 1. Compute the multiplier once;
no rolling quantile calculation is needed. Local fluid links use their base RTT.
This affects receiver patience, while transport timing and request/cancel
propagation retain their existing definitions.
Starting from a fixed 14,600 B reference window, separate from the nominal
chunk target, choose the smallest positive `K` satisfying
`14,600 * (2^K - 1) >= bytes`. The cold allowance is
`min(cold_budget_cap, K * R)`, with a default cold-budget cap of 500 ms.

Two reference windows cover 14,600 + 29,200 = **43,800 B**. With a 20,000 B
checkpoint target and the sampled workload's 16,384 B maximum transaction,
the largest possible selected prefix is 36,383 B, still within two windows.
If the actual selected prefix exceeds 43,800 B, its cold calculation requires
at least three windows. This allowance applies only to fresh or drained queues;
rolling over a checkpoint on a continuously fed pipeline does not earn another
cold allowance.

| Activation of the body request or closure checkpoint | Patience from activation |
|---|---|
| New request on an empty/drained queue | `R + max(T, cold allowance)` |
| Successor already queued at the sender | `max(T, R)` |
| Pending append or replacement on an active flow | `R + max(T, R)` |

At a 1.5 Mb/s expectation, an exactly 20,000 B checkpoint has `T` of about
107 ms; transaction overhang increases it. The activation rules above then
apply the appropriate RTT allowance. The independent checkpoint setting does
not change the body's size-based deadline.

The body clock begins when its request is issued. The first closure checkpoint
begins when assigned; later checkpoints begin when their work becomes the logical
head. Persistent checkpoints require verification of their original members;
default checkpoints can also end on withdrawal. There are **no completion extensions**. The DES permits
one bin of numerical tolerance when checking expiry. Small checkpoints still
receive bounded RTT patience, so this is not a strict sustained-rate guarantee.
The caps bound receiver patience; actual network propagation is not shortened.
There is no additional whole-queue or aggregate service deadline.

### Sender connection state

The body and closure use the same persistent sender connection when fetched
from the same peer. Transitioning to closure fetching preserves its congestion
window, slow-start threshold and congestion-control history. It still pays
request propagation before closure bytes can arrive. A small body may leave
the window close to its initial size: the sender grows it from actual payload
ACK feedback, not from the receiver issuing requests or completing checkpoints.

With the default `--tcp-idle-restart 1`, a WAN sender that has transmitted no
payload for more than one transport RTO restarts at the smaller of its current
and initial windows when service resumes. Outstanding transport work or
recovery prevents this idle restart. A zero-bandwidth poll does not count as
activity or commit a restart. `--tcp-idle-restart 0` disables this behavior.
Local BP-relay links use their separate fluid transport.

The receiver's **cold allowance** is a patience rule based on request-queue
state. Granting that allowance does not reset the sender's TCP window, and a
fresh application request does not necessarily mean a cold TCP connection.
Transport recovery and idle restart use the transport's own RTO; the receiver's
500 ms patience caps do not shorten it. See the
WAN transport mechanics (`ouroboros-network/smol-world/mechanics.md`)
for the detailed model.

### Production wire-order liveness and simulator limits

The core persistent-body policy above is already implemented in
`BodyFetch` (`ouroboros-network/smol-world/src/SmallWorld/BodyFetch.hs`) and exercised by
`BodyFetchProps` (`ouroboros-network/smol-world/test/BodyFetchProps.hs`): keep the primary, use one rotating
hedge, preserve retry backoff across peers, and accept a complete valid late body.
The node integration exposed additional wire-level obligations that the DES does
not represent explicitly. They are production requirements, rather than proof
that those cases were covered by the simulator.

In a production implementation:

- Keep an unfinished FIFO reply's original fixed deadline even if its data was
  acquired elsewhere or its logical request was withdrawn. A committed response
  still blocks later replies until its terminal arrives. If that head expires,
  stop appending distinct work to the upstream and permit bounded rescue of
  still-needed queued work elsewhere. Do not start queued service clocks early
  or count another failure against the already fulfilled/withdrawn request.
- A cancellation acknowledgment is a separate FIFO reply. Start its soft guard
  only when it reaches the head, using one snapshotted conservative RTT. Time
  behind earlier data does not consume that guard. Expiry enables blocked-stream
  rescue without a payload-failure/backoff penalty or duplicate cancellation.
- A cancellation names its original request. A responsive server finishes a
  committed response or returns its canceled terminal, then continues with
  unrelated queued requests. Admission reservations remain charged through the
  original terminal and the cancel ACK when one was issued; logical reassignment
  alone does not refund them. Accepted storage can outlive wire retirement.
- The initial node protocol cancels whole immutable checkpoint batches. It can
  accept complete verified TXs incrementally, but cannot edit an in-flight batch
  or combine partial TX/body payloads from separate attempts.
- Record the first verified whole-body source independently of asynchronous
  durable-write and typed-terminal callbacks. A fast storage commit must not
  cause loser cleanup to cancel its own winning original. This is extra node
  bookkeeping; the DES already observes complete-body winners before its timer
  decisions.

A body may wait behind transactions from another EB or a cancel ACK. Two EB-body
requests can share a pipeline only if reservations fit: the proposed 600,000 B
trusted body reservation and 1 MiB per-peer budget permit just one such body at a
time. Small node test fixtures use a smaller trusted ceiling to exercise the
two-body case as well.

The relevant node-contract clauses are §8, “Agreed liveness rule for an
unfinished data reply” and “Agreed soft guard for a cancel acknowledgment at the
FIFO head,” in `ouroboros-consensus/fetching/cancellation-contract.md` in the
sibling checkout. Its timing tests are named “logical completion retains the
head clock and permits queued rescue” and “ACK guard starts at head and does not
charge data failure.” The body integration also tests a body queued behind
another EB's stalled TX reply and a retiring committed body's separate original
and ACK drainage.

| Boundary | Current DES | Production contract |
|---|---|---|
| Concurrent EBs on one connection | The fetch engine runs one EB at a time; `t22rounds` repeats runs with selected carried state. | Body, TX and cancel replies from different EBs can share one FIFO and its budgets. |
| Cancellation and committed messages | `FetchUnits.applyUnits` (`ouroboros-network/smol-world/src/SmallWorld/FetchUnits.hs`) removes named unit attempts when the modeled cancellation takes effect; already exposed transport payload can still drain. | A committed protocol response cannot be interrupted; its terminal can remain the blocking FIFO head. |
| Cancel ACK | No separate LeiosFetch cancel-ACK message/slot/guard. TCP transport ACK accounting is a different mechanism. | Keep original and cancel-ACK obligations distinct, including their timing and reservations. |
| Checkpoint versus wire completion | Logical checkpoints/episodes can finish when their obligations are verified or, in default mode, withdrawn; physical drain is accounted separately. | Finishing an obligation does not establish wire-head retirement. Retain the head liveness guard. |
| Cancellation granularity | Fine attempts are individually cancelable TX units. | The first node protocol cancels the entire named checkpoint batch and rebuilds replacements from still-missing TXs. |
| Per-attempt hedge ceiling | Body/checkpoint configuration rejects an unscaled base above its configured ceiling. | Use `min(base * F, max(base, ceiling))`, preserving a larger base. This is an agreed change, not yet a simulator-code update. |
| Timing observations | Receiver-time request propagation, logical-head activation and one-bin expiry tolerance. | Ordinary send-return and validated terminal input-span timestamps determine FIFO activation; later callbacks cannot renew a deadline. |

Simulator follow-ups, before claiming direct equivalence on these corners:
model committed batch responses and cancel ACKs, exercise overlapping EBs on
shared connections, and align the body/checkpoint ceiling helpers and validation
with the base-preserving rule. The current DES results still measure its stated
abstractions; they do not validate these additional production wire-liveness
cases. No simulator algorithm or saved experiment result changed with this note.

### Failure, cancellation and late replies

With light body hedging disabled, expiry of a body request counts one peer failure.
Cancel that request and
select a replacement from eligible body offers using the same ranking. There
remains only one logical body request; old payload may still be draining.
The same peer becomes eligible for retry after its cancellation takes effect;
another eligible peer may be requested while that old response drains.
In that default procedure, canceled body replies do not establish a valid body, even if they subsequently
reach the full body length. A retry always requests the whole body again.

With persistent checkpoints disabled, expiry counts **one** peer failure and cancels that
peer's **entire unfinished queue**, including tails outside the checkpoint.
Release its transaction ownership immediately and reassign still-missing work
using the usual offer ranking. Do not charge failures separately for unsent
tails or repeatedly on every timer poll. Keep verified transactions already
obtained from that peer.

Failed peers remain eligible for recycling; there is no permanent zero-byte
blacklist. If no eligible source offers the missing data, wait for an offer.
Increasing failure counts cannot create unavailable data or guarantee capacity.
With light body hedging disabled, a sole body supplier can be canceled and retried repeatedly if
its available service cannot meet the unchanged body deadline.

A fine-MVP or atomic transaction response can still be useful after its request
was canceled. Accept a **complete valid transaction** if it is still missing,
then withdraw its replacement request on the next planning pass. Withdraw only
those named attempts; preserve unrelated requests and their progress. Partial
bytes from different attempts never combine, and canceling an old attempt
cannot cancel a later reissue of the same transaction.

```text
on received transaction data:
    attribute bytes to the exact request attempt
    if one attempt completes a valid transaction still missing:
        mark the transaction held, including if this is a late reply
        withdraw any replacement for it while preserving unrelated requests
    keep the current checkpoint unless all its members are satisfied or withdrawn

on expiry of a closure checkpoint, with persistence disabled:
    count one failure for its peer
    cancel its entire unfinished queue and release all its ownership
    assign still-missing, unowned transactions through eligible offers
    append new assignments and establish their next checkpoints
```

For example, with persistence disabled, A can initially own every missing transaction in atomic mode. If A
verifies a prefix and then stalls, B or C can receive the still-missing suffix
when A's checkpoint expires. Already verified transactions are not requested
again. If a complete late transaction from A arrives during replacement, keep
it and withdraw only its corresponding replacement attempt.

Cancellation has propagation delay, so old and replacement payload may overlap
physically despite single logical ownership. All such traffic still consumes
network capacity and counts in the simulator's byte accounting.

### Completion and forwarding

A node holds the EB once its body and all required transactions are verified.
It then stops assigning new closure work and withdraws outstanding requests.
Previously sent payload can still drain until cancellation takes effect.

Before full completion, fine-MVP serving nodes may already forward verified
chunks. Atomic serving nodes wait for complete closure possession. Both may
forward their complete body independently. Receiver-only clients never forward.

## Initial possession workload

In `t22rounds`, `--fetch-initial-held-frac F` supplies an exact-count deterministic
scattered set of initially held transactions to each honest non-seed receiver.
The default is zero. For variable sizes, the fraction concerns transaction
**count**, not bytes; missing bytes are the actual sum of the unheld transactions.
The library also accepts an explicit per-receiver transaction set.

Initial transactions become available to this EB's completion and relay logic
only after its body verifies. The body still contains every transaction
reference, and a chunk with missing members cannot be advertised. Local
possession earns no upstream service credit. Use `--overlap 0` with these
transaction policies rather than a scalar overlap discount.

Initial possession changes fetching requirements. Validation and deadline
readouts have their own configured CPU model; see
transaction geometry (`ouroboros-network/smol-world/variable-transaction-geometry.md`) and
sampled workloads (`ouroboros-network/smol-world/mainnet-tx-workload.md`) for the workload/accounting contract.

## Simulator selection

Select the current procedures in `t22rounds` with `--core-policy mvp-fine` or
`--core-policy atomic`. When clients are present, choose their procedure
independently using `--client-policy mvp-fine` or `--client-policy atomic`.
The core selector applies to BPs and relays on a pool topology.

`--chunk-kb 16.384` sets the nominal publication-chunk target to 16,384 B,
and `--fetch-checkpoint-kb 20` independently selects a 20,000 B service target.
Neither setting requires fetching an entire publication chunk in one request:
queues contain individually selected whole transactions. Use
`--tx-size-model mainnet-lognormal` with
an independent `--tx-size-seed` to sample variable transaction sizes. The
resulting closure payload, chunk count and actual chunk sizes can differ from
a fixed-size division because transactions are indivisible.

Keep `--chunk-hash-us-per-kb 0` and `--fetch-pipeline-max-chunks 0` with these
transaction policies. Count cap zero does **not** disable pipelining. Worker
queue limits, measured-rate watermarks, expected-rate/slack, worker redundancy,
endgame and completion-extension knobs do not govern these two procedures.
Use `--mvp-body-hedge 1 --mvp-body-hedge-max-ms 2000` to enable the light body
procedure. The separate `--body-hedge` switch does not enable body races for
ordinary atomic or fine-MVP.
For persistent closure fetching, independently add
`--mvp-checkpoint-hedge 1 --mvp-checkpoint-hedge-slots 1`
`--mvp-checkpoint-hedge-max-ms 2000 --mvp-checkpoint-backoff-max 16`
`--mvp-checkpoint-cooldown 1`.
The legacy `--repick` option cannot be combined with persistent checkpoints.
Service qualification and ranking-credit settings also belong to capped and
do not change fine-MVP or ordinary atomic peer selection. The independent
checkpoint option is shared; their simpler failure-count ranking remains intact.
Their queue applicability is summarized in the
mechanics reference (`ouroboros-network/smol-world/mechanics.md`).

For a small honest producer comparison, with no clients:

```bash
for policy in mvp-fine atomic; do
  smol-world t22rounds --core-policy "$policy" \
    --nodes 20 --valency 4 --egress-mbps 200 --share-model rtt \
    --tx-size-model mainnet-lognormal --tx-size-seed 1 \
    --eb-mb 0.262144 --chunk-kb 16.384 --ref-bytes 36 \
    --chunk-hash-us-per-kb 0 --fetch-pipeline-max-chunks 0 \
    --fetch-checkpoint-kb 20 --min-peer-rate-mbps 1.5 \
    --overlap 0 --fetch-initial-held-frac 0 \
    --q-place single --t22-seed-at 0 --adv-stake 0 --seed 1 \
    --eb-rounds 1 --infect-horizon 7 --fetch-phase-stats 1
done
```

This example explicitly selects a 20 kB checkpoint target and a 1.5 Mb/s
service expectation. Use `--fetch-checkpoint-kb 0` to recover the chunk-derived
checkpoint target for comparison.
Set `--fetch-initial-held-frac 0.9` to exercise scattered local possession with
the same body and offer rules.

## Related policies and validation

The checkpoint implementation plan (`ouroboros-network/smol-world/light-checkpoint-hedging-plan.md`) records
the reviewed scope and controls. Checkpoint pilot results (`ouroboros-network/smol-world/benchmark-logs/checkpoint-hedging-pilot/README.md`)
record the lifecycle, accounting and small-topology tests.
The cooldown plan (`ouroboros-network/smol-world/checkpoint-hedge-cooldown-plan.md`) and its
controls (`ouroboros-network/smol-world/benchmark-logs/checkpoint-cooldown-pilot/README.md`) record the
five-failure gate, independent replay and comparison controls.

The light body-hedging plan (`ouroboros-network/smol-world/light-body-hedging-plan.md`) records the integration
contract and validation cases. Pilot controls (`ouroboros-network/smol-world/benchmark-logs/light-body-hedging-pilot-v2/README.md`)
cover persistence, rotating retries, complete late replies, local possession,
mixed policies, transport impairments and tracing parity.

The separate atomic-all variant (`ouroboros-network/smol-world/atomic-all-fetch-policy.md`) shares atomic's
body and full-coverage offer rules but requests from every eligible source and
preserves issued duplicate batches through drain. Its no-cancel lifetime is not
part of ordinary atomic or fine-MVP.

Implementation controls and audits (`ouroboros-network/smol-world/atomic-all-results.md`) include unchanged
ordinary atomic/fine-MVP comparisons, mixed core/client modes and variable-size
transactions. Detailed workload semantics are in
transaction geometry (`ouroboros-network/smol-world/variable-transaction-geometry.md`).
