# LeiosFetch receiver policy

This document describes the ordinary LeiosFetch receiver implementation. It
uses the existing wire protocol, server and whole-response codec, with bounded
requests, local transaction reuse and progressive recovery from slow sources.

## Purpose

Fetch valid EB bodies and their missing transactions while limiting outstanding
work and recovering from slow sources. Reuse local transactions before issuing
network requests. A timeout permits another source to help; an outstanding
request keeps its reservation until its physical exchange settles or its
connection closes.

## Offers and local possession

An EB body offer says that the peer holds the complete body. It says nothing
about closure possession. A full-closure offer separately authorizes requests
for the EB's transactions.

After validating and storing a body, first use the database's existing fills
from confirmed local source locations, then make one bounded pass over matching
mempool transactions. Match the exact serialized transaction's hash and size.
Only confirmed target presence removes transactions from network work. Later
mempool arrivals may still be fetched: local discovery is one pass.

Keep upstream election focus, certified-EB selection, ChainSync offers and
locally forged acquisitions. These determine which EBs matter; source ranking
chooses among the peers offering the selected work.

## Requests and admission

The body and transaction requests have independent per-peer limits, shared
across concurrent EBs. Admit a whole job before queueing it. Each TX request
contains one immutable job, including its original offsets and commitment.
A transaction larger than the target forms a one-transaction job. Otherwise
pack whole transactions without crossing the target. Every complete job must
fit the peer's credit and protocol bounds.

| Starting setting | Default |
|---|---:|
| Regular peer TX credit | 262,144 B |
| Big ledger peer TX credit | 262,144 B |
| Whole-transaction job target | 65,536 B |
| Outstanding bodies per peer | 4 |
| Minimum service benchmark | 300,000 bit/s |

Queued work already consumes its reservation. Waiting for a pipeline slot or
another copy winning does not refund a sent request. Skip wholly obsolete work
before dispatch and refund that unsent reservation exactly once.

## Source selection and progressive recovery

Consider EBs in consensus priority order. Within a selected EB, rank eligible
closure sources by their effective stall penalty, first full-closure offer time
and stable original connection identity. Body selection preserves upstream
peer-map order. Capture arrival times independently for body and closure offers,
including offers learned through ChainSync.

Initially request unassigned work once. When every outstanding copy of some
still-needed work has exhausted its own eligible service allowance, permit one
additional copy from another eligible source. The newly queued copy closes that
recovery opportunity until it too stalls. There is no fixed two-copy policy;
per-peer body/TX budgets and same-peer exclusion continue to apply.

Service evidence belongs to a live registration. Qualifying head stalls add a
penalty after aging existing evidence. Its half-life is 600 seconds, with scores
at or below 0.5 forgiven. Successful service does not reset it. Removal ends
that registration's score; stale timestamps cannot increase saved evidence.

## Timing and FIFO eligibility

Use the original connection's KeepAlive RTT observation, with bounded fallback
and startup allowances. Freeze timer inputs for the attempt. A follower pays no
peer-service time while an earlier response or our own local handler prevents
it from becoming the eligible FIFO head. Accurate complete-message observations
separate network arrival from later decode/handler processing.

A soft deadline opens progressive recovery and retains the original request.
The existing fatal driver guard remains the hard failure boundary. There are no
wire cancellations or partial transaction salvage in this policy.

The coordinator wakes on coalesced readiness, the earliest active deadline, or
a maximum 500 ms idle fallback. The fallback is not a minimum planning interval.

## Ownership and shutdown

Use original registration, client invocation and a checked local ordinal to
identify each request. These are local correlation facts, not wire fields.
Late callbacks cannot release a replacement request's credit or modify a new
registration. Logical jobs and physical outstanding exchanges have distinct
ownership.

A complete-message observation stops network timing; it does not by itself
return credit. Return credit when a validated reply's storage submission is
accepted, or when its handler finishes without submitting work. Accepted receipt
ownership remains until its callback settles.

A handler owns validated work while submitting it. Transfer accepted work to a
node-owned receipt task before interruption or post-acceptance tracing can lose
its owner. Peer teardown does not erase an in-progress submission or accepted
receipt. Complete only fully confirmed immutable jobs; relevant unconfirmed jobs
remain fetchable.

Normal shutdown stops admission and optional local imports, wakes local waiters,
and joins submitting handlers and accepted receipt callbacks before storage
closes. Exceptional shutdown cancels and joins remaining work. Actual storage
failures reach node supervision; peer protocol faults retain their original
connection ownership. One failed callback must not cause repeated exception
delivery during teardown.

## Durable confirmation and garbage collection

A receipt confirms presence at its successful write boundary, including bytes
already present. It adds no pin lasting until notification consumption; existing
certified-data pins remain unchanged. Reject logical updates to retired work.
Notification consumers and caches use their monotonic retirement frontiers, with
pruning before physical GC. Late cache publication cannot replace a newer usable
transaction source with a retired location. A stale guarded DB fill is a miss,
not target possession.

## Tracing

Keep ordinary operator milestones and compact durable acquisition summaries.
Use one separate detail tracer for high-frequency policy observations. Emit
outside STM and shared locks, after detaching compact values from payloads and
large state snapshots. Count distinct fetched transactions separately from
locally reused bytes. Network/mux tracing remains the source of wire traffic.

Routine client events record blocked/resumed sources, progressive hedge causes,
body writer acceptance, exact penalty charges and invocation termination. A
body-winner event marks writer acceptance; durable acquisition remains a separate
milestone. Detail events carry the original invocation/ordinal, frozen RTT and
startup inputs, selected score and first full-closure offer, FIFO activation,
request phases and changes in credit pressure. Logging never refreshes these
observations from the current connection or score.

The closure summary counts positively confirmed target offsets once. Its local
bytes include DB/mempool fills and bytes supplied by our own forge. Its network
bytes describe useful validated acquisition, including confirmed target presence
that another write may already have established; they are not a count of first
SQLite insertions. The summary is emitted once at semantic durable completion.
Unknown acquisition origins remain unknown. Arrival buckets, duplicate completed
responses and mux wire totals answer different questions and must not be added
to this distinct numerator.

## Reading the implementation

| Component | Responsibility |
|---|---|
| `Kernel` | Election focus, peer registrations, availability, first-offer times, shared outstanding-state lock and the event-driven planning loop. |
| `Decision` and `Jobs` | EB priority, ranked source traversal, whole-TX packing, least-requested job selection and logical completion/unpicking. |
| `Runtime` and `RequestState` | Original request identities, physical outstanding credit, FIFO/send/receive facts and pending submission/receipt claims. |
| `Timing`, `PeerPenalty`, `Wait` | Pure service clocks and aged evidence, plus coalesced readiness/deadline/fallback waiting. |
| `Client` and `Client.Replies` | Existing pipelined protocol interpreter, final dispatch recheck, validation and storage submission/settlement. |
| `LocalReuse` and `ReceiptTasks` | Bounded local materialization and node-owned producer/callback lifetimes. |
| `Coverage`, `Trace`, `Trace.Policy` | Confirmed acquisition origins and detached production observations. |

The outstanding-state lock serializes logical planning and removal. Physical
facts live in STM. Admission publishes an immutable descriptor, its credit and
the existing peer command queue under an exception-safe lock-to-STM boundary.
There is one command TVar, exposed through both the peer registration and runtime;
the send, FIFO and installed-receiver queues describe different protocol facts.

A reservation means outstanding request debt, including queued work. It does
not allocate a receive buffer or reserve database storage. Pending ownership
means that one handler or accepted receipt is responsible for the local write;
it is independent of the physical request's credit. A registration identifies
one original peer lifetime, an invocation identifies one client run, and a local
ordinal identifies its immutable request. None adds a wire sequence number.

## Patience calculation

Let `R` be the frozen RTT allowance. A positive connection-owned KeepAlive sample
no older than 30 seconds gives `min(0.5 s, 1.5 × sample)`; a missing, stale,
non-positive or future sample gives `0.5 s`. The source reader is captured for
the original connection, without switching old requests to a replacement's RTT.

For requested payload `B` and service benchmark `v` bits/s, transfer patience is
`T = 8B / v`. With reference initial window `I = 14,600 B`, take the smallest
positive `K` such that `I × (2^K − 1) ≥ B`, and cold patience
`W = min(0.5 s, K × R)`. These are receiver policy references, not observations
of the server's actual TCP congestion window.

Only an eligible FIFO head gets a deadline. Its start is the maximum of send
return plus `R`, the preceding response's original terminal receipt and local
handler readiness, omitting unavailable predecessors. Add `max(T, W)` for a
fresh request, or `max(T, R)` for a continuous pipeline. A later RTT sample or
payload dribble never renews this allowance. An early reply may precede send
return; reconciliation preserves its original receipt and releases no credit
twice. Local handler/storage time delays eligibility of the following head.

While known input is being decoded locally, defer expiry reconciliation: that
input may establish an earlier complete-message receipt. This does not extend
the absolute deadline. If decoding requires more channel input, the original
deadline becomes eligible again immediately.

At expiry, retain the physical request and consider progressive recovery. The
source's actual eligible head can earn one closure-service penalty only while
its job remains needed and no local submission/receipt already owns that work.
Bodies and followers blocked behind another response do not earn that penalty.
A terminal receipt clears wire blockage even while its local handler runs.

## Local materialization and confirmation

The Leios transaction cache supplies reusable bodies and confirmed transaction
locations. The body writer fills target offsets from usable stored locations,
then the bounded mempool pass imports exact matching serialized transactions.
Materialization uses a 20,000 B target and at most 128 entries per slice, allowing
one indivisible transaction to overhang. A shared gate bounds optional local
work; no full mempool snapshot or payload vector is retained across its IO.

The writer's receipt states which submitted target offsets are present at the
successful transaction boundary, including rows already present or held in the
immutable partition. Submission alone is insufficient. Fully confirmed immutable
jobs retire; partly confirmed relevant jobs remain fetchable. The locally forged
path also requires every reference to be confirmed before announcing acquisition.
An incomplete forge receipt cannot establish acquisition. A callback for a
point already pruned from fetching cannot recreate coverage.

This implementation uses the ordinary writer and its existing queue. It does
not introduce an additional storage handoff or capacity reservation. Actual
storage failure reaches node supervision; invalid network data belongs to the
original peer. Normal drain joins submitting producers and accepted callbacks;
exceptional drain cancels and joins them. Neither peer removal nor a failed
post-acceptance observation transfers an old claim to a replacement request.

## Practical limits and follow-ups

Responses are decoded as complete protocol messages. TX requests contain one
whole-transaction job; the policy does not salvage an incomplete message or
retain partial body bytes. Recovery leaves sent copies outstanding until reply
or connection teardown. Per-peer limits bound their credit, rather than imposing
a global duplicate-byte or fixed-copy ceiling.

The mempool pass is one-time discovery. Later local arrivals are not a continuous
resolver. Election/chain urgency stays with consensus focus; source ranking
operates inside that priority. Service settings and the ten-minute aging half-life
are initial tuning choices. The ordinary accounting scripts and selected IOSim
race checks are retained; a broader generated integrated-campaign framework is
separate future work.
