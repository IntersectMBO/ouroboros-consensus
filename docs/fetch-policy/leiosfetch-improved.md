# LeiosFetch receiver policy

Status: under implementation. This document records the intended behavior of
ordinary LeiosFetch while its staged implementation is verified. Individual
sections become implementation descriptions only after their code and tests
land. The wire protocol, server and whole-response decoder remain unchanged.

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
A job containing an indivisible transaction may exceed its byte target and
must still fit the peer's credit and protocol bounds.

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
sources by their effective stall penalty, offer arrival time and stable original
connection identity. Capture arrival times independently for body and closure
offers, including offers learned through ChainSync.

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
