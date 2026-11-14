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

After checking a complete body's hash and trusted size bounds, transfer useful
work to the shared bounded body-preparation pool. Select matching mempool TXs
once, capturing their validation version in the same STM transaction. The
current index read waits while a mempool writer owns its state; this wait
occupies a bounded preparation worker, outside fetch/cache locks. Closing
wakes the transaction and allows optional reuse to be skipped. Project the
selected immutable transaction views to serialized bytes and verify their actual
hash and size outside locks. Prefer these verified mempool bytes; use DB fill
proposals for the other offsets. An unverified
mempool hit must not displace a usable DB fill.

Write the body, remaining DB fills and verified mempool bytes in one transaction.
Only the receipt's confirmed target offsets remove transactions from network
work. Stale fills are misses. Later mempool arrivals can still be fetched: there
is no second scan or continuous resolver. Full-closure offers independently
authorize requests for the remaining transactions.

Keep upstream election focus, certified-EB selection, ChainSync offers and
locally forged acquisitions. These determine which EBs matter; source ranking
chooses among the peers offering the selected work.

The ledger can disable non-empty EB capacity when the committee's registered
voting keys cannot reach its configured quorum. A diffusion-only fixture without
those keys must label any quorum override explicitly; it is not certification
validation or a change to production quorum policy.

## Serialized bodies and local validation

The upstream wire codec carries the body's original bytes as CBOR-in-CBOR. Hash
those exact bytes and decode them with the era-aware inner decoder. Inspect the
validation result before taking the outstanding-state lock; on success, force
the decoded body and its size metadata before admission. Invalid inner encoding
or trusted-bound violations suppress repeated fetching without announcing an
acquisition. A valid body offered at the wrong size is still preserved before
reporting that peer's error.

The decoded body retains its original encoding, so queued work needs no second
serialized copy. The plain codec is adapted once at the observed driver. The
complete outer message's original receipt remains the network timing event;
inner decoding is local handling. The body-message limit includes ten bytes of
outer framing; the inner payload ceiling is unchanged.

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
| Shared outstanding requests per peer | 150 |
| Minimum service benchmark | 300,000 bit/s |

Queued work already consumes its reservation. Waiting for a pipeline slot or
another copy winning does not refund a sent request. Skip wholly obsolete work
before dispatch and refund that unsent reservation exactly once.

The physical lease ledger maintains the shared request total alongside TX bytes
and body count. Queued and sent bodies and TX jobs all count, including zero-byte
jobs. Successful reservation increments it; only removing the exact live lease
decrements it. Repeated or old-invocation callbacks cannot refund a newer request.

The ledger also indexes each body or TX job by its live owners, retaining one
exact attempt key per original peer. Queued requests enter this index when
reserved. Successful release removes the exact lease and its owner entry
once; removing a peer drops its entries while retaining other owners. The last
owner removes the work's index bucket. Same-peer exclusion spans invocations,
so late callbacks cannot erase replacement ownership. Settled descriptors
retained for send-return without a lease are absent from the index.

Owner queries, copy counts and same-peer checks use this index rather than
scanning all leases. Pending local writes and forge claims remain separate
eligibility checks; peer blockage is read from current FIFO facts.

Admission returns its refusal reason from the same transaction that checks
eligibility and reserves credit. Temporary TX-credit pressure differs from a job
that exceeds the peer's whole budget. Closed or blocked sources, ineligible or
same-source work, protocol-invalid jobs, and exhausted byte/body/request limits
remain distinct. Only the actual temporary limit refusal supplies a pressure
observation; no second scan reconstructs that decision.

## Source selection and progressive recovery

Consider EBs in consensus priority order. Within a selected EB, rank eligible
closure sources by their effective stall penalty, first full-closure offer time
and stable original connection identity. Body selection preserves upstream
peer-map order. Capture arrival times independently for body and closure offers,
including offers learned through ChainSync.

Preserve least-requested job selection within each EB: unrequested jobs precede
eligible rescues, and fewer existing copies rank first among rescues. A relevant
job that fits a peer's total TX credit but exceeds its current remainder stops
further TX assignment to that original peer invocation for this planning pass.
The stop covers smaller jobs in the same EB and all later EBs; it creates no
extra reservation or failure penalty. Bodies and other sources remain available.
An exhausted shared request count or closed/blocked source stops both request
kinds; an exhausted body count stops only bodies.

Individual ineligible/same-source, permanently unfit and protocol-invalid jobs
are skipped without blocking other candidates. Do not exclude zero-byte jobs
merely because free TX bytes are zero; they can fit if no earlier priority
barrier applies. Empty job pools are skipped before ranking, including terminal
unavailable pools, without treating their emptiness as acquisition evidence.

Stops expire at the end of the pass. If another source takes a recorded barrier
and makes it ineligible during that pass, publish one coalesced readiness signal
to reconsider the stopped peer's credit. An unchanged no-progress pass does not
notify itself. Runtime admission still rechecks live eligibility and credit
atomically; pass-local stops are advisory and never extend across invocations.

Initially request unassigned work once. When every outstanding copy of some
still-needed work has exhausted its own eligible service allowance, permit one
additional copy from another eligible source. The newly queued copy closes that
recovery opportunity until it too stalls. There is no fixed two-copy policy;
per-peer body/TX budgets and same-peer exclusion continue to apply.

Service evidence belongs to a live registration. Qualifying head stalls add a
penalty after aging existing evidence. Its half-life is 600 seconds, with scores
at or below 0.5 forgiven. Successful service does not reset it. Removal ends
that registration's score; stale timestamps cannot increase saved evidence.

## Readiness and effective changes

Readiness remains a coalescing dirty bit, alongside absolute deadlines and the
500 ms maximum idle fallback. Each assignment pass visits the full consensus-
ordered tracked set, skipping unfocused points and empty terminal pools. Publish
effective state changes before signalling.
Required runtime and offer/frontier signals share the publishing STM transaction.
Do not use a debounce interval or rely on the fallback to repair a
missing signal.

Absent lease releases, unchanged or obsolete pending-owner cleanup, duplicate
forge claims, and unchanged/pruned offers do not create fresh readiness. Compare
only cheap operation outcomes or the individual offer/time entry. A no-op cannot
clear readiness already set by a real change.

Dispatch commitment reserves no new credit and activates no deadline, so it
does not notify. Send return, terminal receipt, handler/FIFO completion and local
decode exit retain their timing wakeups. Priority-barrier reconsideration has
its own conditional signal, independent of dispatch. Pruning signals when its
frontier is published so changed work relevance is reconsidered promptly.

Physical cleanup and logical job release remain distinct. Kernel unregister
must still signal after an earlier client finalizer removed the session and
credit. Likewise, submission/receipt settlement explicitly signals its logical
unpick/acquisition even when the physical release or exact-owner cleanup is a
no-op. Cache, metadata-worker and body/receipt notifications are retained where
they can accompany those broader effects.

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

Announcement acceptance and a bounded node-owned bookkeeping obligation commit
in one STM transaction. A full metadata queue rolls back acceptance; disconnecting
the announcing peer cannot erase admitted work. The metadata worker installs election/fetch state without waiting for database
submission or retaining a cache point.
It retains each obligation until that bookkeeping completes.

Metadata and known forged payloads retain separate queues, bounded at 128 and
four queued/running entries respectively. Keeping bounded validated headers
preserves their onset/bounds and full EB points; the global
announcement set alone does not retain all that processing context. A blocked
payload submission cannot occupy the metadata dispatch turn.

Received bodies share two entries and two fixed workers. Entries remain charged while queued, preparing, waiting for writer
admission and reconciling receipts. Admission transfers a useful network claim
to an exact local-owner token in the same STM transaction that reserves its
entry. The peer handler returns after that handoff; it does not await preparation
or the writer. Its ordinary completion releases physical request credit, while
the local claim survives disconnect and peer cleanup.

When full, received-body handlers wait outside the outstanding-state MVar and
cache locks. Retirement, another useful owner, a forge claim or closing can end
that wait without unrelated entries finishing. A validated received body owns
its exact body while preparing or awaiting confirmation; it does not claim
missing closure transactions. Announcement and certificate focus do no body
lookup or storage work.

Body workers directly reconcile their write receipts under the same entry;
there is no extra body receipt task retaining the operation. Completed body
keys are not remembered separately from logical acquisition. Generic receipt
tasks remain for TX responses and other producers.

Forge admission is independent of whether a peer announced the header first.
Admission installs a known local-payload claim for the full EB point, suppressing
new body and transaction dispatch while its worker waits. It does not refund or
cancel requests already sent. Matching forge callbacks share one bounded operation:
callers wait for writer admission (or retirement), while the owner remains live through durable
settlement. Each accepted header keeps its own announcement obligation. Only positive,
still-current receipt confirmation allows later callbacks to skip resubmission;
an empty or abandoned job pool is insufficient. Retirement also fences late
completion evidence.

Workers activate inside receipt supervision. Work admitted before activation
remains queued. Shutdown
closes admission before draining queued/running work, then drains accepted
receipts before storage closes. Already-admitted workers can still transfer to
receipt ownership during that drain. Exceptional shutdown cancels and joins the
workers, clears their local claims and unblocks admission waiters with failure.
Slow writer and receipt waits remain interruptible.

Use original registration, client invocation and a checked local ordinal to
identify each request. These are local correlation facts, not wire fields.
Late callbacks cannot release a replacement request's credit or modify a new
registration. Logical jobs and physical outstanding exchanges have distinct
ownership.

A complete-message observation stops network timing; it does not by itself
return credit. TX replies return credit at writer acceptance, or when their
handler finishes without submitting work. A body handler can instead finish
after transferring to its bounded node entry, before actual writer acceptance.
Its normal completion returns physical credit; the local operation remains owned.

A TX handler owns validated work while submitting it, then transfers accepted
work to a registered receipt task before later observations can lose its owner.
A receipt task waits for the database's write confirmation, reconciles the
logical jobs and releases its exact pending claim independently of the peer
connection. The submission scope records that transfer explicitly: a fast receipt
may finish before the handler, so current pending state cannot establish whether
the transfer happened. Later cache or tracing failures leave transferred work
with that task.

If a reply has no useful transactions left, it still undergoes the cheap count
and job-size checks, but submits no empty writer operation or receipt task. Its
logical assignment is released while its original physical lease still guards
re-admission; only its own unsubmitted claim is cleared. A successful receipt
instead reconciles the logical assignment before releasing the accepted pending
owner. These orderings prevent old cleanup from unpicking a newer assignment.

Writer acceptance precedes receipt-task installation. If that transfer fails
before installation, the receipt supervisor records the first fault and closes
runtime admission atomically with exact accepted-claim cleanup. Fatal teardown
owns the accepted operation; cleanup does not confirm acquisition or wait inline
for storage. Existing exceptional teardown keeps its original cause. An admitted
producer can still install its receipt task during healthy normal drain.

Body workers already own their preparation and directly await their receipts.
Aborting the pool also invokes exact-owner cleanup for queued operations that
no worker started. Received-body claims transfer atomically with bounded
admission, and their cleanup remains attached to the exact local token. Peer
teardown cannot erase these local owners. Complete only fully confirmed immutable jobs;
relevant unconfirmed jobs remain fetchable.

Normal shutdown stops admission and optional local imports, wakes local waiters,
and joins submitting handlers and accepted receipt callbacks before storage
closes. Exceptional shutdown cancels and joins remaining work. Actual storage
failures reach node supervision; peer protocol faults retain their original
connection ownership. One failed callback must not cause repeated exception
delivery during teardown.

Owned-worker failure reaches its supervisor with the original cause. Dependent
admission, capacity and join callers receive a closed-work signal, which wakes
waiters and terminates their operation. Normal drain still completes admitted
joins. A late complete body is discarded through its usual request finalizers
when local admission has closed; a forge waiting on failed metadata abandons its
claim. These dependent operations do not report the worker's storage failure as
a new peer fault or a second worker failure. The original supervisor still owns
fatal teardown.

## Durable confirmation and garbage collection

A body receipt distinguishes a present body with no held transactions from
an unavailable target. TX receipts separately report positively confirmed
submitted offsets, wrong-size diagnostics and target-body presence. An empty
wrong-size set never proves that a write landed. An absent retired target stops
further fetching without acquisition, coverage, location publication or a peer
failure charge. Emptying its jobs is cleanup, not successful closure delivery.

Confirmation proves presence in the authoritative partition after commit,
including legitimate existing immutable copies. The SQLite writer commits its
volatile transaction, then holds a separate confirmation mutex while selecting
the partition and reading its markers/bounds. Bulk promotion owns its maintenance
connection while copying and committing, and takes the confirmation mutex only
to publish the new boundary. Truncation holds it across deletion and the matching
boundary update. Maintenance acquires its connection mutex first; confirmation
uses the writer's own connections. Tracer IO stays outside the confirmation lock.
This removes the mutex dependency on copying; SQLite/disk contention remains.
If promotion already passed an omitted target, a stale volatile row confirms
nothing. This does not restore a fallback between partitions. The memory backend
establishes the same proof with its promotion state in one STM transaction.

For a TX job, SQLite reads markers and size boundaries only across its valid
submitted offset span, including the last TX's end boundary. An empty or entirely
invalid submission still checks target presence but needs no marker/boundary
read. Sparse spans can include gaps; only submitted offsets may be confirmed.
Whole-body receipts still inspect every marker because they report all held TXs.

A receipt is evidence at that proof boundary, not a pin until notification
consumption. Later retirement can remove its target; logical fetch state,
notification consumers and cache publication independently reject retired work.
Certified closures persist in immutable storage.

The shared storage wrapper prunes cache metadata before forwarding promotion,
releasing the cache lock before storage IO. Its fence remains monotonic after a
failed, interrupted or older promotion. This can remove reuse hints early; it
does not delete stored bytes. Physical GC uses the database's own promoted slot.
Startup truncation precedes population of the fresh cache.

Late cache publication cannot replace a newer usable transaction source with a
retired location. Already-captured fill descriptors remain hints, not pins: the
writer verifies the available source bytes, and an unavailable or stale fill
leaves the target transaction fetchable.

The mutable cache draws its two salt words from a dedicated, previously unused
branch of the LeiosFetch RNG root. Fetch decisions keep the other branch;
KeepAlive, peer sharing, TX submission and the surrounding node streams retain
their existing derivations. Production and deterministic node fixtures allocate
the salted cache and install its retirement wrapper before opening ChainDB.
This separates stream ownership, not cryptographic secrecy: StdGen is not a
security PRNG. Supplying a securely generated 128-bit startup secret remains
a follow-up at the allocation split.

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
| `Runtime` and `RequestState` | Original request identities, physical outstanding credit and its live-owner index, FIFO/send/receive facts and pending submission/receipt claims. |
| `Timing`, `PeerPenalty`, `Wait` | Pure service clocks and aged evidence, plus coalesced readiness/deadline/fallback waiting. |
| `Client` and `Client.Replies` | Existing pipelined protocol interpreter, final dispatch recheck, validation and storage submission/settlement. |
| `LocalReuse`, `OwnedWork` and `ReceiptTasks` | Bounded local materialization, announcement/forge obligations and node-owned producer/callback lifetimes. |
| `Coverage`, `Trace`, `Trace.Policy` | Confirmed acquisition origins and detached production observations. |

The outstanding-state lock serializes logical planning and removal. Physical
facts live in STM. Admission publishes an immutable descriptor, its credit and
the existing peer command queue under an exception-safe lock-to-STM boundary.
There is one command TVar, exposed through both the peer registration and runtime;
the send, FIFO and installed-receiver queues describe different protocol facts.

Send-return and handler callbacks reconcile only their original attempt. A fast
reply's settled descriptor stays until its send return is observed; neither
callback sweeps unrelated descriptors. Continuous-service classification uses
the original invocation's reply FIFO and ignores uncommitted, terminal or
settled requests. Pending-write callbacks carry the original work key and check
its expected network or local owner, even after the connection and descriptor
are gone. Discarding an unsent command edits only its original invocation's
queue. Cross-peer hedge eligibility and peer teardown still inspect their
shared ownership facts.

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
twice. Local handler time, including a wait for bounded body admission, delays
eligibility of the following head. Preparation and storage after body handoff
run independently and do not hold that peer handler open.

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

The transaction cache supplies reference metadata and proposed transaction
source locations. Membership is not target durability. Whole-EB preparation
makes one strict selection from upstream's maintained hash index. The
`getLeiosTxIndex` read returns immutable `LeiosTxMempoolView` values and their one
`VersionWord16` together, so selection and the closing check share that STM
transaction. The view's `leiosTxMempoolViewBytes` projection and the exact serialized
hash/size checks run afterwards, outside STM and fetch/cache locks, before invoking
the writer. Removal from the mempool afterwards does not invalidate the selected
immutable value. Unsupported projections or
missing validation metadata never fabricate validated status.

Whole-EB names the local selection/write scope; remaining network requests keep
the existing job partition and full-closure-offer requirement.

Two admitted whole-EB operations replace the former slice gate, 20,000 B target,
128-reference slicing and residual TX writes. Trusted ledger body/closure maxima
B and C bound their admitted logical encoded payload by 2*(B+C), independently
of a peer's announced size. This is not a whole-node heap bound: decoded selected
TX graphs and encoding buffers have representation overhead; ingress, peer handlers, forged work
and the existing DB writer have their own bounds.

Each operation writes its body, remaining DB fills and verified mempool bytes
once. The receipt proves body presence and held target offsets after commit,
including existing blob rows or immutable-partition copies.
Only confirmed offsets establish held/validated cache state, coverage or reusable
target locations. Missing
fills remain fetchable. Body acquisition follows that combined commit; closure
acquisition follows it only if every offset is confirmed. The forged path also
uses one body/all-TX write and requires complete, still-relevant confirmation.

Source-selection counts and the cache insertion summary describe captured
choices/cache state, separately from confirmed durable origins in the closure
summary. A proposed fill or selected mempool TX is not confirmed target data.
Actual writer acceptance is traced when it happens, after node-work handoff.

The ordinary DB writer and its queue remain in use. Normal shutdown closes
admission before draining owned operations; that closing signal also wakes a
blocked mempool selection, allowing optional local reuse to be skipped while
essential body/DB-fill writes finish. Generic TX receipts drain afterwards.
Exceptional shutdown cancels and joins the fixed workers and receipt tasks
before storage closes. One supervised body-worker group delivers a failure
once, even if both preparations fail concurrently. Exact tokens prevent late
network or local cleanup from erasing another operation's claim.

[Ownership and shutdown](#ownership-and-shutdown) describes admission,
saturation, retirement and cleanup.

## Cache retention

The cache retains reference metadata for the 128 freshest body-bearing points
and indexes only held transactions. Announcements alone retain no cache point.
A held transaction follows its youngest retained reference; a landed location
follows its youngest confirmed source. Packed reference metadata keeps hashes
in one byte string and their exact sizes in an unboxed vector. It preserves the
upstream body's ordered, unique-hash reference sequence and its offsets. This is
not the body's original encoding and does not provide whole-body acquisition.
Body acquisition requires a received body or local forge submission and a positive
target receipt.

One receipt pass publishes every confirmed held body offset: captured mempool
validation facts select validated state; other confirmed offsets use inserted
state. This includes confirmed DB fills. Capture the full 16-bit major protocol
version (`VersionWord16`) during selection rather than resampling it at receipt
completion. An inserted update preserves existing validation; an explicit
validated update replaces that metadata without inferring temporal order from
the version number. Network TX receipts publish inserted state only for confirmed submitted offsets.

Cache publication uses a bounded list of offset, hash, size and state records,
without transaction payload bytes. Materialize its entire spine and strict records
before taking the cache lock. The insertion's counters describe that later cache
state; arrival tracing retains its separate snapshot from before writer admission.

Publication for an evicted point is a no-op. It cannot recreate that point or
redirect the write through another point that references the same hash. A
positive receipt can still settle otherwise-live logical work and release its
exact owner, independently of cache capacity. Unavailable writes publish no
new evidence; partial confirmation leaves missing offsets fetchable. A forged
callback also requires complete confirmation and a still-current logical target.

TX arrival classification uses one read-only, point-aware cache snapshot before
writer admission. A nonretained point counts as evicted; a retained point's
absent transaction counts as needed, and a held transaction as extra. Each
selected occurrence contributes its bytes without cache insertion or deduplication.
Saved counters do not change when another write later lands. Concurrent handlers
can both observe missing data and count it as needed; confirmed distinct closure
origins and mux wire bytes answer separate questions.

## Practical limits and follow-ups

Responses are decoded as complete protocol messages. TX requests contain one
whole-transaction job; the policy does not salvage an incomplete message or
retain partial body bytes. Recovery leaves sent copies outstanding until reply
or connection teardown. Per-peer limits bound their credit, rather than imposing
a global duplicate-byte or fixed-copy ceiling.

The mempool pass is one-time discovery. Later local arrivals are not a continuous
resolver. A readable last-published index during mempool mutation is a separate
follow-up; the current lookup still waits for the authoritative state. Election/chain urgency stays with consensus focus; source ranking
operates inside that priority. Service settings and the ten-minute aging half-life
are initial tuning choices. The ordinary accounting scripts and selected IOSim
race checks are retained; a broader generated integrated-campaign framework is
separate future work.
