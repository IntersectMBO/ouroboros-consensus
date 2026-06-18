# Consensus configuration

This page describes the configuration values that the Consensus layer exposes
to its integrators: the arguments that must (or can) be provided when starting a
node via `Ouroboros.Consensus.Node.run`/`runWith`, and the default values that
the library fills in for everything else.

Most of these values are ultimately set by `cardano-node` from its configuration
file. This page documents the *consensus side* of those values — what they mean,
what their defaults are, and what changing them implies. A companion document in
the `cardano-node` repository is expected to describe the node configuration
file itself and how each of its fields maps to the values described here.

For the command-line tools bundled in this repository (`snapshot-converter`,
`db-analyser`), see [Consensus tools](./consensus_tools).

## How arguments flow into the Consensus layer

The entry point to running a node is `Ouroboros.Consensus.Node.run` (or the more
general `runWith`). Arguments are split over three records, defined in
`ouroboros-consensus-diffusion:Ouroboros.Consensus.Node`:

- **`RunNodeArgs`** — arguments that *every* invocation must specify
  explicitly: tracers, the `ProtocolInfo`, the Genesis configuration, etc.
  There are no defaults for these.
- **`StdRunNodeArgs`** — higher-level arguments for realistic deployments.
  This is the record `cardano-node` fills in from its configuration file. It is
  translated into `LowLevelRunNodeArgs` by `stdLowLevelRunNodeArgsIO`.
- **`LowLevelRunNodeArgs`** — low-level arguments that usually only tests
  provide directly. For ordinary invocations, `stdLowLevelRunNodeArgsIO`
  derives them from `StdRunNodeArgs`, hard-coding sensible defaults.

Independently of these, each storage component (ChainDB, ImmutableDB,
VolatileDB, LedgerDB, …) has a `defaultArgs` value providing defaults for its
own knobs. `ChainDB.defaultArgs` aggregates all of them, and
`stdLowLevelRunNodeArgsIO` applies the `StdRunNodeArgs` overrides on top
(snapshot policy, query batch size, tracer, validation policy).

The sections below go through each record and each component's defaults.

## `RunNodeArgs`: always explicit

These fields have no defaults; whoever invokes the Consensus layer must decide
them.

| Field | Description |
|---|---|
| `rnTraceConsensus` | Consensus tracers (ChainSync client/server, BlockFetch, mempool, forging, …). |
| `rnTraceNTN` | Tracers for the node-to-node mini-protocol codecs/handlers. |
| `rnTraceNTC` | Tracers for the node-to-client mini-protocol codecs/handlers. |
| `rnProtocolInfo` | The `ProtocolInfo`: top-level configuration (ledger config, consensus config, codecs) and the initial (genesis) ledger state. This is where the security parameter `k`, slot lengths, etc. enter the Consensus layer — they come from the genesis files, parsed by the node. |
| `rnNodeKernelHook` | Hook called after the `NodeKernel` is initialised but before the network layer starts. `cardano-node` uses it e.g. to set up the forging credentials. |
| `rnPeerSharing` | Willingness to participate in the PeerSharing mini-protocol (a network-layer flag negotiated in the handshake). |
| `rnGetUseBootstrapPeers` | An `STM` action telling the node whether to use bootstrap peers (legacy alternative to Genesis). |
| `rnGenesisConfig` | The Ouroboros Genesis configuration, see [Genesis configuration](#genesis-configuration). |
| `rnFeatureFlags` | Set of enabled experimental features (e.g. Peras). |
| `rnMempoolTimeoutConfig` | Optional mempool validation timeouts, see [Mempool](#mempool). `Nothing` disables the timeouts. |
| `rnTxSubmissionLogicVersion` | Which version of the tx-submission *logic* to run (see the [network spec](https://ouroboros-network.cardano.intersectmbo.org/pdfs/network-spec/network-spec.pdf)). |
| `rnTxSubmissionInitDelay` | Delay before the tx-submission server starts processing transactions, used to avoid load spikes right after startup. |

## `StdRunNodeArgs`: what the node configures

This record is owned by `cardano-node` (its fields exist so that the node's
configuration file can be mapped onto them) and is translated by
`stdLowLevelRunNodeArgsIO`. Effects of each field:

| Field | Effect |
|---|---|
| `srnBfcMaxConcurrencyBulkSync` | If set, overrides `bfcMaxConcurrencyBulkSync` of the BlockFetch configuration: from how many peers to download blocks concurrently while bulk syncing. |
| `srnBfcMaxConcurrencyDeadline` | If set, overrides `bfcMaxConcurrencyDeadline`: same, but for a caught-up node. |
| `srnChainDbValidateOverride` | If `True`, fully validate the ImmutableDB and VolatileDB on startup, even if the previous shutdown was clean. Otherwise full validation only happens after an unclean shutdown (see [Startup validation](#startup-validation-and-clean-shutdown-marker)). |
| `srnDatabasePath` | Where the databases live on disk, see [`NodeDatabasePaths`](#nodedatabasepaths-on-disk-locations). |
| `srnDiffusionArguments` / `srnDiffusionConfiguration` / `srnDiffusionTracers` | Passed through to the network layer's diffusion (`ouroboros-network`); not interpreted by Consensus. |
| `srnEnableInDevelopmentVersions` | If `False`, the negotiated node-to-node and node-to-client protocol versions are capped at the latest *released* version (`latestReleasedNodeVersion`); if `True`, in-development versions are offered too. |
| `srnTraceChainDB` | Tracer for all ChainDB events (also routed to the ImmutableDB, VolatileDB and LedgerDB tracers). |
| `srnMaybeMempoolCapacityOverride` | Optional mempool capacity override, see [Mempool](#mempool). |
| `srnChainSyncIdleTimeout` | How long the ChainSync server tolerates an idle client before disconnecting. The value (and its default) is owned by the network layer (`Cardano.Network.Diffusion.Configuration`); Consensus only plugs it into the ChainSync codec time limits. |
| `srnSnapshotPolicyArgs` | Ledger snapshot policy overrides, see [Snapshot policy](#snapshot-policy). |
| `srnQueryBatchSize` | Batch size for ledger queries that range-read on-disk tables, see [LedgerDB](#ledgerdb). |
| `srnLedgerDbBackendArgs` | Which LedgerDB backend to use (in-memory or LSM) and its parameters, see [LedgerDB backends](#ledgerdb-backends). It is a function of a `StdGen` because the LSM backend may need to generate a random bloom-filter salt. |

## `LowLevelRunNodeArgs`: defaults chosen by `stdLowLevelRunNodeArgsIO`

Tests provide this record directly; standard invocations get it from
`stdLowLevelRunNodeArgsIO`, which fixes the following values:

| Field | Standard value | Meaning / implication |
|---|---|---|
| `llrnBfcSalt` | random (`randomIO`) | Salt used by the BlockFetch decision logic to break ties differently on each node, so that the whole network doesn't pick the same peers. |
| `llrnRng` | fresh `StdGen` | Seed for the keep-alive, peer-selection, tx-submission and GSM anti-thundering-herd randomness, and for the randomised snapshot delay. |
| `llrnChainDbArgsDefaults` | `ChainDB.defaultArgs` + node overrides | See [ChainDB](#chaindb) and the per-database sections. The node overrides are: snapshot policy, query batch size, the ChainDB tracer, and (if `srnChainDbValidateOverride`) full validation. |
| `llrnCustomiseChainDbArgs` | `id` | Hook for tests to tweak the complete `ChainDbArgs`. |
| `llrnCustomiseNodeKernelArgs` | applies the BlockFetch concurrency and mempool capacity overrides | See `NodeKernelArgs` defaults below. |
| `llrnCustomiseHardForkBlockchainTimeArgs` | `id` | See [Blockchain time](#blockchain-time). |
| `llrnChainSyncIdleTimeout` | `srnChainSyncIdleTimeout` | See above. |
| `llrnMaxCaughtUpAge` | **20 minutes** | If the tip of the selection is older than this, the node exits the `CaughtUp` state of the GSM, see [Genesis State Machine](#genesis-state-machine-gsm). |
| `llrnMaxClockSkew` | **2 seconds** (`InFutureCheck.defaultClockSkew`) | Headers whose slot translates to a time more than this far in the future are considered *invalid* (the peer is disconnected). Headers in the future but within the skew are tolerated, just not selected yet. Accounts for inevitable clock differences between peers. |
| `llrnNodeToNodeVersions` / `llrnNodeToClientVersions` | all supported versions, capped at the latest released one unless `srnEnableInDevelopmentVersions` | Which protocol versions the handshake offers. |
| `llrnVersionDataNTN` / `llrnVersionDataNTC` | network magic from the genesis config, diffusion mode from the diffusion configuration | Handshake version data. |
| `llrnWithCheckedDB` | `stdWithCheckedDB` | Checks the DB marker, takes the DB lock, and determines whether the last shutdown was clean, see [Startup validation](#startup-validation-and-clean-shutdown-marker). |
| `llrnMkImmutableHasFS` / `llrnMkVolatileHasFS` | rooted at `srnDatabasePath` | File systems for the immutable and non-immutable databases, see [`NodeDatabasePaths`](#nodedatabasepaths-on-disk-locations). |
| `llrnLdbFlavorArgs` | from `srnLedgerDbBackendArgs` | LedgerDB backend, see [LedgerDB backends](#ledgerdb-backends). |
| `llrnRunDataDiffusion` | `stdRunDataDiffusion` | Runs the network layer's diffusion; `run` does not return before it does. |

### `NodeKernelArgs` defaults

`mkNodeKernelArgs` (called internally by `runWith`) fixes a few more values,
which `llrnCustomiseNodeKernelArgs` can override:

| Value | Default | Meaning |
|---|---|---|
| `blockFetchConfiguration` | `defaultBlockFetchConfiguration` from `ouroboros-network` (seeded with `llrnBfcSalt`) | BlockFetch decision-logic parameters. The node only exposes the two concurrency overrides (`srnBfcMaxConcurrency*`); the remaining defaults are owned by `ouroboros-network`. |
| `miniProtocolParameters` | `defaultMiniProtocolParameters` from `ouroboros-network` | Pipelining depths and other mini-protocol limits. |
| `mempoolCapacityOverride` | `NoMempoolCapacityBytesOverride` | See [Mempool](#mempool). |
| `getDiffusionPipeliningSupport` | `DiffusionPipeliningOn` | Whether diffusion pipelining is enabled. |

`nodeKernelArgsEnforceInvariants` then clamps customised values so they remain
consistent: `blockFetchPipeliningMax` may not exceed the default (a larger value
would be a protocol violation), and `bfcMaxRequestsInflight` may not exceed
`blockFetchPipeliningMax`.

## `NodeDatabasePaths`: on-disk locations

`srnDatabasePath` has two forms:

- `OnePathForAllDbs path`: everything lives under one directory:
  `path/{immutable,volatile,ledger,gsm}`.
- `MultipleDbPaths immPath volPath`: the ImmutableDB lives under
  `immPath/immutable` and everything else (VolatileDB, ledger snapshots, GSM
  marker file) under `volPath/{volatile,ledger,gsm}`.

The split exists because the ImmutableDB is large but accessed mostly
sequentially, so it can live on a big, slow (cheap) volume, while the other
databases benefit from a fast volume.

## Startup validation and clean-shutdown marker

`stdWithCheckedDB` runs before anything else:

1. It checks the **DB marker file** (which records the network magic) to avoid
   opening a database belonging to a different network, and expects an empty
   directory if the marker is absent.
2. It takes the **DB lock**, so two nodes cannot use the same database.
3. It checks the **clean-shutdown marker**. If the previous shutdown was *not*
   clean, the ImmutableDB and VolatileDB validation policies are upgraded to
   `ValidateAllChunks`/`ValidateAll` (via `ChainDB.ensureValidateAll`) to detect
   and recover from disk corruption. Otherwise the cheaper defaults below apply.
   `srnChainDbValidateOverride` forces the full validation unconditionally.

## ChainDB

Defaults from `defaultSpecificArgs` in
`Ouroboros.Consensus.Storage.ChainDB.Impl.Args`:

| Knob | Default | Meaning / implication |
|---|---|---|
| `cdbsBlocksToAddSize` | **10** | Size of the queue of blocks waiting to be added asynchronously to the ChainDB; bounds how many such blocks are kept in memory when the background thread can't keep up. |
| `cdbsGcDelay` | **60 s** | Delay between *copying* a block to the ImmutableDB and *garbage-collecting* it from the VolatileDB. Gives the OS time to flush the ImmutableDB write to disk, so a crash in between cannot lose the block. |
| `cdbsGcInterval` | **10 s** | Scheduled GCs are batched so that at most one GC runs per interval. With the defaults, the GC queue stays ≤ 7 entries and the overlap between VolatileDB and ImmutableDB stays small (≈ the blocks synced in 70 s). |
| `cdbsLoE` | `LoEDisabled` | The Limit on Eagerness, part of Genesis. Overwritten by the Genesis configuration when LoE/GDD is enabled, see [Genesis configuration](#genesis-configuration). |
| `cdbsTracer` | `nullTracer` | Overridden by `srnTraceChainDB` in standard invocations. |

No default (must be provided): the resource registry, the top-level
configuration, the GSM marker-file filesystem, and the RNG for randomised
snapshot delays — all filled in by `completeChainDbArgs` when the node starts.

## ImmutableDB

Defaults from `ImmutableDB.defaultArgs` in
`Ouroboros.Consensus.Storage.ImmutableDB.Impl`:

| Knob | Default | Meaning / implication |
|---|---|---|
| `immCacheConfig.pastChunksToCache` | **250 chunks** | How many past chunks' indices to cache in memory (≈ 250 MB). One chunk holds roughly one epoch of blocks. If this were lower than the range of chunks peers are actively requesting, indices would be constantly evicted and re-parsed, raising CPU load. |
| `immCacheConfig.expireUnusedAfter` | **5 minutes** | Evict a cached chunk index after it has been unused for this long. |
| `immValidationPolicy` | `ValidateMostRecentChunk` | On startup, only the most recent chunk file is validated rather than the whole history. Upgraded to `ValidateAllChunks` after an unclean shutdown. |
| `immChunkInfo` | *no default* | How many **slots** each chunk file covers. Supplied from the era parameters via `nodeImmutableDbChunkInfo` (for Cardano: 21,600 slots, which is a **Byron epoch**, so for each Shelley epoch there are 20 chunk files). |
| `immCheckIntegrity` | *no default* | How to check a block for corruption (e.g. verifying its hash); supplied via `nodeCheckIntegrity` from the block's `RunNode` instance. |

## VolatileDB

Defaults from `VolatileDB.defaultArgs` in
`Ouroboros.Consensus.Storage.VolatileDB.Impl`:

| Knob | Default | Meaning / implication |
|---|---|---|
| `volMaxBlocksPerFile` | **1000 blocks** | Blocks are appended to a file until it contains this many, then a new file is started. Garbage collection deletes whole files whose blocks are all old enough, so smaller files mean finer-grained GC but more files. |
| `volValidationPolicy` | `NoValidation` | On startup, block files are parsed but blocks are not integrity-checked (fast open). Upgraded to `ValidateAll` after an unclean shutdown. |

## LedgerDB

Defaults from `LedgerDB.defaultArgs` in
`Ouroboros.Consensus.Storage.LedgerDB.Args`:

| Knob | Default | Meaning / implication |
|---|---|---|
| `lgrSnapshotPolicyArgs` | `defaultSnapshotPolicyArgs` | See [Snapshot policy](#snapshot-policy). |
| `lgrQueryBatchSize` | `DefaultQueryBatchSize` = **100,000 keys** | Ledger queries that range-read on-disk tables (e.g. dumping the UTxO set) read in batches of this many keys, bounding memory use. Settable via `srnQueryBatchSize` (`RequestedQueryBatchSize`). |
| `lgrBackendArgs` | **V2 in-memory backend** | See [LedgerDB backends](#ledgerdb-backends). |

### LedgerDB backends

The LedgerDB keeps the ledger state's *tables* (the UTxO set and other large
mappings) in a backend. Two backends ship with this repository:

- **InMemory** (`Ouroboros.Consensus.Storage.LedgerDB.V2.InMemory`): the whole
  UTxO set is held in RAM, like a pre-UTxO-HD node. Fastest, highest memory
  use. This is the default wired into `ChainDB.defaultArgs` (`InMemArgs` has no
  parameters). Snapshots are written as a single CBOR `tables` file.
- **LSM** (`Ouroboros.Consensus.Storage.LedgerDB.V2.LSM`, sublib
  `ouroboros-consensus:lsm`): tables are stored on disk in an
  [`lsm-tree`](https://github.com/IntersectMBO/lsm-tree) database, drastically
  reducing memory at some throughput cost. Built with `mkLSMArgsIO`, whose
  parameters are:

  | Parameter | Default | Meaning |
  |---|---|---|
  | salt | random from the supplied `StdGen` if not provided | Salt for the LSM bloom filters. An existing database's on-disk salt is authoritative, and exported snapshots record the salt they were created with, so importers don't need to know it. Providing it explicitly (e.g. from the node configuration) makes runs reproducible across tooling. |
  | database path | — (chosen by the caller; the node uses `lsm` under the fast storage root) | Where the live LSM session lives, relative to the LSM filesystem root. |
  | export path | `Nothing` (no exports) | If set, every snapshot the node takes is *also* exported as a self-contained directory of LSM run files under this path (relative to the LSM filesystem root). This is what [`snapshot-converter`](./consensus_tools) consumes. |
  | fast-storage root | — | Directory used as the root of the LSM filesystem; should be on a performant volume. |

Which backend (and which of these parameters) `cardano-node` exposes to end
users is decided in `cardano-node`; Consensus accepts the choice through
`srnLedgerDbBackendArgs`.

### Snapshot policy

The LedgerDB periodically writes a **snapshot** of the ledger state to disk so
that a restarting node can resume from a recent state instead of replaying the
whole chain. Only states that are older than `k` blocks (i.e. immutable) are
snapshotted. The policy is configured by `SnapshotPolicyArgs`
(`srnSnapshotPolicyArgs`); every field can be left as `UseDefault` or
overridden. Defaults from `defaultSnapshotPolicy` in
`Ouroboros.Consensus.Storage.LedgerDB.Snapshots`:

| Knob | Default | Meaning / implication |
|---|---|---|
| `spaNum` (snapshots kept on disk) | **2** | When a new snapshot is written, the oldest is deleted, always leaving one intact snapshot in case the write is interrupted (snapshot files are not `fsync`ed). `1` is dangerous for that reason; `0` would delete the snapshot right after writing it. |
| `sfaInterval` | **`2·k` slots** (43,200 on mainnet ≈ 72 min of slots) | Snapshots are taken for the most recent immutable state before each slot in `offset, offset + interval, offset + 2·interval, …`. Nodes with the same interval/offset therefore snapshot *the same slots*, which matters for tools like Mithril that compare snapshots across nodes. Smaller interval = less replay on restart, more snapshot I/O. |
| `sfaOffset` | **0** | Shifts the grid of snapshot slots, see above. |
| `sfaRateLimit` | **10 minutes** | Skip a snapshot if less than this much time passed since the previous one finished. Mainly relevant while syncing, when eligible slots stream past quickly. Non-positive values disable the limit. Should be well below the wall-clock duration of the interval, or snapshots get skipped even when caught up. |
| `sfaDelaySnapshotRange` | **5–10 minutes** | Once a snapshot is due, the node waits a random delay drawn from this range before writing it, so that the network's nodes don't all hit the disk (and slow down) simultaneously. |

Additional points:

- `spaFrequency = DisableSnapshots` turns snapshotting off entirely.
- `mithrilSnapshotPolicyArgs` is a ready-made policy for Mithril: interval
  **432,000** (one Shelley epoch) and offset **388,800**, chosen so that
  snapshots land on Shelley epoch boundaries even while still syncing Byron.
- Snapshots whose directory name carries a suffix (e.g. `4492799_last_Byron`)
  are **never deleted** by the retention policy — useful for pinning a state.
- `sanityCheckSnapshotPolicyArgs` runs at startup and traces a warning for
  suspicious overrides: 0 snapshots on disk, a negative or inverted delay
  range, a disabled or very large rate limit, or an interval incompatible with
  Mithril (not dividing the 432,000-slot epoch).

## Mempool

- **Capacity**: by default (`NoMempoolCapacityBytesOverride`) the mempool holds
  **twice the current block capacity**, tracking the protocol parameters as
  they change. An explicit byte override (`srnMaybeMempoolCapacityOverride`) is
  rounded up to a whole number of blocks (minimum 1). Larger capacity buffers
  more pending transactions at the cost of memory and (re)validation work.
- **Validation timeouts** (`rnMempoolTimeoutConfig`, optional — `Nothing`
  disables them): a defence-in-depth measure against transactions that are
  pathologically slow to validate.

  | Field | Meaning |
  |---|---|
  | `mempoolTimeoutSoft` | A transaction taking longer than this to validate is discarded instead of added. |
  | `mempoolTimeoutHard` | A transaction taking longer than this causes disconnection from the peer that sent it. Must be meaningfully larger than the soft timeout to be useful. |
  | `mempoolTimeoutCapacity` | If the transactions currently in the mempool cumulatively took longer than this to validate, the mempool counts as full. Recommended value: the forging thread's per-block validation-time budget minus `mempoolTimeoutSoft`. |

  There are no defaults in this repository; the values are owned by the caller.

## Genesis State Machine (GSM)

The GSM (`Ouroboros.Consensus.Node.GSM`) tracks whether the node is
`PreSyncing` (the Honest Availability Assumption is not satisfied), `Syncing`,
or `CaughtUp`. Its knobs, as wired by `stdLowLevelRunNodeArgsIO` and
`mkNodeKernelArgs`:

| Knob | Default | Meaning / implication |
|---|---|---|
| `llrnMaxCaughtUpAge` | **20 minutes** | The node leaves `CaughtUp` once its selection's tip is older than this. |
| `gsmMinCaughtUpDuration` | same value (**20 minutes**) | Minimum time the node stays in `CaughtUp` after entering it, regardless of the tip's age. Prevents the network from thrashing between states during a block-production outage. |
| `gsmAntiThunderingHerd` | seeded from `llrnRng` | Each Syncing→CaughtUp transition randomly stretches `gsmMinCaughtUpDuration` by up to **15%**, so nodes don't all leave `CaughtUp` at the same instant. |
| marker file | `gsm/CaughtUpMarker` under the volatile DB path | Persists the `CaughtUp` state across restarts: a node that was caught up (and whose tip is not yet too old) restarts directly in `CaughtUp`. |

The GSM state feeds the diffusion layer's `LedgerStateJudgement`
(`CaughtUp ↦ YoungEnough`, otherwise `TooOld`) and en-/disables the Genesis
components: the [LoE fragment](#genesis-configuration) is the most conservative
one while `PreSyncing`, the actual LoE fragment while `Syncing`, and disabled
when `CaughtUp`.

## Genesis configuration

`rnGenesisConfig` configures Ouroboros Genesis, the mechanism that lets a
syncing node safely choose among competing chains without trusting its peers.
Built with `mkGenesisConfig` in `Ouroboros.Consensus.Node.Genesis`:

- `mkGenesisConfig Nothing` (= `disableGenesisConfig`) disables every Genesis
  component, yielding classic Praos behaviour.
- `mkGenesisConfig (Just flags)` enables them; each flag can disable an
  individual component or override its default:

| Component | Flag / override | Default (enabled) | Meaning / implication |
|---|---|---|---|
| BlockFetch grace period | `gcfBlockFetchGracePeriod` | **10 s** | Minimum time the Genesis BlockFetch logic keeps downloading from a peer before judging (and possibly rotating) it, even if it performs badly. 0 when Genesis is disabled. |
| Limit on Patience (LoP) bucket capacity | `gcfEnableLoP`, `gcfBucketCapacity` | **100,000 tokens** | The ChainSync client disconnects from peers that withhold headers, using a token bucket: the bucket leaks constantly and is refilled on useful headers. The capacity corresponds to 200 s at the default rate — enough to absorb long GC pauses. |
| LoP leak rate | `gcfBucketRate` | **500 tokens/s** | One token per 2 ms; validating a header takes well under 1 ms, so this is conservative. |
| ChainSync Jumping (CSJ) jump size | `gcfEnableCSJ`, `gcfCSJJumpSize` | **4,320 slots** (`2·k`, the Byron forecast range) | With CSJ, only one peer serves headers at a time and the others periodically confirm jumps of this size, saving bandwidth while syncing. The Byron forecast range is used because larger (Shelley-sized) jumps would block while syncing Byron. |
| LoE & GDD | `gcfEnableLoEAndGDD` | enabled | The **Limit on Eagerness** caps chain selection at `k` blocks past the intersection of all candidate chains, and the **Genesis Density Disconnector** disconnects the peers whose chains are provably sparser, allowing the LoE to advance. |
| GDD rate limit | `gcfGDDRateLimit` | **1 s** | Run the (somewhat expensive) GDD evaluation at most once per this interval. |
| Historicity cutoff | — | **`3·k/f` s + 1 h** (129,600 s + 3,600 s on mainnet) | Rejects ChainSync messages about *historical* headers (older than the Shelley stability window, plus a safety margin) outside of syncing; such messages can only originate from adversarial behaviour. Disabled (`Nothing`) when Genesis is off. |

When LoE/GDD is enabled, `mkGenesisNodeKernelArgs` replaces the ChainDB's
`cdbsLoE` (default `LoEDisabled`) with an action reading the actual LoE
fragment, driven by the [GSM state](#genesis-state-machine-gsm).

## Blockchain time

`runWith` constructs the hard-fork-aware blockchain time with
`HardForkBlockchainTimeArgs`, customisable through
`llrnCustomiseHardForkBlockchainTimeArgs`:

| Knob | Default | Meaning / implication |
|---|---|---|
| `hfbtBackoffDelay` | **60 s** | If the current time cannot be translated to a slot (because the ledger state is too far behind to know the era's slot length), retry after this delay. |
| `hfbtMaxClockRewind` | **20 s** | The system clock moving *backwards* (e.g. NTP adjustments) is tolerated up to this much: the blockchain time waits for the clock to catch up rather than crashing the node. Larger rewinds terminate the node with `SystemClockMovedBack`. |

## Header in-future check

`llrnMaxClockSkew` (default **2 s**, `InFutureCheck.defaultClockSkew`) bounds
the permissible clock skew when receiving headers: a header whose slot starts
more than 2 s in the future is treated as invalid and its sender is
disconnected; a header less than 2 s in the future is accepted but not
considered for chain selection until its slot arrives.

## Values owned by other layers

For completeness, knobs that pass *through* the Consensus API but whose
semantics and defaults are owned elsewhere:

- **BlockFetch decision logic** (`BlockFetchConfiguration`): defaults come from
  `ouroboros-network`'s `defaultBlockFetchConfiguration`; Consensus only
  injects the random `bfcSalt` and the two concurrency overrides from
  `StdRunNodeArgs`.
- **Mini-protocol parameters** (`MiniProtocolParameters`, e.g.
  `blockFetchPipeliningMax`): defaults from `ouroboros-network`'s
  `defaultMiniProtocolParameters`; Consensus clamps customisations to
  protocol-safe values (`nodeKernelArgsEnforceInvariants`).
- **ChainSync idle timeout** (`srnChainSyncIdleTimeout`): default defined in
  `Cardano.Network.Diffusion.Configuration`.
- **Diffusion** (addresses, targets, churn, …): entirely owned by
  `ouroboros-network` via `srnDiffusionArguments`/`Configuration`/`Tracers`.
- **Protocol parameters** (`k`, slot length, epoch sizes, …): from the genesis
  files via `rnProtocolInfo`; Consensus never defaults these.

## Appendix: on-disk snapshot layout

Tool authors may need to read snapshots directly. A snapshot is a directory in
the `ledger` folder, named after the slot number of its state, with an optional
suffix: `163470034` or `163470034_my-suffix`.

```
<volatile-path>/ledger/
  163470034/            # <slot>[_<suffix>]
    state               # CBOR-encoded ExtLedgerState (without the tables)
    meta                # JSON metadata (see below)
    tables              # InMemory backend only: CBOR of the UTxO table
    # LSM: instead of `tables`, the node writes a `utxoSize` file here, and
    # the tables live inside the LSM session's own snapshot storage.
```

An **exported LSM snapshot** is self-contained: `lsm-tree`'s `exportSnapshot`
produces a directory of LSM run files (which records the bloom-filter salt it
was created with) under the configured export path. This is the format
[`snapshot-converter`](./consensus_tools) operates on.

The `meta` file is JSON, mirroring `SnapshotMetadata` in
`Ouroboros.Consensus.Storage.LedgerDB.Snapshots`:

```json
{ "backend": "utxohd-mem",   // or "utxohd-lmdb" | "utxohd-lsm"
  "checksum": 2763283495,    // numeric CRC over the snapshot contents
  "tablesCodecVersion": 1 }
```
