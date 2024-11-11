# UTXO-HD

This document aims to provide an comprehensive guide on UTXO-HD, why we are
implementing this feature, what brings to Cardano and what it implies.

## Why does Cardano need UTXO-HD

Cardano is built following the UTXO model. This means that the Ledger state
contains a map from _transaction inputs_ to _transaction outputs_. A transaction
might consume some of those entries and produce new ones. Each entry is owned by
an address which is the one that can spend it.

The UTXO set is an always growing data structure. Currently the `cardano-node`
uses a fair amount of RAM but this amount will keep growing as more traffic
takes place on the network (transactions per second, transaction size, block
size, ...). This is bad for decentralization and sustainability of
the network as eventually only powerful machines would be able to participate on
it.

To improve decentralization, a decision was made to move this data to persistent
storage which, albeit slower, is much cheaper than RAM. The Consensus layer is
reworked so that the flow of data now allows for UTXO entries to come from some
backend storage, which might be on disk or in memory trading memory for speed.

The UTXO-HD feature provides two backends which can be chosen in `cardano-node`'s
configuration file:

- `LedgerDBBackend: V2InMemory`
- `LedgerDBBackend: V1LMDB`

How these backends work is shown below in this document.

## UTXO-HD design in Consensus

> ℹ️ We are going to focus on Shelley based eras, ignoring Byron for now.

### The `NewEpochState` data structure
The Ledger layer defines the data structure that holds the state of the blockchain
after applying some number of blocks, the `NewEpochState`. Among other things,
this data structure holds a UTXO set which is a `Map` from
`TxIn (EraCrypto era)` to `TxOut era`.

In order to apply the different Ledger operations, there is no need for this set
to be complete at all times, as only entries consumed by the transactions will be
accessed. When given a block or a transaction, the Ledger code provides functions
for getting the set of keys that would be necessary to exist in the UTXO set for
that block or transaction to apply correctly.Taking advantage of this, the Consensus layer will modify this
container such that it only contains the entries necessary for the Ledger rules.

### Shelley instantiation and ledger tables

The `LedgerState (ShelleyBlock proto era)` data family instances are augmented
with a new field which will hold these entries that will be extracted from and
injected to the `NewEpochState` before calling the Ledger rules. This new field
(which we call _ledger tables_) is a container-like structure parametrized by a
`TxIn` and `TxOut` type families.

```diff haskell
data instance LedgerState (ShelleyBlock proto era) mk = ShelleyLedgerState {
       shelleyLedgerTip        :: !(WithOrigin (ShelleyTip proto era))
     , shelleyLedgerState      :: !(SL.NewEpochState era)
     , shelleyLedgerTransition :: !ShelleyTransition
+    , shelleyLedgerTables     :: !(LedgerTables (LedgerState (ShelleyBlock proto era)) mk)
    }

data LedgerTables l mk = LedgerTables {
    getLedgerTables :: mk (TxIn l) (TxOut l)
}
```

For a Shelley block, these type families are mapped to the same types as above:

- `TxIn (LedgerState (ShelleyBlock proto era)) = TxIn (EraCrypto era)`
- `TxOut (LedgerState (ShelleyBlock proto era)) = TxOut era`

To instantiate the `mk` type variable, some _mapkinds_ are defined:

| `MapKind :: Type -> Type -> Type` | Container                     | Used  for                                                         |
|-----------------------------------|-------------------------------|------------------------------------------------------------------|
| `ValuesMK k v`                    | `Map k v`                     | Ledger states passed to and from the Ledger rules                |
| `KeysMK k v`                      | `Set k`                       | Querying the disk for the values needed by a block               |
| `DiffMK k v`                      | `Map k (Delta v)`             | Carrying the differences created by applying a block             |
| `EmptyMK k v`                     | $\emptyset$                   | When not needing info about the UTxO set, or the values are inside the `NewEpochState`                         |

The actual invocation of the ledger rules make use of a `NewEpochState` which is unaware of any of this machinery. We use
the `stowLedgerTables`/`unstowLedgerTables` functions to inject and project the values in the
`NewEpochState` to the ledger tables, making this completely transparent for the Ledger layer.

```haskell
stowLedgerTables   :: l ValuesMK -> l EmptyMK
unstowLedgerTables :: l EmptyMK  -> l ValuesMK
```

> ⚠️ It is very important to note that `EmptyMK` just means that _the ledger tables are empty_. This says nothing about whether there are values in the `NewEpochState`'s UTXO set. In the Consensus layer we take much care to ensure that the combination of `EmptyMK` having values in the internal UTXO set only happens at the Ledger layer boundary (via `stowLedgerTables`). Any other instance of `l EmptyMK` will mean that there are no values in the tables nor in the `NewEpochState`.

### Interacting with the Ledger layer (high level)

The Consensus layer invokes essentially 4 Ledger operations: forecast, tick and
applyBlock, applyTx. Each one of these rules have different requirements on the contents
of the UTXO set.

|             | Requirements                                                               | Input to the Ledger layer | Output from the Ledger layer |
|-------------|----------------------------------------------------------------------------|---------------------------|------------------------------|
| Forecasting | Doesn't use the UTXO set                                                   | `EmptyMK`                 | `EmptyMK`                    |
| Ticking     | Doesn't use the UTXO set but might produce changes on it                   | `EmptyMK`                 | `ValuesMK`                   |
| ApplyBlock  | Consumes inputs for the transactions in the block and produces new entries | `ValuesMK`                | `ValuesMK`                   |
| ApplyTx     | Consumes inputs for the transactions in the block and produces new entries | `ValuesMK`                | `ValuesMK`                   |

When ticking and applying a block, the Consensus code computes the difference
between the input and output sets producing `DiffMK` tables. The ticking and
applying steps are executed in sequence, producing a `DiffMK` for the
combined operation. The Consensus layer uses this `DiffMK` to influence the
values that are used when dealing with later blocks.


### Managing the differences

To ensure the properties of the Ouroboros protocols, the Consensus layer needs
to be able to perform rollbacks on the chain of at most `k` blocks (which in
mainnet equals `2160` blocks). Because of this, the differences of the last `k`
blocks cannot be considered immutable and therefore they cannot yet be flushed to
persistent storage. This same principle is the one that dictates that ledger
snapshots (for restarting the node) have to store ledger states before or at the
immutable tip of the chain.

Following this same reasoning, the way differences are carried around changes
depending on the specific backend used by the LedgerDB, whether it lives on the
disk or in memory:

#### On-disk backend

The on-disk backend uses the concept of an _anchor_ which is before or at the
immutable tip. This _anchor_ contains a full UTXO set stored in the disk, in
what we call the `BackingStore`. In order to get values for applying a
particular block, the Consensus layer has to read those values from the anchored
UTXO set and apply all the differences from that point to the tip of the chain.

This means that to the pre-UTXO-HD LedgerDB that held the last `k` ledger
states, a side sequence is added which holds the differences resulted from
applying each of the last `k` blocks. This sequence is held in a `FingerTree`
which contains the combination of all the differences that can be applied
directly to a set of values.

The Consensus layer will periodically flush the differences in between the
anchor and the current immutable tip to the on-disk backend, advancing the
chain.

#### In-memory backend

The in-memory backend augments each of the `k` values contained in the LedgerDB
to hold a full UTXO set. This emulates exactly how the LedgerDB looked like
before UTXO-HD. After each operation with the Ledger, the resulting differences
are applied to the set of values on the tip, producing the new UTXO set.

The memory footprint of this solution is almost equivalent to the pre-UTXO-HD
one. There aren't `k` UTXO sets, but just by Haskell's sharing, there is one
UTXO set, the others sharing most of their contents with each one's predecessor.

### The forker abstraction

In order to perform operations with the Ledger layer, Consensus defines the
`Forker` abstraction as _an API to evaluate forks_. Forkers give access to
reading values at a specific point in the chain. Its implementation depends on
which backend is at use in the LedgerDB abstracting over both of them.

It is important to note that when using the on-disk backend, a `Forker` will
mantain a consistent view of the _anchored_ UTXO set, which means that writes to
the anchor are queued while the `Forker` is held. For this reason, `Forker`s
should be an ephemeral resource, released as soon as possible.

### The mempool

The mempool behaves pretty much as a virtual block. The design is not
particularly complex as we just ask a `Forker` for the inputs for a transaction
when applying it.

The only caveat compared to the pre-UTXO-HD implementation when using the on-disk
backend is that some more re-validation of transactions will take place.
Previously, the mempool cached the latest ledger state and therefore we
could run a separate thread that would sync the mempool with the LedgerDB and
revalidate all the transactions asynchronously once the tip of the LedgerDB had changed.

Now, we might not be able to
apply a transaction if the `LedgerState` on top of which we had applied the
others is gone from the LedgerDB as we would have lost the differences from
the anchored UTXO to that particular state. Therefore, adding a transaction might in some
cases trigger a sync with the LedgerDB and therefore a revalidation of the
previous transactions.

It is important to note that the old behavior (only the thread monitoring the LedgerDB
would trigger a resync) was not crucial, now there is just an innocuous race
between the trigger that monitors the LedgerDB and the process that adds the
transaction, which will result in the same final state regardless of which of
those wins the race.

### Ledger state queries

> TODO: revisit, I think these are much much faster now

Most of the queries don't require the UTXO set, but there are three in particular
that do: `GetUTxOByTxIn`, `GetUTxOWhole` and `GetUTxOByAddress`. We assume that
`GetUTxOWhole` is considered to be a debug query so we don't worry about its performance. For
`GetUTxOByTxIn`, the query is fast because we are accessing explicit entries in
the UTXO set.

However, it is `GetUTxOByAddress` that poses a real problem, as we need to query
the whole UTxO set, apply all the differences to it and then traverse it
entirely to find out the UTxOs belonging to an address. This query is quite
slow even without UTxO-HD and in fact its usage is already discouraged. It
should not be a responsibility of the node to maintain access to this if it is
not needed by the logic that runs the blockchain, so the plan is to move this
into a separate process/client that runs an index of UTxOs by address that can
provide fast access to it (see [#4678](https://github.com/IntersectMBO/cardano-node/issues/4678)).

### The `CardanoBlock`

The Consensus layer is built around the concept of blocks, and for the specific
case of Cardano, a special block is used: the `HardForkBlock`. A `HardForkBlock`
is an n-ary sum type, which contains a particular block out of the list of
blocks that exist in the Cardano blockchain (Byron, Shelley, Allegra, etc).

On the outside, a `HardForkBlock` is made in a way such that its usage is almost
transparent for the Consensus layer, just as any other block, however for ledger
tables there are some complications. Revisiting the `LedgerState (HardForkBlock
xs)` instance, we can see that it is an n-ary sum of ledger states for each of
the blocks:

```haskell
newtype instance LedgerState (HardForkBlock xs) mk = HardForkLedgerState {
      hardForkLedgerStatePerEra :: HardForkState (Flip LedgerState mk) xs
    }

newtype HardForkState f xs = HardForkState {
      getHardForkState :: Telescope (K Past) (Current f) xs
    }
```

So, in reality, when holding a `LedgerState (HardForkBlock xs) ValuesMK`, it
actually contains a `LedgerState a ValuesMK` for the particular era in the n-ary
sum. This implies that the contents of the ledger tables are mappings from
`TxIn a` to `TxOut a`, which change on each era.

However, a value of type `LedgerTables (LedgerState (HardForkBlock xs)) ValuesMK`
will hold mappings from `TxIn (LedgerState (HardForkBlock xs))` to
`TxOut (LedgerState (HardForkBlock xs))`. When defining these type instances we
had two choices:

- Make `TxOut (LedgerState (HardForkBlock xs))` equal to the `TxOut a` of the
  particular era in the ledger state. Aside from the complications implementing this might
  impose (in terms of type-level machinery), this would mean that when transitioning from one era to the next
  one, the whole UTXO set in the tables would have to be updated to translate
  all the entries to the newer era. If this set was on the disk, this would be
  prohibitively costly.

- Make `TxOut (LedgerState (HardForkBlock xs))` a sum type that can hold values of
  any eras. This solution makes it very easy to carry `LedgerTables` in the
  Consensus layer as values do not need to be translated, in fact
  values from older eras might co-exist with those of the current one. The
  disadvantage of this solution is that injecting the ledger tables in the
  ledger state (so `withLedgerTables :: LedgerTables ... mk -> LedgerState ... anymk -> LedgerState ... mk`)
  implies that we are going from hard-fork keys and values to keys and values of
  the particular era, making the necessary era translations on-the-fly.

  This tradeoff was considered acceptable and because of it we put much care
  in only injecting small tables, such as the set of values needed to apply a
  block (which is bound by the maximum size of the block). Developers integrating
  the UTXO solution in other tools should understand this limitation and put
  great care in not violating it for example by injecting and projecting the whole
  UTXO set on every block which would simply blow up the memory consumption.

It is important to note that for any era in the Cardano blockchain, the `EraCrypto`
type family instance is the same (`StandardCrypto`), which makes all `TxIn (EraCrypto era)` keys equal. Thanks
to this, we can define the `TxIn` for `HardForkBlocks` equal to this same type,
which we call a `CanonicalTxIn`.

### Storing snapshots

Before UTXO-HD, ledger state snapshots were CBOR-serialized files containing a
full `ExtLedgerState blk` value. Now there is a separation between the
`ExtLedgerState blk EmptyMK` file and the `LedgerTables (ExtLedgerState blk)
ValuesMK`. This means that snapshots from before UTXO-HD are incompatible with
the UTXO-HD design and replaying the chain will be needed when enabling UTXO-HD
for the first time. Moreover, snapshots created when using one of the UTXO-HD
backends cannot be used with the other backend, and will require a replay.

|           | `ExtLedgerState blk EmptyMK`      | `LedgerTables (ExtLedgerState blk) ValuesMK`      | Live tables                   |
|-----------|-----------------------------------|---------------------------------------------------|-------------------------------|
| In-memory | `<db-root>/ledger/<slotno>/state` | `<db-root>/ledger/<slotno>/state/tables/tvar`     | N/A                           |
| On-disk   | `<db-root>/ledger/<slotno>/state` | `<db-root>/ledger/<slotno>/state/tables/data.mdb` | `<db-root>/ledgerdb/data.mdb` |

In the tables part of the snapshot, the in-memory backend will store a
serialization of the `Map (TxIn (CardanoBlock c)) (TxOut (CardanoBlock c))`,
whereas the on-disk backend will store a copy of the LMDB database.

## Impact on the node

The **in-memory** backend should have very little impact in the node.

The cardano-node will perform two operations on startup, and each of them suffer a varying impact for the **on-disk** backend:

|         | When                                             | Impact                                                 | Estimated time difference |
|---------|--------------------------------------------------|--------------------------------------------------------|---------------------------|
| Syncing | The node has no blocks                           | Low, cryptographic operations dominate the performance | 16h vs 17h                |
| Replay  | The node does not have a valid LedgerDB snapshot | High                                                   | 2h vs 3.5h                |

Note neither of these will be frequent scenarios.

As for the behavior of a cardano-node that is synced to the tip of the chain,
the impact of UTXO-HD should not be problematic because, given the pace at which
blocks are produced (on average every 20s), there is enough time to perform the
UTXO-HD operations.

The mempool likely won't be able to sustain the same peak throughput as before
UTxO-HD, but it should suffice for the typical load between blocks, and even
between the third block, since the mempool buffers more transactions than fit in
one block.
