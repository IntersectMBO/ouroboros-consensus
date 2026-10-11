# LeiosDB

LeiosDB stores Leios endorser blocks (EBs) and their transactions.
Its SQLite backend keeps them in two files, called partitions:

- **Volatile partition** stores fresh EB announcements, bodies and transaction that the node downloads or forges.
- **Immutable partition** stores complete EBs (and their closure transactions) that a certificate on the immutable chain refers to.

The EBs and their closures are copied from the volatile to the immutable partition when they become older than [`k`](../references/glossary.md#security-parameter) (the Praos security parameter). See also the [ImmutableDB](../references/glossary.md#chaindb) glossary entry.

## Database schema

- `ebs` has one row for each EB *announcement*. Thus one EB hash can occur at several slots.
- `ebTxs` is the EB body. It has one row for each transaction of the EB, in order. It stores each EB hash only once.
- `txs` stores the bytes of each transaction once. Transactions stored in this table may belong to one, several or no EBs.

## Volatile partition

```mermaid
erDiagram
    ebs {
        INTEGER ebSlot PK
        BLOB ebHashBytes PK
        INTEGER ebBytesSize
        INTEGER missingTxCount "NULL: no body yet; >0: txs missing; 0: just completed; <0: notified"
        INTEGER status "0: volatile; 1: pinned for copy; 2: copied; 3: marked for GC"
    }
    ebTxs {
        BLOB ebHashBytes PK
        INTEGER txOffset PK
        BLOB txHashBytes
        INTEGER txBytesSize
    }
    txs {
        BLOB txHashBytes PK
        BLOB txBytes
        INTEGER txBytesSize
    }
    ebsMissingTxs {
        BLOB txHashBytes PK
        BLOB ebHashBytes PK
    }
    gcTxCandidates {
        BLOB txHashBytes PK
    }

    ebs }|--o{ ebTxs : "ebHashBytes"
    txs |o--o{ ebTxs : "txHashBytes"
    ebs }|--o{ ebsMissingTxs : "ebHashBytes"
    txs |o--o| gcTxCandidates : "txHashBytes"
```

The queries join the tables on the EB hash (`ebHashBytes`) and on the transaction hash (`txHashBytes`).

- Rows are inserted in `ebTxs` when transactions referenced by an EB body arrive.
- `ebsMissingTxs` lists, for each EB body, the transactions not yet in `txs`. `missingTxCount` counts them, and both are kept in step in one transaction.
- `gcTxCandidates` lists the transactions of EBs marked for GC. The GC sweep phase deletes each one from `txs` once no remaining EB refers to it.

Indexes:

| Index                           | On                                   | Used for                                                                 |
|---------------------------------|--------------------------------------|--------------------------------------------------------------------------|
| `idx_ebs_ebHashBytes`           | `ebs(ebHashBytes)`                   | Looking up an EB by hash                                                 |
| `idx_ebTxs_txHashBytes`         | `ebTxs(txHashBytes)`                 | GC of orphaned transactions: finding the EBs that refer to a transaction |
| `idx_ebsMissingTxs_ebHashBytes` | `ebsMissingTxs(ebHashBytes)`         | Finding the missing transactions of an EB                                |
| `idx_ebs_sweepable`             | `ebs(ebSlot) WHERE status IN (0, 2)` | The GC mark scan                                                         |
| `idx_ebs_markedForGc`           | `ebs(ebHashBytes) WHERE status = 3`  | The sweeper picking EBs to evict                                         |
| `idx_ebs_pinned`                | `ebs(ebSlot) WHERE status = 1`       | The copier picking EBs to copy                                           |

### Lifecycle of an EB row

The `status` column records where an `ebs` row is in promotion and garbage collection:

```mermaid
stateDiagram-v2
    direction LR
    [*] --> Volatile : announce
    Volatile --> Pinned : promote to become immutable
    Pinned --> Copied : copy to the immutable partition
    Volatile --> MarkedForGC : GC mark
    Copied --> MarkedForGC : GC mark
    MarkedForGC --> Pinned : re-promote before the sweep
    MarkedForGC --> [*] : GC sweep

    Volatile : 0 — volatile
    Pinned : 1 — pinned, awaiting copy
    Copied : 2 — copied, evictable
    MarkedForGC : 3 — marked for GC
```

## Immutable partition

```mermaid
erDiagram
    ebs {
        INTEGER ebSlot PK
        BLOB ebHashBytes PK
        INTEGER ebBytesSize
    }
    ebTxs {
        BLOB ebHashBytes PK
        INTEGER txOffset PK
        BLOB txHashBytes
        INTEGER txBytesSize
    }
    txs {
        BLOB txHashBytes PK
        BLOB txBytes
        INTEGER txBytesSize
    }

    ebs }|--|{ ebTxs : "ebHashBytes"
    txs ||--|{ ebTxs : "txHashBytes"
```

The copier copies only complete EBs, and GC never removes EBs from this partition.
Thus every `ebs` row has its full body in `ebTxs`, and every `ebTxs` row has its transaction in `txs`.
For the same reason, the immutable partition does not have `missingTxCount`, `status`, `ebsMissingTxs`, `gcTxCandidates`, or any index other than `idx_ebs_ebHashBytes`.
