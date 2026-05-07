# Header Validation

Top-level validation a node performs when it receives a new block header.

**References:**
- Agda: `Spec.ChainHead` ([ChainHead.lagda](../../agda-spec/src/Spec/ChainHead.lagda))
- Implementation: `Ouroboros.Consensus.HeaderValidation:validateHeader`

**Top-level rule:**

```quint
def validateHeader(prev: Option[LastAppliedBlock], header: BlockHeader): bool = and {
  validSequence(prev, header.body),
  validKesPeriod(header.body.slot, header.body.oc),
  // ... more checks added as we specify them
}
```

Each section below defines one of these predicates in detail.

## Sequence checks

A new header must extend the chain correctly: its slot must advance, its block number must be exactly one more, and its previous-hash pointer must match.

**References:**
- Implementation: `Ouroboros.Consensus.HeaderValidation:validateEnvelope`

**Types:**

```quint
type LastAppliedBlock = {
  slot: Slot,
  blockNo: BlockNo,
  headerHash: HashHeader
}
```

**Rule (RULE-HV-1):**

```quint
def validSequence(prev: LastAppliedBlock, header: HeaderBody): bool = and {
  prev.slot < header.slot,
  prev.blockNo + 1 == header.blockNo,
  Some(prev.headerHash) == header.prevHeader
}
```

When there is no previous block (genesis), all three checks are trivially satisfied.

### Example

```
prev = { slot: 100, blockNo: 5, headerHash: "abc123" }
header = { slot: 105, blockNo: 6, prevHeader: Some("abc123"), ... }

validSequence(prev, header) = true
```

### Counter-examples

**Slot does not advance:**

```
prev = { slot: 100, blockNo: 5, headerHash: "abc123" }
header = { slot: 100, blockNo: 6, prevHeader: Some("abc123"), ... }

validSequence(prev, header) = false  // slot 100 is not greater than 100
```

**Block number skips:**

```
prev = { slot: 100, blockNo: 5, headerHash: "abc123" }
header = { slot: 105, blockNo: 7, prevHeader: Some("abc123"), ... }

validSequence(prev, header) = false  // 5 + 1 != 7
```

**Previous hash mismatch:**

```
prev = { slot: 100, blockNo: 5, headerHash: "abc123" }
header = { slot: 105, blockNo: 6, prevHeader: Some("def456"), ... }

validSequence(prev, header) = false  // "abc123" != "def456"
```

## Operational certificate checks

A new header must carry a valid operational certificate.
See [Operational certificate checks](./operational-certificate.md) for the full specification.
