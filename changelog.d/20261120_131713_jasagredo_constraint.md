<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- Monomorphize types to `ExtLedgerState`:
  - `Forker`, `ForkerEnv`, `SuccessForkerAction`
  - `ValidateArgs`
  - `LedgerDB`, `LedgerDBEnv`, `LedgerDBHandle`, `LedgerDBState`. `LedgerDbCfgF`
  - `LedgerSeq`
  - `TestInternals`
  - `StateRef`, `LedgerTablesHandle`
  - `LSMConstraints`
  - `StreamingBackend`, `YieldArgs`, `SinkArgs`, `Yield` and `Sink`
- Deleted `LedgerDB'`, `TestInternals'`, `Forker'` and `LedgerSeq'`.
- Remove `All Top (HardForkIndices blk)` from
  `Ouroboros.Consensus.Peras.Context.embedBoundedEpochContext` which cascades as
  a now redundant constraint all over the codebase.

<!--
### Non-Breaking

- A bullet item for the Non-Breaking category.

-->
<!--
### Patch

- A bullet item for the Patch category.

-->
