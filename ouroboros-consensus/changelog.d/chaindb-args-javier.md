### Breaking

- Tweak the ChainDB arguments:
  - Remove unused fields in `CDB`:
    - `cdbTraceLedger` this was *always* set to nullTracer, furthermore it would trace the whole LedgerDB.
    - `cdbChunkInfo` was never accessed from the ChainDB.
    - `cdbCheckIntegrity` was never accessed from the ChainDB.
  - Transform `ChainDbArgs` into an isomorphic product of the different arguments of the inner databases.
  - Define most common operations on `ChainDbArgs` as separate functions: `ensureValidateAll`, `updateTracer` and `updateDiskPolicyArgs`
- Tweak the LgrDB arguments:
  - `LgrDB.cfg` and `LgrDbArgs.lgrConfig` are now `LedgerDbCfg (ExtLedgerState blk)` instead of `TopLevelConfig blk`.
  - `defaultArgs` no longer expects a filesystem.
- Tweak the ImmutableDB arguments:
  - `defaultArgs` no longer expects a filesystem.
- Tweak the VolatileDB arguments:
  - `defaultArgs` no longer expects a filesystem.
- Hide the `Identity`/`Defaults` types in `Ouroboros.Consensus.Util.Args` in favor of `Complete`/`Incomplete`.
- Expose `noDefault` to replace `NoDefault`.
