### Breaking

- Renamed `QueryLedger` class to `BlockSupportsLedgerQuery`.
- `StreamAPI` was moved to the new `Ouroboros.Consensus.Storage.ImmutableDB.Stream` module.
   - A `StreamAPI` now can stream specific block components.
   - `NextBlock` was renamed to `NextItem`.
- Removed unused `Ouroboros.Consensus.Util.TraceSize`.
- Removed unused `assertEqWithMessage` function.
- `Mempool.removeTxs` now expects a `NonEmpty (GenTxId blk)` as an argument.
- VolatileDB traces were tweaked
    - `VolatileDB.OpenedVolatileDB` trace message now includes the maximum slot seen.
    - Added `VolatileDB.ClosedDB`.
- Deleted `Ouroboros.Consensus.Util.Some` in favour of `Ouroboros.Network.Protocol.LocalStateQuery.Codec.Some`.

### Non-Breaking

- Provide `NoThunks` instances for:
  - `Sum a`,
  - `RAWLock m st`,
  - `StrictTVar (WithEarlyExit m) a`,
  - `StrictSVar (WithEarlyExit m) a`
- Added `Complete` and `Incomplete` type aliases for arguments.
- Implement `HTrans` instance for `HardForkState`
- `SomeSecond` became poly-kinded.
