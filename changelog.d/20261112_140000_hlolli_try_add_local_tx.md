### Breaking

- Add `tryAddLocalTx` to the `Mempool` record. It attempts local admission
  without waiting for the admission FIFO or free capacity, returning
  `MempoolBusy` or `MempoolFull` separately from ledger validation errors.
  Existing `addTx` behavior is unchanged; callers constructing `Mempool`
  records must supply the new field.
