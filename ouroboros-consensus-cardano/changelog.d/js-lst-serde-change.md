### Breaking

- Serialization of the LedgerState for Conway has changed. Reading a
  ledger state snapshot will fail, but doing a replay of the chain
  will suffice to bring the node back up.