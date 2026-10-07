### Patch

- Read the current set-snapshot pool distribution through the ledger getter in Praos and Peras adapters after the upstream forecasting refactor.

- Delegate initial stake snapshots and Leios committee seeding to ledger initialization. The upstream ledger snapshot format changes require replay of existing ledger-state snapshots.

- Update ledger, key and configuration dependency bounds for the Receiving-aware ledger source proposal. Validate native/key protected recipient admission and parent/child execution-unit capacity through existing ledger adapters.

- Render Receiving purposes by original output index. Missing-redeemer diagnostics retain all purposes sharing a script hash in source order: singleton values keep their existing JSON shape, while multiple values use an array.
