### Patch

- Update ledger, key and configuration dependency bounds for the Receiving-aware ledger source proposal. Validate native/key protected recipient admission and parent/child execution-unit capacity through existing ledger adapters.

- Render Receiving purposes by original output index. Missing-redeemer diagnostics retain all purposes sharing a script hash in source order: singleton values keep their existing JSON shape, while multiple values use an array.
