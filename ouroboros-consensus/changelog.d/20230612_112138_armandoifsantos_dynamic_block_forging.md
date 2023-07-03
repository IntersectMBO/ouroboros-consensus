<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
-->

<!--
### Patch

- A bullet item for the Patch category.

-->
### Non-Breaking

- Change the behaviour of `addBlockRunner` so that it notifies all blocked threads if interrupted.

- Add `closeBlocksToAdd` function

### Breaking

- Remove the `pInfoBlockForging` record field from the `ProtocolInfo` type.

- Remove `ProtocolInfo` monad parameter

- Change `AddBlockPromise` API
  - `blockProcessed` now wraps the return value in a new `Processed` type. This is needed
  for improving the async exception safety.

- Change `BlockToAdd` API
  - `varBlockProcessed` now wraps the return value in a new `Processed` type. This is needed
  for improving the async exception safety.
