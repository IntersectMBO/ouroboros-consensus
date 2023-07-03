<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
-->

<!--
### Patch

- A bullet item for the Patch category.

-->

### Non-Breaking

- Refactor code because block forging credentials got extracted out of
  `ProtocolInfo` type.

### Breaking

- Change the return type of numerous functions to include block forging credentials since
  they got extracted out of `ProtocolInfo` type.
  - Refactor the type signatures to accommodate the fact that `ProtocolInfo` does not
  need monad type variable.
