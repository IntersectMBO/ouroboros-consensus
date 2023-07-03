<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
-->

<!--
### Patch

- A bullet item for the Patch category.

-->

### Non-Breaking

- Change how diffusion initializes block forging credentials: now there's a `TMVar` that
  monitors so we can enable/disable block forging dynamically.
  - There's also a `blockForgingController` that keeps an eye at this `TMVar`.

- Adds new trace `TraceAdoptionThreadDied SlotNo blk` to `TraceForgeEvent`

### Breaking

- Add a new argument to `RekeyM` type, due to the extraction of the block forging
  credentials over at `ouroboros-consensus`.
  - Refactor to accommodate this change

- Add block forging credentials to `TestNodeInitialization`

- Change the type signature of most functions to receive the block forging credentials as
  an argument, since it now doesn't come in the usual bundle.

- Add `setBlockForging` to `NodeKernel` which must be used to set / control
  block forging of the consensus layer.

