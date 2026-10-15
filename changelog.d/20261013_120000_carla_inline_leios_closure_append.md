<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Patch

- `inlineLeiosClosure` now adds the EB's transactions after the block's own,
  instead of replacing them.

- At startup, when the node replays stored blocks, `resolveLeiosBlock` now
  adds an EB's transactions only to a CertRB, as `applyBlock` already did.
  Before, a block after an uncertified EB lost its own transactions to the
  EB's, or the node crashed if it lacked them.
