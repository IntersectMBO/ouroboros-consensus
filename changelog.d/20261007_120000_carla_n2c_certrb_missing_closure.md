<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Non-Breaking

- `Ouroboros.Consensus.MiniProtocol.ChainSync.Server` exports
  `serveBlockWithLeiosClosure`, the step `chainSyncBlocksServer` runs for each
  block, and the new exception `CertRbClosureUnavailable`, which carries the
  CertRB's slot and the `LeiosClosureError`.

### Patch

- The node-to-client ChainSync server throws `CertRbClosureUnavailable` when it
  can't read the closure of the EB a CertRB certifies, which ends the client's
  connection. It used to send the CertRB without the EB's transactions. ChainSel
  only adopts a CertRB once its closure is in the LeiosDb, so this is a bug.

- The Leios ThreadNet test serves each node's chain through
  `serveBlockWithLeiosClosure`. From the node's own LeiosDb it must not throw.
  From an empty LeiosDb it must throw `CertRbClosureUnavailable` at the first
  CertRB.
