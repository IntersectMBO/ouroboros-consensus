<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Breaking

- `chainSyncBlocksServer` needs `HasHeader blk`.

### Non-Breaking

- New `inlineCertifiedEbTxs`, `CertRbWithoutEbTxs` and
  `CertRbWithoutEbTxsReason`, exported from
  `Ouroboros.Consensus.MiniProtocol.ChainSync.Server`.

### Patch

- The node-to-client ChainSync server no longer sends a CertRB without its
  EB's transactions. If it can't add them, it now throws `CertRbWithoutEbTxs`
  and ends the client's connection. Before, it sent the CertRB as stored and
  logged nothing.

- `unstable-consensus-testlib`'s `Test.Util.LeiosTestBlock` exports
  `LeiosTestCodecConfig`.
