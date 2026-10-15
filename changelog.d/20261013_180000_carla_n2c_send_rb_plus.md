<!--
A new scriv changelog fragment.

Uncomment the section that is right (remove the HTML comment wrapper).
For top level release notes, leave all the headers commented out.
-->

### Non-Breaking

- New `inlineCertifiedEbsFollower`, exported from
  `Ouroboros.Consensus.MiniProtocol.ChainSync.Server`. `chainSyncBlocksServer`
  uses it to wrap its follower.

### Patch

- Node-to-client ChainSync now puts a certified EB's transactions in the
  block RB that announced the EB, after RB's own. We call this block RB⁺. The
  ledger applies the transactions at RB, so clients now see them where the
  node does. Before, they were put in the CertRB. CertRBs are now sent
  unchanged.
  - A client that is catching up gets RB⁺ directly.
  - A client at the tip gets RB first. When the CertRB arrives, it gets a
    one-block rollback, then RB⁺, then the CertRB.
  - If the EB is not certified, the client gets plain RB.
  - Not yet handled: after a chain switch that keeps RB, or a reconnection, a
    client can be left with RB where it should have RB⁺, or the reverse.
