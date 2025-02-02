### Breaking

- Updated to `ouroboros-network-0.19.0.2` & `ouroboros-network-framework-0.16`.
- `runWith` and `LowLevelRunNodeArgs` are no longer polymorphic in version
   data.
- `NodeToNode.initiator`, `NodeToNode.initiatorAndResponder` take negotiated
  `NodeToNodeVersionData` as an argument instead of `PeerSharing` (config
   option).
- `NodeToClient.respoinder` take negotiated `NodeToClientVersionData` as an
   argument.
