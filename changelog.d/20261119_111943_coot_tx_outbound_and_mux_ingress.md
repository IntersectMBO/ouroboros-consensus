### Breaking

- Move the transaction-submission logic version argument from
  `Ouroboros.Consensus.Network.NodeToNode.mkHandlers` to `Ouroboros.Consensus.Node.mkNodeKernelArgs`.
  Callers that construct `NodeKernelArgs` directly set its `txSubmissionLogicVersion` field.
