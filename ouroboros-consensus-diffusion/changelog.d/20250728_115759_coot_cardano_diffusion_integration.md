### Breaking

- `Ouroboros.Consensus.Network.NodeToNode.mkApps` takes `StdGen` as an argument, which is passed to `chain-sync` client.
- `LowLevelRunNodeArgs` and `StdRunNodeArgs` were changed to match `ouroboros-network-0.22`
- `NetworkP2PMode` was removed (non-p2p mode is removed from `ouroboros-network`).
- `Ouroboros.Consensus.Node.stdRunDataDiffusion` was changed to match `Cardano.Network.Diffusion` API.

