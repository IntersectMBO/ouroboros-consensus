# Instantiating Consensus

- [Ouroboros.Consensus.Tutorial.Simple](https://github.com/IntersectMBO/ouroboros-consensus/blob/main/ouroboros-consensus/src/unstable-tutorials/Ouroboros/Consensus/Tutorial/Simple.lhs): Simple round-robin instantiation of the abstract Ouroboros consensus protocol.
- [Ouroboros.Consensus.Tutorial.WithEpoch](https://github.com/IntersectMBO/ouroboros-consensus/blob/main/ouroboros-consensus/src/unstable-tutorials/Ouroboros/Consensus/Tutorial/WithEpoch.lhs): Example in which the leader schedule depends on data from the chain.

## Generating documents

From the `ouroboros-consensus` directory, run for your choice of `<output
file>`:

    pandoc -s -f markdown+lhs src-docs/Ouroboros/Consensus/Tutorial/Simple.lhs -o <output file>
