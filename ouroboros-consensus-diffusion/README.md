# Consensus diffusion layer

This packages glues together the [Network
layer](https://github.com/input-output-hk/ouroboros-network) and the [Consensus
core package](../ouroboros-consensus) to be able to initialize the node and
start all the networking protocols. The main module is
`Ouroboros.Consensus.NodeKernel`.

It also defines test-suites that run simulated networks of nodes that have to
succesfully converge to the same chain when running, what we call the _ThreadNet
tests_.
