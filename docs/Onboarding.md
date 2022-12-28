## Design Documents

The following artifacts influence and/or describe the Consensus implementation.

  * From IOHK researchers:

      * Ouroboros BFT https://iohk.io/en/research/library/papers/ouroboros-bfta-simple-byzantine-fault-tolerant-consensus-protocol/

      * Ouroboros Praos https://iohk.io/en/research/library/papers/ouroboros-praosan-adaptively-securesemi-synchronous-proof-of-stake-protocol/

      * Ouroboros Genesis https://iohk.io/en/research/library/papers/ouroboros-genesiscomposable-proof-of-stake-blockchains-with-dynamic-availability/

      * Internal notes, on the IOHK Google Docs

  * The ledger specifications, cf https://github.com/input-output-hk/cardano-ledger/blob/master/README.md , especially:

      * "Shelley design specification" -> "Design Specification for Delegation
        and Incentives in Cardano" (as of this line's latest commit)

      * "Shelley ledger formal specification" -> "A Formal Specification of the
        Cardano Ledger" (as of this line's latest commit)

      * "Byron chain specification" -> "Specification of the Blockchain Layer"
        (as of this line's latest commit)

      * "Byron ledger specification" -> "A Formal Specification of the Cardano
        Ledger (for the Byron release)" (as of this line's latest commit)

  * Documents and presentations linked in this repository's
    [README.md](../../README.md), especially:

      * "Introduction to the design of Data Diffusion and Networking of Cardano
        Shelley"

      * "The Cardano Consensus (and Storage Layer)" (aka "The Consensus Report")

  * Large comments, for example in these modules. This list is a continual work
    in progress -- if you find some comment to be particularly illuminating,
    please open a PR adding it here.

      * `Ouroboros.Consensus.Util.ResourceRegistry`
      * `Ouroboros.Consensus.HeaderValidation`
      * `Ouroboros.Consensus.Mempool.API`
      * `Ouroboros.Consensus.Forecast`
      * `Ouroboros.Consensus.HardFork.History.EraParams`
      * `Ouroboros.Consensus.HardFork.History.Qry`
      * `Ouroboros.Consensus.HardFork.History.Summary`
      * `Ouroboros.Consensus.Protocol.Abstract`
      * `Ouroboros.Consensus.Storage.ChainDB.API`
      * `Ouroboros.Consensus.Storage.ChainDB.Impl.ChainSel`
      * `Ouroboros.Consensus.Storage.ChainDB.Impl.Iterator`
      * `Ouroboros.Network.AnchoredFragment`
      * `Ouroboros.Consensus.MiniProtocol.ChainSync.Client`
      * `Ouroboros.Network.BlockFetch.Decision`
      * `Network.TypedProtocol.Core`

  * CI-built Haddock, at https://input-output-hk.github.io/ouroboros-network/

  * IOHK media:

      * https://iohk.io/en/blog/posts/2017/11/03/writing-a-high-assurance-blockchain-implementation/

      * https://iohk.io/en/blog/posts/2018/06/04/semi-formal-development-the-cardano-wallet/

      * 2018 August wallet video https://www.youtube.com/watch?v=6VWCB0_uLLw

      * https://iohk.io/en/blog/posts/2020/05/07/combinator-makes-easy-work-of-shelley-hard-fork/

      * 2020 July hard fork combinator presentation at Cardano2020, on YouTube
        [part 1](https://www.youtube.com/watch?v=D8OTZULEsaI) and [part 2](
        https://www.youtube.com/watch?v=wNZq6VPLIXg)

      * https://iohk.io/en/blog/posts/2020/05/28/the-abstract-nature-of-the-consensus-layer/
