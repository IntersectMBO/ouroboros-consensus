{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Consensus.Cardano.DiffusionPipelining (tests) where

import Control.Monad (replicateM)
import Data.Containers.ListUtils (nubOrd)
import Data.List (sort)
import Data.Proxy
import Data.SOP.BasicFunctors
import Data.SOP.Constraint
import Data.SOP.Strict
import Data.Traversable (for)
import Ouroboros.Consensus.Block
import Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import Ouroboros.Consensus.Cardano (CardanoBlock)
import Ouroboros.Consensus.HardFork.Combinator
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import Ouroboros.Consensus.Protocol.Abstract
import Ouroboros.Consensus.Protocol.PBFT
import Ouroboros.Consensus.Shelley.Eras
import Ouroboros.Consensus.Shelley.Ledger
  ( ShelleyBlock
  , ShelleyCompatible
  )
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Node.DiffusionPipelining
import Ouroboros.Consensus.TypeFamilyWrappers
import Test.Cardano.Ledger.Binary.Arbitrary ()
import Test.Cardano.Ledger.Core.Arbitrary ()
import Test.Ouroboros.Consensus.DiffusionPipelining
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup
    "Cardano diffusion pipelining"
    [ testProperty "subsequence consistency" $
        prop_cardanoDiffusionPipeliningSubsequenceConsistency
    ]

prop_cardanoDiffusionPipeliningSubsequenceConsistency :: Property
prop_cardanoDiffusionPipeliningSubsequenceConsistency =
  forAllShrink
    (genTentativeHeaderViews p)
    shrinkThvs
    (prop_diffusionPipeliningSubsequenceConsistency p)
 where
  p = Proxy @(CardanoBlock StandardCrypto)

  shrinkThvs = shrinkList (const [])

class GenTentativeHeaderViews blk where
  genTentativeHeaderViews :: Proxy blk -> Gen [TentativeHeaderView blk]

instance All GenTentativeHeaderViews xs => GenTentativeHeaderViews (HardForkBlock xs) where
  genTentativeHeaderViews _ =
    fmap OneEraTentativeHeaderView
      . foldMap hsequence'
      . hcollapse
      . hap injections
      <$> gen
   where
    gen :: Gen (NP ([] :.: WrapTentativeHeaderView) xs)
    gen =
      hctraverse'
        (Proxy @GenTentativeHeaderViews)
        (\p -> Comp . fmap WrapTentativeHeaderView <$> genTentativeHeaderViews p)
        (hpure Proxy)

instance GenTentativeHeaderViews ByronBlock where
  genTentativeHeaderViews _ =
    nubOrd . sort <$> listOf do
      bno <- arbitrary
      isEBB <- toIsEBB <$> arbitrary
      pure $ SelectView bno (PBftTiebreakerView isEBB)

instance ShelleyCompatible proto era => GenTentativeHeaderViews (ShelleyBlock proto era) where
  genTentativeHeaderViews _ = do
    bnos <- nubOrd <$> orderedList
    issuerHashes <- nubOrd <$> replicateM numIssuers arbitrary
    hotIdentities <-
      concat <$> for issuerHashes \issuerHash -> do
        -- Due to the constraints placed by the OCERT rule on how the issue
        -- number can evolve, the number of issue numbers per block number and
        -- issuer (cold) identity is bounded. Note that we don't actually
        -- enforce those exact constraints here across different block numbers
        -- as their details are not relevant for this test.
        numIssueNos <- elements [1, 2]
        issueNos <- take numIssueNos . iterate succ <$> arbitrary
        pure $ HotIdentity issuerHash <$> issueNos
    concat <$> for bnos \bno -> do
      hotIds <- shuffle =<< sublistOf hotIdentities
      pure $ ShelleyTentativeHeaderView bno <$> hotIds
   where
    -- Upper bound on the number of issuer identities
    numIssuers = 5
