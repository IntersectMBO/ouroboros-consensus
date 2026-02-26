{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Consensus.Genesis.Tests.LongRangeAttack (tests) where

import Data.Functor (($>))
import Test.Consensus.Genesis.Setup
import Test.Consensus.Genesis.Setup.Classifiers
  ( allAdversariesForecastable
  , allAdversariesSelectable
  , classifiers
  )
import Test.Consensus.PeerSimulator.Run (defaultSchedulerConfig)
import Test.Consensus.PointSchedule
import Test.Consensus.PointSchedule.Shrinking (shrinkPeerSchedules)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Util.Orphans.IOLike ()
import Test.Util.TestBlock (TestBlock)
import Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup
    "long range attack"
    [ -- NOTE: We want to keep this test to show that Praos is vulnerable to this
      -- attack but Genesis is not. This requires to first fix it as mentioned
      -- above.
      --
      adjustQuickCheckTests (`div` 10) $
        testProperty "one adversary" prop_longRangeAttack
    ]

-- | This test case features a long-range attack with one adversary. The honest
-- peer serves the block tree trunk, while the adversary serves its own chain,
-- forking off the trunk by at least @k@ blocks, but less good than the trunk.
-- The adversary serves the chain more rapidly than the honest peer. We check at
-- the end that the selection is honest. This property does not hold with Praos,
-- but should hold with Genesis.
prop_longRangeAttack :: Property
prop_longRangeAttack =
  -- NOTE: `shrinkPeerSchedules` only makes sense for tests that expect the
  -- honest node to win. Hence the `noShrinking`.

  noShrinking $
    forAllGenesisTest @TestBlock
      ( do
          -- Create a block tree with @1@ alternative chain.
          gt@GenesisTest{gtBlockTree} <- genChains (pure 1)
          -- Create a 'longRangeAttack' schedule based on the generated chains.
          ps <- stToGen (longRangeAttack gtBlockTree)
          let cls = classifiers gt
          if allAdversariesSelectable cls && allAdversariesForecastable cls
            then pure $ gt $> ps
            else discard
      )
      defaultSchedulerConfig
      shrinkPeerSchedules
      -- NOTE: This is the expected behaviour of Praos to be reversed with
      -- Genesis. But we are testing Praos for the moment. Do not forget to remove
      -- 'noShrinking' above when removing this negation.
      (\genesisTest -> not . selectedHonestChain genesisTest)
