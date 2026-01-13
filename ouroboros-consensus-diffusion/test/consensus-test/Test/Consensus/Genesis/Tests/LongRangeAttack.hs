{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Long range attack tests.
module Test.Consensus.Genesis.Tests.LongRangeAttack
  ( TestKey
  , testSuite
  ) where

import Data.Functor (($>))
import Ouroboros.Consensus.Block.Abstract (GetHeader, HasHeader)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Test.Consensus.Genesis.Setup
import Test.Consensus.Genesis.Setup.Classifiers
  ( allAdversariesForecastable
  , allAdversariesSelectable
  , classifiers
  )
import Test.Consensus.Genesis.TestSuite
import Test.Consensus.PeerSimulator.Run (defaultSchedulerConfig)
import qualified Test.Consensus.PointSchedule as Schedule
import Test.Tasty.QuickCheck
import Test.Util.Orphans.IOLike ()

-- | Default adjustment of required property test passes.
-- Can be set individually on each test definition.
adjustDesiredPasses :: Int -> Int
adjustDesiredPasses = (`div` 10)

-- | Each value of this type uniquely corresponds to a test defined in this module.
data TestKey = LongRangeAttack
  deriving stock (Eq, Ord, Generic)
  deriving SmallKey via Generically TestKey

testSuite ::
  ( HasHeader blk
  , GetHeader blk
  , IssueTestBlock blk
  ) =>
  TestSuite blk TestKey
testSuite = group "long range attack" $ newTestSuite $ \case
  -- NOTE: We want to keep this test to show that Praos is vulnerable to this
  -- attack but Genesis is not. This requires to first fix it as mentioned
  -- below.
  LongRangeAttack -> test_longRangeAttack

-- | This test case features a long-range attack with one adversary. The honest
-- peer serves the block tree trunk, while the adversary serves its own chain,
-- forking off the trunk by at least @k@ blocks, but less good than the trunk.
-- The adversary serves the chain more rapidly than the honest peer. We check at
-- the end that the selection is honest. This property does not hold with Praos,
-- but should hold with Genesis.
test_longRangeAttack ::
  forall blk.
  ( AF.HasHeader blk
  , GetHeader blk
  , IssueTestBlock blk
  ) =>
  ConformanceTest blk
test_longRangeAttack =
  mkConformanceTest
    "one adversary"
    adjustDesiredPasses
    id
    ( do
        -- Create a block tree with @1@ alternative chain.
        gt@GenesisTest{gtBlockTree} <- genChains (pure 1)
        -- Create a 'longRangeAttack' schedule based on the generated chains.
        ps <- Schedule.stToGen (Schedule.longRangeAttack gtBlockTree)
        let cls = classifiers gt
        if allAdversariesSelectable cls && allAdversariesForecastable cls
          then pure $ gt $> ps
          else discard
    )
    defaultSchedulerConfig
    mempty
    -- FIXME: This is the expected behaviour of Praos to be reversed with
    -- Genesis. But we are testing Praos for the moment. Do not forget to
    -- use `Test.Consensus.PointSchedule.Shrinking.shrinkPeerSchedules`
    -- above when removing this negation.
    (\genesisTest -> not . selectedHonestChain genesisTest)
