{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Genesis.Tests.LongRangeAttack (tests) where

import           Data.Functor (($>))
import           Ouroboros.Consensus.Block.Abstract (Header, HeaderHash)
import           Ouroboros.Network.AnchoredFragment (headAnchor)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.Genesis.Setup.Classifiers
                     (allAdversariesForecastable, allAdversariesSelectable,
                     classifiers)
import           Test.Consensus.PeerSimulator.Run (defaultSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Shrinking (shrinkPeerSchedules)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock, unTestHash)
import           Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup "long range attack" [
    -- NOTE: We want to keep this test to show that Praos is vulnerable to this
    -- attack but Genesis is not. This requires to first fix it as mentioned
    -- above.
    --
    adjustQuickCheckTests (`div` 10) $
    testProperty "one adversary" prop_longRangeAttack
  ]

prop_longRangeAttack :: Property
prop_longRangeAttack =
  -- NOTE: `shrinkPeerSchedules` only makes sense for tests that expect the
  -- honest node to win. Hence the `noShrinking`.

  noShrinking $ forAllGenesisTest

    (do
        -- Create a block tree with @1@ alternative chain.
        gt@GenesisTest{gtBlockTree} <- genChains (pure 1)
        -- Create a 'longRangeAttack' schedule based on the generated chains.
        ps <- stToGen (longRangeAttack gtBlockTree)
        let cls = classifiers gt
        if allAdversariesSelectable cls && allAdversariesForecastable cls
          then pure $ gt $> ps
          else discard)

    defaultSchedulerConfig

    shrinkPeerSchedules

    -- NOTE: This is the expected behaviour of Praos to be reversed with
    -- Genesis. But we are testing Praos for the moment. Do not forget to remove
    -- `noShrinking` above when removing this negation.
    (\_ -> not . isHonestTestFragH . svSelectedChain)

  where
    isHonestTestFragH :: AF.AnchoredFragment (Header TestBlock) -> Bool
    isHonestTestFragH frag = case headAnchor frag of
        AF.AnchorGenesis   -> True
        AF.Anchor _ hash _ -> isHonestTestHeaderHash hash

    isHonestTestHeaderHash :: HeaderHash TestBlock -> Bool
    isHonestTestHeaderHash = all (0 ==) . unTestHash
