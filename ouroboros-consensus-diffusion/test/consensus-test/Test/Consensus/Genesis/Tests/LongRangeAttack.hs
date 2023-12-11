{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Genesis.Tests.LongRangeAttack (tests) where

import           Ouroboros.Consensus.Block.Abstract (HeaderHash)
import           Ouroboros.Network.AnchoredFragment
                     (Anchor (Anchor, AnchorGenesis), anchor)
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.PeerSimulator.Run (noTimeoutsSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock, unTestHash)

tests :: TestTree
tests =
  testGroup "long range attack" [
    localOption (QuickCheckMaxSize 1) $
    testProperty "one adversary" prop_longRangeAttack
    ]

prop_longRangeAttack :: Property
prop_longRangeAttack =
  forAllGenesisTest'
    (pure $ NumBranches 1) NewLRA
    (noTimeoutsSchedulerConfig defaultPointScheduleConfig)
    (isHonestImmutableTip . svSelectedChain)

  where
    isHonestImmutableTip :: TestFragH -> Bool
    isHonestImmutableTip frag = case anchor frag of
        AnchorGenesis   -> True
        Anchor _ hash _ -> isHonestTestHeaderHash hash

    isHonestTestHeaderHash :: HeaderHash TestBlock -> Bool
    isHonestTestHeaderHash = all (0 ==) . unTestHash
