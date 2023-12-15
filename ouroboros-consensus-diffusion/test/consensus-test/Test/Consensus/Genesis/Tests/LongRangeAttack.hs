{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Genesis.Tests.LongRangeAttack (tests) where

import           Control.Monad.IOSim (runSimOrThrow)
import           Data.List (intercalate)
import           Ouroboros.Consensus.Block.Abstract (HeaderHash)
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Network.AnchoredFragment (headAnchor)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.Genesis.Setup.Classifiers
import           Test.Consensus.PeerSimulator.Run (noTimeoutsSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock, unTestHash)
import           Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup "long range attack" [
    adjustQuickCheckTests (`div` 10) $
    testProperty "one adversary" prop_longRangeAttack
    -- TODO we don't have useful classification logic for multiple adversaries yet – if a selectable
    -- adversary is slow, it might be discarded before it reaches critical length because the faster
    -- ones have served k blocks off the honest chain if their fork anchor is further down the line.
    -- ,
    -- testProperty "three adversaries" (prop_longRangeAttack 1 [2, 5, 10])
  ]

prop_longRangeAttack :: QC.Gen QC.Property
prop_longRangeAttack = do
  -- | Create a block tree with @1@ alternative chain.
  genesisTest <- genChains (pure 1)

  -- | Create a 'longRangeAttack' schedule based on the generated chains.
  schedule <- fromSchedulePoints <$> stToGen (longRangeAttack (gtBlockTree genesisTest))
  let cls = classifiers genesisTest

  -- TODO: not existsSelectableAdversary ==> immutableTipBeforeFork svSelectedChain

  pure $
    classify (genesisWindowAfterIntersection cls) "Full genesis window after intersection" $
    allAdversariesSelectable cls
    ==>
    runSimOrThrow $
      runTest
        (noTimeoutsSchedulerConfig scheduleConfig)
        genesisTest
        schedule
        $ exceptionCounterexample $ \StateView{svSelectedChain} killed ->
            killCounterexample killed $
            -- This is the expected behavior of Praos to be reversed with Genesis.
            -- But we are testing Praos for the moment
            not (isHonestTestFragH svSelectedChain)

  where
    killCounterexample = \case
      [] -> property
      killed -> counterexample ("Some peers were killed: " ++ intercalate ", " (condense <$> killed))

    isHonestTestFragH :: TestFragH -> Bool
    isHonestTestFragH frag = case headAnchor frag of
        AF.AnchorGenesis   -> True
        AF.Anchor _ hash _ -> isHonestTestHeaderHash hash

    isHonestTestHeaderHash :: HeaderHash TestBlock -> Bool
    isHonestTestHeaderHash = all (0 ==) . unTestHash

    scheduleConfig = defaultPointScheduleConfig
