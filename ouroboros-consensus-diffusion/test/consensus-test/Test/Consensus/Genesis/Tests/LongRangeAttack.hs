{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Test.Consensus.Genesis.Tests.LongRangeAttack (tests) where

import           Control.Monad.IOSim (runSimOrThrow)
import           Ouroboros.Consensus.Block.Abstract (HeaderHash)
import           Ouroboros.Network.AnchoredFragment (headAnchor)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.Genesis.Setup.Classifiers
import           Test.Consensus.PeerSimulator.Run (noTimeoutsSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.QuickCheck.Extras (unsafeMapSuchThatJust)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TestBlock (TestBlock, unTestHash)

tests :: TestTree
tests =
  testGroup "long range attack" [
    testProperty "one adversary" (prop_longRangeAttack 1 [10])
    -- TODO we don't have useful classification logic for multiple adversaries yet – if a selectable
    -- adversary is slow, it might be discarded before it reaches critical length because the faster
    -- ones have served k blocks off the honest chain if their fork anchor is further down the line.
    -- ,
    -- testProperty "three adversaries" (prop_longRangeAttack 1 [2, 5, 10])
  ]

genChainsAndSchedule ::
  Word ->
  ScheduleType ->
  QC.Gen (GenesisTest, PointScheduleConfig, PointSchedule)
genChainsAndSchedule numAdversaries scheduleType =
  unsafeMapSuchThatJust do
    gt <- genChains numAdversaries
    let scheduleConfig = defaultPointScheduleConfig (gtSecurityParam gt)
    pure $ ((gt, scheduleConfig,) <$> genSchedule scheduleConfig scheduleType (gtBlockTree gt))

prop_longRangeAttack :: Int -> [Int] -> QC.Gen QC.Property
prop_longRangeAttack honestFreq advFreqs = do
  (genesisTest, scheduleConfig, schedule) <- genChainsAndSchedule (fromIntegral (length advFreqs)) (Frequencies freqs)
  let Classifiers {..} = classifiers genesisTest

  -- TODO: not existsSelectableAdversary ==> immutableTipBeforeFork svSelectedChain

  pure $ withMaxSuccess 10 $
    classify genesisWindowAfterIntersection "Full genesis window after intersection" $
    allAdversariesSelectable
    ==>
    runSimOrThrow $
      runTest
        (noTimeoutsSchedulerConfig scheduleConfig)
        genesisTest
        schedule
        $ exceptionCounterexample $ \StateView{svSelectedChain} ->
            not $ isHonestTestFragH svSelectedChain

  where
    freqs = mkPeers honestFreq advFreqs

    isHonestTestFragH :: TestFragH -> Bool
    isHonestTestFragH frag = case headAnchor frag of
        AF.AnchorGenesis   -> True
        AF.Anchor _ hash _ -> isHonestTestHeaderHash hash

    isHonestTestHeaderHash :: HeaderHash TestBlock -> Bool
    isHonestTestHeaderHash = all (0 ==) . unTestHash
