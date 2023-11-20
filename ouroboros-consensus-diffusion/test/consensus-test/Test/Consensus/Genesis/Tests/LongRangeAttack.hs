{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Test.Consensus.Genesis.Tests.LongRangeAttack (tests) where

import           Control.Monad.IOSim (runSimOrThrow)
import           Data.List (intercalate)
import           Ouroboros.Consensus.Block.Abstract (HeaderHash)
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Network.AnchoredFragment (headAnchor)
import qualified Ouroboros.Network.AnchoredFragment as AF
import           System.Random.Stateful (runSTGen_)
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.Genesis.Setup.Classifiers
import           Test.Consensus.PeerSimulator.Run (noTimeoutsSchedulerConfig)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.QuickCheck.Extras (unsafeMapSuchThatJust)
import           Test.QuickCheck.Random (QCGen)
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

genChainsAndSchedule :: PointScheduleConfig -> Word -> ScheduleType -> QC.Gen (GenesisTest, PointSchedule)
genChainsAndSchedule scheduleConfig numAdversaries scheduleType =
  unsafeMapSuchThatJust do
    gt <- genChains numAdversaries
    seed :: QCGen <- arbitrary
    pure ((gt,) <$> runSTGen_ seed (\ g -> genSchedule g scheduleConfig scheduleType (gtBlockTree gt)))

newLRA :: Bool
newLRA = True

prop_longRangeAttack :: Int -> [Int] -> QC.Gen QC.Property
prop_longRangeAttack honestFreq advFreqs = do
  (genesisTest, schedule) <- genChainsAndSchedule scheduleConfig (fromIntegral (length advFreqs)) sched
  let cls = classifiers genesisTest

  -- TODO: not existsSelectableAdversary ==> immutableTipBeforeFork svSelectedChain

  pure $ withMaxSuccess 10 $
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
    sched | newLRA = NewLRA
          | otherwise = Frequencies freqs

    freqs = mkPeers honestFreq advFreqs

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
