{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Test.Consensus.Genesis.Tests.Uniform (tests) where

import           Cardano.Slotting.Slot (SlotNo (SlotNo), WithOrigin (..))
import           Control.Monad.IOSim (runSimOrThrow)
import           Data.List (intercalate)
import           Ouroboros.Consensus.Util.Condense (Condense (condense))
import qualified Ouroboros.Network.AnchoredFragment as AF
import           Test.Consensus.BlockTree (BlockTree (..))
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.Genesis.Setup.Classifiers
import           Test.Consensus.PeerSimulator.Run
                     (SchedulerConfig (scEnableGdd), noTimeoutsSchedulerConfig,
                     scTraceState)
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PointSchedule
import           Test.Ouroboros.Consensus.ChainGenerator.Params (Delta (Delta))
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.QuickCheck (le)
import           Test.Util.TestEnv (adjustQuickCheckMaxSize,
                     adjustQuickCheckTests)
import           Text.Printf (printf)

tests :: TestTree
tests =
  adjustQuickCheckTests (`div` 10) $
  adjustQuickCheckMaxSize (`div` 10) $
  testProperty "uniform random schedule" prop_uniformSchedule

makeProperty ::
  GenesisTest ->
  Int ->
  StateView ->
  [PeerId] ->
  Property
makeProperty genesisTest advCount StateView {svSelectedChain} killed =
  classify genesisWindowAfterIntersection "Full genesis window after intersection" $
  classify (isOrigin immutableTipHash) "Immutable tip is Origin" $
  label disconnected $
  classify (advCount < length (btBranches gtBlockTree)) "Some adversaries performed rollbacks" $
  counterexample killedPeers $
  -- We require the honest chain to fit a Genesis window, because otherwise its tip may suggest
  -- to the governor that the density is too low.
  longerThanGenesisWindow ==>
  conjoin [
    counterexample "The honest peer was disconnected" (not (HonestPeer `elem` killed)),
    counterexample ("The immutable tip is not honest: " ++ show immutableTip) $
    property (isHonest immutableTipHash),
    immutableTipIsRecent
  ]
  where
    immutableTipIsRecent =
      counterexample ("Age of the immutable tip: " ++ show immutableTipAge) $
      immutableTipAge `le` s + fromIntegral d + 1

    SlotNo immutableTipAge = case (honestTipSlot, immutableTipSlot) of
      (At h, At i)   -> h - i
      (At h, Origin) -> h
      _              -> 0

    isOrigin = null

    isHonest = all (0 ==)

    immutableTipHash = simpleHash (AF.anchorToHash immutableTip)

    immutableTip = AF.anchor svSelectedChain

    immutableTipSlot = AF.anchorToSlotNo (AF.anchor svSelectedChain)

    disconnected =
      printf "disconnected %.1f%% of adversaries" disconnectedPercent

    disconnectedPercent :: Double
    disconnectedPercent =
      100 * fromIntegral (length killed) / fromIntegral advCount

    killedPeers = case killed of
      [] -> "No peers were killed"
      peers -> "Some peers were killed: " ++ intercalate ", " (condense <$> peers)

    honestTipSlot = AF.headSlot (btTrunk gtBlockTree)

    GenesisTest {gtBlockTree, gtGenesisWindow = GenesisWindow s, gtDelay = Delta d} = genesisTest

    Classifiers {genesisWindowAfterIntersection, longerThanGenesisWindow} = classifiers genesisTest

prop_uniformSchedule :: QC.Gen QC.Property
prop_uniformSchedule = do
  (genesisTest, schedule) <- qcFromSchedulePoints $ do
    genesisTest <- genChains (QC.choose (1, 4))
    schedule <- genUniformSchedulePoints genesisTest
    pure (genesisTest, schedule)
  pure $
    runSimOrThrow $
    runTest schedulerConfig genesisTest schedule $
    exceptionCounterexample $
    makeProperty genesisTest (length (peerIds schedule) - 1)

  where
    schedulerConfig = (noTimeoutsSchedulerConfig scheduleConfig) {scEnableGdd = True, scTraceState = False}

    scheduleConfig = defaultPointScheduleConfig

genUniformSchedulePoints
  :: GenesisTest -> QC.Gen (Peers PeerSchedule)
genUniformSchedulePoints gt =
  qcSchedule (uniformPoints (gtGenesisWindow gt) (gtDelay gt) False (gtBlockTree gt))
