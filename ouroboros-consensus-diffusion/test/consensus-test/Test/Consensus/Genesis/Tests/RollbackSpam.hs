{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Test.Consensus.Genesis.Tests.RollbackSpam (tests) where

import           Control.Monad.IOSim (runSimOrThrow)
import           Data.Maybe (fromMaybe)
import           Ouroboros.Consensus.Config (SecurityParam (SecurityParam))
import           Test.Consensus.Genesis.Setup
import           Test.Consensus.Genesis.Setup.Classifiers
import           Test.Consensus.PeerSimulator.Run (noTimeoutsSchedulerConfig)
import           Test.Consensus.PointSchedule
import qualified Test.QuickCheck as QC
import           Test.QuickCheck
import           Test.QuickCheck.Extras (unsafeMapSuchThatJust)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Util.Orphans.IOLike ()

tests :: TestTree
tests = testProperty "rollback spam" prop_rollbackSpam

genChainsAndSchedule :: QC.Gen (GenesisTest, PointScheduleConfig, PointSchedule)
genChainsAndSchedule =
  unsafeMapSuchThatJust $ do
    gt <- genChains 2
    let secParam@(SecurityParam k) = gtSecurityParam gt
    let scheduleConfig = defaultPointScheduleConfig secParam
        mode = RollbackRepeatBranch
        bulk = True
        freq | bulk = 2
             | otherwise = fromIntegral k + 1
        tree = gtBlockTree gt
        result = do
          (schedule, newTree) <- genSchedule' scheduleConfig (RollbackSpam (mkPeers 1 [freq, freq]) bulk mode) tree
          pure (gt {gtBlockTree = fromMaybe tree newTree}, scheduleConfig, schedule)
    pure result

prop_rollbackSpam :: QC.Gen QC.Property
prop_rollbackSpam = do
  (genesisTest, scheduleConfig, schedule) <- genChainsAndSchedule
  let Classifiers {..} = classifiers genesisTest

  pure $ withMaxSuccess 10 $
    classify genesisWindowAfterIntersection "Full genesis window after intersection" $
    runSimOrThrow $
      runTest
        (noTimeoutsSchedulerConfig scheduleConfig)
        genesisTest
        schedule
        $ exceptionCounterexample $ \ _ -> True
