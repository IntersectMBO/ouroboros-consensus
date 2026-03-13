{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Consensus.Genesis.Setup
  ( module Test.Consensus.Genesis.Setup.GenChains
  , ConformanceTest (..)
  , castHeaderHash
  , honestImmutableTip
  , mkConformanceTest
  , runConformanceTest
  , selectedHonestChain
  ) where

import Control.Exception (throw)
import Control.Monad.Class.MonadAsync
  ( AsyncCancelled (AsyncCancelled)
  )
import Control.Monad.IOSim (IOSim, runSimStrictShutdown)
import Control.Tracer (debugTracer, traceWith)
import Data.Maybe (mapMaybe)
import Ouroboros.Consensus.Block.Abstract
  ( ChainHash (..)
  , ConvertRawHash
  , GetHeader
  , Header
  )
import Ouroboros.Consensus.Block.SupportsDiffusionPipelining
  ( BlockSupportsDiffusionPipelining
  )
import Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode)
import Ouroboros.Consensus.HardFork.Abstract
import Ouroboros.Consensus.Ledger.Basics (LedgerState)
import Ouroboros.Consensus.Ledger.Inspect (InspectLedger)
import Ouroboros.Consensus.Ledger.SupportsPeras (LedgerSupportsPeras)
import Ouroboros.Consensus.Ledger.SupportsProtocol
  ( LedgerSupportsProtocol
  )
import Ouroboros.Consensus.MiniProtocol.ChainSync.Client
  ( ChainSyncClientException (..)
  )
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl as ChainDB
import Ouroboros.Consensus.Storage.LedgerDB.API
  ( CanUpgradeLedgerTables
  )
import Ouroboros.Consensus.Util.Condense
import Ouroboros.Consensus.Util.IOLike (Exception, fromException)
import qualified Ouroboros.Network.AnchoredFragment as AF
import Ouroboros.Network.Driver.Limits
  ( ProtocolLimitFailure (ExceededTimeLimit)
  )
import Ouroboros.Network.Util.ShowProxy
import Test.Consensus.BlockTree (onTrunk)
import Test.Consensus.Genesis.Setup.Classifiers
  ( Classifiers (..)
  , ResultClassifiers (..)
  , ScheduleClassifiers (..)
  , classifiers
  , resultClassifiers
  , scheduleClassifiers
  )
import Test.Consensus.Genesis.Setup.GenChains
import Test.Consensus.PeerSimulator.Config ()
import Test.Consensus.PeerSimulator.Run
import Test.Consensus.PeerSimulator.StateView
import Test.Consensus.PeerSimulator.Trace
  ( traceLinesWith
  , tracerTestBlock
  )
import Test.Consensus.PointSchedule
import Test.Consensus.PointSchedule.NodeState (NodeState)
import Test.QuickCheck
import Test.Tasty (TestTree)
import qualified Test.Tasty.QuickCheck as QC
import Test.Util.Orphans.IOLike ()
import Test.Util.QuickCheck (forAllGenRunShrinkCheck)
import Test.Util.TersePrinting (Terse)
import Test.Util.TestBlock (TestBlock)
import Test.Util.TestEnv
  ( adjustQuickCheckMaxSize
  , adjustQuickCheckTests
  )
import Test.Util.Tracer (recordingTracerM)
import Text.Printf (printf)

-- | Contains all necessary data to run a 'GenesisTest'.
-- It is defined to reify the testing infrastructure for
-- the conformance @testgen@ executable.
data ConformanceTest blk = ConformanceTest
  { ctGenerator :: Gen (GenesisTestFull blk)
  -- ^ The test generator.
  , ctSchedulerConfig :: SchedulerConfig
  -- ^ Peer simulator scheduler configuration.
  , ctShrinker :: (GenesisTestFull blk -> StateView blk -> [GenesisTestFull blk])
  -- ^ A shrinker allowed to inspect the output value of a test.
  , ctProperty :: GenesisTestFull blk -> StateView blk -> Property
  -- ^ The property to test.
  , ctDesiredPasses :: Int -> Int
  -- ^ Adjust the default number of test runs to check the property.
  , ctMaxSize :: Int -> Int
  -- ^ Adjust the default test case maximum size.
  , ctDescription :: String
  -- ^ A description for the test.
  }

mkConformanceTest ::
  Testable prop =>
  -- | Test description.
  String ->
  -- | Transformation of the default desired test passes/successes.
  (Int -> Int) ->
  -- | Transformation of the default max test size.
  (Int -> Int) ->
  -- | Test generator.
  Gen (GenesisTestFull blk) ->
  -- | Peer simulator scheduler configuration.
  SchedulerConfig ->
  -- | Result inspecting shrinker.
  (GenesisTestFull blk -> StateView blk -> [GenesisTestFull blk]) ->
  -- | Property on test result.
  (GenesisTestFull blk -> StateView blk -> prop) ->
  ConformanceTest blk
mkConformanceTest ctDescription ctDesiredPasses ctMaxSize ctGenerator ctSchedulerConfig ctShrinker mkProperty =
  let ctProperty = fmap property . mkProperty
   in ConformanceTest{..}

-- | Like 'runSimStrictShutdown' but fail when the main thread terminates if
-- there are other threads still running or blocked. If one is trying to follow
-- a strict thread clean-up policy then this helps testing for that.
runSimStrictShutdownOrThrow :: forall a. (forall s. IOSim s a) -> a
runSimStrictShutdownOrThrow action =
  case runSimStrictShutdown action of
    Left e -> throw e
    Right x -> x

-- | Runs the given 'GenesisTest' and 'PointSchedule' and evaluates the given
-- property on the final 'StateView'.
runGenesisTest ::
  ( Condense (StateView blk)
  , CondenseList (NodeState blk)
  , ShowProxy blk
  , ShowProxy (Header blk)
  , ConfigSupportsNode blk
  , LedgerSupportsProtocol blk
  , LedgerSupportsPeras blk
  , ChainDB.SerialiseDiskConstraints blk
  , BlockSupportsDiffusionPipelining blk
  , InspectLedger blk
  , HasHardForkHistory blk
  , ConvertRawHash blk
  , CanUpgradeLedgerTables (LedgerState blk)
  , HasPointScheduleTestParams blk
  , Eq (Header blk)
  , Eq blk
  , Terse blk
  , Condense (NodeState blk)
  ) =>
  ProtocolInfoArgs blk ->
  SchedulerConfig ->
  GenesisTestFull blk ->
  RunGenesisTestResult blk
runGenesisTest protocolInfoArgs schedulerConfig genesisTest =
  runSimStrictShutdownOrThrow $ do
    (recordingTracer, getTrace) <- recordingTracerM
    let tracer = if scDebug schedulerConfig then debugTracer else recordingTracer

    traceLinesWith tracer $ prettyGenesisTest prettyPointSchedule genesisTest

    rgtrStateView <-
      runPointSchedule protocolInfoArgs schedulerConfig genesisTest =<< tracerTestBlock tracer
    traceWith tracer (condense rgtrStateView)
    rgtrTrace <- unlines <$> getTrace

    pure $ RunGenesisTestResult{rgtrTrace, rgtrStateView}

-- | Variant of 'runGenesisTest' that also takes a property on the final
-- 'StateView' and returns a QuickCheck property. The trace is printed in case
-- of counter-example.
_runGenesisTest' ::
  Testable prop =>
  SchedulerConfig ->
  GenesisTestFull TestBlock ->
  (StateView TestBlock -> prop) ->
  Property
_runGenesisTest' schedulerConfig genesisTest makeProperty = idempotentIOProperty $ do
  protocolInfoArgs <- getProtocolInfoArgs
  let RunGenesisTestResult{rgtrTrace, rgtrStateView} =
        runGenesisTest protocolInfoArgs schedulerConfig genesisTest
  pure $ counterexample rgtrTrace $ makeProperty rgtrStateView

-- | All-in-one helper that generates a 'GenesisTest' and a 'Peers
-- PeerSchedule' from a 'ConformanceTest', runs them with 'runGenesisTest',
-- and checks whether the given property holds on the resulting 'StateView'.
runConformanceTest ::
  forall blk.
  ( Condense (StateView blk)
  , CondenseList (NodeState blk)
  , ShowProxy blk
  , ShowProxy (Header blk)
  , ConfigSupportsNode blk
  , LedgerSupportsProtocol blk
  , LedgerSupportsPeras blk
  , ChainDB.SerialiseDiskConstraints blk
  , BlockSupportsDiffusionPipelining blk
  , InspectLedger blk
  , HasHardForkHistory blk
  , ConvertRawHash blk
  , CanUpgradeLedgerTables (LedgerState blk)
  , HasPointScheduleTestParams blk
  , Eq (Header blk)
  , Eq blk
  , Terse blk
  , Condense (NodeState blk)
  ) =>
  ConformanceTest blk -> TestTree
runConformanceTest ConformanceTest{..} =
  adjustQuickCheckTests ctDesiredPasses . adjustQuickCheckMaxSize ctMaxSize $
    QC.testProperty ctDescription . idempotentIOProperty $ do
      protocolInfoArgs <- getProtocolInfoArgs
      pure $
        forAllGenRunShrinkCheck ctGenerator (runGenesisTest protocolInfoArgs ctSchedulerConfig) shrinker' $
          \genesisTest result ->
            let cls = classifiers genesisTest
                resCls = resultClassifiers genesisTest result
                schCls = scheduleClassifiers genesisTest
                stateView = rgtrStateView result
             in classify (allAdversariesSelectable cls) "All adversaries have more than k blocks after intersection"
                  $ classify
                    (allAdversariesForecastable cls)
                    "All adversaries have at least 1 forecastable block after intersection"
                  $ classify
                    (allAdversariesKPlus1InForecast cls)
                    "All adversaries have k+1 blocks in forecast window after intersection"
                  $ classify (genesisWindowAfterIntersection cls) "Full genesis window after intersection"
                  $ classify (adversaryRollback schCls) "An adversary did a rollback"
                  $ classify (honestRollback schCls) "The honest peer did a rollback"
                  $ classify (allAdversariesEmpty schCls) "All adversaries have empty schedules"
                  $ classify (allAdversariesTrivial schCls) "All adversaries have trivial schedules"
                  $ tabulate "Adversaries killed by LoP" [printf "%.1f%%" $ adversariesKilledByLoP resCls]
                  $ tabulate "Adversaries killed by GDD" [printf "%.1f%%" $ adversariesKilledByGDD resCls]
                  $ tabulate "Adversaries killed by Timeout" [printf "%.1f%%" $ adversariesKilledByTimeout resCls]
                  $ tabulate "Surviving adversaries" [printf "%.1f%%" $ adversariesSurvived resCls]
                  $ counterexample (rgtrTrace result)
                  $ ctProperty genesisTest stateView .&&. hasOnlyExpectedExceptions stateView
 where
  shrinker' gt = ctShrinker gt . rgtrStateView
  hasOnlyExpectedExceptions StateView{svPeerSimulatorResults} =
    conjoin $
      isExpectedException
        <$> mapMaybe
          (pscrToException . pseResult)
          svPeerSimulatorResults
  isExpectedException exn
    | Just EmptyBucket <- e = true
    | Just DensityTooLow <- e = true
    | Just (ExceededTimeLimit _) <- e = true
    | Just AsyncCancelled <- e = true
    | Just CandidateTooSparse{} <- e = true
    | otherwise =
        counterexample
          ("Encountered unexpected exception: " ++ show exn)
          False
   where
    e :: Exception e => Maybe e
    e = fromException exn
    true = property True

-- | The 'StateView.svSelectedChain' produces an 'AnchoredFragment (Header blk)';
-- this function casts this type's hash to its instance, so that it can be used
-- for lookups on a 'BlockTree'.
castHeaderHash :: ChainHash (Header blk) -> ChainHash blk
castHeaderHash = \case
  BlockHash hash -> BlockHash hash
  GenesisHash -> GenesisHash

-- | Check if the immutable tip of the selected chain of a 'GenesisTest' is honest.
-- In this setting, the immutable tip corresponds to the selected chain anchor
-- (see 'Ouroboros.Consensus.Storage.ChainDB.API.getCurrentChain') and
-- the honest chain is represented by the test 'BlockTree' trunk.
honestImmutableTip :: GetHeader blk => GenesisTestFull blk -> StateView blk -> Bool
honestImmutableTip GenesisTest{gtBlockTree} StateView{svSelectedChain} =
  onTrunk gtBlockTree $ AF.anchorPoint svSelectedChain

-- | Check if the tip of the selected chain of a 'GenesisTest' is honest.
-- In this setting, the honest chain corresponds to the test 'BlockTree' trunk.
selectedHonestChain :: GetHeader blk => GenesisTestFull blk -> StateView blk -> Bool
selectedHonestChain GenesisTest{gtBlockTree} StateView{svSelectedChain} =
  onTrunk gtBlockTree $ AF.headPoint $ svSelectedChain
