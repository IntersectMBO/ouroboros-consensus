{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Test.Consensus.Genesis.Setup (
    module Test.Consensus.Genesis.Setup.GenChains
  , forAllGenesisTest
  , runGenesisTest
  , runGenesisTest'
  ) where

import           Control.Exception (throw)
import           Control.Monad.IOSim (IOSim, runSimStrictShutdown)
import           Control.Tracer (debugTracer, traceWith)
import           Ouroboros.Consensus.Util.Condense
import           Test.Consensus.Genesis.Setup.Classifiers (classifiers, Classifiers (..))
import           Test.Consensus.Genesis.Setup.GenChains
import           Test.Consensus.PeerSimulator.Run
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PeerSimulator.Trace (traceLinesWith, mkTracerTestBlock)
import           Test.Consensus.PointSchedule
import           Test.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.QuickCheck (forAllGenRunShrinkCheck)
import           Test.Util.TestBlock (TestBlock)
import           Test.Util.Tracer (recordingTracerTVar)

-- | See 'runGenesisTest'.
data RunGenesisTestResult = RunGenesisTestResult {
  rgtrTrace :: String,
  rgtrStateView :: StateView TestBlock
  }

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
  SchedulerConfig ->
  GenesisTestFull TestBlock ->
  RunGenesisTestResult
runGenesisTest schedulerConfig genesisTest =
  runSimStrictShutdownOrThrow $ do
    (recordingTracer, getTrace) <- recordingTracerTVar
    let tracer = if scDebug schedulerConfig then debugTracer else recordingTracer

    traceLinesWith tracer $ prettyGenesisTest prettyPeersSchedule genesisTest

    rgtrStateView <- runPointSchedule schedulerConfig genesisTest (mkTracerTestBlock tracer)
    traceWith tracer (condense rgtrStateView)
    rgtrTrace <- unlines <$> getTrace

    pure $ RunGenesisTestResult {rgtrTrace, rgtrStateView}

-- | Variant of 'runGenesisTest' that also takes a property on the final
-- 'StateView' and returns a QuickCheck property. The trace is printed in case
-- of counter-example.
runGenesisTest' ::
  Testable prop =>
  SchedulerConfig ->
  GenesisTestFull TestBlock ->
  (StateView TestBlock -> prop) ->
  Property
runGenesisTest' schedulerConfig genesisTest makeProperty =
    counterexample rgtrTrace $ makeProperty rgtrStateView
  where
    RunGenesisTestResult{rgtrTrace, rgtrStateView} =
      runGenesisTest schedulerConfig genesisTest

-- | All-in-one helper that generates a 'GenesisTest' and a 'Peers
-- PeerSchedule', runs them with 'runGenesisTest', check whether the given
-- property holds on the resulting 'StateView'.
forAllGenesisTest ::
  Testable prop =>
  Gen (GenesisTestFull TestBlock) ->
  SchedulerConfig ->
  (GenesisTestFull TestBlock -> StateView TestBlock -> [GenesisTestFull TestBlock]) ->
  (GenesisTestFull TestBlock -> StateView TestBlock -> prop) ->
  Property
forAllGenesisTest generator schedulerConfig shrinker mkProperty =
  forAllGenRunShrinkCheck generator runner shrinker' $ \genesisTest result ->
    let cls = classifiers genesisTest
     in classify (allAdversariesSelectable cls) "All adversaries selectable" $
        classify (allAdversariesForecastable cls) "All adversaries forecastable" $
        classify (allAdversariesKPlus1InForecast cls) "All adversaries have k+1 blocks in forecast window after intersection" $
        classify (genesisWindowAfterIntersection cls) "Full genesis window after intersection" $
        counterexample (rgtrTrace result) $
        mkProperty genesisTest (rgtrStateView result)
  where
    runner = runGenesisTest schedulerConfig
    shrinker' gt = shrinker gt . rgtrStateView
