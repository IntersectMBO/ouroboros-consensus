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

import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Tracer (debugTracer, traceWith)
import           Ouroboros.Consensus.Util.Condense
import           Test.Consensus.Genesis.Setup.Classifiers (classifiers, Classifiers (..))
import           Test.Consensus.Genesis.Setup.GenChains
import           Test.Consensus.PeerSimulator.Run
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PeerSimulator.Trace (traceLinesWith)
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers (Peers)
import           Test.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.QuickCheck (forAllGenRunShrinkCheck)
import           Test.Util.Tracer (recordingTracerTVar)

-- | See 'runGenesisTest'.
data RunGenesisTestResult = RunGenesisTestResult {
  rgtrTrace :: String,
  rgtrStateView :: StateView
  }

-- | Runs the given 'GenesisTest' and 'PointSchedule' and evaluates the given
-- property on the final 'StateView'.
runGenesisTest ::
  SchedulerConfig ->
  GenesisTest (Peers PeerSchedule) ->
  RunGenesisTestResult
runGenesisTest schedulerConfig genesisTest =
  runSimOrThrow $ do
    (recordingTracer, getTrace) <- recordingTracerTVar
    let tracer = if scDebug schedulerConfig then debugTracer else recordingTracer

    traceLinesWith tracer $ prettyGenesisTest genesisTest

    rgtrStateView <- runPointSchedule schedulerConfig genesisTest tracer
    traceWith tracer (condense rgtrStateView)
    rgtrTrace <- unlines <$> getTrace

    pure $ RunGenesisTestResult {rgtrTrace, rgtrStateView}

-- | Variant of 'runGenesisTest' that also takes a property on the final
-- 'StateView' and returns a QuickCheck property. The trace is printed in case
-- of counter-example.
runGenesisTest' ::
  Testable prop =>
  SchedulerConfig ->
  GenesisTest (Peers PeerSchedule) ->
  (StateView -> prop) ->
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
  Gen (GenesisTest (Peers PeerSchedule)) ->
  SchedulerConfig ->
  (GenesisTest (Peers PeerSchedule) -> StateView -> [GenesisTest (Peers PeerSchedule)]) ->
  (GenesisTest (Peers PeerSchedule) -> StateView -> prop) ->
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
