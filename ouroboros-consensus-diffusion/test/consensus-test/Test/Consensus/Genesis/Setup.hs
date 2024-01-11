{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Test.Consensus.Genesis.Setup (
    module Test.Consensus.Genesis.Setup.GenChains
  , forAllGenesisTest
  , forAllGenesisTest'
  , runGenesisTest
  , runGenesisTest'
  ) where

import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Tracer (debugTracer, traceWith)
import           Data.Foldable (for_)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Network.Protocol.ChainSync.Codec
                     (ChainSyncTimeout (..))
import           Test.Consensus.BlockTree (allFragments)
import           Test.Consensus.Genesis.Setup.Classifiers (classifiers, Classifiers (..))
import           Test.Consensus.Genesis.Setup.GenChains
import           Test.Consensus.PeerSimulator.Run
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PeerSimulator.Trace (traceLinesWith)
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Peers (Peers)
import           Test.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TersePrinting (terseFragment)
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
  GenesisTest PointSchedule ->
  RunGenesisTestResult
runGenesisTest schedulerConfig genesisTest =
  runSimOrThrow $ do
    (recordingTracer, getTrace) <- recordingTracerTVar
    let tracer = if scDebug schedulerConfig then debugTracer else recordingTracer

    -- FIXME: should also go in 'prettyGenesisTest' (or 'prettyBlockTree')
    for_ (allFragments gtBlockTree) (traceWith tracer . terseFragment)

    traceLinesWith tracer $ [
      "SchedulerConfig:",
      "  ChainSyncTimeouts:",
      "    canAwait = " ++ show (canAwaitTimeout scChainSyncTimeouts),
      "    intersect = " ++ show (intersectTimeout scChainSyncTimeouts),
      "    mustReply = " ++ show (mustReplyTimeout scChainSyncTimeouts)
      ] ++ prettyGenesisTest genesisTest

    rgtrStateView <- runPointSchedule schedulerConfig genesisTest tracer
    traceWith tracer (condense rgtrStateView)
    rgtrTrace <- unlines <$> getTrace

    pure $ RunGenesisTestResult {rgtrTrace, rgtrStateView}
  where
    SchedulerConfig {scChainSyncTimeouts} = schedulerConfig
    GenesisTest {gtBlockTree} = genesisTest

-- | Variant of 'runGenesisTest' that also takes a property on the final
-- 'StateView' and returns a QuickCheck property. The trace is printed in case
-- of counter-example.
runGenesisTest' ::
  Testable prop =>
  SchedulerConfig ->
  GenesisTest PointSchedule ->
  (StateView -> prop) ->
  Property
runGenesisTest' schedulerConfig genesisTest makeProperty =
    counterexample rgtrTrace $ makeProperty rgtrStateView
  where
    RunGenesisTestResult{rgtrTrace, rgtrStateView} =
      runGenesisTest schedulerConfig genesisTest

-- | All-in-one helper that generates a 'GenesisTest' and a 'PointSchedule',
-- runs them with 'runGenesisTest', check whether the given property holds on
-- the resulting 'StateView'.
forAllGenesisTest ::
  Testable prop =>
  Gen (GenesisTest PointSchedule) ->
  SchedulerConfig ->
  (GenesisTest PointSchedule -> StateView -> [GenesisTest PointSchedule]) ->
  (GenesisTest PointSchedule -> StateView -> prop) ->
  Property
forAllGenesisTest = mkForAllGenesisTest id

-- | Same as 'forAllGenesisTest' but the schedule is a 'Peers PeerSchedule'.
forAllGenesisTest' ::
  Testable prop =>
  Gen (GenesisTest (Peers PeerSchedule)) ->
  SchedulerConfig ->
  (GenesisTest (Peers PeerSchedule) -> StateView -> [GenesisTest (Peers PeerSchedule)]) ->
  (GenesisTest (Peers PeerSchedule) -> StateView -> prop) ->
  Property
forAllGenesisTest' = mkForAllGenesisTest fromSchedulePoints

-- | Common code shared between flavours of 'forAllGenesisTest'.
mkForAllGenesisTest ::
  Testable prop =>
  (schedule -> PointSchedule) ->
  Gen (GenesisTest schedule) ->
  SchedulerConfig ->
  (GenesisTest schedule -> StateView -> [GenesisTest schedule]) ->
  (GenesisTest schedule -> StateView -> prop) ->
  Property
mkForAllGenesisTest mkPointSchedule generator schedulerConfig shrinker mkProperty =
  forAllGenRunShrinkCheck generator runner shrinker' $ \genesisTest result ->
    let cls = classifiers genesisTest
     in classify (allAdversariesSelectable cls) "All adversaries selectable" $
        classify (genesisWindowAfterIntersection cls) "Full genesis window after intersection" $
        counterexample (rgtrTrace result) $
        mkProperty genesisTest (rgtrStateView result)
  where
    runner = runGenesisTest schedulerConfig . fmap mkPointSchedule
    shrinker' gt = shrinker gt . rgtrStateView
