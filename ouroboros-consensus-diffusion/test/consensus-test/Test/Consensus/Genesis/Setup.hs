{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module Test.Consensus.Genesis.Setup (
    module Test.Consensus.Genesis.Setup.GenChains
  , exceptionCounterexample
  , runGenesisTest
  , runGenesisTest'
  , forAllGenesisTest
  , forAllGenesisTest'
  ) where

import           Control.Exception (AsyncException (ThreadKilled))
import           Control.Monad.IOSim (runSimOrThrow)
import           Control.Tracer (debugTracer, traceWith)
import           Data.Either (partitionEithers)
import           Data.Foldable (for_)
import           Data.Function ((&))
import           Data.List (intercalate)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Protocol.ChainSync.Codec
                     (ChainSyncTimeout (..))
import           System.Random.Stateful (runSTGen_)
import           Test.Consensus.BlockTree (allFragments)
import           Test.Consensus.Genesis.Setup.Classifiers
import           Test.Consensus.Genesis.Setup.GenChains
import           Test.Consensus.PeerSimulator.Run
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PeerSimulator.Trace (traceLinesWith, terseFrag)
import           Test.Consensus.PointSchedule
import           Test.Consensus.PointSchedule.Shrink (shrinkPointSchedule)
import           Test.QuickCheck
import           Test.QuickCheck.Random (QCGen)
import           Test.QuickCheck.Extras (unsafeMapSuchThatJust)
import           Test.Util.Orphans.IOLike ()
import           Test.Util.QuickCheck (forAllGenRunShrinkCheck)
import           Test.Util.Tracer (recordingTracerTVar)

data RunGenesisTestResult = RunGenesisTestResult {
  rgtrTrace :: String,
  rgtrStateView :: StateView
  }

-- | Runs the given point schedule and evaluates the given property on the final
-- state view.
runGenesisTest ::
  SchedulerConfig ->
  GenesisTest ->
  PointSchedule ->
  RunGenesisTestResult
runGenesisTest schedulerConfig genesisTest schedule =
  runSimOrThrow $ do
    (recordingTracer, getTrace) <- recordingTracerTVar
    let tracer = if scDebug schedulerConfig then debugTracer else recordingTracer

    -- FIXME: should also go in 'prettyGenesisTest' (or 'prettyBlockTree')
    for_ (allFragments gtBlockTree) (traceWith tracer . terseFrag)

    traceLinesWith tracer $ [
      "SchedulerConfig:",
      "  ChainSyncTimeouts:",
      "    canAwait = " ++ show (canAwaitTimeout scChainSyncTimeouts),
      "    intersect = " ++ show (intersectTimeout scChainSyncTimeouts),
      "    mustReply = " ++ show (mustReplyTimeout scChainSyncTimeouts)
      ] ++ prettyGenesisTest genesisTest

    finalStateView <- runPointSchedule schedulerConfig genesisTest schedule tracer
    traceWith tracer (condense finalStateView)
    trace <- unlines <$> getTrace

    pure $ RunGenesisTestResult trace finalStateView
  where
    SchedulerConfig {scChainSyncTimeouts} = schedulerConfig
    GenesisTest {gtBlockTree} = genesisTest

-- | Simple wrapper around 'runGenesisTest' that also takes a property to apply
-- to the resulting state view.
runGenesisTest' ::
  Testable prop =>
  SchedulerConfig ->
  GenesisTest ->
  PointSchedule ->
  (StateView -> prop) ->
  Property
runGenesisTest' schedulerConfig genesisTest schedule makeProperty = do
  runGenesisTest schedulerConfig genesisTest schedule
    & \RunGenesisTestResult {rgtrTrace, rgtrStateView} ->
      counterexample rgtrTrace $ makeProperty rgtrStateView

-- | Print counterexamples if the test result contains exceptions.
exceptionCounterexample :: Testable a => (StateView -> [PeerId] -> a) -> StateView -> Property
exceptionCounterexample makeProperty stateView =
  case svChainSyncExceptions stateView of
    exns | ([], killed) <- partitionEithers (genesisException <$> exns) ->
      property $ makeProperty stateView killed
    exns ->
      counterexample ("exceptions: " <> show exns) False
  where
    genesisException = \case
      (ChainSyncException peer e) | Just ThreadKilled <- fromException e -> Right peer
      exc -> Left exc

-- | All-in-one helper that generates a 'GenesisTest' and a point schedule, runs
-- them with 'runGenesisTest', check whether the given property holds on the
-- resulting 'StateView' and attempts to shrink if it does not.
forAllGenesisTest ::
  Testable prop =>
  Gen (GenesisTest, PointSchedule) ->
  SchedulerConfig ->
  (StateView -> prop) ->
  Property
forAllGenesisTest generator schedulerConfig mkProperty =
  forAllGenRunShrinkCheck generator runner shrinker
    $ \(genesisTest, _) RunGenesisTestResult{rgtrTrace, rgtrStateView} ->
      let cls = classifiers genesisTest in
      classify (allAdversariesSelectable cls) "All adversaries selectable" $
      classify (genesisWindowAfterIntersection cls) "Full genesis window after intersection" $
      counterexample rgtrTrace $
      exceptionCounterexample
        (\stateView' killed ->
          killCounterexample killed $
          mkProperty stateView')
        rgtrStateView

  where
    runner = uncurry (runGenesisTest schedulerConfig)
    shrinker (gt, ps) _ = (gt,) <$> shrinkPointSchedule (gtBlockTree gt) ps
    killCounterexample [] = property
    killCounterexample killed = counterexample ("Some peers were killed: " ++ intercalate ", " (condense <$> killed))

-- | Variant of 'forAllGenesisTest' that generate a 'GenesisTest' and a
-- 'PointSchedule' given a number of alternative branches in the block tree and
-- a schedule type.
forAllGenesisTest' ::
  Testable prop =>
  Gen Word -> -- ^ number of alternative branches in the block tree
  ScheduleType ->
  SchedulerConfig ->
  (StateView -> prop) ->
  Property
forAllGenesisTest' numBranches scheduleType schedulerConfig@SchedulerConfig{scSchedule} =
    forAllGenesisTest genChainsAndSchedule schedulerConfig
  where
    genChainsAndSchedule :: Gen (GenesisTest, PointSchedule)
    genChainsAndSchedule =
      unsafeMapSuchThatJust do
        gt@GenesisTest{gtBlockTree} <- genChains =<< numBranches
        seed :: QCGen <- arbitrary
        pure $ (gt,) <$> runSTGen_ seed (\g -> genSchedule g scSchedule scheduleType gtBlockTree)
