{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.Genesis.Setup
  ( module Test.Consensus.Genesis.Setup,
    module Test.Consensus.Genesis.Setup.GenChains
  )
where

import           Control.Monad.Class.MonadTime (MonadTime)
import           Control.Monad.Class.MonadTimer.SI (MonadTimer)
import           Control.Tracer (traceWith)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import qualified Test.Consensus.BlockTree as BT
import           Test.Consensus.PointSchedule
import           Test.Consensus.PeerSimulator.Run
import           Test.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Tracer (recordingTracerTVar)
import Test.Consensus.Genesis.Setup.GenChains
import Test.Consensus.PeerSimulator.StateView

runTest ::
  (IOLike m, MonadTime m, MonadTimer m) =>
  GenesisTest ->
  PointSchedule ->
  (TestFragH -> Property) ->
  m Property
runTest genesisTest@GenesisTest {gtBlockTree, gtHonestAsc} schedule makeProperty = do
    (tracer, getTrace) <- recordingTracerTVar
    -- let tracer = debugTracer

    traceWith tracer $ "Honest active slot coefficient: " ++ show gtHonestAsc

    mapM_ (traceWith tracer) $ BT.prettyPrint gtBlockTree

    finalStateView <- runPointSchedule schedulerConfig genesisTest schedule tracer
    trace <- unlines <$> getTrace

    pure
      $ counterexample trace
      $ case svChainSyncExceptions finalStateView of
          [] ->
            let fragment = svSelectedChain finalStateView
            in counterexample ("result: " <> condense fragment) (makeProperty fragment)
          exns ->
            counterexample ("exceptions: " <> show exns) False

    where
      schedulerConfig = SchedulerConfig {enableTimeouts = False}
