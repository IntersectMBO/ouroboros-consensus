{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Test.Consensus.Genesis.Setup
  ( module Test.Consensus.Genesis.Setup.GenChains,
    runTest,
    exceptionCounterexample,
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
import Ouroboros.Network.Protocol.ChainSync.Codec (ChainSyncTimeout(..))

runTest ::
  (IOLike m, MonadTime m, MonadTimer m, Testable a) =>
  SchedulerConfig ->
  GenesisTest ->
  PointSchedule ->
  (StateView -> a) ->
  m Property
runTest schedulerConfig genesisTest schedule makeProperty = do
    (tracer, getTrace) <- recordingTracerTVar

    traceWith tracer $ "Security param k = " ++ show gtSecurityParam
    traceWith tracer $ "Honest active slot coefficient asc = " ++ show gtHonestAsc
    traceWith tracer $ "Genesis window scg = " ++ show gtGenesisWindow

    traceWith tracer $ "SchedulerConfig:"
    traceWith tracer $ "  ChainSyncTimeouts:"
    traceWith tracer $ "    canAwait = " ++ show (canAwaitTimeout scChainSyncTimeouts)
    traceWith tracer $ "    intersect = " ++ show (intersectTimeout scChainSyncTimeouts)
    traceWith tracer $ "    mustReply = " ++ show (mustReplyTimeout scChainSyncTimeouts)

    mapM_ (traceWith tracer) $ BT.prettyPrint gtBlockTree

    finalStateView <- runPointSchedule schedulerConfig genesisTest schedule tracer
    trace <- unlines <$> getTrace

    pure $ counterexample trace $ makeProperty finalStateView
  where
    SchedulerConfig {scChainSyncTimeouts} = schedulerConfig
    GenesisTest {gtSecurityParam, gtHonestAsc, gtGenesisWindow, gtBlockTree} = genesisTest

-- | Print counterexamples if the test result contains exceptions.
exceptionCounterexample :: Testable a => (StateView -> a) -> StateView -> Property
exceptionCounterexample makeProperty stateView =
  case svChainSyncExceptions stateView of
    [] ->
      counterexample ("result: " <> condense (svSelectedChain stateView)) $
        makeProperty stateView
    exns ->
      counterexample ("exceptions: " <> show exns) False
