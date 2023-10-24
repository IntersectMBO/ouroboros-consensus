{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Consensus.Genesis.Setup
  ( module Test.Consensus.Genesis.Setup.GenChains,
    runTest,
    runTest'
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
import Test.Consensus.Network.Driver.Limits.Extras (chainSyncNoTimeouts)
import Ouroboros.Network.Protocol.ChainSync.Codec (ChainSyncTimeout(..))

runTest ::
  (IOLike m, MonadTime m, MonadTimer m) =>
  SchedulerConfig ->
  GenesisTest ->
  PointSchedule ->
  (StateView -> Property) ->
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

-- | Same as 'runTest' except it fails the test in case of exception and it does
-- not feature any timeouts.
runTest' ::
  (IOLike m, MonadTime m, MonadTimer m) =>
  GenesisTest ->
  PointSchedule ->
  (StateView -> Property) ->
  m Property
runTest' genesisTest schedule makeProperty =
  runTest
    schedulerConfig
    genesisTest
    schedule
    $ \stateView ->
    case svChainSyncExceptions stateView of
      [] ->
        counterexample ("result: " <> condense (svSelectedChain stateView)) $
          makeProperty stateView
      exns ->
        counterexample ("exceptions: " <> show exns) False
  where
    schedulerConfig = SchedulerConfig {
      scChainSyncTimeouts = chainSyncNoTimeouts
      }
