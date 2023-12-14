{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Test.Consensus.Genesis.Setup (
    module Test.Consensus.Genesis.Setup.GenChains
  , exceptionCounterexample
  , runTest
  ) where

import           Control.Exception (AsyncException (ThreadKilled))
import           Control.Monad.Class.MonadTime (MonadTime)
import           Control.Monad.Class.MonadTimer.SI (MonadTimer)
import           Control.Tracer (debugTracer, traceWith)
import           Data.Either (partitionEithers)
import           Data.Foldable (for_)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Network.Protocol.ChainSync.Codec
                     (ChainSyncTimeout (..))
import           Test.Consensus.BlockTree (allFragments)
import           Test.Consensus.Genesis.Setup.GenChains
import           Test.Consensus.PeerSimulator.Run
import           Test.Consensus.PeerSimulator.StateView
import           Test.Consensus.PeerSimulator.Trace (traceLinesWith)
import           Test.Consensus.PointSchedule
import           Test.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.TersePrinting (terseFragment)
import           Test.Util.Tracer (recordingTracerTVar)

-- | Runs the given point schedule and evaluates the given property on the final
-- state view.
runTest ::
  (IOLike m, MonadTime m, MonadTimer m, Testable a) =>
  SchedulerConfig ->
  GenesisTest ->
  PointSchedule ->
  (StateView -> a) ->
  m Property
runTest schedulerConfig genesisTest schedule makeProperty = do
    (recordingTracer, getTrace) <- recordingTracerTVar
    let tracer = if scDebug schedulerConfig then debugTracer else recordingTracer

    -- TODO: should also go in 'prettyGenesisTest' (or 'prettyBlockTree')
    for_ (allFragments gtBlockTree) (traceWith tracer . terseFragment)

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

    pure $ counterexample trace $ makeProperty finalStateView
  where
    SchedulerConfig {scChainSyncTimeouts} = schedulerConfig
    GenesisTest {gtBlockTree} = genesisTest

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
