{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module Test.Consensus.Genesis.Setup
  (module Test.Consensus.Genesis.Setup.GenChains
  , runTest
  , runTest'
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
import Data.List.NonEmpty (NonEmpty)

runTest ::
  (IOLike m, MonadTime m, MonadTimer m) =>
  GenesisTest ->
  PointSchedule ->
  (TestFragH -> Property) ->
  m Property
runTest genesisTest schedule makeProperty =
  runTest' genesisTest schedule $ \case
    Left exn -> counterexample ("exception: " <> show exn) False
    Right fragment -> counterexample ("result: " <> condense fragment) $ makeProperty fragment

-- | Same as 'runTest' except the predicate also gives access to the case where
-- exceptions were risen.
runTest' ::
  (IOLike m, MonadTime m, MonadTimer m) =>
  GenesisTest ->
  PointSchedule ->
  (Either (NonEmpty ChainSyncException) TestFragH -> Property) ->
  m Property
runTest' genesisTest@GenesisTest {gtBlockTree, gtHonestAsc} schedule makeProperty = do
    (tracer, getTrace) <- recordingTracerTVar
    -- let tracer = debugTracer

    traceWith tracer $ "Honest active slot coefficient: " ++ show gtHonestAsc

    mapM_ (traceWith tracer) $ BT.prettyPrint gtBlockTree

    result <- runPointSchedule genesisTest schedule tracer
    trace <- unlines <$> getTrace

    pure $ counterexample trace $ makeProperty result
