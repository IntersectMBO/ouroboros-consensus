{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Ouroboros.Consensus.Genesis.Setup
  ( module Test.Ouroboros.Consensus.Genesis.Setup,
    module Test.Ouroboros.Consensus.Genesis.Setup.GenChains
  )
where

import           Control.Monad.Class.MonadTime (MonadTime)
import           Control.Monad.Class.MonadTimer.SI (MonadTimer)
import           Control.Tracer (traceWith)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import qualified Test.Ouroboros.Consensus.BlockTree as BT
import           Test.Ouroboros.Consensus.PointSchedule
import           Test.Ouroboros.Consensus.PeerSimulator.Run
import           Test.QuickCheck
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Tracer (recordingTracerTVar)
import Test.Ouroboros.Consensus.Genesis.Setup.GenChains

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

    result <- runPointSchedule genesisTest schedule tracer
    trace <- unlines <$> getTrace

    let
      prop = case result of
        Left exn ->
          counterexample ("exception: " <> show exn) False
        Right fragment ->
          counterexample ("result: " <> condense fragment) (makeProperty fragment)

    pure $ counterexample trace prop
