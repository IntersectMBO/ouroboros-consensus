{-# LANGUAGE RankNTypes #-}

module Test.Consensus.Committee.WFALS (tests) where

import qualified Data.Map.Strict as Map
import Test.Consensus.Committee.WFALS.Conformance (conformsToRustImplementation)
import qualified Test.Consensus.Committee.WFALS.Model as Model
import qualified Test.Consensus.Committee.WFALS.Model.Tests as Model
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "WFALS"
    [ Model.tests
    , modelConformsToRustImplementation
    ]

-- | Check that the model implementation matches the Rust one
modelConformsToRustImplementation :: TestTree
modelConformsToRustImplementation =
  conformsToRustImplementation
    "Model conforms to Rust implementation"
    mkStakeDistr
    model
 where
  -- NOTE: this is not a good tiebreaker for real-world use, but it is
  -- sufficient here because the actual order of pools with the same stake does
  -- not matter when computing the persistent vs. non-persistent seat /counts/.
  tiebreaker =
    compare

  mkStakeDistr =
    Map.map Model.rationalToStake

  model stakeDistr targetCommitteeSize = do
    let totalSeats = fromIntegral targetCommitteeSize
    case Model.weightedFaitAccompliPersistentSeats tiebreaker totalSeats stakeDistr of
      Left err ->
        error $ "Model implementation failed with error: " <> show err
      Right (persistentSeats, numNonPersistentSeats, _) ->
        ( fromIntegral (Map.size persistentSeats)
        , fromIntegral numNonPersistentSeats
        )
