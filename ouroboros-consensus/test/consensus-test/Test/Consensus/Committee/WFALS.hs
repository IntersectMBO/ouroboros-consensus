module Test.Consensus.Committee.WFALS
  ( tests
  )
where

import qualified Data.Map.Strict as Map
import Test.Consensus.Committee.WFALS.Conformance (conformsToRustImplementation)
import qualified Test.Consensus.Committee.WFALS.Model as Model
import qualified Test.Consensus.Committee.WFALS.Model.Test as Model
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.Util.TestEnv (adjustQuickCheckTests)

tests :: TestTree
tests =
  testGroup
    "weighted Fait-Accompli committee selection tests"
    [ Model.tests
    , modelConformsToRustImplementation
    ]

-- | Check that the model implementation matches the Rust one
modelConformsToRustImplementation :: TestTree
modelConformsToRustImplementation =
  adjustQuickCheckTests (* 10) $
    testProperty "model conforms to Rust implementation" $
      conformsToRustImplementation model
 where
  model stakeDistr targetCommitteeSize =
    let (persistentSeats, numNonPersistentSeats, _) =
          Model.weightedFaitAccompliPersistentSeats
            (fromIntegral targetCommitteeSize)
            (Map.map Model.rationalToStake stakeDistr)
     in ( Map.size persistentSeats
        , fromIntegral numNonPersistentSeats
        )
