module Test.Consensus.Peras.Committee
  ( tests
  )
where

import qualified Test.Consensus.Peras.Committee.Model as Model
import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Peras committee selection tests"
    [ Model.tests
    ]
