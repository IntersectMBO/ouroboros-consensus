module Test.Consensus.Genesis.Tests (tests) where

import qualified Test.Consensus.Genesis.Tests.DensityDisconnect as GDD
import qualified Test.Consensus.Genesis.Tests.LoE as LoE
import qualified Test.Consensus.Genesis.Tests.LongRangeAttack as LongRangeAttack
import qualified Test.Consensus.Genesis.Tests.LoP as LoP
import qualified Test.Consensus.Genesis.Tests.Uniform as Uniform
import           Test.Tasty

tests :: TestTree
tests = testGroup "Genesis tests"
    [ GDD.tests
    , LongRangeAttack.tests
    , LoE.tests
    , LoP.tests
    , Uniform.tests
    ]
