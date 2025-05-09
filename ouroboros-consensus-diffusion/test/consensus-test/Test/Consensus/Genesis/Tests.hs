module Test.Consensus.Genesis.Tests (tests) where

import Test.Consensus.Genesis.Tests.CSJ qualified as CSJ
import Test.Consensus.Genesis.Tests.DensityDisconnect qualified as GDD
import Test.Consensus.Genesis.Tests.LoE qualified as LoE
import Test.Consensus.Genesis.Tests.LoP qualified as LoP
import Test.Consensus.Genesis.Tests.LongRangeAttack qualified as LongRangeAttack
import Test.Consensus.Genesis.Tests.Uniform qualified as Uniform
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "Genesis tests"
    [ CSJ.tests
    , GDD.tests
    , LongRangeAttack.tests
    , LoE.tests
    , LoP.tests
    , Uniform.tests
    ]
