module Test.Ouroboros.Consensus.ChainGenerator.Tests (tests) where

import Test.Ouroboros.Consensus.ChainGenerator.Tests.Adversarial qualified as A
import Test.Ouroboros.Consensus.ChainGenerator.Tests.BitVector qualified as BV
import Test.Ouroboros.Consensus.ChainGenerator.Tests.Counting qualified as C
import Test.Ouroboros.Consensus.ChainGenerator.Tests.Honest qualified as H
import Test.Tasty qualified as TT

-----

tests :: TT.TestTree
tests =
  TT.testGroup "ChainGenerator" $
    []
      <> A.tests
      <> BV.tests
      <> C.tests
      <> H.tests
