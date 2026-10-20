-- | Tests of LeiosFetch's codec, and of its client and server as the node runs
-- them.
module Test.Consensus.Leios.LeiosFetch (tests) where

import qualified Test.Consensus.Leios.LeiosFetch.Codec as Codec
import qualified Test.Consensus.Leios.LeiosFetch.Node as Node
import Test.Tasty

tests :: TestTree
tests = testGroup "LeiosFetch" [Codec.tests, Node.tests]
