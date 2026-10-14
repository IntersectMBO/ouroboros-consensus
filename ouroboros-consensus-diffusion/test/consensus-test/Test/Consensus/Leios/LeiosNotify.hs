-- | Tests of LeiosNotify's client, server and codec, on their own and as the
-- node runs them.
module Test.Consensus.Leios.LeiosNotify (tests) where

import qualified Test.Consensus.Leios.LeiosNotify.Codec as Codec
import qualified Test.Consensus.Leios.LeiosNotify.Node as Node
import qualified Test.Consensus.Leios.LeiosNotify.Protocol as Protocol
import Test.Tasty

tests :: TestTree
tests = testGroup "LeiosNotify" [Protocol.tests, Codec.tests, Node.tests]
