-- | Tests of LeiosFetch's codec, its decision logic, and its client and server
-- as the node runs them.
module Test.Consensus.Leios.LeiosFetch (tests) where

import qualified Test.Consensus.Leios.LeiosFetch.Admission as Admission
import qualified Test.Consensus.Leios.LeiosFetch.AtomicCodec as AtomicCodec
import qualified Test.Consensus.Leios.LeiosFetch.AtomicReceive as AtomicReceive
import qualified Test.Consensus.Leios.LeiosFetch.AtomicServer as AtomicServer
import qualified Test.Consensus.Leios.LeiosFetch.Codec as Codec
import qualified Test.Consensus.Leios.LeiosFetch.Decision as Decision
import qualified Test.Consensus.Leios.LeiosFetch.Handoff as Handoff
import qualified Test.Consensus.Leios.LeiosFetch.Node as Node
import qualified Test.Consensus.Leios.LeiosFetch.Replies as Replies
import qualified Test.Consensus.Leios.LeiosFetch.Timing as Timing
import qualified Test.Consensus.Leios.LeiosFetch.Wait as Wait
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "LeiosFetch"
    [ AtomicServer.tests
    , AtomicReceive.tests
    , AtomicCodec.tests
    , Admission.tests
    , Timing.tests
    , Wait.tests
    , Handoff.tests
    , Codec.tests
    , Decision.tests
    , Replies.tests
    , Node.tests
    ]
