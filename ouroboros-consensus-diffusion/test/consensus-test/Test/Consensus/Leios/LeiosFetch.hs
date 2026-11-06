-- | Tests of LeiosFetch's codec, its decision logic, and its client and server
-- as the node runs them.
module Test.Consensus.Leios.LeiosFetch (tests) where

import qualified Test.Consensus.Leios.LeiosFetch.Acquisition as Acquisition
import qualified Test.Consensus.Leios.LeiosFetch.Admission as Admission
import qualified Test.Consensus.Leios.LeiosFetch.AtomicClient as AtomicClient
import qualified Test.Consensus.Leios.LeiosFetch.AtomicCodec as AtomicCodec
import qualified Test.Consensus.Leios.LeiosFetch.AtomicReceive as AtomicReceive
import qualified Test.Consensus.Leios.LeiosFetch.AtomicServer as AtomicServer
import qualified Test.Consensus.Leios.LeiosFetch.Body as Body
import qualified Test.Consensus.Leios.LeiosFetch.Closure as Closure
import qualified Test.Consensus.Leios.LeiosFetch.Codec as Codec
import qualified Test.Consensus.Leios.LeiosFetch.Decision as Decision
import qualified Test.Consensus.Leios.LeiosFetch.Handoff as Handoff
import qualified Test.Consensus.Leios.LeiosFetch.Node as Node
import qualified Test.Consensus.Leios.LeiosFetch.Pilots as Pilots
import qualified Test.Consensus.Leios.LeiosFetch.Replies as Replies
import qualified Test.Consensus.Leios.LeiosFetch.Retry as Retry
import qualified Test.Consensus.Leios.LeiosFetch.Timing as Timing
import qualified Test.Consensus.Leios.LeiosFetch.Wait as Wait
import Test.Tasty

tests :: TestTree
tests =
  testGroup
    "LeiosFetch"
    [ Retry.tests
    , Closure.tests
    , Body.tests
    , AtomicClient.tests
    , Acquisition.tests
    , AtomicServer.tests
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
    , Pilots.tests
    ]
